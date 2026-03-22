# Integrating Spark gRPC Proto Files into SparkRpcClient

This guide walks through converting the existing Go-generated protobuf stubs in `spark-rpc/src/main/protobuf/`
into proper `.proto` source files and building a `SparkRpcClient` using PekkoGrpcPlugin (already configured in
`build.sbt`).

---

## Overview

The `spark-rpc` module is already wired up:

- `PekkoGrpcPlugin` enabled in `build.sbt`
- `pekko-grpc-sbt-plugin` in `project/plugins.sbt`
- Dependencies in `Deps.sparkRpc` (mirrors `lndRpc`)
- `SparkInstance.scala` stub exists

The `src/main/protobuf/` directories currently contain **Go-generated** `.pb.go` files — not `.proto` source files.
PekkoGrpcPlugin requires actual `.proto` files. These Go files are useful references for reconstructing the proto
schemas.

### Services to implement

| Proto file            | Package          | Service(s)                          |
|-----------------------|------------------|-------------------------------------|
| `spark.proto`         | `spark`          | `SparkService` (40+ RPCs)           |
| `spark_authn.proto`   | `spark_authn`    | `SparkAuthnService` (auth flow)     |
| `spark_token.proto`   | `spark_token`    | `SparkTokenService` (token ops)     |
| `common.proto`        | `common`         | (shared message types, no service)  |

---

## Step 1: Obtain the `.proto` Source Files

The Go files show the source is `github.com/lightsparkdev/spark`. Clone the repo to extract the proto files:

```bash
git clone https://github.com/lightsparkdev/spark /tmp/spark-proto-source
```

The proto files live under the `proto/` directory:

```
/tmp/spark-proto-source/proto/
  common/common.proto
  spark/spark.proto
  spark_authn/spark_authn.proto
  spark_token/spark_token.proto
  spark_token_internal/spark_token_internal.proto
  spark_internal/spark_internal.proto
  frost/frost.proto
  dkg/dkg.proto
  gossip/gossip.proto
  multisig/multisig.proto
  mock/mock.proto
```

Copy the proto files into the bitcoin-s module, **replacing** the Go files:

```bash
PROTO_SRC=/tmp/spark-proto-source/proto
DEST=/Users/chrisstewart/dev/bitcoin-s/spark-rpc/src/main/protobuf

# Remove Go stubs
find "$DEST" -name "*.pb.go" -delete

# Copy proto files
for dir in common spark spark_authn spark_token spark_token_internal \
           spark_internal frost dkg gossip multisig mock; do
  cp "$PROTO_SRC/$dir/$dir.proto" "$DEST/$dir/"
done
```

> **Tip:** If the repo layout differs, search for `service SparkService` and `service SparkAuthnService` in the
> cloned repo to locate the proto files.

---

## Step 2: Configure Proto Files for Scala / PekkoGrpcPlugin

PekkoGrpcPlugin uses `scalapb` under the hood. You need to add a `scalapb.proto` import option to each proto file
to control code generation. At minimum, add a `scala_package` option so the generated classes land in a predictable
package.

At the **top of each `.proto` file**, add:

```protobuf
import "scalapb/scalapb.proto";

option (scalapb.options) = {
    package_name: "org.bitcoins.spark.rpc.proto"
    flat_package: true
};
```

For `spark.proto` specifically, also add a `scala_package_name` in the preamble. For example:

```protobuf
syntax = "proto3";
package spark;

import "scalapb/scalapb.proto";
import "common/common.proto";
import "google/protobuf/empty.proto";
import "google/protobuf/timestamp.proto";

option (scalapb.options) = {
    package_name: "org.bitcoins.spark.rpc.proto.spark"
    flat_package: true
};
```

Do the same for the other packages:

| Proto file          | `package_name`                                          |
|---------------------|---------------------------------------------------------|
| `common.proto`      | `org.bitcoins.spark.rpc.proto.common`                   |
| `spark.proto`       | `org.bitcoins.spark.rpc.proto.spark`                    |
| `spark_authn.proto` | `org.bitcoins.spark.rpc.proto.spark_authn`              |
| `spark_token.proto` | `org.bitcoins.spark.rpc.proto.spark_token`              |

---

## Step 3: Verify the Build Generates Scala Code

Run:

```bash
sbt sparkRpc/compile
```

SBT will invoke `protoc` via PekkoGrpcPlugin and generate Scala sources under:

```
spark-rpc/target/scala-2.13/src_managed/main/
```

You should see generated files like:
- `spark/SparkServiceClient.scala`
- `spark_authn/SparkAuthnServiceClient.scala`
- `spark_token/SparkTokenServiceClient.scala`

If proto files reference each other (e.g. `spark.proto` imports `common.proto`), make sure the imports use paths
relative to the `src/main/protobuf` root.

---

## Step 4: Implement `SparkRpcClient`

### 4a. Update `SparkInstance.scala`

Add remote support following the pattern from `LndInstance`:

```scala
// spark-rpc/src/main/scala/org/bitcoins/spark/rpc/SparkInstance.scala
package org.bitcoins.spark.rpc

import java.net.URI

sealed trait SparkInstance {
  def rpcUri: URI
}

/** Connect to a locally-managed Spark node */
case class SparkInstanceLocal(rpcUri: URI) extends SparkInstance

/** Connect to a remote Spark operator node */
case class SparkInstanceRemote(
    rpcUri: URI,
    /** hex-encoded secp256k1 public key for challenge-response auth */
    publicKey: String,
    /** optional TLS certificate (PEM string) */
    certificateOpt: Option[String] = None
) extends SparkInstance
```

### 4b. Create `SparkRpcClient.scala`

The Spark API uses a **challenge-response authentication** mechanism via `SparkAuthnService`:

1. Call `GetChallenge(public_key)` → receive a challenge
2. Sign the challenge bytes with the corresponding private key
3. Call `VerifyChallenge(signed_challenge)` → receive a session token
4. Attach the session token in metadata for all subsequent `SparkService` calls

```scala
// spark-rpc/src/main/scala/org/bitcoins/spark/rpc/SparkRpcClient.scala
package org.bitcoins.spark.rpc

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.grpc.GrpcClientSettings
import org.bitcoins.commons.util.BitcoinSLogger
import org.bitcoins.core.util.StartStopAsync
import io.grpc.{CallCredentials, Metadata}
import org.bitcoins.spark.rpc.proto.spark._
import org.bitcoins.spark.rpc.proto.spark_authn._
import org.bitcoins.spark.rpc.proto.spark_token._
import com.google.protobuf.ByteString

import java.io.{ByteArrayInputStream, FileInputStream, InputStream}
import java.util.concurrent.Executor
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class SparkRpcClient(instance: SparkInstance)(
    implicit val system: ActorSystem
) extends StartStopAsync[SparkRpcClient]
    with BitcoinSLogger {

  implicit val executionContext: ExecutionContext = system.dispatcher

  // ── TLS ─────────────────────────────────────────────────────────────────────

  private lazy val certStreamOpt: Option[InputStream] = instance match {
    case SparkInstanceRemote(_, _, Some(cert)) =>
      Some(
        new ByteArrayInputStream(
          cert.getBytes(java.nio.charset.StandardCharsets.UTF_8)
        )
      )
    case _ => None
  }

  // ── Session-token auth ──────────────────────────────────────────────────────

  /** Holds the session token obtained from VerifyChallenge */
  private var sessionTokenOpt: Option[String] = None

  private val sessionTokenKey =
    Metadata.Key.of("authorization", Metadata.ASCII_STRING_MARSHALLER)

  private lazy val callCredentials = new CallCredentials {
    def applyRequestMetadata(
        requestInfo: CallCredentials.RequestInfo,
        appExecutor: Executor,
        applier: CallCredentials.MetadataApplier
    ): Unit = {
      appExecutor.execute(() => {
        Try {
          sessionTokenOpt.foreach { token =>
            val metadata = new Metadata()
            metadata.put(sessionTokenKey, s"Bearer $token")
            applier(metadata)
          }
        }
        ()
      })
    }

    override def thisUsesUnstableApi(): Unit = ()
  }

  // ── gRPC client settings ────────────────────────────────────────────────────

  private lazy val clientSettings: GrpcClientSettings = {
    import org.apache.pekko.grpc.SSLContextUtils

    val base = GrpcClientSettings
      .connectToServiceAt(instance.rpcUri.getHost, instance.rpcUri.getPort)
      .withCallCredentials(callCredentials)

    certStreamOpt match {
      case Some(stream) =>
        base.withTrustManager(SSLContextUtils.trustManagerFromStream(stream))
      case None =>
        // Disable TLS for plaintext (dev/local only)
        base.withTls(false)
    }
  }

  // ── Generated service stubs ─────────────────────────────────────────────────

  lazy val sparkService: SparkServiceClient =
    SparkServiceClient(clientSettings)

  lazy val authnService: SparkAuthnServiceClient =
    SparkAuthnServiceClient(clientSettings)

  lazy val tokenService: SparkTokenServiceClient =
    SparkTokenServiceClient(clientSettings)

  // ── Authentication flow ─────────────────────────────────────────────────────

  /**
   * Performs the full challenge-response authentication and caches the session
   * token. Call this once before invoking SparkService methods.
   *
   * @param publicKeyBytes  uncompressed secp256k1 public key (33 or 65 bytes)
   * @param sign            function that signs the raw challenge bytes and
   *                        returns the DER-encoded signature
   */
  def authenticate(
      publicKeyBytes: Array[Byte],
      sign: Array[Byte] => Array[Byte]
  ): Future[Unit] = {
    for {
      challengeResp <- authnService.getChallenge(
        GetChallengeRequest(
          publicKey = ByteString.copyFrom(publicKeyBytes)
        )
      )
      challengeBytes = challengeResp.challenge.toByteArray
      signature      = sign(challengeBytes)
      verifyResp <- authnService.verifyChallenge(
        VerifyChallengeRequest(
          publicKey = ByteString.copyFrom(publicKeyBytes),
          signature = ByteString.copyFrom(signature)
        )
      )
    } yield {
      sessionTokenOpt = Some(verifyResp.token)
      logger.info("Spark authentication successful")
    }
  }

  // ── Convenience wrappers ────────────────────────────────────────────────────

  def queryBalance(
      request: QueryBalanceRequest
  ): Future[QueryBalanceResponse] = {
    logger.trace("spark calling query_balance")
    sparkService.queryBalance(request)
  }

  def generateDepositAddress(
      request: GenerateDepositAddressRequest
  ): Future[GenerateDepositAddressResponse] = {
    logger.trace("spark calling generate_deposit_address")
    sparkService.generateDepositAddress(request)
  }

  def queryPendingTransfers(
      filter: TransferFilter
  ): Future[QueryTransfersResponse] = {
    logger.trace("spark calling query_pending_transfers")
    sparkService.queryPendingTransfers(filter)
  }

  def queryAllTransfers(
      filter: TransferFilter
  ): Future[QueryTransfersResponse] = {
    logger.trace("spark calling query_all_transfers")
    sparkService.queryAllTransfers(filter)
  }

  // ── StartStopAsync ──────────────────────────────────────────────────────────

  override def start(): Future[SparkRpcClient] =
    Future.successful(this)

  override def stop(): Future[SparkRpcClient] = {
    for {
      _ <- sparkService.close()
      _ <- authnService.close()
      _ <- tokenService.close()
    } yield this
  }
}
```

---

## Step 5: Wire Authentication with `ECPrivateKey`

Bitcoin-S has `ECPrivateKey` in `crypto`. Use it to sign challenges:

```scala
import org.bitcoins.crypto.{ECPrivateKey, CryptoUtil}
import scodec.bits.ByteVector

val privKey = ECPrivateKey.freshPrivateKey
val pubKeyBytes = privKey.publicKey.bytes.toArray

val client = SparkRpcClient(
  SparkInstanceRemote(
    rpcUri = new URI("https://your-spark-operator:9090"),
    publicKey = privKey.publicKey.hex
  )
)

// Authenticate once at startup
val authF = client.authenticate(
  publicKeyBytes = pubKeyBytes,
  sign = bytes => {
    val hash = CryptoUtil.sha256(ByteVector(bytes))
    privKey.sign(hash).bytes.toArray
  }
)
```

---

## Step 6: Add `sparkRpc` to the Root Aggregate (Optional)

In `build.sbt`, `sparkRpc` is not yet in the `bitcoin-s` aggregate. Add it if desired:

```scala
// build.sbt — bitcoin-s root project
lazy val `bitcoin-s` = project
  .aggregate(
    // ...existing projects...
    sparkRpc
  )
  .dependsOn(
    // ...existing projects...
    sparkRpc
  )
```

---

## Step 7: Write a Basic Test

Create `spark-rpc-test/src/test/scala/org/bitcoins/spark/rpc/SparkRpcClientTest.scala`:

```scala
package org.bitcoins.spark.rpc

import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.spark.rpc.proto.spark.QueryBalanceRequest

import java.net.URI

class SparkRpcClientTest extends BitcoinSAsyncTest {

  // Requires a running Spark operator on localhost:9090 with TLS disabled
  lazy val client = SparkRpcClient(
    SparkInstanceLocal(rpcUri = new URI("http://localhost:9090"))
  )

  it must "query balance" in {
    client.queryBalance(QueryBalanceRequest()).map { resp =>
      assert(resp.balance >= 0)
    }
  }
}
```

Run with:
```bash
sbt "sparkRpc/test"
```

---

## Troubleshooting

| Problem | Fix |
|---------|-----|
| `proto` import not found (`google/protobuf/empty.proto`) | PekkoGrpcPlugin bundles well-known types; no manual download needed. |
| `scalapb` option not recognised | Ensure `scalapb/scalapb.proto` is in the protobuf search path — PekkoGrpcPlugin puts it in `target/protobuf_external/`. |
| Cross-package proto imports fail | All imports must be relative to the `src/main/protobuf` root. Use `import "common/common.proto";` (not an absolute path). |
| `withTls(false)` compile error | Older Pekko gRPC versions use `.withUseTls(false)`; check the 1.2.x API. |
| TLS certificate errors | Pass the PEM string via `SparkInstanceRemote.certificateOpt`. |
| Session token expired | Re-call `authenticate()` to refresh — Spark tokens have a TTL. |

---

## Summary of File Changes

| File | Action |
|------|--------|
| `src/main/protobuf/**/*.pb.go` | **Delete** — replaced by `.proto` files |
| `src/main/protobuf/**/*.proto` | **Add** — copied from `github.com/lightsparkdev/spark/proto` |
| `src/main/scala/.../SparkInstance.scala` | **Update** — add `SparkInstanceRemote` |
| `src/main/scala/.../SparkRpcClient.scala` | **Create** — new gRPC client |
| `spark-rpc-test/.../SparkRpcClientTest.scala` | **Create** — integration test |

