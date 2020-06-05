package org.bitcoins.server

import akka.actor.ActorSystem
import akka.event.Logging
import akka.http.scaladsl._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import akka.http.scaladsl.server.directives.DebuggingDirectives
import akka.http.scaladsl.settings.ServerSettings
import akka.util.ByteString
import de.heikoseeberger.akkahttpupickle.UpickleSupport._
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.db.AppConfig
import upickle.{default => up}

import scala.concurrent.Future

case class Server(
    conf: AppConfig,
    handlers: Seq[ServerRoute],
    rpcport: Int = 9999)(implicit system: ActorSystem)
    extends BitcoinSLogger {
  implicit private lazy val config: AppConfig = conf

  import system.dispatcher

  /** Handles all server commands by throwing a MethodNotFound */
  private lazy val catchAllHandler: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand(name, _) => throw HttpError.MethodNotFound(name)
  }

  /** HTTP directive that handles both exceptions and rejections */
  private def withErrorHandling(route: Route): Route = {

    val rejectionHandler =
      RejectionHandler
        .newBuilder()
        .handleNotFound {
          complete {
            Server.httpError(
              """Resource not found. Hint: all RPC calls are made against root ('/')""",
              StatusCodes.BadRequest)
          }
        }
        .result()

    val exceptionHandler = ExceptionHandler {
      case HttpError.MethodNotFound(method) =>
        complete(
          Server.httpError(s"'$method' is not a valid method",
                           StatusCodes.BadRequest))
      case err: Throwable =>
        logger.info(s"Unhandled error in server:", err)
        complete(Server.httpError(s"Request failed: ${err.getMessage}"))
    }

    handleRejections(rejectionHandler) {
      handleExceptions(exceptionHandler) {
        route
      }
    }
  }

  def route: Route = {
    // TODO implement better logging
/*    DebuggingDirectives.logRequestResult("http-rpc-server", Logging.InfoLevel) {
      withErrorHandling {
        pathSingleSlash {
          post {
            entity(as[ServerCommand]) { cmd =>
              val init = PartialFunction.empty[ServerCommand, StandardRoute]
              val handler = handlers.foldLeft(init) {
                case (accum, curr) => accum.orElse(curr.handleCommand)
              }
              handler.orElse(catchAllHandler).apply(cmd)
            }
          }
        }
      }
    }*/
    ???
  }

  def start(): Future[Http.ServerBinding] = try {
    println(s"Begin")
    val entity = HttpEntity.Empty
    val response = HttpResponse(status = 503,
      entity = entity)
    require(response != null, s"Response was null")
    println(s"Http response not null")
    require(response.entity != null, s"response.entity was null")
    println(s"response.entity not null")
    require(response.entity.isKnownEmpty(), s"response.entity.isKnownEmpty() was null")
    println(s"not null isKnownEmpty()")
    val serverSettings = ServerSettings(system)
      .withVerboseErrorMessages(true)
      .withTerminationDeadlineExceededResponse(response)
    println(s"Server settings=${serverSettings.toString}")
    val httpFut = {
      Http().bindAndHandle(handler = route,
        interface = "localhost",
        port = rpcport,
        settings = serverSettings)
    }
    println(s"Starting Bitcoin-S HTTP server")
    httpFut.foreach { http =>
      logger.info(s"Started Bitcoin-S HTTP server at ${http.localAddress}")
    }
    httpFut
  } catch {
    case scala.util.control.NonFatal(exn) =>
      println(s"EXN THROWN ${exn}")
      sys.exit(1)
  }
}

object Server {

  // TODO id parameter
  case class Response(
      result: Option[ujson.Value] = None,
      error: Option[String] = None) {

    def toJsonMap: Map[String, ujson.Value] = {
      Map(
        "result" -> (result match {
          case None      => ujson.Null
          case Some(res) => res
        }),
        "error" -> (error match {
          case None      => ujson.Null
          case Some(err) => err
        })
      )
    }
  }

  /** Creates a HTTP response with the given body as a JSON response */
  def httpSuccess[T](body: T)(
      implicit writer: up.Writer[T]): HttpEntity.Strict = {
    val response = Response(result = Some(up.writeJs(body)))
    HttpEntity(
      ContentTypes.`application/json`,
      up.write(response.toJsonMap)
    )
  }

  def httpError(
      msg: String,
      status: StatusCode = StatusCodes.InternalServerError): HttpResponse = {

    val entity = {
      val response = Response(error = Some(msg))
      HttpEntity(
        ContentTypes.`application/json`,
        up.write(response.toJsonMap)
      )
    }

    HttpResponse(status = status, entity = entity)
  }
}
