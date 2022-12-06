package org.bitcoins.scripts.bitmex;

import java.util.List;

public class BitmexProof {

  private long height = 0;
  private String blockhash;
  private String chain;
  private Long total;

  private List<String> keys;

  private BitmexClaim claim;

  private String xpub;


  public void setHeight(Long h) {
    height = h;
  }

  public Long getHeight() {
    return height;
  }

  public void setBlockhash(String hash) {
    blockhash = hash;
  }

  public String getBlockhash() {
    return blockhash;
  }

  public void setChain(String c) {
    chain = c;
  }

  public String getChain() {
    return chain;
  }

  public void setTotal(Long t) {
    total = t;
  }

  public Long getTotal() {
    return total;
  }

  public void setKeys(List<String> k)  {
    keys = k;
  }

  public List<String> getKeys() {
    return keys;
  }

  public void setXpub(String pub) {
    xpub = pub;
  }

  public String getXpub() {
    return xpub;
  }

  public void setClaim(BitmexClaim c) {
    claim = c;
  }

  public BitmexClaim getClaim() {
    return claim;
  }
}
