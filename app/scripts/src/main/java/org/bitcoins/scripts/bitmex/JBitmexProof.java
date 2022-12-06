package org.bitcoins.scripts.bitmex;

import java.util.List;

public class JBitmexProof {

  private long height;
  private String blockhash;
  private String chain;
  private Long total;

  private List<String> keys;

  private JBitmexClaim claim;

  private List<String> xpub;

  private List<JBitmexUtxo> address;

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

  public void setXpub(List<String> pubs) {
    xpub = pubs;
  }

  public List<String> getXpub() {
    return xpub;
  }

  public void setClaim(JBitmexClaim c) {
    claim = c;
  }

  public JBitmexClaim getClaim() {
    return claim;
  }

  public void setAddress(List<JBitmexUtxo> addrs) {
    address = addrs;
  }

  public List<JBitmexUtxo> getAddress() {
    return address;
  }
}
