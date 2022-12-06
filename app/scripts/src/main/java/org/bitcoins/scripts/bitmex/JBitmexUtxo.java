package org.bitcoins.scripts.bitmex;

public class JBitmexUtxo {

    private String addr_type;
    private String addr;
    private String script;
    private Long balance;

    public void setAddr_type(String c) {
        addr_type = c;
    }

    public String getAddr_type() {
        return addr_type;
    }

    public void setAddr(String addr) {
        this.addr = addr;
    }

    public String getAddr() {
        return addr;
    }

    public void setScript(String script) {
        this.script = script;
    }

    public String getScript() {
        return script;
    }

    public void setBalance(Long balance) {
        this.balance = balance;
    }

    public Long getBalance() {
        return balance;
    }
}
