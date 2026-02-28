package com.coboltwin.custproc.model;

public class CustomerRecord {
    private String custId;
    private String name;
    private String balance;
    private String status;
    private String segment;

    public CustomerRecord() {}

    public CustomerRecord(String custId, String name, String balance) {
        this.custId = custId;
        this.name = name;
        this.balance = balance;
    }

    public String getCustId()  { return custId; }
    public String getName()    { return name; }
    public String getBalance() { return balance; }
    public String getStatus()  { return status; }
    public String getSegment() { return segment; }

    public void setCustId(String custId)   { this.custId = custId; }
    public void setName(String name)       { this.name = name; }
    public void setBalance(String balance) { this.balance = balance; }
    public void setStatus(String status)   { this.status = status; }
    public void setSegment(String segment) { this.segment = segment; }
}
