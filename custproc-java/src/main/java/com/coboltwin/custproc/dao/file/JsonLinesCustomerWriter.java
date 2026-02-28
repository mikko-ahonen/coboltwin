package com.coboltwin.custproc.dao.file;

import com.coboltwin.custproc.dao.CustomerWriter;
import com.coboltwin.custproc.model.CustomerRecord;

import java.io.PrintStream;

/**
 * Emits JSON Lines to a PrintStream using manual string concatenation.
 * Matches the exact format of the COBOL DISPLAY statement in TEST-WRAPPER.cbl.
 */
public class JsonLinesCustomerWriter implements CustomerWriter {

    private final PrintStream out;

    public JsonLinesCustomerWriter(PrintStream out) {
        this.out = out;
    }

    @Override
    public void write(CustomerRecord rec) {
        out.println(
            "{\"cust_id\":\"" + rec.getCustId()
            + "\",\"name\":\"" + rec.getName()
            + "\",\"balance\":\"" + rec.getBalance()
            + "\",\"status\":\"" + rec.getStatus()
            + "\",\"segment\":\"" + rec.getSegment() + "\"}");
    }

    @Override
    public void close() {
        out.flush();
    }
}
