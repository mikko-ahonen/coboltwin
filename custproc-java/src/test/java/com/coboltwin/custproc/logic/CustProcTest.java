package com.coboltwin.custproc.logic;

import com.coboltwin.custproc.dao.DliService;
import com.coboltwin.custproc.model.CustomerRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

class CustProcTest {

    private CustProc custProc;

    @BeforeEach
    void setUp() {
        DliService stubDli = custId -> "RETAIL";
        custProc = new CustProc(stubDli);
    }

    @Test
    void validCustomer() {
        CustomerRecord rec = new CustomerRecord(
                "0000000001", "ALICE                         ", "+0000123.45");
        custProc.process(rec);
        assertEquals("VALID     ", rec.getStatus());
        assertEquals("RETAIL    ", rec.getSegment());
    }

    @Test
    void blankNameIsInvalid() {
        CustomerRecord rec = new CustomerRecord(
                "0000000002", "                              ", "+0000005.00");
        custProc.process(rec);
        assertEquals("INVALID   ", rec.getStatus());
        assertEquals("RETAIL    ", rec.getSegment());
    }

    @Test
    void negativeBalanceIsInvalid() {
        CustomerRecord rec = new CustomerRecord(
                "0000000003", "BOB                           ", "-0000001.00");
        custProc.process(rec);
        assertEquals("INVALID   ", rec.getStatus());
        assertEquals("RETAIL    ", rec.getSegment());
    }

    @Test
    void padRightExact() {
        assertEquals("VALID     ", CustProc.padRight("VALID", 10));
    }

    @Test
    void padRightTruncate() {
        assertEquals("ABCDE", CustProc.padRight("ABCDEFGH", 5));
    }

    @Test
    void padRightAlreadyCorrect() {
        assertEquals("RETAIL    ", CustProc.padRight("RETAIL    ", 10));
    }
}
