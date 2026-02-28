package com.coboltwin.custproc.logic;

import com.coboltwin.custproc.dao.CustomerReader;
import com.coboltwin.custproc.dao.CustomerWriter;
import com.coboltwin.custproc.dao.DliService;
import com.coboltwin.custproc.model.CustomerRecord;

/**
 * Business logic mirroring CUSTPROC.cbl.
 *
 * Rules:
 *   1. If name is blank → status = INVALID
 *   2. Else if balance < 0 → status = INVALID
 *   3. Else → status = VALID
 *   4. Enrich with DL/I segment
 */
public class CustProc {

    private final DliService dliService;

    public CustProc(DliService dliService) {
        this.dliService = dliService;
    }

    public void process(CustomerRecord rec) {
        if (rec.getName().trim().isEmpty()) {
            rec.setStatus(padRight("INVALID", 10));
        } else if (rec.getBalance().startsWith("-")) {
            rec.setStatus(padRight("INVALID", 10));
        } else {
            rec.setStatus(padRight("VALID", 10));
        }

        String segment = dliService.getSegment(rec.getCustId());
        rec.setSegment(padRight(segment, 10));
    }

    public void run(CustomerReader reader, CustomerWriter writer) {
        for (CustomerRecord rec : reader) {
            process(rec);
            writer.write(rec);
        }
    }

    static String padRight(String s, int len) {
        if (s.length() >= len) return s.substring(0, len);
        return s + " ".repeat(len - s.length());
    }
}
