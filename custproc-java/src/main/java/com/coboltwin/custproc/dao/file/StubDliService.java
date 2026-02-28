package com.coboltwin.custproc.dao.file;

import com.coboltwin.custproc.dao.DliService;

/**
 * Returns a canned segment matching the COBOL DLI-STUB.cbl behavior.
 */
public class StubDliService implements DliService {

    @Override
    public String getSegment(String custId) {
        return "RETAIL";
    }
}
