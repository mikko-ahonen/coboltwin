package com.coboltwin.custproc;

import com.coboltwin.custproc.dao.file.FlatFileCustomerReader;
import com.coboltwin.custproc.dao.file.JsonLinesCustomerWriter;
import com.coboltwin.custproc.dao.file.StubDliService;
import com.coboltwin.custproc.logic.CustProc;

import java.nio.file.Path;

/**
 * Twin-test entry point: reads db2_in.dat, processes with CustProc,
 * emits JSON Lines to stdout -- identical to COBOL TEST-WRAPPER output.
 */
public class TwinMain {
    public static void main(String[] args) throws Exception {
        Path input = Path.of("db2_in.dat");
        try (var reader = new FlatFileCustomerReader(input);
             var writer = new JsonLinesCustomerWriter(System.out)) {
            CustProc proc = new CustProc(new StubDliService());
            proc.run(reader, writer);
        }
    }
}
