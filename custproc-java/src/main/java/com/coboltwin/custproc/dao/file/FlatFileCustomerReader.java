package com.coboltwin.custproc.dao.file;

import com.coboltwin.custproc.dao.CustomerReader;
import com.coboltwin.custproc.model.CustomerRecord;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Reads fixed-width records: CUST-ID(10) + NAME(30) + BALANCE(10) = 50 chars.
 *
 * Balance normalization mirrors COBOL PIC S9(7)V99 DISPLAY SIGN LEADING SEPARATE:
 *   Input  "0000012345" → sign position '0' (positive) + 9 digits "000012345"
 *   Output "+0000123.45" (sign + 7 integer digits + '.' + 2 decimal digits)
 */
public class FlatFileCustomerReader implements CustomerReader {

    private final BufferedReader reader;

    public FlatFileCustomerReader(Path path) throws IOException {
        this.reader = Files.newBufferedReader(path);
    }

    @Override
    public Iterator<CustomerRecord> iterator() {
        return new Iterator<>() {
            private String nextLine = readLine();

            private String readLine() {
                try {
                    return reader.readLine();
                } catch (IOException e) {
                    throw new UncheckedIOException(e);
                }
            }

            @Override
            public boolean hasNext() {
                return nextLine != null;
            }

            @Override
            public CustomerRecord next() {
                if (nextLine == null) throw new NoSuchElementException();
                String line = nextLine;
                nextLine = readLine();
                return parseLine(line);
            }
        };
    }

    private static CustomerRecord parseLine(String line) {
        String custId  = line.substring(0, 10);
        String name    = line.substring(10, 40);
        String rawBal  = line.substring(40, 50);
        String balance = normalizeBalance(rawBal);
        return new CustomerRecord(custId, name, balance);
    }

    /**
     * Normalizes a 10-char flat-file balance to COBOL DISPLAY format (11 chars).
     *
     * COBOL PIC S9(7)V99 DISPLAY SIGN LEADING SEPARATE stores 10 bytes:
     *   byte 0 = sign character ('+', '-', or digit if unsigned)
     *   bytes 1-9 = 9 digits (7 integer + 2 decimal)
     *
     * Output format: sign + 7 digits + '.' + 2 digits = 11 chars.
     */
    static String normalizeBalance(String raw) {
        char first = raw.charAt(0);
        String sign;
        String digits;
        if (first == '+' || first == '-') {
            sign = String.valueOf(first);
            digits = raw.substring(1);
        } else {
            sign = "+";
            digits = raw.substring(1);
        }
        return sign + digits.substring(0, 7) + "." + digits.substring(7, 9);
    }

    @Override
    public void close() throws IOException {
        reader.close();
    }
}
