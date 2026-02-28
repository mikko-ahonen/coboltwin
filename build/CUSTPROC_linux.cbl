
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROC.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DB2-IN-FILE ASSIGN TO 'db2_in.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT DB2-OUT-FILE ASSIGN TO 'db2_out.dat'
               ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.

       * --- Auto-generated DB2 mock files ---
       FD  DB2-IN-FILE.
       01  DB2-IN-REC.
           05  IN-WS-CUST-ID PIC X(10).
           05  IN-WS-NAME PIC X(30).
           05  IN-WS-BALANCE PIC S9(7)V99 DISPLAY.
           05  IN-WS-STATUS PIC X(10).
           05  IN-WS-SEGMENT PIC X(10).
           05  IN-WS-CUST-ID PIC X(10).

       FD  DB2-OUT-FILE.
       01  DB2-OUT-REC.
           05  OUT-WS-CUST-ID PIC X(10).
           05  OUT-WS-NAME PIC X(30).
           05  OUT-WS-BALANCE PIC S9(7)V99 DISPLAY.
           05  OUT-WS-STATUS PIC X(10).
           05  OUT-WS-SEGMENT PIC X(10).
       * --- End auto-generated files ---

       WORKING-STORAGE SECTION.
       01  SQLCODE PIC S9(9) COMP VALUE 0.
       01  WS-EOF  PIC X VALUE 'N'.
           88  EOF VALUE 'Y'.
       01 WS-CUST-ID   PIC X(10).
       01 WS-NAME      PIC X(30).
       01 WS-BALANCE   PIC S9(7)V99 DISPLAY.
       01 WS-STATUS    PIC X(10).
       01 WS-SEGMENT   PIC X(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

           CONTINUE.

           PERFORM DB2-OPEN.

           PERFORM UNTIL SQLCODE = 100

           PERFORM DB2-FETCH.

           STOP RUN.

       JSON-EMIT.
           DISPLAY '{' WITH NO ADVANCING.
           DISPLAY '"ws_cust_id":"' WITH NO ADVANCING.
           DISPLAY WS-CUST-ID WITH NO ADVANCING.
           DISPLAY '",' WITH NO ADVANCING.
           DISPLAY '"ws_name":"' WITH NO ADVANCING.
           DISPLAY WS-NAME WITH NO ADVANCING.
           DISPLAY '",' WITH NO ADVANCING.
           DISPLAY '"ws_balance":"' WITH NO ADVANCING.
           DISPLAY WS-BALANCE WITH NO ADVANCING.
           DISPLAY '",' WITH NO ADVANCING.
           DISPLAY '"ws_status":"' WITH NO ADVANCING.
           DISPLAY WS-STATUS WITH NO ADVANCING.
           DISPLAY '",' WITH NO ADVANCING.
           DISPLAY '"ws_segment":"' WITH NO ADVANCING.
           DISPLAY WS-SEGMENT WITH NO ADVANCING.
           DISPLAY '",' WITH NO ADVANCING.
           DISPLAY '"ws_cust_id":"' WITH NO ADVANCING.
           DISPLAY WS-CUST-ID WITH NO ADVANCING.
           DISPLAY '",' WITH NO ADVANCING.
           DISPLAY '"ws_status":"' WITH NO ADVANCING.
           DISPLAY WS-STATUS WITH NO ADVANCING.
           DISPLAY '",' WITH NO ADVANCING.
           DISPLAY '"ws_segment":"' WITH NO ADVANCING.
           DISPLAY WS-SEGMENT WITH NO ADVANCING.
           DISPLAY '"' WITH NO ADVANCING.
           DISPLAY '}' .
           EXIT.

       * --- Auto-generated DB2 stub paragraphs ---
       DB2-OPEN.
           OPEN INPUT DB2-IN-FILE.
           OPEN OUTPUT DB2-OUT-FILE.
           EXIT.

       DB2-FETCH.
           READ DB2-IN-FILE INTO DB2-IN-REC
               AT END
                   MOVE 100 TO SQLCODE
                   SET EOF TO TRUE
               NOT AT END
                   MOVE 0 TO SQLCODE
           MOVE IN-WS-CUST-ID TO WS-CUST-ID.
           MOVE IN-WS-NAME TO WS-NAME.
           MOVE IN-WS-BALANCE TO WS-BALANCE.
           MOVE IN-WS-STATUS TO WS-STATUS.
           MOVE IN-WS-SEGMENT TO WS-SEGMENT.
           MOVE IN-WS-CUST-ID TO WS-CUST-ID.
           END-READ.
           EXIT.

       DB2-UPDATE.
           MOVE WS-CUST-ID TO OUT-WS-CUST-ID.
           MOVE WS-NAME TO OUT-WS-NAME.
           MOVE WS-BALANCE TO OUT-WS-BALANCE.
           MOVE WS-STATUS TO OUT-WS-STATUS.
           MOVE WS-SEGMENT TO OUT-WS-SEGMENT.
           WRITE DB2-OUT-REC.
           EXIT.

       DB2-CLOSE.
           CLOSE DB2-IN-FILE.
           CLOSE DB2-OUT-FILE.
           EXIT.

       * --- End auto-generated DB2 stubs ---

