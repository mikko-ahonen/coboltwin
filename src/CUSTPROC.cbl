       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTPROC.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       EXEC SQL INCLUDE SQLCA END-EXEC.

       01 WS-CUST-ID   PIC X(10).
       01 WS-NAME      PIC X(30).
       01 WS-BALANCE   PIC S9(7)V99 COMP-3.
       01 WS-STATUS    PIC X(10).
       01 WS-SEGMENT   PIC X(10).

       PROCEDURE DIVISION.
       MAIN-LOGIC.

           EXEC SQL
              DECLARE C1 CURSOR FOR
              SELECT CUST_ID, NAME, BALANCE
              FROM CUSTOMER_DATA
           END-EXEC.

           EXEC SQL
              OPEN C1
           END-EXEC.

           PERFORM UNTIL SQLCODE = 100

              EXEC SQL
                 FETCH C1 INTO
                 :WS-CUST-ID,
                 :WS-NAME,
                 :WS-BALANCE
              END-EXEC

              IF WS-NAME = SPACES
                 MOVE 'INVALID' TO WS-STATUS
              ELSE
                 IF WS-BALANCE < 0
                    MOVE 'INVALID' TO WS-STATUS
                 ELSE
                    MOVE 'VALID' TO WS-STATUS
                 END-IF
              END-IF

      * Enrichment from DL/I
              CALL 'CBLTDLI' USING WS-SEGMENT

              EXEC SQL
                 UPDATE CUSTOMER_DATA
                 SET STATUS = :WS-STATUS,
                     SEGMENT = :WS-SEGMENT
                 WHERE CUST_ID = :WS-CUST-ID
              END-EXEC

           END-PERFORM.

           EXEC SQL
              CLOSE C1
           END-EXEC.

           STOP RUN.
