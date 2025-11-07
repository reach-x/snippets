       IDENTIFICATION DIVISION.
       PROGRAM-ID. BASIC-OPERATIONS.
       AUTHOR. CLAUDE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1                 PIC 9(3) VALUE 10.
       01 NUM2                 PIC 9(3) VALUE 20.
       01 RESULT               PIC 9(4).
       01 NAME-VAR             PIC X(20) VALUE 'ALICE'.
       01 AGE-VAR              PIC 99 VALUE 30.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY ' '.
           DISPLAY '=== BASIC OPERATIONS IN COBOL ==='.
           DISPLAY ' '.

           DISPLAY 'NUM1: ' NUM1.
           DISPLAY 'NUM2: ' NUM2.

           ADD NUM1 TO NUM2 GIVING RESULT.
           DISPLAY ' '.
           DISPLAY 'ADD: ' RESULT.

           SUBTRACT NUM1 FROM NUM2 GIVING RESULT.
           DISPLAY 'SUBTRACT: ' RESULT.

           MULTIPLY NUM1 BY NUM2 GIVING RESULT.
           DISPLAY 'MULTIPLY: ' RESULT.

           DIVIDE NUM2 BY NUM1 GIVING RESULT.
           DISPLAY 'DIVIDE: ' RESULT.

           DISPLAY ' '.
           DISPLAY 'NAME: ' NAME-VAR.
           DISPLAY 'AGE: ' AGE-VAR.

           STOP RUN.
