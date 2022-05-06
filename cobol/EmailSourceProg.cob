IDENTIFICATION DIVISION.
 PROGRAM-ID.    SPNames
 AUTHOR.        KODE LEGACY.
 DATE-WRITTEN.  01ST JULY 2013.
 ENVIRONMENT DIVISION.
 CONFIGURATION SECTION.
    SOURCE-COMPUTER. IBM-370.
    SPECIAL-NAMES.
    CLASS WS-VALID-EMAIL IS
          '@' '_' '.'
          'a' THRU 'i'
          'j' THRU 'r'
          's' THRU 'z'
          '0' THRU '9'.
 DATA DIVISION.
 WORKING-STORAGE SECTION.
    01  WS-EMAIL        PIC X(50).
    01  WS-SPACE-CNT    PIC X(2) VALUE ZEROES.
 PROCEDURE DIVISION.
    MOVE "abc_def@xyz.com"    TO WS-EMAIL.
******** Below INSPECT statement counts characters before initial space to exclude all Spaces
******** in 50 byte Email field as Space is not a valid character. Below reference
******** modification does it by treating (count - 1) as the length of the actual Email.
    INSPECT WS-EMAIL TALLYING WS-SPACE-CNT FOR CHARACTERS BEFORE INITIAL SPACE.
 
******** WS-VALID-EMAIL is the class declared above for valid characters.
    IF WS-EMAIL(1:(WS-SPACE-CNT-1)) IS NOT WS-VALID-EMAIL
       DISPLAY "EMAIL ADDRESS IS INVALID."
    ELSE
       DISPLAY "EMAIL ADDRESS IS VALID."
    END-IF.
 
    STOP RUN.
