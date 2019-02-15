FUNCTION ZFFI_AP_ACC_GETOPENITEMS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(COMPANYCODE) LIKE  BAPI3008_1-COMP_CODE
*"     VALUE(VENDOR) LIKE  BAPI3008_1-VENDOR
*"     VALUE(KEYDATE) LIKE  BAPI3008-KEY_DATE
*"     VALUE(NOTEDITEMS) LIKE  BAPI3008-NTDITMS_RQ DEFAULT SPACE
*"  EXPORTING
*"     VALUE(RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"  TABLES
*"      LINEITEMS STRUCTURE  ZEFI_BAPI3008_2
*"----------------------------------------------------------------------
  CLEAR   RETURN.
  CLEAR   T001.
  CLEAR   LINEITEMS.
  REFRESH LINEITEMS.
  CLEAR   XLINEITEMS.
  REFRESH XLINEITEMS.
  CLEAR   ZLINEITEMS.
  REFRESH ZLINEITEMS.
  CLEAR   ULINEITEMS.
  REFRESH ULINEITEMS.
  CLEAR   BSTAT.
  REFRESH BSTAT.

  BSTAT-LOW    = SPACE.
  BSTAT-OPTION = 'EQ'.
  BSTAT-SIGN   = 'I'.
  APPEND BSTAT.
  IF NOT NOTEDITEMS IS INITIAL.
    BSTAT-LOW    = 'S'.
    BSTAT-OPTION = 'EQ'.
    BSTAT-SIGN   = 'I'.
    APPEND BSTAT.
  ENDIF.

  PERFORM READ_T001 USING COMPANYCODE RETURN.
  CHECK RETURN IS INITIAL.

  PERFORM READ_KONTO USING  VENDOR
                            COMPANYCODE RETURN.
  CHECK RETURN IS INITIAL.

  SELECT * FROM BSIK APPENDING CORRESPONDING FIELDS OF TABLE XLINEITEMS
    WHERE BUDAT <= KEYDATE
    AND   BUKRS EQ COMPANYCODE
    AND   LIFNR EQ VENDOR
    AND   BSTAT IN BSTAT.

  SELECT * FROM BSAK APPENDING CORRESPONDING FIELDS OF TABLE XLINEITEMS
    WHERE BUDAT <= KEYDATE
    AND   AUGDT >  KEYDATE
    AND   BUKRS EQ COMPANYCODE
    AND   LIFNR EQ VENDOR
    AND   BSTAT IN BSTAT.

  PERFORM READ_BELEG_DATA TABLES XLINEITEMS ULINEITEMS
                          USING  RETURN CHAR_X.
  CHECK RETURN IS INITIAL.

  ZLINEITEMS[] = ULINEITEMS[].
  LOOP AT ZLINEITEMS.
    MOVE-CORRESPONDING ZLINEITEMS TO LINEITEMS.
    APPEND LINEITEMS.
  ENDLOOP.

  DESCRIBE TABLE LINEITEMS LINES LINECOUNT.
  IF LINECOUNT = 0.
    CLEAR MESSAGE.
*   MESSAGE = 'IFN531'.
  MESSAGE-MSGTY = 'I'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '531'.
    MESSAGE-MSGV1 = VENDOR.
    MESSAGE-MSGV2 = COMPANYCODE.
    PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                               CHANGING RETURN.
    IF 1 = 2.            " Für Verwendungsnachweis Message
     MESSAGE I531(FN) WITH VENDOR   COMPANYCODE.
    ENDIF.
  ENDIF.
ENDFUNCTION.
