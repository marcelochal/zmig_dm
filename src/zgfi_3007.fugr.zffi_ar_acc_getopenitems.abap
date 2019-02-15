FUNCTION ZFFI_AR_ACC_GETOPENITEMS.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(COMPANYCODE) LIKE  BAPI3007_1-COMP_CODE
*"     VALUE(CUSTOMER) LIKE  BAPI3007_1-CUSTOMER
*"     VALUE(KEYDATE) LIKE  BAPI3007-KEY_DATE
*"     VALUE(NOTEDITEMS) LIKE  BAPI3007-NTDITMS_RQ DEFAULT SPACE
*"     VALUE(SECINDEX) LIKE  BAPI3007-SINDEX_RQ DEFAULT SPACE
*"  EXPORTING
*"     VALUE(RETURN) LIKE  BAPIRETURN STRUCTURE  BAPIRETURN
*"  TABLES
*"      LINEITEMS STRUCTURE  ZEFI_BAPI3007_2
*"----------------------------------------------------------------------
  CLEAR:
    return,
    t001,
    lineitems,
    xlineitems,
    zlineitems,
    ulineitems,
    bstat.

  IMPORT mem_no_ext_conv FROM MEMORY ID 'FSSC_NO_EXT_CONV'. "n1968346

  idxsel_x = secindex.

  bstat-low    = space.
  bstat-option = 'EQ'.
  bstat-sign   = 'I'.
  APPEND bstat.
  IF NOT noteditems IS INITIAL.
    bstat-low    = 'S'.
    bstat-option = 'EQ'.
    bstat-sign   = 'I'.
    APPEND bstat.
  ENDIF.

  PERFORM read_t001 USING companycode return.
  CHECK return IS INITIAL.

  PERFORM read_konto USING  customer
                            companycode return.
  CHECK return IS INITIAL.

  SELECT * FROM bsid APPENDING CORRESPONDING FIELDS OF TABLE xlineitems
    WHERE budat <= keydate
    AND   bukrs EQ companycode
    AND   kunnr EQ customer
    AND   bstat IN bstat.

  SELECT * FROM bsad APPENDING CORRESPONDING FIELDS OF TABLE xlineitems
    WHERE budat <= keydate
    AND   augdt >  keydate
    AND   bukrs EQ companycode
    AND   kunnr EQ customer
    AND   bstat IN bstat.

  PERFORM read_beleg_data TABLES xlineitems ulineitems
                          USING  return char_x idxsel_x.
  CHECK return IS INITIAL.

*  zlineitems[] = ulineitems[].                              "#EC ENHOK
*  LOOP AT zlineitems.
*    MOVE-CORRESPONDING zlineitems TO lineitems.
*    APPEND lineitems.
*  ENDLOOP.

  APPEND LINES OF ulineitems TO lineitems.

  DESCRIBE TABLE lineitems LINES linecount.
  IF linecount = 0.
    CLEAR message.
*   MESSAGE = 'IFN517'.
    message-msgty = 'I'.
    message-msgid = 'FN'.
    message-msgno = '517'.
    message-msgv1 = customer.
    message-msgv2 = companycode.
    PERFORM set_return_message USING    message
                               CHANGING return.
    IF 1 = 2.            " Für Verwendungsnachweis Message
      MESSAGE i517(fn) WITH customer companycode.
    ENDIF.
  ENDIF.
ENDFUNCTION.
