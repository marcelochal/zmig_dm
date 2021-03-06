*----------------------------------------------------------------------*
*   INCLUDE L3008F00                                                   *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
*       Form  AUTHORITYCHECK_F_BKPF_BEK
*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Kontenberechtigung Buchen                *
*----------------------------------------------------------------------*
*  -->  PR_BEGRU  Berechtigungsgruppe
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_BKPF_BEK USING PR_BEGRU
                                     PR_SET LIKE BAPIRETURN.
  CHECK NOT PR_BEGRU IS INITIAL.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BEK'
           ID 'BRGRU' FIELD PR_BEGRU
           ID 'ACTVT' FIELD AUTH-ACTVT.
  CHECK SY-SUBRC NE 0.
  CLEAR MESSAGE.
* MESSAGE = 'EFN527'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '527'.
  PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                             CHANGING PR_SET.
  IF 1 = 2.            " Für Verwendungsnachweis Message
   MESSAGE E527(FN).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  AUTHORITYCHECK_F_BKPF_BUK
*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Bukrs-Berechtigung Buchen                *
*----------------------------------------------------------------------*
*  -->  PR_BUKRS  Buchungskreis
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_BKPF_BUK USING PR_BUKRS
                                     PR_SET LIKE BAPIRETURN.
  AUTHORITY-CHECK OBJECT 'F_BKPF_BUK'
           ID 'BUKRS' FIELD PR_BUKRS
           ID 'ACTVT' FIELD AUTH-ACTVT.
  CHECK SY-SUBRC NE 0.
  CLEAR MESSAGE.
* MESSAGE = 'EFN526'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '526'.
  MESSAGE-MSGV1 = PR_BUKRS.
  PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                             CHANGING PR_SET.
  IF 1 = 2.            " Für Verwendungsnachweis Message
   MESSAGE E526(FN) WITH PR_BUKRS.
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  AUTHORITYCHECK_F_BKPF_KOA
*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Kontoartberechtigung Buchen              *
*----------------------------------------------------------------------*
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_BKPF_KOA USING PR_SET LIKE BAPIRETURN.
  AUTHORITY-CHECK OBJECT 'F_BKPF_KOA'
           ID 'KOART' FIELD 'K'
           ID 'ACTVT' FIELD AUTH-ACTVT.
  CHECK SY-SUBRC NE 0.
  CLEAR MESSAGE.
* MESSAGE = 'EFN525'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '525'.
  PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                             CHANGING PR_SET.
  IF 1 = 2.            " Für Verwendungsnachweis Message
   MESSAGE E525(FN).
  ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  AUTHORITYCHECK_F_LFA1_APP
*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Anzeige Kreditor / Applikatiion F        *
*----------------------------------------------------------------------*
*  -->  PR_APPLK  Anwendung
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_LFA1_APP USING PR_APPKZ PR_TOSET PR_SET.
  AUTHORITY-CHECK OBJECT 'F_LFA1_APP'
           ID 'ACTVT' FIELD AUTH-ACTVT
           ID 'APPKZ' FIELD PR_APPKZ.
  CHECK SY-SUBRC NE 0.
  PR_SET = PR_TOSET.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  AUTHORITYCHECK_F_LFA1_BEK
*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Berechtigungsgruppe                      *
*----------------------------------------------------------------------*
*  -->  PR_BEGRU  Berechtigungsgruppe
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_LFA1_BEK USING PR_BEGRU PR_TOSET PR_SET.
  CHECK NOT PR_BEGRU IS INITIAL.
  AUTHORITY-CHECK OBJECT 'F_LFA1_BEK'
           ID 'BRGRU' FIELD PR_BEGRU
           ID 'ACTVT' FIELD AUTH-ACTVT.
  CHECK SY-SUBRC NE 0.
  PR_SET = PR_TOSET.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  AUTHORITYCHECK_F_LFA1_BUK
*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Anzeige Kreditor im Buchungskreis        *
*----------------------------------------------------------------------*
*  -->  PR_BUKRS  Buchungskreis
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_LFA1_BUK USING PR_BUKRS PR_TOSET PR_SET.
  AUTHORITY-CHECK OBJECT 'F_LFA1_BUK'
           ID 'BUKRS' FIELD PR_BUKRS
           ID 'ACTVT' FIELD AUTH-ACTVT.
  CHECK SY-SUBRC NE 0.
  PR_SET = PR_TOSET.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  AUTHORITYCHECK_F_LFA1_GEN
*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Allgemeine Daten                         *
*----------------------------------------------------------------------*
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_LFA1_GEN USING PR_TOSET PR_SET.
  AUTHORITY-CHECK OBJECT 'F_LFA1_GEN'
           ID 'ACTVT' FIELD AUTH-ACTVT.
  CHECK SY-SUBRC NE 0.
  PR_SET = PR_TOSET.
ENDFORM.

*----------------------------------------------------------------------*
*       Berechtigungsprüfung: Kontengruppe                             *
*----------------------------------------------------------------------*
*  -->  PR_KTOKK  Kontengruppe
*  -->  PR_TOSET  Zu setzender Returncode
*  <--  PR_SET    Gesetzter Returncode
*----------------------------------------------------------------------*
FORM AUTHORITYCHECK_F_LFA1_GRP USING PR_KTOKK PR_TOSET PR_SET.
  CHECK NOT PR_KTOKK IS INITIAL.
  AUTHORITY-CHECK OBJECT 'F_LFA1_GRP'
           ID 'KTOKK' FIELD PR_KTOKK
           ID 'ACTVT' FIELD AUTH-ACTVT.
  CHECK SY-SUBRC NE 0.
  PR_SET = PR_TOSET.
ENDFORM.

*----------------------------------------------------------------------*
*       Form  CONV_TO_EXT
*----------------------------------------------------------------------*
*       Konvertieren eines Betragsfeldes intern -> extern              *
*----------------------------------------------------------------------*
*  -->  PR_INTAMOUNT: Betrag intern
*  -->  PR_WAERS    : Währung
*  <--  PR_EXTAMOUNT: Betrag extern
*----------------------------------------------------------------------*
FORM CONV_TO_EXT USING    PR_INTAMOUNT PR_WAERS
                 CHANGING PR_EXTAMOUNT.
  CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
       EXPORTING
            CURRENCY        = PR_WAERS
            AMOUNT_INTERNAL = PR_INTAMOUNT
       IMPORTING
            AMOUNT_EXTERNAL = PR_EXTAMOUNT
       EXCEPTIONS
            OTHERS          = 1.
* CALL FUNCTION 'CURRENCY_AMOUNT_SAP_TO_IDOC'
*      EXPORTING
*           CURRENCY    = PR_WAERS
*           SAP_AMOUNT  = PR_INTAMOUNT
*      IMPORTING
*           IDOC_AMOUNT = PR_EXTAMOUNT
*      EXCEPTIONS
*           OTHERS      = 1.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM READ_BELEG_DATA                                           *
*----------------------------------------------------------------------*
*       Lesen Belegdaten                                               *
*----------------------------------------------------------------------*
*  -->  YRETURN YLINEITEMS data.                                       *
*  <--  YRETURN YLINEITEMS                                             *
*----------------------------------------------------------------------*
FORM READ_BELEG_DATA TABLES YLINEITEMS STRUCTURE XLINEITEMS
                            WLINEITEMS STRUCTURE ULINEITEMS
                     USING YRETURN LIKE BAPIRETURN
                           DATA.
  clear    wlineitems.
  refresh  wlineitems.
  LOOP AT YLINEITEMS.
    clear wlineitems.
    move-corresponding ylineitems to wlineitems.
    CLEAR BSEG. CLEAR BSEC. CLEAR BSED. CLEAR BKPF.
    WLINEITEMS-HWAER = T001-WAERS.
    PERFORM READ_BKPF USING WLINEITEMS-BUKRS WLINEITEMS-BELNR
                            WLINEITEMS-GJAHR.
    if sy-subrc = 0.
    WLINEITEMS-AWTYP = BKPF-AWTYP.
    WLINEITEMS-AWREF = BKPF-AWKEY(10).
    WLINEITEMS-AWORG = BKPF-AWKEY+10(10).
    WLINEITEMS-stblg = BKPF-stblg.

    CALL FUNCTION 'REF_DOC_NO_CONVERSION_OUTBOUND'
      EXPORTING
        I_REF_DOC_NO_LONG       = bkpf-xblnr
      IMPORTING
        E_REF_DOC_NO            = WLINEITEMS-xblnr
        E_REF_DOC_NO_LONG       = WLINEITEMS-xblnr_long.

*   MODIFY WLINEITEMS.
    IF NOT WLINEITEMS-XZAHL IS INITIAL
    OR     WLINEITEMS-PSWSL IS INITIAL.
      PERFORM READ_BSEG USING WLINEITEMS-BUKRS WLINEITEMS-BELNR
                              WLINEITEMS-GJAHR WLINEITEMS-BUZEI.
      IF SY-SUBRC NE 0.
        CLEAR MESSAGE.
*       MESSAGE = 'EFN512'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '512'.
        PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                                   CHANGING YRETURN.
        IF 1 = 2.            " Für Verwendungsnachweis Message
         MESSAGE E512(FN).
        ENDIF.
      ELSE.
        MOVE-CORRESPONDING BSEG TO WLINEITEMS.
*       MODIFY WLINEITEMS.
      ENDIF.
    ENDIF.
    IF NOT WLINEITEMS-XCPDD IS INITIAL.
      IF NOT DATA IS INITIAL.
        PERFORM READ_BSEC USING WLINEITEMS-BUKRS WLINEITEMS-BELNR
                                WLINEITEMS-GJAHR WLINEITEMS-BUZEI.
        IF SY-SUBRC NE 0.
          CLEAR MESSAGE.
*         MESSAGE = 'EFN513'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '513'.
          PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                                     CHANGING YRETURN.
          IF 1 = 2.            " Für Verwendungsnachweis Message
           MESSAGE E513(FN).
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING BSEC TO WLINEITEMS.
*         MODIFY YLINEITEMS.
        ENDIF.
      ENDIF.
    ENDIF.
    IF WLINEITEMS-UMSKS = 'W'.
      IF NOT DATA IS INITIAL.
        PERFORM READ_BSED USING WLINEITEMS-BUKRS WLINEITEMS-BELNR
                                WLINEITEMS-GJAHR WLINEITEMS-BUZEI.
        IF SY-SUBRC NE 0.
          CLEAR MESSAGE.
*         MESSAGE = 'EFN514'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '514'.
          PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                                     CHANGING YRETURN.
          IF 1 = 2.            " Für Verwendungsnachweis Message
           MESSAGE E514(FN).
          ENDIF.
        ELSE.
          MOVE-CORRESPONDING BSED TO WLINEITEMS.
*         MODIFY WLINEITEMS.
        ENDIF.
      ENDIF.
    ENDIF.
   ENDIF.

    PERFORM CONV_TO_EXT USING WLINEITEMS-DMBTR WLINEITEMS-HWAER
                              WLINEITEMS-DMBTR.
    PERFORM CONV_TO_EXT USING WLINEITEMS-WRBTR WLINEITEMS-WAERS
                              WLINEITEMS-WRBTR.
    PERFORM CONV_TO_EXT USING WLINEITEMS-MWSTS WLINEITEMS-HWAER
                              WLINEITEMS-MWSTS.
    PERFORM CONV_TO_EXT USING WLINEITEMS-WMWST WLINEITEMS-WAERS
                              WLINEITEMS-WMWST.
    PERFORM CONV_TO_EXT USING WLINEITEMS-SKFBT WLINEITEMS-WAERS
                              WLINEITEMS-SKFBT.
    PERFORM CONV_TO_EXT USING WLINEITEMS-SKNTO WLINEITEMS-HWAER
                              WLINEITEMS-SKNTO.
    PERFORM CONV_TO_EXT USING WLINEITEMS-WSKTO WLINEITEMS-WAERS
                              WLINEITEMS-WSKTO.
    PERFORM CONV_TO_EXT USING WLINEITEMS-PSWBT WLINEITEMS-PSWSL
                              WLINEITEMS-PSWBT.
    PERFORM CONV_TO_EXT USING WLINEITEMS-NEBTR WLINEITEMS-WAERS
                              WLINEITEMS-NEBTR.
    PERFORM CONV_TO_EXT USING WLINEITEMS-WSTHW WLINEITEMS-HWAER
                              WLINEITEMS-WSTHW.
    PERFORM CONV_TO_EXT USING WLINEITEMS-WSTFW WLINEITEMS-WAERS
                              WLINEITEMS-WSTFW.
    PERFORM CONV_TO_EXT USING WLINEITEMS-WINHW WLINEITEMS-HWAER
                              WLINEITEMS-WINHW.
    PERFORM CONV_TO_EXT USING WLINEITEMS-WINFW WLINEITEMS-WAERS
                              WLINEITEMS-WINFW.
    PERFORM CONV_TO_EXT USING WLINEITEMS-QSSHB WLINEITEMS-WAERS
                              WLINEITEMS-QSSHB.
    PERFORM CONV_TO_EXT USING WLINEITEMS-QBSHB WLINEITEMS-WAERS
                              WLINEITEMS-QBSHB.
    PERFORM CONV_TO_EXT USING WLINEITEMS-QSFBT WLINEITEMS-WAERS
                              WLINEITEMS-QSFBT.
*   MODIFY WLINEITEMS.
    APPEND WLINEITEMS.
  ENDLOOP.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM READ_BKPF                                                 *
*----------------------------------------------------------------------*
*       Lesen Belegkopf                                                *
*----------------------------------------------------------------------*
*  -->  BUKRS BELNR GJAHR                                              *
*  <--  BKPF                                                           *
*----------------------------------------------------------------------*
FORM READ_BKPF USING BUKRS BELNR GJAHR.

  SELECT SINGLE * FROM  BKPF
         WHERE  BUKRS       = BUKRS
         AND    BELNR       = BELNR
         AND    GJAHR       = GJAHR.


ENDFORM.

*----------------------------------------------------------------------*
*       FORM READ_BSEC                                                 *
*----------------------------------------------------------------------*
*       Lesen CpD Daten                                                *
*----------------------------------------------------------------------*
*  -->  BUKRS BELNR GJAHR BUZEI                                        *
*  <--  BSEC                                                           *
*----------------------------------------------------------------------*
FORM READ_BSEC USING BUKRS BELNR GJAHR BUZEI.

  SELECT SINGLE * FROM  BSEC
         WHERE  BUKRS       = BUKRS
         AND    BELNR       = BELNR
         AND    GJAHR       = GJAHR
         AND    BUZEI       = BUZEI.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM READ_BSED                                                 *
*----------------------------------------------------------------------*
*       Lesen Wechseldaten                                             *
*----------------------------------------------------------------------*
*  -->  BUKRS BELNR GJAHR BUZEI                                        *
*  <--  BSED                                                           *
*----------------------------------------------------------------------*
FORM READ_BSED USING BUKRS BELNR GJAHR BUZEI.

  SELECT SINGLE * FROM  BSED
         WHERE  BUKRS       = BUKRS
         AND    BELNR       = BELNR
         AND    GJAHR       = GJAHR
         AND    BUZEI       = BUZEI.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM READ_BSEG                                                 *
*----------------------------------------------------------------------*
*       Lesen Belegzeile                                               *
*----------------------------------------------------------------------*
*  -->  BUKRS BELNR GJAHR BUZEI                                        *
*  <--  BSEG                                                           *
*----------------------------------------------------------------------*
FORM READ_BSEG USING BUKRS BELNR GJAHR BUZEI.

  SELECT SINGLE * FROM  BSEG
         WHERE  BUKRS       = BUKRS
         AND    BELNR       = BELNR
         AND    GJAHR       = GJAHR
         AND    BUZEI       = BUZEI.

ENDFORM.

*----------------------------------------------------------------------*
*       FORM READ_KONTO                                                *
*----------------------------------------------------------------------*
*       Lesen Kontoinformationen                                       *
*----------------------------------------------------------------------*
*  -->  Konto BUKRS YRETURN                                            *
*  <--  LFA1 LFB1                                                      *
*----------------------------------------------------------------------*
FORM READ_KONTO USING KONTO BUKRS YRETURN.
  SELECT SINGLE * FROM  LFA1
         WHERE  LIFNR       = KONTO.
         IF SY-SUBRC NE 0.
           CLEAR MESSAGE.
*          MESSAGE = 'EFN521'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '521'.
           MESSAGE-MSGV1 = KONTO.
           PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                                      CHANGING YRETURN.
           IF 1 = 2.            " Für Verwendungsnachweis Message
            MESSAGE E521(FN) WITH KONTO.
           ENDIF.
         ELSE.
           SELECT SINGLE * FROM  LFB1
                  WHERE  BUKRS       = BUKRS
                  AND    LIFNR       = KONTO.
                  IF SY-SUBRC NE 0.
                    CLEAR MESSAGE.
*                   MESSAGE = 'EFN522'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '522'.
                    MESSAGE-MSGV1 = KONTO.
                    MESSAGE-MSGV2 = BUKRS.
                    PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                                               CHANGING YRETURN.
                    IF 1 = 2.            " Für Verwendungsnachweis Msg
                     MESSAGE E522(FN) WITH KONTO BUKRS.
                    ENDIF.
                  ELSE.
                    PERFORM AUTHORITYCHECK_F_BKPF_KOA
                                  USING YRETURN.
*                                 USING '95'     YRETURN.
                    PERFORM AUTHORITYCHECK_F_BKPF_BUK
                                  USING LFB1-BUKRS YRETURN.
*                                 USING LFB1-BUKRS '96'     YRETURN.
                    PERFORM AUTHORITYCHECK_F_BKPF_BEK
                                  USING LFA1-BEGRU YRETURN.
*                                 USING LFA1-BEGRU '97'     YRETURN.
                    PERFORM AUTHORITYCHECK_F_BKPF_BEK
                                  USING LFB1-BEGRU YRETURN.
*                                 USING LFB1-BEGRU '97'     YRETURN.
                  ENDIF.
         ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM READ_T001                                                 *
*----------------------------------------------------------------------*
*       Lesen Buchungskreisdaten                                      *
*----------------------------------------------------------------------*
*  -->  BUKRS YRETURN                                                  *
*  <--  T001  YRETURN                                                  *
*----------------------------------------------------------------------*
FORM READ_T001 USING BUKRS
                     YRETURN LIKE BAPIRETURN.
  CALL FUNCTION 'FI_COMPANY_CODE_DATA'
       EXPORTING
            I_BUKRS       = BUKRS
       IMPORTING
            E_T001        = T001
       EXCEPTIONS
            ERROR_MESSAGE = 1.
    IF SY-SUBRC NE 0.
      CLEAR MESSAGE.
*     MESSAGE = 'EFN510'.
  MESSAGE-MSGTY = 'E'.
  MESSAGE-MSGID = 'FN'.
  MESSAGE-MSGNO = '510'.
      MESSAGE-MSGV1 = BUKRS.
      PERFORM SET_RETURN_MESSAGE USING    MESSAGE
                                 CHANGING YRETURN.
      IF 1 = 2.            " Für Verwendungsnachweis Message
       MESSAGE E510(FN) WITH BUKRS.
      ENDIF.
    ENDIF.
ENDFORM.

*----------------------------------------------------------------------*
*       FORM SET_RETURN_MESSAGE                                        *
*----------------------------------------------------------------------*
*       set return parameter for output
*----------------------------------------------------------------------*
*      -->P_MESSAGE    messageid                                       *
*      <--P_RETURN     RETURN parameter                                *
*----------------------------------------------------------------------*
FORM SET_RETURN_MESSAGE USING    VALUE(P_MESSAGE) LIKE MESSAGE
                        CHANGING P_RETURN         LIKE BAPIRETURN.

  CHECK NOT MESSAGE IS INITIAL.

  CALL FUNCTION 'BALW_BAPIRETURN_GET'
       EXPORTING
            TYPE       = P_MESSAGE-MSGTY
            CL         = P_MESSAGE-MSGID
            NUMBER     = P_MESSAGE-MSGNO
            PAR1       = P_MESSAGE-MSGV1
            PAR2       = P_MESSAGE-MSGV2
            PAR3       = P_MESSAGE-MSGV3
            PAR4       = P_MESSAGE-MSGV4
*          LOG_NO     = ' '
*          LOG_MSG_NO = ' '
       IMPORTING
            BAPIRETURN = P_RETURN
       EXCEPTIONS
            OTHERS     = 1.

ENDFORM.                               " SET_RETURN_MESSAGE
