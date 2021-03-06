FUNCTION-POOL ZGFI_3008.                    "MESSAGE-ID ..

TABLES:  LFA1,
         LFB1,
         LFC1,
         LFC3,
         LFBK.

TABLES:  T001,
         BKPF,
         BSEG,
         BSEC,
         BSED,
         BSIK,
         BSAK.

* BAPI3008   ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008,
           LIFNR      like LFA1-LIFNR           ,
           BUKRS      like LFB1-BUKRS           ,
           STIDA      LIKE BAPI3008-KEY_DATE ,
           DATU1      LIKE BAPI3008-FROM_DATE   ,
           DATU2      LIKE BAPI3008-TO_DATE     ,
           XSSHB      LIKE BAPI3008-NTDITMS_RQ  ,
           XSBEL      LIKE BAPI3008-BAL_SGLIND  ,
         END   OF HBAPI3008.

* BAPI3008_1 ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008_1,
           BUKRS      like LFB1-BUKRS           ,
           LIFNR      like LFA1-LIFNR           ,
         END   OF HBAPI3008_1.

* BAPI3008_2 ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008_2,
           BUKRS      like LFB1-BUKRS           ,
           LIFNR      like LFA1-LIFNR           ,
           UMSKZ      like BSIK-UMSKZ           ,
           AUGDT      like BSIK-AUGDT           ,
           AUGBL      like BSIK-AUGBL           ,
           ZUONR      like BSIK-ZUONR           ,
           GJAHR      like LFC3-GJAHR           ,
           BELNR      like BSIK-BELNR           ,
           BUZEI      like BSIK-BUZEI           ,
           BUDAT      like BSIK-BUDAT           ,
           BLDAT      like BSIK-BLDAT           ,
           CPUDT      like BSIK-CPUDT           ,
           WAERS      like BSIK-WAERS           ,
           HWAER      like BKPF-HWAER           ,
           XBLNR      like BSIK-XBLNR           ,
           BLART      like BSIK-BLART           ,
           MONAT      like BSIK-MONAT           ,
           BSCHL      like BSIK-BSCHL           ,
           SHKZG      like BSIK-SHKZG           ,
           GSBER      like BSIK-GSBER           ,
           MWSKZ      like BSIK-MWSKZ           ,
           DMBTR      LIKE BAPI3008_7-BALANCE   ,
           WRBTR      LIKE BAPI3008_7-BALANCE   ,
           MWSTS      LIKE BAPI3008_7-BALANCE   ,
           WMWST      LIKE BAPI3008_7-BALANCE   ,
           SGTXT      like BSIK-SGTXT           ,
           FILKD      like BSIK-FILKD           ,
           ZFBDT      like BSIK-ZFBDT           ,
           ZTERM      like LFB1-ZTERM           ,
           ZBD1T      like BSIK-ZBD1T           ,
           ZBD2T      like BSIK-ZBD2T           ,
           ZBD3T      like BSIK-ZBD3T           ,
           ZBD1P      like BSIK-ZBD1P           ,
           ZBD2P      like BSIK-ZBD2P           ,
           SKFBT      LIKE BAPI3008_7-BALANCE   ,
           SKNTO      LIKE BAPI3008_7-BALANCE   ,
           WSKTO      LIKE BAPI3008_7-BALANCE   ,
           ZLSCH      like BSIK-ZLSCH           ,
           ZLSPR      like BSIK-ZLSPR           ,
           ZBFIX      like BSIK-ZBFIX           ,
           REBZG      like BSIK-REBZG           ,
           REBZJ      like BSIK-REBZJ           ,
           REBZZ      like BSIK-REBZZ           ,
           MANSP      like BSIK-MANSP           ,
           MSCHL      like BSIK-MSCHL           ,
           MADAT      like BSIK-MADAT           ,
           MANST      like BSIK-MANST           ,
           MABER      like BSIK-MABER           ,
           QSSKZ      like LFB1-QSSKZ           ,
           QSSHB      LIKE BAPI3008_7-BALANCE   ,
           QBSHB      LIKE BAPI3008_7-BALANCE   ,
           BSTAT      like BSIK-BSTAT           ,
           REBZT      like BSIK-REBZT           ,
           STCEG      like LFA1-STCEG           ,
           QSZNR      like LFB1-QSZNR           ,
           QSFBT      LIKE BAPI3008_7-BALANCE   ,
           RSTGR      like BSIK-RSTGR           ,
           UZAWE      like LFB1-UZAWE           ,
           XREF1      like BSIK-XREF1           ,
           XREF2      like BSIK-XREF2           ,
           PSWSL      like BSIK-PSWSL           ,
           PSWBT      LIKE BAPI3008_7-BALANCE   ,
           NEBTR      LIKE BAPI3008_7-BALANCE   ,
           NAME1      like LFA1-NAME1           ,
           NAME2      like LFA1-NAME2           ,
           NAME3      like LFA1-NAME3           ,
           NAME4      like LFA1-NAME4           ,
           PSTLZ      like LFA1-PSTLZ           ,
           ORT01      like LFA1-ORT01           ,
           LAND1      like LFA1-LAND1           ,
           STRAS      like LFA1-STRAS           ,
           PFACH      like LFA1-PFACH           ,
           PSTL2      like LFA1-PSTL2           ,
           PSKTO      like BSEC-PSKTO           ,
           BANKN      like BSEC-BANKN           ,
           BANKL      like BSEC-BANKL           ,
           BANKS      like BSEC-BANKS           ,
           STCD1      like LFA1-STCD1           ,
           STCD2      like LFA1-STCD2           ,
           STKZU      like LFA1-STKZU           ,
           STKZA      like LFA1-STKZA           ,
           REGIO      like LFA1-REGIO           ,
           BKONT      like BSEC-BKONT           ,
           DTAWS      like LFA1-DTAWS           ,
           EMPFG      like BSEC-EMPFG           ,
           SPRAS      like LFA1-SPRAS           ,
           WELGF      like BSED-WELGF           ,
           WSTKZ      like BSED-WSTKZ           ,
           WSTHW      LIKE BAPI3008_7-BALANCE   ,
           WSTFW      LIKE BAPI3008_7-BALANCE   ,
           WINHW      LIKE BAPI3008_7-BALANCE   ,
           WINFW      LIKE BAPI3008_7-BALANCE   ,
           WMWKZ      like BSED-WMWKZ           ,
           WDATE      like BSED-WDATE           ,
           WVERD      like BSED-WVERD           ,
           WEVWV      like BSED-WEVWV           ,
           WBANK      like BSED-WBANK           ,
           WNAME      like BSED-WNAME           ,
           WLZBP      like BSED-WLZBP           ,
           WORT1      like BSED-WORT1           ,
           WBZOG      like BSED-WBZOG           ,
           WORT2      like BSED-WORT2           ,
           DISKT      like BSED-DISKT           ,
           DISKP      like BSED-DISKP           ,
           XAKTZ      like BSED-XAKTZ           ,
           WSTAT      like BSED-WSTAT           ,
           WGBKZ      like BSED-WGBKZ           ,
           XSIWE      like BSED-XSIWE           ,
           AWTYP      LIKE ACCIT-AWTYP          ,
           AWREF      LIKE ACCIT-AWREF          ,
           AWORG      LIKE ACCIT-AWORG          ,
           STBLG      LIKE Bkpf-stblg           ,
           umsks      LIKE bseg-umsks           ,
           xnegp      LIKE bseg-xnegp           ,
           saknr      like bseg-saknr           ,
           vbund      type vbund,
           bvtyp      type bvtyp,
           hbkid      type hbkid,
           bupla      type bupla,
           empfb      type empfb,
         END   OF HBAPI3008_2.

* BAPI3008_2 ---------------------------------------------------*
DATA:    BEGIN OF UBAPI3008_2,
           BUKRS      like LFB1-BUKRS           ,
           LIFNR      like LFA1-LIFNR           ,
           UMSKZ      like BSIK-UMSKZ           ,
           AUGDT      like BSIK-AUGDT           ,
           AUGBL      like BSIK-AUGBL           ,
           ZUONR      like BSIK-ZUONR           ,
           GJAHR      like LFC3-GJAHR           ,
           BELNR      like BSIK-BELNR           ,
           BUZEI      like BSIK-BUZEI           ,
           BUDAT      like BSIK-BUDAT           ,
           BLDAT      like BSIK-BLDAT           ,
           CPUDT      like BSIK-CPUDT           ,
           WAERS      like BSIK-WAERS           ,
           HWAER      like BKPF-HWAER           ,
           XBLNR      like BAPI3008_2-REF_DOC_NO,
           BLART      like BSIK-BLART           ,
           MONAT      like BSIK-MONAT           ,
           BSCHL      like BSIK-BSCHL           ,
           SHKZG      like BSIK-SHKZG           ,
           GSBER      like BSIK-GSBER           ,
           MWSKZ      like BSIK-MWSKZ           ,
           DMBTR      LIKE BAPI3008_7-BALANCE   ,
           WRBTR      LIKE BAPI3008_7-BALANCE   ,
           MWSTS      LIKE BAPI3008_7-BALANCE   ,
           WMWST      LIKE BAPI3008_7-BALANCE   ,
           SGTXT      like BSIK-SGTXT           ,
           FILKD      like BSIK-FILKD           ,
           ZFBDT      like BSIK-ZFBDT           ,
           ZTERM      like LFB1-ZTERM           ,
           ZBD1T      like BSIK-ZBD1T           ,
           ZBD2T      like BSIK-ZBD2T           ,
           ZBD3T      like BSIK-ZBD3T           ,
           ZBD1P      like BSIK-ZBD1P           ,
           ZBD2P      like BSIK-ZBD2P           ,
           SKFBT      LIKE BAPI3008_7-BALANCE   ,
           SKNTO      LIKE BAPI3008_7-BALANCE   ,
           WSKTO      LIKE BAPI3008_7-BALANCE   ,
           ZLSCH      like BSIK-ZLSCH           ,
           ZLSPR      like BSIK-ZLSPR           ,
           ZBFIX      like BSIK-ZBFIX           ,
           REBZG      like BSIK-REBZG           ,
           REBZJ      like BSIK-REBZJ           ,
           REBZZ      like BSIK-REBZZ           ,
           MANSP      like BSIK-MANSP           ,
           MSCHL      like BSIK-MSCHL           ,
           MADAT      like BSIK-MADAT           ,
           MANST      like BSIK-MANST           ,
           MABER      like BSIK-MABER           ,
           QSSKZ      like LFB1-QSSKZ           ,
           QSSHB      LIKE BAPI3008_7-BALANCE   ,
           QBSHB      LIKE BAPI3008_7-BALANCE   ,
           BSTAT      like BSIK-BSTAT           ,
           REBZT      like BSIK-REBZT           ,
           STCEG      like LFA1-STCEG           ,
           QSZNR      like LFB1-QSZNR           ,
           QSFBT      LIKE BAPI3008_7-BALANCE   ,
           RSTGR      like BSIK-RSTGR           ,
           UZAWE      like LFB1-UZAWE           ,
           XREF1      like BSIK-XREF1           ,
           XREF2      like BSIK-XREF2           ,
           PSWSL      like BSIK-PSWSL           ,
           PSWBT      LIKE BAPI3008_7-BALANCE   ,
           NEBTR      LIKE BAPI3008_7-BALANCE   ,
           NAME1      like LFA1-NAME1           ,
           NAME2      like LFA1-NAME2           ,
           NAME3      like LFA1-NAME3           ,
           NAME4      like LFA1-NAME4           ,
           PSTLZ      like LFA1-PSTLZ           ,
           ORT01      like LFA1-ORT01           ,
           LAND1      like LFA1-LAND1           ,
           STRAS      like LFA1-STRAS           ,
           PFACH      like LFA1-PFACH           ,
           PSTL2      like LFA1-PSTL2           ,
           PSKTO      like BSEC-PSKTO           ,
           BANKN      like BSEC-BANKN           ,
           BANKL      like BSEC-BANKL           ,
           BANKS      like BSEC-BANKS           ,
           STCD1      like LFA1-STCD1           ,
           STCD2      like LFA1-STCD2           ,
           STKZU      like LFA1-STKZU           ,
           STKZA      like LFA1-STKZA           ,
           REGIO      like LFA1-REGIO           ,
           BKONT      like BSEC-BKONT           ,
           DTAWS      like LFA1-DTAWS           ,
           EMPFG      like BSEC-EMPFG           ,
           SPRAS      like LFA1-SPRAS           ,
           WELGF      like BSED-WELGF           ,
           WSTKZ      like BSED-WSTKZ           ,
           WSTHW      LIKE BAPI3008_7-BALANCE   ,
           WSTFW      LIKE BAPI3008_7-BALANCE   ,
           WINHW      LIKE BAPI3008_7-BALANCE   ,
           WINFW      LIKE BAPI3008_7-BALANCE   ,
           WMWKZ      like BSED-WMWKZ           ,
           WDATE      like BSED-WDATE           ,
           WVERD      like BSED-WVERD           ,
           WEVWV      like BSED-WEVWV           ,
           WBANK      like BSED-WBANK           ,
           WNAME      like BSED-WNAME           ,
           WLZBP      like BSED-WLZBP           ,
           WORT1      like BSED-WORT1           ,
           WBZOG      like BSED-WBZOG           ,
           WORT2      like BSED-WORT2           ,
           DISKT      like BSED-DISKT           ,
           DISKP      like BSED-DISKP           ,
           XAKTZ      like BSED-XAKTZ           ,
           WSTAT      like BSED-WSTAT           ,
           WGBKZ      like BSED-WGBKZ           ,
           XSIWE      like BSED-XSIWE           ,
           AWTYP      LIKE ACCIT-AWTYP          ,
           AWREF      LIKE ACCIT-AWREF          ,
           AWORG      LIKE ACCIT-AWORG          ,
           STBLG      LIKE Bkpf-stblg           ,
           umsks      LIKE bseg-umsks           ,
           xnegp      LIKE bseg-xnegp           ,
           XBLNR_long like BAPI3008_2-REF_DOC_NO_LONG,
           saknr      like bseg-saknr,
           vbund      type vbund,
           bvtyp      type bvtyp,
           hbkid      type hbkid,
           bupla      type bupla,
           empfb      type empfb,
         END   OF UBAPI3008_2.

* BAPI3008_3 ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008_3,
           UMSKZ      like BSIK-UMSKZ           ,
           WAERS      like BSIK-WAERS           ,
           SHKZG      like BSIK-SHKZG           ,
           SALFW      like BAPI3008_7-BALANCE   ,
           HWAER      like BKPF-HWAER           ,
           SALHW      like BAPI3008_7-BALANCE   ,
         END   OF HBAPI3008_3.

* BAPI3008_7 ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008_7,
           LIFNR      like LFA1-LIFNR           ,
           BUKRS      like LFB1-BUKRS           ,
           GJAHR      like LFC3-GJAHR           ,
           MONAT      like BSIK-MONAT           ,
           SOLL       like BAPI3008_7-BALANCE   ,
           HABEN      like BAPI3008_7-BALANCE   ,
           SALDO      like BAPI3008_7-BALANCE   ,
           UMSATZ     like BAPI3008_7-BALANCE   ,
           WAERS      like BSIK-WAERS           ,
         END   OF HBAPI3008_7.

* BAPI3008_8 ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008_8,
           LIFNR      like LFA1-LIFNR           ,
           BUKRS      like LFB1-BUKRS           ,
           GJAHR      like LFC3-GJAHR           ,
           SHBKZ      like LFC3-SHBKZ           ,
           SALDV      like BAPI3008_7-BALANCE   ,
           SOLL       like BAPI3008_7-BALANCE   ,
           HABEN      like BAPI3008_7-BALANCE   ,
         END   OF HBAPI3008_8.

* BAPI3008_9 ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008_9,
           VRTRG      like BAPI3008_7-BALANCE   ,
           SALDO      like BAPI3008_7-BALANCE   ,
           VRTRGT     like BAPI3008_7-BALANCE   ,
           SALDOT     like BAPI3008_7-BALANCE   ,
           WAERS      like BSIK-WAERS           ,
         END   OF HBAPI3008_9.

* BAPI3008_A ---------------------------------------------------*
DATA:    BEGIN OF HBAPI3008_A,
           LIFNR      like LFA1-LIFNR           ,
           BUKRS      like LFB1-BUKRS           ,
           GJAHR      like LFC3-GJAHR           ,
           SHBKZ      like LFC3-SHBKZ           ,
           VRTRG      like BAPI3008_7-BALANCE   ,
           SOLL       like BAPI3008_7-BALANCE   ,
           HABEN      like BAPI3008_7-BALANCE   ,
           WAERS      like BSIK-WAERS           ,
         END   OF HBAPI3008_A.

RANGES:  BSTAT FOR BSIK-BSTAT.

DATA:    BEGIN OF INTFIELDS,
           XZAHL LIKE BSIK-XZAHL,
*          UMSKS LIKE BSIK-UMSKS,
           XCPDD LIKE BSIK-XCPDD,
         END   OF INTFIELDS.

DATA:    BEGIN OF XLINEITEMS OCCURS 10.
           INCLUDE STRUCTURE HBAPI3008_2.
           INCLUDE STRUCTURE INTFIELDS.
DATA:    END   OF XLINEITEMS.

DATA:    BEGIN OF ZLINEITEMS OCCURS 10.
           INCLUDE STRUCTURE ZEFI_BAPI3008_2.
           INCLUDE STRUCTURE INTFIELDS.
DATA:    END   OF ZLINEITEMS.

DATA:    BEGIN OF ULINEITEMS OCCURS 10.
           INCLUDE STRUCTURE UBAPI3008_2.
           INCLUDE STRUCTURE INTFIELDS.
DATA:    END   OF ULINEITEMS.

DATA:    BEGIN OF SALDO OCCURS 10,
           UMSKZ LIKE HBAPI3008_3-UMSKZ,
           WAERS LIKE HBAPI3008_3-WAERS,
           SALFW LIKE BAPI3008_7-BALANCE,                      "1380599
           HWAER LIKE HBAPI3008_3-HWAER,
           SALHW LIKE BAPI3008_7-BALANCE,                      "1380599
         END   OF SALDO.

DATA:    XACTUAL_BALANCE LIKE HBAPI3008_9.
DATA:    X_BALANCES LIKE HBAPI3008_7 OCCURS 2  WITH HEADER LINE.
DATA:    X_SPECIAL_BALANCES   LIKE HBAPI3008_A OCCURS 2
                                               WITH HEADER LINE.

* ------ Local Memory --------------------------------------------------
DATA:    LM_GENERAL_DETAIL LIKE BAPI1008_4.
DATA:    LM_COMPANY_DETAIL LIKE BAPI1008_5.
DATA:    LM_BANK_DETAIL    LIKE BAPI1008_6 OCCURS 2 WITH HEADER LINE.

* ------ Berechtigungsprüfungen ----------------------------------------
DATA:    BEGIN OF AUTH,
           ACTVT        LIKE TACTZ-ACTVT VALUE '03',
         END   OF AUTH.

* ------ Messages ------------------------------------------------------
DATA: BEGIN OF MESSAGE,
        MSGTY LIKE SY-MSGTY,
        MSGID LIKE SY-MSGID,
        MSGNO LIKE SY-MSGNO,
        MSGV1 LIKE SY-MSGV1,
        MSGV2 LIKE SY-MSGV2,
        MSGV3 LIKE SY-MSGV3,
        MSGV4 LIKE SY-MSGV4,
     END OF MESSAGE.

* ------ Einzelfelder --------------------------------------------------
DATA:    GJAHR          LIKE BKPF-GJAHR,
         HABEN(10),
         MONAT          LIKE BKPF-MONAT,
*        PERIODEN(32)   TYPE C
*                         VALUE ' 1 2 3 4 5 6 7 8 910111213141516',
         RCODE          LIKE SY-SUBRC,
         REFE1          like BAPI3008_7-BALANCE   ,
         REFE2          like BAPI3008_7-BALANCE   ,
         SOLL(10),
         UMNNS          LIKE KNC1-UM01S,
         UMNNH          LIKE KNC1-UM01H,
         UMSATZ(10).

data: begin of perioden,
        per01 like bkpf-monat value ' 1',
        per02 like bkpf-monat value ' 2',
        per03 like bkpf-monat value ' 3',
        per04 like bkpf-monat value ' 4',
        per05 like bkpf-monat value ' 5',
        per06 like bkpf-monat value ' 6',
        per07 like bkpf-monat value ' 7',
        per08 like bkpf-monat value ' 8',
        per09 like bkpf-monat value ' 9',
        per10 like bkpf-monat value '10',
        per11 like bkpf-monat value '11',
        per12 like bkpf-monat value '12',
        per13 like bkpf-monat value '13',
        per14 like bkpf-monat value '14',
        per15 like bkpf-monat value '15',
        per16 like bkpf-monat value '16',
      end   of perioden.

DATA:    CHAR_X     TYPE C VALUE 'X'.
DATA:    LINECOUNT TYPE I.

* ------ Feldsymbole  --------------------------------------------------
FIELD-SYMBOLS:
         <S>,
         <H>,
         <U>.
