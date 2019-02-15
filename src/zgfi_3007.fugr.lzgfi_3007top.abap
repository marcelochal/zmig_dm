FUNCTION-POOL zgfi_3007.                    "MESSAGE-ID ..

TABLES:  kna1,
         knb1,
         knc1,
         knc3,
         knbk.

TABLES:  t001,
         bkpf,
         bseg,
         bsec,
         bsed,
         bsid,
         bsad.

* BAPI3007   ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007,
           kunnr LIKE kna1-kunnr,
           bukrs LIKE knb1-bukrs,
           stida LIKE bapi3007-key_date,
           datu1 LIKE bapi3007-from_date,
           datu2 LIKE bapi3007-to_date,
           xsshb LIKE bapi3007-ntditms_rq,
           xsbel LIKE bapi3007-bal_sglind,
         END   OF hbapi3007.

* BAPI3007_1 ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007_1,
           bukrs LIKE knb1-bukrs,
           kunnr LIKE kna1-kunnr,
         END   OF hbapi3007_1.

* BAPI3007_2 ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007_2,
           bukrs      LIKE knb1-bukrs,
           kunnr      LIKE kna1-kunnr,
           umskz      LIKE bsid-umskz,
           augdt      LIKE bsid-augdt,
           augbl      LIKE bsid-augbl,
           zuonr      LIKE bsid-zuonr,
           gjahr      LIKE knc3-gjahr,
           belnr      LIKE bsid-belnr,
           buzei      LIKE bsid-buzei,
           budat      LIKE bsid-budat,
           bldat      LIKE bsid-bldat,
           cpudt      LIKE bsid-cpudt,
           waers      LIKE bsid-waers,
           hwaer      LIKE bkpf-hwaer,
           xblnr      LIKE bsid-xblnr,
           blart      LIKE bsid-blart,
           monat      LIKE bsid-monat,
           bschl      LIKE bsid-bschl,
           shkzg      LIKE bsid-shkzg,
           gsber      LIKE bsid-gsber,
           mwskz      LIKE bsid-mwskz,
           dmbtr      LIKE bapi3007_7-balance,
           wrbtr      LIKE bapi3007_7-balance,
           mwsts      LIKE bapi3007_7-balance,
           wmwst      LIKE bapi3007_7-balance,
           sgtxt      LIKE bsid-sgtxt,
           filkd      LIKE bsid-filkd,
           zfbdt      LIKE bsid-zfbdt,
           zterm      LIKE knb1-zterm,
           zbd1t      LIKE bsid-zbd1t,
           zbd2t      LIKE bsid-zbd2t,
           zbd3t      LIKE bsid-zbd3t,
           zbd1p      LIKE bsid-zbd1p,
           zbd2p      LIKE bsid-zbd2p,
           skfbt      LIKE bapi3007_7-balance,
           sknto      LIKE bapi3007_7-balance,
           wskto      LIKE bapi3007_7-balance,
           zlsch      LIKE bsid-zlsch,
           zlspr      LIKE bsid-zlspr,
           zbfix      LIKE bsid-zbfix,
           rebzg      LIKE bsid-rebzg,
           rebzj      LIKE bsid-rebzj,
           rebzz      LIKE bsid-rebzz,
           mansp      LIKE bsid-mansp,
           mschl      LIKE bsid-mschl,
           madat      LIKE bsid-madat,
           manst      LIKE bsid-manst,
           maber      LIKE bsid-maber,
           bstat      LIKE bsid-bstat,
           rebzt      LIKE bsid-rebzt,
           stceg      LIKE kna1-stceg,
           rstgr      LIKE bsid-rstgr,
           uzawe      LIKE knb1-uzawe,
           xref1      LIKE bsid-xref1,
           xref2      LIKE bsid-xref2,
           pswsl      LIKE bsid-pswsl,
           pswbt      LIKE bapi3007_7-balance,
           nebtr      LIKE bapi3007_7-balance,
           name1      LIKE kna1-name1,
           name2      LIKE kna1-name2,
           name3      LIKE kna1-name3,
           name4      LIKE kna1-name4,
           pstlz      LIKE kna1-pstlz,
           ort01      LIKE kna1-ort01,
           land1      LIKE kna1-land1,
           stras      LIKE kna1-stras,
           pfach      LIKE kna1-pfach,
           pstl2      LIKE kna1-pstl2,
           pskto      LIKE bsec-pskto,
           bankn      LIKE bsec-bankn,
           bankl      LIKE bsec-bankl,
           banks      LIKE bsec-banks,
           stcd1      LIKE kna1-stcd1,
           stcd2      LIKE kna1-stcd2,
           stkzu      LIKE kna1-stkzu,
           stkza      LIKE kna1-stkza,
           regio      LIKE kna1-regio,
           bkont      LIKE bsec-bkont,
           dtaws      LIKE kna1-dtaws,
           empfg      LIKE bsec-empfg,
           spras      LIKE kna1-spras,
           welgf      LIKE bsed-welgf,
           wstkz      LIKE bsed-wstkz,
           wsthw      LIKE bapi3007_7-balance,
           wstfw      LIKE bapi3007_7-balance,
           winhw      LIKE bapi3007_7-balance,
           winfw      LIKE bapi3007_7-balance,
           wmwkz      LIKE bsed-wmwkz,
           wdate      LIKE bsed-wdate,
           wverd      LIKE bsed-wverd,
           wevwv      LIKE bsed-wevwv,
           wbank      LIKE bsed-wbank,
           wname      LIKE bsed-wname,
           wlzbp      LIKE bsed-wlzbp,
           wort1      LIKE bsed-wort1,
           wbzog      LIKE bsed-wbzog,
           wort2      LIKE bsed-wort2,
           diskt      LIKE bsed-diskt,
           diskp      LIKE bsed-diskp,
           xaktz      LIKE bsed-xaktz,
           wstat      LIKE bsed-wstat,
           wgbkz      LIKE bsed-wgbkz,
           xsiwe      LIKE bsed-xsiwe,
           awtyp      LIKE accit-awtyp,
           awref      LIKE accit-awref,
           aworg      LIKE accit-aworg,
           stblg      LIKE bkpf-stblg,
           umsks      LIKE bseg-umsks,
           xnegp      LIKE bseg-xnegp,
           vbeln      LIKE bseg-vbeln,
           saknr      LIKE bseg-saknr,
           anfbn      LIKE bseg-anfbn,
           vbund      LIKE bseg-vbund,
           bupla      LIKE bseg-bupla,
           xblnr_long LIKE bapi3007_2-ref_doc_no_long,
         END   OF hbapi3007_2.

* BAPI3007_2 ---------------------------------------------------*
*DATA:    BEGIN OF ubapi3007_2,
*           bukrs      LIKE knb1-bukrs,
*           kunnr      LIKE kna1-kunnr,
*           umskz      LIKE bsid-umskz,
*           augdt      LIKE bsid-augdt,
*           augbl      LIKE bsid-augbl,
*           zuonr      LIKE bsid-zuonr,
*           gjahr      LIKE knc3-gjahr,
*           belnr      LIKE bsid-belnr,
*           buzei      LIKE bsid-buzei,
*           budat      LIKE bsid-budat,
*           bldat      LIKE bsid-bldat,
*           cpudt      LIKE bsid-cpudt,
*           waers      LIKE bsid-waers,
*           hwaer      LIKE bkpf-hwaer,
*           xblnr      LIKE bapi3007_2-ref_doc_no,
*           blart      LIKE bsid-blart,
*           monat      LIKE bsid-monat,
*           bschl      LIKE bsid-bschl,
*           shkzg      LIKE bsid-shkzg,
*           gsber      LIKE bsid-gsber,
*           mwskz      LIKE bsid-mwskz,
*           dmbtr      LIKE bapi3007_7-balance,
*           wrbtr      LIKE bapi3007_7-balance,
*           mwsts      LIKE bapi3007_7-balance,
*           wmwst      LIKE bapi3007_7-balance,
*           sgtxt      LIKE bsid-sgtxt,
*           filkd      LIKE bsid-filkd,
*           zfbdt      LIKE bsid-zfbdt,
*           zterm      LIKE knb1-zterm,
*           zbd1t      LIKE bsid-zbd1t,
*           zbd2t      LIKE bsid-zbd2t,
*           zbd3t      LIKE bsid-zbd3t,
*           zbd1p      LIKE bsid-zbd1p,
*           zbd2p      LIKE bsid-zbd2p,
*           skfbt      LIKE bapi3007_7-balance,
*           sknto      LIKE bapi3007_7-balance,
*           wskto      LIKE bapi3007_7-balance,
*           zlsch      LIKE bsid-zlsch,
*           zlspr      LIKE bsid-zlspr,
*           zbfix      LIKE bsid-zbfix,
*           rebzg      LIKE bsid-rebzg,
*           rebzj      LIKE bsid-rebzj,
*           rebzz      LIKE bsid-rebzz,
*           mansp      LIKE bsid-mansp,
*           mschl      LIKE bsid-mschl,
*           madat      LIKE bsid-madat,
*           manst      LIKE bsid-manst,
*           maber      LIKE bsid-maber,
*           bstat      LIKE bsid-bstat,
*           rebzt      LIKE bsid-rebzt,
*           stceg      LIKE kna1-stceg,
*           rstgr      LIKE bsid-rstgr,
*           uzawe      LIKE knb1-uzawe,
*           xref1      LIKE bsid-xref1,
*           xref2      LIKE bsid-xref2,
*           pswsl      LIKE bsid-pswsl,
*           pswbt      LIKE bapi3007_7-balance,
*           nebtr      LIKE bapi3007_7-balance,
*           name1      LIKE kna1-name1,
*           name2      LIKE kna1-name2,
*           name3      LIKE kna1-name3,
*           name4      LIKE kna1-name4,
*           pstlz      LIKE kna1-pstlz,
*           ort01      LIKE kna1-ort01,
*           land1      LIKE kna1-land1,
*           stras      LIKE kna1-stras,
*           pfach      LIKE kna1-pfach,
*           pstl2      LIKE kna1-pstl2,
*           pskto      LIKE bsec-pskto,
*           bankn      LIKE bsec-bankn,
*           bankl      LIKE bsec-bankl,
*           banks      LIKE bsec-banks,
*           stcd1      LIKE kna1-stcd1,
*           stcd2      LIKE kna1-stcd2,
*           stkzu      LIKE kna1-stkzu,
*           stkza      LIKE kna1-stkza,
*           regio      LIKE kna1-regio,
*           bkont      LIKE bsec-bkont,
*           dtaws      LIKE kna1-dtaws,
*           empfg      LIKE bsec-empfg,
*           spras      LIKE kna1-spras,
*           welgf      LIKE bsed-welgf,
*           wstkz      LIKE bsed-wstkz,
*           wsthw      LIKE bapi3007_7-balance,
*           wstfw      LIKE bapi3007_7-balance,
*           winhw      LIKE bapi3007_7-balance,
*           winfw      LIKE bapi3007_7-balance,
*           wmwkz      LIKE bsed-wmwkz,
*           wdate      LIKE bsed-wdate,
*           wverd      LIKE bsed-wverd,
*           wevwv      LIKE bsed-wevwv,
*           wbank      LIKE bsed-wbank,
*           wname      LIKE bsed-wname,
*           wlzbp      LIKE bsed-wlzbp,
*           wort1      LIKE bsed-wort1,
*           wbzog      LIKE bsed-wbzog,
*           wort2      LIKE bsed-wort2,
*           diskt      LIKE bsed-diskt,
*           diskp      LIKE bsed-diskp,
*           xaktz      LIKE bsed-xaktz,
*           wstat      LIKE bsed-wstat,
*           wgbkz      LIKE bsed-wgbkz,
*           xsiwe      LIKE bsed-xsiwe,
*           awtyp      LIKE accit-awtyp,
*           awref      LIKE accit-awref,
*           aworg      LIKE accit-aworg,
*           stblg      LIKE bkpf-stblg,
*           umsks      LIKE bseg-umsks,
*           xnegp      LIKE bseg-xnegp,
*           xblnr_long LIKE bapi3007_2-ref_doc_no_long,
*           vbeln      LIKE bseg-vbeln,
*           saknr      LIKE bseg-saknr,
*           anfbn      LIKE bseg-anfbn,
*           vbund      LIKE bseg-vbund,
*           bupla      LIKE bseg-bupla,
*         END   OF ubapi3007_2.

* BAPI3007_3 ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007_3,
           umskz LIKE bsid-umskz,
           waers LIKE bsid-waers,
           shkzg LIKE bsid-shkzg,
           salfw LIKE bapi3007_7-balance,
           hwaer LIKE bkpf-hwaer,
           salhw LIKE bapi3007_7-balance,
         END   OF hbapi3007_3.

* BAPI3007_7 ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007_7,
           kunnr  LIKE kna1-kunnr,
           bukrs  LIKE knb1-bukrs,
           gjahr  LIKE knc3-gjahr,
           monat  LIKE bsid-monat,
           soll   LIKE bapi3007_7-balance,
           haben  LIKE bapi3007_7-balance,
           saldo  LIKE bapi3007_7-balance,
           umsatz LIKE bapi3007_7-balance,
           waers  LIKE bsid-waers,
         END   OF hbapi3007_7.

* BAPI3007_8 ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007_8,
           kunnr LIKE kna1-kunnr,
           bukrs LIKE knb1-bukrs,
           gjahr LIKE knc3-gjahr,
           shbkz LIKE knc3-shbkz,
           saldv LIKE bapi3007_7-balance,
           soll  LIKE bapi3007_7-balance,
           haben LIKE bapi3007_7-balance,
         END   OF hbapi3007_8.

* BAPI3007_9 ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007_9,
           vrtrg  LIKE bapi3007_7-balance,
           saldo  LIKE bapi3007_7-balance,
           vrtrgt LIKE bapi3007_7-balance,
           saldot LIKE bapi3007_7-balance,
           waers  LIKE bsid-waers,
         END   OF hbapi3007_9.

* BAPI3007_A ---------------------------------------------------*
DATA:    BEGIN OF hbapi3007_a,
           kunnr LIKE kna1-kunnr,
           bukrs LIKE knb1-bukrs,
           gjahr LIKE knc3-gjahr,
           shbkz LIKE knc3-shbkz,
           vrtrg LIKE bapi3007_7-balance,
           soll  LIKE bapi3007_7-balance,
           haben LIKE bapi3007_7-balance,
           waers LIKE bsid-waers,
         END   OF hbapi3007_a.

RANGES:  bstat FOR bsid-bstat.

DATA:    BEGIN OF intfields,
           xzahl LIKE bsid-xzahl,
*          UMSKS LIKE BSID-UMSKS,
           xcpdd LIKE bsid-xcpdd,
         END   OF intfields.

DATA:    BEGIN OF xlineitems OCCURS 10.
        INCLUDE STRUCTURE zefi_bapi3007_2.
*        INCLUDE STRUCTURE hbapi3007_2.
        INCLUDE STRUCTURE intfields.
DATA:    END   OF xlineitems.

DATA:    BEGIN OF zlineitems OCCURS 10.
        INCLUDE STRUCTURE zefi_bapi3007_2.
*        INCLUDE STRUCTURE hbapi3007_2.
        INCLUDE STRUCTURE intfields.
DATA:    END   OF zlineitems.

DATA:    BEGIN OF ulineitems OCCURS 10.
*        INCLUDE STRUCTURE ubapi3007_2.
        INCLUDE STRUCTURE zefi_bapi3007_2.
        INCLUDE STRUCTURE intfields.
DATA:    END   OF ulineitems.

DATA:    BEGIN OF saldo OCCURS 10,
           umskz LIKE hbapi3007_3-umskz,
           waers LIKE hbapi3007_3-waers,
           salfw LIKE bapi3007_7-balance,                   "1380599
           hwaer LIKE hbapi3007_3-hwaer,
           salhw LIKE bapi3007_7-balance,                   "1380599
         END   OF saldo.

DATA:    xactual_balance LIKE hbapi3007_9.
DATA:    x_balances LIKE hbapi3007_7 OCCURS 2  WITH HEADER LINE.
DATA:    x_special_balances   LIKE hbapi3007_a OCCURS 2
                                               WITH HEADER LINE.

* ------ Local Memory --------------------------------------------------
DATA:    lm_general_detail LIKE bapi1007_4.
DATA:    lm_company_detail LIKE bapi1007_5.
DATA:    lm_bank_detail    LIKE bapi1007_6 OCCURS 2 WITH HEADER LINE.

* ------ Berechtigungsprüfungen ----------------------------------------
DATA:    BEGIN OF auth,
           actvt LIKE tactz-actvt VALUE '03',
         END   OF auth.

* ------ Messages ------------------------------------------------------
DATA: BEGIN OF message,
        msgty LIKE sy-msgty,
        msgid LIKE sy-msgid,
        msgno LIKE sy-msgno,
        msgv1 LIKE sy-msgv1,
        msgv2 LIKE sy-msgv2,
        msgv3 LIKE sy-msgv3,
        msgv4 LIKE sy-msgv4,
      END OF message.

* ------ Einzelfelder --------------------------------------------------
DATA:    gjahr      LIKE bkpf-gjahr,
         haben(10),
         monat      LIKE bkpf-monat,
*        PERIODEN(32)   TYPE C
*                         VALUE ' 1 2 3 4 5 6 7 8 910111213141516',

         rcode      LIKE sy-subrc,
         refe1      LIKE bapi3007_7-balance,
         refe2      LIKE bapi3007_7-balance,
         soll(10),
         umnns      LIKE knc1-um01s,
         umnnh      LIKE knc1-um01h,
         umsatz(10).

DATA: BEGIN OF perioden,
        per01 LIKE bkpf-monat VALUE ' 1',
        per02 LIKE bkpf-monat VALUE ' 2',
        per03 LIKE bkpf-monat VALUE ' 3',
        per04 LIKE bkpf-monat VALUE ' 4',
        per05 LIKE bkpf-monat VALUE ' 5',
        per06 LIKE bkpf-monat VALUE ' 6',
        per07 LIKE bkpf-monat VALUE ' 7',
        per08 LIKE bkpf-monat VALUE ' 8',
        per09 LIKE bkpf-monat VALUE ' 9',
        per10 LIKE bkpf-monat VALUE '10',
        per11 LIKE bkpf-monat VALUE '11',
        per12 LIKE bkpf-monat VALUE '12',
        per13 LIKE bkpf-monat VALUE '13',
        per14 LIKE bkpf-monat VALUE '14',
        per15 LIKE bkpf-monat VALUE '15',
        per16 LIKE bkpf-monat VALUE '16',
      END   OF perioden.

DATA:    char_x     TYPE c VALUE 'X'.
DATA:    linecount TYPE i.
DATA:    idxsel_x   TYPE c.
DATA:    mem_no_ext_conv TYPE c.                            "n1968346
* ------ Feldsymbole  --------------------------------------------------
FIELD-SYMBOLS:
  <s>,
  <h>,
  <u>.
