* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_FAGL_ACCOUNT_BALANCE                              *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração de saldos de contas do razão FI-GL            *
* Data.............: 16/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920579 | AGIR - Extração de Dados para Migração - Saldos Contas GL    *
* --------------------------------------------------------------------------*
REPORT zrag_fagl_account_balance.

TABLES: faglflext,
        skc1a,
        rf42b,
        sscrfields,
        bkpf.

TYPE-POOLS: abap.

TYPES: BEGIN OF type_skb1,
         bukrs TYPE skb1-bukrs,
         saknr TYPE skb1-saknr,
       END OF type_skb1,

       type_ti_skb1 TYPE TABLE OF type_skb1.

DATA:
  ti_skb1         TYPE type_ti_skb1,
  ti_return       TYPE TABLE OF bapiret2,
  ti_balance      TYPE TABLE OF bapi1028_4  WITH HEADER LINE,
  ti_balance_line TYPE TABLE OF zefi_fdbl_balance_line,
  wa_return       TYPE bapiret2,
  lo_table        TYPE REF TO cl_salv_table,
  lp_balance      TYPE bapi1028_4-balance,
  e_return        TYPE bapireturn,
  e_skb1          TYPE type_skb1,
  e_balance       TYPE bapi1028_4,
  e_balance_line  TYPE zefi_fdbl_balance_line.


FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

* select-options and parameters
SELECT-OPTIONS:
      so_acc FOR faglflext-racct MODIF ID 001 MEMORY ID acc,
      so_emp FOR bkpf-bukrs MODIF ID 002 MEMORY ID buk.
PARAMETERS:
  p_year  TYPE faglflext-ryear MODIF ID 019 MEMORY ID gjr,
  p_curtp TYPE rfpdo2-allgcrtp.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num DEFAULT 100,
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = sy-repid
      variant              = 'DEFAULT'
    EXCEPTIONS
      variant_not_existent = 01
      variant_obsolete     = 02.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      "Selecionar contas do razão por empresa
      SELECT bukrs saknr
        INTO TABLE ti_skb1
        FROM skb1
        UP TO p_maxreg ROWS
        WHERE bukrs IN so_emp
          AND saknr IN so_acc. "#EC CI_GENBUFF

      LOOP AT ti_skb1 INTO e_skb1.

        CALL FUNCTION 'BAPI_GL_GETGLACCPERIODBALANCES'
          EXPORTING
            companycode             = e_skb1-bukrs
            glacct                  = e_skb1-saknr
            fiscalyear              = p_year
            currencytype            = p_curtp
          IMPORTING
            balance_carried_forward = lp_balance
            return                  = e_return
          TABLES
            account_balances        = ti_balance.

        SORT ti_balance BY comp_code gl_account fisc_year fis_period.


        LOOP AT ti_balance INTO e_balance.

          MOVE-CORRESPONDING e_balance TO e_balance_line.

          MOVE: e_balance-debits_per TO e_balance_line-debit,
                e_balance-credit_per TO e_balance_line-credit.

          COLLECT e_balance_line INTO ti_balance_line.

          MOVE e_balance-balance TO e_balance_line-balance.
          MODIFY ti_balance_line INDEX sy-tabix FROM e_balance_line TRANSPORTING balance.
        ENDLOOP.

      ENDLOOP.
*      DATA <table> TYPE TABLE OF any.

*      SUBMIT fagl_account_balance USING SELECTION-SCREEN  W rbukrs
*      AND RETURN .

      IF ti_balance_line IS INITIAL.

        "Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_balance_line ).
        lo_table->display( ).

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_balance_line TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.

  ENDTRY.
