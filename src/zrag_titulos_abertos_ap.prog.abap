* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_TITULOS_ABERTOS_AP                                *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração de títulos em aberto contas a pagar           *
* Data.............: 06/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920483 | AGIR - Extração de Dados para Migração - Títulos Aberto AP   *
* --------------------------------------------------------------------------*

REPORT zrag_titulos_abertos_ap .

TYPES: BEGIN OF type_lfb1,
         lifnr TYPE lfb1-lifnr,
         bukrs TYPE lfb1-bukrs,
       END OF type_lfb1.


DATA:
  gd_tot_amount  TYPE bapiwrbtr,
  gc_bukrs       TYPE bukrs,
  gc_lifnr       TYPE lifnr,
  gi_lines       TYPE i,
  wa_return      TYPE bapiret2,
  lo_table       TYPE REF TO cl_salv_table,
  ti_return      TYPE TABLE OF bapiret2,
  ti_ext_titulos TYPE ztfi_titulos_abertos_ap,
  ti_lineitems   TYPE TABLE OF zefi_bapi3008_2,
  ti_lfb1        TYPE TABLE OF type_lfb1,
  e_lfb1         TYPE type_lfb1,
  e_ext_titulos  TYPE zefi_titulos_abertos_ap,
  e_lineitems    TYPE zefi_bapi3008_2,
  e_return       TYPE bapireturn ##NEEDED.


FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_emp   FOR gc_bukrs MATCHCODE OBJECT c_t001,
                 so_forn  FOR gc_lifnr MATCHCODE OBJECT kred.

PARAMETERS p_keydat TYPE budat OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN SKIP.

PARAMETERS: x_umskz TYPE umskz AS CHECKBOX DEFAULT 'X'.

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

      "Selecionar fornecedores da empresa
      SELECT lifnr bukrs
        INTO TABLE ti_lfb1
        FROM lfb1
        WHERE lifnr IN so_forn
          AND bukrs IN so_emp.

      IF ti_lfb1 IS INITIAL.
        "Erro extraindo dados usando o programa &
        MESSAGE i002(zag_mig) WITH sy-repid DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.
      ENDIF.

      LOOP AT ti_lfb1 INTO e_lfb1.

        REFRESH ti_lineitems.

        "Selecionar títulos em aberto contas a pagar
        CALL FUNCTION 'ZFFI_AP_ACC_GETOPENITEMS'
          EXPORTING
            companycode = e_lfb1-bukrs
            vendor      = e_lfb1-lifnr
            keydate     = p_keydat
          IMPORTING
            return      = e_return
          TABLES
            lineitems   = ti_lineitems.

        "Preencher tabela de saída com títulos em aberto
        LOOP AT ti_lineitems INTO e_lineitems.

          "Desconsiderar operações do razão especial
          IF x_umskz IS INITIAL AND e_lineitems-sp_gl_ind IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          MOVE-CORRESPONDING e_lineitems TO e_ext_titulos ##ENH_OK.

* inicio alteração - ma004818 marcelo alvares - 22.08.2018 16:29:31
          SELECT SINGLE bktxt FROM bkpf INTO e_ext_titulos-bktxt
          WHERE bukrs EQ e_lineitems-comp_code AND
                belnr EQ e_lineitems-doc_no    AND
                gjahr EQ e_lineitems-fisc_year.
* Fim alteração - MA004818 Marcelo Alvares - 22.08.2018 16:29:35

          "Determinar a data de vencimento líquido
          PERFORM determinar_dt_venc USING e_ext_titulos
                                  CHANGING e_ext_titulos-faedt.

          APPEND e_ext_titulos TO ti_ext_titulos.

          IF e_lineitems-db_cr_ind = 'H'. "Crédito
            SUBTRACT e_lineitems-lc_amount FROM gd_tot_amount.
          ELSEIF e_lineitems-db_cr_ind = 'S'. "Débito
            ADD e_lineitems-lc_amount TO gd_tot_amount.
          ENDIF.

          DESCRIBE TABLE ti_ext_titulos LINES gi_lines.
          IF gi_lines = p_maxreg.
            EXIT.
          ENDIF.
        ENDLOOP.

        IF gi_lines = p_maxreg.
          EXIT.
        ENDIF.

      ENDLOOP.

      IF ti_ext_titulos IS INITIAL.

        "Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_titulos ).
        lo_table->display( ).

*        MESSAGE i008(zag_mig) WITH 'Montante total na MI' gd_tot_amount.

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_ext_titulos TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.

  ENDTRY.

*&---------------------------------------------------------------------*
*&      Form  DETERMINAR_DT_VENC
*&---------------------------------------------------------------------*
*  Determinar a data de vencimento líquido
*----------------------------------------------------------------------*
FORM determinar_dt_venc  USING    p_ext_titulos  TYPE zefi_titulos_abertos_ap
                         CHANGING p_dtvenc       TYPE faedt_fpos.

  DATA: ls_faede TYPE faede.

  MOVE:
    'K'                      TO ls_faede-koart,
    p_ext_titulos-bline_date TO ls_faede-zfbdt,  "Data base para cálculo do vencimento
    p_ext_titulos-dsct_days1 TO ls_faede-zbd1t,  "dias de desconto 1
    p_ext_titulos-dsct_days2 TO ls_faede-zbd2t,  "dias de desconto 2
    p_ext_titulos-netterms   TO ls_faede-zbd3t,  "prazo para condição líquida
    p_ext_titulos-dsct_pct1  TO ls_faede-sk1dt,  "desconto 1 vencimento
    p_ext_titulos-dsct_pct2  TO ls_faede-sk2dt,  "desconto 2 vencimento
    p_ext_titulos-doc_date   TO ls_faede-bldat.  "data no documento

  CALL FUNCTION 'DETERMINE_DUE_DATE'
    EXPORTING
      i_faede = ls_faede
    IMPORTING
      e_faede = ls_faede
    EXCEPTIONS
      OTHERS  = 1.

  p_dtvenc = ls_faede-netdt.

ENDFORM.
