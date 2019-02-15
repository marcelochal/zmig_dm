* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_TITULOS_ABERTOS_GL                                *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração de ítens abertos GL                           *
* Data.............: 06/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920557 | AGIR - Extração de Dados para Migração - Ïtens Abertos GL    *
* --------------------------------------------------------------------------*

REPORT zrag_itens_abertos_gl.

DATA:
  gc_bukrs     TYPE bukrs,
  gc_saknr     TYPE saknr,
  wa_return    TYPE bapiret2,
  lo_table     TYPE REF TO cl_salv_table,
  ti_return    TYPE TABLE OF bapiret2,
  ti_ext_items TYPE /sappce/rfposxext.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_emp    FOR gc_bukrs MATCHCODE OBJECT c_t001,
                 so_conta  FOR gc_saknr MATCHCODE OBJECT sako.

*PARAMETERS p_planc TYPE ktopl.

PARAMETERS p_migdt TYPE budat OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN SKIP.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.


INITIALIZATION.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = sy-repid
      variant              = 'DEFAULT'
    EXCEPTIONS
      variant_not_existent = 01
      variant_obsolete     = 02 ##FM_SUBRC_OK.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      "Selecionar ítens em aberto do GL
      PERFORM obter_itens_abertos USING so_emp[]
                                        so_conta[]
                                        p_migdt
                                        p_maxreg
                                  CHANGING ti_ext_items.

      IF ti_ext_items IS INITIAL.

        "Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_items ).
        lo_table->display( ).

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_ext_items TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.

  ENDTRY.
*&---------------------------------------------------------------------*
*&      Form  OBTER_ITENS_ABERTOS
*&---------------------------------------------------------------------*
*  Selecionar ítens em aberto to GL
*----------------------------------------------------------------------*
FORM obter_itens_abertos  USING    p_ti_empresa TYPE fagl_range_t_bukrs
                                   p_ti_conta   TYPE fagl_mm_t_range_saknr
                                   p_dtaberto   TYPE budat
                                   p_maxreg     TYPE /sapcnd/code_line_num
                          CHANGING p_ti_items   TYPE /sappce/rfposxext.

  DATA: ti_op_sel TYPE TABLE OF rsparams,
        e_op_sel  LIKE LINE OF ti_op_sel,
        e_empresa TYPE fagl_range_bukrs,
        e_conta   TYPE fagl_mm_range_saknr,
        lr_data   TYPE REF TO data.

  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE.

  "Preencher parâmetros de seleção para execução do report RFITEMGL
  LOOP AT p_ti_empresa INTO e_empresa.
    e_op_sel-selname = 'SD_BUKRS'.
    e_op_sel-kind    = 'S'.
    MOVE-CORRESPONDING e_empresa TO e_op_sel.
    APPEND e_op_sel TO ti_op_sel.
  ENDLOOP.

  LOOP AT p_ti_conta INTO e_conta.
    e_op_sel-selname = 'SD_SAKNR'.
    e_op_sel-kind    = 'S'.
    MOVE-CORRESPONDING e_conta TO e_op_sel.
    APPEND e_op_sel TO ti_op_sel.
  ENDLOOP.


  IF ti_op_sel IS NOT INITIAL.

    "Não exibir o ALV
    cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_false
                                            metadata = abap_false
                                            data     = abap_true ).

    "Executar relatório Partidas Individuais Contas do Razaão (transação FBL3N)
    SUBMIT rfitemgl
    WITH SELECTION-TABLE ti_op_sel
    WITH pa_stida = p_dtaberto
    WITH pa_nmax  = p_maxreg
    AND RETURN.

    TRY.
        "Carregar os dados do ALV (FBL3N)
        cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
        ASSIGN lr_data->* TO <lt_data>.
        cl_salv_bs_runtime_info=>clear_all( ).

      CATCH cx_salv_bs_sc_runtime_info ##NO_HANDLER.
    ENDTRY.

    IF <lt_data> IS ASSIGNED.
      p_ti_items = <lt_data>.
    ENDIF.

  ENDIF.

ENDFORM.
