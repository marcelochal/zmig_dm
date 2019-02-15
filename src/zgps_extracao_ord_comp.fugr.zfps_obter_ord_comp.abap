* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_OBTER_ORD_COMP                                    *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Obter os compromissos para Ordens (KOB2)               *
* Data.............: 17/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920686 | AGIR - Extração de Dados para Migração - H989 - Ord - Comp   *
* --------------------------------------------------------------------------*

FUNCTION zfps_obter_ord_comp.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_SO_ORDEM) TYPE  RANGE_T_AUFNR OPTIONAL
*"     REFERENCE(P_I_GRP_ORD) TYPE  AUFGR OPTIONAL
*"     REFERENCE(P_I_SO_EMPRESAS) TYPE  ZTPS_RANGE_VBUKR
*"     REFERENCE(P_I_SO_STATUS) TYPE  BAPI_ITOB_T_SEL_STATUS
*"     REFERENCE(P_I_SO_DATA_DEB) TYPE  ISU_RANGES_DATE
*"     REFERENCE(P_I_SO_TPORD) TYPE  MNT_T_AUART_RANGE
*"     REFERENCE(P_I_NUM_LINHAS) TYPE  /SAPCND/CODE_LINE_NUM
*"  EXPORTING
*"     REFERENCE(P_E_TI_EXT_ORD_COMP) TYPE  ZTPS_EXT_ORD_COMPROMISSO
*"  RAISING
*"      CX_SALV_BS_SC_RUNTIME_INFO
*"----------------------------------------------------------------------

  DATA: lc_ordem_validada TYPE char01,
        ti_op_sel         TYPE TABLE OF rsparams,
        ti_status_en      TYPE bu_tj02t_t ##NEEDED,
        ti_ext_ord_comp   TYPE ztps_ext_ord_compromisso,
        wa_op_sel         LIKE LINE  OF ti_op_sel,
        wa_ext_ord_comp   TYPE zeps_ext_ord_compromisso,
        wa_rg_data        TYPE isu_range_date,
        wa_rg_ordem       TYPE range_s_aufnr,
        lr_data           TYPE REF   TO data.

  FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE,
                 <ls_data> TYPE any.

*--------------------------------------------------*
*--------------------------------------------------*
  REFRESH: p_e_ti_ext_ord_comp.

* Converte os Status a serem desconsiderados para o Inglês
  CALL FUNCTION 'ZFPS_CONV_STATUS_ES'
    EXPORTING
      p_i_rg_status    = p_i_so_status[]
    IMPORTING
      p_e_ti_status_en = ti_status_en[].

*--------------------------------------------------*
* Área contab.custos
  wa_op_sel-selname = 'P_KOKRS'.
  wa_op_sel-kind    = 'P'.
  wa_op_sel-sign    = 'I'.
  wa_op_sel-option  = 'EQ'.
  wa_op_sel-low     = 'TB00'.     "Taesa
  APPEND wa_op_sel TO ti_op_sel.
  CLEAR  wa_op_sel.

* Transação
  wa_op_sel-selname = 'P_TCODE'.
  wa_op_sel-kind    = 'P'.
  wa_op_sel-sign    = 'I'.
  wa_op_sel-option  = 'EQ'.
  wa_op_sel-low     = 'KOB2'.
  APPEND wa_op_sel TO ti_op_sel.
  CLEAR  wa_op_sel.

* Seleção por Ordem
  IF ( NOT p_i_so_ordem[] IS INITIAL OR p_i_so_ordem[] IS INITIAL ) AND
     (     p_i_grp_ord    IS INITIAL ) ##BOOL_OK.

    LOOP AT p_i_so_ordem INTO wa_rg_ordem.

      wa_op_sel-selname = 'AUFNR'.
      wa_op_sel-kind    = 'S'.
      wa_op_sel-sign    = wa_rg_ordem-sign.
      wa_op_sel-option  = wa_rg_ordem-option.
      wa_op_sel-low     = wa_rg_ordem-low.
      wa_op_sel-high    = wa_rg_ordem-high.
      APPEND wa_op_sel TO ti_op_sel.
      CLEAR  wa_op_sel.

    ENDLOOP.

* Seleção por Grupo de ordens
  ELSE.

    wa_op_sel-selname = 'AUFGR'.
    wa_op_sel-kind    = 'P'.
    wa_op_sel-sign    = 'I'.
    wa_op_sel-option  = 'EQ'.
    wa_op_sel-low     = p_i_grp_ord.
    APPEND wa_op_sel TO ti_op_sel.
    CLEAR  wa_op_sel.

  ENDIF.

*--------------------------------------------------*
* Data
  IF NOT p_i_so_data_deb[] IS INITIAL.

    READ TABLE p_i_so_data_deb INTO wa_rg_data INDEX 1.

    wa_op_sel-selname = 'R_OBDAT'.
    wa_op_sel-kind    = 'S'.
    wa_op_sel-sign    = wa_rg_data-sign.
    wa_op_sel-option  = wa_rg_data-option.
    wa_op_sel-low     = wa_rg_data-low.
    wa_op_sel-high    = wa_rg_data-high.
    APPEND wa_op_sel TO ti_op_sel.
    CLEAR  wa_op_sel.

  ELSE.

    wa_op_sel-selname = 'R_OBDAT'.
    wa_op_sel-kind    = 'S'.
    wa_op_sel-sign    = 'I'.
    wa_op_sel-option  = 'BT'.
    wa_op_sel-low     = '19000101'.
    wa_op_sel-high    = '23001231'.
    APPEND wa_op_sel TO ti_op_sel.
    CLEAR  wa_op_sel.

  ENDIF.

*--------------------------------------------------*
* Número Máximo de ocorr
  wa_op_sel-selname = 'P_MAXSEL'.
  wa_op_sel-kind    = 'P'.
  wa_op_sel-sign    = 'I'.
  wa_op_sel-option  = 'EQ'.
  wa_op_sel-low     = '2147483647'. "Máximo SAP
  APPEND wa_op_sel TO ti_op_sel.
  CLEAR  wa_op_sel.

*--------------------------------------------------*

  IF ti_op_sel[] IS INITIAL.
    RETURN.
  ENDIF.

*--------------------------------------------------*
* Não exibir o ALV
  cl_salv_bs_runtime_info=>set(
    EXPORTING display  = abap_false
              metadata = abap_false
              data     = abap_true ).

*--------------------------------------------------------*
* Isso faz a lógica Standard do Report RKAEP000 (KOB2)
* não "rechamar" a tela de seleção.
* Impede a “rechamada” do próprio programa Standard.
*--------------------------------------------------------*

** Evitar a "rechamada" da tela de seleção do Report Standard
*  CALL FUNCTION 'ZFPS_COMM_HANDLER_APPLSTACK'
*    EXPORTING
*      p_i_nome_prog_standad = 'RKAEP000'. "Nome do programa Standard que será chamado (KOB2)

*--------------------------------------------------*
* Buscar as Ordens: partidas individuais reais (KOB2)
  SUBMIT rkaep000
  WITH SELECTION-TABLE ti_op_sel
  AND RETURN.

* Os dados do ALV (KOB2) serão carregados aqui
  cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
  ASSIGN lr_data->* TO <lt_data>.
  cl_salv_bs_runtime_info=>clear_all( ).

*--------------------------------------------------*
  IF <lt_data> IS ASSIGNED.

    LOOP AT <lt_data> ASSIGNING <ls_data>.

      MOVE-CORRESPONDING <ls_data> TO wa_ext_ord_comp.

*     Validar a ordem
      CALL FUNCTION 'ZFPS_VALIDAR_ORD_COMP'
        EXPORTING
          p_i_ordem          = wa_ext_ord_comp-aufnr
          p_i_empresa        = wa_ext_ord_comp-bukrs
          p_i_valor_tot      = wa_ext_ord_comp-wkgbtr
          p_i_so_empresas    = p_i_so_empresas[]
          p_i_so_tpord       = p_i_so_tpord[]
          p_i_ti_status_en   = ti_status_en[]
        IMPORTING
          p_e_ordem_validada = lc_ordem_validada.

      IF lc_ordem_validada EQ abap_true.

        APPEND wa_ext_ord_comp TO ti_ext_ord_comp.
        CLEAR  wa_ext_ord_comp.

      ENDIF.

    ENDLOOP.

  ENDIF.

*--------------------------------------------------*
  IF NOT ti_ext_ord_comp[] IS INITIAL.
    p_e_ti_ext_ord_comp[] = ti_ext_ord_comp[].
  ENDIF.

ENDFUNCTION.
