* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_OBTER_PEPS                                        *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Obter os PEPs (CN43N)                                  *
* Data.............: 14/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920275 | AGIR - Extração de Dados para Migração - PEPs                *
* --------------------------------------------------------------------------*

FUNCTION zfps_obter_peps.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_CHAMADA_FINAL) TYPE  CHAR01
*"     REFERENCE(P_I_PROJETOS) TYPE  ZTPS_RANGE_PROJ OPTIONAL
*"     REFERENCE(P_I_PEPS) TYPE  CURTO_PSPNR_RANGE_T OPTIONAL
*"     REFERENCE(P_I_EMPRESAS) TYPE  ZTPS_RANGE_VBUKR
*"     REFERENCE(P_I_RG_STATUS) TYPE  BAPI_ITOB_T_SEL_STATUS
*"     REFERENCE(P_I_NUM_LINHAS) TYPE  /SAPCND/CODE_LINE_NUM
*"  EXPORTING
*"     REFERENCE(P_E_TI_EXT_PEPS) TYPE  ZTPS_EXT_PEPS
*"     REFERENCE(P_E_TI_STATUS_EN) TYPE  BU_TJ02T_T
*"     REFERENCE(P_E_TI_OBJETOS) TYPE  TTO_ADSPC_OBJ
*"     REFERENCE(P_E_RG_PEPS) TYPE  CURTO_PSPNR_RANGE_T
*"  RAISING
*"      CX_SALV_BS_SC_RUNTIME_INFO
*"----------------------------------------------------------------------

  DATA: ti_op_sel       TYPE TABLE OF rsparams,
        ti_ext_def_proj TYPE ztps_ext_def_proj,
        ti_status_en    TYPE bu_tj02t_t,
        ti_ext_peps     TYPE ztps_ext_peps,
        ti_objetos      TYPE tto_adspc_obj,
        ti_rg_pep       TYPE curto_pspnr_range_t,
        ti_peps         TYPE typ_ti_pep,
        wa_op_sel       LIKE LINE  OF ti_op_sel,
        wa_ext_pep      TYPE zeps_ext_pep,
        wa_range_pep    TYPE curto_pspnr_range,
        wa_objeto       TYPE onrpr,
        wa_pep          TYPE typ_pep,
        lr_data         TYPE REF   TO data,
        lc_pep_validado	TYPE char01.

  FIELD-SYMBOLS: <lt_data>     TYPE ANY TABLE,
                 <ls_data>     TYPE any,
                 <fs_pep>      TYPE typ_pep,
                 <fs_def_proj> TYPE zeps_ext_def_proj.

*--------------------------------------------------*
*--------------------------------------------------*
  REFRESH: p_e_ti_ext_peps,
           p_e_ti_status_en,
           p_e_ti_objetos,
           p_e_rg_peps.

  IF ( NOT p_i_projetos[] IS INITIAL OR p_i_projetos[] IS INITIAL ) AND
     (     p_i_peps[]     IS INITIAL ) ##BOOL_OK.

    TRY.

*       Obter as definições de projetos (CN42N)
        CALL FUNCTION 'ZFPS_OBTER_DEF_PROJETOS'
          EXPORTING
            p_i_chamada_final   = abap_false
            p_i_projetos        = p_i_projetos[]
            p_i_empresas        = p_i_empresas[]
            p_i_rg_status       = p_i_rg_status[]
            p_i_num_linhas      = p_i_num_linhas
          IMPORTING
            p_e_ti_ext_def_proj = ti_ext_def_proj
            p_e_ti_status_en    = ti_status_en.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    IF ti_ext_def_proj[] IS INITIAL.
      RETURN.
    ENDIF.

*----------------------------------------*
*   Projetos
    LOOP AT ti_ext_def_proj ASSIGNING <fs_def_proj>.

      wa_op_sel-selname = 'CN_PROJN'.
      wa_op_sel-kind    = 'S'.
      wa_op_sel-sign    = 'I'.
      wa_op_sel-option  = 'EQ'.
      wa_op_sel-low     = <fs_def_proj>-pspid.
      APPEND wa_op_sel TO ti_op_sel.
      CLEAR  wa_op_sel.

    ENDLOOP.

  ELSE.

*   Converte os Status a serem desconsiderados para o Inglês
    CALL FUNCTION 'ZFPS_CONV_STATUS_ES'
      EXPORTING
        p_i_rg_status    = p_i_rg_status[]
      IMPORTING
        p_e_ti_status_en = ti_status_en[].

    IF NOT ti_status_en[] IS INITIAL.
      p_e_ti_status_en[] = ti_status_en[].
    ENDIF.

*----------------------------------------*
*   PEPs
    LOOP AT p_i_peps INTO wa_range_pep.

      wa_op_sel-selname = 'CN_PSPNR'.
      wa_op_sel-kind    = 'S'.
      wa_op_sel-sign    = wa_range_pep-sign.
      wa_op_sel-option  = wa_range_pep-option.
      wa_op_sel-low     = wa_range_pep-low.
      wa_op_sel-high    = wa_range_pep-high.
      APPEND wa_op_sel TO ti_op_sel.
      CLEAR  wa_op_sel.

    ENDLOOP.

  ENDIF.

*--------------------------------------------------*
* Nível
  CLEAR wa_op_sel.
  wa_op_sel-selname = 'CN_STUFE'.
  wa_op_sel-kind    = 'S'.
  wa_op_sel-sign    = 'I'.
  wa_op_sel-option  = 'BT'.
  wa_op_sel-low     = '1'.
  wa_op_sel-high    = '99'.
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
* Isso faz a lógica Standard do Report RPSISPD000 (CN43N)
* não "rechamar" a tela de seleção do
* Banco de Dados Lógico PSJ
* Impede a “rechamada” do próprio programa Standard.
*--------------------------------------------------------*

* Evitar a "rechamada" da tela de seleção do LDB
  CALL FUNCTION 'ZFPS_COMM_HANDLER_APPLSTACK'
    EXPORTING
*     Nome do programa Standard que será chamado (CN43N)
      p_i_nome_prog_standad = 'RPSISPE000'.

*--------------------------------------------------*
* Buscar os PEPs (CN43N)
  SUBMIT rpsispe000
  WITH SELECTION-TABLE ti_op_sel
  AND RETURN.

* Os dados do ALV (CN43N) serão carregados aqui
  cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
  ASSIGN lr_data->* TO <lt_data>.
  cl_salv_bs_runtime_info=>clear_all( ).

*--------------------------------------------------*
* Obter campos adicionais de PEP
  SELECT pspnr
         posid
         stort
         FROM prps
         INTO TABLE ti_peps
*        Índice E
         WHERE posid IN p_i_peps[].

*--------------------------------------------------*
  LOOP AT <lt_data> ASSIGNING <ls_data>.

    CLEAR lc_pep_validado.

    MOVE-CORRESPONDING <ls_data> TO wa_ext_pep.
    MOVE-CORRESPONDING <ls_data> TO wa_objeto.
    MOVE-CORRESPONDING <ls_data> TO wa_pep.

*-------------------------------------*
*   Obter campos adicionais de PEP
    READ TABLE ti_peps ASSIGNING <fs_pep> WITH TABLE KEY posid = wa_ext_pep-posid.

    IF sy-subrc EQ 0.
      wa_ext_pep-stort = <fs_pep>-stort.
    ENDIF.

*-------------------------------------*
    IF wa_ext_pep-pbukr IN p_i_empresas[].

*     Validar o PEP
      CALL FUNCTION 'ZFPS_VALIDAR_PEP'
        EXPORTING
          p_i_pep          = wa_ext_pep-posid
          p_i_ti_status_en = ti_status_en[]
        IMPORTING
          p_e_pep_validado = lc_pep_validado.

      IF lc_pep_validado EQ abap_true.

        IF p_i_chamada_final EQ abap_true.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
            EXPORTING
              input  = wa_ext_pep-pspid
            IMPORTING
              output = wa_ext_pep-pspid.

          CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
            EXPORTING
              input  = wa_ext_pep-posid
            IMPORTING
              output = wa_ext_pep-posid.

        ENDIF.

        APPEND wa_ext_pep      TO ti_ext_peps.
        APPEND wa_objeto-objnr TO p_e_ti_objetos.

        wa_range_pep-sign   = 'I'.
        wa_range_pep-option = 'EQ'.
        wa_range_pep-low    = wa_pep-posid.
        wa_range_pep-high   = space.
        APPEND wa_range_pep TO ti_rg_pep.
        CLEAR  wa_range_pep.

      ENDIF.

    ENDIF.

  ENDLOOP.

*--------------------------------------------------*
  IF NOT ti_ext_peps[] IS INITIAL.
    p_e_ti_ext_peps[]     = ti_ext_peps[].
  ENDIF.

*--------------------------------------------------*
  IF NOT ti_objetos[] IS INITIAL.
    p_e_ti_objetos[] = ti_objetos[].
  ENDIF.

*--------------------------------------------------*
  IF NOT ti_rg_pep[] IS INITIAL.
    p_e_rg_peps[] = ti_rg_pep[].
  ENDIF.

ENDFUNCTION.
