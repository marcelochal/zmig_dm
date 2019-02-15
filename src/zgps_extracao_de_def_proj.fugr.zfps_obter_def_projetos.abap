* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_OBTER_DEF_PROJETOS                                *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Obter as definições de projetos (CN42N)                *
* Data.............: 12/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920257 | AGIR - Extração de Dados para Migração - Definição Projeto   *
* --------------------------------------------------------------------------*

FUNCTION zfps_obter_def_projetos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_CHAMADA_FINAL) TYPE  CHAR01
*"     REFERENCE(P_I_PROJETOS) TYPE  ZTPS_RANGE_PROJ
*"     REFERENCE(P_I_EMPRESAS) TYPE  ZTPS_RANGE_VBUKR
*"     REFERENCE(P_I_RG_STATUS) TYPE  BAPI_ITOB_T_SEL_STATUS
*"     REFERENCE(P_I_NUM_LINHAS) TYPE  /SAPCND/CODE_LINE_NUM
*"  EXPORTING
*"     REFERENCE(P_E_TI_EXT_DEF_PROJ) TYPE  ZTPS_EXT_DEF_PROJ
*"     REFERENCE(P_E_TI_STATUS_EN) TYPE  BU_TJ02T_T
*"     REFERENCE(P_E_TI_OBJETOS) TYPE  TTO_ADSPC_OBJ
*"     REFERENCE(P_E_RG_PROJETOS) TYPE  ZTPS_RANGE_PROJ
*"  RAISING
*"      CX_SALV_BS_SC_RUNTIME_INFO
*"----------------------------------------------------------------------

  DATA: lr_data             TYPE REF   TO data,
        lc_projeto_validado	TYPE char01,
        ti_op_sel           TYPE TABLE OF rsparams,
        ti_ext_def_proj     TYPE ztps_ext_def_proj,
        ti_def_proj         TYPE typ_ti_def_proj,
        ti_status_en        TYPE bu_tj02t_t,
        ti_objetos          TYPE tto_adspc_obj,
        ti_rg_proj          TYPE ztps_range_proj,
        wa_op_sel           LIKE LINE  OF ti_op_sel,
        wa_ext_def_proj     TYPE zeps_ext_def_proj,
        wa_proj             TYPE typ_proj,
        wa_objeto           TYPE onrpr,
        wa_rg_proj          TYPE zeps_range_proj.

  FIELD-SYMBOLS: <lt_data>     TYPE ANY TABLE,
                 <ls_data>     TYPE any,
                 <fs_def_proj> TYPE typ_def_proj.

*--------------------------------------------------*
*--------------------------------------------------*
  REFRESH: p_e_ti_ext_def_proj,
           p_e_ti_status_en,
           p_e_ti_objetos,
           p_e_rg_projetos.

* Converte os Status a serem desconsiderados para o Inglês
  CALL FUNCTION 'ZFPS_CONV_STATUS_ES'
    EXPORTING
      p_i_rg_status    = p_i_rg_status[]
    IMPORTING
      p_e_ti_status_en = ti_status_en[].

  IF NOT ti_status_en[] IS INITIAL.
    p_e_ti_status_en[] = ti_status_en[].
  ENDIF.

*--------------------------------------------------*
* Obter os Projetos
  SELECT pspnr
         pspid
         objnr
         vbukr
         stort
         FROM proj
         INTO TABLE ti_def_proj
         UP TO p_i_num_linhas  ROWS
         WHERE pspid IN p_i_projetos
           AND vbukr IN p_i_empresas.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

*--------------------------------------------------*
* Projetos
*--------------------------------------------------*
  LOOP AT ti_def_proj ASSIGNING <fs_def_proj>.

    CLEAR lc_projeto_validado.

*   Validar a definição de projeto
    CALL FUNCTION 'ZFPS_VALIDAR_DEF_PROJETOS'
      EXPORTING
        p_i_project_definition = <fs_def_proj>-pspid
        p_i_ti_status_en       = ti_status_en[]
      IMPORTING
        p_e_projeto_validado   = lc_projeto_validado.

    IF lc_projeto_validado EQ abap_true.

      wa_op_sel-selname = 'CN_PROJN'.
      wa_op_sel-kind    = 'S'.
      wa_op_sel-sign    = 'I'.
      wa_op_sel-option  = 'EQ'.
      wa_op_sel-low     = <fs_def_proj>-pspid.
      APPEND wa_op_sel TO ti_op_sel.
      CLEAR  wa_op_sel.

    ENDIF.

  ENDLOOP.

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
* Isso faz a lógica Standard do Report RPSISPD000 (CN42N)
* não "rechamar" a tela de seleção do
* Banco de Dados Lógico PSJ
* Impede a “rechamada” do próprio programa Standard.
*--------------------------------------------------------*

* Evitar a "rechamada" da tela de seleção do LDB
  CALL FUNCTION 'ZFPS_COMM_HANDLER_APPLSTACK'
    EXPORTING
*     Nome do programa Standard que será chamado (CN42N)
      p_i_nome_prog_standad = 'RPSISPD000'.

*--------------------------------------------------*
* Buscar as Definições de projetos (CN42N)
  SUBMIT rpsispd000
  WITH SELECTION-TABLE ti_op_sel
  AND RETURN.

* Os dados do ALV (CN42N) serão carregados aqui
  cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
  ASSIGN lr_data->* TO <lt_data>.
  cl_salv_bs_runtime_info=>clear_all( ).

*--------------------------------------------------*
  IF <lt_data> IS ASSIGNED.

    LOOP AT <lt_data> ASSIGNING <ls_data>.

      MOVE-CORRESPONDING <ls_data> TO wa_ext_def_proj.
      MOVE-CORRESPONDING <ls_data> TO wa_objeto.
      MOVE-CORRESPONDING <ls_data> TO wa_proj.

*-------------------------------------*
*     Obter dados adicionais do Projeto
      READ TABLE ti_def_proj ASSIGNING <fs_def_proj> WITH TABLE KEY pspid = wa_ext_def_proj-pspid.

      IF sy-subrc EQ 0.
        wa_ext_def_proj-stort = <fs_def_proj>-stort.
      ENDIF.

*-------------------------------------*
      IF p_i_chamada_final EQ abap_true.

        CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
          EXPORTING
            input  = wa_ext_def_proj-pspid
          IMPORTING
            output = wa_ext_def_proj-pspid.

      ENDIF.

      APPEND wa_ext_def_proj TO ti_ext_def_proj.
      APPEND wa_objeto-objnr TO ti_objetos.

      wa_rg_proj-sign   = 'I'.
      wa_rg_proj-option = 'EQ'.
      wa_rg_proj-low    = wa_proj-pspid.
      wa_rg_proj-high   = space.
      APPEND wa_rg_proj TO ti_rg_proj.
      CLEAR  wa_rg_proj.

    ENDLOOP.

  ENDIF.

*--------------------------------------------------*
  IF NOT ti_ext_def_proj[] IS INITIAL.
    p_e_ti_ext_def_proj[] = ti_ext_def_proj[].
  ENDIF.

*--------------------------------------------------*
  IF NOT ti_objetos[] IS INITIAL.
    p_e_ti_objetos[] = ti_objetos[].
  ENDIF.

*--------------------------------------------------*
  IF NOT ti_rg_proj[] IS INITIAL.
    p_e_rg_projetos[] = ti_rg_proj[].
  ENDIF.

ENDFUNCTION.
