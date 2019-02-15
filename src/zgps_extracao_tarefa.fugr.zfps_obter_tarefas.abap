* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_OBTER_TAREFAS                                     *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Obter as Tarefas (CN47N)                               *
* Data.............: 17/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920299 | AGIR - Extração de Dados para Migração - Tarefa              *
* --------------------------------------------------------------------------*

FUNCTION zfps_obter_tarefas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_PROJETOS) TYPE  ZTPS_RANGE_PROJ OPTIONAL
*"     REFERENCE(P_I_PEPS) TYPE  CURTO_PSPNR_RANGE_T OPTIONAL
*"     REFERENCE(P_I_EMPRESAS) TYPE  ZTPS_RANGE_VBUKR
*"     REFERENCE(P_I_RG_STATUS) TYPE  BAPI_ITOB_T_SEL_STATUS
*"     REFERENCE(P_I_NUM_LINHAS) TYPE  /SAPCND/CODE_LINE_NUM
*"  EXPORTING
*"     REFERENCE(P_E_TI_EXT_TAREFA) TYPE  ZTPS_EXT_TAREFA
*"     REFERENCE(P_E_TI_OBJETOS) TYPE  TTO_ADSPC_OBJ
*"  RAISING
*"      CX_SALV_BS_SC_RUNTIME_INFO
*"----------------------------------------------------------------------

  DATA: ti_op_sel       TYPE TABLE OF rsparams,
        ti_ext_def_proj TYPE ztps_ext_def_proj,
        ti_ext_peps     TYPE ztps_ext_peps,
        ti_ext_tarefa	  TYPE ztps_ext_tarefa,
        ti_status_en    TYPE bu_tj02t_t,
        wa_op_sel       LIKE LINE OF ti_op_sel,
        wa_ext_tar      TYPE zeps_ext_tarefa,
        wa_objeto       TYPE onrpr,
        lr_data         TYPE REF TO data,
        lc_tar_validada TYPE char01.

  FIELD-SYMBOLS: <lt_data>     TYPE ANY TABLE,
                 <ls_data>     TYPE any,
                 <fs_def_proj> TYPE zeps_ext_def_proj,
                 <fs_pep>      TYPE zeps_ext_pep.

*--------------------------------------------------*
*--------------------------------------------------*
  REFRESH: p_e_ti_ext_tarefa,
           p_e_ti_objetos.

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

*--------------------------------------------------*
*   Projetos
    LOOP AT ti_ext_def_proj ASSIGNING <fs_def_proj>.

      CLEAR wa_op_sel.
      wa_op_sel-selname = 'CN_PROJN'.
      wa_op_sel-kind    = 'S'.
      wa_op_sel-sign    = 'I'.
      wa_op_sel-option  = 'EQ'.
      wa_op_sel-low     = <fs_def_proj>-pspid.
      APPEND wa_op_sel TO ti_op_sel.
      CLEAR  wa_op_sel.

    ENDLOOP.

  ELSE.

    TRY.

*       Obter os PEPs (CN43N)
        CALL FUNCTION 'ZFPS_OBTER_PEPS'
          EXPORTING
            p_i_chamada_final = abap_false
            p_i_projetos      = p_i_projetos[]
            p_i_peps          = p_i_peps[]
            p_i_empresas      = p_i_empresas[]
            p_i_rg_status     = p_i_rg_status[]    "Lista de Status a desconsiderar
            p_i_num_linhas    = p_i_num_linhas
          IMPORTING
            p_e_ti_ext_peps   = ti_ext_peps
            p_e_ti_status_en  = ti_status_en.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    IF ti_ext_peps[] IS INITIAL.
      RETURN.
    ENDIF.

*   PEPs
    LOOP AT ti_ext_peps ASSIGNING <fs_pep>.

      wa_op_sel-selname = 'CN_PSPNR'.
      wa_op_sel-kind    = 'S'.
      wa_op_sel-sign    = 'I'.
      wa_op_sel-option  = 'EQ'.
      wa_op_sel-low     = <fs_pep>-posid.
      APPEND wa_op_sel TO ti_op_sel.
      CLEAR  wa_op_sel.

    ENDLOOP.

  ENDIF.

* Diagrams de Rede
  CLEAR wa_op_sel.
  wa_op_sel-selname = 'CN_NETNR'.
  wa_op_sel-kind    = 'S'.
  wa_op_sel-sign    = 'I'.
  wa_op_sel-option  = 'CP'.
  wa_op_sel-low     = '*'.
  APPEND wa_op_sel TO ti_op_sel.
  CLEAR  wa_op_sel.

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
* Isso faz a lógica Standard do Report RPSISNP000 (CN47N)
* não "rechamar" a tela de seleção do
* Banco de Dados Lógico PSJ
* Impede a “rechamada” do próprio programa Standard.
*--------------------------------------------------------*

* Evitar a "rechamada" da tela de seleção do LDB
  CALL FUNCTION 'ZFPS_COMM_HANDLER_APPLSTACK'
    EXPORTING
*     Nome do programa Standard que será chamado (CN47N)
      p_i_nome_prog_standad = 'RPSISVG000'.

*--------------------------------------------------*
* Buscar as Tarefas / Operações (CN47N)
  SUBMIT rpsisvg000
  WITH SELECTION-TABLE ti_op_sel
  AND RETURN.

* Os dados do ALV (CN47N) serão carregados aqui
  cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
  ASSIGN lr_data->* TO <lt_data>.
  cl_salv_bs_runtime_info=>clear_all( ).

*--------------------------------------------------*
  LOOP AT <lt_data> ASSIGNING <ls_data>.

    CLEAR lc_tar_validada.

    MOVE-CORRESPONDING <ls_data> TO wa_ext_tar.
    MOVE-CORRESPONDING <ls_data> TO wa_objeto.

    IF wa_ext_tar-bukrs IN p_i_empresas[].

*     Validar a Tarefa
      CALL FUNCTION 'ZFPS_VALIDAR_TAREFA'
        EXPORTING
          p_i_dr           = wa_ext_tar-aufnr
          p_i_ti_status_en = ti_status_en[]
        IMPORTING
          p_e_tar_validada = lc_tar_validada.

      IF lc_tar_validada EQ abap_true.

        CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
          EXPORTING
            input  = wa_ext_tar-pspid
          IMPORTING
            output = wa_ext_tar-pspid.

        CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
          EXPORTING
            input  = wa_ext_tar-posid
          IMPORTING
            output = wa_ext_tar-posid.

        APPEND wa_ext_tar      TO ti_ext_tarefa.
        APPEND wa_objeto-objnr TO p_e_ti_objetos.

      ENDIF.

    ENDIF.

  ENDLOOP.

*--------------------------------------------------*
  IF NOT ti_ext_tarefa[] IS INITIAL.
    p_e_ti_ext_tarefa[]    = ti_ext_tarefa[].
  ENDIF.

ENDFUNCTION.
