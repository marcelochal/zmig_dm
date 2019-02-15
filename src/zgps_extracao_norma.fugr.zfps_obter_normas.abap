* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_OBTER_NORMAS                                      *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Obter as Normas de apropriação (KOSRLIST_PR)           *
* Data.............: 03/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920445 | AGIR - Extração de Dados para Migração - H55 - Norma         *
* --------------------------------------------------------------------------*

FUNCTION zfps_obter_normas.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_PROJETOS) TYPE  ZTPS_RANGE_PROJ OPTIONAL
*"     REFERENCE(P_I_PEPS) TYPE  CURTO_PSPNR_RANGE_T OPTIONAL
*"     REFERENCE(P_I_PEFIS) TYPE  ZTPS_RANGE_APROF
*"     REFERENCE(P_I_EMPRESAS) TYPE  ZTPS_RANGE_VBUKR
*"     REFERENCE(P_I_RG_STATUS) TYPE  BAPI_ITOB_T_SEL_STATUS
*"     REFERENCE(P_I_NUM_LINHAS) TYPE  /SAPCND/CODE_LINE_NUM
*"  EXPORTING
*"     REFERENCE(P_E_TI_EXT_NORMA) TYPE  ZTPS_EXT_NORMA
*"  RAISING
*"      CX_SALV_BS_SC_RUNTIME_INFO
*"----------------------------------------------------------------------

  DATA: ti_op_sel       TYPE TABLE OF rsparams,
        ti_ext_norma    TYPE ztps_ext_norma,
        ti_objetos      TYPE tto_adspc_obj,
        ti_obj_proj     TYPE tto_adspc_obj,
        ti_obj_pep      TYPE tto_adspc_obj,
        ti_obj_dr       TYPE tto_adspc_obj,
        ti_obj_tar      TYPE tto_adspc_obj,
        ti_rg_projetos  TYPE ztps_range_proj,
        ti_rg_peps      TYPE curto_pspnr_range_t,
        ti_header       TYPE typ_ti_header,
        ti_data         TYPE typ_ti_data,
        wa_op_sel       LIKE LINE OF ti_op_sel,
        wa_ext_norma    TYPE zeps_ext_norma,
        lr_data         TYPE REF TO data,
        lc_tar_validada TYPE char01,
        lc_perfil       TYPE char6.

  FIELD-SYMBOLS: <ls_data>      TYPE ys_alv_data,
                 <ls_header>    TYPE ys_alv_header,
                 <fs_def_proj>  TYPE zeps_ext_def_proj,
                 <fs_objeto>    TYPE j_objnr,
                 <lt_data>      TYPE ANY TABLE,
                 <fs_so_aprof>  TYPE ANY TABLE,
                 <fs_ti_header> TYPE ANY TABLE,
                 <fs_ti_data>   TYPE ANY TABLE.

  DATA: lc_header(21) VALUE '(SAPLKOBS)GT_HEADER[]',
        lc_data(19)   VALUE '(SAPLKOBS)GT_DATA[]'.

  DATA:
    lt_objnr LIKE jsto_pre OCCURS 0 WITH HEADER LINE,
    lt_cobra LIKE cobra    OCCURS 0 WITH HEADER LINE,
    lt_cobrb LIKE cobrb    OCCURS 0 WITH HEADER LINE.

*--------------------------------------------------*
*--------------------------------------------------*
  REFRESH p_e_ti_ext_norma.

*--------------------------------------------------*
* Seleção por Projeto                              *
*--------------------------------------------------*
  IF ( NOT p_i_projetos[] IS INITIAL OR p_i_projetos[] IS INITIAL ) AND
     (     p_i_peps[]     IS INITIAL ) ##BOOL_OK.

    TRY.

*       Obter as definições de projetos (CN42N)
        CALL FUNCTION 'ZFPS_OBTER_DEF_PROJETOS'
          EXPORTING
            p_i_chamada_final = abap_false
            p_i_projetos      = p_i_projetos[]
            p_i_empresas      = p_i_empresas[]
            p_i_rg_status     = p_i_rg_status[]  "Lista de Status a desconsiderar
            p_i_num_linhas    = p_i_num_linhas
          IMPORTING
            p_e_ti_objetos    = ti_obj_proj      "PD
            p_e_rg_projetos   = ti_rg_projetos.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    APPEND LINES OF ti_obj_proj TO ti_objetos.

*---------------------------------*
    TRY.

*       Obter os PEPs (CN43N)
        CALL FUNCTION 'ZFPS_OBTER_PEPS'
          EXPORTING
            p_i_chamada_final = abap_false
            p_i_projetos      = ti_rg_projetos[]
            p_i_empresas      = p_i_empresas[]
            p_i_rg_status     = p_i_rg_status[]    "Lista de Status a desconsiderar
            p_i_num_linhas    = p_i_num_linhas
          IMPORTING
            p_e_ti_objetos    = ti_obj_pep    "PR
            p_e_rg_peps       = ti_rg_peps.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    APPEND LINES OF ti_obj_pep TO ti_objetos.

*---------------------------------*
    TRY.

*       Obter os Diagramas de Rede (CN46N)
        CALL FUNCTION 'ZFPS_OBTER_DIAGRAMAS_DE_REDE'
          EXPORTING
            p_i_peps       = ti_rg_peps[]
            p_i_empresas   = p_i_empresas[]
            p_i_rg_status  = p_i_rg_status[]    "Lista de Status a desconsiderar
            p_i_num_linhas = p_i_num_linhas
          IMPORTING
            p_e_ti_objetos = ti_obj_dr
            p_e_rg_peps    = ti_rg_peps.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    APPEND LINES OF ti_obj_dr TO ti_objetos.

*---------------------------------*
    TRY.

*       Obter as Tarefas (CN47N)
        CALL FUNCTION 'ZFPS_OBTER_TAREFAS'
          EXPORTING
            p_i_peps       = ti_rg_peps[]
            p_i_empresas   = p_i_empresas[]
            p_i_rg_status  = p_i_rg_status[]    "Lista de Status a desconsiderar
            p_i_num_linhas = p_i_num_linhas
          IMPORTING
            p_e_ti_objetos = ti_obj_tar.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    APPEND LINES OF ti_obj_tar TO ti_objetos.

*--------------------------------------------------*
* Seleção por PEP                              *
*--------------------------------------------------*
  ELSE.

    TRY.

*       Obter os PEPs (CN43N)
        CALL FUNCTION 'ZFPS_OBTER_PEPS'
          EXPORTING
            p_i_chamada_final = abap_false
            p_i_peps          = p_i_peps[]
            p_i_empresas      = p_i_empresas[]
            p_i_rg_status     = p_i_rg_status[]    "Lista de Status a desconsiderar
            p_i_num_linhas    = p_i_num_linhas
          IMPORTING
            p_e_ti_objetos    = ti_obj_pep
            p_e_rg_peps       = ti_rg_peps.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    APPEND LINES OF ti_rg_peps TO ti_objetos.

*---------------------------------*
    TRY.

*       Obter os Diagramas de Rede (CN46N)
        CALL FUNCTION 'ZFPS_OBTER_DIAGRAMAS_DE_REDE'
          EXPORTING
            p_i_peps       = ti_rg_peps[]
            p_i_empresas   = p_i_empresas[]
            p_i_rg_status  = p_i_rg_status[]    "Lista de Status a desconsiderar
            p_i_num_linhas = p_i_num_linhas
          IMPORTING
            p_e_ti_objetos = ti_obj_dr
            p_e_rg_peps    = ti_rg_peps.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    APPEND LINES OF ti_obj_dr TO ti_objetos.

*---------------------------------*
    TRY.

*       Obter as Tarefas (CN47N)
        CALL FUNCTION 'ZFPS_OBTER_TAREFAS'
          EXPORTING
            p_i_peps       = ti_rg_peps[]
            p_i_empresas   = p_i_empresas[]
            p_i_rg_status  = p_i_rg_status[]    "Lista de Status a desconsiderar
            p_i_num_linhas = p_i_num_linhas
          IMPORTING
            p_e_ti_objetos = ti_obj_tar.

      CATCH cx_salv_bs_sc_runtime_info.

        RETURN.

    ENDTRY.

    APPEND LINES OF ti_obj_tar TO ti_objetos.

  ENDIF.

*--------------------------------------------------*
  IF ti_objetos[] IS INITIAL.
    RETURN.
  ELSE.
    lt_objnr[] = ti_objetos[].
  ENDIF.

*-----------------------------------------------------------------------
  PERFORM select_objects_with_objnr TABLES p_i_pefis
                                           lt_objnr
                                           lt_cobra
                                           lt_cobrb.

*-----------------------------------------------------------------------
* Fill output table
*-----------------------------------------------------------------------
  PERFORM alv_output_tables_fill IN PROGRAM saplkobs
                                   IF FOUND TABLES lt_cobra
                                                   lt_cobrb.

  ASSIGN (lc_header) TO <fs_ti_header>.
  IF <fs_ti_header> IS ASSIGNED.
    ti_header[] = <fs_ti_header>[].
  ENDIF.

  ASSIGN (lc_data)   TO <fs_ti_data>.
  IF <fs_ti_data> IS ASSIGNED.
    ti_data[] = <fs_ti_data>[].
  ENDIF.

  LOOP AT ti_header ASSIGNING <ls_header>.

    LOOP AT ti_data ASSIGNING <ls_data> WHERE objnr EQ <ls_header>-objnr.

      MOVE-CORRESPONDING <ls_header> TO wa_ext_norma.
      MOVE-CORRESPONDING <ls_data>   TO wa_ext_norma.
      APPEND wa_ext_norma TO ti_ext_norma.
      CLEAR  wa_ext_norma.

    ENDLOOP.

  ENDLOOP.

*--------------------------------------------------*
  IF NOT ti_ext_norma[] IS INITIAL.
    p_e_ti_ext_norma[]  = ti_ext_norma[].
  ENDIF.

ENDFUNCTION.
