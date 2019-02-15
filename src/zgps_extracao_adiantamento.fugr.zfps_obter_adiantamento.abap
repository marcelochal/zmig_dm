* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_OBTER_ADIANTAMENTO                                *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Obter os adiantamentos (CJI3 - cat. valores 12 e 60)   *
* Data.............: 10/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920544 | AGIR - Extração de Dados para Migração - H54 - Adiantamento  *
* --------------------------------------------------------------------------*

FUNCTION zfps_obter_adiantamento.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_PROJETOS) TYPE  ZTPS_RANGE_PROJ OPTIONAL
*"     REFERENCE(P_I_PEPS) TYPE  CURTO_PSPNR_RANGE_T OPTIONAL
*"     REFERENCE(P_I_EMPRESAS) TYPE  ZTPS_RANGE_VBUKR
*"     REFERENCE(P_I_RG_STATUS) TYPE  BAPI_ITOB_T_SEL_STATUS
*"     REFERENCE(P_I_DATA_LANC) TYPE  ISU_RANGES_DATE
*"     REFERENCE(P_I_NUM_LINHAS) TYPE  /SAPCND/CODE_LINE_NUM
*"  EXPORTING
*"     REFERENCE(P_E_TI_EXT_ADIANTAMENTO) TYPE  ZTPS_EXT_ADIANTAMENTO
*"  RAISING
*"      CX_SALV_BS_SC_RUNTIME_INFO
*"----------------------------------------------------------------------
* H54 – Adiantamentos -> Buscar todos os adiantamentos realizados no projetos,
* para isso utilizar report da transação CJI3 com as categorias de valores
* 12 (Adiantamento com despesa) e
* 60 (Documento Pré-editado).

*"--------------------------------------------------------------------
  DATA: ti_op_sel       TYPE TABLE OF rsparams,
        ti_ext_def_proj TYPE ztps_ext_def_proj,
        ti_ext_peps     TYPE ztps_ext_peps,
        ti_status_en    TYPE bu_tj02t_t ##NEEDED,
        ti_ext_adiant   TYPE ztps_ext_adiantamento,
        wa_op_sel       LIKE LINE  OF ti_op_sel,
        wa_ext_adiant   TYPE zeps_ext_adiantamento,
        wa_rg_data      TYPE isu_range_date,
        wa_cs_record    TYPE kaep_covp_ext,
        lr_data         TYPE REF   TO data.

  FIELD-SYMBOLS: <lt_data>     TYPE ANY TABLE,
                 <ls_data>     TYPE any,
                 <fs_def_proj> TYPE zeps_ext_def_proj,
                 <fs_pep>      TYPE zeps_ext_pep.

*--------------------------------------------------*
*--------------------------------------------------*
  REFRESH: p_e_ti_ext_adiantamento.

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

*--------------------------------------------------*
* Data
  IF NOT p_i_data_lanc[] IS INITIAL.

    READ TABLE p_i_data_lanc INTO wa_rg_data INDEX 1.

    wa_op_sel-selname = 'R_BUDAT'.
    wa_op_sel-kind    = 'S'.
    wa_op_sel-sign    = wa_rg_data-sign.
    wa_op_sel-option  = wa_rg_data-option.
    wa_op_sel-low     = wa_rg_data-low.
    wa_op_sel-high    = wa_rg_data-high.
    APPEND wa_op_sel TO ti_op_sel.
    CLEAR  wa_op_sel.

  ELSE.

    wa_op_sel-selname = 'R_BUDAT'.
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
* Categorias de valor
  wa_op_sel-selname = 'G_WRTTP'.
  wa_op_sel-kind    = 'S'.
  wa_op_sel-sign    = 'I'.
  wa_op_sel-option  = 'EQ'.
  wa_op_sel-low     = '12'.       "Adiantamento com despesa
  APPEND wa_op_sel TO ti_op_sel.
  CLEAR  wa_op_sel.

  wa_op_sel-selname = 'G_WRTTP'.
  wa_op_sel-kind    = 'S'.
  wa_op_sel-sign    = 'I'.
  wa_op_sel-option  = 'EQ'.
  wa_op_sel-low     = '60'.       "Documento Pré-editado
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
* Isso faz a lógica Standard do Report RKPEP003 (CJI3)
* não "rechamar" a tela de seleção do
* Banco de Dados Lógico PSJ
* Impede a “rechamada” do próprio programa Standard.
*--------------------------------------------------------*

** Evitar a "rechamada" da tela de seleção do LDB
*  CALL FUNCTION 'ZFPS_COMM_HANDLER_APPLSTACK'
*    EXPORTING
*      p_i_nome_prog_standad = 'RKPEP003'. "Nome do programa Standard que será chamado (CJI3)

*--------------------------------------------------*
* Buscar os PEPs (CJI3)
  SUBMIT rkpep003
  WITH SELECTION-TABLE ti_op_sel
  AND RETURN.

* Os dados do ALV (CJI3) serão carregados aqui
  cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
  ASSIGN lr_data->* TO <lt_data>.
  cl_salv_bs_runtime_info=>clear_all( ).

*--------------------------------------------------*
  IF <lt_data> IS ASSIGNED.

    LOOP AT <lt_data> ASSIGNING <ls_data>.

      MOVE-CORRESPONDING <ls_data> TO wa_ext_adiant.

      MOVE-CORRESPONDING <ls_data> TO wa_cs_record.

      CALL FUNCTION 'ZFPS_EXIT_SAPLKAEP_001'
        CHANGING
          cs_record = wa_cs_record.

      wa_ext_adiant-z_classificacao = wa_cs_record-z_classificacao.
      wa_ext_adiant-zaneel          = wa_cs_record-zaneel.
      wa_ext_adiant-zuar            = wa_cs_record-zuar.
      wa_ext_adiant-ztuc            = wa_cs_record-ztuc.
      wa_ext_adiant-ztbem           = wa_cs_record-ztbem.
      wa_ext_adiant-zti             = wa_cs_record-zti.
      wa_ext_adiant-zsortf          = wa_cs_record-zsortf.
      wa_ext_adiant-zodi            = wa_cs_record-zodi.
      wa_ext_adiant-zcm             = wa_cs_record-zcm.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = wa_ext_adiant-pspid
        IMPORTING
          output = wa_ext_adiant-pspid.

      CALL FUNCTION 'CONVERSION_EXIT_ABPSN_OUTPUT'
        EXPORTING
          input  = wa_ext_adiant-posid
        IMPORTING
          output = wa_ext_adiant-posid.

      APPEND wa_ext_adiant TO ti_ext_adiant.
      CLEAR  wa_ext_adiant.

    ENDLOOP.

  ENDIF.

*--------------------------------------------------*
  IF NOT ti_ext_adiant[] IS INITIAL.
    p_e_ti_ext_adiantamento[] = ti_ext_adiant[].
  ENDIF.

ENDFUNCTION.
