* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZRAG_CUSTO_REAL                                        *
* Transação........:                                                        *
* Tipo de Prg......: Report                                                 *
* Objetivo.........: Extração de custos reais (CJI3 - cat. valores 4 e 11)  *
* Data.............: 10/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920540 | AGIR - Extração de Dados para Migração - H52 - Custos reais  *
* --------------------------------------------------------------------------*

* 52 – Custos Reais -> Buscar todos os custos realizados no projetos,
* para isso utilizar report da transação CJI3 com as categorias de valores
* 4  (Valor Real) e
* 11 (Valor estatístico).

REPORT zrag_custo_real.

DATA:
  gc_vbukr          TYPE ps_vbukr,
  gc_txt_status     TYPE j_txt04,
  gc_proj           TYPE ps_pspid,
  gc_pep            TYPE ps_posid,
  gd_data           TYPE co_budat,
  wa_return         TYPE bapiret2                ##NEEDED,
  lo_table          TYPE REF TO cl_salv_table    ##NEEDED,
  ti_return         TYPE TABLE OF bapiret2       ##NEEDED,
  ti_ext_custo_real TYPE ztps_ext_custo_real     ##NEEDED.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE  ##NEEDED.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECTION-SCREEN BEGIN OF BLOCK b3.

SELECT-OPTIONS : so_proj FOR gc_proj,
                 so_pep  FOR gc_pep MATCHCODE OBJECT prp.

SELECTION-SCREEN END OF BLOCK b3.

SELECT-OPTIONS : so_data FOR gd_data,
                 so_emp  FOR gc_vbukr MATCHCODE OBJECT c_t001,
                 so_st   FOR gc_txt_status NO INTERVALS MATCHCODE OBJECT zrag_status_syst.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num DEFAULT 100,
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

**************************************
**************************************
AT SELECTION-SCREEN ON BLOCK b3.
**************************************
**************************************

  IF ( NOT so_proj[] IS INITIAL ) AND
     ( NOT so_pep[]  IS INITIAL ).

*   Preencher Projeto(s) ou PEP(s)
    MESSAGE e006(zag_mig).


  ENDIF.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      TRY.

*         Obter os custos reais (CJI3 - cat. valores 4 e 11)
          CALL FUNCTION 'ZFPS_OBTER_CUSTO_REAL'
            EXPORTING
              p_i_projetos          = so_proj[]
              p_i_peps              = so_pep[]
              p_i_empresas          = so_emp[]
              p_i_rg_status         = so_st[]    "Lista de Status a desconsiderar
              p_i_data_lanc         = so_data[]
              p_i_num_linhas        = p_maxreg
            IMPORTING
              p_e_ti_ext_custo_real = ti_ext_custo_real.

        CATCH cx_salv_bs_sc_runtime_info.

*         Não é possível recuperar os dados ALV (CJI3)
          MESSAGE i005(zag_mig) DISPLAY LIKE 'E'.
          wa_return-type   = 'E'.
          wa_return-id     = sy-msgid.
          wa_return-number = sy-msgno.
          APPEND wa_return TO ti_return.

      ENDTRY.

      IF ti_ext_custo_real IS INITIAL.

*       Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_custo_real ).
        lo_table->display( ).

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_ext_custo_real TO <t_tab>.
        EXPORT:
          <t_tab>   FROM <t_tab>   TO MEMORY ID 'AG',
          ti_return FROM ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH cx_root ##CATCH_ALL.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
