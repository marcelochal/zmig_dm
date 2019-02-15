* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZRAG_DEF_PROJETOS                                      *
* Transação........:                                                        *
* Tipo de Prg......: Report                                                 *
* Objetivo.........: Extração de Definições de Projetos (CN42N)             *
* Data.............: 12/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920257 | AGIR - Extração de Dados para Migração - Definição Projeto   *
* --------------------------------------------------------------------------*

REPORT zrag_def_projetos.

DATA:
  gc_vbukr        TYPE ps_vbukr,
  gc_txt_status   TYPE j_txt04,
  gc_proj         TYPE ps_pspid,
  wa_return       TYPE bapiret2,
  lo_table        TYPE REF TO cl_salv_table,
  ti_return       TYPE TABLE OF bapiret2,
  ti_ext_def_proj TYPE  ztps_ext_def_proj.


FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECTION-SCREEN BEGIN OF BLOCK b3.

SELECT-OPTIONS : so_proj FOR gc_proj.

SELECTION-SCREEN END OF BLOCK b3.

SELECT-OPTIONS : so_emp  FOR gc_vbukr                   MATCHCODE OBJECT c_t001,
                 so_st   FOR gc_txt_status NO INTERVALS MATCHCODE OBJECT zrag_status_syst.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num DEFAULT 100,
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      TRY.

*         Obter as definições de projetos (CN42N)
          CALL FUNCTION 'ZFPS_OBTER_DEF_PROJETOS'
            EXPORTING
              p_i_chamada_final   = abap_true
              p_i_projetos        = so_proj[]
              p_i_empresas        = so_emp[]
              p_i_rg_status       = so_st[]  "Lista de Status a desconsiderar
              p_i_num_linhas      = p_maxreg
            IMPORTING
              p_e_ti_ext_def_proj = ti_ext_def_proj.

        CATCH cx_salv_bs_sc_runtime_info.

*         Não é possível recuperar os dados ALV (CN42N)
          MESSAGE i005(zag_mig) DISPLAY LIKE 'E'.
          wa_return-type   = 'E'.
          wa_return-id     = sy-msgid.
          wa_return-number = sy-msgno.
          APPEND wa_return TO ti_return.

      ENDTRY.

      IF ti_ext_def_proj IS INITIAL.

*       Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_def_proj ).
        lo_table->display( ).

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_ext_def_proj TO <t_tab>.
        EXPORT:
          <t_tab>   FROM <t_tab>   TO MEMORY ID 'AG',
          ti_return FROM ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
