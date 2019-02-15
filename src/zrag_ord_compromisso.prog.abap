* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZRAG_ORD_COMPROMISSO                                   *
* Transação........:                                                        *
* Tipo de Prg......: Report                                                 *
* Objetivo.........: Extração de compromissos para Ordens (KOB2)            *
* Data.............: 17/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920686 | AGIR - Extração de Dados para Migração - H989 - Ord - Comp   *
* --------------------------------------------------------------------------*

REPORT zrag_ord_compromisso.

DATA:
  gc_vbukr        TYPE ps_vbukr,
  gc_txt_status   TYPE j_txt04,
  gd_data         TYPE co_budat,
  gc_ord          LIKE rkpln-aufnr,
  gc_grup         LIKE rkpln-aufgr              ##NEEDED,
  gc_tpord        TYPE aufart,
  wa_return       TYPE bapiret2                 ##NEEDED,
  lo_table        TYPE REF TO cl_salv_table     ##NEEDED,
  ti_return       TYPE TABLE OF bapiret2        ##NEEDED,
  ti_ext_ord_comp TYPE ztps_ext_ord_compromisso ##NEEDED.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE  ##NEEDED.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECTION-SCREEN BEGIN OF BLOCK b3.

SELECT-OPTIONS : so_ord FOR gc_ord
                        MATCHCODE OBJECT orde.

PARAMETERS:      pa_grp LIKE gc_grup.

SELECTION-SCREEN END OF BLOCK b3.

SELECT-OPTIONS : so_tpord FOR gc_tpord MATCHCODE OBJECT auart,
                 so_data  FOR gd_data NO-EXTENSION,
                 so_emp   FOR gc_vbukr MATCHCODE OBJECT c_t001,
                 so_st    FOR gc_txt_status NO INTERVALS MATCHCODE OBJECT zrag_status_syst.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num DEFAULT 100,
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

************************************************
************************************************
AT SELECTION-SCREEN ON BLOCK b3.
************************************************
************************************************

  IF ( NOT so_ord[] IS INITIAL ) AND
     ( NOT pa_grp   IS INITIAL ).

*   Preencher Ordem(s) ou Grupo de ordens
    MESSAGE e009(zag_mig).

  ENDIF.

  IF ( so_ord[] IS INITIAL ) AND
     ( pa_grp   IS INITIAL ).

*   Preencher Ordem(s) ou Grupo de ordens
    MESSAGE e009(zag_mig).

  ENDIF.

************************************************
************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pa_grp.
************************************************
************************************************

  PERFORM f4_groups IN PROGRAM rkaep000 USING    'AUFNR'
                                                 space
                                        CHANGING pa_grp.


************************************************
************************************************
START-OF-SELECTION.
************************************************
************************************************

  TRY.

      TRY.

*         Obter os compromissos para Ordens (KOB2)
          CALL FUNCTION 'ZFPS_OBTER_ORD_COMP'
            EXPORTING
              p_i_so_ordem        = so_ord[]
              p_i_grp_ord         = pa_grp
              p_i_so_empresas     = so_emp[]
              p_i_so_status       = so_st[]    "Lista de Status a desconsiderar
              p_i_so_data_deb     = so_data[]
              p_i_so_tpord        = so_tpord[]
              p_i_num_linhas      = p_maxreg
            IMPORTING
              p_e_ti_ext_ord_comp = ti_ext_ord_comp.

        CATCH cx_salv_bs_sc_runtime_info.

*         Não é possível recuperar os dados ALV (KOB2)
          MESSAGE i005(zag_mig) DISPLAY LIKE 'E'.
          wa_return-type   = 'E'.
          wa_return-id     = sy-msgid.
          wa_return-number = sy-msgno.
          APPEND wa_return TO ti_return.

      ENDTRY.

      IF ti_ext_ord_comp IS INITIAL.

*       Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_ord_comp ).
        lo_table->display( ).

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_ext_ord_comp TO <t_tab>.
        EXPORT:
          <t_tab>   FROM <t_tab>   TO MEMORY ID 'AG',
          ti_return FROM ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH cx_root ##CATCH_ALL.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
