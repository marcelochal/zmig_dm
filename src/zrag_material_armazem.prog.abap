*&---------------------------------------------------------------------*
*& Report ZRAG_MATERIAL_ARMAZEM
*&---------------------------------------------------------------------*
*&  Programa de Extração MM - MESTRE MATERIAL - VISAO ARMAZEM
*&  Programador: Wilson Perotta Jose
*&---------------------------------------------------------------------*
REPORT ZRAG_MATERIAL_ARMAZEM.

TYPES:
  BEGIN OF ty_mat_exp,
    matnr TYPE matnr,
    maktx TYPE maktx,
  END OF ty_mat_exp,

  BEGIN OF ty_mat,
    matnr TYPE matnr,
  END OF ty_mat.

DATA:
  wa_return                TYPE bapiret2,
  gc_matnr                 TYPE matnr,
  gc_werks                 TYPE werks_d,
  lo_table                 TYPE REF TO cl_salv_table,
  ti_return                TYPE TABLE OF bapiret2,
  ti_marc                  TYPE TABLE OF marc,
  ti_mat_imp               TYPE TABLE OF ty_mat,
  ti_mat_exp               TYPE TABLE OF ty_mat_exp,
  wa_mat_exp               TYPE ty_mat_exp,
  wa_mat                   TYPE ty_mat,
  li_pos                   TYPE i,
  ti_layout_armazem_cockpit TYPE STANDARD TABLE OF ZEMM_EXT_MAT_ARMAZEM,
  wa_layout_armazem_cockpit TYPE ZEMM_EXT_MAT_ARMAZEM.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
SELECT-OPTIONS : so_werks  FOR gc_werks NO INTERVALS OBLIGATORY.
SELECT-OPTIONS : so_matnr  FOR gc_matnr MATCHCODE OBJECT mat1 NO INTERVALS.

PARAMETERS:
  p_corte  TYPE d OBLIGATORY DEFAULT '20170101',
  p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  LOOP AT so_matnr.
    wa_mat-matnr = so_matnr-low.
    APPEND wa_mat TO ti_mat_imp.
  ENDLOOP.


*** Seleciona o universo de materiais a serem extraidos
  CALL FUNCTION 'ZFMM_EXTRACAO_SELECIONA_MAT'
    EXPORTING
      PI_MATNR       = ti_mat_imp
      PI_CORTE       = p_corte
   IMPORTING
      PE_MATNR       = ti_mat_exp.

  TRY .

      IF  ti_mat_exp[] IS NOT INITIAL.

        SELECT *
          FROM marc
            UP TO p_maxreg ROWS
          INTO TABLE ti_marc
           FOR ALL ENTRIES IN ti_mat_exp
         WHERE matnr = ti_mat_exp-matnr
           AND werks IN so_werks.


      ENDIF.

      IF ti_marc[] IS INITIAL.
        MESSAGE ID 'ZAG_MIG' TYPE 'I' NUMBER 004 DISPLAY LIKE 'E'.

        MOVE sy-msgid TO wa_return-id.
        MOVE sy-msgno TO wa_return-number.
        MOVE sy-msgty TO wa_return-type.
        MOVE sy-msgv1 TO wa_return-message_v1.
        MOVE sy-msgv2 TO wa_return-message_v2.
        MOVE sy-msgv3 TO wa_return-message_v3.
        MOVE sy-msgv4 TO wa_return-message_v4.
        APPEND wa_return TO ti_return.
      ENDIF.

      MOVE-CORRESPONDING ti_marc TO ti_layout_armazem_cockpit.

** Monta dados da mara

      FIELD-SYMBOLS: <fs_mat> TYPE ZEMM_EXT_MAT_ARMAZEM.

      SORT ti_mat_exp BY matnr.

      LOOP AT ti_layout_armazem_cockpit ASSIGNING <fs_mat>.

        READ TABLE ti_mat_exp INTO wa_mat_exp WITH KEY matnr = <fs_mat>-matnr BINARY SEARCH.
        IF sy-subrc = 0.
            <fs_mat>-maktx  = wa_mat_exp-maktx.
        ENDIF.

      ENDLOOP.

      UNASSIGN <fs_mat>.

      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_layout_armazem_cockpit ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_layout_armazem_cockpit TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
