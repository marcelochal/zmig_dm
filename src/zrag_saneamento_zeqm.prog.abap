*&---------------------------------------------------------------------*
*& Report ZRAG_SANEAMENTO_ZEQM
*&---------------------------------------------------------------------*
*&  Programa de Extração MM - Classe Materiais ZEQM
*&  Programador: Wilson Perotta Jose
*&---------------------------------------------------------------------*

REPORT ZRAG_SANEAMENTO_ZEQM.

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
  lo_table                 TYPE REF TO cl_salv_table,
  ti_return                TYPE TABLE OF bapiret2,
  ti_ztipouc               TYPE TABLE OF ztipouc,
  wa_ztipouc               TYPE ztipouc,
  ti_ztipbem               TYPE TABLE OF ztipbem,
  wa_ztipbem               TYPE ztipbem,

  ti_zparatr               TYPE TABLE OF zparatr,
  wa_zparatr               TYPE zparatr,

  ti_zcodatr               TYPE TABLE OF zcodatr,
  wa_zcodatr               TYPE zcodatr,

  ti_zforcad               TYPE TABLE OF zforcad,
  wa_zforcad               TYPE zforcad,

  ti_zequmat               TYPE TABLE OF zequmat,
  wa_zequmat               TYPE zequmat,
  ti_mat_imp               TYPE TABLE OF ty_mat,
  ti_mat_exp               TYPE TABLE OF ty_mat_exp,
  wa_mat_exp               TYPE  ty_mat_exp,
  wa_mat                   TYPE ty_mat,
  gc_matnr                 TYPE matnr,
  lc_classe(6)             TYPE C,
  li_index                 TYPE I,
  lc_val(2)                TYPE C,

  ti_layout_zeqm_cockpit TYPE STANDARD TABLE OF zemm_ext_saneamento_zeqm,
  wa_layout_zeqm_cockpit TYPE zemm_ext_saneamento_zeqm.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
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
          FROM zequmat
            UP TO p_maxreg ROWS
          INTO TABLE ti_zequmat
           FOR ALL ENTRIES IN ti_mat_exp
         WHERE matnr = ti_mat_exp-matnr.

        IF ti_zequmat[] IS NOT INITIAL.

           SELECT tuc desctuc
             FROM ztipouc
             INTO CORRESPONDING FIELDS OF TABLE ti_ztipouc
              FOR ALL ENTRIES IN ti_zequmat
            WHERE tuc = ti_zequmat-tuc.

           SELECT tuc tbem desctbem
             FROM ztipbem
             INTO CORRESPONDING FIELDS OF TABLE ti_ztipbem
              FOR ALL ENTRIES IN ti_zequmat
            WHERE tuc  = ti_zequmat-tuc
              AND tbem = ti_zequmat-tbem.

           SELECT *
             FROM zparatr
             INTO TABLE ti_zparatr.

           SELECT *
             FROM zcodatr
             INTO TABLE ti_zcodatr.

           SELECT *
             FROM zforcad
             INTO TABLE ti_zforcad.


         ENDIF.

      ENDIF.

      IF ti_zequmat IS INITIAL.
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


**** Monta dados file

      SORT ti_mat_exp     BY matnr.
      SORT ti_ztipouc     BY tuc.
      SORT ti_ztipbem     BY tuc tbem.
      SORT ti_zparatr     BY tuc tbem.
      SORT ti_zcodatr     BY atributo codigo.
      SORT ti_zforcad     BY cadastro.


      MOVE-CORRESPONDING ti_zequmat TO ti_layout_zeqm_cockpit.

      FIELD-SYMBOLS: <fs_zeqm> TYPE ZEMM_EXT_SANEAMENTO_ZEQM.

      LOOP AT ti_layout_zeqm_cockpit ASSIGNING <fs_zeqm>.

        READ TABLE ti_mat_exp INTO wa_mat_exp WITH KEY matnr = <fs_zeqm>-matnr BINARY SEARCH.

        IF sy-subrc = 0.
           <fs_zeqm>-maktx  = wa_mat_exp-maktx.
        ENDIF.

        READ TABLE ti_ztipouc INTO wa_ztipouc WITH KEY tuc = <fs_zeqm>-tuc BINARY SEARCH.

        IF sy-subrc = 0.
           <fs_zeqm>-desctuc = wa_ztipouc-desctuc.
        ENDIF.

        READ TABLE ti_ztipbem INTO wa_ztipbem WITH KEY tuc = <fs_zeqm>-tuc  tbem = <fs_zeqm>-tbem BINARY SEARCH.

        IF sy-subrc = 0.
           <fs_zeqm>-desctbem = wa_ztipbem-desctbem.
        ENDIF.

        <fs_zeqm>-carac1 = <fs_zeqm>-zaneel+7(2).
        <fs_zeqm>-carac2 = <fs_zeqm>-zaneel+10(2).
        <fs_zeqm>-carac3 = <fs_zeqm>-zaneel+13(2).
        <fs_zeqm>-carac4 = <fs_zeqm>-zaneel+16(2).
        <fs_zeqm>-carac5 = <fs_zeqm>-zaneel+19(2).

        READ TABLE ti_zparatr INTO wa_zparatr WITH KEY tuc = <fs_zeqm>-tuc  tbem = <fs_zeqm>-tbem BINARY SEARCH.

        IF sy-subrc = 0.

            READ TABLE ti_zcodatr INTO wa_zcodatr WITH KEY atributo = wa_zparatr-chavea2 codigo = <fs_zeqm>-carac1  BINARY SEARCH.
            IF sy-subrc = 0.
               <fs_zeqm>-descatributo1 = wa_zcodatr-descatributo.
            ENDIF.

            READ TABLE ti_zcodatr INTO wa_zcodatr WITH KEY atributo = wa_zparatr-chavea3 codigo = <fs_zeqm>-carac2  BINARY SEARCH.
            IF sy-subrc = 0.
               <fs_zeqm>-descatributo2 = wa_zcodatr-descatributo.
            ENDIF.

            READ TABLE ti_zcodatr INTO wa_zcodatr WITH KEY atributo = wa_zparatr-chavea4 codigo = <fs_zeqm>-carac3  BINARY SEARCH.
            IF sy-subrc = 0.
               <fs_zeqm>-descatributo3 = wa_zcodatr-descatributo.
            ENDIF.

            READ TABLE ti_zcodatr INTO wa_zcodatr WITH KEY atributo = wa_zparatr-chavea5 codigo = <fs_zeqm>-carac4  BINARY SEARCH.
            IF sy-subrc = 0.
               <fs_zeqm>-descatributo4 = wa_zcodatr-descatributo.
            ENDIF.

            READ TABLE ti_zcodatr INTO wa_zcodatr WITH KEY atributo = wa_zparatr-chavea6 codigo = <fs_zeqm>-carac5 BINARY SEARCH.
            IF sy-subrc = 0.
               <fs_zeqm>-descatributo5 = wa_zcodatr-descatributo.
            ENDIF.

            READ TABLE ti_zforcad INTO wa_zforcad WITH KEY cadastro = <fs_zeqm>-cadastro BINARY SEARCH.
            IF sy-subrc = 0.
               <fs_zeqm>-descfcad = wa_zforcad-descfcad.
            ENDIF.


        ENDIF.

      ENDLOOP.

      UNASSIGN <fs_zeqm>.

      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_layout_zeqm_cockpit ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_layout_zeqm_cockpit TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
