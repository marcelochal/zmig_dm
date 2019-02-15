*&---------------------------------------------------------------------*
*& Report ZRAG_FORNECEDOR_GERAL
*&---------------------------------------------------------------------*
*&  Programa de Extração MM - MESTRE FORNECEDOR - VISAO VENDA
*&  Programador: Wilson Perotta Jose
*&---------------------------------------------------------------------*

REPORT ZRAG_FORNECEDOR_GERAL.


TYPES:

  BEGIN OF ty_fornec,
    lifnr TYPE lifnr,
    name1 TYPE name1_gp,
    name2 TYPE name2_gp,
  END OF ty_fornec.


DATA:
  wa_return                TYPE bapiret2,
  gc_lifnr                 TYPE lifnr,
  lo_table                 TYPE REF TO cl_salv_table,
  ti_return                TYPE TABLE OF bapiret2,
  ti_lfa1                  TYPE TABLE OF lfa1,
  ti_adrc                  TYPE TABLE OF adrc,
  wa_adrc                  TYPE adrc,
  ti_adr2                  TYPE TABLE OF adr2,
  wa_adr2                  TYPE adr2,
  ti_adr6                  TYPE TABLE OF adr6,
  wa_adr6                  TYPE adr6,
  ti_fornec_imp            TYPE TABLE OF ty_fornec,
  ti_fornec_exp            TYPE TABLE OF ty_fornec,
  wa_fornec                TYPE ty_fornec,
  li_pos                   TYPE i,
  ti_layout_fornec_cockpit TYPE STANDARD TABLE OF ZEMM_EXT_MAT_FORNEC_GERAL,
  wa_layout_fornec_cockpit TYPE ZEMM_EXT_MAT_FORNEC_GERAL.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
SELECT-OPTIONS : so_lifnr  FOR gc_lifnr MATCHCODE OBJECT KRED_C NO INTERVALS.
PARAMETERS:
  p_corte  TYPE D OBLIGATORY DEFAULT '20170101',
  p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.

  LOOP AT so_lifnr.
    wa_fornec-lifnr = so_lifnr-low.
    APPEND wa_fornec TO ti_fornec_imp.
  ENDLOOP.


CALL FUNCTION 'ZFMM_EXTRACAO_SELECIONA_FORNEC'
  EXPORTING
    PI_LIFNR       = ti_fornec_imp
    PI_CORTE       = p_corte
  IMPORTING
    PE_LIFNR       = ti_fornec_exp.


  TRY .

      IF  ti_fornec_exp[] IS NOT INITIAL.

        SELECT *
          FROM lfa1
            UP TO p_maxreg ROWS
          INTO TABLE ti_lfa1
           FOR ALL ENTRIES IN ti_fornec_exp
         WHERE lifnr = ti_fornec_exp-lifnr.

        IF ti_lfa1[] IS NOT INITIAL.

          SELECT *
            FROM adrc
            INTO TABLE ti_adrc
             FOR ALL ENTRIES IN ti_lfa1
           WHERE addrnumber = ti_lfa1-adrnr
             AND date_from = '00010101'.

          SELECT *
            FROM adr2
            INTO  TABLE ti_adr2
             FOR ALL ENTRIES IN ti_lfa1
           WHERE addrnumber = ti_lfa1-adrnr
             AND date_from  = '00010101'
             AND persnumber = ''
             AND ( flgdefault = 'X' OR r3_user = '3' ).

           SELECT *
            FROM adr6
            INTO  TABLE ti_adr6
             FOR ALL ENTRIES IN ti_lfa1
           WHERE addrnumber = ti_lfa1-adrnr
             AND date_from  = '00010101'
             AND persnumber = ''
             AND flgdefault = 'X'.

        ENDIF.

      ENDIF.

      IF ti_lfa1[] IS INITIAL.
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

      MOVE-CORRESPONDING ti_lfa1 TO ti_layout_fornec_cockpit.

** Monta dados da mara

      FIELD-SYMBOLS: <fs_for> TYPE ZEMM_EXT_MAT_FORNEC_GERAL.

      SORT ti_adrc BY addrnumber.
      SORT ti_adr2 BY addrnumber.
      SORT ti_adr6 BY addrnumber.


      LOOP AT ti_layout_fornec_cockpit ASSIGNING <fs_for>.

*        READ TABLE ti_fornec_exp INTO wa_fornec WITH KEY lifnr = <fs_for>-lifnr BINARY SEARCH.
*        IF sy-subrc = 0.
*            <fs_for>-name1  = wa_fornec-name1.
*            <fs_for>-name2  = wa_fornec-name2.
*        ENDIF.

         READ TABLE ti_adrc INTO wa_adrc WITH KEY addrnumber = <fs_for>-adrnr BINARY SEARCH.
         IF sy-subrc = 0.
            MOVE-CORRESPONDING wa_adrc TO <fs_for>.
         ENDIF.

         LOOP AT ti_adr2 INTO  wa_adr2 WHERE addrnumber = <fs_for>-adrnr.
           IF wa_adr2-flgdefault = 'X'.
             <fs_for>-telnr_long = wa_adr2-tel_number.
           ELSEIF  wa_adr2-r3_user = '3'.
             <fs_for>-mobile_long = wa_adr2-tel_number.
           ENDIF.
         ENDLOOP.

         READ TABLE ti_adr6 INTO wa_adr6 WITH KEY addrnumber = <fs_for>-adrnr BINARY SEARCH.
         IF sy-subrc = 0.
            <fs_for>-smtp_addr = wa_adr6-smtp_addr.
         ENDIF.

      ENDLOOP.

      UNASSIGN <fs_for>.

      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_layout_fornec_cockpit ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_layout_fornec_cockpit TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
