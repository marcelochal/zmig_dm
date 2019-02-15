*&---------------------------------------------------------------------*
*& Report ZRAG_FORNECEDOR_BANCO
*&---------------------------------------------------------------------*
*&  Programa de Extração MM - MESTRE FORNECEDOR - BANCO
*&  Programador: Wilson Perotta Jose
*&---------------------------------------------------------------------*

REPORT ZRAG_FORNECEDOR_BANCO.


TYPES:

  BEGIN OF ty_fornec,
    lifnr TYPE lifnr,
    name1 TYPE name1_gp,
    name2 TYPE name2_gp,
  END OF ty_fornec.


DATA:
  wa_return                TYPE bapiret2,
  gc_lifnr                 TYPE lifnr,
  gc_banks                 TYPE banks,
  lo_table                 TYPE REF TO cl_salv_table,
  ti_return                TYPE TABLE OF bapiret2,
  ti_lfbk                  TYPE TABLE OF lfbk,
  ti_tiban                 TYPE TABLE OF tiban,
  wa_tiban                 TYPE tiban,
  ti_bnka                  TYPE TABLE OF bnka,
  wa_bnka                  TYPE bnka,
  ti_fornec_imp            TYPE TABLE OF ty_fornec,
  ti_fornec_exp            TYPE TABLE OF ty_fornec,
  wa_fornec                TYPE ty_fornec,
  li_pos                   TYPE i,
  ti_layout_fornec_cockpit TYPE STANDARD TABLE OF ZEMM_EXT_MAT_FORNEC_BANCO,
  wa_layout_fornec_cockpit TYPE ZEMM_EXT_MAT_FORNEC_BANCO.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
SELECT-OPTIONS : so_lifnr  FOR gc_lifnr MATCHCODE OBJECT KRED_C NO INTERVALS.
SELECT-OPTIONS : so_banks  FOR gc_banks NO INTERVALS.

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
          FROM lfbk
            UP TO p_maxreg ROWS
          INTO TABLE ti_lfbk
           FOR ALL ENTRIES IN ti_fornec_exp
         WHERE lifnr = ti_fornec_exp-lifnr
           AND banks IN so_banks.

        IF ti_lfbk[] IS NOT INITIAL.

          SELECT banks bankl banka
            INTO CORRESPONDING FIELDS OF TABLE ti_bnka
            FROM bnka
             FOR ALL ENTRIES IN ti_lfbk
           WHERE banks = ti_lfbk-banks
             AND bankl = ti_lfbk-bankl.

        ENDIF.

      ENDIF.

      IF ti_lfbk[] IS INITIAL.
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

      MOVE-CORRESPONDING ti_lfbk TO ti_layout_fornec_cockpit.


      IF ti_layout_fornec_cockpit[] IS NOT INITIAL.

         SELECT banks bankl  bankn iban
           FROM tiban
           INTO CORRESPONDING FIELDS OF TABLE ti_tiban
            FOR ALL ENTRIES IN ti_layout_fornec_cockpit
          WHERE banks = ti_layout_fornec_cockpit-banks
            AND bankl = ti_layout_fornec_cockpit-bankl
            AND bankn = ti_layout_fornec_cockpit-bankn.

      ENDIF.

** Monta dados da mara

      FIELD-SYMBOLS: <fs_for> TYPE ZEMM_EXT_MAT_FORNEC_BANCO.

      SORT ti_fornec_exp BY lifnr.

      SORT ti_tiban BY banks bankl bankn.

      SORT ti_bnka BY banks bankl.

      LOOP AT ti_layout_fornec_cockpit ASSIGNING <fs_for>.

        READ TABLE ti_fornec_exp INTO wa_fornec WITH KEY lifnr = <fs_for>-lifnr BINARY SEARCH.
        IF sy-subrc = 0.
            <fs_for>-name1  = wa_fornec-name1.
            <fs_for>-name2  = wa_fornec-name2.
        ENDIF.

        READ TABLE ti_tiban INTO wa_tiban WITH KEY banks = <fs_for>-banks   bankl = <fs_for>-bankl  bankn = <fs_for>-bankn  BINARY SEARCH.
        IF sy-subrc = 0.
            <fs_for>-iban  = wa_tiban-iban.
        ENDIF.

        READ TABLE ti_bnka INTO wa_bnka WITH KEY banks = <fs_for>-banks   bankl = <fs_for>-bankl BINARY SEARCH.
        IF sy-subrc = 0.
            <fs_for>-banka  = wa_bnka-banka.
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
