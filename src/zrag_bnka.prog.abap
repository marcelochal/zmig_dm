*&---------------------------------------------------------------------*
*& Report ZRAG_BNKA
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrag_bnka.
*TYPES:
*  BEGIN OF ty_layout_bank_cockpit, "Cockpit de migração
*    banks LIKE bnka-banks,
*    bankl LIKE bnka-bankl,
*    banka LIKE bnka-banka,
*    provz LIKE bnka-provz,
*    stras LIKE bnka-stras,
*    ort01 LIKE bnka-ort01,
*    swift LIKE bnka-swift,
*    xpgro LIKE bnka-xpgro,
*    bnklz LIKE bnka-bnklz,
*    brnch LIKE bnka-brnch,
*  END OF ty_layout_bank_cockpit.

DATA:
  wa_return              TYPE bapiret2,
  lo_table               TYPE REF TO cl_salv_table,
  ti_return              TYPE TABLE OF bapiret2,
  ti_bnka                TYPE TABLE OF bnka,
  ti_layout_bank_cockpit TYPE STANDARD TABLE OF zeagir_bnka,
  wa_layout_bank_cockpit TYPE zeagir_bnka.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.
  TRY .
      SELECT *
        FROM bnka
          UP TO p_maxreg ROWS
        INTO TABLE ti_bnka.   "#EC CI_NOWHERE.



      IF ti_bnka IS INITIAL.
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

      MOVE-CORRESPONDING ti_bnka TO ti_layout_bank_cockpit.

      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_layout_bank_cockpit ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_layout_bank_cockpit TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
