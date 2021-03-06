*&---------------------------------------------------------------------*
*& Report ZRAG_CLASSE
*&---------------------------------------------------------------------*
*&  Programa de Extração MM - Classe Materiais
*&  Programador: Wilson Perotta Jose
*&---------------------------------------------------------------------*

REPORT ZRAG_CLASSE.

DATA:
  wa_return                TYPE bapiret2,
  gc_klart                 TYPE KLASSENART,
  lo_table                 TYPE REF TO cl_salv_table,
  ti_return                TYPE TABLE OF bapiret2,
  ti_classe                TYPE TABLE OF m_clasb,
  ti_layout_classe_cockpit TYPE STANDARD TABLE OF zemm_ext_classe,
  wa_layout_classe_cockpit TYPE zemm_ext_classe.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
SELECT-OPTIONS : so_klart  FOR gc_klart MATCHCODE OBJECT h_tcla.
PARAMETERS:
  p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


START-OF-SELECTION.
  TRY .
      SELECT *
        FROM m_clasb
          UP TO p_maxreg ROWS
        INTO TABLE ti_classe
       WHERE spras = 'PT'
         AND klart IN so_klart.


      IF ti_classe IS INITIAL.
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

      MOVE-CORRESPONDING ti_classe TO ti_layout_classe_cockpit.

      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_layout_classe_cockpit ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_layout_classe_cockpit TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
