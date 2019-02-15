REPORT zrag_csks.

DATA:
  wa_return TYPE bapiret2,
  lo_table  TYPE REF TO cl_salv_table,
  ti_return TYPE TABLE OF bapiret2,
  ti_csks   TYPE TABLE OF zefi_centros_custo.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS:
  p_kokrs  TYPE kokrs MATCHCODE OBJECT csh_tka01 DEFAULT 'TB00'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.



START-OF-SELECTION.
  TRY .
      SELECT *
        FROM csks AS s
          LEFT OUTER JOIN cskt AS t
                       ON t~spras = sy-langu AND
                          t~kokrs = s~kokrs AND
                          t~kostl = s~kostl AND
                          t~datbi = s~datbi
        UP TO p_maxreg  ROWS
        INTO CORRESPONDING FIELDS OF TABLE ti_csks
        WHERE s~kokrs = p_kokrs.  "#EC CI_BUFFJOIN

      IF ti_csks IS INITIAL.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_csks ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_csks TO <t_tab>.
        EXPORT:
          <t_tab>   FROM <t_tab>   TO MEMORY ID 'AG',
          ti_return FROM ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
