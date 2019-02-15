REPORT zrag_tcurr.

TABLES: tcurr.

DATA:
  gc_kurst  type kurst_curr,
  gc_fcurr  type fcurr_curr,
  ti_tcurr  TYPE TABLE OF tcurr,
  wa_return TYPE bapiret2,
  lo_table  TYPE REF TO cl_salv_table,
  ti_return TYPE TABLE OF bapiret2.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

ASSIGN ti_tcurr TO <t_tab>.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
SELECT-OPTIONS: so_kurst  FOR gc_kurst MATCHCODE OBJECT H_KURST, "Categoria da taxa de câmbio
                so_fcurr  FOR gc_fcurr MATCHCODE OBJECT H_TCURC. "Moeda de procedência
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE i OBLIGATORY DEFAULT 7000,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.
  TRY .

      SELECT *
        FROM tcurr
          UP TO @p_maxreg  ROWS
        INTO CORRESPONDING FIELDS OF TABLE @<t_tab>
        WHERE kurst in @so_kurst and
              fcurr in @so_fcurr.   "#EC CI_GENBUFF

      ">>>>> ZAG_MIG: Aqui substitua o teste 0 = 0 pela verificação se a sua tabela não está vazia
      IF <t_tab> IS INITIAL.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.
        ">>>>> ZAG_MIG: substitua aqui ti_myTab pela tabela interna da seleção de dados
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = <t_tab> ).
        lo_table->display( ).

      ELSE.
        ">>>>> ZAG_MIG: substitua aqui ti_myTab pela tabela interna da seleção de dados
*        ASSIGN ti_zrag_cepc TO <t_tab>.
        "Export para memória - Obrigatório

        IF <t_tab> IS ASSIGNED.
          EXPORT:
              <t_tab>   TO MEMORY ID 'AG',
              ti_return TO MEMORY ID 'RET'.
        ENDIF.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
