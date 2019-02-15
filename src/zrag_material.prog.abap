REPORT ZRAG_MATERIAL.

  data:
    wa_return    type bapiret2,
    lo_table     type ref to cl_salv_table,
    ti_return    type table of bapiret2,
    ti_zmaterial type table of mara.  "zmaterial.

  FIELD-SYMBOLS <t_tab>  type standard table.

  selection-screen begin of block b1 with frame title text-t01.
  parameters:
    p_mstae  type mstae MATCHCODE OBJECT H_T141.
  selection-screen end of block b1.

  selection-screen begin of block b2 with frame title text-t00.
  parameters:
    p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
    p_local  type c as CHECKBOX.
  selection-screen end of block b2.



START-OF-SELECTION.
  TRY .
    select *
      from mara
        up to p_maxreg  rows
      into table ti_zmaterial
     where mstae = p_mstae.

    IF ti_zmaterial is initial.
      message i004(zag_mig) display like 'E'.
      wa_return-type   = 'E'.
      wa_return-id     = sy-msgid.
      wa_return-number = sy-msgno.
      append wa_return to ti_return.

    ENDIF.

    IF p_local is not initial.
      cl_salv_table=>factory( importing r_salv_table = lo_table changing t_table = ti_zmaterial ).
      lo_table->display( ).

    else.
      "AGIR - Extração.
      "Comandos obrigatórios
      "Exporta resultado para Id de Memória
      assign ti_zmaterial to <t_tab>.
      export:
        <t_tab>   to MEMORY ID 'AG',
        ti_return to MEMORY ID 'RET'.
      leave program.
     ENDIF.

  CATCH cx_root.
    message e002(zag_mig) with sy-repid.
  ENDTRY.
