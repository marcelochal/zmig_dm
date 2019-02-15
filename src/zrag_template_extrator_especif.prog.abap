REPORT ZRAG_TEMPLATE_EXTRATOR_ESPECIF.


">>>>> ZAG_MIG: Instruções gerais
">>>>> A tabela fictícia myTab deve ser trocada pela tabela do objeto a extrair


  data:
">>>>> ZAG_MIG: ti_myTab type table of myTab,
    wa_return type bapiret2,
    lo_table  type ref to cl_salv_table,
    ti_return type table of bapiret2.


  FIELD-SYMBOLS <t_tab>  type standard table.

  selection-screen begin of block b1 with frame title text-t01.
*  parameters:
*    p_kokrs  type kokrs MATCHCODE OBJECT CSH_TKA01.
*  select-OPTIONS:
*  so_fld for ...
  selection-screen end of block b1.

">>>>> ZAG_MIG: Esse bloco aqui é obrigatório e não deve ser mexido
  selection-screen begin of block b2 with frame title text-t00.
  parameters:
    p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
    p_local  type c as CHECKBOX.
  selection-screen end of block b2.


START-OF-SELECTION.
  TRY .

">>>>> ZAG_MIG: Codifique aqui sua seleção de dados

*    select *
*      from MYTABLE
*        up to p_maxreg  rows
*      into table ti_MYTABLE
*     where param = p_MyPARAM.


">>>>> ZAG_MIG: Aqui substitua o teste 0 = 0 pela verificação se a sua tabela não está vazia
    IF 0 = 0. "ti_myTab is initial.
      message i004(zag_mig) display like 'E'.
      wa_return-type   = 'E'.
      wa_return-id     = sy-msgid.
      wa_return-number = sy-msgno.
      append wa_return to ti_return.

    ENDIF.

    IF p_local is not initial.
      ">>>>> ZAG_MIG: substitua aqui ti_myTab pela tabela interna da seleção de dados
      "cl_salv_table=>factory( importing r_salv_table = lo_table changing t_table = ti_myTab ).
      lo_table->display( ).

    else.
      ">>>>> ZAG_MIG: substitua aqui ti_myTab pela tabela interna da seleção de dados
      ">>>>> ZAG_MIG: assign ti_myTab to <t_tab>.
      "Export para memória - Obrigatório
      export:
        <t_tab>   to MEMORY ID 'AG',
        ti_return to MEMORY ID 'RET'.
      leave program.
     ENDIF.

  CATCH cx_root.
    message e002(zag_mig) with sy-repid.
  ENDTRY.
