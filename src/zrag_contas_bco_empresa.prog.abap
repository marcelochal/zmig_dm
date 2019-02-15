* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_CONTAS_BCO_EMPRESA                                *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração das contas bancárias                          *
* Data.............: 31/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920398 | AGIR - Extração de Dados para Migração - Contas bcos. empresa*
* --------------------------------------------------------------------------*

report zrag_contas_bco_empresa.

data:
  gc_bukrs        type bukrs,
  wa_return       type bapiret2,
  lo_table        type ref to cl_salv_table,
  ti_return       type table of bapiret2,
  ti_ext_ctas_bco type ztfi_ext_contas_bco_empresa.


field-symbols <t_tab>  type standard table.

selection-screen begin of block b1 with frame title text-t01.

select-options : so_emp  for gc_bukrs matchcode object c_t001.

selection-screen end of block b1.

selection-screen begin of block b2 with frame title text-t00.
parameters:
  p_maxreg type /sapcnd/code_line_num obligatory default 100,     "AGIR - Obrigatório
  p_local  type c as checkbox.
selection-screen end of block b2.

**************************************
**************************************
start-of-selection.
**************************************
**************************************

  try.

      "Obter contas nos bancos da empresa
      call function 'ZFFI_OBTER_CONTAS_BCO_EMPRESA'
        exporting
          p_i_empresas        = so_emp[]
          p_i_num_linhas      = p_maxreg
        importing
          p_e_ti_ext_ctas_bco = ti_ext_ctas_bco.

      if ti_ext_ctas_bco is initial.

        "Nenhum dado selecionado para o filtro informado.
        message i004(zag_mig) display like 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        append wa_return to ti_return.

      endif.

      if p_local is not initial.

        cl_salv_table=>factory( importing r_salv_table = lo_table changing t_table = ti_ext_ctas_bco ).
        lo_table->display( ).

      else.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        assign ti_ext_ctas_bco to <t_tab>.
        export:
          <t_tab>   to memory id 'AG',
          ti_return to memory id 'RET'.
        leave program.

      endif.

    catch cx_root.
      message e002(zag_mig) with sy-repid.
  endtry.
