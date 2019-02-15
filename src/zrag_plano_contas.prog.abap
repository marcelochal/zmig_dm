* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_PLANO_CONTAS                                      *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração do plano de contas                            *
* Data.............: 02/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920411 | AGIR - Extração de Dados para Migração - Plano de Contas     *
* --------------------------------------------------------------------------*

REPORT zrag_plano_contas.

DATA:
  gc_bukrs         TYPE bukrs,
  gc_ktopl         TYPE ktopl,
  wa_return        TYPE bapiret2,
  lo_table         TYPE REF TO cl_salv_table,
  ti_return        TYPE TABLE OF bapiret2,
  ti_ext_plano_cta TYPE ztfi_ext_plano_contas.


FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_emp  FOR gc_bukrs MATCHCODE OBJECT c_t001,
                 so_ktopl FOR gc_ktopl MATCHCODE OBJECT h_t004.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      "Selecionar contas
      SELECT
        a~saknr,
        a~ktopl,
        a~xbilk,
        a~sakan,
        a~bilkt,
        a~gvtyp,
        a~ktoks,
        a~mustr,
        a~vbund,
        a~xloev,
        a~xspea,
        a~xspeb,
        a~xspep,
        a~mcod1 ,
        a~func_area,
        b~bukrs,
        b~begru,
        b~busab,
        b~datlz,
        b~fdgrv,
        b~fdlev,
        b~fipls,
        b~fstag,
        b~hbkid,
        b~hktid,
        b~kdfsl,
        b~mitkz,
        b~mwskz,
        b~stext,
        b~vzskz,
        b~waers,
        b~wmeth,
        b~xgkon,
        b~xintb,
        b~xkres,
        b~xloeb,
        b~xnkon,
        b~xopvw,
        b~xspeb,
        b~zindt,
        b~zinrt,
        b~zuawa,
        b~altkt,
        b~xmitk,
        b~recid,
        b~fipos,
        b~xmwno,
        b~xsalh,
        b~bewgp,
        b~infky,
        b~togru,
        b~xlgclr,
        b~mcakey,
        t~txt20,
        t~txt50
        FROM ska1 AS a
        LEFT OUTER JOIN skb1 AS b ON b~saknr = a~saknr "#EC CI_BUFFJOIN
        LEFT OUTER JOIN skat AS t ON t~spras = @sy-langu AND
                                     t~ktopl = a~ska1-ktopl AND
                                     t~saknr = a~ska1-saknr
        INTO TABLE @ti_ext_plano_cta
        UP TO @p_maxreg ROWS
        WHERE a~ktopl IN @so_ktopl AND
              b~bukrs IN @so_emp.

      IF ti_ext_plano_cta IS INITIAL.

        "Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_plano_cta ).
        lo_table->display( ).

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_ext_plano_cta TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
