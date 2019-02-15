REPORT zrag_cepc.

DATA:
  ti_zrag_cepc TYPE TABLE OF ztag_cepc,
  wa_return    TYPE bapiret2,
  lo_table     TYPE REF TO cl_salv_table,
  ti_return    TYPE TABLE OF bapiret2.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS:
  p_kokrs  TYPE kokrs MATCHCODE OBJECT csh_tka01 DEFAULT 'TB00'.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
* Inicio Alteração - MA004818 Marcelo Alvares - 14.08.2018 16:40:46
*  p_maxreg TYPE i OBLIGATORY DEFAULT 7000,     "AGIR - Obrigatório
  p_maxreg TYPE i DEFAULT 7000,
* Fim alteração - MA004818 Marcelo Alvares - 14.08.2018 16:40:52

  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b2.


START-OF-SELECTION.
  TRY .

      SELECT
        a~prctr, a~datbi, a~kokrs, a~datab, a~abtei, a~verak, a~land1, a~anred, a~name1,
        a~name2, a~name3, a~name4, a~ort01, a~ort02, a~stras, a~pfach, a~pstlz, a~pstl2,
        a~spras, a~telbx, a~telf1, a~telf2, a~telfx, a~teltx, a~telx1, a~datlt, a~drnam,
        a~khinr, a~txjcd, a~regio, a~lock_ind, a~pca_template, a~segment,
        a~verak_user, b~bukrs, c~ktext, c~ltext

        FROM cepc             AS a
* Inicio Alteração - MA004818 Marcelo Alvares - 14.08.2018 16:16:08
*        LEFT JOIN cepc_bukrs  AS b
        LEFT JOIN cepc_bukrs  AS b
          ON a~kokrs = b~kokrs AND
             a~prctr = b~prctr

*       LEFT JOIN cepct        AS c
        LEFT JOIN cepct        AS c
          ON a~kokrs = c~kokrs  AND
             a~spras = @sy-langu AND
             a~prctr = c~prctr  AND
             a~datbi = c~datbi

* Fim alteração - MA004818 Marcelo Alvares - 14.08.2018 16:16:15
          UP TO @p_maxreg  ROWS
        INTO CORRESPONDING FIELDS OF TABLE @ti_zrag_cepc
        WHERE a~kokrs = @p_kokrs.                      "#EC CI_BUFFJOIN

      ">>>>> ZAG_MIG: Aqui substitua o teste 0 = 0 pela verificação se a sua tabela não está vazia
      IF ti_zrag_cepc IS INITIAL.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.
        ">>>>> ZAG_MIG: substitua aqui ti_myTab pela tabela interna da seleção de dados
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_zrag_cepc ).
        lo_table->display( ).

      ELSE.
        ">>>>> ZAG_MIG: substitua aqui ti_myTab pela tabela interna da seleção de dados
        ASSIGN ti_zrag_cepc TO <t_tab>.
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
