REPORT zrag_extrator_ordem_manut.
*&====================================================================*&
*&                       VARIÁVEIS GLOBAIS                            *&
*&====================================================================*&
TABLES aufk.
DATA: o_table  TYPE REF TO cl_salv_table.
FIELD-SYMBOLS <t_tab> TYPE STANDARD TABLE.

INITIALIZATION.
*&====================================================================*&
*&                     TELA DE SELEÇÃO                                *&
*&====================================================================*&
  " Tela de informações de entrada
  SELECTION-SCREEN BEGIN OF BLOCK input_blk WITH FRAME TITLE text-002.
  SELECT-OPTIONS s_dat FOR aufk-erdat OBLIGATORY NO-EXTENSION.
  PARAMETERS: p_maxreg TYPE i OBLIGATORY DEFAULT 100,
              p_local  TYPE c AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK input_blk.

START-OF-SELECTION.
  "Instanciando o objeto
  DATA(lo_class_info) = NEW zcl_extrator_info_ordem_manut( i_rang_data = s_dat[] i_maxreg = p_maxreg ).
  lo_class_info->exporta_dados( IMPORTING e_itab_ordem_manut = DATA(t_ordens_manut) e_itab_return = DATA(t_return) ).
  SORT t_ordens_manut BY orderid.
  CASE p_local.
    WHEN abap_true.
      cl_salv_table=>factory( IMPORTING r_salv_table = o_table CHANGING t_table = t_ordens_manut ).
      o_table->display( ).
    WHEN abap_false.
      ASSIGN t_ordens_manut TO <t_tab>.
      EXPORT: <t_tab> FROM <t_tab> TO MEMORY ID 'AG', t_return FROM t_return TO MEMORY ID 'RET'. LEAVE PROGRAM.
  ENDCASE.

END-OF-SELECTION.
