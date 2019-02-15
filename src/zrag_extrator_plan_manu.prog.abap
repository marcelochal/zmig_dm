REPORT zrag_extrator_plan_manu.
*&====================================================================*&
*&                       VARIÁVEIS GLOBAIS                            *&
*&====================================================================*&
TABLES mpla.
DATA: o_table  TYPE REF TO cl_salv_table.
FIELD-SYMBOLS <t_tab> TYPE STANDARD TABLE.

INITIALIZATION.
*&====================================================================*&
*&                     TELA DE SELEÇÃO                                *&
*&====================================================================*&
  " Tela de informações de entrada
  SELECTION-SCREEN BEGIN OF BLOCK input_blk WITH FRAME TITLE text-002.
  PARAMETERS: p_maxreg TYPE i OBLIGATORY DEFAULT 100,
              p_local  TYPE c AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK input_blk.

START-OF-SELECTION.
  "Instanciando o objeto
  DATA(lo_class_info) = NEW zcl_extrator_pm( i_progname = sy-repid i_maxreg = p_maxreg ).
  lo_class_info->exporta_dados( IMPORTING e_itab_plan_manut = DATA(t_plan_manut) e_itab_return = DATA(t_return) ).
  SORT t_plan_manut BY warpl.
  CASE p_local.
    WHEN abap_true.
      cl_salv_table=>factory( IMPORTING r_salv_table = o_table CHANGING t_table = t_plan_manut ).
      o_table->display( ).
    WHEN abap_false.
      ASSIGN t_plan_manut TO <t_tab>.
      EXPORT: <t_tab> FROM <t_tab> TO MEMORY ID 'AG', t_return FROM t_return TO MEMORY ID 'RET'. LEAVE PROGRAM.
  ENDCASE.

END-OF-SELECTION.
