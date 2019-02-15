REPORT zrag_extrator_tasks_list.
*&====================================================================*&
*&                       VARIÁVEIS GLOBAIS                            *&
*&====================================================================*&
TABLES plko.
DATA: o_table  TYPE REF TO cl_salv_table.
FIELD-SYMBOLS <t_tab> TYPE STANDARD TABLE.

INITIALIZATION.
*&====================================================================*&
*&                     TELA DE SELEÇÃO                                *&
*&====================================================================*&
  " Tela de informações de entrada
  SELECTION-SCREEN BEGIN OF BLOCK input_blk WITH FRAME TITLE text-002.
  SELECT-OPTIONS s_plnnr FOR plko-plnnr.
  PARAMETERS: p_maxreg TYPE i OBLIGATORY DEFAULT 100,
              p_local  TYPE c AS CHECKBOX.
  SELECTION-SCREEN END OF BLOCK input_blk.

START-OF-SELECTION.
  "Instanciando o objeto
  DATA(lo_class_info) = NEW zcl_extrator_pm( i_progname = sy-repid i_maxreg = p_maxreg i_rang_list_taf = s_plnnr[] ). "#NEEDED
  lo_class_info->exporta_dados( IMPORTING e_itab_list_taref = DATA(t_list_tarefas) e_itab_return = DATA(t_return) ). "#NEEDED
  SORT t_list_tarefas BY plnnr vornr.
  CASE p_local.
    WHEN abap_true.
      cl_salv_table=>factory( IMPORTING r_salv_table = o_table CHANGING t_table = t_list_tarefas ).
      o_table->display( ).
    WHEN abap_false.
      ASSIGN t_list_tarefas TO <t_tab>.
      EXPORT: <t_tab> FROM <t_tab> TO MEMORY ID 'AG', t_return FROM t_return TO MEMORY ID 'RET'. LEAVE PROGRAM.
  ENDCASE.

END-OF-SELECTION.
