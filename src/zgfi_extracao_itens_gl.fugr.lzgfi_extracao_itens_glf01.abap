*----------------------------------------------------------------------*
***INCLUDE LZGFI_EXTRACAO_ITENS_GLF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  create_dynamic_table
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDLIST_INTO  text
*      <--P_LR_STRUC_INTO  text
*      <--P_LR_TAB_INTO  text
*----------------------------------------------------------------------*
FORM create_dynamic_table USING  ut_fieldlist_into TYPE fagl_t_mig_field_into
                          CHANGING lr_struc_into
                                   lr_tab_into.



  DATA:  ls_abap_comp       TYPE abap_componentdescr,
         lt_abap_comp       TYPE abap_component_tab,
         lo_struc_type      TYPE REF TO cl_abap_structdescr,
         lo_tab_type        TYPE REF TO cl_abap_tabledescr,
         ls_field_bseg_into TYPE fagl_s_mig_field_into,
         ld_string          TYPE string.


  LOOP AT ut_fieldlist_into INTO ls_field_bseg_into.
    CASE ls_field_bseg_into-name.     "Für Mengen und Betragsfelder die breiten Typen aus der FAGLFLEXT nehmen. Exemplarisch TSLVT
      WHEN 'MENGE'.                                                                    "MENGE
        ls_field_bseg_into-tab = 'FAGLFLEXT'.                                           "MENGE
        ls_field_bseg_into-name = 'MSLVT'.                                              "MENGE
        CONCATENATE ls_field_bseg_into-tab '-' ls_field_bseg_into-name INTO ld_string.  "MENGE
        MOVE: ls_field_bseg_into-name TO ls_abap_comp-name,                             "MENGE
            cl_abap_typedescr=>describe_by_name( ld_string ) ?TO ls_abap_comp-type.     "MENGE
        ls_abap_comp-name = 'MENGE'.                                                    "MENGE
        APPEND ls_abap_comp TO lt_abap_comp.                                            "MENGE
      WHEN 'DMBTR'.
        ls_field_bseg_into-tab = 'FAGLFLEXT'.                                           "MENGE
        ls_field_bseg_into-name = 'TSLVT'.                                              "MENGE
        CONCATENATE ls_field_bseg_into-tab '-' ls_field_bseg_into-name INTO ld_string.  "MENGE
        MOVE: ls_field_bseg_into-name TO ls_abap_comp-name,                             "MENGE
            cl_abap_typedescr=>describe_by_name( ld_string ) ?TO ls_abap_comp-type.     "MENGE
        ls_abap_comp-name = 'DMBTR'.                                                    "MENGE
        APPEND ls_abap_comp TO lt_abap_comp.
      WHEN 'DMBE2'.
        ls_field_bseg_into-tab = 'FAGLFLEXT'.                                           "MENGE
        ls_field_bseg_into-name = 'TSLVT'.                                              "MENGE
        CONCATENATE ls_field_bseg_into-tab '-' ls_field_bseg_into-name INTO ld_string.  "MENGE
        MOVE: ls_field_bseg_into-name TO ls_abap_comp-name,                             "MENGE
            cl_abap_typedescr=>describe_by_name( ld_string ) ?TO ls_abap_comp-type.     "MENGE
        ls_abap_comp-name = 'DMBE2'.                                                    "MENGE
        APPEND ls_abap_comp TO lt_abap_comp.
      WHEN 'DMBE3'.
        ls_field_bseg_into-tab = 'FAGLFLEXT'.                                           "MENGE
        ls_field_bseg_into-name = 'TSLVT'.                                              "MENGE
        CONCATENATE ls_field_bseg_into-tab '-' ls_field_bseg_into-name INTO ld_string.  "MENGE
        MOVE: ls_field_bseg_into-name TO ls_abap_comp-name,                             "MENGE
            cl_abap_typedescr=>describe_by_name( ld_string ) ?TO ls_abap_comp-type.     "MENGE
        ls_abap_comp-name = 'DMBE3'.                                                    "MENGE
        APPEND ls_abap_comp TO lt_abap_comp.
      WHEN 'PSWBT'.
        ls_field_bseg_into-tab = 'FAGLFLEXT'.                                           "MENGE
        ls_field_bseg_into-name = 'TSLVT'.                                              "MENGE
        CONCATENATE ls_field_bseg_into-tab '-' ls_field_bseg_into-name INTO ld_string.  "MENGE
        MOVE: ls_field_bseg_into-name TO ls_abap_comp-name,                             "MENGE
            cl_abap_typedescr=>describe_by_name( ld_string ) ?TO ls_abap_comp-type.     "MENGE
        ls_abap_comp-name = 'PSWBT'.                                                    "MENGE
        APPEND ls_abap_comp TO lt_abap_comp.
      WHEN OTHERS.
        CONCATENATE ls_field_bseg_into-tab '-' ls_field_bseg_into-name INTO ld_string.
        MOVE: ls_field_bseg_into-name TO ls_abap_comp-name,
              cl_abap_typedescr=>describe_by_name( ld_string ) ?TO ls_abap_comp-type.
        APPEND ls_abap_comp TO lt_abap_comp.
    ENDCASE.
  ENDLOOP.

*    CLEAR ls_abap_comp.
*    ls_abap_comp-name = 'FIX'.
*    ls_abap_comp-type ?= cl_abap_typedescr=>describe_by_name( 'FAGL_S_OPEN_ITEMS_FIX' ).
*    ls_abap_comp-as_include = 'X'.
*    APPEND ls_abap_comp TO lt_abap_comp.

  lo_struc_type = cl_abap_structdescr=>create( lt_abap_comp ).
  lo_tab_type = cl_abap_tabledescr=>create( lo_struc_type ).

  CREATE DATA: lr_struc_into TYPE HANDLE lo_struc_type,
               lr_tab_into TYPE HANDLE lo_tab_type.

ENDFORM.                    " create_dynamic_table
