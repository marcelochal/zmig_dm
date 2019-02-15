FUNCTION zffi_fagl_get_open_items_gl.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_MIGDT) TYPE  BUDAT
*"     VALUE(I_BUKRS) TYPE  BUKRS
*"     VALUE(IT_HKONT) TYPE  FAGL_T_SAKNR
*"     VALUE(IT_FIELDLIST) TYPE  TTFIELDNAME
*"     VALUE(IT_FIELDLIST_INTO) TYPE  FAGL_T_MIG_FIELD_INTO
*"     VALUE(I_READ_BSEG) TYPE  FLAG
*"     VALUE(I_MAX_REG) TYPE  /BCV/QRM_ST_RECORDS_MAX OPTIONAL
*"  EXPORTING
*"     VALUE(ET_OPEN_ITEMS) TYPE  ZTFI_ITENS_ABERTOS_GL
*"----------------------------------------------------------------------
  DATA: ld_gjahr           TYPE gjahr,
        ls_field_bseg_into TYPE fagl_s_mig_field_into,
        lf_fieldname       TYPE fieldname,
        lt_bkpf_fields     TYPE ttfieldname,
        lt_bseg_fields     TYPE ttfieldname,
        lt_bkpf            TYPE SORTED TABLE OF bkpf WITH UNIQUE KEY bukrs belnr gjahr,
        lt_bseg            TYPE SORTED TABLE OF bseg WITH UNIQUE KEY bukrs belnr gjahr buzei,
        ls_bkpf            TYPE bkpf,
        ls_bseg            TYPE bseg.


*  DATA: ls_abap_comp TYPE abap_componentdescr,
*        lt_abap_comp TYPE abap_component_tab,
*        lo_struc_type TYPE REF TO cl_abap_structdescr,
*        lo_tab_type TYPE REF TO cl_abap_tabledescr.
*
*
*
  STATICS: lr_struc_into TYPE REF TO data,
           lr_tab_into   TYPE REF TO data.



  FIELD-SYMBOLS: <ls_open_item>    TYPE any,
                 <lt_open_items>   TYPE table,
                 <ls_open_items_x> TYPE x,
                 <ls_struc_into>   TYPE any,
                 <lf_from>         TYPE any,
                 <lf_to>           TYPE any,
                 <ls_bkpf>         TYPE bkpf,
                 <ls_bseg>         TYPE bseg.


  CHECK NOT it_hkont IS INITIAL.
  ld_gjahr = i_migdt(4).

  CLEAR et_open_items.

  IF NOT <lt_open_items> IS ASSIGNED.     "macht diese abfrage sinn?
*  create dynamic struc type out of 'into' bseg fieldlist:

    PERFORM create_dynamic_table USING it_fieldlist_into
                                 CHANGING lr_struc_into
                                          lr_tab_into.


    ASSIGN: lr_struc_into->* TO <ls_struc_into>,
            lr_tab_into->* TO <lt_open_items>.

*LOOP AT it_fieldlist_into INTO ls_field_bseg_into.
*      CONCATENATE ls_field_bseg_into-tab '-' ls_field_bseg_into-name INTO lf_string.
*
*      MOVE: ls_field_bseg_into-name TO ls_abap_comp-name,
*            cl_abap_typedescr=>describe_by_name( lf_string ) ?TO ls_abap_comp-type.
*      APPEND ls_abap_comp TO lt_abap_comp.
*    ENDLOOP.
*
**    CLEAR ls_abap_comp.
**    ls_abap_comp-name = 'FIX'.
**    ls_abap_comp-type ?= cl_abap_typedescr=>describe_by_name( 'FAGL_S_OPEN_ITEMS_FIX' ).
**    ls_abap_comp-as_include = 'X'.
**    APPEND ls_abap_comp TO lt_abap_comp.
*
*    lo_struc_type = cl_abap_structdescr=>create( lt_abap_comp ).
*    lo_tab_type = cl_abap_tabledescr=>create( lo_struc_type ).
*
*    CREATE DATA: lr_struc_into TYPE HANDLE lo_struc_type,
*                 lr_tab_into TYPE HANDLE lo_tab_type.
*
*    ASSIGN: lr_struc_into->* TO <ls_struc_into>,
*            lr_tab_into->* TO <lt_open_items>.

  ENDIF.

  CLEAR: <lt_open_items>.


  SELECT (it_fieldlist) FROM bsis
     INTO CORRESPONDING FIELDS OF TABLE <lt_open_items>
     UP TO i_max_reg ROWS
     FOR ALL ENTRIES IN it_hkont
     WHERE hkont =  it_hkont-table_line
       AND bukrs =  i_bukrs
       AND gjahr <  ld_gjahr
       AND budat <  i_migdt
       AND xopvw <> space           "only OP accounts FRAGE: KÖNNTE MAN SICH DAS SPAREN, DA JA EH NUR NOCH OP-KONTEN REINKOMMEN?
       AND bstat =  space .




  SELECT (it_fieldlist) FROM bsas
    APPENDING CORRESPONDING FIELDS OF TABLE <lt_open_items>
    FOR ALL ENTRIES IN it_hkont
     WHERE hkont = it_hkont-table_line
      AND bukrs =  i_bukrs
      AND gjahr <  ld_gjahr
      AND budat <  i_migdt
      AND augdt >= i_migdt
      AND bstat =  space.

  CHECK NOT <lt_open_items> IS INITIAL.

*Nachlesen der BKPF und BSEG

  PERFORM read_bkpf_and_bseg USING    it_fieldlist
                                      it_fieldlist_into
                                      i_bukrs
                                      i_read_bseg
                             CHANGING <lt_open_items>.

**Aufbau der Feldliste für BSEG und BKPF (leider müssen bukrs, belnr etc auch in die feldliste (wobei ja schon aus bs* selektiert))
*  LOOP AT it_fieldlist_into INTO ls_field_bseg_into.
*    IF ls_field_bseg_into-tab = 'BKPF'.
*      INSERT ls_field_bseg_into-name INTO TABLE lt_bkpf_fields.
*    ENDIF.
*    IF ls_field_bseg_into-tab = 'BSEG'.
*      INSERT ls_field_bseg_into-name INTO TABLE lt_bseg_fields.
*    ENDIF.
*  ENDLOOP.
*
**werden auch für BSEG selektion gebraucht
*  lf_fieldname = 'BUKRS'.
*  INSERT lf_fieldname INTO TABLE lt_bseg_fields.
*  lf_fieldname = 'BELNR'.
*  INSERT lf_fieldname INTO TABLE lt_bseg_fields.
*  lf_fieldname = 'GJAHR'.
*  INSERT lf_fieldname INTO TABLE lt_bseg_fields.
*
*  SELECT (lt_bkpf_fields) FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
*    FOR ALL ENTRIES IN <lt_open_items>
*       WHERE bukrs = i_bukrs
*        AND ('BELNR = <LT_OPEN_ITEMS>-BELNR')
*        AND ('GJAHR = <LT_OPEN_ITEMS>-GJAHR').
*
*  SELECT (lt_bseg_fields) FROM bseg INTO CORRESPONDING FIELDS OF TABLE lt_bseg
*    FOR ALL ENTRIES IN <lt_open_items>
*        WHERE bukrs = i_bukrs
*        AND ('BELNR = <LT_OPEN_ITEMS>-BELNR')
*        AND ('GJAHR = <LT_OPEN_ITEMS>-GJAHR')
*        AND ('BUZEI = <LT_OPEN_ITEMS>-BUZEI').
*
**übernehmen der Felder aus BKPF und BSEG nach OPEN_ITEMS
*  LOOP AT <lt_open_items> ASSIGNING <ls_open_item>.
*    MOVE-CORRESPONDING <ls_open_item> TO ls_bkpf.
*    READ TABLE lt_bkpf ASSIGNING <ls_bkpf> FROM ls_bkpf.
*    ASSERT sy-subrc = 0.
*    LOOP AT lt_bkpf_fields INTO lf_fieldname.
*      ASSIGN COMPONENT lf_fieldname OF STRUCTURE: <ls_bkpf> TO <lf_from>,
*                                                  <ls_open_item> TO <lf_to>.
*      CHECK <lf_from> NE <lf_to>.    "verhindern, dass bereits gefüllte Felder überschrieben werden
*      MOVE <lf_from> TO <lf_to>.
*    ENDLOOP.
*
*    MOVE-CORRESPONDING <ls_open_item> TO ls_bseg.
*    READ TABLE lt_bseg ASSIGNING <ls_bseg> FROM ls_bseg.
*    ASSERT sy-subrc = 0.
*    LOOP AT lt_bseg_fields INTO lf_fieldname.
*      ASSIGN COMPONENT lf_fieldname OF STRUCTURE: <ls_bseg> TO <lf_from>,
*                                                  <ls_open_item> TO <lf_to>.
*      CHECK <lf_from> NE <lf_to>.
*      MOVE <lf_from> TO <lf_to>.  "verhindern, dass bereits gefüllte Felder überschrieben werden
*    ENDLOOP.
*
*  ENDLOOP.


** Início exclusão -  Migração S4 - Extração de ítens abertos GL - Luís - 13/08/2018 *****
** for rfc-interface, move it to xstring container
*  LOOP AT <lt_open_items> ASSIGNING <ls_open_items_x> CASTING.
*    APPEND <ls_open_items_x> TO et_open_items.
*  ENDLOOP.
** Fim exclusão -  Migração S4 - Extração de ítens abertos GL - Luís - 13/08/2018 *********

** Início inclusão -  Migração S4 - Extração de ítens abertos GL - Luís - 13/08/2018 *****
  DATA le_open_items TYPE zefi_itens_abertos_gl. "bsis. - TESTE

  LOOP AT <lt_open_items> ASSIGNING <ls_open_item>.
    MOVE-CORRESPONDING <ls_open_item> TO le_open_items.
    APPEND le_open_items TO et_open_items.
  ENDLOOP.
** Fim inclusão -  Migração S4 - Extração de ítens abertos GL - Luís - 13/08/2018 *********

ENDFUNCTION.
