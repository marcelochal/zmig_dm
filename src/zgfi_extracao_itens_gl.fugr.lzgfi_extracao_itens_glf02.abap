*----------------------------------------------------------------------*
***INCLUDE LZGFI_EXTRACAO_ITENS_GLF02.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  read_bkpf_and_bseg
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_IT_FIELDLIST_INTO  text
*      -->P_I_BUKRS  text
*      -->P_I_READ_BSEG  text
*      <--P_<LT_OPEN_ITEMS>  text
*----------------------------------------------------------------------*
FORM read_bkpf_and_bseg  USING    ut_fieldlist TYPE ttfieldname
                                  ut_fieldlist_into TYPE fagl_t_mig_field_into
                                  u_bukrs
                                  ud_read_bseg
                         CHANGING ct_open_items TYPE table.

  DATA:   ls_field_bseg_into TYPE fagl_s_mig_field_into,
          ld_fieldname TYPE fieldname,
          lt_bkpf_fields TYPE ttfieldname,
          lt_bseg_fields TYPE ttfieldname,
          lt_bkpf TYPE HASHED TABLE OF bkpf WITH UNIQUE KEY bukrs belnr gjahr,
          lt_bseg TYPE HASHED TABLE OF bseg WITH UNIQUE KEY bukrs belnr gjahr buzei,
          ls_bkpf TYPE bkpf,
          ls_bseg TYPE bseg.

  FIELD-SYMBOLS: <ls_open_item> TYPE ANY,
                 <ld_from> TYPE ANY,
                 <ld_to> TYPE ANY,
                 <ls_bkpf> TYPE bkpf,
                 <ls_bseg> TYPE bseg.


*Aufbau der Feldliste für BSEG und BKPF (leider müssen bukrs, belnr etc auch in die feldliste (wobei ja schon aus bs* selektiert))
  LOOP AT ut_fieldlist_into INTO ls_field_bseg_into.
    IF ls_field_bseg_into-tab = 'BKPF'.
      INSERT ls_field_bseg_into-name INTO TABLE lt_bkpf_fields.
    ENDIF.
    CHECK NOT ud_read_bseg IS INITIAL.
    IF ls_field_bseg_into-tab = 'BSEG'.
      READ TABLE ut_fieldlist WITH KEY table_line = ls_field_bseg_into-name TRANSPORTING NO FIELDS.
      CHECK sy-subrc NE 0.
      INSERT ls_field_bseg_into-name INTO TABLE lt_bseg_fields.
    ENDIF.
  ENDLOOP.



  SELECT (lt_bkpf_fields) FROM bkpf INTO CORRESPONDING FIELDS OF TABLE lt_bkpf
    FOR ALL ENTRIES IN ct_open_items
       WHERE bukrs = u_bukrs
        AND ('BELNR = CT_OPEN_ITEMS-BELNR')
        AND ('GJAHR = CT_OPEN_ITEMS-GJAHR').

  IF NOT ud_read_bseg IS INITIAL.

*werden auch für bseg selektion gebraucht
    ld_fieldname = 'BUKRS'.
    INSERT ld_fieldname INTO TABLE lt_bseg_fields.
    ld_fieldname = 'BELNR'.
    INSERT ld_fieldname INTO TABLE lt_bseg_fields.
    ld_fieldname = 'GJAHR'.
    INSERT ld_fieldname INTO TABLE lt_bseg_fields.
    ld_fieldname = 'BUZEI'.
    INSERT ld_fieldname INTO TABLE lt_bseg_fields.

    SELECT (lt_bseg_fields) FROM bseg INTO CORRESPONDING FIELDS OF TABLE lt_bseg
      FOR ALL ENTRIES IN ct_open_items
          WHERE bukrs = u_bukrs
          AND ('BELNR = CT_OPEN_ITEMS-BELNR')
          AND ('GJAHR = CT_OPEN_ITEMS-GJAHR')
          AND ('BUZEI = CT_OPEN_ITEMS-BUZEI').
  ENDIF.

*übernehmen der Felder aus BKPF und BSEG nach OPEN_ITEMS
  LOOP AT ct_open_items ASSIGNING <ls_open_item>.
*    UNASSIGN: <ls_bkpf>, <ld_from>, <ld_to>.
    MOVE-CORRESPONDING <ls_open_item> TO ls_bkpf.
    READ TABLE lt_bkpf ASSIGNING <ls_bkpf> FROM ls_bkpf.
    ASSERT sy-subrc = 0.
    LOOP AT lt_bkpf_fields INTO ld_fieldname.
      CHECK ld_fieldname NE 'BUKRS' AND ld_fieldname NE 'BELNR'
         AND ld_fieldname NE 'GJAHR'.
      ASSIGN COMPONENT ld_fieldname OF STRUCTURE <ls_bkpf> TO <ld_from>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT ld_fieldname OF STRUCTURE <ls_open_item> TO <ld_to>.
      ASSERT sy-subrc = 0.
*      CHECK <ld_from> NE <ld_to>.    "verhindern, dass bereits gefüllte Felder überschrieben werden
      MOVE <ld_from> TO <ld_to>.
    ENDLOOP.

    CHECK NOT ud_read_bseg IS INITIAL.
*    UNASSIGN: <ls_bseg>, <ld_from>, <ld_to>.
    MOVE-CORRESPONDING <ls_open_item> TO ls_bseg.
    READ TABLE lt_bseg ASSIGNING <ls_bseg> FROM ls_bseg.
    ASSERT sy-subrc = 0.
    LOOP AT lt_bseg_fields INTO ld_fieldname.
      CHECK ld_fieldname NE 'BUKRS' AND ld_fieldname NE 'BELNR'
               AND ld_fieldname NE 'GJAHR' AND ld_fieldname NE 'BUZEI'.
      ASSIGN COMPONENT ld_fieldname OF STRUCTURE <ls_bseg> TO <ld_from>.
      ASSERT sy-subrc = 0.
      ASSIGN COMPONENT ld_fieldname OF STRUCTURE <ls_open_item> TO <ld_to>.
      ASSERT sy-subrc = 0.
*      CHECK <ld_from> NE <ld_to>. "verhindern, dass bereits gefüllte Felder überschrieben werden
      MOVE <ld_from> TO <ld_to>.
    ENDLOOP.

  ENDLOOP.

ENDFORM.                    " read_bkpf_and_bseg
