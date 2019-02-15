*----------------------------------------------------------------------*
***INCLUDE LZGPS_EXTRACAO_NORMAF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  SELECT_OBJECTS_WITH_OBJNR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM select_objects_with_objnr TABLES so-pefis TYPE	ztps_range_aprof
                                      it_objnr STRUCTURE jsto_pre
                                      ct_cobra STRUCTURE cobra
                                      ct_cobrb STRUCTURE cobrb.

  SELECT * FROM cobra INTO TABLE ct_cobra
               FOR ALL ENTRIES IN it_objnr
               WHERE objnr = it_objnr-objnr
               AND   aprof IN so-pefis.

  IF NOT ct_cobra[] IS INITIAL.

    SELECT * FROM cobrb
            INTO TABLE ct_cobrb
            FOR ALL ENTRIES IN ct_cobra
                 WHERE objnr = ct_cobra-objnr.
  ENDIF.

ENDFORM.
