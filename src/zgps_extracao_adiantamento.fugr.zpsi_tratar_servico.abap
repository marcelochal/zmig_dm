
break abap-axxiom3.

cs_record-ztuc            = esll-extsrvno(3).
cs_record-ztbem           = esll-extsrvno+4(2).
*      cs_record-ziduc           =
cs_record-zsortf          = esll-extsrvno.
cs_record-z_classificacao = esll-extsrvno+7(3).
cs_record-zcm             = afvc-wempf.

*SELECT SINGLE * FROM covp WHERE belnr = cs_record-belnr.
*IF sy-subrc = 0.
*  IF covp-rbest IS INITIAL AND
*     covp-ebeln IS INITIAL.
*
*    CALL FUNCTION 'BAPI_GOODSMVT_GETDETAIL'
*      EXPORTING
*        materialdocument = covp-refbn
*        matdocumentyear  = covp-refgj
*      TABLES
*        goodsmvt_items   = i_itens
*        return           = i_return.
*
*    READ TABLE i_itens INDEX 1.
*
*
*** Utilizar a BAPI NETWORK_COMP_GETDETAIL
*    SELECT SINGLE * FROM resb WHERE rsnum = i_itens-reserv_no AND
*                                    rspos = i_itens-res_item.
*    IF sy-subrc = 0.
*      cs_record-ztuc            = resb-sortf(3).   " TUC
*      cs_record-ztbem           = resb-sortf+4(2). " TIPO DE BEM A1
*      cs_record-ziduc           = resb-ablad.      " IdUC
*      cs_record-zsortf          = resb-sortf.      " criterio de ordenacao
*      cs_record-z_classificacao = resb-sortf+7(3). " classificacao
*      cs_record-zcm             = resb-wempf.      " centro modular
*    ELSE.
*      CLEAR  resb.
*    ENDIF.
CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
  EXPORTING
    input  = cs_record-posid
  IMPORTING
    output = v_posid.
****buscar os campos ODI e tipo de instalacao inicio
*    SELECT SINGLE * FROM prps WHERE pspnr = v_posid.
*    IF sy-subrc IS INITIAL.
*      cs_record-zodi        = prps-usr02.
*      cs_record-zti         = prps-usr03.
*
****buscar os campos ODI e tipo de instalacao fim
****buscar dos dados de equivalência de material inicio
*
*      SELECT SINGLE * FROM zcododi WHERE bukrs = prps-pbukr  AND werks = prps-werks AND
*                                         stand = prps-stort  AND codti = prps-usr03 AND
*                                         odi   = prps-usr02.
*
*SELECT SINGLE * FROM zequmat WHERE matnr = resb-matnr       AND
*                                   tuc   = resb-sortf(3)    AND
*                                   tbem  = resb-sortf+4(2)  AND
*                                   gti   = zcododi-gti.
*      IF sy-subrc IS INITIAL AND ( resb-sortf+7(3) NE 'COM' AND resb-sortf+7(2) NE 'CA' ).
*        cs_record-zform_cad       = zequmat-cadastro.
*        cs_record-zaneel          = zequmat-zaneel.
*        cs_record-zdesc_aneel     = zequmat-descaneel.
*        cs_record-zuar            = zequmat-coduar.
*        cs_record-zuar_desc       = zequmat-descuar.
*      ELSE.
*        IF  resb-sortf IS INITIAL.
*          CLEAR:   cs_record-zodi, cs_record-zti.
*        ENDIF.
*      ENDIF.
*    ENDIF. "IF sy-subrc is INITIAL.ODI e tipo de instalacao
****buscar dos dados de equivalência de material fim
*  ELSE.
*    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_INPUT'
*      EXPORTING
*        input  = cs_record-posid
*      IMPORTING
*        output = v_posid.
*
*    SELECT SINGLE * FROM aufk WHERE pspel = v_posid.
*    IF sy-subrc = 0.
*      SELECT SINGLE * FROM afko WHERE aufnr = aufk-aufnr.
*      IF sy-subrc = 0.
*        SELECT SINGLE * FROM rsdb WHERE rsnum = afko-rsnum      AND
*                                        ebeln = cs_record-ebeln AND
*                                        ebelp = cs_record-ebelp.
*        IF sy-subrc = 0.
*
*          SELECT SINGLE * FROM resb WHERE rsnum = rsdb-rsnum AND
*                                          rspos = rsdb-rspos.
*          IF sy-subrc = 0.
*            cs_record-ztuc            = resb-sortf(3).   " TUC
*            cs_record-ztbem           = resb-sortf+4(2). " TIPO DE BEM A1
*            cs_record-ziduc           = resb-ablad.      " IdUC
*            cs_record-zsortf          = resb-sortf.      " criterio de ordenacao
*            cs_record-z_classificacao = resb-sortf+7(3). " classificacao
*            cs_record-zcm             = resb-wempf.      " centro modular
*          ELSE.
*            CLEAR resb.
*          ENDIF.
*        ELSE.
*          SELECT SINGLE * FROM eban WHERE ebeln = cs_record-ebeln AND
*                                          ebelp = cs_record-ebelp.
*          IF sy-subrc = 0.
*            UNPACK eban-arsps TO v_arsps.
*            SELECT SINGLE * FROM resb WHERE rsnum = eban-arsnr AND
*                                            rspos = v_arsps.
*            IF sy-subrc = 0.
*              cs_record-ztuc            = resb-sortf(3).   " TUC
*              cs_record-ztbem           = resb-sortf+4(2). " TIPO DE BEM A1
*              cs_record-ziduc           = resb-ablad.      " IdUC
*              cs_record-zsortf          = resb-sortf.      " criterio de ordenacao
*              cs_record-z_classificacao = resb-sortf+7(3). " classificacao
*              cs_record-zcm             = resb-wempf.      " centro modular
*            ELSE.
*              CLEAR resb.
*            ENDIF.
*          ENDIF.
*        ENDIF.
***buscar os campos ODI e tipo de instalacao inicio
CLEAR prps.
SELECT SINGLE * FROM prps WHERE pspnr = v_posid.
IF sy-subrc IS INITIAL.
  cs_record-zodi        = prps-usr02.
  cs_record-zti         = prps-usr03.

***buscar os campos ODI e tipo de instalacao fim
***buscar dos dados de equivalência de material inicio
  SELECT SINGLE * FROM zcododi WHERE bukrs = prps-pbukr  AND werks = prps-werks AND
                                     stand = prps-stort  AND codti = prps-usr03 AND
                                     odi   = prps-usr02.

  SELECT SINGLE * FROM zequmat WHERE matnr = esll-srvpos
                                 AND tuc   = esll-extsrvno(3)
                                 AND tbem  = esll-extsrvno+4(2)  AND
                                     gti   = zcododi-gti.
  IF sy-subrc IS INITIAL AND ( esll-extsrvno+7(3) NE 'COM' AND esll-extsrvno+7(2) NE 'CA' ).
    cs_record-zform_cad       = zequmat-cadastro.
    cs_record-zaneel          = zequmat-zaneel.
    cs_record-zdesc_aneel     = zequmat-descaneel.
    cs_record-zuar            = zequmat-coduar.
    cs_record-zuar_desc       = zequmat-descuar.
  ELSE.
    IF  esll-extsrvno IS INITIAL.
      CLEAR:   cs_record-zodi, cs_record-zti.
    ENDIF.
  ENDIF.
  CLEAR prps.
ENDIF. "IF sy-subrc is INITIAL.ODI e tipo de instalacao
****buscar dos dados de equivalência de material fim
*      ENDIF.
*    ENDIF.
*  ENDIF.
*ENDIF.
