* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_VALIDAR_ORD_CR                                    *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Validar a ordem                                        *
* Data.............: 16/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920664 | AGIR - Extração de Dados para Migração - HXX - Ord - C Reais *
* --------------------------------------------------------------------------*

FUNCTION zfps_validar_ord_cr.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_ORDEM) TYPE  AUFNR
*"     REFERENCE(P_I_EMPRESA) TYPE  BUKRS
*"     REFERENCE(P_I_CODIGO_DC) TYPE  BEKNZ
*"     REFERENCE(P_I_SO_EMPRESAS) TYPE  ZTPS_RANGE_VBUKR
*"     REFERENCE(P_I_SO_TPORD) TYPE  MNT_T_AUART_RANGE
*"     REFERENCE(P_I_TI_STATUS_EN) TYPE  BU_TJ02T_T
*"  EXPORTING
*"     REFERENCE(P_E_ORDEM_VALIDADA) TYPE  CHAR01
*"----------------------------------------------------------------------

  DATA: lc_tipo_ordem  TYPE aufart,
        lc_objeto      TYPE j_objnr,
        lc_status_line TYPE sttxt,
        lc_spras       TYPE syst_langu,
        wa_status_en   TYPE tj02t.

*---------------------------------------------------------*
*---------------------------------------------------------*
*  Desconsiderar Status de Sistema

*  Aberto    (I0001 - ABER (PT) - CRTD (EN))
*     Liberado parcialmente (I0042 - LBPA (PT) - PREL (EN))

*  Encerrado (I0046 - ENCE (PT) - CLSD (EN)) e

*--------------------------------------------------*
  p_e_ordem_validada = abap_true.

  IF NOT p_i_empresa IN p_i_so_empresas[].
    CLEAR  p_e_ordem_validada. "Não validado
    RETURN.
  ENDIF.

* Deve ser um Objeto que já foi liquidado
  IF p_i_codigo_dc EQ 'A'.
    CLEAR  p_e_ordem_validada. "Não validado
    RETURN.
  ENDIF.

  SELECT SINGLE auart objnr
         INTO ( lc_tipo_ordem, lc_objeto )
         FROM aufk
         WHERE aufnr EQ p_i_ordem.

  IF sy-subrc NE 0.
    CLEAR  p_e_ordem_validada. "Não validado
    RETURN.
  ENDIF.

  IF NOT lc_tipo_ordem IN p_i_so_tpord[].
    CLEAR  p_e_ordem_validada. "Não validado
    RETURN.
  ENDIF.

  IF NOT p_i_ti_status_en[] IS INITIAL.

*   Obter os Status de Sistema da ordem em Inglês
    lc_spras = 'EN'.
    CALL FUNCTION 'STATUS_TEXT_EDIT'
      EXPORTING
        client           = sy-mandt
        objnr            = lc_objeto
        only_active      = 'X'
        spras            = lc_spras
      IMPORTING
        line             = lc_status_line
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.

    IF sy-subrc <> 0.
      CLEAR  p_e_ordem_validada. "Não validado
      RETURN.
    ENDIF.

*   Para cada Status a ser desconsiderado
    LOOP AT p_i_ti_status_en INTO wa_status_en.

      IF wa_status_en-txt04 EQ 'CRTD'.

*       Se o Objeto está Aberto (Com "Status de sistema" CRTD).
        FIND 'CRTD' IN lc_status_line.
        IF sy-subrc EQ 0.
*         Se o Objeto não está Liberado parcialmente (Com "Status de sistema" PREL).
          FIND 'PREL' IN lc_status_line.
          IF sy-subrc NE 0.
            CLEAR  p_e_ordem_validada. "Não validado
            RETURN.
          ENDIF.
        ENDIF.

      ELSE.

        FIND wa_status_en-txt04 IN lc_status_line.
        IF sy-subrc EQ 0.
          CLEAR  p_e_ordem_validada.   "Não validado
          RETURN.
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDIF.

ENDFUNCTION.
