* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_VALIDAR_PEP                                       *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Validar o PEP                                          *
* Data.............: 16/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920275 | AGIR - Extração de Dados para Migração - PEPs                *
* --------------------------------------------------------------------------*

FUNCTION zfps_validar_pep.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_PEP) TYPE  PS_POSID
*"     REFERENCE(P_I_TI_STATUS_EN) TYPE  BU_TJ02T_T
*"  EXPORTING
*"     REFERENCE(P_E_PEP_VALIDADO) TYPE  CHAR01
*"----------------------------------------------------------------------

  DATA: wa_return        TYPE bapireturn1,
        ti_system_status TYPE STANDARD TABLE OF bapi_wbs_system_status,
        ti_wbs_elements  TYPE STANDARD TABLE OF bapi_wbs_elements,
        wa_status_en     TYPE tj02t.

*---------------------------------------------------------*
*---------------------------------------------------------*
*  Desconsiderar Status de Sistema
*  Encerrado (I0046 - ENCE (PT) - CLSD (EN)) e
*  Aberto    (I0001 - ABER (PT) - CRTD (EN))

*--------------------------------------------------*
  p_e_pep_validado = abap_true.

* if P_I_EMPRESA



  IF  p_i_ti_status_en[] IS INITIAL.
    RETURN.
  ENDIF.

  APPEND p_i_pep TO ti_wbs_elements.

* Obter os Status de Sistema do PEP
  CALL FUNCTION 'BAPI_BUS2054_GET_STATUS'
    IMPORTING
      return          = wa_return
    TABLES
      i_wbs_elements  = ti_wbs_elements
      e_system_status = ti_system_status.

  IF wa_return-type EQ 'E'.
    CLEAR  p_e_pep_validado. "Não validado
    RETURN.
  ENDIF.

* Para cada Status a ser desconsiderado
  LOOP AT  p_i_ti_status_en INTO wa_status_en.

    IF wa_status_en-txt04 EQ 'CRTD'.

*     Se o Objeto está Aberto (Com "Status de sistema" CRTD).
      FIND 'CRTD' IN TABLE ti_system_status.
      IF sy-subrc EQ 0.
*       Se o Objeto não está Liberado parcialmente (Com "Status de sistema" PREL).
        FIND 'PREL' IN TABLE ti_system_status.
        IF sy-subrc NE 0.
          CLEAR  p_e_pep_validado. "Não validado
          RETURN.
        ENDIF.
      ENDIF.

    ELSE.

      FIND wa_status_en-txt04 IN TABLE ti_system_status.
      IF sy-subrc EQ 0.
        CLEAR  p_e_pep_validado.   "Não validado
        RETURN.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.
