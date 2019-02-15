* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_VALIDAR_DEF_PROJETOS                              *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Validar a definição de projeto                         *
* Data.............: 16/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920257 | AGIR - Extração de Dados para Migração - Definição Projeto   *
* --------------------------------------------------------------------------*

FUNCTION zfps_validar_def_projetos.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_PROJECT_DEFINITION) TYPE  PS_PSPID
*"     REFERENCE(P_I_TI_STATUS_EN) TYPE  BU_TJ02T_T
*"  EXPORTING
*"     REFERENCE(P_E_PROJETO_VALIDADO) TYPE  CHAR01
*"----------------------------------------------------------------------

  DATA: wa_return        TYPE bapireturn1,
        ti_system_status TYPE STANDARD TABLE OF bapi_system_status,
        wa_status_en     TYPE tj02t.

*---------------------------------------------------------*
*---------------------------------------------------------*
*  Desconsiderar Status de Sistema

*  Aberto    (I0001 - ABER (PT) - CRTD (EN))
*     Liberado parcialmente (I0042 - LBPA (PT) - PREL (EN))

*  Encerrado (I0046 - ENCE (PT) - CLSD (EN)) e

*--------------------------------------------------*
  p_e_projeto_validado = abap_true.

  IF p_i_ti_status_en[] IS INITIAL.
    RETURN.
  ENDIF.

* Obter os Status de Sistema da Definição de Projeto.
  CALL FUNCTION 'BAPI_BUS2001_GET_STATUS'
    EXPORTING
      project_definition = p_i_project_definition
    IMPORTING
      return             = wa_return
    TABLES
      e_system_status    = ti_system_status.

  IF wa_return-type EQ 'E'.
    CLEAR  p_e_projeto_validado. "Não validado
    RETURN.
  ENDIF.

* Para cada Status a ser desconsiderado
  LOOP AT p_i_ti_status_en INTO wa_status_en.

    IF wa_status_en-txt04 EQ 'CRTD'.

*    Se o Objeto está Aberto (Com "Status de sistema" CRTD).
      FIND 'CRTD' IN TABLE ti_system_status.
      IF sy-subrc EQ 0.
*      Se o Objeto não está Liberado parcialmente (Com "Status de sistema" PREL).
        FIND 'PREL' IN TABLE ti_system_status.
        IF sy-subrc NE 0.
          CLEAR  p_e_projeto_validado. "Não validado
          RETURN.
        ENDIF.
      ENDIF.

    ELSE.

      FIND wa_status_en-txt04 IN TABLE ti_system_status.
      IF sy-subrc EQ 0.
        CLEAR  p_e_projeto_validado.   "Não validado
        RETURN.
      ENDIF.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.
