* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_CONV_STATUS_ES                                    *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Converte os Status para Inglês                         *
* Data.............: 22/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920257 | AGIR - Extração de Dados para Migração - Definição Projeto   *
* --------------------------------------------------------------------------*

FUNCTION zfps_conv_status_es.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_RG_STATUS) TYPE  BAPI_ITOB_T_SEL_STATUS
*"  EXPORTING
*"     REFERENCE(P_E_TI_STATUS_EN) TYPE  BU_TJ02T_T
*"----------------------------------------------------------------------

  DATA: ti_status    TYPE bu_tj02t_t,
        ti_status_en TYPE bu_tj02t_t.

*----------------------------------------------------------------*
*----------------------------------------------------------------*
  REFRESH: p_e_ti_status_en.

  IF p_i_rg_status[] IS INITIAL.
    RETURN.
  ENDIF.

  SELECT * INTO TABLE ti_status
*          Textos de status de sistema
           FROM tj02t
*          Índice 001
           WHERE txt04 IN p_i_rg_status
             AND spras EQ sy-langu.

  IF sy-subrc EQ 0.

    SELECT * INTO TABLE ti_status_en
*            Textos de status de sistema
             FROM tj02t
             FOR ALL ENTRIES IN ti_status
             WHERE istat EQ ti_status-istat
               AND spras EQ 'EN'.

    IF sy-subrc EQ 0.
      p_e_ti_status_en[] = ti_status_en[].
    ENDIF.

  ENDIF.

ENDFUNCTION.
