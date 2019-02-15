* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Richard de Aquino Rodrigues                            *
* Funcional........: André Santos                                           *
* Módulo...........: PS                                                     *
* Programa.........: ZFPS_COMM_HANDLER_APPLSTACK                            *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Evitar a "rechamada" da tela de seleção do LDB         *
* Data.............: 14/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920257 | AGIR - Extração de Dados para Migração - Definição Projeto   *
* --------------------------------------------------------------------------*

FUNCTION zfps_comm_handler_applstack.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_NOME_PROG_STANDAD) TYPE  RSTI_RONAM
*"----------------------------------------------------------------------

  CONSTANTS con_appl_stack_mem_id(32) VALUE 'COMMUNICATION-HANDLER-APPLSTACK'.

* Application stack
  DATA: it_appl_stack TYPE STANDARD TABLE OF rstirec,
        wa_appl_stack TYPE rstirec.

*--------------------------------------------------*
*--------------------------------------------------*
*--------------------------------------------------------*
* Isso faz a lógica Standard dos Report RPSIS* (CN42N) (CN43N) ...
* não "rechamar" a tela de seleção do
* Banco de Dados Lógico PSJ
* Impede a “rechamada” do próprio programa Standard.
*--------------------------------------------------------*

  wa_appl_stack-rtool = 'RT'.
  wa_appl_stack-rappl = space.
  wa_appl_stack-rsubc = space.
* Nome do programa Standard que será chamado (CN42N) / (CN43N) ...
  wa_appl_stack-ronam = p_i_nome_prog_standad.
  wa_appl_stack-rdest = space.
  wa_appl_stack-rvari = space.
  wa_appl_stack-rtext = text-001. "Resolver problema da exibição da Tela de Seleção do LDB.

  APPEND wa_appl_stack TO it_appl_stack.
  APPEND wa_appl_stack TO it_appl_stack.

* Report RPSISPD000 / RPSISPE000 / ...
*         SUBMIT RPSISSEL00
*            CALL FUNCTION 'REUT_ITEM_LIST_RSTI_CHECK'
*               CALL FUNCTION 'RSTI_APPL_STACK_POP'
*                  PERFORM STACK_POP TABLES IT_APPL_STACK.
*                     IMPORT IT_APPL_STACK FROM MEMORY ID CON_APPL_STACK_MEM_ID.

* Application stack
  EXPORT it_appl_stack FROM it_appl_stack TO MEMORY ID con_appl_stack_mem_id.

ENDFUNCTION.
