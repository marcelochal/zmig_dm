* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_TAXAS_JUROS                                       *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração das taxas de juros                            *
* Data.............: 03/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920449 | AGIR - Extração de Dados para Migração - Taxas de Juros      *
* --------------------------------------------------------------------------*

REPORT ZRAG_TAXAS_JUROS.

DATA:
  WA_RETURN         TYPE BAPIRET2,
  LO_TABLE          TYPE REF TO CL_SALV_TABLE,
  TI_RETURN         TYPE TABLE OF BAPIRET2,
  TI_V_T056P        TYPE TABLE OF V_T056P,
  TI_EXT_TAXA_JUROS TYPE ZTFI_TAXAS_JUROS,
  E_V_T056P         TYPE V_T056P,
  E_EXT_TAXA_JUROS  TYPE ZEFI_TAXAS_JUROS,
  GC_DATAB          TYPE CHAR10.

FIELD-SYMBOLS <T_TAB>  TYPE STANDARD TABLE.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE TEXT-T00.
PARAMETERS:
  P_MAXREG TYPE /SAPCND/CODE_LINE_NUM OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  P_LOCAL  TYPE C AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK B2.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      "Selecionar taxas de juros
      SELECT
       P~REFERENZ,
       P~DATAB,
       P~ZSOLL,
       R~ZILABEZ,
       R~ZIKUBEZ
       FROM T056P AS P
         LEFT OUTER JOIN T056R AS R ON R~REFERENZ = P~REFERENZ
         UP TO @P_MAXREG  ROWS
       INTO CORRESPONDING FIELDS OF TABLE @TI_V_T056P. "#TOO_MANY_ITAB_FIELDS "#EC CI_BUFFJOIN

      IF TI_V_T056P IS INITIAL.

        "Nenhum dado selecionado para o filtro informado.
        MESSAGE I004(ZAG_MIG) DISPLAY LIKE 'E'.
        WA_RETURN-TYPE   = 'E'.
        WA_RETURN-ID     = SY-MSGID.
        WA_RETURN-NUMBER = SY-MSGNO.
        APPEND WA_RETURN TO TI_RETURN.

      ELSE.

        SORT TI_V_T056P BY REFERENZ DATAB.

        "Converter data
        LOOP AT TI_V_T056P INTO E_V_T056P.
          "Converte data invertida para formato externo DD.MM.AAAA
          CALL FUNCTION 'CONVERSION_EXIT_INVDT_OUTPUT'
            EXPORTING
              INPUT  = E_V_T056P-DATAB
            IMPORTING
              OUTPUT = GC_DATAB.

          MOVE-CORRESPONDING E_V_T056P TO E_EXT_TAXA_JUROS.

          "Formata data em AAAAMMDD
          CONCATENATE GC_DATAB+6(4) GC_DATAB+3(2) GC_DATAB(2) INTO E_EXT_TAXA_JUROS-DATAB.

          APPEND E_EXT_TAXA_JUROS TO TI_EXT_TAXA_JUROS.
        ENDLOOP.

      ENDIF.

      IF P_LOCAL IS NOT INITIAL.

        CL_SALV_TABLE=>FACTORY( IMPORTING R_SALV_TABLE = LO_TABLE CHANGING T_TABLE = TI_EXT_TAXA_JUROS ).
        LO_TABLE->DISPLAY( ).

      ELSE.

        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN TI_EXT_TAXA_JUROS TO <T_TAB>.
        EXPORT:
          <T_TAB>   TO MEMORY ID 'AG',
          TI_RETURN TO MEMORY ID 'RET'.
        LEAVE PROGRAM.

      ENDIF.

    CATCH CX_ROOT.
      MESSAGE E002(ZAG_MIG) WITH SY-REPID.
  ENDTRY.
