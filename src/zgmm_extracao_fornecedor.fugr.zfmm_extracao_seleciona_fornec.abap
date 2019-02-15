FUNCTION ZFMM_EXTRACAO_SELECIONA_FORNEC.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PI_LIFNR) TYPE  ZEMM_EXTRACAO_LIFNR
*"     REFERENCE(PI_CORTE) TYPE  DATUM
*"  EXPORTING
*"     REFERENCE(PE_LIFNR) TYPE  ZEMM_EXTRACAO_LIFNR
*"----------------------------------------------------------------------

* tem como objetivo selecionar os fornecedores

  TYPES:

     BEGIN OF ty_fornec,
        lifnr TYPE lifnr,
        name1 TYPE name1_gp,
        name2 TYPE name2_gp,
     END OF ty_fornec.


  DATA: it_fornec_mov     TYPE TABLE OF ty_fornec,
        it_fornec_pedido  TYPE TABLE OF ty_fornec,
        it_fornec_projeto TYPE TABLE OF ty_fornec,
        it_ekpo           TYPE TABLE OF ekpo,
        it_jest        TYPE TABLE OF jest,
        it_prps        TYPE TABLE OF prps,
        it_ekkn        TYPE TABLE OF ekkn,
        it_makt        TYPE TABLE OF makt,
        it_lfa1        TYPE TABLE OF lfa1,
        wa_lfa1        TYPE lfa1,
        wa_makt        TYPE makt,
        it_saida       TYPE TABLE OF ty_fornec,
        ld_corte       TYPE d.

  ld_corte = pi_corte.

** Seleciona materiais com saldo em projeto

** Seleciona os peps de projetos lib.
   SELECT objnr
     INTO CORRESPONDING FIELDS OF TABLE it_jest
     FROM jest
    WHERE objnr LIKE 'PR%'
      AND ( stat = 'I0002' OR stat = 'I0042' OR stat = 'I0045' ).

   IF it_jest[] IS NOT INITIAL.

     SELECT PSPNR
       INTO CORRESPONDING FIELDS OF TABLE it_prps
       FROM prps
       FOR ALL ENTRIES IN it_jest
      WHERE objnr = it_jest-objnr.

     IF it_prps[] IS NOT INITIAL.

       SELECT ebeln
         INTO CORRESPONDING FIELDS OF TABLE it_ekkn
         FROM ekkn
         FOR ALL ENTRIES IN it_prps
        WHERE PS_PSP_PNR = it_prps-PSPNR.

        IF it_ekkn[] IS NOT INITIAL.

           SELECT DISTINCT lifnr
             INTO CORRESPONDING FIELDS OF TABLE it_fornec_projeto
             FROM ekko
              FOR ALL ENTRIES IN it_ekkn
            WHERE ebeln = it_ekkn-ebeln
              AND bedat >= ld_corte.

        ENDIF.
     ENDIF.
   ENDIF.

** Selecina materiais com movimento ultimo ano

  SELECT DISTINCT lifnr
    INTO CORRESPONDING FIELDS OF TABLE it_fornec_mov
    FROM mseg
   WHERE budat_mkpf >= ld_corte.

** Seleciona materiais com pedido em aberto

  SELECT DISTINCT ebeln
    INTO CORRESPONDING FIELDS OF TABLE it_ekpo
    FROM ekpo
   WHERE elikz = ''
     AND loekz = ''.

  IF it_ekpo[] IS NOT INITIAL.

    SELECT DISTINCT lifnr
      INTO CORRESPONDING FIELDS OF TABLE it_fornec_pedido
      FROM ekko
       FOR ALL ENTRIES IN it_ekpo
     WHERE ebeln = it_ekpo-ebeln
       AND bedat >= ld_corte.

  ENDIF.

**

  APPEND LINES OF pi_lifnr          TO it_saida.
  APPEND LINES OF it_fornec_mov     TO it_saida.
  APPEND LINES OF it_fornec_projeto TO it_saida.
  APPEND LINES OF it_fornec_pedido  TO it_saida.

  SORT it_saida BY lifnr.
  DELETE it_saida WHERE lifnr = ''.
  DELETE ADJACENT DUPLICATES FROM it_saida COMPARING lifnr.

*** Monta o texto descritivo do Material

  IF it_saida[] IS NOT INITIAL.

    SELECT lifnr name1 name2
      INTO CORRESPONDING FIELDS OF TABLE it_lfa1
      FROM lfa1
       FOR ALL ENTRIES IN it_saida
     WHERE lifnr = it_saida-lifnr.

    SORT it_lfa1 BY lifnr.

    FIELD-SYMBOLS <it_saida> TYPE ZEMM_EXT_LIFNR.

    LOOP AT it_saida ASSIGNING <it_saida>.
      READ TABLE it_lfa1 INTO wa_lfa1 WITH KEY lifnr = <it_saida>-lifnr BINARY SEARCH.
      IF sy-subrc = 0.
        <it_saida>-name1 = wa_lfa1-name1.
        <it_saida>-name2 = wa_lfa1-name2.
      ENDIF.
    ENDLOOP.

    UNASSIGN <it_saida>.

  ENDIF.

  pe_lifnr = it_saida[].

ENDFUNCTION.
