FUNCTION ZFMM_EXTRACAO_SELECIONA_MAT .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(PI_MATNR) TYPE  ZFMM_EXTRACAO_MATNR
*"     REFERENCE(PI_CORTE) TYPE  DATUM
*"  EXPORTING
*"     REFERENCE(PE_MATNR) TYPE  ZFMM_EXTRACAO_MATNR_EXP
*"----------------------------------------------------------------------
** tem como objetivo selecionar os materiais que serao extraidos

  TYPES:

     BEGIN OF ty_mat_exp,
        matnr TYPE matnr,
        maktx TYPE maktx,
     END OF ty_mat_exp,


     BEGIN OF ty_mat,
        matnr TYPE matnr,
     END OF ty_mat.


  DATA: it_mat_mov     TYPE TABLE OF ty_mat,
        it_mat_estoq   TYPE TABLE OF ty_mat,
        it_mat_pedido  TYPE TABLE OF ty_mat,
        it_mat_projeto TYPE TABLE OF ty_mat,
        it_mat_ativo   TYPE TABLE OF ty_mat,
        it_jest        TYPE TABLE OF jest,
        it_prps        TYPE TABLE OF prps,
        it_ekko        TYPE TABLE OF ekko,
        it_ekkn        TYPE TABLE OF ekkn,
        it_makt        TYPE TABLE OF makt,
        wa_makt        TYPE makt,
        it_saida       TYPE TABLE OF ty_mat_exp,
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

          SELECT ebeln bedat
            INTO CORRESPONDING FIELDS OF TABLE it_ekko
            FROM ekko
             FOR ALL ENTRIES IN it_ekkn
            WHERE ebeln = it_ekkn-ebeln
              AND bedat >= ld_corte.

          IF it_ekko[] IS NOT INITIAL.

            SELECT DISTINCT matnr
              INTO CORRESPONDING FIELDS OF TABLE it_mat_projeto
              FROM ekpo
               FOR ALL ENTRIES IN it_ekko
             WHERE ebeln = it_ekko-ebeln
               AND knttp = 'P'
               AND elikz = ''
               AND loekz = ''.

           ENDIF.
        ENDIF.
     ENDIF.
   ENDIF.

** Selecina materiais com movimento ultimo ano

  SELECT DISTINCT matnr
    INTO CORRESPONDING FIELDS OF TABLE it_mat_mov
    FROM mseg
   WHERE budat_mkpf >= ld_corte.

** Seleciona materiais com saldo em estoque

  SELECT DISTINCT matnr
    INTO CORRESPONDING FIELDS OF TABLE it_mat_estoq
    FROM mbew
   WHERE lbkum > 0.

** Seleciona materiais com pedido em aberto

  SELECT ebeln bedat
    INTO CORRESPONDING FIELDS OF TABLE it_ekko
    FROM ekko
   WHERE bedat >= ld_corte.

  IF it_ekko[] IS NOT INITIAL.

    SELECT DISTINCT matnr
      INTO CORRESPONDING FIELDS OF TABLE it_mat_pedido
      FROM ekpo
      FOR ALL ENTRIES IN it_ekko
     WHERE ebeln = it_ekko-ebeln
       AND elikz = ''
       AND loekz = ''.

  ENDIF.

** Materiais imobilizados e consumo

*  SELECT DISTINCT matnr
*    INTO CORRESPONDING FIELDS OF TABLE it_mat_ativo
*    FROM mara
*   WHERE mtart = 'NLAG' OR mtart = 'ZAG' OR mtart = 'ZAO'.

**

  APPEND LINES OF pi_matnr       TO it_saida.
  APPEND LINES OF it_mat_mov     TO it_saida.
  APPEND LINES OF it_mat_estoq   TO it_saida.
  APPEND LINES OF it_mat_pedido  TO it_saida.
  APPEND LINES OF it_mat_projeto TO it_saida.
*  APPEND LINES OF it_mat_ativo   TO it_saida.

  SORT it_saida BY matnr.
  DELETE ADJACENT DUPLICATES FROM it_saida COMPARING matnr.

*** Monta o texto descritivo do Material

  IF it_saida[] IS NOT INITIAL.

    SELECT matnr maktx
      INTO CORRESPONDING FIELDS OF TABLE it_makt
      FROM makt
       FOR ALL ENTRIES IN it_saida
     WHERE matnr = it_saida-matnr
       AND spras = 'PT'.

    SORT it_makt BY matnr.

    FIELD-SYMBOLS <it_saida> TYPE ZEMM_EXT_MATNR_EXP.

    LOOP AT it_saida ASSIGNING <it_saida>.
      READ TABLE it_makt INTO wa_makt WITH KEY matnr = <it_saida>-matnr BINARY SEARCH.
      IF sy-subrc = 0.
        <it_saida>-maktx = wa_makt-maktx.
      ENDIF.
    ENDLOOP.

    UNASSIGN <it_saida>.

  ENDIF.

  pe_matnr = it_saida[].

ENDFUNCTION.
