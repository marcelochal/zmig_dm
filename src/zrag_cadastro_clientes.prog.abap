* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_CADASTRO_CLIENTES                                 *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração do cadastro de clientes                       *
* Data.............: 18/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920711 | AGIR - Extração de Dados para Migração - Cadstro de clientes *
* --------------------------------------------------------------------------*

REPORT zrag_cadastro_clientes.

TYPES BEGIN OF type_gerais.
        INCLUDE STRUCTURE zefi_dados_gerais_cliente.
TYPES   stcd1	TYPE stcd1.
TYPES   stcd2	TYPE stcd2.
TYPES   stcd3	TYPE stcd3.
TYPES   stcd4	TYPE stcd4.
TYPES   name1 TYPE name1_gp.
TYPES   name2 TYPE name2_gp.
TYPES   name3 TYPE name3_gp.
TYPES   name4 TYPE name4_gp.
TYPES END OF type_gerais.

TYPES type_ti_gerais TYPE TABLE OF type_gerais.

DATA:
  gc_bukrs          TYPE bukrs,
  gc_ktokd          TYPE ktokd,
  gc_vkorg          TYPE vkorg,
  gc_vtweg          TYPE vtweg,
  gc_spart          TYPE spart,
*  wa_return        TYPE bapiret2,
  lo_table          TYPE REF TO cl_salv_table,
*  ti_return        TYPE TABLE OF bapiret2,
  ti_ext_gerais     TYPE ztfi_dados_gerais_cliente,
  ti_ext_vendas     TYPE ztfi_dados_vendas,
  ti_ext_parceiros  TYPE ztfi_dados_parceiros,
  ti_ext_empresa    TYPE ztfi_dados_empresa,
  ti_ext_cobranca   TYPE ztfi_dados_cobranca,
  ti_ext_banco      TYPE ztfi_dados_banco,
  ti_ext_impostos   TYPE ztfi_dados_impostos,
  ti_ext_contato    TYPE ztfi_pessoa_contato,
  ti_ext_taxnumbers TYPE ztfi_tax_numbers,
  ti_gerais         TYPE type_ti_gerais.


*FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_ktokd FOR gc_ktokd MATCHCODE OBJECT h_t077d,
                 so_emp   FOR gc_bukrs MATCHCODE OBJECT c_t001,
                 so_vkorg FOR gc_vkorg MATCHCODE OBJECT h_tvko,
                 so_vtweg FOR gc_vtweg MATCHCODE OBJECT csh_tvtw,
                 so_spart FOR gc_spart MATCHCODE OBJECT csh_tspa.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num OBLIGATORY DEFAULT 2109999999,     "AGIR - Obrigatório
  p_fname  TYPE localfile MODIF ID p1,
  p_local  TYPE c AS CHECKBOX.

SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

  CONSTANTS:
    c_prefix_fe(20) TYPE c         VALUE 'C:\temp\extract_' ##NO_TEXT,
    c_sufix(20)     TYPE c         VALUE '.xlsx'            ##NO_TEXT.

  CONCATENATE c_prefix_fe 'FI_87' '_' sy-sysid sy-mandt
              '_' sy-datlo sy-timlo c_sufix INTO p_fname.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      "Selecionar dados gerais dos clientes
      PERFORM selecionar_dados_gerais TABLES   so_emp
                                               so_ktokd
                                      USING    p_maxreg
                                      CHANGING ti_gerais
                                               ti_ext_gerais.

      "Selecionar dados de vendas
      PERFORM selecionar_dados_vendas TABLES   so_emp
                                               so_vkorg
                                               so_vtweg
                                               so_spart
                                      USING    p_maxreg
                                      CHANGING ti_ext_vendas.

      "Selecionar parceiros
      PERFORM selecionar_parceiros TABLES   so_emp
                                            so_vkorg
                                            so_vtweg
                                            so_spart
                                   USING    p_maxreg
                                   CHANGING ti_ext_parceiros.

      "Selecionar dados de empresa
      PERFORM selecionar_dados_empresa TABLES   so_emp
                                       USING    p_maxreg
                                       CHANGING ti_ext_empresa.

      "Selecionar dados de cobrança
      PERFORM selecionar_dados_cobranca TABLES   so_emp
                                        USING    p_maxreg
                                        CHANGING ti_ext_cobranca.

      "Selecionar dados de banco
      PERFORM selecionar_dados_banco TABLES   so_emp
                                     USING    p_maxreg
                                     CHANGING ti_ext_banco.

      "Selecionar indicadores de impostos
      PERFORM selecionar_impostos TABLES   so_emp
                                  USING    p_maxreg
                                  CHANGING ti_ext_impostos.

      "Selecionar pessoas de contato
      PERFORM selecionar_contato TABLES   so_emp
                                  USING    p_maxreg
                                  CHANGING ti_ext_contato.

      "Montar saída tax numbers
      PERFORM montar_tax_numbers CHANGING ti_gerais
                                          ti_ext_taxnumbers.

      IF ti_ext_gerais IS INITIAL.

        "Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
*        wa_return-type   = 'E'.
*        wa_return-id     = sy-msgid.
*        wa_return-number = sy-msgno.
*        APPEND wa_return TO ti_return.

      ENDIF.

      IF p_local IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_gerais ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        PERFORM despacha_extracao.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_GERAIS
*&---------------------------------------------------------------------*
*   Selecionar dados gerais dos clientes
*----------------------------------------------------------------------*
FORM selecionar_dados_gerais  TABLES   p_ti_emp    STRUCTURE so_emp
                                       p_ti_ktokd  STRUCTURE so_ktokd
                              USING    p_maxreg    TYPE /sapcnd/code_line_num
                              CHANGING p_ti_gerais TYPE type_ti_gerais
                                       p_ti_ext_gerais TYPE ztfi_dados_gerais_cliente.

  DATA: li_last      TYPE i,
        e_gerais     TYPE type_gerais,
        e_ext_gerais TYPE zefi_dados_gerais_cliente.

  FIELD-SYMBOLS <itab> TYPE any.

  SELECT  a~kunnr,
          a~ktokd,
          a~anred,
          c~sort1,
          c~sort2,
          a~lifnr,
          a~konzs,
          a~vbund,
          c~street,
          c~house_num1,
          c~city2,
          c~home_city,
          c~post_code1,
          c~city1,
          c~country,
          c~region,
          a~lzone,
          c~building,
          c~roomnumber,
          c~floor,
          c~name_co,
          c~str_suppl1,
          c~str_suppl2,
          c~str_suppl3,
          c~location,
          c~taxjurcode,
          c~po_box,
          c~post_code2,
          c~po_box_loc,
          c~po_box_cty,
          c~po_box_reg,
          c~post_code3,
          r2~telnr_long,
          r3~faxnr_long,
          r6~smtp_addr,
          a~dtams,
          a~dtaws,
          a~knrza,
          a~niels,
          a~rpmkr,
          a~kukla,
          a~hzuor,
          a~bran1,
          a~bran2,
          a~bran3,
          a~bran4,
          a~bran5,
          a~sperr,
          a~stcd1,
          a~stcd2,
          a~stcd3,
          a~stcd4,
          a~name1,
          a~name2,
          a~name3,
          a~name4
    FROM kna1 AS a
*    LEFT OUTER JOIN knb1 AS b  ON b~kunnr = a~kunnr
    LEFT OUTER JOIN adrc AS c  ON c~addrnumber  = a~adrnr
    LEFT OUTER JOIN adr2 AS r2 ON r2~addrnumber = a~adrnr AND r2~persnumber = @space
    LEFT OUTER JOIN adr3 AS r3 ON r3~addrnumber = a~adrnr AND r3~persnumber = @space
    LEFT OUTER JOIN adr6 AS r6 ON r6~addrnumber = a~adrnr AND r6~persnumber = @space
    INTO CORRESPONDING FIELDS OF TABLE @p_ti_gerais
    UP TO @p_maxreg ROWS
*    WHERE b~bukrs IN @p_ti_emp AND
      WHERE a~kunnr IN ( SELECT kunnr FROM knb1 AS b  WHERE b~kunnr = a~kunnr AND b~bukrs IN @p_ti_emp )
    AND a~ktokd IN @p_ti_ktokd   ##TOO_MANY_ITAB_FIELDS.

  LOOP AT p_ti_gerais INTO e_gerais.

    MOVE-CORRESPONDING e_gerais TO e_ext_gerais.

    "Atribuir grupo de conta do cliente ao grupo do BP
    e_ext_gerais-bu_group = e_ext_gerais-ktokd.

    "Preenche nome da pessoa física
    IF e_ext_gerais-ktokd EQ 'NACF'. "Pessoa Física

      SPLIT e_gerais-name1 AT space INTO TABLE DATA(itab).
      DESCRIBE TABLE itab LINES li_last.

      LOOP AT itab ASSIGNING <itab>.
        CASE sy-tabix.
          WHEN 1.
            e_ext_gerais-name_first = <itab>.
          WHEN li_last.
            e_ext_gerais-name_last = <itab>.
          WHEN OTHERS.
            CONCATENATE e_ext_gerais-name_middle <itab> INTO e_ext_gerais-name_middle SEPARATED BY space.
        ENDCASE.
      ENDLOOP.

      "Preenche campo NAMORG com nome da empresa quando for Pessoa Juridica
    ELSE.
      e_ext_gerais-namorg1 = e_gerais-name1.
      e_ext_gerais-namorg2 = e_gerais-name2.
      e_ext_gerais-namorg3 = e_gerais-name3.
      e_ext_gerais-namorg4 = e_gerais-name4.
    ENDIF.

    APPEND e_ext_gerais TO p_ti_ext_gerais.

  ENDLOOP.

  SORT p_ti_ext_gerais BY kunnr.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_VENDAS
*&---------------------------------------------------------------------*
*  Selecionar dados de vendas
*----------------------------------------------------------------------*
FORM selecionar_dados_vendas  TABLES   p_ti_emp   STRUCTURE   so_emp
                                       p_ti_vkorg STRUCTURE so_vkorg
                                       p_ti_vtweg STRUCTURE so_vtweg
                                       p_ti_spart STRUCTURE so_spart
                              USING    p_maxreg   TYPE /sapcnd/code_line_num
                              CHANGING p_ti_ext_vendas TYPE ztfi_dados_vendas.


  SELECT  v~kunnr, v~vkorg, v~vtweg, v~spart, v~kdgrp,
          v~bzirk, v~vkbur, v~vkgrp, v~eikto, v~awahr, v~klabc,
          v~waers, v~konda, v~pltyp, v~kalks, v~versg, v~lprio,
          v~kzazu, v~vsbed, v~vwerk, v~autlf, v~kztlf, v~perfk,
          v~inco1, v~inco2, v~zterm, v~ktgrd, v~aufsd, v~lifsd, v~faksd
   FROM knvv AS v
*   LEFT OUTER JOIN knb1 AS b ON b~kunnr = v~kunnr
   INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_vendas
   UP TO @p_maxreg ROWS
*   WHERE b~bukrs IN @p_ti_emp AND
   WHERE v~kunnr IN ( SELECT kunnr FROM knb1 AS b  WHERE b~kunnr = v~kunnr AND b~bukrs IN @p_ti_emp ) AND
         v~vkorg IN @p_ti_vkorg AND
         v~vtweg IN @p_ti_vtweg AND
         v~spart IN @p_ti_spart     ##TOO_MANY_ITAB_FIELDS.

  SORT p_ti_ext_vendas BY kunnr vkorg vtweg spart.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_PARCEIROS
*&---------------------------------------------------------------------*
*  Selecionar parceiros
*----------------------------------------------------------------------*
FORM selecionar_parceiros  TABLES   p_ti_emp   STRUCTURE   so_emp
                                    p_ti_vkorg STRUCTURE so_vkorg
                                    p_ti_vtweg STRUCTURE so_vtweg
                                    p_ti_spart STRUCTURE so_spart
                           USING    p_maxreg   TYPE /sapcnd/code_line_num
                           CHANGING p_ti_ext_parceiros TYPE ztfi_dados_parceiros.

  SELECT  p~kunnr,p~vkorg,p~vtweg,p~spart,p~parvw,
          p~kunn2,p~lifnr,p~pernr,p~parnr,p~knref,
          p~defpa
   FROM knvp AS p
*   LEFT OUTER JOIN knb1 AS b ON b~kunnr = p~kunnr
   INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_parceiros
   UP TO @p_maxreg ROWS
*   WHERE b~bukrs IN @p_ti_emp AND
   WHERE p~kunnr IN ( SELECT kunnr FROM knb1 AS b  WHERE b~kunnr = p~kunnr AND b~bukrs IN @p_ti_emp ) AND
         p~vkorg IN @p_ti_vkorg AND
         p~vtweg IN @p_ti_vtweg AND
         p~spart IN @p_ti_spart.

  SORT p_ti_ext_parceiros BY kunnr vkorg vtweg spart parvw.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_EMPRESA
*&---------------------------------------------------------------------*
*  Selecionar dados de empresa
*----------------------------------------------------------------------*
FORM selecionar_dados_empresa  TABLES   p_ti_emp   STRUCTURE   so_emp
                               USING    p_maxreg   TYPE /sapcnd/code_line_num
                               CHANGING p_ti_ext_empresa TYPE ztfi_dados_empresa.

  SELECT  b1~kunnr, b1~bukrs, b1~sperr, b1~zahls, b1~altkn,
          b5~mahna, b5~mansp, b5~knrma, b5~madat, b5~gmvdt,
          b5~mahns, b1~busab, b1~eikto, b1~tlfns, b1~tlfxs,
          b1~xdezv, b1~kverm, b1~perkz, b1~zterm, b1~zwels,
          b1~hbkid, b1~togru, b1~xverr, b1~xzver, b1~xpore,
          b1~xedip, b1~knrzb, b1~akont, b1~knrze, b1~zuawa,
          b1~fdgrv
   FROM knb1 AS b1
   LEFT OUTER JOIN knb5 AS b5 ON b5~kunnr = b1~kunnr AND
                                 b5~bukrs = b1~bukrs
   INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_empresa    ##TOO_MANY_ITAB_FIELDS
   UP TO @p_maxreg ROWS
   WHERE b1~bukrs IN @p_ti_emp.

  SORT p_ti_ext_empresa BY kunnr bukrs.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_COBRANCA
*&---------------------------------------------------------------------*
*  Selecionar dados de cobrança
*----------------------------------------------------------------------*
FORM selecionar_dados_cobranca  TABLES   p_ti_emp   STRUCTURE   so_emp
                                USING    p_maxreg   TYPE /sapcnd/code_line_num
                                CHANGING p_ti_ext_cobranca TYPE ztfi_dados_cobranca.

  SELECT b5~kunnr, b5~bukrs, b5~maber, b5~mahna, b5~mansp, b5~mahns
   FROM knb5 AS b5
*   LEFT OUTER JOIN knb1 AS b1 ON b1~kunnr = b5~kunnr AND
*                                 b1~bukrs = b5~bukrs
   INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_cobranca
   UP TO @p_maxreg ROWS
*   WHERE b1~bukrs IN @p_ti_emp.
   WHERE b5~kunnr IN ( SELECT kunnr FROM knb1 AS b1  WHERE b1~kunnr = b5~kunnr AND b1~bukrs IN @p_ti_emp ).

  SORT p_ti_ext_cobranca BY kunnr bukrs maber.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_BANCO
*&---------------------------------------------------------------------*
*  Selecionar dados de banco
*----------------------------------------------------------------------*
FORM selecionar_dados_banco  TABLES   p_ti_emp   STRUCTURE   so_emp
                             USING    p_maxreg   TYPE /sapcnd/code_line_num
                             CHANGING p_ti_ext_banco TYPE ztfi_dados_banco.

  SELECT  k~kunnr, k~banks, k~bankl, k~bankn, k~bkont,
          k~xezer, k~bkref, k~koinh, k~ebpp_accname
   FROM knbk AS k
*   LEFT OUTER JOIN knb1 AS b ON b~kunnr = k~kunnr
   INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_banco
   UP TO @p_maxreg ROWS
*   WHERE b~bukrs IN @p_ti_emp   ##TOO_MANY_ITAB_FIELDS.
   WHERE k~kunnr IN ( SELECT kunnr FROM knb1 AS b  WHERE b~kunnr = k~kunnr AND b~bukrs IN @p_ti_emp ).

  SORT p_ti_ext_banco BY kunnr banks bankl bankn iban.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_IMPOSTOS
*&---------------------------------------------------------------------*
*  Selecionar indicadores de impostos
*----------------------------------------------------------------------*
FORM selecionar_impostos  TABLES   p_ti_emp   STRUCTURE   so_emp
                          USING    p_maxreg   TYPE /sapcnd/code_line_num
                          CHANGING p_ti_ext_impostos TYPE ztfi_dados_impostos.

  SELECT i~kunnr,
         i~aland,
         i~tatyp,
         i~taxkd
  FROM knvi AS i
  LEFT OUTER JOIN knb1 AS b ON b~kunnr = i~kunnr
  INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_impostos
  UP TO @p_maxreg ROWS
  WHERE i~kunnr IN ( SELECT kunnr FROM knb1 AS b  WHERE b~kunnr = i~kunnr AND b~bukrs IN @p_ti_emp ).

  SORT p_ti_ext_impostos BY kunnr aland.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_CONTATO
*&---------------------------------------------------------------------*
*  Selecionar pessoas de contato
*----------------------------------------------------------------------*
FORM selecionar_contato  TABLES   p_ti_emp   STRUCTURE   so_emp
                         USING    p_maxreg   TYPE /sapcnd/code_line_num
                         CHANGING p_ti_ext_contato TYPE ztfi_pessoa_contato.

  SELECT k~kunnr,  k~parnr,      k~namev,       k~abtnr,
         k~pafkt,  k~pavip,      c~country,     c~post_code1,  c~city1,
         c~street, c~house_num1, r2~tel_number, r3~fax_number, r6~smtp_addr
  FROM knvk AS k
  LEFT OUTER JOIN kna1 AS a  ON a~kunnr = k~kunnr
*  LEFT OUTER JOIN knb1 AS b  ON b~kunnr = k~kunnr
  LEFT OUTER JOIN adrc AS c  ON c~addrnumber  = a~adrnr
  LEFT OUTER JOIN adr2 AS r2 ON r2~addrnumber = a~adrnr AND
                                r2~persnumber = k~prsnr
  LEFT OUTER JOIN adr3 AS r3 ON r3~addrnumber = a~adrnr AND
                                r3~persnumber = k~prsnr
  LEFT OUTER JOIN adr6 AS r6 ON r6~addrnumber = a~adrnr AND
                                r6~persnumber = k~prsnr
  INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_contato
  UP TO @p_maxreg ROWS
*  WHERE b~bukrs IN @p_ti_emp    ##TOO_MANY_ITAB_FIELDS.
   WHERE k~kunnr IN ( SELECT kunnr FROM knb1 AS b  WHERE b~kunnr = k~kunnr AND b~bukrs IN @p_ti_emp ).

  SORT p_ti_ext_contato BY kunnr parnr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  MONTAR_TAX_NUMBERS
*&---------------------------------------------------------------------*
*  Montar saída tax numbers
*----------------------------------------------------------------------*
FORM montar_tax_numbers  CHANGING p_ti_gerais TYPE type_ti_gerais
                                  p_ti_taxnumbers TYPE ztfi_tax_numbers.

  DATA: e_gerais     TYPE type_gerais,
        e_taxnumbers TYPE zefi_tax_numbers.

  DELETE ADJACENT DUPLICATES FROM p_ti_gerais COMPARING kunnr.

  LOOP AT p_ti_gerais INTO e_gerais.

    e_taxnumbers-kunnr = e_gerais-kunnr.

    IF e_gerais-stcd1 IS NOT INITIAL.
      e_taxnumbers-taxtype = 'BR1'.
      e_taxnumbers-taxnum  = e_gerais-stcd1.
      APPEND e_taxnumbers TO p_ti_taxnumbers.
    ENDIF.

    IF e_gerais-stcd2 IS NOT INITIAL.
      e_taxnumbers-taxtype = 'BR2'.
      e_taxnumbers-taxnum  = e_gerais-stcd2.
      APPEND e_taxnumbers TO p_ti_taxnumbers.
    ENDIF.

    IF e_gerais-stcd3 IS NOT INITIAL.
      e_taxnumbers-taxtype = 'BR3'.
      e_taxnumbers-taxnum  = e_gerais-stcd3.
      APPEND e_taxnumbers TO p_ti_taxnumbers.
    ENDIF.

    IF e_gerais-stcd4 IS NOT INITIAL.
      e_taxnumbers-taxtype = 'BR4'.
      e_taxnumbers-taxnum  = e_gerais-stcd4.
      APPEND e_taxnumbers TO p_ti_taxnumbers.
    ENDIF.

  ENDLOOP.

  SORT p_ti_taxnumbers BY kunnr.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form DESPACHA_EXTRACAO
*&---------------------------------------------------------------------*
*& Gerar planilha excel com dados extraídos
*&---------------------------------------------------------------------*
FORM despacha_extracao.

  DATA:
    lo_excel     TYPE REF TO zcl_excel,
    lo_worksheet TYPE REF TO zcl_excel_worksheet,
    lo_writer    TYPE REF TO zif_excel_writer,
    lx_data      TYPE xstring,             " Will be used for sending as email
    ti_rawdata   TYPE solix_tab,           " Will be used for downloading or open directly
    bytecount    TYPE i,                   " Will be used for downloading or open directly
    filename     TYPE string.

  TRY.
      " Creates active sheet
      CREATE OBJECT lo_excel.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Dados Gerais'
                              ti_ext_gerais.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Sales Data'
                              ti_ext_vendas.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Sales Partner'
                              ti_ext_parceiros.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Company Data'
                              ti_ext_empresa.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Dunning Data'
                              ti_ext_cobranca.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Bank Data'
                              ti_ext_banco.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Tax Classifications'
                              ti_ext_impostos.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Contact person'
                              ti_ext_contato.

      PERFORM set_table USING lo_excel
                              lo_worksheet
                              'Tax Numbers'
                              ti_ext_taxnumbers.

      lo_worksheet = lo_excel->add_new_worksheet( ).

      CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.

      lx_data = lo_writer->write_file( lo_excel ).

      ti_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lx_data ).

      bytecount = xstrlen( lx_data ).

      filename = p_fname.

      cl_gui_frontend_services=>gui_download( EXPORTING bin_filesize = bytecount
                                                        filename     = filename
                                                        filetype     = 'BIN'
                                               CHANGING data_tab     = ti_rawdata ).
    CATCH zcx_excel.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TABLE
*&---------------------------------------------------------------------*
*& Montar planilha excel
*&---------------------------------------------------------------------*
*       --> Instância Excel
*       --> Instância Worksheet
*&      --> P_ABA       Nome da aba
*&      --> P_TI_TABLE  Tabela com os dados extraídos
*&---------------------------------------------------------------------*
FORM set_table USING lo_excel     TYPE REF TO zcl_excel
                     lo_worksheet TYPE REF TO zcl_excel_worksheet
                     p_aba        TYPE string
                     p_ti_table   TYPE ANY TABLE.

  DATA lc_title TYPE zexcel_sheet_title.

  lc_title = p_aba.

  TRY.
      " Get active sheet
      lo_worksheet = lo_excel->get_active_worksheet( ).
      lo_worksheet->set_title( ip_title = lc_title ).

*      TRY.
      lo_worksheet->set_table(
        EXPORTING
          ip_table           = p_ti_table
*      ip_hdr_style       =     " Style identifier
*      ip_body_style      =     " Style identifier
          ip_table_title     = p_aba
      ip_top_left_column = 'A'    " Cell Column
      ip_top_left_row    =  1   " Cell Row
*      ip_transpose       =     " Transpose table
*      ip_no_header       =     " Do not write header
      ).
*        CATCH zcx_excel.    "
*      ENDTRY.

      lo_worksheet = lo_excel->add_new_worksheet( ).

    CATCH zcx_excel.    "
  ENDTRY.

ENDFORM.
