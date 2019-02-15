* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_IMOBILIZADO                                       *
* Transação........: N/A                                                    *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração do imobilizado                                *
* Data.............: 22/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920773 | AGIR - Extração de Dados para Migração - Imobilizado         *
* --------------------------------------------------------------------------*
REPORT zrag_imobilizado.

TABLES: anla, anlc.

DATA:
  ti_ext_gerais     TYPE ztfi_mestre_imobilizado,
  ti_ext_areas      TYPE ztfi_areas_depreciacao,
  ti_ext_valoracc   TYPE ztfi_valores_acumulados,
  ti_ext_valorlanc  TYPE ztfi_valores_lancados,
  ti_ext_transacoes TYPE ztfi_transacoes_imobilizado,
  ti_ext_anlu       TYPE ztfi_bapi_te_anlu.

CONSTANTS:
  c_prefix_fe TYPE c LENGTH 20  VALUE 'C:\temp\extract_'  ##NO_TEXT,
  c_prefix_be TYPE c LENGTH 20  VALUE '/tmp/extract_'     ##NO_TEXT,
  c_sufix     TYPE c LENGTH 5   VALUE '.xlsx'             ##NO_TEXT,
  c_name      TYPE c LENGTH 18  VALUE 'FI_89_IMOBILIZADO' ##NO_TEXT.


SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_emp   FOR anla-bukrs, " MATCHCODE OBJECT c_t001,
                 so_class FOR anla-anlkl, " MATCHCODE OBJECT sh_anlka,
                 so_imo   FOR anla-anln1, " MATCHCODE OBJECT aanl,
                 so_gjahr FOR anlc-gjahr, "
* Inicio Alteração - MA004818 Marcelo Alvares - 06.02.2019 14:12:47
                 so_deakt FOR anla-deakt.
* Fim alteração - MA004818 Marcelo Alvares - 06.02.2019 14:12:51

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num DEFAULT 0,     "AGIR - Obrigatório

  rb_down  RADIOBUTTON GROUP rb1 DEFAULT 'X',
  rb_show  RADIOBUTTON GROUP rb1,
  rb_back  RADIOBUTTON GROUP rb1.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.

PARAMETERS:
  p_fname TYPE localfile MODIF ID p1,
  p_spath TYPE eseftappl MODIF ID p2 DEFAULT 'X'.


SELECTION-SCREEN END OF BLOCK b2.


INITIALIZATION.

  CONCATENATE c_prefix_fe c_name '_' sy-sysid sy-mandt
              '_' sy-datlo sy-timlo c_sufix INTO p_fname.

  CONCATENATE c_prefix_be c_name '_' sy-sysid sy-mandt
              '_' sy-datlo sy-timlo c_sufix INTO p_spath.

  APPEND INITIAL LINE TO so_deakt.
  READ TABLE so_deakt ASSIGNING FIELD-SYMBOL(<fs_deakt>) INDEX 1.
  <fs_deakt>-option = 'EQ'.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      "Selecionar dados gerais do imobilizado
      PERFORM selecionar_dados_gerais TABLES   so_emp
                                               so_imo
                                      USING    p_maxreg
                                      CHANGING ti_ext_gerais.

      IF ti_ext_gerais IS INITIAL.
        "Nenhum dado selecionado para o filtro informado.
        MESSAGE s004(zag_mig) DISPLAY LIKE 'E'.
        RETURN.

      ENDIF.

      "Selecionar as áreas de depreciaçao
      PERFORM selecionar_areas_depreciacao TABLES   so_emp
                                                    so_imo
                                           USING    p_maxreg
                                           CHANGING ti_ext_areas.
      "Selecionar valores do imobilizado
      PERFORM selecionar_valores TABLES   so_emp
                                          so_imo
                                 USING    p_maxreg
                                 CHANGING ti_ext_valoracc
                                          ti_ext_valorlanc.

      "Selecionar os movimentos do imobilizado
      PERFORM selecionar_transacoes TABLES   so_emp
                                             so_imo
                                    USING    p_maxreg
                                    CHANGING ti_ext_transacoes.

      "Selecionar campos de usuário
      PERFORM selecionar_campos_usuario TABLES so_emp
                                               so_imo
                                        USING  p_maxreg
                                        CHANGING ti_ext_anlu.

      "AGIR - Extração.
      PERFORM despacha_extracao.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_DADOS_GERAIS
*&---------------------------------------------------------------------*
*   Selecionar dados gerais do imobilizado
*----------------------------------------------------------------------*
FORM selecionar_dados_gerais  TABLES   p_ti_emp    STRUCTURE so_emp
                                       p_ti_imo    STRUCTURE so_imo
                              USING    p_maxreg    TYPE /sapcnd/code_line_num
                              CHANGING p_ti_ext_gerais TYPE ztfi_mestre_imobilizado.

  SELECT a~bukrs,
         a~anln1,
         a~anln2,
         a~anlkl,
         a~txt50,
         a~txa50,
         a~sernr,
         a~menge,
         a~meins,
         a~aktiv,
         z~werks,
         z~stort,
         z~kostl,
         a~aibn1,
         a~invnr,
         a~ivdat,
         a~invzu,
         a~inken,
         a~posnr
   FROM anla AS a
   LEFT OUTER JOIN anlz AS z  ON z~bukrs = a~bukrs AND
                                 z~anln1 = a~anln1 AND
                                 z~anln2 = a~anln2

   INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_gerais
   UP TO @p_maxreg ROWS
   WHERE a~bukrs IN @p_ti_emp AND
         a~anln1 IN @p_ti_imo AND
         a~anlkl IN @so_class AND
* Inicio Alteração - MA004818 Marcelo Alvares - 15.01.2019 16:12:16
         z~bdatu GE @sy-datum AND "Atribuições de imobilizado com data valor erro Retorna duplicidade
         a~deakt IN @so_deakt.
* Fim alteração - MA004818 Marcelo Alvares - 15.01.2019 16:12:21

  SORT p_ti_ext_gerais BY bukrs anln1 anln2.

  LOOP AT  p_ti_ext_gerais ASSIGNING FIELD-SYMBOL(<fs_ti_ext_gerais>)
    WHERE posnr IS NOT INITIAL.

    CALL FUNCTION 'CONVERSION_EXIT_ABPSP_OUTPUT'
      EXPORTING
        input  = <fs_ti_ext_gerais>-posnr
      IMPORTING
        output = <fs_ti_ext_gerais>-posid.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_AREAS_DEPRECIACAO
*&---------------------------------------------------------------------*
*   Selecionar as áreas de depreciação
*----------------------------------------------------------------------*
FORM selecionar_areas_depreciacao  TABLES   p_ti_emp    STRUCTURE so_emp
                                            p_ti_imo    STRUCTURE so_imo
                                   USING    p_maxreg    TYPE /sapcnd/code_line_num
                                   CHANGING p_ti_ext_areas TYPE ztfi_areas_depreciacao.

* Parâmetro de depreciação
  SELECT a~bukrs    "Empresa
         a~anln1    "Nº principal do imobilizado
         a~anln2    "Subnº do imobilizado
         a~afabe    "Área de avaliação efetiva
         a~afasl    "Chave de depreciação
         a~ndjar
         a~ndper
         a~afabg
  FROM anlb AS a
  INTO CORRESPONDING FIELDS OF TABLE p_ti_ext_areas
  FOR ALL ENTRIES IN ti_ext_gerais
  WHERE a~bukrs EQ   ti_ext_gerais-bukrs AND
        a~anln1 EQ   ti_ext_gerais-anln1 AND
        a~anln2 EQ   ti_ext_gerais-anln2.
*  UP TO p_maxreg ROWS
*  WHERE bukrs IN p_ti_emp AND
*        anln1 IN p_ti_imo AND
*        anlkl IN @so_class.

  SORT p_ti_ext_areas BY bukrs anln1 anln2 afabe.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_VALORES
*&---------------------------------------------------------------------*
*   Selecionar valores do imobilizado
*----------------------------------------------------------------------*
FORM selecionar_valores  TABLES   p_ti_emp STRUCTURE so_emp
                                  p_ti_imo STRUCTURE so_imo
                         USING    p_maxreg TYPE /sapcnd/code_line_num
                         CHANGING p_ti_ext_valoracc  TYPE ztfi_valores_acumulados
                                  p_ti_ext_valorlanc TYPE ztfi_valores_lancados.

  DATA: BEGIN OF ti_valores OCCURS 10,
          bukrs  TYPE bukrs,
          anln1  TYPE anln1,
          anln2  TYPE anln2,
          afabe  TYPE afabe_d,
          gjahr  TYPE gjahr,
          kansw  TYPE kansw,
          knafa  TYPE knafa,
          nafag  TYPE nafag,
          aafag  TYPE aafag,
          afblpe TYPE afblpe,
        END OF ti_valores.

  SELECT  bukrs
          anln1
          anln2
          afabe
          gjahr
          kansw
          knafa
          nafag
          aafag
          afblpe
  FROM anlc AS a
    INTO CORRESPONDING FIELDS OF TABLE ti_valores
    FOR ALL ENTRIES IN ti_ext_gerais
    WHERE a~bukrs EQ   ti_ext_gerais-bukrs AND
          a~anln1 EQ   ti_ext_gerais-anln1 AND
          a~anln2 EQ   ti_ext_gerais-anln2 AND
          a~gjahr IN   so_gjahr.
*  UP TO p_maxreg ROWS
*  WHERE bukrs IN p_ti_emp AND
*        anln1 IN p_ti_imo.

  MOVE-CORRESPONDING ti_valores[] TO p_ti_ext_valoracc.
  MOVE-CORRESPONDING ti_valores[] TO p_ti_ext_valorlanc.

  SORT p_ti_ext_valoracc  BY bukrs anln1 anln2 afabe gjahr.
  SORT p_ti_ext_valorlanc BY bukrs anln1 anln2 afabe gjahr.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_TRANSACOES
*&---------------------------------------------------------------------*
*   Selecionar os movimentos do imobilizado
*----------------------------------------------------------------------*
FORM selecionar_transacoes  TABLES   p_ti_emp STRUCTURE so_emp
                                     p_ti_imo STRUCTURE so_imo
                            USING    p_maxreg TYPE /sapcnd/code_line_num
                            CHANGING p_ti_ext_transacoes  TYPE ztfi_transacoes_imobilizado.

  SELECT p~bukrs,
         p~anln1,
         p~anln2,
         p~afabe,
         p~bwasl,
         p~gjahr,
         p~lnran,
         p~bzdat,
         p~anbtr,
         b~waers
 FROM anep AS p
    LEFT OUTER JOIN t093b AS b ON b~bukrs = p~bukrs AND
                                  b~afabe = p~afabe
    INTO CORRESPONDING FIELDS OF TABLE @p_ti_ext_transacoes
    FOR ALL ENTRIES IN @ti_ext_gerais
    WHERE p~bukrs EQ   @ti_ext_gerais-bukrs AND
          p~anln1 EQ   @ti_ext_gerais-anln1 AND
          p~anln2 EQ   @ti_ext_gerais-anln2 AND
          p~gjahr IN   @so_gjahr.
* UP TO @p_maxreg ROWS
* WHERE p~bukrs IN @p_ti_emp AND
*       p~anln1 IN @p_ti_imo.                           "#EC CI_BUFFJOIN

  SORT p_ti_ext_transacoes BY bukrs anln1 anln2 afabe bwasl gjahr lnran.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SELECIONAR_CAMPOS_USUARIO
*&---------------------------------------------------------------------*
*   Selecionar campos de usuário
*----------------------------------------------------------------------*
FORM selecionar_campos_usuario TABLES   p_ti_emp STRUCTURE so_emp
                                        p_ti_imo STRUCTURE so_imo
                               USING    p_maxreg TYPE /sapcnd/code_line_num
                               CHANGING p_ti_ext_anlu TYPE ztfi_bapi_te_anlu.

  SELECT  bukrs
          anln1
          anln2
          zzatrib2
          zzatrib3
          zzatrib4
          zzatrib5
          zzatrib6
          zzcoduar
          zuar
          zcontrato
          zodi
          zti
          zcm
          ztuc
          zatra1
          zatra2
          zatra3
          zatra4
          zatra5
          zatra6
          zfc
          zmatnr
          zcon_patrimonial
    FROM anlu AS a
    INTO CORRESPONDING FIELDS OF TABLE p_ti_ext_anlu
    FOR ALL ENTRIES IN ti_ext_gerais
    WHERE a~bukrs EQ   ti_ext_gerais-bukrs AND
          a~anln1 EQ   ti_ext_gerais-anln1 AND
          a~anln2 EQ   ti_ext_gerais-anln2.
*  UP TO p_maxreg ROWS
*  WHERE bukrs IN p_ti_emp AND
*        anln1 IN p_ti_imo  ##TOO_MANY_ITAB_FIELDS.

  SORT p_ti_ext_anlu BY bukrs anln1 anln2.
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
    lx_data      TYPE xstring,
    ti_rawdata   TYPE solix_tab,        " Will be used for downloading or open directly
    lv_bytecount TYPE i,                " Will be used for downloading or open directly
    lv_filename  TYPE string.

  TRY.
      " Creates active sheet
      CREATE OBJECT lo_excel.

      PERFORM set_table USING lo_excel
                              'Master_Details'
                              ti_ext_gerais
                              abap_true.

      PERFORM set_table USING lo_excel
                              'Depreciation_Areas'
                              ti_ext_areas
                              abap_false.

      PERFORM set_table USING lo_excel
                              'Cumulative_Values'
                              ti_ext_valoracc
                              abap_false.

      PERFORM set_table USING lo_excel
                              'Posted_Values'
                              ti_ext_valorlanc
                              abap_false.

      PERFORM set_table USING lo_excel
                              'Transactions'
                              ti_ext_transacoes
                              abap_false.

      PERFORM set_table USING lo_excel
                              'Campos_de_usuario(ANLU)'
                              ti_ext_anlu
                              abap_false.

      " Define a primeira planilha como ativa
      lo_excel->set_active_sheet_index( 1 ).

      CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.

      lx_data = lo_writer->write_file( lo_excel ).

      ti_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lx_data ).

      lv_bytecount = xstrlen( lx_data ).

      lv_filename = p_fname.

      CASE 'X'.

        WHEN rb_back.

          PERFORM file_save_server USING ti_rawdata
                                         lv_bytecount
                                         p_spath.

        WHEN rb_down.

          IF sy-batch IS NOT INITIAL. " Verifica se é background

            MESSAGE 'Não é possivel baixar o arquivo pois a execução é em background' TYPE 'S' DISPLAY LIKE 'E'.
            MESSAGE ID 'FES' TYPE 'S' NUMBER '002' DISPLAY LIKE 'E'.

          ENDIF.

          cl_gui_frontend_services=>gui_download(
          EXPORTING
            bin_filesize              = lv_bytecount
            filename                  = lv_filename
            filetype                  = 'BIN'
            show_transfer_status      = abap_true
          CHANGING
            data_tab                  = ti_rawdata
          EXCEPTIONS
            file_write_error          = 1
            no_batch                  = 2
            gui_refuse_filetransfer   = 3
            invalid_type              = 4
            no_authority              = 5
            unknown_error             = 6
            header_not_allowed        = 7
            separator_not_allowed     = 8
            filesize_not_allowed      = 9
            header_too_long           = 10
            dp_error_create           = 11
            dp_error_send             = 12
            dp_error_write            = 13
            unknown_dp_error          = 14
            access_denied             = 15
            dp_out_of_memory          = 16
            disk_full                 = 17
            dp_timeout                = 18
            file_not_found            = 19
            dataprovider_exception    = 20
            control_flush_error       = 21
            not_supported_by_gui      = 22
            error_no_gui              = 23
            OTHERS                    = 24  ).
          IF sy-subrc NE 0.
            MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno DISPLAY LIKE 'E'
                       WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
            RETURN.
          ENDIF.

        WHEN rb_show.

          PERFORM display_online USING ti_rawdata
                                       lv_bytecount.

      ENDCASE.

    CATCH zcx_excel.
  ENDTRY.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_TABLE
*&---------------------------------------------------------------------*
*& Montar planilha excel
*&---------------------------------------------------------------------*
*&       --> Instância Excel
*&      --> P_ABA       Nome da aba
*&      --> P_TI_TABLE  Tabela com os dados extraídos
*&---------------------------------------------------------------------*
FORM set_table USING po_excel     TYPE REF TO zcl_excel
                     p_aba        TYPE string
                     p_ti_table   TYPE ANY TABLE
                     p_first      TYPE abap_bool.

  DATA:
    lc_title          TYPE zexcel_sheet_title,
    ls_table_settings TYPE zexcel_s_table_settings,
    lo_worksheet      TYPE REF TO zcl_excel_worksheet.

  lc_title = p_aba.

  ls_table_settings-top_left_column = 'A'.
  ls_table_settings-top_left_row    = 1.
  ls_table_settings-table_name      = lc_title.
  ls_table_settings-table_style     = zcl_excel_table=>builtinstyle_medium6.

  TRY.

      " Get active sheet
      IF p_first IS NOT INITIAL.
        lo_worksheet = po_excel->get_active_worksheet( ).
      ELSE.
        lo_worksheet = po_excel->add_new_worksheet( ).
      ENDIF.

      lo_worksheet->set_title( ip_title = lc_title ).


      lo_worksheet->bind_table(
        EXPORTING
          ip_table          = p_ti_table
*          it_field_catalog  =
          is_table_settings = ls_table_settings
*          iv_default_descr  =
*        IMPORTING
*          es_table_settings =
      ).
*        CATCH zcx_excel.    "

      DATA(lv_column) = lo_worksheet->get_highest_column( ).
      DATA count TYPE i VALUE 1.
      DO lv_column TIMES.
        lo_worksheet->set_column_width(
          EXPORTING
            ip_column         = count
            ip_width_autosize = abap_true ).
        count = count + 1.
      ENDDO.

    CATCH zcx_excel.
  ENDTRY.

ENDFORM.

FORM display_online USING p_rawdata     TYPE solix_tab
                          p_bytecount   TYPE i.

  DATA:
    error       TYPE REF TO i_oi_error,
    t_errors    TYPE STANDARD TABLE OF REF TO i_oi_error WITH NON-UNIQUE DEFAULT KEY,
    cl_control  TYPE REF TO i_oi_container_control, "OIContainerCtrl
    cl_document TYPE REF TO i_oi_document_proxy.


  IF sy-batch IS NOT INITIAL. " Verifica se é background

    MESSAGE ID 'FES' TYPE 'S' NUMBER '002' DISPLAY LIKE 'E'.
    MESSAGE 'Não é exibido o resultado em tela pois a execução é em background' TYPE 'S' DISPLAY LIKE 'E'.
    RETURN.

  ENDIF.

  c_oi_container_control_creator=>get_container_control( IMPORTING control = cl_control
                                                                   error   = error ).
  APPEND error TO t_errors.

  cl_control->init_control( EXPORTING  inplace_enabled     = abap_true
                                       no_flush            = abap_true
                                       r3_application_name = 'Document Container'
                                       parent              = cl_gui_container=>screen0
                            IMPORTING  error               = error
                            EXCEPTIONS OTHERS              = 2 ).
  APPEND error TO t_errors.

  cl_control->get_document_proxy( EXPORTING document_type  = 'Excel.Sheet'                " EXCEL
                                            no_flush       = ' '
                                  IMPORTING document_proxy = cl_document
                                            error          = error ).
  APPEND error TO t_errors.
* Errorhandling should be inserted here

  cl_document->open_document_from_table( EXPORTING document_size    = p_bytecount
                                                   document_table   = p_rawdata
                                                   open_inplace     = abap_true ).

  WRITE: '.'.  " To create an output.  That way screen0 will exist

ENDFORM.

FORM file_save_server USING p_t_rawdata   TYPE solix_tab
                            p_v_bytecount TYPE i
                            p_v_spath     TYPE eseftappl.

  DATA:
    lv_bytes_remain TYPE i,
    lv_msg          TYPE string.

  OPEN DATASET p_v_spath FOR OUTPUT IN BINARY MODE.
  IF sy-subrc IS NOT INITIAL.
    MESSAGE 'Erro ao abrir a pasta no servidor:' TYPE 'S'.
    RETURN.
  ENDIF.


  lv_bytes_remain = p_v_bytecount.

  LOOP AT p_t_rawdata ASSIGNING FIELD-SYMBOL(<fs_rawdata>).

    AT LAST.
      CHECK lv_bytes_remain >= 0.
      TRANSFER <fs_rawdata> TO p_v_spath  LENGTH lv_bytes_remain.
      EXIT.
    ENDAT.

    TRANSFER <fs_rawdata> TO p_v_spath.
    SUBTRACT 255 FROM lv_bytes_remain.  " Solix has length 255

  ENDLOOP.

  CLOSE DATASET p_v_spath.

  " no need to display anything if download was selected and report was called for demo purposes
*  IF sy-repid NE sy-cprog AND sy-cprog IS NOT INITIAL.
*    LEAVE PROGRAM.
*  ELSE.

  CONCATENATE 'Arquivo gravado no servidor:' p_v_spath INTO lv_msg SEPARATED BY space.
  MESSAGE lv_msg TYPE 'S'.
  WRITE / lv_msg.
*    RETURN.
*  ENDIF.

ENDFORM.
