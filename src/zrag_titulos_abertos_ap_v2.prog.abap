* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Marcelo Alvares                                        *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_TITULOS_ABERTOS_AP_v2                             *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração de títulos em aberto contas a pagar           *
* Data.............: 13/02/2019                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK921294 | AGIR - Extração de Dados para Migração - Títulos Aberto AP   *
* --------------------------------------------------------------------------*

REPORT zrag_titulos_abertos_ap_v2 .

TYPES:

  BEGIN OF ty_open_itens_ap,
    bukrs  TYPE bukrs,      " Empresa
    vendor TYPE lifnr,      " Nº conta do fornecedor
    xblnr  TYPE xblnr1,     " Nº documento de referência
    umskz  TYPE umskz,      " Código de Razão Especial
    bldat  TYPE bldat,      " Data no documento
    budat  TYPE budat,      " Data de lançamento no documento
    bktxt  TYPE bktxt,      " Texto de cabeçalho de documento
    sgtxt  TYPE sgtxt,      " Texto do item
    wrshb  TYPE wrshb_x8,   " Montante em moeda doc.(moeda estrangeira)
    dmshb  TYPE dmshb_x8,   " Montante em moeda interna com sinal +/-
    bwwr2  TYPE bwwr2_x8,   " Montante avaliado em moeda interna-2
    bwwr3  TYPE bwwr3_x8,   " Montante avaliado em moeda interna-3
    waers  TYPE waers,      " Código da moeda
    hwaer  TYPE hwaer,      " Moeda interna
    hwae2  TYPE hwae2,      " Código da moeda da segunda moeda interna
    hwae3  TYPE hwae3,      " Código da moeda da terceira moeda interna
    mwskz  TYPE mwskz,      " Código do IVA
    prctr  TYPE prctr,      " Centro de lucro
    fkber  TYPE fkber,      " Área funcional
    zterm  TYPE dzterm,     " Chave de condições de pagamento
    zfbdt  TYPE dzfbdt,     " Data base para cálculo do vencimento
    zbd1t  TYPE dzbd1t,     " Dias de desconto 1
    zbd1p  TYPE dzbd1p,     " Taxa de desconto 1
    zbd2t  TYPE dzbd2t,     " Dias de desconto 2
    zbd2p  TYPE dzbd2p,     " Taxa de desconto 2
    zbd3t  TYPE dzbd3t,     " Prazo para condição líquida
    wskto  TYPE wskto_x8,   " Montante com direito a desconto em moeda do documento
    zlspr  TYPE dzlspr,     " Chave para o bloqueio de pagamento
    zlsch  TYPE dzlsch,     " Forma de pagamento
    bvtyp  TYPE bvtyp,      " Tipo de banco do parceiro
    zuonr  TYPE dzuonr,     " Nº atribuição
    hbkid  TYPE hbkid,      " Chave breve de um banco da empresa
    hktid  TYPE hktid,      " Chave breve das coordenadas de uma conta
    bupla  TYPE bupla,      " Local de negócios
    belnr  TYPE belnr_d,    " Nº documento de um documento contábil
    gjahr  TYPE gjahr,      " Exercício
    hkont  TYPE hkont,      " Nº conta do Razão
    gsber  TYPE gsber,      " Divisão
    bschl  TYPE bschl,      " Chave de lançamento
    blart  TYPE blart,      " Tipo de documento
    vbund  TYPE vbund,      " Nº sociedade
    faedt  TYPE faedt_fpos, " Vencimento líquido
    empfb  TYPE empfb,      " Recebedor de pagamento/pagador
    shkzg  TYPE shkzg,      " Código débito/crédito
    ebeln  TYPE ebeln,      " Nº do documento de compras
    ebelp  TYPE ebelp,      " Nº item do documento de compra
    esrnr  TYPE esrnr,      " Nº participante NDR
    esrre  TYPE esrre,      " Nº referência NDR (nota de depósito com nº referência)
    esrpz  TYPE esrpz,      " Dígito de controle de nota de depósito c/nº referência (NDR)
    brcde  TYPE brcde,      " Representação númerica de cód.barras em form.pagamento
    augbl  TYPE augbl,      " Nº documento de compensação
    augdt  TYPE augdt,      " Data de compensação
  END OF ty_open_itens_ap.

DATA:
  gc_bukrs         TYPE bukrs,
  gc_lifnr         TYPE lifnr,
  lo_table         TYPE REF TO cl_salv_table,
  ls_open_itens_ap TYPE ty_open_itens_ap,
  it_open_itens_ap TYPE TABLE OF ty_open_itens_ap,
  e_return         TYPE bapireturn ##NEEDED,
  ti_op_sel        TYPE TABLE OF rsparams,
  ls_op_sel        LIKE LINE  OF ti_op_sel,
  lr_data          TYPE REF   TO data,
  lt_rfposx        TYPE TABLE OF rfposx.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_bukrs FOR gc_bukrs MATCHCODE OBJECT c_t001,
                 so_lifnr FOR gc_lifnr MATCHCODE OBJECT kred.

PARAMETERS p_keydat TYPE budat OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN SKIP.

PARAMETERS: x_norm  TYPE xnorm AS CHECKBOX DEFAULT 'X',
            x_umskz TYPE umskz AS CHECKBOX.


SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num,

  rb_down  RADIOBUTTON GROUP rb1 DEFAULT 'X',
  rb_show  RADIOBUTTON GROUP rb1,
  rb_back  RADIOBUTTON GROUP rb1.

SELECTION-SCREEN SKIP.
PARAMETERS: x_viass TYPE boolean AS CHECKBOX DEFAULT abap_false.

SELECTION-SCREEN SKIP.
SELECTION-SCREEN ULINE.

PARAMETERS:
  p_fname TYPE localfile MODIF ID p1,
  p_spath TYPE eseftappl MODIF ID p2 DEFAULT '/tmp/'.

SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.

  PERFORM set_filename.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = sy-repid
      variant              = '/DEFAULT'
    EXCEPTIONS
      variant_not_existent = 01
      variant_obsolete     = 02.

**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************

  TRY.

      PERFORM set_fields_selection.

*--------------------------------------------------*
* Não exibir o ALV
      CALL METHOD cl_salv_bs_runtime_info=>set(
        EXPORTING
          display  = abap_false
          metadata = abap_false
          data     = abap_true ).

*--------------------------------------------------*
* Report Vendor Line Item Display

      IF x_viass IS INITIAL OR sy-batch IS NOT INITIAL.
        SUBMIT rfitemap
          WITH SELECTION-TABLE ti_op_sel
            AND RETURN.

      ELSE. "Show Select Screen
        SUBMIT rfitemap VIA SELECTION-SCREEN
        WITH SELECTION-TABLE ti_op_sel
          AND RETURN.
      ENDIF.


*--------------------------------------------------*
* ALV Data
      CALL METHOD cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
      ASSIGN lr_data->* TO FIELD-SYMBOL(<fs_t_rfposx>).
      CALL METHOD cl_salv_bs_runtime_info=>clear_all( ).

      IF <fs_t_rfposx> IS NOT ASSIGNED.
        "Nenhum dado selecionado para o filtro informado.
        MESSAGE ID 'ZAG_MIG' TYPE 'S' NUMBER 004 DISPLAY LIKE 'E'.
        RETURN.
      ELSEIF <fs_t_rfposx> IS INITIAL.
        "Nenhum dado selecionado para o filtro informado.
        MESSAGE ID 'ZAG_MIG' TYPE 'S' NUMBER 004 DISPLAY LIKE 'E'.
        RETURN.
      ENDIF.

      MOVE-CORRESPONDING <fs_t_rfposx> TO lt_rfposx.

      LOOP AT lt_rfposx ASSIGNING FIELD-SYMBOL(<fs_s_rfposx>).

        MOVE-CORRESPONDING <fs_s_rfposx> TO ls_open_itens_ap.

        ls_open_itens_ap-vendor    = <fs_s_rfposx>-konto.

        SELECT SINGLE bktxt FROM bkpf INTO ls_open_itens_ap-bktxt
                  WHERE bukrs EQ <fs_s_rfposx>-bukrs AND
                        belnr EQ <fs_s_rfposx>-belnr AND
                        gjahr EQ <fs_s_rfposx>-gjahr.

        "Select additional fields
        SELECT SINGLE hbkid hktid empfb bvtyp esrnr esrre esrpz
            FROM bseg INTO CORRESPONDING FIELDS OF ls_open_itens_ap
            WHERE bukrs EQ <fs_s_rfposx>-bukrs AND
                  belnr EQ <fs_s_rfposx>-belnr AND
                  gjahr EQ <fs_s_rfposx>-gjahr AND
                  buzei EQ <fs_s_rfposx>-buzei.

        DATA lv_dmbtr TYPE dmbtr.
        lv_dmbtr = abs( ls_open_itens_ap-wrshb ).

        CALL FUNCTION 'J_1B_BARCODE_REVERT'
          EXPORTING
            iv_esrre            = ls_open_itens_ap-esrre
            iv_esrnr            = ls_open_itens_ap-esrnr
            iv_esrpz            = ls_open_itens_ap-esrpz
            iv_dmbtr            = lv_dmbtr
          IMPORTING
            ev_reverted_barcode = ls_open_itens_ap-brcde.


        APPEND ls_open_itens_ap TO it_open_itens_ap.
      ENDLOOP.


      IF rb_show IS NOT INITIAL.

        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = it_open_itens_ap ).
        lo_table->display( ).

      ELSE.

        PERFORM despacha_extracao.

      ENDIF.

    CATCH cx_root.
      MESSAGE s002(zag_mig) WITH sy-repid DISPLAY LIKE 'E'.
      RETURN.

  ENDTRY.

*&---------------------------------------------------------------------*
*& Form DESPACHA_EXTRACAO
*&---------------------------------------------------------------------*
*& Gerar planilha excel com dados extraidos
*&---------------------------------------------------------------------*
FORM despacha_extracao.

  DATA:
    lo_excel     TYPE REF TO zcl_excel,
    lo_writer    TYPE REF TO zif_excel_writer,
    lx_data      TYPE xstring,
    ti_rawdata   TYPE solix_tab,        " Will be used for downloading or open directly
    lv_bytecount TYPE i,                " Will be used for downloading or open directly
    lv_filename  TYPE string.

  TRY.
      " Creates active sheet
      CREATE OBJECT lo_excel.

      PERFORM set_table
        IN PROGRAM zrag_imobilizado
        USING   lo_excel
                'Contas_pagar'
                it_open_itens_ap
                abap_true.

      " Define a primeira planilha como ativa
      lo_excel->set_active_sheet_index( 1 ).

      CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.

      lx_data = lo_writer->write_file( lo_excel ).

      ti_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lx_data ).

      lv_bytecount = xstrlen( lx_data ).

      lv_filename = p_fname.

      CASE abap_true.

        WHEN rb_back.

          PERFORM file_save_server
            IN PROGRAM zrag_imobilizado
            USING   ti_rawdata
                    lv_bytecount
                    p_spath.

        WHEN rb_down.

          IF sy-batch IS NOT INITIAL. " Verifica se é background

            MESSAGE 'Não é possivel baixar o arquivo pois a execução é em background' TYPE 'S' DISPLAY LIKE 'E'.
*            MESSAGE ID 'FES' TYPE 'S' NUMBER '002' DISPLAY LIKE 'E'.

            PERFORM file_save_server
              IN PROGRAM zrag_imobilizado
              USING   ti_rawdata
                      lv_bytecount
                      p_spath.

            RETURN.
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

      ENDCASE.

    CATCH zcx_excel.
  ENDTRY.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_PATH
*&---------------------------------------------------------------------*
*& Set File Name
*&---------------------------------------------------------------------*
FORM set_filename.

  CONSTANTS:
    c_prefix_fe TYPE c LENGTH 20  VALUE 'C:\temp\extract_'  ##NO_TEXT,
    c_prefix_be TYPE c LENGTH 20  VALUE '/tmp/extract_'     ##NO_TEXT,
    c_sufix     TYPE c LENGTH 5   VALUE '.xlsx'             ##NO_TEXT,
    c_name      TYPE c LENGTH 09  VALUE 'FI_097_AP'         ##NO_TEXT.

  CONCATENATE c_prefix_fe c_name '_' sy-sysid sy-mandt
    '_' sy-datlo sy-timlo c_sufix INTO p_fname.

  CONCATENATE c_prefix_be c_name '_' sy-sysid sy-mandt
              '_' sy-datlo sy-timlo c_sufix INTO p_spath.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form SET_FIELDS_SELECTION.
*&---------------------------------------------------------------------*
*& Set Fields Selection for FBL1N
*&---------------------------------------------------------------------*
FORM set_fields_selection.


*--------------------------------------------------*
* Vendor
  LOOP AT so_lifnr ASSIGNING FIELD-SYMBOL(<fs_lifnr>).
    CLEAR  ls_op_sel.
    ls_op_sel-selname = 'KD_LIFNR'.
    ls_op_sel-kind    = 'P'.
    MOVE-CORRESPONDING <fs_lifnr> TO ls_op_sel.
    APPEND ls_op_sel TO ti_op_sel.
  ENDLOOP.

*--------------------------------------------------*
* Company
  LOOP AT so_bukrs ASSIGNING FIELD-SYMBOL(<fs_emp>).
    CLEAR  ls_op_sel.
    ls_op_sel-selname = 'KD_BUKRS'.
    ls_op_sel-kind    = 'P'.
    MOVE-CORRESPONDING <fs_emp> TO ls_op_sel.
    APPEND ls_op_sel TO ti_op_sel.
  ENDLOOP.

*--------------------------------------------------*
* Date
  CLEAR  ls_op_sel.
  ls_op_sel-selname = 'PA_STIDA'.
  ls_op_sel-kind    = 'P'.
  ls_op_sel-sign    = 'I'.
  ls_op_sel-option  = 'EQ'.
  ls_op_sel-low     = p_keydat.
  APPEND ls_op_sel TO ti_op_sel.

*--------------------------------------------------*
* Special General Ledger Transactions
  CLEAR  ls_op_sel.
  ls_op_sel-selname = 'X_SHBV'.
  ls_op_sel-kind    = 'P'.
  ls_op_sel-sign    = 'I'.
  ls_op_sel-option  = 'EQ'.
  ls_op_sel-low     = x_umskz.
  APPEND ls_op_sel TO ti_op_sel.

*--------------------------------------------------*
* Normal Items
  CLEAR  ls_op_sel.
  ls_op_sel-selname = 'X_NORM'.
  ls_op_sel-kind    = 'P'.
  ls_op_sel-sign    = 'I'.
  ls_op_sel-option  = 'EQ'.
  ls_op_sel-low     = x_norm.
  APPEND ls_op_sel TO ti_op_sel.

*--------------------------------------------------*
* Normal Items
  CLEAR  ls_op_sel.
  ls_op_sel-selname = 'PA_NMAX'.
  ls_op_sel-kind    = 'P'.
  ls_op_sel-sign    = 'I'.
  ls_op_sel-option  = 'EQ'.
  ls_op_sel-low     = p_maxreg.
  APPEND ls_op_sel TO ti_op_sel.

ENDFORM.
