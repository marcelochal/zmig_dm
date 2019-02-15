* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_TITULOS_ABERTOS_AR_v2                             *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração de títulos em aberto contas a receber         *
* Data.............: 06/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK921290 | AGIR - Extração de Dados para Migração - Títulos Aberto AR   *
* --------------------------------------------------------------------------*

REPORT zrag_titulos_abertos_ar_v2.

DATA:
  gd_tot_amount  TYPE bapiwrbtr VALUE IS INITIAL,
  gc_bukrs       TYPE bukrs,
  gc_kunnr       TYPE kunnr,
  ti_ext_titulos TYPE ztfi_titulos_abertos_ar,
  ls_ext_titulos TYPE zefi_titulos_abertos_ar,
  ti_lineitems   TYPE TABLE OF zefi_bapi3007_2,
  lr_data        TYPE REF   TO data,
  lt_rfposx      TYPE TABLE OF rfposx,
  ti_op_sel      TYPE TABLE OF rsparams,
  ls_op_sel      LIKE LINE  OF ti_op_sel.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_emp   FOR gc_bukrs MATCHCODE OBJECT c_t001,
                 so_clien FOR gc_kunnr MATCHCODE OBJECT debi.

PARAMETERS p_keydat TYPE budat OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN SKIP.

PARAMETERS: x_umskz TYPE umskz AS CHECKBOX DEFAULT abap_false.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num,     "AGIR - ObrigatÃ³rio
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

*----------------------------------------------------------------------*
* INITIALIZATION
*----------------------------------------------------------------------*
INITIALIZATION.
  CONSTANTS:
    c_prefix_fe TYPE c LENGTH 20  VALUE 'C:\temp\extract_'  ##NO_TEXT,
    c_prefix_be TYPE c LENGTH 20  VALUE '/tmp/extract_'     ##NO_TEXT,
    c_sufix     TYPE c LENGTH 5   VALUE '.xlsx'             ##NO_TEXT,
    c_name      TYPE c LENGTH 18  VALUE 'FI_95_AR'          ##NO_TEXT.

  CALL FUNCTION 'RS_SUPPORT_SELECTIONS'
    EXPORTING
      report               = sy-repid
      variant              = 'DEFAULT'
    EXCEPTIONS
      variant_not_existent = 01
      variant_obsolete     = 02.

  CONCATENATE c_prefix_fe c_name '_' sy-sysid sy-mandt
              '_' sy-datlo sy-timlo c_sufix INTO p_fname.

  CONCATENATE c_prefix_be c_name '_' sy-sysid sy-mandt
              '_' sy-datlo sy-timlo c_sufix INTO p_spath.


**************************************
**************************************
START-OF-SELECTION.
**************************************
**************************************
  TRY.

*--------------------------------------------------*
* Cliente
      LOOP AT so_clien ASSIGNING FIELD-SYMBOL(<fs_clien>).
        CLEAR  ls_op_sel.
        ls_op_sel-selname = 'DD_KUNNR'.
        ls_op_sel-kind    = 'P'.
        ls_op_sel-sign    = <fs_clien>-sign.
        ls_op_sel-option  = <fs_clien>-option.
        ls_op_sel-low     = <fs_clien>-low.
        ls_op_sel-high    = <fs_clien>-high.
        APPEND ls_op_sel TO ti_op_sel.
      ENDLOOP.

*--------------------------------------------------*
* Empresa
      LOOP AT so_emp ASSIGNING FIELD-SYMBOL(<fs_emp>).
        CLEAR  ls_op_sel.
        ls_op_sel-selname = 'DD_BUKRS'.
        ls_op_sel-kind    = 'P'.
        ls_op_sel-sign    = <fs_emp>-sign.
        ls_op_sel-option  = <fs_emp>-option.
        ls_op_sel-low     = <fs_emp>-low.
        ls_op_sel-high    = <fs_emp>-high.
        APPEND ls_op_sel TO ti_op_sel.
      ENDLOOP.

*--------------------------------------------------*
* Data
      CLEAR  ls_op_sel.
      ls_op_sel-selname = 'PA_STIDA'.
      ls_op_sel-kind    = 'P'.
      ls_op_sel-sign    = 'I'.
      ls_op_sel-option  = 'EQ'.
      ls_op_sel-low     = p_keydat.
      APPEND ls_op_sel TO ti_op_sel.

*--------------------------------------------------*
* Selecionar partidas de razão especial
      CLEAR  ls_op_sel.
      ls_op_sel-selname = 'X_SHBV'.
      ls_op_sel-kind    = 'P'.
      ls_op_sel-sign    = 'I'.
      ls_op_sel-option  = 'EQ'.
      ls_op_sel-low     = x_umskz.
      APPEND ls_op_sel TO ti_op_sel.

*--------------------------------------------------*
* Não exibir o ALV
      CALL METHOD cl_salv_bs_runtime_info=>set(
        EXPORTING
          display  = abap_false
          metadata = abap_false
          data     = abap_true ).

*--------------------------------------------------*
* Relatório de partidas individuais de clientes

      IF x_viass IS INITIAL OR sy-batch IS NOT INITIAL.
        SUBMIT rfitemar
          WITH SELECTION-TABLE ti_op_sel
            AND RETURN.

      ELSE.
        SUBMIT rfitemar VIA SELECTION-SCREEN
        WITH SELECTION-TABLE ti_op_sel
          AND RETURN.
      ENDIF.


*--------------------------------------------------*
* Os dados do ALV serÃ£o carregados aqui
      CALL METHOD cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).

      ASSIGN lr_data->* TO FIELD-SYMBOL(<fs_t_rfposx>).

      CALL METHOD cl_salv_bs_runtime_info=>clear_all( ).

      CHECK <fs_t_rfposx> IS ASSIGNED.


      MOVE-CORRESPONDING <fs_t_rfposx> TO lt_rfposx.

      LOOP AT lt_rfposx ASSIGNING FIELD-SYMBOL(<fs_s_rfposx>).

        MOVE-CORRESPONDING <fs_s_rfposx> TO ls_ext_titulos.

        ls_ext_titulos-kunnr    = <fs_s_rfposx>-konto.
        ls_ext_titulos-wrbtr    = <fs_s_rfposx>-wrshb.
        ls_ext_titulos-dmbtr    = <fs_s_rfposx>-bwwrt.

        SELECT SINGLE bktxt FROM bkpf INTO ls_ext_titulos-bktxt
                  WHERE bukrs EQ <fs_s_rfposx>-bukrs AND
                        belnr EQ <fs_s_rfposx>-belnr AND
                        gjahr EQ <fs_s_rfposx>-gjahr.

        SELECT SINGLE saknr anfbn hbkid hktid
            FROM bseg INTO CORRESPONDING FIELDS OF ls_ext_titulos
            WHERE bukrs EQ <fs_s_rfposx>-bukrs AND
                  belnr EQ <fs_s_rfposx>-belnr AND
                  gjahr EQ <fs_s_rfposx>-gjahr AND
                  buzei EQ <fs_s_rfposx>-buzei.

        IF <fs_s_rfposx>-shkzg = 'H'. "Crédito
          SUBTRACT ls_ext_titulos-dmbtr FROM gd_tot_amount.
          CLEAR gd_tot_amount.
          SUBTRACT ls_ext_titulos-wrbtr FROM gd_tot_amount.
        ELSEIF <fs_s_rfposx>-shkzg = 'S'. "Débito
          ADD ls_ext_titulos-dmbtr TO gd_tot_amount.
          CLEAR gd_tot_amount.
          ADD ls_ext_titulos-wrbtr TO gd_tot_amount.
        ENDIF.


        APPEND ls_ext_titulos TO ti_ext_titulos.
      ENDLOOP.

      PERFORM despacha_extracao.


    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.

  ENDTRY.

*&---------------------------------------------------------------------*
*& Form DESPACHA_EXTRACAO
*&---------------------------------------------------------------------*
*& Gerar planilha excel com dados extraÃ­dos
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

      PERFORM set_table
        IN PROGRAM zrag_imobilizado
        USING   lo_excel
                'Contas a Receber'
                ti_ext_titulos
                abap_true.

      " Define a primeira planilha como ativa
      lo_excel->set_active_sheet_index( 1 ).

      CREATE OBJECT lo_writer TYPE zcl_excel_writer_2007.

      lx_data = lo_writer->write_file( lo_excel ).

      ti_rawdata = cl_bcs_convert=>xstring_to_solix( iv_xstring  = lx_data ).

      lv_bytecount = xstrlen( lx_data ).

      lv_filename = p_fname.

      CASE 'X'.

        WHEN rb_back.

          PERFORM file_save_server
            IN PROGRAM zrag_imobilizado
            USING   ti_rawdata
                    lv_bytecount
                    p_spath.

        WHEN rb_down.

          IF sy-batch IS NOT INITIAL. " Verifica se é background

            MESSAGE 'Não é possivel baixar o arquivo pois a execução é em background' TYPE 'S' DISPLAY LIKE 'E'.
            MESSAGE ID 'FES' TYPE 'S' NUMBER '002' DISPLAY LIKE 'E'.

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

*          PERFORM display_online USING ti_rawdata
*                                       lv_bytecount.

      ENDCASE.

    CATCH zcx_excel.
  ENDTRY.

ENDFORM.
