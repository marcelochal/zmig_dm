* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZRAG_TITULOS_ABERTOS_AR                                *
* Transação........:                                                        *
* Tipo de Prg......: REPORT                                                 *
* Objetivo.........: Extração de títulos em aberto contas a receber         *
* Data.............: 06/08/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920509 | AGIR - Extração de Dados para Migração - Títulos Aberto AR   *
* --------------------------------------------------------------------------*

REPORT zrag_titulos_abertos_ar.

TYPES: BEGIN OF type_knb1,
         kunnr TYPE knb1-kunnr,
         bukrs TYPE knb1-bukrs,
       END OF type_knb1.

DATA:
  gd_tot_amount  TYPE bapiwrbtr,
  gc_bukrs       TYPE bukrs,
  gc_kunnr       TYPE kunnr,
  gi_lines       TYPE i,
  wa_return      TYPE bapiret2,
  lo_table       TYPE REF TO cl_salv_table,
  ti_return      TYPE TABLE OF bapiret2,
  ti_ext_titulos TYPE ztfi_titulos_abertos_ar,
  ti_lineitems   TYPE TABLE OF zefi_bapi3007_2,
  ti_knb1        TYPE TABLE OF type_knb1,
*  e_knb1         TYPE type_knb1,
  e_ext_titulos  TYPE zefi_titulos_abertos_ar,
*  e_lineitems    TYPE zefi_bapi3007_2,
  e_return       TYPE bapireturn ##NEEDED.


FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.

SELECT-OPTIONS : so_emp   FOR gc_bukrs MATCHCODE OBJECT c_t001,
                 so_clien FOR gc_kunnr MATCHCODE OBJECT debi.

PARAMETERS p_keydat TYPE budat OBLIGATORY DEFAULT sy-datum.

SELECTION-SCREEN SKIP.

PARAMETERS: x_umskz TYPE umskz AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t00.
PARAMETERS:
  p_maxreg TYPE /sapcnd/code_line_num DEFAULT 100,     "AGIR - Obrigatório
*  p_local  TYPE c AS CHECKBOX.
  rb_down  RADIOBUTTON GROUP rb1 DEFAULT 'X',
  rb_show  RADIOBUTTON GROUP rb1,
  rb_back  RADIOBUTTON GROUP rb1.

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

      "Selecionar clientes da empresa
      SELECT kunnr bukrs
        INTO TABLE ti_knb1
        FROM knb1
        WHERE kunnr IN so_clien
          AND bukrs IN so_emp.

      IF ti_knb1 IS INITIAL.
        "Erro extraindo dados usando o programa &
        MESSAGE i008(zag_mig) WITH 'Não foi encontrado nenhum cliente' DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.
      ENDIF.

*Start  - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 04.01.2019 16:30
*      LOOP AT ti_knb1 INTO e_knb1.
      LOOP AT ti_knb1 ASSIGNING FIELD-SYMBOL(<fs_knb1>).
*END    - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 04.01.2019 16:30


        REFRESH ti_lineitems.

        "Selecionar títulos em aberto contas a receber
        CALL FUNCTION 'ZFFI_AR_ACC_GETOPENITEMS'
          EXPORTING
            companycode = <fs_knb1>-bukrs
            customer    = <fs_knb1>-kunnr
            keydate     = p_keydat
          IMPORTING
            return      = e_return
          TABLES
            lineitems   = ti_lineitems.

        "Preencher tabela de saída com títulos em aberto
*Start    - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 04.01.2019 16:05
*        LOOP AT ti_lineitems INTO e_lineitems.
        LOOP AT ti_lineitems ASSIGNING FIELD-SYMBOL(<fs_lineitems>).

          "Desconsiderar operações do razão especial
          IF x_umskz IS INITIAL AND <fs_lineitems>-umskz IS NOT INITIAL.
            CONTINUE.
          ENDIF.

          MOVE-CORRESPONDING <fs_lineitems> TO e_ext_titulos ##ENH_OK.

* inicio alteração - ma004818 marcelo alvares - 22.08.2018 16:29:31
          SELECT SINGLE bktxt FROM bkpf INTO e_ext_titulos-bktxt
          WHERE bukrs EQ <fs_lineitems>-bukrs AND
                belnr EQ <fs_lineitems>-belnr    AND
                gjahr EQ <fs_lineitems>-gjahr.
* Fim alteração - MA004818 Marcelo Alvares - 22.08.2018 16:29:35

          "Determinar a data de vencimento líquido
          PERFORM determinar_dt_venc USING e_ext_titulos
*Start    - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 12.02.2019 21:28
*                                  CHANGING e_ext_titulos-dt_venc.
                                  CHANGING e_ext_titulos-faedt.
*END    - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 12.02.2019 21:28

          APPEND e_ext_titulos TO ti_ext_titulos.

          IF <fs_lineitems>-shkzg = 'H'. "Crédito
            SUBTRACT <fs_lineitems>-dmbtr FROM gd_tot_amount.
          ELSEIF <fs_lineitems>-shkzg = 'S'. "Débito
            ADD <fs_lineitems>-dmbtr TO gd_tot_amount.
          ENDIF.

          DESCRIBE TABLE ti_ext_titulos LINES gi_lines.
          IF gi_lines = p_maxreg AND
             p_maxreg IS NOT INITIAL AND
             gi_lines IS NOT INITIAL.
            EXIT. "LOOP
          ENDIF.
        ENDLOOP.

        IF gi_lines = p_maxreg AND
           p_maxreg IS NOT INITIAL AND
           gi_lines IS NOT INITIAL.
          EXIT.
        ENDIF.

      ENDLOOP.
*END    - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 04.01.2019 16:05

      IF ti_ext_titulos IS INITIAL.

        "Nenhum dado selecionado para o filtro informado.
        MESSAGE i004(zag_mig) DISPLAY LIKE 'E'.
        wa_return-type   = 'E'.
        wa_return-id     = sy-msgid.
        wa_return-number = sy-msgno.
        APPEND wa_return TO ti_return.

      ENDIF.

*Start    - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 06.02.2019 11:13

*      IF p_local IS NOT INITIAL.
      PERFORM despacha_extracao.

*      IF rb_show IS NOT INITIAL.
*        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_ext_titulos ).
*        lo_table->display( ).
*
*        MESSAGE i008(zag_mig) WITH 'Montante total na MI' gd_tot_amount.
*
*      ELSE.
*
*        "AGIR - Extração.
*        "Comandos obrigatórios
*        "Exporta resultado para Id de Memória
*        ASSIGN ti_ext_titulos TO <t_tab>.
*        EXPORT:
*          <t_tab>   TO MEMORY ID 'AG',
*          ti_return TO MEMORY ID 'RET'.
*        LEAVE PROGRAM.
*
*      ENDIF.
*END    - Marcelo Alvares - ABAP-INTPR11 TBD ZRAG_TITULOS_ABERTOS_AR ZAG_MIG - 06.02.2019 11:13
    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.

  ENDTRY.
*&---------------------------------------------------------------------*
*&      Form  DETERMINAR_DT_VENC
*&---------------------------------------------------------------------*
*  Determinar a data de vencimento líquido
*----------------------------------------------------------------------*
FORM determinar_dt_venc  USING    p_ext_titulos  TYPE zefi_titulos_abertos_ar
                         CHANGING p_dtvenc       TYPE gtr_fi_due_date.

  DATA: e_faede TYPE faede.

  MOVE:
    'D'                      TO e_faede-koart,
    p_ext_titulos-zfbdt TO e_faede-zfbdt,  "Data base para cálculo do vencimento
    p_ext_titulos-zbd1t TO e_faede-zbd1t,  "dias de desconto 1
    p_ext_titulos-zbd2t TO e_faede-zbd2t,  "dias de desconto 2
    p_ext_titulos-zbd3t TO e_faede-zbd3t,  "prazo para condição líquida
    p_ext_titulos-zbd1p TO e_faede-sk1dt,  "desconto 1 vencimento
    p_ext_titulos-zbd2p TO e_faede-sk2dt,  "desconto 2 vencimento
    p_ext_titulos-bldat TO e_faede-bldat.  "data no documento

  CALL FUNCTION 'DETERMINE_DUE_DATE'
    EXPORTING
      i_faede = e_faede
    IMPORTING
      e_faede = e_faede
    EXCEPTIONS
      OTHERS  = 1.

  p_dtvenc = e_faede-netdt.

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
