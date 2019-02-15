*&---------------------------------------------------------------------*
*& Report ZRAG_EXTRATOR_GENERICO
*&---------------------------------------------------------------------*
*& Programa que chama programas de extração de dados do ECC.
*& Trabalha em conjunto com a tabela ZTAG_EXT_CONFIG que configura
*& para cada objeto de extração, um programa extrator e uma estrutura
*& de retorno desse programa.
*& Permite determinar se os resultados serão apenas exibidos para o
*& usuário ou se um arquivo será gerado, localmente ou no servidor
*& Também exibe um popup em caso de erro encontrado no programa chamado
*&
*&---------------------------------------------------------------------*
REPORT zrag_extrator_generico.

CONSTANTS:
  c_repository    TYPE pgmid     VALUE 'R3TR',
  c_program       TYPE trobjtype VALUE 'PROG',
  c_prefix_fe(20) TYPE c         VALUE 'C:\temp\extract_' ##NO_TEXT,
  c_prefix_be(20) TYPE c         VALUE '/tmp/extract_'    ##NO_TEXT,
  c_sufix(20)     TYPE c         VALUE '.xlsx'             ##NO_TEXT.

"Cast table to extraction
DATA:
  o_strtype         TYPE REF TO cl_abap_structdescr,
  o_struct          TYPE REF TO cl_abap_structdescr,
  o_tabtype         TYPE REF TO cl_abap_tabledescr,

  l_listobject      TYPE TABLE OF abaplist,
  ti_ztag_extr_prog TYPE TABLE OF ztag_extr_prog,
  ti_return         TYPE TABLE OF bapiret2,

  lt_comp           TYPE cl_abap_structdescr=>component_table,
  lv_structure      TYPE ztag_extr_prog-zstructure,
  lv_name_prog      TYPE ztag_extr_prog-zprograma,
  list              TYPE vrm_values,
  value             TYPE vrm_value,
  gv_save           TYPE c,

  wa_data           TYPE REF TO data.

FIELD-SYMBOLS:
  <wa_tab> TYPE any,
  <t_tab>  TYPE STANDARD TABLE. "Ponteiro para manipular tabela interna dinâmica

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t01.
PARAMETERS:  p_extr TYPE ztag_extr_prog-zextrator AS LISTBOX
                    VISIBLE LENGTH 20
                    USER-COMMAND ext.
SELECTION-SCREEN END OF BLOCK b1.


SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-t02.
PARAMETERS:
* Inicio Alteração - MA004818 Marcelo Alvares - 06.08.2018 10:29:35
*  p_maxreg TYPE i OBLIGATORY DEFAULT 100, "Remover obrigatoriedade
  p_maxreg TYPE i DEFAULT '2100999999',
* Fim alteração - MA004818 Marcelo Alvares - 06.08.2018 10:29:43
*  p_cabec  TYPE c AS CHECKBOX,
  p_save   TYPE c AS CHECKBOX USER-COMMAND sav,
  p_serv   TYPE c AS CHECKBOX USER-COMMAND srv,
  p_fname  TYPE localfile MODIF ID p1.
SELECTION-SCREEN END OF BLOCK b2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.
*  PERFORM set_screen.
  IF p_save IS NOT INITIAL.

    CALL FUNCTION 'F4_FILENAME'
      EXPORTING
        field_name = 'p_fname'
      IMPORTING
        file_name  = p_fname.
  ENDIF.

AT SELECTION-SCREEN.
*  gd_ucomm = sy-ucomm. "capture user command
  PERFORM set_screen.

AT SELECTION-SCREEN OUTPUT.
  PERFORM set_screen.


INITIALIZATION.

  "Get extractor´s names into listbox field
  SELECT * FROM ztag_extr_prog INTO TABLE ti_ztag_extr_prog.
  LOOP AT ti_ztag_extr_prog ASSIGNING FIELD-SYMBOL(<ztag_extr_prog>).
    value-key  = <ztag_extr_prog>-zextrator.
    value-text = <ztag_extr_prog>-zdescricao.
    APPEND value TO list.
    CLEAR value.
  ENDLOOP.

* Inicio Alteração - MA004818 Marcelo Alvares - 06.08.2018 18:01:46
  SORT list ASCENDING AS TEXT BY text.
* Fim alteração - MA004818 Marcelo Alvares - 06.08.2018 18:01:49

  "Calling function module to populate values on dropdown
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = 'p_extr'  " Field name
      values = list. "Dropdown values

  PERFORM set_screen.

START-OF-SELECTION.

  TRY .

* Não tem nenhum objeto de extração selecionado
      CHECK p_extr IS NOT INITIAL.

      "Get program extractor name and return structure
      SELECT SINGLE zprograma zstructure
        FROM ztag_extr_prog
        INTO (lv_name_prog, lv_structure)
       WHERE zextrator = p_extr.

* Não encontrou o programa nem a estrutura
      CHECK sy-subrc IS INITIAL.

      TRANSLATE lv_name_prog TO UPPER CASE.

      "Get components from returning structure
      "Assign extraction table and respective work are

      CALL METHOD cl_abap_structdescr=>describe_by_name
        EXPORTING
          p_name         = lv_structure
        EXCEPTIONS
          type_not_found = 1
          OTHERS         = 2.
      IF sy-subrc NE 0.
        MESSAGE ID 'ZAG_MIG' TYPE 'S' NUMBER '010' DISPLAY LIKE 'E'
            WITH lv_structure.
        STOP.
      ENDIF.

* Determine type of table structure from Data Dictionary
      o_struct ?= cl_abap_typedescr=>describe_by_name( lv_structure ).

      lt_comp  = o_struct->get_components( ).
      o_strtype = cl_abap_structdescr=>create( lt_comp ).

      o_tabtype = cl_abap_tabledescr=>create( p_line_type  = o_strtype
                                              p_table_kind = cl_abap_tabledescr=>tablekind_std
                                              p_unique     = abap_false ).


      TRY.
          CREATE DATA wa_data  TYPE HANDLE o_tabtype.
        CATCH cx_sy_create_data_error.
          "Implement error treatment
      ENDTRY.

      TRY.
          ASSIGN wa_data->* TO <t_tab>.
        CATCH cx_sy_assign_cast_illegal_cast.
          "Implement error treatment
        CATCH cx_sy_assign_cast_unknown_type.
          "Implement error treatment
        CATCH cx_sy_assign_out_of_range.
          "Implement error treatment
      ENDTRY.

      TRY.
          CREATE DATA wa_data  TYPE HANDLE o_strtype.
        CATCH cx_sy_create_data_error.
          "Implement error treatment
      ENDTRY.

      TRY.
          ASSIGN wa_data->* TO <wa_tab>.
        CATCH cx_sy_assign_cast_illegal_cast.
          "Implement error treatment
        CATCH cx_sy_assign_cast_unknown_type.
          "Implement error treatment
        CATCH cx_sy_assign_out_of_range.
          "Implement error treatment
      ENDTRY.

      TRY .
          "Verify if extractor program exists
          SELECT SINGLE obj_name
            FROM tadir
            INTO @DATA(lv_prog)
           WHERE pgmid    = @c_repository
             AND object   = @c_program
             AND obj_name = @lv_name_prog.

          IF lv_prog IS INITIAL.
            MESSAGE i003(zag_mig) WITH lv_name_prog DISPLAY LIKE 'E' .
            RETURN.
          ENDIF.
          "Calls the extractor program
          SUBMIT (lv_name_prog)
             VIA SELECTION-SCREEN
            WITH p_maxreg = p_maxreg
              AND RETURN.
        CATCH cx_root.
          "Implement suitable error handling here
      ENDTRY.

      IMPORT: <t_tab>   FROM MEMORY ID 'AG',
              ti_return FROM MEMORY ID 'RET'.

* Verifica se a importação dos parametros em memoria
* Se não existir, não foram passados dados.
      CHECK sy-subrc IS INITIAL.

      FREE MEMORY ID: 'AG', 'RET'.


* log standard para todos os registros da ti_return
* OBJETO    - ZPROJ_HANA
* SUBOBJETO - ZAG_EXTRACAO

      PERFORM gera_log USING lv_name_prog ti_return.

      "To show return messages
      "call function 'C14Z_MESSAGES_SHOW_AS_POPUP'
      READ TABLE ti_return WITH KEY type = 'E'
                           ASSIGNING FIELD-SYMBOL(<return>).
      IF sy-subrc IS INITIAL.
        CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
          EXPORTING
            i_msgid = <return>-id
            i_msgty = <return>-type
            i_msgno = <return>-number
            i_msgv1 = <return>-message_v1
            i_msgv2 = <return>-message_v2
            i_msgv3 = <return>-message_v3
            i_msgv4 = <return>-message_v4
*           I_LINENO            =
*         TABLES
*           I_MESSAGE_TAB       =
          .
      ELSE.
        "Save or show the extracted table
        PERFORM despacha_extracao USING p_fname
                                        p_save
                                        p_serv
                                        <t_tab>
                                        lv_structure.

      ENDIF.


    CATCH cx_root.
      MESSAGE i001(zag_mig) DISPLAY LIKE 'E' .
      RETURN.
  ENDTRY.


*&---------------------------------------------------------------------*
*& Form DESPACHA_EXTRACAO
*&---------------------------------------------------------------------*
*& Grava ou exibe dados extraídos
*&---------------------------------------------------------------------*
*&      --> P_FNAME     Nome do arquivo a gravar a extração
*&      --> P_SAVE      Flag para gravar extração em disco
*&      --> P_SERV      Flag para gravação no servidor / arqvuivo local
*&      --> P_TI_TABLE  Tabela com os dados extraídos
*&---------------------------------------------------------------------*
FORM despacha_extracao USING p_fname     TYPE localfile
                             p_save      TYPE c
                             p_serv      TYPE c
                             p_ti_table  TYPE ANY TABLE
                             p_structure TYPE ztag_extr_prog-zstructure.

  DATA:
*    lv_xml       TYPE string,
    lv_nr_lines  TYPE i,
    lo_table     TYPE REF TO cl_salv_table,
    lo_xml_doc   TYPE REF TO cl_xml_document,
    lx_xml       TYPE xstring,
    vl_file      TYPE string,
    l_length     TYPE i,
    l_xml_stream TYPE xml_rawdata.

*  DATA: lc_col      TYPE fieldname,
*        lc_col_desc TYPE scrtext_m,
*        it_dd04     TYPE TABLE OF dd04t,
*        wa_dd04     TYPE dd04t,
*        it_dd03     TYPE TABLE OF dd03l.

  TRY .
      cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = p_ti_table ).

      IF p_save IS INITIAL.
*        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = p_ti_table ).
        lo_table->set_screen_status( pfstatus = 'SALV_STANDARD'
                                report = 'SALV_DEMO_TABLE_FUNCTIONS'
                                set_functions = lo_table->c_functions_all ).
        lo_table->display( ).

      ELSE.

        lx_xml = lo_table->to_xml( xml_type = '10' ). "XLSX

        vl_file = p_fname.

        CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
          EXPORTING
            buffer        = lx_xml
          IMPORTING
            output_length = l_length
          TABLES
            binary_tab    = l_xml_stream.

        CALL METHOD cl_gui_frontend_services=>gui_download
          EXPORTING
            bin_filesize = l_length
            filetype     = 'BIN'
            filename     = vl_file
          CHANGING
            data_tab     = l_xml_stream
          EXCEPTIONS
            OTHERS       = 1.

        IF sy-subrc <> 0.
*       MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*                  WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
        ENDIF.


*        CALL TRANSFORMATION id
*                     SOURCE data_node = p_ti_table
*                     RESULT XML lv_xml.
*
*        REPLACE FIRST OCCURRENCE OF
*            '<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">'
*          IN lv_xml
*          WITH '<asx:abap xmlns:asx="http://www.sap.com/abapxml">'.
*
*
*        IF p_cabec IS NOT INITIAL AND p_serv IS INITIAL.
*
*          SELECT *
*            INTO TABLE it_dd03
*            FROM dd03l
*           WHERE tabname = p_structure.
*
*          IF it_dd03[] IS NOT INITIAL.
*
*            SELECT *
*              INTO TABLE it_dd04
*              FROM dd04t FOR ALL ENTRIES IN it_dd03
*             WHERE rollname = it_dd03-fieldname
*               AND ddlanguage = 'PT'.
*
*            LOOP AT  it_dd04 INTO wa_dd04.
*              IF wa_dd04-scrtext_m IS NOT INITIAL.
*                CONDENSE wa_dd04-rollname NO-GAPS.
*                CONDENSE wa_dd04-scrtext_m NO-GAPS.
**                 REPLACE ALL OCCURRENCES OF wa_dd04-rollname IN lv_xml WITH wa_dd04-scrtext_m.
*              ENDIF.
*            ENDLOOP.
*          ENDIF.
*        ENDIF.
*
*        CREATE OBJECT lo_xml_doc.
*        lo_xml_doc->parse_string( lv_xml ).
*        IF p_serv IS INITIAL.
*          lo_xml_doc->export_to_file( p_fname ).
*        ELSE.
*          "Gerando o arquivo no servidor.
*          OPEN DATASET p_fname FOR OUTPUT IN TEXT MODE ENCODING DEFAULT.
*          IF sy-subrc = 0.
*            LOOP AT p_ti_table ASSIGNING FIELD-SYMBOL(<line>).
*              TRANSFER <line> TO p_fname.
*            ENDLOOP.
*            CLOSE DATASET p_fname.
*          ENDIF.
*        ENDIF.

        DESCRIBE TABLE p_ti_table LINES lv_nr_lines.
        WRITE: /10 text-001, ': ', lv_nr_lines.
      ENDIF.
    CATCH cx_root.
      MESSAGE i001(zag_mig) DISPLAY LIKE 'E' .
  ENDTRY.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GERA_LOG
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_TI_RETURN  text
*----------------------------------------------------------------------*
FORM gera_log   USING  p_name_prog TYPE ztag_extr_prog-zprograma
                       p_ti_return LIKE ti_return.

  DATA: wa_return     TYPE bapiret2,
        wa_log        TYPE bal_s_log,
        lc_msg        TYPE bal_s_msg,
        it_log_handle TYPE bal_t_logh,
        wa_log_handle TYPE LINE OF bal_t_logh,
        lc_log_handle TYPE balloghndl.

  wa_log-object    = 'ZPROJ_HANA'.
  wa_log-subobject = 'ZAG_EXTRACAO'.
  wa_log-alprog    = p_name_prog.
  wa_log-aldate    = sy-datum.
  wa_log-altime    = sy-uzeit.
  wa_log-aluser    = sy-uname.

  CALL FUNCTION 'BAL_LOG_CREATE'
    EXPORTING
      i_s_log                 = wa_log
    IMPORTING
      e_log_handle            = lc_log_handle
    EXCEPTIONS
      log_header_inconsistent = 1
      OTHERS                  = 2.

  IF p_ti_return[] IS INITIAL.
    lc_msg-msgty  = 'S'.
    lc_msg-msgid  = 'ZAG_MIG'.
    lc_msg-msgno  = '007'.

    CALL FUNCTION 'BAL_LOG_MSG_ADD'
      EXPORTING
        i_log_handle     = lc_log_handle
        i_s_msg          = lc_msg
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.

    wa_log_handle = lc_log_handle.
    APPEND wa_log_handle TO it_log_handle.

  ELSE.
    LOOP AT p_ti_return INTO wa_return.

      CLEAR lc_msg.
      lc_msg-msgty  = 'E'.
      lc_msg-msgid  = 'ZAG_MIG'.
      lc_msg-msgno  = '008'.
      lc_msg-msgv1  = wa_return-message_v1.
      lc_msg-msgv2  = wa_return-message_v2.
      lc_msg-msgv3  = wa_return-message_v3.
      lc_msg-msgv4  = wa_return-message_v4.

      CALL FUNCTION 'BAL_LOG_MSG_ADD'
        EXPORTING
          i_log_handle     = lc_log_handle
          i_s_msg          = lc_msg
        EXCEPTIONS
          log_not_found    = 1
          msg_inconsistent = 2
          log_is_full      = 3
          OTHERS           = 4.

      wa_log_handle = lc_log_handle.
      APPEND wa_log_handle TO it_log_handle.

    ENDLOOP.
  ENDIF.

  CALL FUNCTION 'BAL_DB_SAVE'
    EXPORTING
      i_t_log_handle   = it_log_handle[]
    EXCEPTIONS
      log_not_found    = 1
      save_not_allowed = 2
      numbering_error  = 3
      OTHERS           = 4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  SET_SCREEN
*&---------------------------------------------------------------------*
FORM set_screen .

  DATA:
    lv_prefix(20) TYPE c,
    lv_name(40)   TYPE c,
    wa_screen     TYPE screen.

  "Set file´s prefix
  IF p_serv IS INITIAL.
    lv_prefix = c_prefix_fe.
  ELSE.
    lv_prefix = c_prefix_be.
  ENDIF.

  "Set the file name
  IF  sy-ucomm EQ 'EXT' OR
      sy-ucomm EQ 'SRV' OR    "listbox selection made
      sy-ucomm EQ 'SAV'.      "checkbox checked
    CONCATENATE '_' sy-sysid sy-mandt '_' sy-datlo sy-timlo c_sufix INTO lv_name.
    CONCATENATE lv_prefix p_extr lv_name INTO p_fname.

  ENDIF.

  "Show / hide filename field

  LOOP AT SCREEN INTO wa_screen.
    "Use cs as this then captures all elements of the field inc text
*    IF wa_screen-name CS 'P_FNAME'.
    IF wa_screen-group1 EQ 'P1'.
      IF p_save IS NOT INITIAL.
        wa_screen-active = 1. "Display field on screen
*        wa_screen-input  = 1. "set field as display only
*        wa_screen-invisible = 0.
        MODIFY screen FROM wa_screen.
      ELSE.
        wa_screen-active = 0. "remove field from screen
*        wa_screen-input  = 0. "set field as display only
*        wa_screen-invisible = 1.
        MODIFY screen FROM wa_screen.
      ENDIF.
    ENDIF.

  ENDLOOP.


ENDFORM.
