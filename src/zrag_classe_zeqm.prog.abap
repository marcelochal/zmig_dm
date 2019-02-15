*&---------------------------------------------------------------------*
*& Report ZRAG_CLASSE_ZEQM
*&---------------------------------------------------------------------*
*&  Programa de Extração MM - Classe Materiais ZEQM
*&  Programador: Wilson Perotta Jose
*&---------------------------------------------------------------------*

REPORT ZRAG_CLASSE_ZEQM.

TYPES:
 BEGIN OF ty_mat_exp,
    matnr TYPE matnr,
    maktx TYPE maktx,
  END OF ty_mat_exp,

  BEGIN OF ty_mat,
    matnr TYPE matnr,
  END OF ty_mat.


TYPES : BEGIN OF ty_record,
          class(18)                TYPE c,
          kschg(60)                TYPE c,
        END OF ty_record,

        ti_record TYPE STANDARD TABLE OF ty_record.

DATA:
  wa_return                TYPE bapiret2,
  lo_table                 TYPE REF TO cl_salv_table,
  ti_return                TYPE TABLE OF bapiret2,
  ti_zequmat               TYPE TABLE OF zequmat,
  wa_zequmat               TYPE zequmat,
  ti_mat_imp               TYPE TABLE OF ty_mat,
  ti_mat_exp               TYPE TABLE OF ty_mat_exp,
  ti_m_clasb               TYPE TABLE OF m_clasb,
  wa_m_clasb               TYPE m_clasb,
  wa_mat_exp               TYPE  ty_mat_exp,
  wa_mat                   TYPE ty_mat,
  gc_matnr                 TYPE matnr,
  ti_file_classe           TYPE ti_record,
  wa_file_classe           TYPE ty_record,

  ti_file_zeqm             TYPE TABLE OF ZEMM_EXT_SANEAMENTO_ZEQM,
  wa_file_zeqm             TYPE  ZEMM_EXT_SANEAMENTO_ZEQM,

  ti_layout_classe_cockpit TYPE STANDARD TABLE OF zemm_ext_classe_zeqm,
  wa_layout_classe_cockpit TYPE zemm_ext_classe_zeqm.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
*  SELECT-OPTIONS : so_matnr  FOR gc_matnr MATCHCODE OBJECT mat1 NO INTERVALS.
  PARAMETERS  p_classe LIKE rlgrap-filename OBLIGATORY .
  PARAMETERS  p_zeqm   LIKE rlgrap-filename OBLIGATORY.

PARAMETERS:
  p_corte  TYPE d OBLIGATORY DEFAULT '20170101',
  p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_classe.

  PERFORM seleciona_arquivo.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zeqm.

  PERFORM seleciona_arquivo_zeqm.


START-OF-SELECTION.

*** Upload do Arquivo
  IF p_classe IS NOT INITIAL.
     PERFORM upload_arquivo   TABLES ti_file_classe
                              USING  p_classe.

    PERFORM upload_arquivo_zeqm   TABLES ti_file_zeqm
                              USING  p_zeqm.


  ENDIF.


  TRY .

      IF ti_file_zeqm IS INITIAL.
        MESSAGE ID 'ZAG_MIG' TYPE 'I' NUMBER 004 DISPLAY LIKE 'E'.

        MOVE sy-msgid TO wa_return-id.
        MOVE sy-msgno TO wa_return-number.
        MOVE sy-msgty TO wa_return-type.
        MOVE sy-msgv1 TO wa_return-message_v1.
        MOVE sy-msgv2 TO wa_return-message_v2.
        MOVE sy-msgv3 TO wa_return-message_v3.
        MOVE sy-msgv4 TO wa_return-message_v4.
        APPEND wa_return TO ti_return.
      ENDIF.


**** Monta dados file

     MOVE-CORRESPONDING ti_file_zeqm TO ti_layout_classe_cockpit.

** Monta dados da mara

      FIELD-SYMBOLS: <fs_classe> TYPE zemm_ext_classe_zeqm.

*      SORT ti_mat_exp BY matnr.
      SORT ti_file_classe BY class.
      SORT ti_mat_exp BY matnr.

      LOOP AT ti_layout_classe_cockpit ASSIGNING <fs_classe>.

*        READ TABLE ti_mat_exp INTO wa_mat_exp WITH KEY matnr = <fs_mat>-matnr BINARY SEARCH.
*        IF sy-subrc = 0.
*            <fs_mat>-maktx  = wa_mat_exp-maktx.
*        ENDIF.

        <fs_classe>-class = <fs_classe>-tuc && '_' && <fs_classe>-tbem.

        READ TABLE ti_file_classe INTO wa_file_classe WITH KEY class = <fs_classe>-class BINARY SEARCH.
        IF sy-subrc = 0.
            <fs_classe>-kschg  = wa_file_classe-kschg.
        ENDIF.


      ENDLOOP.

      UNASSIGN <fs_classe>.


      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_layout_classe_cockpit ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_layout_classe_cockpit TO <t_tab>.
        EXPORT:
          <t_tab>   TO MEMORY ID 'AG',
          ti_return TO MEMORY ID 'RET'.
        LEAVE PROGRAM.
      ENDIF.

    CATCH cx_root.
      MESSAGE e002(zag_mig) WITH sy-repid.
  ENDTRY.

*&---------------------------------------------------------------------*
*&      Form  SELECIONA_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_arquivo .
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name = p_classe
    EXCEPTIONS
      OTHERS    = 2.
ENDFORM.

*----------------------------------------------------------------------*
FORM seleciona_arquivo_zeqm .
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name = p_zeqm
    EXCEPTIONS
      OTHERS    = 2.
ENDFORM.



*&---------------------------------------------------------------------*
*&      Form  UPLOAD_ARQUIVO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_T_FILE  text
*      -->P_P_FILE  text
*----------------------------------------------------------------------*
FORM upload_arquivo TABLES   p_t_file  TYPE ti_record
                    USING    p_file.

  DATA : l_arquivo TYPE rlgrap-filename.


  l_arquivo = p_file.

  TYPE-POOLS: truxs.

  DATA: it_raw TYPE truxs_t_text_data.


  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
*      i_line_header        = 'X'
      i_tab_raw_data       = it_raw       " WORK TABLE
      i_filename           = l_arquivo
    TABLES
      i_tab_converted_data = p_t_file[]    "ACTUAL DATA
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.


ENDFORM.

FORM upload_arquivo_zeqm TABLES   p_t_file STRUCTURE ZEMM_EXT_SANEAMENTO_ZEQM
                         USING    p_file.

  DATA : l_arquivo TYPE rlgrap-filename.


  l_arquivo = p_file.

  TYPE-POOLS: truxs.

  DATA: it_raw TYPE truxs_t_text_data.


  CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
    EXPORTING
*     I_FIELD_SEPERATOR    =
*      i_line_header        = 'X'
      i_tab_raw_data       = it_raw       " WORK TABLE
      i_filename           = l_arquivo
    TABLES
      i_tab_converted_data = p_t_file[]    "ACTUAL DATA
    EXCEPTIONS
      conversion_failed    = 1
      OTHERS               = 2.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.


ENDFORM.
