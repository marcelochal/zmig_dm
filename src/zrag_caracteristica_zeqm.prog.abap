*&---------------------------------------------------------------------*
*& Report ZRAG_CARACTERISTICA_ZEQM
*&---------------------------------------------------------------------*
*&  Programa de Extração MM - Classe Materiais ZEQM
*&  Programador: Wilson Perotta Jose
*&---------------------------------------------------------------------*

REPORT ZRAG_CARACTERISTICA_ZEQM.

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
          atinn(30)                TYPE c,
        END OF ty_record,

        ti_record TYPE STANDARD TABLE OF ty_record.

TYPES : BEGIN OF ty_record2,
          atinn(30)                TYPE C,
          atzhl(4)                 TYPE C,
          annel(2)                 TYPE C,
          atwrt(30)                TYPE C,
        END OF ty_record2,

        ti_record2 TYPE STANDARD TABLE OF ty_record2.


DATA:
  wa_return                TYPE bapiret2,
  lo_table                 TYPE REF TO cl_salv_table,
  ti_return                TYPE TABLE OF bapiret2,


  ti_mat_imp               TYPE TABLE OF ty_mat,
  ti_mat_exp               TYPE TABLE OF ty_mat_exp,
  wa_mat_exp               TYPE  ty_mat_exp,
  wa_mat                   TYPE ty_mat,
  gc_matnr                 TYPE matnr,
  lc_classe(6)             TYPE C,
  li_index                 TYPE I,
  lc_val(2)                TYPE C,

  ti_file_zeqm             TYPE TABLE OF ZEMM_EXT_SANEAMENTO_ZEQM,
  wa_file_zeqm             TYPE  ZEMM_EXT_SANEAMENTO_ZEQM,

  t_file_classe            TYPE ti_record,
  wa_file_classe           TYPE ty_record,
  t_file_carval            TYPE ti_record2,
  wa_file_carval           TYPE ty_record2,

  ti_layout_carac_cockpit TYPE STANDARD TABLE OF zemm_ext_carac_zeqm,
  wa_layout_carac_cockpit TYPE zemm_ext_carac_zeqm.

FIELD-SYMBOLS <t_tab>  TYPE STANDARD TABLE.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-t00.
  SELECT-OPTIONS : so_matnr  FOR gc_matnr MATCHCODE OBJECT mat1 NO INTERVALS.
  PARAMETERS  p_classe LIKE rlgrap-filename  OBLIGATORY.
  PARAMETERS  p_carval LIKE rlgrap-filename  OBLIGATORY.
  PARAMETERS  p_zeqm   LIKE rlgrap-filename  OBLIGATORY.
PARAMETERS:
  p_corte  TYPE d OBLIGATORY DEFAULT '20170101',
  p_maxreg TYPE i OBLIGATORY DEFAULT 100,     "AGIR - Obrigatório
  p_local  TYPE c AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.


AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_classe.

  PERFORM seleciona_arquivo.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_carval.

  PERFORM seleciona_arquivo2.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_zeqm.

  PERFORM seleciona_arquivo3.


START-OF-SELECTION.

*** Upload do Arquivo
  PERFORM upload_arquivo   TABLES t_file_classe
                              USING  p_classe.

  PERFORM upload_arquivo2   TABLES t_file_carval
                               USING  p_carval.

  PERFORM upload_arquivo_zeqm   TABLES ti_file_zeqm
                               USING  p_zeqm.


  TRY .


      IF ti_file_zeqm[] IS INITIAL.
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
      "SORT t_file_classe BY class.
      "SORT t_file_carval BY atinn annel.

      LOOP AT ti_file_zeqm INTO wa_file_zeqm.

        lc_classe = wa_file_zeqm-tuc && '_' && wa_file_zeqm-tbem.

        CLEAR: li_index, wa_layout_carac_cockpit.

        "Encontra as características da classe lc_classe
        READ TABLE  t_file_classe INTO wa_file_classe WITH KEY class = lc_classe.

        IF sy-subrc = 0.

           LOOP AT t_file_classe INTO wa_file_classe WHERE class = lc_classe.

             ADD 1 TO li_index.

             CLEAR: lc_val , wa_layout_carac_cockpit.

             IF      li_index = 1.
               lc_val = wa_file_zeqm-carac1.
             ELSEIF  li_index = 2.
               lc_val = wa_file_zeqm-carac2.
             ELSEIF  li_index = 3.
               lc_val = wa_file_zeqm-carac3.
             ELSEIF  li_index = 4.
               lc_val = wa_file_zeqm-carac4.
             ELSEIF  li_index = 5.
               lc_val = wa_file_zeqm-carac5.
             ENDIF.

*             "Encontra o valor da característica
*             READ TABLE t_file_carval INTO wa_file_carval
*                                  WITH KEY atinn = wa_file_classe-atinn
*                                           annel = lc_val. "BINARY SEARCH.
*
*             IF sy-subrc = 0.
*
*                CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
*                  EXPORTING
*                    INTEXT                  = wa_file_carval-atinn
*                  IMPORTING
*                    OUTTEXT                 = wa_file_carval-atinn.
*
*                CALL FUNCTION 'SCP_REPLACE_STRANGE_CHARS'
*                  EXPORTING
*                    INTEXT                  = wa_file_carval-atwrt
*                  IMPORTING
*                    OUTTEXT                 = wa_file_carval-atwrt.
*
*                "Preenche
*                wa_layout_carac_cockpit-atzhl  = wa_file_carval-atzhl.
*                wa_layout_carac_cockpit-atwrt  = wa_file_carval-atwrt.
*
*             ENDIF.

              wa_layout_carac_cockpit-atwrt  = lc_val.
              wa_layout_carac_cockpit-atinn  = wa_file_classe-atinn.    "característica
              wa_layout_carac_cockpit-matnr  = wa_file_zeqm-matnr.
              wa_layout_carac_cockpit-maktx  = wa_file_zeqm-maktx.
              wa_layout_carac_cockpit-class  = lc_classe.
              APPEND wa_layout_carac_cockpit TO  ti_layout_carac_cockpit.
           ENDLOOP.
        ELSE.


           wa_layout_carac_cockpit-matnr  = wa_file_zeqm-matnr.
           wa_layout_carac_cockpit-maktx  = wa_file_zeqm-maktx.
           wa_layout_carac_cockpit-class  = lc_classe.
           APPEND wa_layout_carac_cockpit TO  ti_layout_carac_cockpit.
        ENDIF.

      ENDLOOP.

      IF p_local IS NOT INITIAL.
        cl_salv_table=>factory( IMPORTING r_salv_table = lo_table CHANGING t_table = ti_layout_carac_cockpit ).
        lo_table->display( ).

      ELSE.
        "AGIR - Extração.
        "Comandos obrigatórios
        "Exporta resultado para Id de Memória
        ASSIGN ti_layout_carac_cockpit TO <t_tab>.
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


FORM seleciona_arquivo2 .
  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    CHANGING
      file_name = p_carval
    EXCEPTIONS
      OTHERS    = 2.
ENDFORM.


FORM seleciona_arquivo3 .
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

FORM upload_arquivo2 TABLES   p_t_file  TYPE ti_record2
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
