CLASS zcl_extrator_info_nota_manut DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES:  BEGIN OF ty_crhd,
              objid TYPE cr_objid,
              begda TYPE begdatum,
              endda TYPE enddatum,
              arbpl TYPE arbpl,
              werks TYPE werks_d,
              verwe TYPE ap_verwe,
              planv TYPE ap_planv,
              stand TYPE ap_stand,
              veran TYPE ap_veran,
              vgwts TYPE vorgschl,
              steus TYPE steus,
              fort1 TYPE ap_form_t1,
            END OF ty_crhd,

            BEGIN OF ty_qmel,
              qmnum TYPE qmnum,
              objnr TYPE qmobjnr,
            END OF ty_qmel,

            BEGIN OF ty_jsto,
              objnr TYPE j_objnr,
              stsma TYPE j_stsma,
            END OF ty_jsto,

            BEGIN OF ty_jest,
              objnr TYPE j_objnr,
              stat  TYPE j_status,
              inact TYPE j_inact,
            END OF ty_jest,

            BEGIN OF ty_tj30t,
              stsma TYPE j_stsma,
              estat TYPE j_estat,
              spras TYPE spras,
              txt04 TYPE j_txt04,
            END OF ty_tj30t.

    "Categoria de tabelas
    TYPES: ty_nota_manu_excel_tab TYPE STANDARD TABLE OF zeagir_nota_manu WITH DEFAULT KEY,
           ty_return_tab          TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY,
           ty_range_data          TYPE RANGE OF qmel-qmdat.

    "Constantes
    CONSTANTS: BEGIN OF c_tab_valores,
                 tipo_erro TYPE bapi_mtype VALUE 'E',
                 data_val  TYPE datbi      VALUE '99991231',
                 objty     TYPE cr_objty   VALUE 'A',
                 status_1  TYPE char3      VALUE 'EXC',
                 status_2  TYPE char3      VALUE 'exc',
                 status_3  TYPE char3      VALUE 'Exc',
                 status_4  TYPE char7      VALUE 'EXCLUIR',
                 status_5  TYPE char7      VALUE 'excluir',
                 status_6  TYPE char7      VALUE 'Excluir',
                 status_7  TYPE char4      VALUE 'INAT',
                 status_8  TYPE char7      VALUE 'inat',
                 status_9  TYPE char7      VALUE 'Inat',
                 status_10 TYPE char7      VALUE 'INATIVO',
                 status_11 TYPE char7      VALUE 'inativo',
                 status_12 TYPE char7      VALUE 'Inativo',
                 status_13 TYPE char12     VALUE 'NÃO UTILIZAR',
                 status_14 TYPE char12     VALUE 'não utilizar',
                 status_15 TYPE char12     VALUE 'Não utilizar',
                 intervalo TYPE sign       VALUE 'I',
                 option    TYPE option     VALUE 'EQ',
                 option_cp TYPE option     VALUE 'CP',
                 nota_1    TYPE qmart      VALUE 'T1',
                 nota_2    TYPE qmart      VALUE 'T2',
                 nota_3    TYPE qmart      VALUE 'T3',
                 nota_4    TYPE qmart      VALUE 'T4',
                 nota_5    TYPE qmart      VALUE 'T5',
                 st_nota_1 TYPE sttxt      VALUE 'MSPN',
                 st_nota_2 TYPE sttxt      VALUE 'MSPR',
                 st_nota_3 TYPE sttxt      VALUE 'MSPR ORDA',
                 line_1    TYPE syst_tabix VALUE 1,
                 tip_prior TYPE artpr      VALUE 'OC',
                 stat      TYPE char1      VALUE 'E',
                 ini_qm    TYPE char2      VALUE 'QM',
                 id_text   TYPE tdid       VALUE 'LTXT',
                 obj_text  TYPE tdobject   VALUE 'QMEL',
                 asterisco TYPE char1      VALUE '*',
                 tipo_x    TYPE char1      VALUE 'X',
               END OF c_tab_valores .

    "Métodos
    METHODS: constructor   IMPORTING VALUE(i_maxreg)    TYPE i
                                     VALUE(i_rang_data) TYPE ty_range_data,
      exporta_dados EXPORTING VALUE(e_itab_nota_manut) TYPE ty_nota_manu_excel_tab
                              VALUE(e_itab_return)     TYPE ty_return_tab .
  PROTECTED SECTION.
  PRIVATE SECTION.
    "Atributos
    DATA: t_notas_manut TYPE STANDARD TABLE OF zeagir_notas,
          t_crhd        TYPE STANDARD TABLE OF ty_crhd,
          t_qmel        TYPE STANDARD TABLE OF ty_qmel,
          t_jsto        TYPE STANDARD TABLE OF ty_jsto,
          t_jest        TYPE STANDARD TABLE OF ty_jest,
          t_tj30t       TYPE STANDARD TABLE OF ty_tj30t.

    DATA: lv_maxreg TYPE i,
          r_qmart   TYPE RANGE OF qmel-qmart,
          r_status  TYPE RANGE OF t132t-sttxt.

    "Métodos
    METHODS: set_tipos_notas,
      get_notas_manutencao IMPORTING VALUE(i_rang_data) TYPE ty_range_data,
      get_status_usuario,
      get_centro_trabalho,
      exporta_nota_manutencao EXPORTING VALUE(e_itab_nota_manut) TYPE ty_nota_manu_excel_tab
                                        VALUE(e_itab_return)     TYPE ty_return_tab,
      check_desc_notas_manute CHANGING VALUE(c_itab_nota_manut)  TYPE ty_nota_manu_excel_tab,
      check_dados_notas_manut IMPORTING VALUE(i_itab_nota_manut) TYPE ty_nota_manu_excel_tab
                              RETURNING VALUE(r_itab_return)     TYPE ty_return_tab.
ENDCLASS.

CLASS zcl_extrator_info_nota_manut IMPLEMENTATION.

  METHOD check_dados_notas_manut.
    IF i_itab_nota_manut IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.

  METHOD check_desc_notas_manute.
    LOOP AT c_itab_nota_manut ASSIGNING FIELD-SYMBOL(<fs_nota_manut>).
      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_1 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_2 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_3 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_4 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_5 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_6 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_7 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_8 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_9 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_10 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_11 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_12 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_13 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_14 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_nota_manut>-qmtxt FOR c_tab_valores-status_15 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_nota_manut INDEX sy-tabix. CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD constructor.
    lv_maxreg = i_maxreg.
    set_tipos_notas( ).
    get_notas_manutencao( i_rang_data ).
    get_status_usuario( ).
    get_centro_trabalho( ).
  ENDMETHOD.

  METHOD exporta_dados.
    " Extraindo as informações do Plano de Manutenção
    exporta_nota_manutencao( IMPORTING e_itab_nota_manut = e_itab_nota_manut e_itab_return = e_itab_return ). RETURN.

    "Limpando todas as tabelas.
    FREE: t_notas_manut, r_qmart, r_status, t_qmel, t_jsto, t_jest, t_tj30t. CLEAR lv_maxreg.
  ENDMETHOD.

  METHOD exporta_nota_manutencao.
    DATA: lv_objnr     TYPE qmobjnr,
          lv_text_nota TYPE tdobname,
          lv_text      TYPE string,
          t_tline      TYPE STANDARD TABLE OF tline.

    LOOP AT t_notas_manut ASSIGNING FIELD-SYMBOL(<fs_notas_manut>).
      APPEND INITIAL LINE TO e_itab_nota_manut ASSIGNING FIELD-SYMBOL(<fs_excel_nota_manut>).
      MOVE-CORRESPONDING <fs_notas_manut> TO <fs_excel_nota_manut>.
      <fs_excel_nota_manut>-txt04 = <fs_notas_manut>-txt04.
      <fs_excel_nota_manut>-swerk = <fs_notas_manut>-swerk.
      <fs_excel_nota_manut>-mncod = <fs_notas_manut>-mncod.

      lv_text_nota = <fs_notas_manut>-qmnum.
      TRY.
          CALL FUNCTION 'READ_TEXT'
            EXPORTING
              client                  = sy-mandt
              id                      = c_tab_valores-id_text
              language                = sy-langu
              name                    = lv_text_nota
              object                  = c_tab_valores-obj_text
            TABLES
              lines                   = t_tline
            EXCEPTIONS
              id                      = 1
              language                = 2
              name                    = 3
              not_found               = 4
              object                  = 5
              reference_check         = 6
              wrong_access_to_archive = 7
              OTHERS                  = 8.

          IF sy-subrc NE 0.
            CLEAR lv_text_nota.
          ENDIF.

          IF NOT t_tline IS INITIAL.
            LOOP AT t_tline ASSIGNING FIELD-SYMBOL(<fs_tline>).
              IF lv_text IS INITIAL.
                lv_text = <fs_tline>-tdline.
              ELSE.
                CONCATENATE lv_text <fs_tline>-tdline INTO lv_text SEPARATED BY space.
              ENDIF.
            ENDLOOP.
            <fs_excel_nota_manut>-tdline = lv_text. CLEAR lv_text.

          ENDIF.
          UNASSIGN <fs_tline>. CLEAR lv_text_nota. FREE t_tline.

        CATCH cx_root.
      ENDTRY.

      <fs_excel_nota_manut>-urcod = <fs_notas_manut>-urcod.
      <fs_excel_nota_manut>-urgrp = <fs_notas_manut>-urgrp.

      READ TABLE t_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>) WITH KEY objid = <fs_notas_manut>-arbpl BINARY SEARCH.
      IF <fs_crhd> IS ASSIGNED.
        <fs_excel_nota_manut>-gewrk = <fs_crhd>-arbpl.
        <fs_excel_nota_manut>-arbplwerk = <fs_crhd>-werks.
      ENDIF.
      UNASSIGN <fs_crhd>.

      CONCATENATE c_tab_valores-ini_qm <fs_notas_manut>-qmnum INTO lv_objnr.
      READ TABLE t_qmel ASSIGNING FIELD-SYMBOL(<fs_qmel>) WITH KEY objnr = lv_objnr BINARY SEARCH. CLEAR lv_objnr.
      IF <fs_qmel> IS ASSIGNED.
        READ TABLE t_jsto ASSIGNING FIELD-SYMBOL(<fs_jsto>) WITH KEY objnr = <fs_qmel>-objnr BINARY SEARCH.
        IF <fs_jsto> IS ASSIGNED.
          READ TABLE t_tj30t ASSIGNING FIELD-SYMBOL(<fs_tj30t>) WITH KEY stsma = <fs_jsto>-stsma BINARY SEARCH.
          IF <fs_tj30t> IS ASSIGNED.
            <fs_excel_nota_manut>-status_usuario = <fs_tj30t>-txt04.
          ENDIF.
          UNASSIGN <fs_tj30t>.
        ENDIF.
        UNASSIGN <fs_jsto>.
      ENDIF.
      UNASSIGN <fs_qmel>.

      SELECT SINGLE priokx INTO @<fs_excel_nota_manut>-priokx
      FROM t356_t WHERE spras EQ @sy-langu AND artpr EQ @c_tab_valores-tip_prior AND priok EQ @<fs_excel_nota_manut>-priok.

    ENDLOOP.
    UNASSIGN: <fs_notas_manut>, <fs_excel_nota_manut>. FREE t_crhd.

    IF NOT e_itab_nota_manut IS INITIAL.
      check_desc_notas_manute( CHANGING c_itab_nota_manut = e_itab_nota_manut ).
      e_itab_return = check_dados_notas_manut( e_itab_nota_manut ).
    ENDIF.
  ENDMETHOD.

  METHOD get_notas_manutencao.
    TRY.
        CALL FUNCTION 'ZFPM_OBTER_NOTAS_MANUT'
          EXPORTING
            i_range_data  = i_rang_data
            i_range_qmart = r_qmart
            i_maxreg      = lv_maxreg
          TABLES
            t_notas_t2_t3 = t_notas_manut.

        IF NOT t_notas_manut IS INITIAL.
          DELETE t_notas_manut WHERE txt04 NOT IN r_status.
        ENDIF.

      CATCH cx_root.
        FREE r_qmart. CLEAR lv_maxreg. EXIT.
    ENDTRY.

  ENDMETHOD.

  METHOD get_status_usuario.

    IF NOT t_notas_manut IS INITIAL.
      SELECT objnr, objnr
      INTO TABLE @t_qmel
      FROM qmel
      FOR ALL ENTRIES IN @t_notas_manut
      WHERE qmnum EQ @t_notas_manut-qmnum.
    ENDIF.

    IF NOT t_qmel IS INITIAL.
      SORT t_qmel BY objnr.

      SELECT objnr, stsma
      INTO TABLE @t_jsto
      FROM jsto
      FOR ALL ENTRIES IN @t_qmel
      WHERE objnr EQ @t_qmel-objnr.
    ENDIF.

    IF NOT t_jsto IS INITIAL.
      SORT t_jsto BY objnr.

      SELECT objnr, stat, inact
      INTO TABLE @t_jest
      FROM jest
      FOR ALL ENTRIES IN @t_jsto
      WHERE objnr EQ @t_jsto-objnr AND
            inact EQ @abap_off.

      SORT t_jest BY objnr.
      DELETE t_jest WHERE stat(1) NE c_tab_valores-stat.
    ENDIF.

    IF NOT t_jsto IS INITIAL.
      SELECT stsma, estat, spras, txt04
      INTO TABLE @t_tj30t
      FROM tj30t
      FOR ALL ENTRIES IN @t_jsto
      WHERE stsma EQ @t_jsto-stsma AND spras EQ @sy-langu.

      IF NOT t_tj30t IS INITIAL.
        IF NOT t_jest IS INITIAL.
          LOOP AT t_jest ASSIGNING FIELD-SYMBOL(<fs_jest>).
            DELETE t_tj30t WHERE estat NE <fs_jest>-stat.
          ENDLOOP.
          UNASSIGN <fs_jest>.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.

  METHOD get_centro_trabalho.
    IF NOT t_notas_manut IS INITIAL.
      SELECT objid, begda, endda, arbpl, werks, verwe, planv, stand, veran, vgwts, steus, fort1
      INTO TABLE @t_crhd
      FROM crhd
      FOR ALL ENTRIES IN @t_notas_manut
      WHERE objty EQ @c_tab_valores-objty AND
            objid EQ @t_notas_manut-arbpl.              "#EC CI_NOFIRST

      SORT t_crhd BY objid.
    ENDIF.
  ENDMETHOD.

  METHOD set_tipos_notas.
    APPEND INITIAL LINE TO r_status ASSIGNING FIELD-SYMBOL(<fs_status>).
    <fs_status>-sign   = c_tab_valores-intervalo.
    <fs_status>-option = c_tab_valores-option.
    <fs_status>-low    = c_tab_valores-st_nota_1.
    UNASSIGN <fs_status>.

    APPEND INITIAL LINE TO r_status ASSIGNING <fs_status>.
    <fs_status>-sign   = c_tab_valores-intervalo.
    <fs_status>-option = c_tab_valores-option.
    <fs_status>-low    = c_tab_valores-st_nota_2.
    UNASSIGN <fs_status>.

    APPEND INITIAL LINE TO r_status ASSIGNING <fs_status>.
    <fs_status>-sign   = c_tab_valores-intervalo.
    <fs_status>-option = c_tab_valores-option.
    <fs_status>-low    = c_tab_valores-st_nota_3.
    UNASSIGN <fs_status>.

  ENDMETHOD.

ENDCLASS.
