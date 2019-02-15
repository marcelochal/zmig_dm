CLASS zcl_extrator_info_ordem_manut DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_aufk,
             aufnr TYPE aufnr,
             kostv TYPE aufkostv,
             abkrs TYPE aufabkrs,
             scope TYPE scope_cv,
           END OF ty_aufk,

           BEGIN OF ty_afih,
             aufnr TYPE aufnr,
             ilart TYPE ila,
           END OF ty_afih.

    TYPES:
 "Categoria de tabelas
 ty_ordem_manu_excel_tab TYPE STANDARD TABLE OF zeagir_ordem_manu WITH DEFAULT KEY .
    TYPES:
      ty_return_tab          TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY .
    TYPES:
      ty_range_data          TYPE RANGE OF aufk-erdat.

    TYPES: ty_ordens_tab     TYPE STANDARD TABLE OF zeagir_ordens WITH DEFAULT KEY.

    CONSTANTS:
      "Constantes
      BEGIN OF c_tab_valores,
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
        st_nota_1 TYPE sttxt      VALUE 'MSPN',
        st_nota_2 TYPE sttxt      VALUE 'MSPR',
        st_nota_3 TYPE sttxt      VALUE 'MSPR ORDA',
        line_1    TYPE syst_tabix VALUE 1,
        tip_prior TYPE artpr      VALUE 'OP',
      END OF c_tab_valores .

    "Métodos
    METHODS constructor
      IMPORTING
        VALUE(i_maxreg)    TYPE i
        VALUE(i_rang_data) TYPE ty_range_data .
    METHODS exporta_dados
      EXPORTING
        VALUE(e_itab_ordem_manut) TYPE ty_ordem_manu_excel_tab
        VALUE(e_itab_return)      TYPE ty_return_tab .
  PROTECTED SECTION.
  PRIVATE SECTION.
    "Atributos
    DATA: t_ordens_manut   TYPE STANDARD TABLE OF zeagir_ordens,
          t_ampliacao      TYPE STANDARD TABLE OF zes_ampliacao_list,
          t_header         TYPE STANDARD TABLE OF bapi_alm_order_header_e,
          t_ordens_inf_add TYPE STANDARD TABLE OF bapi_alm_order_operation_e,
          t_nomes          TYPE STANDARD TABLE OF zpmmega_nomes,
          t_aufk           TYPE STANDARD TABLE OF ty_aufk,
          t_afih           TYPE STANDARD TABLE OF ty_afih.

    DATA lv_maxreg TYPE i.

    "Métodos
    METHODS: get_ordens_manutencao IMPORTING VALUE(i_rang_data)  TYPE ty_range_data,
      get_tipo_ativ_manuten,
      get_inform_add        IMPORTING VALUE(i_tab_ordens) TYPE ty_ordens_tab,
      exporta_ordem_manutencao EXPORTING VALUE(e_itab_ordem_manut) TYPE ty_ordem_manu_excel_tab
                                         VALUE(e_itab_return)      TYPE ty_return_tab,
      check_dados_ordens_manut IMPORTING VALUE(i_itab_ordem_manut) TYPE ty_ordem_manu_excel_tab
                               RETURNING VALUE(r_itab_return)      TYPE ty_return_tab.
ENDCLASS.



CLASS ZCL_EXTRATOR_INFO_ORDEM_MANUT IMPLEMENTATION.


  METHOD check_dados_ordens_manut.
    IF i_itab_ordem_manut IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD constructor.
    lv_maxreg = i_maxreg.
    get_ordens_manutencao( i_rang_data ).
    get_tipo_ativ_manuten( ).
    get_inform_add( t_ordens_manut ).
  ENDMETHOD.


  METHOD exporta_dados.
    " Extraindo as informações do Plano de Manutenção
    exporta_ordem_manutencao( IMPORTING e_itab_ordem_manut = e_itab_ordem_manut e_itab_return = e_itab_return ). RETURN.

    "Limpando todas as tabelas.
    FREE: t_ordens_manut, t_afih, t_ordens_inf_add, t_header, t_aufk, t_nomes. CLEAR lv_maxreg.
  ENDMETHOD.


  METHOD exporta_ordem_manutencao.
    LOOP AT t_ordens_manut ASSIGNING FIELD-SYMBOL(<fs_ordens_manut>).
      APPEND INITIAL LINE TO e_itab_ordem_manut ASSIGNING FIELD-SYMBOL(<fs_excel_ordem_manut>).
      <fs_excel_ordem_manut>-order_type  = <fs_ordens_manut>-order_type.
      <fs_excel_ordem_manut>-ktext       = <fs_ordens_manut>-short_text.
      <fs_excel_ordem_manut>-s_status    = <fs_ordens_manut>-s_status.
      <fs_excel_ordem_manut>-u_status    = <fs_ordens_manut>-u_status.
      <fs_excel_ordem_manut>-tplnr       = <fs_ordens_manut>-funcloc.
      <fs_excel_ordem_manut>-equipment   = <fs_ordens_manut>-equipment.
      <fs_excel_ordem_manut>-orderid     = <fs_ordens_manut>-orderid.
      <fs_excel_ordem_manut>-plangroup   = <fs_ordens_manut>-plangroup.
      <fs_excel_ordem_manut>-planplant   = <fs_ordens_manut>-planplant.
      <fs_excel_ordem_manut>-mn_wk_ctr   = <fs_ordens_manut>-mn_wk_ctr.
      <fs_excel_ordem_manut>-vawrk       = <fs_ordens_manut>-vawrk.

      READ TABLE t_afih ASSIGNING FIELD-SYMBOL(<fs_afih>) WITH KEY aufnr = <fs_ordens_manut>-orderid BINARY SEARCH.
      IF <fs_afih> IS ASSIGNED.
        <fs_excel_ordem_manut>-ilatx  = <fs_afih>-ilart.
      ENDIF.
      UNASSIGN <fs_afih>.

      <fs_excel_ordem_manut>-anlzu       = <fs_ordens_manut>-anlzu.
      <fs_excel_ordem_manut>-start_date  = <fs_ordens_manut>-start_date.
      <fs_excel_ordem_manut>-finish_date = <fs_ordens_manut>-finish_date.
      <fs_excel_ordem_manut>-priokx      = <fs_ordens_manut>-priokx.
      <fs_excel_ordem_manut>-maintplant  = <fs_ordens_manut>-maintplant.
      <fs_excel_ordem_manut>-kostl       = <fs_ordens_manut>-kostl.
      <fs_excel_ordem_manut>-rm_num      = <fs_ordens_manut>-rm_num.

      READ TABLE t_header ASSIGNING FIELD-SYMBOL(<fs_header>) WITH KEY orderid = <fs_ordens_manut>-orderid BINARY SEARCH.
      IF <fs_header> IS ASSIGNED.
        <fs_excel_ordem_manut>-notif_no        = <fs_header>-notif_no.
        <fs_excel_ordem_manut>-priok           = <fs_header>-priority.
        <fs_excel_ordem_manut>-ltxa1           = <fs_header>-short_text.
        <fs_excel_ordem_manut>-comp_code       = <fs_header>-comp_code.
        <fs_excel_ordem_manut>-gsber           = <fs_header>-bus_area.
        <fs_excel_ordem_manut>-profit_ctr      = <fs_header>-profit_ctr.
        <fs_excel_ordem_manut>-maintplant      = <fs_header>-maintplant.
        <fs_excel_ordem_manut>-location        = <fs_header>-location.
        <fs_excel_ordem_manut>-kostl           = <fs_header>-costcenter.
        <fs_excel_ordem_manut>-task_list_type  = <fs_header>-task_list_type.
        <fs_excel_ordem_manut>-task_list_group = <fs_header>-task_list_group.
        <fs_excel_ordem_manut>-group_counter   = <fs_header>-group_counter.

        READ TABLE t_ordens_inf_add ASSIGNING FIELD-SYMBOL(<fs_ordens_inf_add>) WITH KEY notif_no = <fs_header>-notif_no
                                                                                BINARY SEARCH.
        IF <fs_ordens_inf_add> IS ASSIGNED.
          <fs_excel_ordem_manut>-calc_key             = <fs_ordens_inf_add>-calc_key.
          <fs_excel_ordem_manut>-work_cntr            = <fs_ordens_inf_add>-work_cntr.
          <fs_excel_ordem_manut>-plant                = <fs_ordens_inf_add>-plant.
          <fs_excel_ordem_manut>-control_key          = <fs_ordens_inf_add>-control_key.
          <fs_excel_ordem_manut>-acttype              = <fs_ordens_inf_add>-acttype.
          <fs_excel_ordem_manut>-work_activity        = <fs_ordens_inf_add>-work_activity.
          <fs_excel_ordem_manut>-un_work              = <fs_ordens_inf_add>-un_work.
          <fs_excel_ordem_manut>-number_of_capacities = <fs_ordens_inf_add>-number_of_capacities.
          <fs_excel_ordem_manut>-duration_normal      = <fs_ordens_inf_add>-duration_normal.
          <fs_excel_ordem_manut>-duration_normal_unit = <fs_ordens_inf_add>-duration_normal_unit.
          <fs_excel_ordem_manut>-activity             = <fs_ordens_inf_add>-activity.

        ENDIF.
        UNASSIGN <fs_ordens_inf_add>.

        READ TABLE t_aufk ASSIGNING FIELD-SYMBOL(<fs_aufk>) WITH KEY aufnr = <fs_ordens_manut>-orderid BINARY SEARCH.
        IF <fs_aufk> IS ASSIGNED.
          <fs_excel_ordem_manut>-kostv = <fs_aufk>-kostv.
          <fs_excel_ordem_manut>-scope = <fs_aufk>-scope.
          <fs_excel_ordem_manut>-abkrs = <fs_aufk>-abkrs.

          READ TABLE t_nomes ASSIGNING FIELD-SYMBOL(<fs_nomes>) WITH KEY zaufnr = <fs_aufk>-aufnr BINARY SEARCH.
          IF <fs_nomes> IS ASSIGNED.
            <fs_excel_ordem_manut>-zsolic      = <fs_nomes>-znome.
            <fs_excel_ordem_manut>-zexec_taesa = <fs_nomes>-znome.
          ENDIF.
          UNASSIGN <fs_nomes>.

        ENDIF.
        UNASSIGN <fs_aufk>.

      ENDIF.
      UNASSIGN <fs_header>.

    ENDLOOP.
    UNASSIGN: <fs_ordens_manut>, <fs_excel_ordem_manut>.

    IF NOT e_itab_ordem_manut IS INITIAL.
      e_itab_return = check_dados_ordens_manut( e_itab_ordem_manut ).
    ENDIF.
  ENDMETHOD.


  METHOD get_inform_add.
    DATA: wa_header_aux    TYPE bapi_alm_order_header_e,
          t_ordens_inf_aux TYPE STANDARD TABLE OF bapi_alm_order_operation_e,
          t_return         TYPE STANDARD TABLE OF bapiret2.

    LOOP AT i_tab_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>).
      TRY.
          CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
            EXPORTING
              number        = <fs_ordens>-orderid
            IMPORTING
              es_header     = wa_header_aux
            TABLES
              et_operations = t_ordens_inf_aux
              return        = t_return.

          IF NOT wa_header_aux IS INITIAL.
            APPEND INITIAL LINE TO t_header ASSIGNING FIELD-SYMBOL(<fs_header>).
            MOVE-CORRESPONDING wa_header_aux TO <fs_header>.
          ENDIF.
          UNASSIGN <fs_header>. CLEAR wa_header_aux.

          IF NOT t_ordens_inf_aux IS INITIAL.
            LOOP AT t_ordens_inf_aux ASSIGNING FIELD-SYMBOL(<fs_ordens_aux>).
              APPEND INITIAL LINE TO t_ordens_inf_add ASSIGNING FIELD-SYMBOL(<fs_ordens_add>).
              MOVE-CORRESPONDING <fs_ordens_aux> TO <fs_ordens_add>.
            ENDLOOP.
            UNASSIGN <fs_ordens_aux>.
          ENDIF.
          FREE: t_return, t_ordens_inf_aux.

          IF ( NOT t_ordens_inf_add IS INITIAL ) OR ( NOT t_header IS INITIAL ).
            SORT: t_ordens_inf_add BY activity, t_header BY orderid.
          ENDIF.

        CATCH cx_root.
          EXIT.
      ENDTRY.
    ENDLOOP.

    IF not i_tab_ordens is initial.
      SELECT aufnr, kostv, abkrs, scope
      INTO TABLE @t_aufk
      FROM aufk
      FOR ALL ENTRIES IN @i_tab_ordens
      WHERE aufnr EQ @i_tab_ordens-orderid.
    ENDIF.

    IF NOT t_aufk IS INITIAL.
      SELECT zaufnr, znome
      INTO TABLE @t_nomes
      FROM zpmmega_nomes
      FOR ALL ENTRIES IN @t_aufk
      WHERE zaufnr EQ @t_aufk-aufnr.
    ENDIF.

    IF ( NOT t_aufk IS INITIAL ) OR ( NOT t_nomes IS INITIAL ).
      SORT: t_aufk BY aufnr, t_nomes BY zaufnr.
    ENDIF.

  ENDMETHOD.


  METHOD get_ordens_manutencao.
    TRY.
        CALL FUNCTION 'ZFPM_OBTER_ORDEM_MANUT'
          EXPORTING
            i_range_data = i_rang_data
            i_maxreg     = lv_maxreg
          TABLES
            t_result_out = t_ordens_manut
            i_ampliacao  = t_ampliacao.

        IF NOT t_ordens_manut IS INITIAL.
          SORT t_ordens_manut BY orderid.
        ENDIF.

      CATCH cx_root.
        CLEAR lv_maxreg. EXIT.
    ENDTRY.

  ENDMETHOD.


  METHOD get_tipo_ativ_manuten.
    IF NOT t_ordens_manut IS INITIAL.
      SELECT aufnr, ilart
      INTO TABLE @t_afih
      FROM afih
      FOR ALL ENTRIES IN @t_ordens_manut
      WHERE aufnr EQ @t_ordens_manut-orderid.

      IF NOT t_afih IS INITIAL.
        SORT t_afih BY aufnr.
      ENDIF.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
