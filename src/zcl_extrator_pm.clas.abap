CLASS zcl_extrator_pm DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      "Tipos
      BEGIN OF ty_equi,
        equnr            TYPE equnr,
        erdat            TYPE erdat,
        begru            TYPE iautg,
        eqtyp            TYPE eqtyp,
        eqart            TYPE eqart,
        invnr            TYPE invnr,
        groes            TYPE gross,
        brgew            TYPE obj_weight,
        gewei            TYPE weight_unit,
        ansdt            TYPE andti,
        answt            TYPE answt,
        waers            TYPE waers,
        herst            TYPE herst,
        herld            TYPE herld,
        serge            TYPE serge,
        typbz            TYPE typbz,
        baujj            TYPE baujj,
        baumm            TYPE baumm,
        inbdt            TYPE ilom_datab,
        matnr            TYPE matnr,
        sernr            TYPE gernr,
        zzlatitude_equi  TYPE char30,
        zzlongitude_equi TYPE char30,
      END OF ty_equi .
    TYPES:
      BEGIN OF ty_eqtx,
        equnr TYPE equnr,
        spras TYPE spras,
        eqktx TYPE ktx01,
      END OF ty_eqtx .
    TYPES:
      BEGIN OF ty_equz,
        equnr TYPE equnr,
        datbi TYPE datbi,
        equzn TYPE eqnnr,
        datab TYPE datab,
        iwerk TYPE iwerk,
        mapar TYPE mapar,
        hequi TYPE hequi,
        heqnr TYPE heqnr,
        ingrp TYPE ingrp,
        gewrk TYPE lgwid,
        iloan TYPE iloan,
        rbnr  TYPE rbnr,
      END OF ty_equz .
    TYPES:
      BEGIN OF ty_crhd,
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
      END OF ty_crhd .
    TYPES:
      BEGIN OF ty_iloa,
        iloan TYPE iloan,
        tplnr TYPE tplnr,
        abckz TYPE abckz,
        eqfnr TYPE eqfnr,
        swerk TYPE swerk,
        stort TYPE pmloc,
        msgrp TYPE raumnr,
        beber TYPE beber,
        gsber TYPE gsber,
        kostl TYPE kostl,
        proid TYPE ps_psp_pnr,
        bukrs TYPE bukrs,
        anlnr TYPE anln1,
        anlun TYPE anln2,
        daufn TYPE daufn,
        aufnr TYPE ilom_ordst,
      END OF ty_iloa .
    TYPES:
      BEGIN OF ty_plko,
        plnty      TYPE plnty,
        plnnr      TYPE plnnr,
        profidnetz TYPE profid_std,
        plnal      TYPE plnal,
        zaehl      TYPE cim_count,
        datuv      TYPE datuv,
        loekz      TYPE lkenz,
        verwe      TYPE pln_verwe,
        werks      TYPE werks_d,
        statu      TYPE plnst,
        vagrp      TYPE vagrp,
        ktext      TYPE plantext,
        strat      TYPE strat,
        istru      TYPE istru,
        anlzu      TYPE anlzu,
        extnum     TYPE qextnum,
      END OF ty_plko .
    TYPES:
      BEGIN OF ty_plpo,
        plnty TYPE plnty,
        plnnr TYPE plnnr,
        plnkn TYPE plnkn,
        zaehl TYPE cim_count,
        vornr TYPE vornr,
        steus TYPE steus,
        arbid TYPE objektid,
        werks TYPE werks_d,
        ltxa1 TYPE ltxa1,
        vplal TYPE vplnal,
        plifz TYPE plifz,
        preis TYPE iprei,
        peinh TYPE epein,
        waers TYPE waers,
        dauno TYPE daunor,
        daune TYPE daunore,
        arbei TYPE arbeit,
        arbeh TYPE arbeite,
        anzzl TYPE anzkap,
        prznt TYPE aprozent,
        slwid TYPE slwid,
        usr00 TYPE usrchar20,
        indet TYPE indet,
        larnt TYPE lstar,
        bmeih TYPE bmeih,
        bmvrg TYPE bmvrg,
        aufkt TYPE afakt,
        equnr TYPE equnr,
      END OF ty_plpo .

    TYPES: BEGIN OF ty_plas,
             plnty TYPE plnty,
             plnnr TYPE plnnr,
             plnal TYPE plnal,
             plnkn TYPE plnkn,
             zaehl TYPE cim_count,
             loekz TYPE lkenz,
           END OF ty_plas.

    TYPES:
      BEGIN OF ty_iflot,
        tplnr TYPE tplnr,
        tplkz TYPE tplkz,
        fltyp TYPE fltyp,
        tplma TYPE tplma,
        iwerk TYPE iwerk,
        ingrp TYPE ingrp,
        lgwid TYPE lgwid,
        eqart TYPE eqart,
      END OF ty_iflot .
    TYPES:
      BEGIN OF ty_iflotx,
        tplnr TYPE tplnr,
        spras TYPE spras,
        pltxt TYPE pltxt,
      END OF ty_iflotx .
    TYPES:
      BEGIN OF ty_dados_medicoes,
        point TYPE  imrc_point,
        mdocm TYPE  imrc_mdocm,
        equnr TYPE  equnr,
        mptyp TYPE  imrc_mptyp,
        indct TYPE  imrc_indct,
        pttxt TYPE  imrc_pttxt,
        atnam TYPE  atnam,
        desir TYPE  imrc_desir,
      END OF ty_dados_medicoes .
    TYPES:
      BEGIN OF ty_doc_medicoes,
        mdocm TYPE imrc_mdocm,
        point TYPE imrc_point,
      END OF ty_doc_medicoes .
    TYPES:
      BEGIN OF ty_centro_custo,
        objty TYPE cr_objty,
        objid TYPE cr_objid,
        endda TYPE enddatum,
        lanum TYPE cr_lanum,
        kostl TYPE kostl,
        lstar TYPE lstar,
        forml TYPE ap_form_c1,
      END OF ty_centro_custo .
    TYPES:
      BEGIN OF ty_crtx,
        objty TYPE cr_objty,
        objid TYPE cr_objid,
        spras TYPE spras,
        ktext TYPE cr_ktext,
      END OF ty_crtx .
    TYPES:
      BEGIN OF ty_crca,
        objty TYPE cr_objty,
        objid TYPE cr_objid,
        kapid TYPE kapid,
        fork1 TYPE ap_form_k1,
      END OF ty_crca .
    TYPES:
      BEGIN OF ty_kako,
        kapid  TYPE kapid,
        aznor  TYPE kapanzahl,
        begzt  TYPE kapbegzt,
        endzt  TYPE kapendzt,
        kalid  TYPE cr_wfcid,
        kapar  TYPE kapart,
        meins  TYPE kapbasis,
        ngrad  TYPE nutzgrad,
        pause  TYPE kappause,
        planr  TYPE kapplaner,
        kapter TYPE kap_kzter,
        kapavo TYPE kap_kzavo,
        kapeh  TYPE cr_kapeh,
      END OF ty_kako .
    TYPES:
      BEGIN OF ty_tc26t,
        spras TYPE spras,
        kapar TYPE kapart,
        txt   TYPE kaparttext,
      END OF ty_tc26t .
    TYPES:
      "Categoria de tabelas
      ty_equi_excel_tab       TYPE STANDARD TABLE OF zeagir_equipamentos WITH DEFAULT KEY .
    TYPES:
      ty_list_taref_excel_tab TYPE STANDARD TABLE OF zeagir_tasks_list WITH DEFAULT KEY .
    TYPES:
      ty_local_instalacao_tab TYPE STANDARD TABLE OF zeagir_local_inst WITH DEFAULT KEY .
    TYPES:
      ty_pontos_medicoes_tab  TYPE STANDARD TABLE OF zeagir_pont_med WITH DEFAULT KEY .
    TYPES:
      ty_centro_trab_tab      TYPE STANDARD TABLE OF zeagir_cent_trab WITH DEFAULT KEY .
    TYPES:
      ty_plano_manut_tab      TYPE STANDARD TABLE OF zeagir_plan_manut WITH DEFAULT KEY .
    TYPES:
      ty_range_medicao        TYPE RANGE OF imptt-point .
    TYPES:
      ty_range_centro         TYPE RANGE OF crhd-werks .

    TYPES: ty_range_loc_int    TYPE RANGE OF iflot-tplnr.
    TYPES: ty_range_list_taref TYPE RANGE OF plko-plnnr.
    TYPES:
      ty_return_tab           TYPE STANDARD TABLE OF bapiret2 WITH DEFAULT KEY .

    CONSTANTS:
      "Constantes
      BEGIN OF c_tab_valores,
        prog_equip      TYPE progname   VALUE 'ZRAG_EXTRATOR_EQUIP',
        prog_tarefa     TYPE progname   VALUE 'ZRAG_EXTRATOR_TASKS_LIST',
        prog_loc_ins    TYPE progname   VALUE 'ZRAG_EXTRATOR_LOCAL_INST',
        prog_cen_tra    TYPE progname   VALUE 'ZRAG_EXTRATOR_CENT_TRAB',
        prog_pon_med    TYPE progname   VALUE 'ZRAG_EXTRATOR_PONT_MEDI',
        prog_pla_man    TYPE progname   VALUE 'ZRAG_EXTRATOR_PLAN_MANU',
        tipo_erro       TYPE bapi_mtype VALUE 'E',
        data_val        TYPE datbi      VALUE '99991231',
        objty           TYPE cr_objty   VALUE 'A',
        status_1        TYPE char3      VALUE 'EXC',
        status_2        TYPE char3      VALUE 'exc',
        status_3        TYPE char3      VALUE 'Exc',
        status_4        TYPE char7      VALUE 'EXCLUIR', ##NO_TEXT
        status_5        TYPE char7      VALUE 'excluir', ##NO_TEXT
        status_6        TYPE char7      VALUE 'Excluir', ##NO_TEXT
        status_7        TYPE char4      VALUE 'INAT', ##NO_TEXT
        status_8        TYPE char7      VALUE 'inat', ##NO_TEXT
        status_9        TYPE char7      VALUE 'Inat', ##NO_TEXT
        status_10       TYPE char7      VALUE 'INATIVO', ##NO_TEXT
        status_11       TYPE char7      VALUE 'inativo', ##NO_TEXT
        status_12       TYPE char7      VALUE 'Inativo', ##NO_TEXT
        status_13       TYPE char12     VALUE 'NÃO UTILIZAR', ##NO_TEXT
        status_14       TYPE char12     VALUE 'não utilizar', ##NO_TEXT
        status_15       TYPE char12     VALUE 'Não utilizar', ##NO_TEXT
        chave_tarefa    TYPE plnnr      VALUE '00000283',
        intervalo       TYPE sign       VALUE 'I',
        option          TYPE option     VALUE 'EQ',
        option_cp       TYPE option     VALUE 'CP',
        todos_regs      TYPE char1      VALUE '*',
        red_standard    TYPE plnty      VALUE '0',
        receita_mest    TYPE plnty      VALUE '2',
        perfil_plan     TYPE plnty      VALUE '3',
        instru_mant     TYPE plnty      VALUE 'A',
        list_taref      TYPE plnty      VALUE 'E',
        rot_stand       TYPE plnty      VALUE 'M',
        roteiro         TYPE plnty      VALUE 'N',
        plan_control    TYPE plnty      VALUE 'Q',
        rotprodrep      TYPE plnty      VALUE 'R',
        rotstandard     TYPE plnty      VALUE 'S',
        rotlocalinst    TYPE plnty      VALUE 'T',
        ktext_l         TYPE char1      VALUE 'L',
        ktext_s         TYPE char1      VALUE 'S',
        lanum           TYPE cr_lanum   VALUE 0007,
        cod_loc_inst    TYPE ilom_strno VALUE 'TA-NVT',
        line_1          TYPE syst_tabix VALUE 1,
        categ_inst_l    TYPE fltyp      VALUE 'L',
        plan_manut_1041 TYPE warpl      VALUE '000000001041',
        tip_prior       TYPE artpr      VALUE 'OP',
      END OF c_tab_valores .
    CONSTANTS:
      BEGIN OF c_tab_transacoes,
        medicoes TYPE tcode VALUE 'IK17',
        pro_med  TYPE rsti_ronam VALUE 'RIIMR020',
      END OF   c_tab_transacoes .

    "Métodos
    METHODS constructor
      IMPORTING
        VALUE(i_progname)      TYPE progname
        VALUE(i_maxreg)        TYPE i
        VALUE(i_data)          TYPE erdat OPTIONAL
        VALUE(i_loc_int)       TYPE ty_range_loc_int OPTIONAL
        VALUE(i_rang_list_taf) TYPE ty_range_list_taref OPTIONAL
        VALUE(i_rang_med)      TYPE ty_range_medicao OPTIONAL
        VALUE(i_rang_ctrab)    TYPE ty_range_centro OPTIONAL .
    METHODS exporta_dados
      EXPORTING
        VALUE(e_itab_equip)      TYPE ty_equi_excel_tab
        VALUE(e_itab_list_taref) TYPE ty_list_taref_excel_tab
        VALUE(e_itab_loc_instal) TYPE ty_local_instalacao_tab
        VALUE(e_itab_medicoes)   TYPE ty_pontos_medicoes_tab
        VALUE(e_itab_cent_trab)  TYPE ty_centro_trab_tab
        VALUE(e_itab_plan_manut) TYPE ty_plano_manut_tab
        VALUE(e_itab_return)     TYPE ty_return_tab .
  PROTECTED SECTION.
  PRIVATE SECTION.

    "Atributos
    DATA lv_programa TYPE progname .
    DATA lv_maxreg TYPE i .
    DATA:
      t_equi        TYPE STANDARD TABLE OF ty_equi .
    DATA:
      t_eqtx        TYPE STANDARD TABLE OF ty_eqtx .
    DATA:
      t_equz        TYPE STANDARD TABLE OF ty_equz .
    DATA:
      t_crhd        TYPE STANDARD TABLE OF ty_crhd .
    DATA:
      t_iloa        TYPE STANDARD TABLE OF ty_iloa .
    DATA:
      t_plko        TYPE STANDARD TABLE OF ty_plko .
    DATA:
      t_plpo        TYPE STANDARD TABLE OF ty_plpo .
    DATA: t_plas    TYPE STANDARD TABLE OF ty_plas.
    DATA:
      t_iflot       TYPE STANDARD TABLE OF ty_iflot .
    DATA:
      t_iflotx      TYPE STANDARD TABLE OF ty_iflotx .
    DATA:
      t_pont_med    TYPE STANDARD TABLE OF ty_dados_medicoes .
    DATA:
      t_doc_med     TYPE STANDARD TABLE OF ty_doc_medicoes .
    DATA:
      t_cent_cust   TYPE STANDARD TABLE OF ty_centro_custo .
    DATA:
      t_crtx        TYPE STANDARD TABLE OF ty_crtx .
    DATA:
      t_crca        TYPE STANDARD TABLE OF ty_crca .
    DATA:
      t_kako        TYPE STANDARD TABLE OF ty_kako .
    DATA:
      t_tc26t       TYPE STANDARD TABLE OF ty_tc26t .
    DATA:
      t_plan_manut  TYPE STANDARD TABLE OF zepm_vimhio .
    DATA:
      r_tip_roteiro TYPE RANGE OF plko-plnty .

    "Métodos
    METHODS get_equipamentos
      IMPORTING
        VALUE(i_data) TYPE erdat OPTIONAL.
    METHODS get_text_equipam .
    METHODS get_int_equipam .
    METHODS get_cab_cent_trab
      IMPORTING
        VALUE(i_itab_cent_trab) TYPE ty_range_centro OPTIONAL .
    METHODS get_local_obj_pm .
    METHODS set_tipo_roteiros .
    METHODS get_cabecalho_plano IMPORTING VALUE(i_tab_list_tarefa) TYPE ty_range_list_taref OPTIONAL.
    METHODS get_plano_operacao .
    METHODS get_selecao_itens.
    METHODS get_local_instalacao IMPORTING VALUE(i_rang_loc_inst) TYPE ty_range_loc_int OPTIONAL
                                           VALUE(i_data)          TYPE erdat.
    METHODS get_text_local_instal .
    METHODS get_centro_custo .
    METHODS get_centro_capacidade .
    METHODS get_text_centro_trab .
    METHODS get_plano_manutencao .
    METHODS get_qtdade_doc_med
      IMPORTING
        VALUE(i_pont_med)    TYPE imrc_point
      RETURNING
        VALUE(r_qtdade_docs) TYPE i .
    METHODS get_pontos_medicoes
      IMPORTING
        VALUE(i_itab_medicoes) TYPE ty_range_medicao .
    METHODS get_num_doc_medicoes
      IMPORTING
        VALUE(i_ponto_med)   TYPE imrc_point
      RETURNING
        VALUE(r_st_medicoes) TYPE imrg .
    METHODS get_inf_add_pont_med
      IMPORTING
        VALUE(i_ponto_med)   TYPE imrc_point
      RETURNING
        VALUE(r_st_add_info) TYPE impt .
    METHODS get_local_instal_view .
    METHODS exporta_equipamentos
      EXPORTING
        VALUE(e_itab_equip)  TYPE ty_equi_excel_tab
        VALUE(e_itab_return) TYPE ty_return_tab .
    METHODS exporta_lista_tarefas
      EXPORTING
        VALUE(e_itab_list_taref) TYPE ty_list_taref_excel_tab
        VALUE(e_itab_return)     TYPE ty_return_tab .
    METHODS exporta_local_install
      EXPORTING
        VALUE(e_itab_loc_instal) TYPE ty_local_instalacao_tab
        VALUE(e_itab_return)     TYPE ty_return_tab .
    METHODS exporta_ponto_medicoes
      EXPORTING
        VALUE(e_itab_medicoes) TYPE ty_pontos_medicoes_tab
        VALUE(e_itab_return)   TYPE ty_return_tab .
    METHODS exporta_centro_trabalho
      EXPORTING
        VALUE(e_itab_cent_trab) TYPE ty_centro_trab_tab
        VALUE(e_itab_return)    TYPE ty_return_tab .
    METHODS exporta_plan_manutencao
      EXPORTING
        VALUE(e_itab_plan_manut) TYPE ty_plano_manut_tab
        VALUE(e_itab_return)     TYPE ty_return_tab .
    METHODS set_param_ponto_med
      CHANGING
        VALUE(c_itab_medicoes) TYPE ty_range_medicao .
    METHODS set_param_centr_trab
      CHANGING
        VALUE(c_itab_cent_trab) TYPE ty_range_centro .
    METHODS check_param_pont_med
      IMPORTING
        VALUE(i_itab_medicoes) TYPE ty_range_medicao
      EXPORTING
        VALUE(e_num_registros) TYPE i
        VALUE(e_ponto_medicao) TYPE imrc_point .
    METHODS check_desc_equipamento
      CHANGING
        VALUE(c_itab_equip) TYPE ty_equi_excel_tab .
    METHODS check_desc_list_tarefa
      CHANGING
        VALUE(c_itab_list_taref) TYPE ty_list_taref_excel_tab .
    METHODS check_desc_loc_install
      CHANGING
        VALUE(c_itab_loc_instal) TYPE ty_local_instalacao_tab .
    METHODS check_desc_plan_manute
      CHANGING
        VALUE(c_itab_plan_manut) TYPE ty_plano_manut_tab .
    METHODS check_dados_equipament
      IMPORTING
        VALUE(i_itab_equip)  TYPE ty_equi_excel_tab
      RETURNING
        VALUE(r_itab_return) TYPE ty_return_tab .
    METHODS check_dados_list_taref
      IMPORTING
        VALUE(i_itab_list_taref) TYPE ty_list_taref_excel_tab
      RETURNING
        VALUE(r_itab_return)     TYPE ty_return_tab .
    METHODS check_dados_local_inst
      IMPORTING
        VALUE(i_itab_loc_instal) TYPE ty_local_instalacao_tab
      RETURNING
        VALUE(r_itab_return)     TYPE ty_return_tab .
    METHODS check_dados_pont_med
      IMPORTING
        VALUE(i_itab_medicoes) TYPE ty_pontos_medicoes_tab
      RETURNING
        VALUE(r_itab_return)   TYPE ty_return_tab .
    METHODS check_dados_cent_trab
      IMPORTING
        VALUE(i_itab_cent_trab) TYPE ty_centro_trab_tab
      RETURNING
        VALUE(r_itab_return)    TYPE ty_return_tab .
    METHODS check_dados_plan_manut
      IMPORTING
        VALUE(i_itab_plan_manut) TYPE ty_plano_manut_tab
      RETURNING
        VALUE(r_itab_return)     TYPE ty_return_tab .
ENDCLASS.



CLASS ZCL_EXTRATOR_PM IMPLEMENTATION.


  METHOD check_dados_cent_trab.
    IF i_itab_cent_trab IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD check_dados_equipament.
    IF i_itab_equip IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD check_dados_list_taref.
    IF i_itab_list_taref IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD check_dados_local_inst.
    IF i_itab_loc_instal IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD check_dados_plan_manut.
    IF i_itab_plan_manut IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD check_dados_pont_med.
    IF i_itab_medicoes IS INITIAL.
      APPEND INITIAL LINE TO r_itab_return ASSIGNING FIELD-SYMBOL(<fs_return>).
      <fs_return>-type   = c_tab_valores-tipo_erro.
      <fs_return>-id     = sy-msgid.
      <fs_return>-number = sy-msgno.
      MESSAGE i004(zag_mig) DISPLAY LIKE c_tab_valores-tipo_erro.
    ENDIF.
    UNASSIGN <fs_return>.
  ENDMETHOD.


  METHOD check_desc_equipamento.
    LOOP AT c_itab_equip ASSIGNING FIELD-SYMBOL(<fs_equip>).
      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_1 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_2 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_3 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_4 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_5 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_6 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_7 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_8 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_9 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_10 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_11 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_12 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_13 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_14 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_equip>-eqktx FOR c_tab_valores-status_15 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_equip INDEX sy-tabix. CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_desc_list_tarefa.
    LOOP AT c_itab_list_taref ASSIGNING FIELD-SYMBOL(<fs_list_taref>).

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_1 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_2 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_3 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_4 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_5 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_6 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_7 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_8 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_9 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_10 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_11 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_12 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_13 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_14 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_list_taref>-ktext FOR c_tab_valores-status_15 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_list_taref INDEX sy-tabix. CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_desc_loc_install.
    LOOP AT c_itab_loc_instal ASSIGNING FIELD-SYMBOL(<fs_local_install>).
      SEARCH <fs_local_install>-pltxt FOR c_tab_valores-status_10 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_loc_instal INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_local_install>-pltxt FOR c_tab_valores-status_11 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_loc_instal INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_local_install>-pltxt FOR c_tab_valores-status_12 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_loc_instal INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_local_install>-pltxt FOR c_tab_valores-status_13 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_loc_instal INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_local_install>-pltxt FOR c_tab_valores-status_14 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_loc_instal INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_local_install>-pltxt FOR c_tab_valores-status_15 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_loc_instal INDEX sy-tabix. CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_desc_plan_manute.
    LOOP AT c_itab_plan_manut ASSIGNING FIELD-SYMBOL(<fs_plan_manut>).
      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_1 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_2 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_3 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_4 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_5 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_6 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_7 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_8 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_9 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_10 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_11 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_12 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_13 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_14 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.

      SEARCH <fs_plan_manut>-wptxt FOR c_tab_valores-status_15 AND MARK.
      IF sy-subrc EQ 0.
        DELETE c_itab_plan_manut INDEX sy-tabix. CONTINUE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_param_pont_med.
    DESCRIBE TABLE i_itab_medicoes LINES e_num_registros.

    IF e_num_registros EQ c_tab_valores-line_1.

      READ TABLE i_itab_medicoes ASSIGNING FIELD-SYMBOL(<fs_medicoes>) INDEX e_num_registros.
      IF <fs_medicoes>-high IS INITIAL.
        CLEAR e_num_registros.
        e_num_registros = get_qtdade_doc_med( <fs_medicoes>-low ).
        e_ponto_medicao = <fs_medicoes>-low.
      ELSE.
        CLEAR e_num_registros.
      ENDIF.

    ENDIF.
    UNASSIGN <fs_medicoes>.

  ENDMETHOD.


  METHOD constructor.
    lv_programa = i_progname. lv_maxreg = i_maxreg.

    CASE lv_programa.
      WHEN c_tab_valores-prog_equip.

        " Buscando os dados mestres - Equipamentos
        get_equipamentos( EXPORTING i_data = i_data ).
        get_text_equipam( ).
        get_int_equipam( ).
        get_cab_cent_trab( ).
        get_local_obj_pm( ).

      WHEN c_tab_valores-prog_tarefa.

        "Buscando as informações para lista de tarefas
        get_cabecalho_plano( i_rang_list_taf ).
        get_selecao_itens( ).
        get_plano_operacao( ).
        get_equipamentos( ).
        get_int_equipam( ).
        get_cab_cent_trab( ).
        get_local_obj_pm( ).

      WHEN c_tab_valores-prog_loc_ins.

        "Buscando as informações do local de instalação
        get_local_instalacao( EXPORTING i_rang_loc_inst = i_loc_int i_data = i_data ).
        get_local_obj_pm( ).
        get_cab_cent_trab( ).
        get_text_local_instal( ).

      WHEN c_tab_valores-prog_cen_tra.

        " Buscando as informações do centro de trabalho
        set_param_centr_trab( CHANGING c_itab_cent_trab = i_rang_ctrab ).
        get_cab_cent_trab( i_rang_ctrab ).
        get_text_centro_trab( ).
        get_centro_capacidade( ).
        get_centro_custo( ).

      WHEN c_tab_valores-prog_pon_med.

        "Buscando informações do Ponto de Medição
        get_pontos_medicoes( i_rang_med ).

      WHEN c_tab_valores-prog_pla_man.

        "Buscando as informações do Plano de Manutenção
        get_plano_manutencao( ).
        get_cab_cent_trab( ).

      WHEN OTHERS.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD exporta_centro_trabalho.
    LOOP AT t_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>).
      APPEND INITIAL LINE TO e_itab_cent_trab ASSIGNING FIELD-SYMBOL(<fs_excel_cent_trab>).
      <fs_excel_cent_trab>-arbpl = <fs_crhd>-arbpl.
      <fs_excel_cent_trab>-werks = <fs_crhd>-werks.
      <fs_excel_cent_trab>-veran = <fs_crhd>-veran.
      <fs_excel_cent_trab>-planv = <fs_crhd>-planv.
      <fs_excel_cent_trab>-verwe = <fs_crhd>-verwe.
      <fs_excel_cent_trab>-vgwts = <fs_crhd>-vgwts.
      <fs_excel_cent_trab>-steus = <fs_crhd>-steus.
      <fs_excel_cent_trab>-fort1 = <fs_crhd>-fort1.
      <fs_excel_cent_trab>-begda = <fs_crhd>-begda.
      <fs_excel_cent_trab>-endda = <fs_crhd>-endda.
      <fs_excel_cent_trab>-stand = <fs_crhd>-stand.

      " Recuperando a Denominação
      READ TABLE t_crtx ASSIGNING FIELD-SYMBOL(<fs_crtx>) WITH KEY objid = <fs_crhd>-objid BINARY SEARCH.
      IF <fs_crtx> IS ASSIGNED.
        <fs_excel_cent_trab>-ktext_cb = <fs_crtx>-ktext.
      ENDIF.

      " Recuperando as informações de capacidade
      READ TABLE t_crca ASSIGNING FIELD-SYMBOL(<fs_crca>) WITH KEY objid = <fs_crhd>-objid BINARY SEARCH.
      IF <fs_crca> IS ASSIGNED.
        <fs_excel_cent_trab>-fork1 = <fs_crca>-fork1.

        READ TABLE t_kako ASSIGNING FIELD-SYMBOL(<fs_kako>) WITH KEY kapid = <fs_crca>-kapid BINARY SEARCH.
        IF <fs_kako> IS ASSIGNED.
          <fs_excel_cent_trab>-aznor  = <fs_kako>-aznor.
          <fs_excel_cent_trab>-begzt  = <fs_kako>-begzt.
          <fs_excel_cent_trab>-endzt  = <fs_kako>-endzt.
          <fs_excel_cent_trab>-kalid  = <fs_kako>-kalid.
          <fs_excel_cent_trab>-kapar  = <fs_kako>-kapar.
          <fs_excel_cent_trab>-meins  = <fs_kako>-meins.
          <fs_excel_cent_trab>-ngrad  = <fs_kako>-ngrad.
          <fs_excel_cent_trab>-pause  = <fs_kako>-pause.
          <fs_excel_cent_trab>-planr  = <fs_kako>-planr.
          <fs_excel_cent_trab>-kapter = <fs_kako>-kapter.
          <fs_excel_cent_trab>-kapavo = <fs_kako>-kapavo.
          <fs_excel_cent_trab>-kapeh  = <fs_kako>-kapeh.

          READ TABLE t_tc26t ASSIGNING FIELD-SYMBOL(<fs_tc26t>) WITH KEY kapar = <fs_kako>-kapar BINARY SEARCH.
          IF <fs_tc26t> IS ASSIGNED.
            <fs_excel_cent_trab>-txt = <fs_tc26t>-txt.
          ENDIF.
        ENDIF.
      ENDIF.

      " Recuperando o Centro de custos
      READ TABLE t_cent_cust ASSIGNING FIELD-SYMBOL(<fs_cent_custo>) WITH KEY objid = <fs_crhd>-objid BINARY SEARCH.
      IF <fs_cent_custo> IS ASSIGNED.
        <fs_excel_cent_trab>-kostl = <fs_cent_custo>-kostl.
        <fs_excel_cent_trab>-lstar = <fs_cent_custo>-lstar.
        <fs_excel_cent_trab>-forml = <fs_cent_custo>-forml.
      ENDIF.

    ENDLOOP.
    UNASSIGN: <fs_crhd>, <fs_cent_custo>, <fs_crtx>, <fs_crca>, <fs_kako>, <fs_tc26t>, <fs_excel_cent_trab>.
    FREE: t_crhd, t_crtx, t_crca, t_kako, t_tc26t, t_cent_cust.

    IF NOT e_itab_cent_trab IS INITIAL.
      e_itab_return = check_dados_cent_trab( e_itab_cent_trab ).
    ENDIF.
  ENDMETHOD.


  METHOD exporta_dados.
    CASE lv_programa.
      WHEN c_tab_valores-prog_equip.

        " Extraindo as informações de equipamentos
        exporta_equipamentos( IMPORTING e_itab_equip = e_itab_equip e_itab_return = e_itab_return ). RETURN.

      WHEN c_tab_valores-prog_tarefa.

        " Extraindo as informações da lista de tarefas
        exporta_lista_tarefas( IMPORTING e_itab_list_taref = e_itab_list_taref  e_itab_return = e_itab_return ). RETURN.

      WHEN c_tab_valores-prog_loc_ins.

        " Extraindo as informações de locais de instalações
        exporta_local_install( IMPORTING e_itab_loc_instal = e_itab_loc_instal e_itab_return = e_itab_return ). RETURN.

      WHEN c_tab_valores-prog_cen_tra.

        " Extraindo as informações do Centro de Trabalho
        exporta_centro_trabalho( IMPORTING e_itab_cent_trab = e_itab_cent_trab e_itab_return = e_itab_return ). RETURN.

      WHEN c_tab_valores-prog_pon_med.
        " Extraindo as informações do Ponto de Medição
        exporta_ponto_medicoes( IMPORTING e_itab_medicoes = e_itab_medicoes e_itab_return = e_itab_return ). RETURN.

      WHEN c_tab_valores-prog_pla_man.
        " Extraindo as informações do Plano de Manutenção
        exporta_plan_manutencao( IMPORTING e_itab_plan_manut = e_itab_plan_manut e_itab_return = e_itab_return ). RETURN.

      WHEN OTHERS.
        RETURN.
    ENDCASE.
    "Limpando todas as tabelas.
    FREE: t_equi, t_equz, t_crhd, t_iloa, t_plko, t_plpo, t_plas, t_iflot, t_iflotx. CLEAR lv_programa.
  ENDMETHOD.


  METHOD exporta_equipamentos.
    LOOP AT t_equi ASSIGNING FIELD-SYMBOL(<fs_equi>).
      APPEND INITIAL LINE TO e_itab_equip ASSIGNING FIELD-SYMBOL(<fs_equip_excel>).
      <fs_equip_excel>-equnr = <fs_equi>-equnr.
      <fs_equip_excel>-eqtyp = <fs_equi>-eqtyp.

      READ TABLE t_eqtx ASSIGNING FIELD-SYMBOL(<fs_eqtx>) WITH KEY equnr = <fs_equi>-equnr BINARY SEARCH.
      IF <fs_eqtx> IS ASSIGNED.
        <fs_equip_excel>-eqktx = <fs_eqtx>-eqktx.
      ENDIF.

      <fs_equip_excel>-eqart     = <fs_equi>-eqart.
      <fs_equip_excel>-begru     = <fs_equi>-begru.

      <fs_equip_excel>-inbdt     = <fs_equi>-inbdt.
      <fs_equip_excel>-waers     = <fs_equi>-waers.
      <fs_equip_excel>-ansdt     = <fs_equi>-ansdt.
      <fs_equip_excel>-herst     = <fs_equi>-herst.
      <fs_equip_excel>-herld     = <fs_equi>-herld.
      <fs_equip_excel>-typbz     = <fs_equi>-typbz.
      <fs_equip_excel>-baujj     = <fs_equi>-baujj.
      <fs_equip_excel>-baumm     = <fs_equi>-baumm.
      <fs_equip_excel>-serge     = <fs_equi>-serge.

      READ TABLE t_equz ASSIGNING FIELD-SYMBOL(<fs_equz>) WITH KEY equnr = <fs_equi>-equnr BINARY SEARCH.
      IF <fs_equz> IS ASSIGNED.
        <fs_equip_excel>-datab  = <fs_equz>-datab.
        <fs_equip_excel>-iwerk  = <fs_equz>-iwerk.
        <fs_equip_excel>-ingrp  = <fs_equz>-ingrp.
        <fs_equip_excel>-gewrk  = <fs_equz>-gewrk.
        <fs_equip_excel>-rbnr   = <fs_equz>-rbnr.
        <fs_equip_excel>-hequi  = <fs_equz>-hequi.
      ENDIF.

      READ TABLE t_iloa ASSIGNING FIELD-SYMBOL(<fs_iloa>) WITH KEY iloan = <fs_equz>-iloan BINARY SEARCH.
      IF <fs_iloa> IS ASSIGNED.
        <fs_equip_excel>-swerk     = <fs_iloa>-swerk.
        <fs_equip_excel>-stort     = <fs_iloa>-stort.
        <fs_equip_excel>-sortfield = <fs_iloa>-eqfnr.
        <fs_equip_excel>-bukrs     = <fs_iloa>-bukrs.
        <fs_equip_excel>-gsber     = <fs_iloa>-gsber.
        <fs_equip_excel>-kostl     = <fs_iloa>-kostl.
        <fs_equip_excel>-tplnr     = <fs_iloa>-tplnr.
      ENDIF.

      READ TABLE t_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>) WITH KEY objid = <fs_equz>-gewrk BINARY SEARCH.
      IF <fs_crhd> IS ASSIGNED.
        <fs_equip_excel>-gewrk    = <fs_crhd>-arbpl.
        <fs_equip_excel>-wergw    = <fs_crhd>-werks.
      ENDIF.
      UNASSIGN: <fs_eqtx>, <fs_equz>, <fs_iloa>, <fs_crhd>.
    ENDLOOP.
    UNASSIGN: <fs_equi>, <fs_equip_excel>.

    IF NOT e_itab_equip IS INITIAL.
      check_desc_equipamento( CHANGING c_itab_equip = e_itab_equip ).
      e_itab_return = check_dados_equipament( e_itab_equip ).
    ENDIF.
  ENDMETHOD.


  METHOD exporta_lista_tarefas.

    LOOP AT t_plko ASSIGNING FIELD-SYMBOL(<fs_plko>).

      READ TABLE t_plas ASSIGNING FIELD-SYMBOL(<fs_plas>) WITH KEY plnnr = <fs_plko>-plnnr BINARY SEARCH.

      IF <fs_plas> IS ASSIGNED.
        LOOP AT t_plpo ASSIGNING FIELD-SYMBOL(<fs_plpo>) WHERE plnnr EQ <fs_plas>-plnnr.
          APPEND INITIAL LINE TO e_itab_list_taref ASSIGNING FIELD-SYMBOL(<fs_list_taref_excel>).
          <fs_list_taref_excel>-plnty      = <fs_plko>-plnty.
          <fs_list_taref_excel>-plnnr      = <fs_plko>-plnnr.
          <fs_list_taref_excel>-profidnetz = <fs_plko>-profidnetz.
          <fs_list_taref_excel>-plnal      = <fs_plko>-plnal.
          <fs_list_taref_excel>-datuv      = <fs_plko>-datuv.
          <fs_list_taref_excel>-ktext      = <fs_plko>-ktext.
          <fs_list_taref_excel>-werks      = <fs_plko>-werks.
          <fs_list_taref_excel>-verwe      = <fs_plko>-verwe.
          <fs_list_taref_excel>-vagrp      = <fs_plko>-vagrp.
          <fs_list_taref_excel>-anlzu      = <fs_plko>-anlzu.
          <fs_list_taref_excel>-statu      = <fs_plko>-statu.
          <fs_list_taref_excel>-strat      = <fs_plko>-strat.
          <fs_list_taref_excel>-istru      = <fs_plko>-istru.
          <fs_list_taref_excel>-extnum     = <fs_plko>-extnum.

          <fs_list_taref_excel>-vornr      = <fs_plpo>-vornr.
          <fs_list_taref_excel>-werks_op   = <fs_plpo>-werks.
          <fs_list_taref_excel>-steus      = <fs_plpo>-steus.
          <fs_list_taref_excel>-ltxa1      = <fs_plpo>-ltxa1.
          <fs_list_taref_excel>-aufkt      = <fs_plpo>-aufkt.
          <fs_list_taref_excel>-equnr      = <fs_plpo>-equnr.
          <fs_list_taref_excel>-arbei      = <fs_plpo>-arbei.
          <fs_list_taref_excel>-arbeh      = <fs_plpo>-arbeh.
          <fs_list_taref_excel>-anzzl      = <fs_plpo>-anzzl.
          <fs_list_taref_excel>-dauno      = <fs_plpo>-dauno.
          <fs_list_taref_excel>-daune      = <fs_plpo>-daune.
          <fs_list_taref_excel>-indet      = <fs_plpo>-indet.
          <fs_list_taref_excel>-larnt      = <fs_plpo>-larnt.
          <fs_list_taref_excel>-prznt      = <fs_plpo>-prznt.
          <fs_list_taref_excel>-bmvrg      = <fs_plpo>-bmvrg.
          <fs_list_taref_excel>-bmeih      = <fs_plpo>-bmeih.
          <fs_list_taref_excel>-preis      = <fs_plpo>-preis.
          <fs_list_taref_excel>-waers      = <fs_plpo>-waers.
          <fs_list_taref_excel>-peinh      = <fs_plpo>-peinh.
          <fs_list_taref_excel>-slwid      = <fs_plpo>-slwid.
          <fs_list_taref_excel>-usr00      = <fs_plpo>-usr00.

          READ TABLE t_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>) WITH KEY objid = <fs_plpo>-arbid BINARY SEARCH.
          IF <fs_crhd> IS ASSIGNED.
            <fs_list_taref_excel>-arbpl_op = <fs_crhd>-arbpl.
          ENDIF.

          READ TABLE t_equi ASSIGNING FIELD-SYMBOL(<fs_equi>) WITH KEY equnr = <fs_plpo>-equnr BINARY SEARCH.
          IF <fs_equi> IS ASSIGNED.
            READ TABLE t_equz ASSIGNING FIELD-SYMBOL(<fs_equz>) WITH KEY equnr = <fs_equi>-equnr BINARY SEARCH.
            IF <fs_equz> IS ASSIGNED.
              READ TABLE t_iloa ASSIGNING FIELD-SYMBOL(<fs_iloa>) WITH KEY iloan = <fs_equz>-iloan BINARY SEARCH.
              IF <fs_iloa> IS ASSIGNED.
                <fs_list_taref_excel>-stort = <fs_iloa>-stort.
              ENDIF.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ENDIF.
      UNASSIGN <fs_plas>.
    ENDLOOP.
    UNASSIGN: <fs_plko>, <fs_plpo>, <fs_equi>, <fs_equz>, <fs_crhd>, <fs_iloa>, <fs_list_taref_excel>.

    IF NOT e_itab_list_taref IS INITIAL.
      check_desc_list_tarefa( CHANGING c_itab_list_taref = e_itab_list_taref ).
      e_itab_return = check_dados_list_taref( e_itab_list_taref ).
    ENDIF.
  ENDMETHOD.


  METHOD exporta_local_install.
    LOOP AT t_iflot ASSIGNING FIELD-SYMBOL(<fs_iflot>).
      APPEND INITIAL LINE TO e_itab_loc_instal ASSIGNING FIELD-SYMBOL(<fs_loc_inst_excel>).
      <fs_loc_inst_excel>-iwerk = <fs_iflot>-iwerk.
      <fs_loc_inst_excel>-ingrp = <fs_iflot>-ingrp.

      READ TABLE t_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>) WITH KEY objid = <fs_iflot>-lgwid BINARY SEARCH.
      IF <fs_crhd> IS ASSIGNED.
        <fs_loc_inst_excel>-arbpl = <fs_crhd>-arbpl.
        <fs_loc_inst_excel>-werks = <fs_crhd>-werks.
      ENDIF.
      UNASSIGN <fs_crhd>.

      READ TABLE t_iloa ASSIGNING FIELD-SYMBOL(<fs_iloa>) WITH KEY tplnr = <fs_iflot>-tplnr BINARY SEARCH.
      IF <fs_iloa> IS ASSIGNED.
        <fs_loc_inst_excel>-swerk       = <fs_iloa>-swerk.
        <fs_loc_inst_excel>-stort       = <fs_iloa>-stort.
        <fs_loc_inst_excel>-bukrs       = <fs_iloa>-bukrs.
        <fs_loc_inst_excel>-kostl       = <fs_iloa>-kostl.
        <fs_loc_inst_excel>-gsber       = <fs_iloa>-gsber.
      ENDIF.
      UNASSIGN <fs_iloa>.

      <fs_loc_inst_excel>-tplkz = <fs_iflot>-tplkz.
      <fs_loc_inst_excel>-fltyp = <fs_iflot>-fltyp.

      <fs_loc_inst_excel>-eqart = <fs_iflot>-eqart.
      IF <fs_loc_inst_excel>-fltyp EQ c_tab_valores-categ_inst_l.
        CLEAR <fs_loc_inst_excel>-eqart.
      ENDIF.
      <fs_loc_inst_excel>-tplma = <fs_iflot>-tplma.

      READ TABLE t_iflotx ASSIGNING FIELD-SYMBOL(<fs_iflotx>) WITH KEY tplnr = <fs_iflot>-tplnr BINARY SEARCH.
      IF <fs_iflotx> IS ASSIGNED.
        <fs_loc_inst_excel>-tplnr = <fs_iflotx>-tplnr.
        <fs_loc_inst_excel>-pltxt = <fs_iflotx>-pltxt.
      ENDIF.
      UNASSIGN <fs_iflotx>.
    ENDLOOP.
    UNASSIGN: <fs_loc_inst_excel>, <fs_iflot>.

    IF NOT e_itab_loc_instal IS INITIAL.
      check_desc_loc_install( CHANGING c_itab_loc_instal = e_itab_loc_instal ).
      e_itab_return = check_dados_local_inst( e_itab_loc_instal ).
    ENDIF.
  ENDMETHOD.


  METHOD exporta_plan_manutencao.
    LOOP AT t_plan_manut ASSIGNING FIELD-SYMBOL(<fs_plan_manut>).
      APPEND INITIAL LINE TO e_itab_plan_manut ASSIGNING FIELD-SYMBOL(<fs_excel_plan_manut>).
      <fs_excel_plan_manut>-warpl    = <fs_plan_manut>-warpl.
      <fs_excel_plan_manut>-wapos    = <fs_plan_manut>-wapos.
      <fs_excel_plan_manut>-mptyp    = <fs_plan_manut>-mptyp.
      <fs_excel_plan_manut>-wptxt    = <fs_plan_manut>-wptxt.


      <fs_excel_plan_manut>-zykl1      = <fs_plan_manut>-zykl1.
      TRY.
          CALL FUNCTION 'FLTP_CHAR_CONVERSION_FROM_SI'
            EXPORTING
              char_unit       = 'JHR'
              fltp_value_si   = <fs_plan_manut>-zykl1
              indicator_value = abap_on
            IMPORTING
              char_value      = <fs_excel_plan_manut>-zykl1_conv
            EXCEPTIONS
              no_unit_given   = 1
              unit_not_found  = 2
              OTHERS          = 3.
        CATCH cx_root.
          EXIT.
      ENDTRY.

      <fs_excel_plan_manut>-zeieh    = <fs_plan_manut>-zeieh.
      <fs_excel_plan_manut>-pak_text = <fs_plan_manut>-pak_text.
      <fs_excel_plan_manut>-tplnr    = <fs_plan_manut>-tplnr.
      <fs_excel_plan_manut>-equnr    = <fs_plan_manut>-equnr.
      <fs_excel_plan_manut>-iwerk    = <fs_plan_manut>-iwerk.
      <fs_excel_plan_manut>-auart    = <fs_plan_manut>-auart.

      <fs_excel_plan_manut>-gewrk    = <fs_plan_manut>-gewrk.
      READ TABLE t_crhd ASSIGNING FIELD-SYMBOL(<fs_crhd>) WITH KEY objid = <fs_plan_manut>-gewrk BINARY SEARCH.
      IF <fs_crhd> IS ASSIGNED.
        <fs_excel_plan_manut>-arbpl    = <fs_crhd>-arbpl.
        <fs_excel_plan_manut>-werks    = <fs_crhd>-werks.
      ENDIF.

      <fs_excel_plan_manut>-priok    = <fs_plan_manut>-priok.
      SELECT SINGLE priokx INTO @<fs_excel_plan_manut>-priokx
      FROM t356_t WHERE spras EQ @sy-langu AND artpr EQ @c_tab_valores-tip_prior AND priok EQ @<fs_plan_manut>-priok.

      <fs_excel_plan_manut>-wpgrp        = <fs_plan_manut>-wpgrp.
      <fs_excel_plan_manut>-ilart        = <fs_plan_manut>-ilart.
      <fs_excel_plan_manut>-gsber        = <fs_plan_manut>-gsber.
      <fs_excel_plan_manut>-plnty        = <fs_plan_manut>-plnty.
      <fs_excel_plan_manut>-plnnr        = <fs_plan_manut>-plnnr.
      <fs_excel_plan_manut>-plnal        = <fs_plan_manut>-plnal.
      <fs_excel_plan_manut>-swerk        = <fs_plan_manut>-swerk.
      <fs_excel_plan_manut>-stort        = <fs_plan_manut>-stort.
      <fs_excel_plan_manut>-eqfnr        = <fs_plan_manut>-eqfnr.
      <fs_excel_plan_manut>-bukrs        = <fs_plan_manut>-bukrs.
      <fs_excel_plan_manut>-anlnr        = <fs_plan_manut>-anlnr.
      <fs_excel_plan_manut>-anlun        = <fs_plan_manut>-anlun.
      <fs_excel_plan_manut>-kostl        = <fs_plan_manut>-kostl.
      <fs_excel_plan_manut>-kokrs        = <fs_plan_manut>-kokrs.
      <fs_excel_plan_manut>-vspos        = <fs_plan_manut>-vspos.
      <fs_excel_plan_manut>-topos        = <fs_plan_manut>-topos.
      <fs_excel_plan_manut>-vsneg        = <fs_plan_manut>-vsneg.
      <fs_excel_plan_manut>-toneg        = <fs_plan_manut>-toneg.
      <fs_excel_plan_manut>-sfakt        = <fs_plan_manut>-sfakt.
      <fs_excel_plan_manut>-horiz        = <fs_plan_manut>-horiz.
      <fs_excel_plan_manut>-abrho        = <fs_plan_manut>-abrho.
      <fs_excel_plan_manut>-hunit        = <fs_plan_manut>-hunit.
      <fs_excel_plan_manut>-call_confirm = <fs_plan_manut>-call_confirm.
      <fs_excel_plan_manut>-stich        = <fs_plan_manut>-stich.
    ENDLOOP.
    UNASSIGN: <fs_plan_manut>, <fs_crhd>, <fs_excel_plan_manut>. FREE: t_plan_manut,  t_crhd.

    IF NOT e_itab_plan_manut IS INITIAL.
      check_desc_plan_manute( CHANGING c_itab_plan_manut = e_itab_plan_manut ).
      e_itab_return = check_dados_plan_manut( e_itab_plan_manut ).
    ENDIF.
  ENDMETHOD.


  METHOD exporta_ponto_medicoes.
    " Exportando as todas informações para pegar a medição mais recente de cada equipamento
    LOOP AT t_pont_med ASSIGNING FIELD-SYMBOL(<fs_ponto_med>) TO lv_maxreg.

      APPEND INITIAL LINE TO e_itab_medicoes ASSIGNING FIELD-SYMBOL(<fs_medicoes>).
      MOVE-CORRESPONDING <fs_ponto_med> TO <fs_medicoes>.

    ENDLOOP.
    UNASSIGN: <fs_ponto_med>, <fs_medicoes>. FREE: t_pont_med.

    IF NOT e_itab_medicoes IS INITIAL.
      e_itab_return = check_dados_pont_med( e_itab_medicoes ).
    ENDIF.
  ENDMETHOD.


  METHOD get_cabecalho_plano.
    set_tipo_roteiros( ).
    IF i_tab_list_tarefa IS INITIAL.
      SELECT plnty, plnnr, profidnetz, plnal, zaehl, datuv, loekz, verwe, werks, statu, vagrp, ktext, strat, istru, anlzu, extnum
      INTO TABLE @t_plko
      UP TO @lv_maxreg ROWS
      FROM plko
      WHERE plnty IN @r_tip_roteiro AND plnnr GE @c_tab_valores-chave_tarefa.
    ELSE.
      SELECT plnty, plnnr, profidnetz, plnal, zaehl, datuv, loekz, verwe, werks, statu, vagrp, ktext, strat, istru, anlzu, extnum
      INTO TABLE @t_plko
      UP TO @lv_maxreg ROWS
      FROM plko
      WHERE plnty IN @r_tip_roteiro AND plnnr IN @i_tab_list_tarefa.
    ENDIF.

    DELETE t_plko WHERE ( ktext(1) EQ c_tab_valores-ktext_l ) OR ( ktext(1) EQ c_tab_valores-ktext_s ).
    "Eliminando todos os registros marcados para eliminação
    DELETE t_plko WHERE loekz EQ abap_on.
    CLEAR: lv_maxreg. FREE r_tip_roteiro.

    IF NOT t_plko IS INITIAL.
      SORT t_plko BY plnnr.
    ENDIF.
  ENDMETHOD.


  METHOD get_cab_cent_trab.
    IF lv_programa EQ c_tab_valores-prog_equip.

      IF NOT t_equz IS INITIAL.
        SELECT objid, begda, endda, arbpl, werks, verwe, planv, stand, veran, vgwts, steus, fort1
        INTO TABLE @t_crhd
        FROM crhd
        FOR ALL ENTRIES IN @t_equz
        WHERE objty EQ @c_tab_valores-objty AND
              objid EQ @t_equz-gewrk.                   "#EC CI_NOFIRST

        SORT t_crhd BY objid.
      ENDIF.

    ELSEIF lv_programa EQ c_tab_valores-prog_loc_ins.

      IF NOT t_iflot IS INITIAL.
        SELECT objid, begda, endda, arbpl, werks, verwe, planv, stand, veran, vgwts, steus, fort1
        INTO TABLE @t_crhd
        FROM crhd
        FOR ALL ENTRIES IN @t_iflot
        WHERE objty EQ @c_tab_valores-objty AND
              objid EQ @t_iflot-lgwid.
        SORT t_crhd BY objid.                           "#EC CI_NOFIRST
      ENDIF.

    ELSEIF lv_programa EQ c_tab_valores-prog_tarefa.

      IF NOT t_plpo IS INITIAL.
        SELECT objid, begda, endda, arbpl, werks, verwe, planv, stand, veran, vgwts, steus, fort1
        INTO TABLE @t_crhd
        FROM crhd
        FOR ALL ENTRIES IN @t_plpo
        WHERE objid EQ @t_plpo-arbid.                   "#EC CI_NOFIRST
        SORT t_crhd BY objid.
      ENDIF.

    ELSEIF lv_programa EQ c_tab_valores-prog_cen_tra.

      SELECT objid, begda, endda, arbpl, werks, verwe, planv, stand, veran, vgwts, steus, fort1
      INTO TABLE @t_crhd
      UP TO @lv_maxreg ROWS
      FROM crhd
      WHERE objty EQ @c_tab_valores-objty AND
            werks IN @i_itab_cent_trab.                 "#EC CI_NOFIRST

      SORT t_crhd BY werks objid.

    ELSEIF lv_programa EQ c_tab_valores-prog_pla_man.
      IF NOT t_plan_manut IS INITIAL.
        SELECT objid, begda, endda, arbpl, werks, verwe, planv, stand, veran, vgwts, steus, fort1
        INTO TABLE @t_crhd
        FROM crhd
        FOR ALL ENTRIES IN @t_plan_manut
        WHERE objid EQ @t_plan_manut-gewrk.             "#EC CI_NOFIRST
      ENDIF.
      SORT t_crhd BY objid.
    ENDIF.

  ENDMETHOD.


  METHOD get_centro_capacidade.
    IF NOT t_crhd IS INITIAL.
      SELECT objty, objid, kapid, fork1
      INTO TABLE @t_crca
      FROM crca
      FOR ALL ENTRIES IN @t_crhd
      WHERE objid EQ @t_crhd-objid.
    ENDIF.
    SORT t_crca BY objid.

    IF NOT t_crca IS INITIAL.
      SELECT kapid, aznor, begzt, endzt, kalid ,kapar, meins, ngrad, pause, planr, kapter, kapavo, kapeh
      INTO TABLE @t_kako
      FROM kako
      FOR ALL ENTRIES IN @t_crca
      WHERE kapid EQ @t_crca-kapid.
    ENDIF.
    SORT t_kako BY kapid.

    IF NOT t_kako IS INITIAL.
      SELECT spras, kapar, txt
      INTO TABLE @t_tc26t
      FROM tc26t
      FOR ALL ENTRIES IN @t_kako
      WHERE spras EQ @sy-langu AND
            kapar EQ @t_kako-kapar.
    ENDIF.
    SORT t_tc26t BY kapar.

  ENDMETHOD.


  METHOD get_centro_custo.
    IF NOT t_crhd IS INITIAL.
      SELECT objty, objid, endda, lanum, kostl, lstar, forml
      INTO TABLE @t_cent_cust
      FROM crco
      FOR ALL ENTRIES IN @t_crhd
      WHERE objid EQ @t_crhd-objid AND
            endda EQ @t_crhd-endda AND
            lanum EQ @c_tab_valores-lanum.              "#EC CI_NOFIRST
    ENDIF.
    SORT t_cent_cust BY objid.
  ENDMETHOD.


  METHOD get_equipamentos.
    IF NOT i_data IS INITIAL.
      SELECT equnr, erdat, begru, eqtyp, eqart, invnr, groes, brgew, gewei, ansdt, answt, waers, herst, herld, serge,
             typbz, baujj, baumm, inbdt, matnr, sernr, zzlatitude_equi, zzlongitude_equi
      INTO TABLE @t_equi
      UP TO @lv_maxreg ROWS
      FROM equi WHERE erdat GE @i_data.                 "#EC CI_NOFIELD
      SORT t_equi BY equnr. CLEAR lv_maxreg.
    ELSE.
      IF NOT t_plpo IS INITIAL.
        SELECT equnr, erdat, begru, eqtyp, eqart, invnr, groes, brgew, gewei, ansdt, answt, waers, herst, herld, serge,
               typbz, baujj, baumm, inbdt, matnr, sernr, zzlatitude_equi, zzlongitude_equi
        INTO TABLE @t_equi
        FROM equi
        FOR ALL ENTRIES IN @t_plpo
        WHERE equnr EQ @t_plpo-equnr.
        SORT t_equi BY equnr.
      ENDIF.                                            "#EC CI_NOFIELD
    ENDIF.

  ENDMETHOD.


  METHOD get_inf_add_pont_med.
    TRY.
        CALL FUNCTION 'MEASUREM_POINT_READ'
          EXPORTING
            point           = i_ponto_med
          IMPORTING
            impt_wa         = r_st_add_info
          EXCEPTIONS
            imptt_not_found = 01.
      CATCH cx_root.
        EXIT.
    ENDTRY.
  ENDMETHOD.


  METHOD get_int_equipam.
    IF NOT t_equi IS INITIAL.
      SELECT equnr, datbi, equzn, datab, iwerk, mapar, hequi, heqnr, ingrp, gewrk, iloan, rbnr
      INTO TABLE @t_equz
      FROM equz
      FOR ALL ENTRIES IN @t_equi
      WHERE equnr EQ @t_equi-equnr AND
            datbi EQ @c_tab_valores-data_val.
      SORT t_equz BY equnr.
    ENDIF.
  ENDMETHOD.


  METHOD get_local_instalacao.
    SELECT tplnr, tplkz, fltyp, tplma, iwerk, ingrp, lgwid, eqart
    INTO TABLE @t_iflot
    FROM iflot
    WHERE tplnr IN @i_rang_loc_inst AND
          erdat GE @i_data.

    DELETE t_iflot WHERE tplnr(6) EQ c_tab_valores-cod_loc_inst.
    SORT t_iflot BY tplnr lgwid.
  ENDMETHOD.


  METHOD get_local_instal_view.
  ENDMETHOD.


  METHOD get_local_obj_pm.
    IF NOT t_equz IS INITIAL.
      SELECT iloan, tplnr, abckz, eqfnr, swerk, stort, msgrp, beber, gsber, kostl, proid, bukrs,
             anlnr, anlun, daufn, aufnr
      INTO TABLE @t_iloa
      FROM iloa
      FOR ALL ENTRIES IN @t_equz
      WHERE iloan EQ @t_equz-iloan.
      SORT t_iloa BY iloan.

    ELSEIF NOT t_iflot IS INITIAL.
      SELECT iloan, tplnr, abckz, eqfnr, swerk, stort, msgrp, beber, gsber, kostl, proid, bukrs,
             anlnr, anlun, daufn, aufnr
      INTO TABLE @t_iloa
      FROM iloa
      FOR ALL ENTRIES IN @t_iflot
      WHERE tplnr EQ @t_iflot-tplnr.
      SORT t_iloa BY tplnr.
    ENDIF.

  ENDMETHOD.


  METHOD get_num_doc_medicoes.
    TRY.
        CALL FUNCTION 'MEASUREM_DOCUM_READ_LAST'
          EXPORTING
            point           = i_ponto_med
          IMPORTING
            imrg_wa         = r_st_medicoes
          EXCEPTIONS
            imrg_not_found  = 1
            imptt_not_found = 2
            OTHERS          = 3.
      CATCH cx_root.
        EXIT.
    ENDTRY.
  ENDMETHOD.


  METHOD get_plano_manutencao.
    SELECT *
    INTO TABLE @t_plan_manut
    UP TO @lv_maxreg ROWS
    FROM zepm_vimhio
    WHERE warpl GE @c_tab_valores-plan_manut_1041.
  ENDMETHOD.


  METHOD get_plano_operacao.
    IF NOT t_plas IS INITIAL.
      SELECT plnty, plnnr, plnkn, zaehl, vornr, steus, arbid, werks, ltxa1, vplal, plifz, preis, peinh, waers, dauno, daune, arbei, arbeh,
             anzzl, prznt, slwid, usr00, indet, larnt, bmeih, bmvrg, aufkt, equnr
      INTO TABLE @t_plpo
      FROM plpo
      FOR ALL ENTRIES IN @t_plas
      WHERE plnty EQ @t_plas-plnty AND
            plnnr EQ @t_plas-plnnr AND
            zaehl EQ @t_plas-zaehl.                     "#EC CI_NOFIRST

      IF NOT t_plpo IS INITIAL.
        SORT t_plpo BY plnnr.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_pontos_medicoes.
    DATA: lr_data      TYPE REF TO data,
          wa_doc_med   TYPE imrg, wa_dados_med TYPE impt.

    FIELD-SYMBOLS: <lt_data> TYPE ANY TABLE, <fs_data> TYPE any.

    set_param_ponto_med( CHANGING c_itab_medicoes = i_itab_medicoes  ).

    " Verificando a qtdade de medições informadas na tela de entrada
    check_param_pont_med( EXPORTING i_itab_medicoes = i_itab_medicoes
                          IMPORTING e_num_registros = DATA(lv_registros) e_ponto_medicao = DATA(lv_point_aux) ).

    " Checando se veio mais de uma medição para executar o report com os dados do ALV
    IF lv_registros NE c_tab_valores-line_1.
      " Não exibir o ALV
      cl_salv_bs_runtime_info=>set( EXPORTING display  = abap_off metadata = abap_off data = abap_on ).

      " Buscar as informações dos pontos de medições para recuperar as informações em ALV
      SUBMIT riimr020 WITH point IN i_itab_medicoes AND RETURN.

      TRY.
          " Os dados do ALV (IK03) serão carregados aqui
          cl_salv_bs_runtime_info=>get_data_ref( IMPORTING r_data = lr_data ).
          ASSIGN lr_data->* TO <lt_data>.
          cl_salv_bs_runtime_info=>clear_all( ).
        CATCH cx_root.
          EXIT.
      ENDTRY.

      IF <lt_data> IS ASSIGNED.

        LOOP AT <lt_data> ASSIGNING <fs_data>.
          APPEND INITIAL LINE TO t_pont_med ASSIGNING FIELD-SYMBOL(<fs_pont_med>).
          MOVE-CORRESPONDING <fs_data> TO <fs_pont_med>.
          " Recuperando as informações adicionais do ponto de medição
          wa_dados_med = get_inf_add_pont_med( <fs_pont_med>-point ).
          <fs_pont_med>-mptyp = wa_dados_med-mptyp.
          <fs_pont_med>-indct = wa_dados_med-indct.
          <fs_pont_med>-atnam = wa_dados_med-atnam.
          <fs_pont_med>-desir = wa_dados_med-desir.

        ENDLOOP.
        UNASSIGN <fs_data>. FREE <lt_data>.

      ENDIF.

    ELSE. " Se houver uma unica medição retornada na tela de seleção

      APPEND INITIAL LINE TO t_pont_med ASSIGNING <fs_pont_med>.
      <fs_pont_med>-point = lv_point_aux.

      " Recuperando o número do documento de medição
      wa_doc_med = get_num_doc_medicoes( <fs_pont_med>-point ).
      <fs_pont_med>-mdocm = wa_doc_med-mdocm.

      " Recuperando as informações adicionais do ponto de medição
      wa_dados_med = get_inf_add_pont_med( <fs_pont_med>-point ).
      <fs_pont_med>-equnr = wa_dados_med-mpobj+2(18).
      <fs_pont_med>-mptyp = wa_dados_med-mptyp.
      <fs_pont_med>-indct = wa_dados_med-indct.
      <fs_pont_med>-pttxt = wa_dados_med-pttxt.
      <fs_pont_med>-atnam = wa_dados_med-atnam.
      <fs_pont_med>-desir = wa_dados_med-desir.

    ENDIF.
    UNASSIGN <fs_pont_med>. CLEAR: wa_doc_med, wa_dados_med, lv_point_aux, lv_registros.

  ENDMETHOD.


  METHOD get_qtdade_doc_med.
    SELECT mdocm, point
    INTO TABLE @t_doc_med
    FROM imrg
    WHERE point EQ @i_pont_med.

    IF NOT t_doc_med IS INITIAL.
      DESCRIBE TABLE t_doc_med LINES r_qtdade_docs.
    ENDIF.
  ENDMETHOD.


  METHOD get_selecao_itens.
    IF NOT t_plko IS INITIAL.
      SELECT plnty, plnnr, plnal, plnkn, zaehl, loekz
      INTO TABLE @t_plas
      FROM plas
      FOR ALL ENTRIES IN @t_plko
      WHERE plnnr EQ @t_plko-plnnr AND
            plnal EQ @t_plko-plnal AND
            loekz EQ @abap_off.

      IF NOT t_plas IS INITIAL.
        SORT t_plas BY plnnr zaehl.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_text_centro_trab.
    IF NOT t_crhd IS INITIAL.
      SELECT objty, objid, spras, ktext
      INTO TABLE @t_crtx
      FROM crtx
      FOR ALL ENTRIES IN @t_crhd
      WHERE objid EQ @t_crhd-objid AND
            spras EQ @sy-langu.                         "#EC CI_NOFIRST
    ENDIF.
    SORT t_crtx BY objid.
  ENDMETHOD.


  METHOD get_text_equipam.
    IF NOT t_equi IS INITIAL.
      SELECT equnr, spras, eqktx
      INTO TABLE @t_eqtx
      FROM eqkt
      FOR ALL ENTRIES IN @t_equi
      WHERE equnr EQ @t_equi-equnr AND
            spras EQ @sy-langu.
      SORT t_eqtx BY equnr.
    ENDIF.
  ENDMETHOD.


  METHOD get_text_local_instal.
    IF NOT t_iflot IS INITIAL.
      SELECT tplnr, spras, pltxt
      INTO TABLE @t_iflotx
      FROM iflotx
      FOR ALL ENTRIES IN @t_iflot
      WHERE tplnr EQ @t_iflot-tplnr AND
            spras EQ @sy-langu.
      SORT t_iflotx BY tplnr.
    ENDIF.
  ENDMETHOD.


  METHOD set_param_centr_trab.
    " Preenchendo os intervalos do parâmetro de entrada - Caso não haja preenchimento
    IF c_itab_cent_trab IS INITIAL.
      APPEND INITIAL LINE TO c_itab_cent_trab ASSIGNING FIELD-SYMBOL(<fs_param>).
      <fs_param>-sign   = c_tab_valores-intervalo.
      <fs_param>-option = c_tab_valores-option_cp.
      <fs_param>-low    = c_tab_valores-todos_regs.
      UNASSIGN <fs_param>.
    ENDIF.
  ENDMETHOD.


  METHOD set_param_ponto_med.
    " Preenchendo os intervalos do parâmetro de entrada - Caso não haja preenchimento
    IF c_itab_medicoes IS INITIAL.
      APPEND INITIAL LINE TO c_itab_medicoes ASSIGNING FIELD-SYMBOL(<fs_param>).
      <fs_param>-sign   = c_tab_valores-intervalo.
      <fs_param>-option = c_tab_valores-option_cp.
      <fs_param>-low    = c_tab_valores-todos_regs.
    ENDIF.
  ENDMETHOD.


  METHOD set_tipo_roteiros.
    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING FIELD-SYMBOL(<fs_tip_roteiro>).
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-red_standard.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-receita_mest.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-perfil_plan.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-instru_mant.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-list_taref.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-rot_stand.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-roteiro.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-plan_control.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-rotprodrep.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-rotstandard.
    UNASSIGN <fs_tip_roteiro>.

    APPEND INITIAL LINE TO r_tip_roteiro ASSIGNING <fs_tip_roteiro>.
    <fs_tip_roteiro>-sign   = c_tab_valores-intervalo.
    <fs_tip_roteiro>-option = c_tab_valores-option.
    <fs_tip_roteiro>-low    = c_tab_valores-rotlocalinst.
    UNASSIGN <fs_tip_roteiro>.
  ENDMETHOD.
ENDCLASS.
