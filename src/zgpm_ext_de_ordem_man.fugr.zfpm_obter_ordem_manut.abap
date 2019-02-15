*$*$ ---------------------------------------------------------------*$*$
*$*$                          TAESA                                 *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Desenvolvedor  : Daniel Alves Menezes - INTECHPRO              *$*$
*$*$ Funcional      : Fábio Corrêa                                  *$*$
*$*$ Data de criação: 20/08/2018                                    *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Esp.técnica: XXX_XXX_XXXXX                                     *$*$
*$*$ Observação : Função tem como finalidade extrair as ordens de   *$*$
*$*$              manutenção                                        *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Histórico de modificações                                      *$*$
*$*$ Data       | Autor             | Descrição                     *$*$
*$*$ xx/xx/xxxx | xxxxxxxxxxxxxxx   | xxxxxxxxxxxxxxxxxxxxxxxxxxx   *$*$
*$*$ -------------------------------------------------------------- *$*$
FUNCTION zfpm_obter_ordem_manut.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_RANGE_DATA) TYPE  RANGE_T_DATS
*"     VALUE(I_MAXREG) TYPE  I
*"  TABLES
*"      T_RESULT_OUT TYPE  ZTAGIR_ORDENS OPTIONAL
*"      I_AMPLIACAO STRUCTURE  ZES_AMPLIACAO_LIST
*"----------------------------------------------------------------------

*======================================================================*
* DECLARAÇÃO DE TIPOS                                                  *
*======================================================================*
  TYPES: BEGIN OF ty_caufv,
           aufnr  TYPE aufnr,
           auart  TYPE aufart,
           ktext  TYPE auftext,
           vaplz  TYPE gewrk,
           gsber  TYPE gsber,
           stort  TYPE stort,
           objnr  TYPE imzo-objnr,
           rm_num TYPE zrm_num,
           aezeit TYPE aezeit,
           erdat  TYPE erdat,
         END   OF ty_caufv,

         BEGIN OF ty_crhd,
           arbpl TYPE arbpl,
           objid TYPE crhd-objid,
           objty TYPE crhd-objty,
           werks TYPE crhd-werks,
         END OF ty_crhd,

         BEGIN OF ty_crco,
           objty TYPE crco-objty,
           objid TYPE crco-objid,
           kostl TYPE crco-kostl,
         END   OF ty_crco,

         BEGIN OF ty_amplia,
           zaufnr TYPE zpmmega_nomes-zaufnr,
           ztipo  TYPE zpmmega_nomes-ztipo,
           znome  TYPE zpmmega_nomes-znome,
         END OF ty_amplia,

         BEGIN OF ty_jest,
           objnr TYPE j_objnr,
           stat  TYPE j_status,
         END OF ty_jest,

         BEGIN OF ty_jcds,
           objnr TYPE j_objnr,
           stat  TYPE j_status,
           udate TYPE udate,
           utime TYPE utime,
         END OF ty_jcds,

         BEGIN OF ty_jsto,
           objnr TYPE j_objnr,
           stsma TYPE j_stsma,
         END OF ty_jsto,

         BEGIN OF ty_tj30t,
           stsma   TYPE j_stsma,
           estat   TYPE j_estat,
           txt04_u TYPE astxt,
           txt30   TYPE j_txt30,
         END OF ty_tj30t,

         BEGIN OF ty_afih,
           aufnr TYPE afih-aufnr,
           warpl TYPE afih-warpl,
           abnum TYPE afih-abnum,
           anlzu TYPE afih-anlzu,
           addat TYPE afih-addat,
           equnr TYPE afih-equnr,
           ilart TYPE afih-ilart,
           ingpr TYPE afih-ingpr,
           iloan TYPE afih-iloan,
           iwerk TYPE afih-iwerk,
           qmnum TYPE afih-qmnum,
         END OF ty_afih,

         BEGIN OF ty_mhis,
           warpl TYPE mhis-warpl,
           abnum TYPE mhis-abnum,
           nplda TYPE mhis-nplda,
         END OF ty_mhis,

         BEGIN OF ty_equi,
           equnr TYPE equi-equnr,
           eqart TYPE equi-eqart,
           eqfnr TYPE eqfnr,
         END OF ty_equi,

         BEGIN OF ty_eqkt,
           equnr TYPE eqkt-equnr,
           eqktx TYPE eqkt-eqktx,
         END OF ty_eqkt,

         BEGIN OF ty_iloa,
           tplnr TYPE iloa-tplnr,
           iloan TYPE iloa-iloan,
         END OF ty_iloa,

         BEGIN OF ty_iflo,
           tplnr TYPE iflo-tplnr,
           pltxt TYPE iflo-pltxt,
         END OF ty_iflo,

         BEGIN OF ty_viqmel,
           qmnum TYPE viqmel-qmnum,
           ausvn TYPE viqmel-ausvn,
         END OF ty_viqmel,

         BEGIN OF ty_t353i_t,
           ilart TYPE t353i_t-ilart,
           spras TYPE t353i_t-spras,
           ilatx TYPE t353i_t-ilatx,
         END OF ty_t353i_t.

*======================================================================*
* DECLARAÇÃO DE TABELAS INTERNAS                                       *
*======================================================================*
  DATA: ti_caufv   TYPE STANDARD TABLE OF ty_caufv,
        ti_crhd    TYPE STANDARD TABLE OF ty_crhd,
        ti_crco    TYPE STANDARD TABLE OF ty_crco,
        ti_jest    TYPE STANDARD TABLE OF ty_jest,
        ti_jcds    TYPE STANDARD TABLE OF ty_jcds,
        ti_jsto    TYPE STANDARD TABLE OF ty_jsto,
        ti_tj30t   TYPE STANDARD TABLE OF ty_tj30t,
        ti_amplia  TYPE STANDARD TABLE OF ty_amplia,
        ti_t353i_t TYPE STANDARD TABLE OF ty_t353i_t,
        ti_afih    TYPE STANDARD TABLE OF ty_afih,
        ti_iloa    TYPE STANDARD TABLE OF ty_iloa,
        ti_iflo    TYPE STANDARD TABLE OF ty_iflo,
        ti_viqmel  TYPE STANDARD TABLE OF ty_viqmel,
        ti_eqkt    TYPE STANDARD TABLE OF ty_eqkt,
        ti_mhis    TYPE STANDARD TABLE OF ty_mhis,
        ti_equi    TYPE STANDARD TABLE OF ty_equi.

*======================================================================*
* FIELD-SYMBOLS                                                        *
*======================================================================*
  FIELD-SYMBOLS: <fs_caufv>   TYPE ty_caufv,
                 <fs_crhd>    TYPE ty_crhd,
                 <fs_crco>    TYPE ty_crco,
                 <fs_amplia>  TYPE ty_amplia,
                 <fs_jest>    TYPE ty_jest,
                 <fs_jcds>    TYPE ty_jcds,
                 <fs_jsto>    TYPE ty_jsto,
                 <fs_tj30t>   TYPE ty_tj30t,
                 <fs_afih>    TYPE ty_afih,
                 <fs_viqmel>  TYPE ty_viqmel,
                 <fs_iloa>    TYPE ty_iloa,
                 <fs_iflo>    TYPE ty_iflo,
                 <fs_t353i_t> TYPE ty_t353i_t,
                 <fs_mhis>    TYPE ty_mhis,
                 <fs_equi>    TYPE ty_equi,
                 <fs_eqkt>    TYPE ty_eqkt,
                 <fs_olist>   TYPE bapi_alm_order_objectlist.

*======================================================================*
* DECLARAÇÃO DE CONSTANTES                                             *
*======================================================================*
  CONSTANTS:c_type_e TYPE bapi_mtype VALUE 'E',
            c_type_w TYPE bapi_mtype VALUE 'W',
            c_type_s TYPE bapi_mtype VALUE 'S',
            c_msg_id TYPE symsgid    VALUE '00',
            c_msg_nr TYPE symsgno    VALUE '398',
            c_auart  TYPE auart      VALUE 'T'.

*======================================================================*
* VARIÁVEIS                                                            *
*======================================================================*
  DATA: wa_result_out TYPE zpmemega_result_1,
        wa_return_out TYPE zpmemega_retorno,
        wa_ampliacao  TYPE zpmmega_nomes,
        return        TYPE STANDARD TABLE OF bapiret2,
        return_aux    TYPE STANDARD TABLE OF bapiret2,
        et_olist      TYPE STANDARD TABLE OF bapi_alm_order_objectlist,
        es_header     LIKE bapi_alm_order_header_e.

  DATA: e_sysst   TYPE bsvx-sttxt,
        e_anwst   TYPE bsvx-sttxt,
        lv_priokx TYPE t356_t-priokx.

  DATA: lr_oref      TYPE REF TO cx_root,
        l_error_text TYPE string.

  DATA: v_message    TYPE bapi_msg,
        w_return_out TYPE zpmemega_retorno.

*======================================================================*
* LÓGICA DO PROGRAMA                                                   *
*======================================================================*
  TRY.
      SELECT aufnr auart ktext vaplz gsber stort objnr rm_num aezeit erdat
      INTO TABLE ti_caufv
      FROM caufv
      UP TO i_maxreg ROWS
      WHERE autyp EQ '30' AND erdat IN i_range_data.

      IF sy-subrc EQ 0.
        SORT ti_caufv BY aufnr.
        DELETE ti_caufv WHERE auart(1) NE 'T'.

        SELECT arbpl objid objty werks
          INTO TABLE ti_crhd
          FROM crhd
           FOR ALL ENTRIES IN ti_caufv
         WHERE arbpl EQ ti_caufv-vaplz
          AND lvorm NE 'X'.

        IF sy-subrc EQ 0.
          SORT ti_crhd BY objid.
          SELECT a~objty a~objid b~kostl
            INTO TABLE ti_crco
            FROM crco AS a INNER JOIN csks AS b ON a~kostl = b~kostl AND a~kokrs = b~kokrs
             FOR ALL ENTRIES IN ti_crhd
           WHERE a~objty EQ ti_crhd-objty
             AND a~objid EQ ti_crhd-objid
             AND b~datbi EQ '99991231'
             AND a~endda EQ '99991231'.

          IF sy-subrc EQ 0.
            SORT ti_crco BY objid.
          ENDIF.

        ENDIF.

        IF NOT ti_caufv IS INITIAL.
          SELECT objnr
                 stat
            FROM jest
            INTO TABLE ti_jest
             FOR ALL ENTRIES IN ti_caufv
           WHERE objnr EQ ti_caufv-objnr
             AND inact NE abap_true.

          DELETE ti_jest WHERE stat(1) NE 'E'.


          IF NOT ti_jest[] IS INITIAL.
            SORT ti_jest BY objnr.
            SELECT objnr
                   stat
                   udate
                   utime
              FROM jcds
              INTO TABLE ti_jcds
               FOR ALL ENTRIES IN ti_jest
             WHERE objnr EQ ti_jest-objnr
               AND stat  EQ ti_jest-stat
               AND inact NE abap_true.

            DELETE ti_jcds WHERE stat(1) NE 'E'.

            IF NOT ti_jcds[] IS INITIAL.
              SORT ti_jcds BY objnr udate DESCENDING utime DESCENDING.
            ENDIF.

          ENDIF.

          SELECT objnr stsma
            INTO TABLE ti_jsto
            FROM jsto
            FOR ALL ENTRIES IN ti_caufv
           WHERE objnr EQ ti_caufv-objnr.

          IF sy-subrc EQ 0.
            SORT ti_jsto BY objnr.
            SELECT stsma
                   estat
                   txt04
                   txt30
              FROM tj30t
              INTO TABLE ti_tj30t
               FOR ALL ENTRIES IN ti_jsto
             WHERE stsma EQ ti_jsto-stsma
               AND estat NE 'X'
               AND spras EQ sy-langu.

          ENDIF.

          SELECT zaufnr ztipo znome
          INTO TABLE ti_amplia
            FROM zpmmega_nomes
            FOR ALL ENTRIES IN ti_caufv
            WHERE zaufnr EQ ti_caufv-aufnr.

          IF NOT ti_amplia IS INITIAL.
            SORT ti_amplia BY zaufnr.
          ENDIF.

          SELECT aufnr warpl abnum anlzu addat equnr ilart ingpr iloan iwerk qmnum
            INTO TABLE ti_afih
            FROM afih
             FOR ALL ENTRIES IN ti_caufv
           WHERE aufnr EQ ti_caufv-aufnr.

          IF sy-subrc EQ 0.
            SORT ti_afih BY aufnr.
            SELECT qmnum ausvn
              INTO TABLE ti_viqmel
              FROM viqmel
               FOR ALL ENTRIES IN ti_afih
             WHERE qmnum EQ ti_afih-qmnum.

            IF NOT ti_viqmel IS INITIAL.
              SORT ti_viqmel BY qmnum.
            ENDIF.

            SELECT ilart spras ilatx
              INTO TABLE ti_t353i_t
              FROM t353i_t
               FOR ALL ENTRIES IN ti_afih
             WHERE ilart EQ ti_afih-ilart
               AND spras EQ 'PT'.

            IF NOT ti_t353i_t IS INITIAL.
               SORT ti_t353i_t BY ilart.
            ENDIF.

            SELECT tplnr iloan
              INTO TABLE ti_iloa
              FROM iloa
               FOR ALL ENTRIES IN ti_afih
             WHERE iloan EQ ti_afih-iloan.

            IF sy-subrc EQ 0.
              SORT ti_iloa BY iloan.
              SELECT tplnr pltxt
                INTO TABLE ti_iflo
                FROM iflo
                 FOR ALL ENTRIES IN ti_iloa
               WHERE tplnr EQ ti_iloa-tplnr.
              IF NOT ti_iflo IS INITIAL.
                 SORT ti_iflo BY tplnr.
              ENDIF.
            ENDIF.

            SELECT warpl abnum nplda
              INTO TABLE ti_mhis
              FROM mhis
               FOR ALL ENTRIES IN ti_afih
             WHERE warpl EQ ti_afih-warpl
               AND abnum EQ ti_afih-abnum.

            IF NOT ti_mhis IS INITIAL.
               SORT ti_mhis BY warpl.
            ENDIF.

            SELECT equnr eqart eqfnr
              INTO TABLE ti_equi
              FROM v_equi
               FOR ALL ENTRIES IN ti_afih
             WHERE equnr EQ ti_afih-equnr.

            IF sy-subrc EQ 0.
              SORT ti_equi BY equnr.
              SELECT equnr eqktx
                INTO TABLE ti_eqkt
                FROM eqkt
                 FOR ALL ENTRIES IN ti_equi
               WHERE equnr EQ ti_equi-equnr
                 AND spras EQ 'PT'.

              IF NOT ti_eqkt IS INITIAL.
                 SORT ti_eqkt BY equnr.
              ENDIF.

            ENDIF.

          ENDIF.
        ENDIF.
      ENDIF.

      LOOP AT ti_caufv ASSIGNING <fs_caufv>.

        READ TABLE ti_jcds ASSIGNING <fs_jcds> WITH KEY objnr = <fs_caufv>-objnr BINARY SEARCH.

        IF sy-subrc EQ 0.

          READ TABLE ti_jsto ASSIGNING <fs_jsto> WITH KEY objnr = <fs_caufv>-objnr BINARY SEARCH.

          IF sy-subrc EQ 0.

            READ TABLE ti_tj30t ASSIGNING <fs_tj30t> WITH KEY stsma = <fs_jsto>-stsma
                                                              estat = <fs_jcds>-stat BINARY SEARCH.

            IF sy-subrc EQ 0.
              MOVE <fs_tj30t>-txt04_u TO wa_result_out-u_status.
            ENDIF.

          ENDIF.

        ENDIF.

*       Ampliação.
        LOOP AT ti_amplia ASSIGNING <fs_amplia> WHERE zaufnr EQ <fs_caufv>-aufnr.

          wa_ampliacao-zaufnr = <fs_amplia>-zaufnr.
          wa_ampliacao-ztipo  = <fs_amplia>-ztipo.
          wa_ampliacao-znome  = <fs_amplia>-znome.

          APPEND wa_ampliacao TO i_ampliacao.
          CLEAR  wa_ampliacao.

        ENDLOOP.

        READ TABLE ti_crhd ASSIGNING <fs_crhd> WITH KEY arbpl = <fs_caufv>-vaplz BINARY SEARCH.

        IF sy-subrc EQ 0.

          wa_result_out-vawrk = <fs_crhd>-werks.

          READ TABLE ti_crco ASSIGNING <fs_crco> WITH KEY objty = <fs_crhd>-objty
                                                          objid = <fs_crhd>-objid BINARY SEARCH.

          IF sy-subrc EQ 0.
            wa_result_out-kostl  = <fs_crco>-kostl.
          ENDIF.

        ENDIF.

        wa_result_out-orderid    = <fs_caufv>-aufnr .
        wa_result_out-order_type = <fs_caufv>-auart .
        wa_result_out-short_text = <fs_caufv>-ktext .
        wa_result_out-mn_wk_ctr  = <fs_caufv>-vaplz .
        wa_result_out-rm_num     = <fs_caufv>-rm_num.
        wa_result_out-gsber      = <fs_caufv>-gsber.
        wa_result_out-maintloc   = <fs_caufv>-stort.
        wa_result_out-aezeit     = <fs_caufv>-aezeit.
        wa_result_out-object_no  = <fs_caufv>-objnr.
        wa_result_out-erdat      = <fs_caufv>-erdat.

        READ TABLE ti_afih ASSIGNING <fs_afih> WITH KEY aufnr = <fs_caufv>-aufnr BINARY SEARCH.

        IF sy-subrc EQ 0.

          wa_result_out-anlzu     = <fs_afih>-anlzu .
          wa_result_out-addat     = <fs_afih>-addat.
          wa_result_out-plangroup = <fs_afih>-ingpr.
          wa_result_out-planplant = <fs_afih>-iwerk.

          READ TABLE ti_t353i_t ASSIGNING <fs_t353i_t> WITH KEY ilart = <fs_afih>-ilart BINARY SEARCH.

          IF sy-subrc EQ 0.
            wa_result_out-ilatx = <fs_t353i_t>-ilatx.
          ENDIF.

          READ TABLE ti_viqmel ASSIGNING <fs_viqmel> WITH KEY qmnum = <fs_afih>-qmnum BINARY SEARCH.

          IF sy-subrc EQ 0.
            wa_result_out-actual_release_date = <fs_viqmel>-ausvn.
          ENDIF.

          READ TABLE ti_iloa ASSIGNING <fs_iloa> WITH KEY iloan = <fs_afih>-iloan BINARY SEARCH.

          IF sy-subrc EQ 0.

            wa_result_out-funcloc = <fs_iloa>-tplnr.

            READ TABLE ti_iflo ASSIGNING <fs_iflo> WITH KEY tplnr = <fs_iloa>-tplnr BINARY SEARCH.

            IF sy-subrc EQ 0.
              wa_result_out-funcldescr = <fs_iflo>-pltxt.
            ENDIF.

          ENDIF.

          READ TABLE ti_mhis ASSIGNING <fs_mhis> WITH KEY warpl = <fs_afih>-warpl
                                                          abnum = <fs_afih>-abnum BINARY SEARCH.

          IF sy-subrc EQ 0.
            wa_result_out-nplda = <fs_mhis>-nplda.
            wa_result_out-warpl = <fs_mhis>-warpl.
          ENDIF.

          READ TABLE ti_equi ASSIGNING <fs_equi> WITH KEY equnr = <fs_afih>-equnr BINARY SEARCH.

          IF sy-subrc EQ 0.

            wa_result_out-eqart     = <fs_equi>-eqart.
            wa_result_out-sortfield = <fs_equi>-eqfnr.
            wa_result_out-equipment = <fs_equi>-equnr.

            READ TABLE ti_eqkt ASSIGNING <fs_eqkt> WITH KEY equnr = <fs_equi>-equnr BINARY SEARCH.

            IF sy-subrc EQ 0.
              wa_result_out-equidescr = <fs_eqkt>-eqktx.
            ENDIF.

          ENDIF.

          CALL FUNCTION 'AIP9_STATUS_READ'
            EXPORTING
              i_objnr = <fs_caufv>-objnr
              i_spras = sy-langu
            IMPORTING
              e_sysst = e_sysst
              e_anwst = e_anwst.

          IF sy-subrc EQ 0.
            wa_result_out-s_status = e_sysst.
*           wa_result_out-u_status = e_anwst.
          ENDIF.

        ENDIF.

        CLEAR: es_header, et_olist, return.

        CALL FUNCTION 'BAPI_ALM_ORDER_GET_DETAIL'
          EXPORTING
            number    = <fs_caufv>-aufnr
          IMPORTING
            es_header = es_header
          TABLES
            return    = return_aux.

        DATA l_artpr TYPE char2.

        CASE es_header-order_type.
          WHEN 'TCOR'. l_artpr = 'OC'.
          WHEN 'TPRE'. l_artpr = 'OP'.
          WHEN 'TOPR'. l_artpr = 'PP'.
          WHEN OTHERS. l_artpr = 'PM'.
        ENDCASE.

        SELECT SINGLE priokx
          INTO lv_priokx
          FROM t356_t
         WHERE spras = sy-langu
           AND artpr = l_artpr
           AND priok = es_header-priority.

        IF sy-subrc = 0.
          MOVE lv_priokx TO wa_result_out-priokx .
        ENDIF.

        wa_result_out-start_date  = es_header-start_date.
        wa_result_out-finish_date = es_header-finish_date.
        wa_result_out-change_date = es_header-change_date.
        wa_result_out-maintplant  = es_header-maintplant.

        APPEND wa_result_out TO t_result_out.
        CLEAR  wa_result_out .

      ENDLOOP.

      SORT t_result_out BY orderid. SORT i_ampliacao  BY aufnr.

    CATCH cx_root.
      EXIT.
  ENDTRY.

ENDFUNCTION.
