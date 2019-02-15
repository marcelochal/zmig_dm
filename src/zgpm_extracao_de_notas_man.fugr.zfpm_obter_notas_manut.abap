*$*$ ---------------------------------------------------------------*$*$
*$*$                          TAESA                                 *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Desenvolvedor  : Daniel Alves Menezes - INTECHPRO              *$*$
*$*$ Funcional      : Fábio Corrêa                                  *$*$
*$*$ Data de criação: 20/08/2018                                    *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Esp.técnica: XXX_XXX_XXXXX                                     *$*$
*$*$ Observação : Função tem como finalidade extrair as notas de    *$*$
*$*$              manutenção                                        *$*$
*$*$ ---------------------------------------------------------------*$*$
*$*$ Histórico de modificações                                      *$*$
*$*$ Data       | Autor             | Descrição                     *$*$
*$*$ xx/xx/xxxx | xxxxxxxxxxxxxxx   | xxxxxxxxxxxxxxxxxxxxxxxxxxx   *$*$
*$*$ -------------------------------------------------------------- *$*$

FUNCTION zfpm_obter_notas_manut.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_RANGE_DATA) TYPE  RANGE_T_DATS
*"     REFERENCE(I_RANGE_QMART) TYPE  ZRANGE_T_QMART
*"     VALUE(I_MAXREG) TYPE  I
*"  TABLES
*"      T_NOTAS_T2_T3 TYPE  ZTAGIR_NOTAS
*"----------------------------------------------------------------------
*--------------------------------------------------------------------*
* Declaraçao de Estruturas
*--------------------------------------------------------------------*
  TYPES: BEGIN OF ty_equi,
           equnr TYPE equi-equnr,
           eqart TYPE equi-eqart,
         END OF ty_equi,

         BEGIN OF ty_qmfe,
           qmnum TYPE qmfe-qmnum,
           fegrp TYPE qmfe-fegrp,
           fecod TYPE qmfe-fecod,
           otgrp TYPE qmfe-otgrp,
           oteil TYPE qmfe-oteil,
         END OF ty_qmfe,

         BEGIN OF ty_qmur,
           qmnum TYPE qmur-qmnum,
           urgrp TYPE qmur-urgrp,
           urcod TYPE qmur-urcod,
         END OF ty_qmur,

         BEGIN OF ty_qmma,
           qmnum TYPE qmma-qmnum,
           mngrp TYPE qmma-mngrp,
           mncod TYPE qmma-mncod,
         END OF ty_qmma,

         BEGIN OF ty_crtx,
           objty TYPE cr_objty,
           objid TYPE cr_objid,
           ktext TYPE cr_ktext,
         END OF ty_crtx,

         BEGIN OF ty_t499s,
           werks TYPE werks_d,
           stand TYPE stort_t499s,
           ktext TYPE text40,
         END OF ty_t499s.

*--------------------------------------------------------------------*
* Declaraçao de tabelas internas
*--------------------------------------------------------------------*
  DATA: ti_viqmel TYPE STANDARD TABLE OF zepm_viqmel,
        ti_equi   TYPE STANDARD TABLE OF ty_equi,
        ti_crtx   TYPE STANDARD TABLE OF ty_crtx,
        ti_qmfe   TYPE STANDARD TABLE OF ty_qmfe,
        ti_qmur   TYPE STANDARD TABLE OF ty_qmur,
        ti_qmma   TYPE STANDARD TABLE OF ty_qmma,
        ti_t499s  TYPE STANDARD TABLE OF ty_t499s.

*--------------------------------------------------------------------*
* Declaraçao Field-Symbols
*--------------------------------------------------------------------*
  FIELD-SYMBOLS: <fs_viqmel>      TYPE zepm_viqmel,
                 <fs_equi>        TYPE ty_equi,
                 <fs_qmfe>        TYPE ty_qmfe,
                 <fs_qmur>        TYPE ty_qmur,
                 <fs_qmma>        TYPE ty_qmma,
                 <fs_crtx>        TYPE ty_crtx,
                 <fs_t499s>       TYPE ty_t499s,
                 <fs_notas_out>   TYPE zeagir_notas,
                 <fs_retorno_out> TYPE zpmemega_retorno.

*--------------------------------------------------------------------*
* DECLARAÇÃO DE ESTRUTURAS                                             *
*--------------------------------------------------------------------*
  DATA: wa_notlongtxt     TYPE bapi2080_notfulltxte,
        wa_notlongtxt_out TYPE zpmemega_fulltxte,
        w_return_out      TYPE zpmemega_retorno.

*--------------------------------------------------------------------*
* Declaraçao de Constantes
*--------------------------------------------------------------------*
  CONSTANTS: c_e      TYPE c LENGTH 1 VALUE 'E',
             c_s      TYPE c LENGTH 1 VALUE 'S',
             c_w      TYPE bapi_mtype VALUE 'W',
             c_msg_id TYPE symsgid    VALUE '00',
             c_msg_nr TYPE symsgno    VALUE '398',
             c_spras  TYPE spras      VALUE 'P'.

*--------------------------------------------------------------------*
* Declaraçao de Variáveis
*--------------------------------------------------------------------*
  DATA: notlongtxt    TYPE STANDARD TABLE OF bapi2080_notfulltxte,
        lr_oref       TYPE REF TO cx_root,
        lv_error_text TYPE string.

  DATA: e_sysst   TYPE bsvx-sttxt,
        e_sysst2  TYPE bsvx-sttxt,
        e_anwst   TYPE bsvx-sttxt,
        v_astxt   TYPE bsvx-sttxt,
        e_stsma   TYPE jsto-stsma,
        v_message TYPE bapi_msg.

  DATA: v_nunc TYPE auszt,
        v_var  TYPE p DECIMALS 2.

*----------------------------------------------------------------------*
* Inicia busca dos dados
*----------------------------------------------------------------------*
  TRY. " Runtime error capture
*  Seleção a partir da Data de Criação - ERDAT.
      SELECT *
      INTO TABLE @ti_viqmel
      FROM zepm_viqmel
      UP TO @i_maxreg ROWS
      WHERE qmart  IN @i_range_qmart AND
            erdat  IN @i_range_data.

      IF NOT ti_viqmel[] IS INITIAL.
        SORT ti_viqmel BY qmnum.

*       Busca Centro Trabalho ID
        SELECT objty
               objid
               ktext
          FROM crtx
          INTO TABLE ti_crtx
           FOR ALL ENTRIES IN ti_viqmel
         WHERE objty EQ ti_viqmel-crobjty
           AND objid EQ ti_viqmel-arbpl
           AND spras EQ sy-langu.

        IF NOT ti_crtx IS INITIAL.
          SORT ti_crtx BY objid.
        ENDIF.

*       Busca Localizacao ID
        SELECT werks
               stand
               ktext
          FROM t499s
          INTO TABLE ti_t499s
           FOR ALL ENTRIES IN ti_viqmel
         WHERE werks EQ ti_viqmel-swerk
           AND stand EQ ti_viqmel-stort.

        IF NOT ti_t499s IS INITIAL.
          SORT ti_t499s BY werks.
        ENDIF.

*       Busca Tipo de Objeto
        SELECT equnr
               eqart
          FROM equi
          INTO TABLE ti_equi
           FOR ALL ENTRIES IN ti_viqmel
         WHERE equnr EQ ti_viqmel-equnr.

        IF NOT ti_equi IS INITIAL.
          SORT ti_equi BY equnr.
        ENDIF.

        SELECT qmnum
               fegrp
               fecod
               otgrp
               oteil
          FROM qmfe
          INTO TABLE ti_qmfe
           FOR ALL ENTRIES IN ti_viqmel
         WHERE qmnum EQ ti_viqmel-qmnum
           AND kzloesch = space.   "não marcada para eliminação

        IF NOT ti_qmfe IS INITIAL.
          SORT ti_qmfe BY qmnum.
        ENDIF.

        SELECT qmnum
               urgrp
               urcod
          FROM qmur
          INTO TABLE ti_qmur
           FOR ALL ENTRIES IN ti_viqmel
         WHERE qmnum EQ ti_viqmel-qmnum
           AND kzloesch = space.   "não marcada para eliminação

        IF NOT ti_qmur IS INITIAL.
          SORT ti_qmur BY qmnum.
        ENDIF.

        SELECT qmnum
               mngrp
               mncod
          FROM qmma
          INTO TABLE ti_qmma
           FOR ALL ENTRIES IN ti_viqmel
         WHERE qmnum EQ ti_viqmel-qmnum
           AND kzloesch = space.   "não marcada para eliminação

        IF NOT ti_qmma IS INITIAL.
          SORT ti_qmma BY qmnum.
        ENDIF.

      ENDIF.

      LOOP AT ti_viqmel ASSIGNING <fs_viqmel>.

        APPEND INITIAL LINE TO t_notas_t2_t3 ASSIGNING <fs_notas_out>.

        MOVE-CORRESPONDING <fs_viqmel> TO <fs_notas_out>.
        <fs_notas_out>-bukrs   = <fs_viqmel>-bukrs.
        <fs_notas_out>-anlnr   = <fs_viqmel>-anlnr.
        <fs_notas_out>-anlun   = <fs_viqmel>-anlun.
        <fs_notas_out>-gsber   = <fs_viqmel>-gsber.
        <fs_notas_out>-gsber_d = <fs_viqmel>-gsber_d.
        <fs_notas_out>-kostl   = <fs_viqmel>-kostl.
        <fs_notas_out>-kokrs   = <fs_viqmel>-kokrs.

        CALL FUNCTION 'UNIT_CONVERSION_SIMPLE'
          EXPORTING
            input         = <fs_viqmel>-auszt
            no_type_check = 'X'
            round_sign    = ' '
            unit_in       = 'S'
            unit_out      = 'H'
          IMPORTING
            output        = v_nunc.

        MOVE: v_nunc TO v_var, v_var TO <fs_notas_out>-auszt.
        REPLACE  '.' WITH ',' INTO <fs_notas_out>-auszt.

        CALL FUNCTION 'AIP9_STATUS_READ'
          EXPORTING
            i_objnr = <fs_viqmel>-objnr
            i_spras = sy-langu
          IMPORTING
            e_sysst = e_sysst
            e_anwst = e_anwst.

        <fs_notas_out>-txt04 = e_sysst.

*       Atribui Tipo de Objeto
        READ TABLE ti_equi ASSIGNING <fs_equi> WITH KEY equnr = <fs_viqmel>-equnr BINARY SEARCH.

        IF sy-subrc EQ 0.
          MOVE <fs_equi>-eqart TO <fs_notas_out>-eqart.
        ENDIF.
        UNASSIGN <fs_equi>.

        READ TABLE ti_qmfe ASSIGNING <fs_qmfe> WITH KEY qmnum = <fs_viqmel>-qmnum BINARY SEARCH.

        IF sy-subrc EQ 0.
          MOVE <fs_qmfe>-fegrp TO <fs_notas_out>-fegrp.
          MOVE <fs_qmfe>-fecod TO <fs_notas_out>-fecod.
          MOVE <fs_qmfe>-otgrp TO <fs_notas_out>-otgrp.
          MOVE <fs_qmfe>-oteil TO <fs_notas_out>-oteil.
        ENDIF.
        UNASSIGN <fs_qmfe>.

        READ TABLE ti_qmur ASSIGNING <fs_qmur> WITH KEY qmnum = <fs_viqmel>-qmnum BINARY SEARCH.

        IF sy-subrc EQ 0.
          MOVE <fs_qmur>-urgrp TO <fs_notas_out>-urgrp.
          MOVE <fs_qmur>-urcod TO <fs_notas_out>-urcod.
        ENDIF.
        UNASSIGN <fs_qmur>.

        READ TABLE ti_qmma ASSIGNING <fs_qmma> WITH KEY qmnum = <fs_viqmel>-qmnum BINARY SEARCH.

        IF sy-subrc EQ 0.
          MOVE <fs_qmma>-mngrp TO <fs_notas_out>-mngrp.
          MOVE <fs_qmma>-mncod TO <fs_notas_out>-mncod.
        ENDIF.
        UNASSIGN <fs_qmma>.

*       Atribui ID do centro de trabalho
        READ TABLE ti_crtx ASSIGNING <fs_crtx> WITH KEY objty = <fs_viqmel>-crobjty
                                                        objid = <fs_viqmel>-arbpl BINARY SEARCH.

        IF sy-subrc EQ 0.
          MOVE <fs_crtx>-ktext TO <fs_notas_out>-ktext.
        ENDIF.
        UNASSIGN <fs_crtx>.

*       Atribui ID da localização
        READ TABLE ti_t499s ASSIGNING <fs_t499s> WITH KEY werks = <fs_viqmel>-swerk
                                                          stand = <fs_viqmel>-stort BINARY SEARCH.

        IF sy-subrc EQ 0.
          MOVE <fs_t499s>-ktext TO <fs_notas_out>-ktxt40.
        ENDIF.
      ENDLOOP.
    CATCH cx_root.                                       "#EC CATCH_ALL
      EXIT.
  ENDTRY.

ENDFUNCTION.
