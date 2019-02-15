* --------------------------------------------------------------------------*
*                  T A E S A - MIGRAÇÃO - AGIR                              *
* --------------------------------------------------------------------------*
* Consultoria .....: Intechpro                                              *
* ABAP.............: Luís Fernando de Vasconcellos                          *
* Funcional........: Marcelo Alvares                                        *
* Módulo...........: FI                                                     *
* Programa.........: ZFFI_OBTER_CONTAS_BCO_EMPRESA                          *
* Transação........:                                                        *
* Tipo de Prg......: FUNÇÃO                                                 *
* Objetivo.........: Obter as contas bancárias                              *
* Data.............: 31/07/2018                                             *
* --------------------------------------------------------------------------*
* Request    | Descrição                                                    *
* --------------------------------------------------------------------------*
* TBDK920398 | AGIR - Extração de Dados para Migração - Contas bcos. empresa*
* --------------------------------------------------------------------------*

FUNCTION zffi_obter_contas_bco_empresa.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(P_I_EMPRESAS) TYPE  FAGL_RANGE_T_BUKRS
*"     REFERENCE(P_I_NUM_LINHAS) TYPE  /SAPCND/CODE_LINE_NUM
*"  EXPORTING
*"     REFERENCE(P_E_TI_EXT_CTAS_BCO) TYPE  ZTFI_EXT_CONTAS_BCO_EMPRESA
*"----------------------------------------------------------------------

  DATA: ti_ext_ctas_bco TYPE ztfi_ext_contas_bco_empresa,
        ti_t042i        TYPE TABLE OF t042i,
        e_t042i         TYPE t042i,
        e_ext_ctas_bco  TYPE zefi_ext_contas_bco_empresa.


*--------------------------------------------------*
* Selecionar as contas nos bancos por empresa
  SELECT
      k~bukrs,
      k~hbkid,
      k~hktid,
      x~banks,
      x~bankl,
      k~bankn,
      k~bkont,
      x~name1,
      a~stras,
      a~ort01,
      t~text1,
      k~waers,
      k~bnkn2,
      k~fdgrp,
      k~abwae,
      k~hkont,
      k~wekon,
      k~mindt,
      k~hbid1,
      k~hkid1,
      k~hbid2,
      k~hkid2,
      k~wkkon,
      k~wikon
      FROM t012 AS x
      LEFT OUTER JOIN t012k AS k ON k~bukrs = x~bukrs AND
                                    k~hbkid = x~hbkid
      LEFT OUTER JOIN bnka AS a ON a~banks = x~banks AND
                                   a~bankl = x~bankl
      LEFT OUTER JOIN t012t AS t ON t~spras = @sy-langu AND
                                    t~bukrs = k~bukrs AND
                                    t~hbkid = k~hbkid AND
                                    t~hktid = k~hktid  "#EC CI_BUFFJOIN
      INTO TABLE @ti_ext_ctas_bco
      UP TO @p_i_num_linhas  ROWS
      WHERE x~bukrs IN @p_i_empresas.

  IF sy-subrc NE 0.
    RETURN.
  ENDIF.

  "Selecionar divisões para bancos
  SELECT *
    INTO TABLE ti_t042i
    FROM t042i
    WHERE zbukr IN p_i_empresas.

  SORT ti_t042i BY zbukr hbkid.

  IF ti_t042i IS NOT INITIAL.

    LOOP AT ti_ext_ctas_bco INTO e_ext_ctas_bco.

      READ TABLE ti_t042i INTO e_t042i
        WITH KEY zbukr = e_ext_ctas_bco-bukrs
                 hbkid = e_ext_ctas_bco-hbkid BINARY SEARCH.
      IF sy-subrc = 0.
        e_ext_ctas_bco-gsber = e_t042i-gsber.
        MODIFY ti_ext_ctas_bco FROM e_ext_ctas_bco TRANSPORTING gsber.
      ENDIF.

    ENDLOOP.

  ENDIF.


  p_e_ti_ext_ctas_bco[] = ti_ext_ctas_bco[].

ENDFUNCTION.
