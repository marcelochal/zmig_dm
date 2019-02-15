FUNCTION-POOL ZGPS_EXTRACAO_MAT_SERV.    "MESSAGE-ID ..

* INCLUDE LZGPS_EXTRACAO_DE_DEF_PROJD...     " Local class definition

TYPES:
  BEGIN OF typ_def_proj,
    pspnr TYPE ps_intnr,
    pspid TYPE ps_pspid,
    objnr TYPE j_objnr,
    vbukr TYPE ps_vbukr,
    stat  TYPE j_status,
    inact TYPE j_inact,
  END   OF typ_def_proj.

TYPES: typ_ti_def_proj TYPE STANDARD TABLE OF typ_def_proj ##NEEDED.
