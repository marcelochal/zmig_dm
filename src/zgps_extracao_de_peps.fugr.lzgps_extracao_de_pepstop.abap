FUNCTION-POOL zgps_extracao_de_peps.    "MESSAGE-ID ..

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

TYPES:
  BEGIN OF typ_pep,
    pspnr TYPE ps_posnr,
    posid TYPE ps_posid,
    stort TYPE ps_stort,
  END   OF typ_pep.

TYPES: typ_ti_pep TYPE HASHED TABLE OF typ_pep WITH UNIQUE KEY posid ##NEEDED.
