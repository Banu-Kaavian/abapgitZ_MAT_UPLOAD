class ZCL_ZPROGRAM_READ_DPC_EXT definition
  public
  inheriting from ZCL_ZPROGRAM_READ_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZPROGRAM_READ_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITY
**  EXPORTING
**    iv_entity_name          =
**    iv_entity_set_name      =
**    iv_source_name          =
**    it_key_tab              =
**    it_navigation_path      =
**    io_tech_request_context =
**  IMPORTING
**    er_entity               =
**    es_response_context     =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
*   select from tadir
*    inner join reposrc on tadir.obj_name = reposrc.progname
*
*        key tadir.obj_name             "// Program Name
*        tadir.devclass         "// Development Package
*        tadir.author              "// Created By
*        reposrc.data    "// Program Source Code
*
*    where tadir.object = 'PROG'  // Fetch only ABAP Programs
*      and reposrc.r3state = 'A'; // Fetch only active versions
*
*    select
*    a~obj_name
*    a~devclassa
*    a~author
*    b~data
*    FROM tadir as a
*    INNER JOIN reposrc as b
*    a~


  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET.
**TRY.
*CALL METHOD SUPER->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_ENTITYSET
**  EXPORTING
**    iv_entity_name           =
**    iv_entity_set_name       =
**    iv_source_name           =
**    it_filter_select_options =
**    it_order                 =
**    is_paging                =
**    it_navigation_path       =
**    it_key_tab               =
**    iv_filter_string         =
**    iv_search_string         =
**    io_tech_request_context  =
**  IMPORTING
**    er_entityset             =
**    es_response_context      =
*    .
**  CATCH /iwbep/cx_mgw_busi_exception.
**  CATCH /iwbep/cx_mgw_tech_exception.
**ENDTRY.
*    data : v_xstr type xstring,
*        t_xtab TYPE TABLE OF x255,
*        v_file TYPE string,
*        ProgramName type ZCL_ZPROGRAM_READ_mPC_EXT=>ts_read,
*          v_xlen TYPE i,
*        ls_key_tab like LINE OF it_key_tab.
*
* read TABLE it_key_tab into ls_key_tab INDEX 1.
*        if ls_key_tab is not initial.
*  " -- Fetch compressed source code
*  SELECT SINGLE data INTO v_xstr FROM reposrc
*          WHERE progname = ls_key_tab-value AND r3state = 'A'.
* endif.
*  v_xlen = XSTRLEN( v_xstr ).
*v_file = 'C:\temp\ABAP_Source.txt'.
*  " -- Plug source into a table
*  CALL METHOD cl_swf_utl_convert_xstring=>xstring_to_table
*    EXPORTING
*      i_stream = v_xstr
*    IMPORTING
*      e_table  = t_xtab
*    EXCEPTIONS
*      OTHERS   = 1.
*
*  " -- Download to local file
*  CALL FUNCTION 'GUI_DOWNLOAD'
*    EXPORTING
*      filetype     = 'BIN'
*      filename     = v_file
*      bin_filesize = v_xlen
*    TABLES
*      data_tab     = t_xtab
*    EXCEPTIONS
*      OTHERS       = 0.


    DATA: lt_source TYPE  TABLE OF string,
          ls_source like LINE OF lt_source,
          ls_entity type  ZCL_ZPROGRAM_READ_mpc_EXT=>ts_read,
          ls_entityset type TABLE OF ZCL_ZPROGRAM_READ_mpc_EXT=>ts_read,
*          ls_entity like ls_data,
*          er_entityset TYPE TABLE OF data,
      lv_repname TYPE sy-repid.

    data:lv_full_source TYPE   string.  " To store concatenated source code



    " Get Report Name from Request
  READ TABLE it_filter_select_options INTO DATA(ls_filter)
    WITH KEY property = 'ProgramName'.

  IF sy-subrc = 0 AND ls_filter-select_options IS NOT INITIAL.
    lv_repname = ls_filter-select_options[ 1 ]-low.
  ELSE.
    RETURN.
  ENDIF.





*lv_repname = ls_entity-programname.  " Name of the report

READ REPORT lv_repname INTO lt_source.

IF sy-subrc = 0.
  LOOP AT lt_source INTO DATA(lv_line).
*  LOOP AT lt_source INTO ls_Source.
*    CONCATENATE lv_full_source lv_line cl_abap_char_utilities=>cr_lf INTO lv_full_source.
*    write : / lv_full_source.
*    CLEAR ls_entity.

* CONCATENATE lv_full_source lv_line INTO lv_full_source.

 CONCATENATE LINES OF lt_source INTO lv_full_source.


      ENDLOOP.
*    BREAK-POINT.
*LOOP AT ls_entityset INTO DATA(ls_debug).
*  WRITE: / ls_debug-SourceCode.
*ENDLOOP.
 ls_entity-programName = lv_repname.
*        ls_entity-SourceCode = lv_line.
        ls_entity-SourceCode = lv_full_source.
APPEND ls_entity TO ls_entityset.
 copy_data_to_ref(
        EXPORTING
          is_data = ls_entityset
        CHANGING
          cr_data = er_entityset ).





ELSE.
  WRITE: 'Report not found or cannot be read.'.
ENDIF.
*  endmethod.
  endmethod.
ENDCLASS.
