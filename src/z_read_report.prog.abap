*&---------------------------------------------------------------------*
*& Report Z_READ_REPORT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_READ_REPORT.

**DATA: lt_source TYPE STANDARD TABLE OF string,
**      lv_repname TYPE sy-repid.
**
**lv_repname = 'Z_833_COLLECT'.  " Name of the report
**
**READ REPORT lv_repname INTO lt_source.
**
**IF sy-subrc = 0.
**  LOOP AT lt_source INTO DATA(lv_line).
**    WRITE: / lv_line.
**  ENDLOOP.
**ELSE.
**  WRITE: 'Report not found or cannot be read.'.
*ENDIF.


*DATA: lo_factory      TYPE REF TO cl_srvd_rt_factory,
*      lo_persist      TYPE REF TO if_srvd_rt_persist,
*      lt_exposed_art  TYPE if_srvd_metadata=>tt_exposed_artifact,
*      lt_incl_service TYPE if_srvd_rt_persist=>tt_included_service,
**      lt_annotation   TYPE tt_annotation,
**      lt_contracts    TYPE if_srvd_metadata=>tt_provider_contract,
*      lv_srvdname     TYPE srvdname VALUE 'ZMY_DYNAMIC_SERVICE4',
*      lv_service_raw  TYPE srvdname_raw VALUE 'ZMY_DYNAMIC_SERVICE4',
*      lv_dt_uuid      TYPE sysuuid_c32,
*      lv_source_type  TYPE srvdsourcetype VALUE 'S',
*      tr_no VALUE 'S4HK903174',
*      lv_gen_version  TYPE int1 VALUE 1.
**CONSTANTS:lv_language     VALUE sy-langu.
*
** Generate a UUID for the service definition
*CALL FUNCTION 'GUID_CREATE'
*  IMPORTING ev_guid_32 = lv_dt_uuid.
*
** Get an instance of the service persistence handler
*lo_persist = cl_srvd_rt_factory=>create_instance( ).
*
*
*DATA: lt_exposed_artifact TYPE if_srvd_metadata=>tt_exposed_artifact,
*      ls_exposed_artifact TYPE if_srvd_metadata=>ty_exposed_artifact.
*data : LV_TR TYPE TRKORR VALUE 'S4HK903175'.
*data: lv_devclass type DEVCLASS value 'ZVIEW_DEMO'.
*
*
*
** Fill the structure with required values
*ls_exposed_artifact-exposed_artifact = 'ZC_BILLING_REPORT'.  " Exposed CDS View name
*ls_exposed_artifact-exposed_alias   = 'ZCDS_REPORT'. " Provide a valid alias name
*ls_exposed_artifact-origin_artifact = 'ZC_BILLING_REPORT'.  " Original artifact name
*
** Append to the table
*APPEND ls_exposed_artifact TO lt_exposed_artifact.
*
*
*IF sy-subrc = 0.
* CALL FUNCTION 'TRINT_TADIR_INSERT'
*    EXPORTING
*      author    = sy-uname        " Current user
*      devclass  = 'Z_833'   " Your actual package name
*      object    = 'SRVD'          " Object type for Service Definition
*      obj_name  = lv_srvdname     " Service Definition name
*      pgmid     = 'R3TR'         " Program ID
**      trkorr    = 'S4HK903175'.   " Transport Request number
*
* EXCEPTIONS
*   OBJECT_EXISTS_GLOBAL       = 1
*   OBJECT_EXISTS_LOCAL        = 2
*   OTHERS                     = 3.
*  WRITE: 'Service Definition Assigned to Package Successfully.'.
*ELSE.
*  WRITE: 'Service Definition Creation Failed.'.
*ENDIF.
*
*
** Call the SAVE_RT_OBJECT_DATA method
*CALL METHOD lo_persist->save_rt_object_data
*  EXPORTING
*    i_srvdname         = lv_srvdname
*    i_service_raw      = lv_service_raw
*    it_exposed_artifact = lt_exposed_artifact
*    it_incl_service    = lt_incl_service
*    i_dt_uuid          = lv_dt_uuid
*    i_source_type      = lv_source_type
*    i_original_language = sy-langu
*    i_gen_version      = lv_gen_version.
**    it_annotation      = lt_annotation
**    it_contracts       = lt_contracts.
*
** Check if the service definition was created successfully in table SRVD_RT_HEADER
*DATA: lt_srvd_header TYPE TABLE OF srvd_rt_header,
*      ls_srvd_header TYPE srvd_rt_header.
*
*SELECT SINGLE * FROM srvd_rt_header INTO ls_srvd_header
*WHERE srvdname = lv_srvdname.
*
*  WRITE: 'Service Definition Created Successfully.'.
** Assign Service Definition to a Package

* TYPES:
*    BEGIN OF ts_entity,
*      cds_name TYPE string,
*      alias    TYPE string,
*    END OF ts_entity .
*  TYPES:
*    tt_cds_entities TYPE STANDARD TABLE OF ts_entity WITH DEFAULT KEY .

DATA(lo_srvd_gen) = NEW cl_rap_opxy_edmx_srvd_gen( ).  " Instantiate the class
TYPES:
  BEGIN OF ts_cds_entity,
    cds_name TYPE string,
    alias    TYPE string,
  END OF ts_cds_entity.

TYPES: tt_cds_entity TYPE STANDARD TABLE OF ts_cds_entity WITH DEFAULT KEY.

  .
 DATA: ls_cds_entity TYPE ts_cds_entity,
      lt_cds_entities TYPE tt_cds_entity.

ls_cds_entity-cds_name = 'ZC_BILLING_REPORT'.
ls_cds_entity-alias = 'ZC_BILLING'. " Ensure alias is assigned (even if empty)

APPEND ls_cds_entity TO lt_cds_entities.


    data:
      lv_transport    TYPE trkorr VALUE 'S4HK903175',  "Change this
      lv_package      TYPE devclass VALUE 'Z_833'.      "Change this


TRY.
  lo_srvd_gen->create(
        iv_srvd_name        = 'ZMY_DYN_SERVICE833'  "Service Definition Name
        iv_srvd_description = 'Dynamic Service Definition'
        it_cds_entities     = lt_cds_entities
        iv_transport        = lv_transport
        iv_package          = lv_package
        iv_srvd_namespace   = ''  "Keep empty if not required
    ).
    WRITE: 'Service Definition Created Successfully!'.
CATCH cx_root INTO DATA(lx).
    WRITE: 'Error:', lx->get_text( ).
ENDTRY.
