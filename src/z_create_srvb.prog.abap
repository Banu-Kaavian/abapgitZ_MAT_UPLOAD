*&---------------------------------------------------------------------*
*& Report Z_CREATE_SRVB
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT z_create_srvb.



DATA: lv_srvb_name(40)     TYPE c VALUE 'Z_ABAP_PROGRAM_BINDING_833',
      lv_description(60)   TYPE c VALUE 'DYNAMIC SRV BINDING',
      lv_package       TYPE devclass VALUE 'Z_833',
      lv_transport     TYPE trkorr value 'S4HK903175',
      lt_message       TYPE STANDARD TABLE OF REF TO cx_rap_service WITH DEFAULT KEY ,
lv_version(4) type n VALUE '001',
lv_srv_name(40) TYPE c value 'ZNEW_DEF'.
TRY.

 cl_rap_service_binding_odata=>get_instance(
   RECEIVING
     ro_instance = data(lo_ref)
 ).


lo_ref->publish_locally(
  iv_srvb_name       = lv_srvb_name
  iv_service_name    = lv_srv_name
  iv_service_version =  lv_version
).
 CATCH cx_rap_service INTO DATA(lx_rap).
    MESSAGE lx_rap->get_text( ) TYPE 'E'.
ENDTRY.
*CATCH cx_rap_service_binding_odata. " RESTful ABAP: OData Service Gen. Exception

* ENDTRY.
* CATCH cx_rap_service_binding_odata. " RESTful ABAP: OData Service Gen. Exception
*      EXPORTING
*        iv_srvb_name     = lv_srvb_name
**        is_srvb_obj_data = ls_srvb_obj_data
*        iv_description   = lv_description
*        iv_package       = lv_package
*        iv_transport     = lv_transport
*      IMPORTING
*        et_message       = lt_message
*    ).

    " Process messages
*    LOOP AT lt_message INTO DATA(ls_message).
*      WRITE: / ls_message->get_text().
*    ENDLOOP.

**  CATCH cx_root INTO DATA(lx_exception).
*    " Handle exceptions
*    WRITE: / 'Error:', lx_exception->get_text( ).
**ENDTRY.

**
*TYPES: BEGIN OF ty_in_entity,
*         external_name TYPE string,
*         internal_name TYPE string,
*         object_type   TYPE wbobjtype,
*         etag_support  TYPE abap_bool,
*       END OF ty_in_entity.
*
*TYPES: tt_in_entities TYPE STANDARD TABLE OF ty_in_entity WITH DEFAULT KEY.
*
**DATA: lt_entities TYPE tt_in_entities.
*TYPES: ty_version TYPE n LENGTH 1.
*
*TYPES: BEGIN OF ENUM em_odata_version BASE TYPE ty_version,
*         v2      VALUE 2,
*         v4      VALUE 4,
*         unknown VALUE IS INITIAL,
*       END OF ENUM em_odata_version .
*
*TYPES: ty_service_namespace TYPE string.
*TYPES ty_message           TYPE REF TO cx_root .
*TYPES ty_property_name     TYPE string.
**  TYPES ty_service_namespace TYPE string.
*
*TYPES: BEGIN OF ty_entity_flavors,
*         createable TYPE abap_bool,
*         deleteable TYPE abap_bool,
*         updateable TYPE abap_bool,
*       END OF ty_entity_flavors.
*
*TYPES tt_messages TYPE STANDARD TABLE OF REF TO cx_root WITH DEFAULT KEY .
*
*
*TYPES:BEGIN OF ty_entity,
*        external_name TYPE string,
*        internal_name TYPE string,
*        object_type   TYPE wbobjtype,
*        etag_support  TYPE abap_bool,       "implement
*        flavors       TYPE ty_entity_flavors,
*        etag_property TYPE ty_property_name,
*        messages      TYPE tt_messages,
*      END OF ty_entity .
*TYPES:
*  tt_entities TYPE STANDARD TABLE OF ty_entity WITH DEFAULT KEY .
*
*
*
*DATA : lo_ref TYPE REF TO cl_rap_service_consumption.
*
*DATA:
*      lv_package       TYPE devclass VALUE 'Z_833',   " Change as needed
*      lv_transport     TYPE trkorr VALUE 'S4HK903175',       " Change as needed
*      lv_srvd_name     TYPE srvd_artifact_name VALUE 'ZC_BILLING_REPORT_SRC', " Change as needed
*      lt_entities      TYPE tt_in_entities,
**      lv_odata_version   TYPE if_rap_service_consumption=>em_odata_version,
**      lv_odata_version TYPE em_odata_version VALUE 'v2',
**       lv_odata_version TYPE c VALUE 'v2',
*      lv_service_ns    TYPE ty_service_namespace,
*      lt_out_entities  TYPE tt_entities.
*DATA: lo_service TYPE REF TO cl_rap_service_consumption.
*
*DATA lv_odata_version TYPE if_rap_service_consumption=>em_odata_version.
**lv_odata_version = if_rap_service_consumption=>em_odata_version-v2.
**data : lv_xml type string.
*TRY.
*    " Generate service binding dynamically
**    CALL METHOD cl_rap_service_consumption=>generate
*
**     CALL METHOD lo_service->generate
*
*DATA(lv_xml) = |<root>|
*             && |<entry>|
*             && |<id>http://54.208.196.245:50000/sap/opu/odata/sap/Z_SRVBD_SRV/Create_Set(SRVD_NAME='ZC_BILLING_REPORT_SRC',CDS_NAME='ZC_BILLING_REPORT')</id>|
*             && |<uri>http://54.208.196.245:50000/sap/opu/odata/sap/Z_SRVBD_SRV/Create_Set(SRVD_NAME='ZC_BILLING_REPORT_SRC',CDS_NAME='ZC_BILLING_REPORT')</uri>|
*             && |<type>Z_SRVBD_SRV.Create</type>|
*             && |</entry>|
*             && |</root>|.
** call method IF_RAP_SERVICE_CONSUMPTION=>GENERATE
*    CALL METHOD lo_service->if_rap_service_consumption~generate
*      EXPORTING
*        iv_xml               = lv_xml
*        iv_package           = lv_package
*        iv_transport         = lv_transport
*        iv_srvd_name         = lv_srvd_name
*        it_entities          = lt_entities
*      IMPORTING
**        ev_odata_version     = lv_odata_version
*        ev_service_namespace = lv_service_ns
*        et_entities          = lt_out_entities.
*
*    WRITE: 'Service Binding Created Successfully:', lv_srvd_name.
*
*  CATCH cx_root INTO DATA(lo_error).
*    WRITE: 'Error:', lo_error->get_text( ).
*ENDTRY.
