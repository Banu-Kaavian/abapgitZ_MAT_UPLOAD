class ZCL_Z_SRVBD_DPC_EXT definition
  public
  inheriting from ZCL_Z_SRVBD_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_Z_SRVBD_DPC_EXT IMPLEMENTATION.


  METHOD /iwbep/if_mgw_appl_srv_runtime~create_entity.
    DATA: ls_data   TYPE zcl_z_srvbd_mpc_ext=>ts_create,
          ls_entity LIKE ls_data.
    DATA(lo_srvd_gen) = NEW cl_rap_opxy_edmx_srvd_gen( ).  " Instantiate the class
    TYPES:
      BEGIN OF ts_cds_entity,
        cds_name TYPE string,
        alias    TYPE string,
      END OF ts_cds_entity.

    TYPES: tt_cds_entity TYPE STANDARD TABLE OF ts_cds_entity WITH DEFAULT KEY.

    CALL METHOD io_data_provider->read_entry_data
      IMPORTING
        es_data = ls_entity.

    IF ls_entity IS NOT INITIAL.
ls_data-srvd_name = ls_entity-srvd_name.
ls_data-srvd_DES = ls_entity-srvd_DES.
ls_data-CDS_name = ls_entity-CDS_name.
ls_data-alies_name = ls_entity-alies_name.

ENDIF.
      .
      DATA: ls_cds_entity   TYPE ts_cds_entity,
            lt_cds_entities TYPE tt_cds_entity.

      ls_cds_entity-cds_name = ls_entity-CDS_name.
      ls_cds_entity-alias = ls_entity-alies_name. " Ensure alias is assigned (even if empty)

      APPEND ls_cds_entity TO lt_cds_entities.




      DATA:
        lv_transport TYPE trkorr VALUE 'S4HK903463',  "Change this
        lv_package   TYPE devclass VALUE 'Z_833V2'.      "Change this


      TRY.
          lo_srvd_gen->create(
                iv_srvd_name        = ls_entity-srvd_name  "Service Definition Name
                iv_srvd_description = ls_entity-srvd_DES
                it_cds_entities     = lt_cds_entities
                iv_transport        = lv_transport
                iv_package          = lv_package
                iv_srvd_namespace   = ''  "Keep empty if not required
            ).
*          WRITE: 'Service Definition Created Successfully!'.
*        CATCH cx_root INTO DATA(lx).
*          WRITE: 'Error:', lx->get_text( ).
      ENDTRY.



      copy_data_to_ref(
          EXPORTING
            is_data = ls_data
            CHANGING
              cr_data = er_entity ).

    ENDMETHOD.
ENDCLASS.
