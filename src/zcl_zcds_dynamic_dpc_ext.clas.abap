class ZCL_ZCDS_DYNAMIC_DPC_EXT definition
  public
  inheriting from ZCL_ZCDS_DYNAMIC_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZCDS_DYNAMIC_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_ENTITY.


    data : ls_data type ZCL_ZCDS_DYNAMIC_mPC_EXT=>ts_data,
*      ls_tr TYPE ZCL_ZCDS_CREATE_mpc_EXT=>ts_trfolder,
      ls_entity like ls_data,
*      ls_trno like ls_tr,
      lv_putstate type OBJSTATE value 'N',
      lv_prid type sytabix,
      lv_devclass type DEVCLASS value 'Z_833V2',
      LV_TR TYPE TRKORR VALUE 'S4HK903463',

      lv_username type syuname value 'KAAV13'.
data : lo_ref TYPE REF TO CL_DD_DDL_HANDLER.




call method io_data_provider->read_entry_data
IMPORTING
  es_data = ls_entity.

if ls_entity is NOT INITIAL.
  ls_data-ddlname = ls_entity-ddlname.
  ls_data-ddtext = ls_entity-ddtext.
  ls_data-ddlanguage = sy-langu.
  ls_data-source = ls_entity-source.
  ENDIF.
*  endif.
*DATA lv_exists TYPE abap_bool.


DATA(ddl_handler) = cl_dd_ddl_handler_factory=>create( ).


call METHOD ddl_handler->WRITE_TRKORR
EXPORTING
  TRKORR =    LV_TR
OBJECTNAME = ls_data-ddlname
PRID = LV_PRID.



call METHOD ddl_handler->write_tadir
  EXPORTING
    objectname = ls_data-ddlname
*    objecttype = lv_objtype
    DEVCLASS = lv_devclass
    USERNAME = lv_username
    prid = lv_prid.




try.
  call METHOD ddl_handler->save
  EXPORTING
name  =   ls_data-ddlname
 put_state    = lv_putstate
 ddddlsrcv_wa =  ls_data.


*  *   Activate the DDL source
  CALL METHOD ddl_handler->activate
    EXPORTING name = ls_data-ddlname.

*  CATCH cx_dd_ddl_save .
    CATCH cx_dd_ddl_save INTO DATA(lx_ddl).
        MESSAGE lx_ddl->get_text( ) TYPE 'E'.









ENDTRY.


 copy_data_to_ref(
          EXPORTING
            is_data = ls_data
            CHANGING
              cr_data = er_entity ).

*ENDTRY.
  endmethod.
ENDCLASS.
