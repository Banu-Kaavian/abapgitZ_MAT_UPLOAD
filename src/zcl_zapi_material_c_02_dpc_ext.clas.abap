class ZCL_ZAPI_MATERIAL_C_02_DPC_EXT definition
  public
  inheriting from ZCL_ZAPI_MATERIAL_C_02_DPC
  create public .

public section.

  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
    redefinition .
  methods /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
    redefinition .
protected section.

  constants GC_3 type CRMT_MPK_DECIMALS value '3' ##NO_TEXT.
  constants GC_CHAR type ATFOR value 'CHAR' ##NO_TEXT.
  constants GC_NUM type ATFOR value 'NUM' ##NO_TEXT.
  constants GC_CURR type ATFOR value 'CURR' ##NO_TEXT.
  constants GC_FINISHED_PRODUCT type MTART value 'FERT' ##NO_TEXT.
  constants GC_MESSAGE_CLASS type SYST_MSGID value 'Z_MSG_CLASS_MAT' ##NO_TEXT.

  methods FORMAT_MESSAGE
    importing
      !IV_MESSAGE_TYPE type SYST_MSGTY
      !IV_MESSAGE_NUM type SYST_MSGNO
      value(IV_MSGV1) type SYST_MSGV optional
      value(IV_MSGV2) type SYST_MSGV optional
      value(IV_MSGV3) type SYST_MSGV optional
      value(IV_MSGV4) type SYST_MSGV optional
    changing
      !CT_RETURN type BAPIRET2_TAB .
  methods GET_EXPANDED_CLASSES
    importing
      !IM_EXPAND type ref to /IWBEP/IF_MGW_ODATA_EXPAND
      !IM_PARENT type /IWBEP/IF_MGW_ODATA_EXPAND=>TY_S_NODE_CHILD optional
      !IT_CHILDREN type /IWBEP/IF_MGW_ODATA_EXPAND=>TY_T_NODE_CHILDREN
    changing
      !CT_EXPANDED_TECH_CLAUSES type STRING_TABLE .
  methods GET_OBJKEY
    importing
      !IV_MATNR type MATNR
    exporting
      !EX_OBJKEY type OBJNUM .
  methods MATERIAL_CLASS_CHAR_CREATE
    importing
      !IM_MAT_DEEP_ENTITY type ANY
    exporting
      !ET_RETURN type BAPIRET2_T .
private section.
ENDCLASS.



CLASS ZCL_ZAPI_MATERIAL_C_02_DPC_EXT IMPLEMENTATION.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY.
 DATA:
      lt_return      TYPE BAPIRET2_t,
      ls_return      TYPE bapiret2,
      lo_exception   TYPE REF TO /iwbep/cx_mgw_busi_exception,
      ls_deep_entity TYPE zcl_zapi_material_char_mpc_ext=>ts_deep_mat.


    io_data_provider->read_entry_data(
    IMPORTING
    es_data = ls_deep_entity ).

*-- Call method to create Material Chracteristics data
    CALL METHOD me->material_class_char_create
      EXPORTING
        im_mat_deep_entity = ls_deep_entity
      IMPORTING
        et_return          = lt_return.

    READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
    IF sy-subrc <> 0.
*-- Add success code here
      me->mo_context->get_message_container( )->add_messages_from_bapi( it_bapi_messages = lt_return
       iv_add_to_response_header = 'X' ).
*-- Returning data
      CALL METHOD copy_data_to_ref(
        EXPORTING
          is_data = ls_deep_entity
        CHANGING
          cr_data = er_deep_entity ).
    ELSE.

*--  Create Object Exception
      CREATE OBJECT lo_exception.

      lo_exception->get_msg_container( )->add_messages_from_bapi( it_bapi_messages = lt_return
       iv_add_to_response_header = 'X' ).
      RAISE EXCEPTION lo_exception.



    ENDIF.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY.
DATA:
      lt_deep_mat_class TYPE TABLE OF ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_deep_mat,
      lt_char_value     TYPE TABLE OF ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class_char,
      lt_class_char     TYPE TABLE OF ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class,
      lo_exception      TYPE REF TO /iwbep/cx_mgw_busi_exception,
      ls_deep_mat_class TYPE ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_deep_mat,
      ls_class_char     TYPE ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class,
      ls_char_value     TYPE ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class_char,
      lv_clint          TYPE clint.



TYPES:
     BEGIN OF ty_s_values,
       matnr TYPE matnr,
       cuobj TYPE cuobj,
       klart TYPE klassenart,
       atinn TYPE atinn,
       atwrt TYPE atwrt,
       atflv TYPE atflv,
      END OF ty_s_values,

      BEGIN OF ty_s_cabn,
        atinn TYPE atinn,
        atnam TYPE atnam,
        atfor TYPE atfor,
     END OF ty_s_cabn,
     BEGIN OF ty_s_klah_ksml,
       imerk TYPE atinn,
       clint TYPE clint,
       klart TYPE	klassenart,
       class  TYPE klasse_d,
     END OF ty_s_klah_ksml.

  DATA: lt_values     TYPE TABLE OF ty_s_values,
        lt_cabn       TYPE TABLE OF ty_s_cabn,
        lt_klah_ksml  TYPE TABLE OF ty_s_klah_ksml,
        lt_selopt     TYPE TABLE OF selopt,
        ls_klah_ksml  TYPE ty_s_klah_ksml,
        ls_cabn       TYPE ty_s_cabn,
        ls_values     TYPE ty_s_values,
        lv_char_value TYPE crmt_mpk_value_visible_ui,
        ls_key        TYPE /iwbep/s_mgw_name_value_pair,
        ls_sel        TYPE selopt,
        lv_msgv1      TYPE symsgv.


FIELD-SYMBOLS:
         <fs_header> TYPE any.

   CASE iv_entity_set_name.
     WHEN 'MaterialSet'.


       IF it_key_tab IS NOT INITIAL.
*-- Then fill filter values
         LOOP AT it_key_tab INTO ls_key WHERE name = 'material'.
           ls_sel-sign = 'I'.
           ls_sel-option = 'EQ'.
           CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
             EXPORTING
               input             = ls_key-value
            IMPORTING
              output             = ls_sel-low
            EXCEPTIONS
              length_error       = 1
              OTHERS             = 2
                     .
           IF sy-subrc <> 0.
*--  Create Object Exception
             lv_msgv1 = ls_key-value.
             CREATE OBJECT lo_exception.
             CALL METHOD lo_exception->get_msg_container( )->add_message(
             EXPORTING
               iv_msg_type               = 'E'
               iv_msg_id                 = 'Z_MSG_CLASS_MAT'
               iv_msg_number             = '004'
               iv_add_to_response_header = abap_true
               ).

             RAISE EXCEPTION lo_exception.

           ENDIF.


           APPEND ls_sel TO lt_selopt.
           CLEAR ls_sel.
         ENDLOOP.
       ENDIF.

*-- Check if there are request paramters in IT_KEY_TAB


*-- Get material classification values for all Material with type FINISHED PRODUCT
  SELECT a~matnr
         b~cuobj
         b~klart
         c~atinn
         c~atwrt
         c~atflv
  INTO TABLE lt_values
  FROM mara AS a INNER JOIN
       inob AS b ON ( b~objek = a~matnr ) INNER JOIN
       ausp AS c ON ( c~objek = b~cuobj )
  WHERE a~matnr IN lt_selopt
  AND   a~mtart = gc_finished_product.


IF sy-subrc = 0.
*-- Sort by Internal Characteristic number
  SORT lt_values BY atinn.


*-- Get Characteristics name and Characteristics type
  SELECT atinn
         atnam
         atfor
  FROM cabn
  INTO TABLE lt_cabn
  FOR ALL ENTRIES IN lt_values
  WHERE atinn = lt_values-atinn.
  IF sy-subrc = 0.
*-- Sort by Charcteristic internal number
    SORT lt_cabn BY atinn.
  ENDIF.

*-- Get Class name by Characteristics.
  SELECT a~imerk
         b~clint
         b~klart
         b~class
  INTO TABLE lt_klah_ksml
  FROM ksml AS a INNER JOIN klah AS b
    ON b~clint = a~clint
  FOR ALL ENTRIES IN lt_values
  WHERE a~imerk = lt_values-atinn.

  IF sy-subrc = 0.
*-- Sort by Charcteristic internal number
    SORT lt_klah_ksml BY imerk.
  ENDIF.


*-- Create Expanded Entity set .

SORT lt_values BY matnr
                  klart
                  atinn.

LOOP AT lt_values INTO ls_values.

  AT NEW klart.
*-- Assign Material number and class_type
    ls_deep_mat_class-matnr = ls_values-matnr.
    ls_deep_mat_class-class_type = ls_values-klart.
  ENDAT.
  CLEAR: ls_klah_ksml.

  READ TABLE lt_klah_ksml INTO ls_klah_ksml
                          WITH KEY imerk = ls_values-atinn.


  IF  lv_clint IS INITIAL.
    lv_clint = ls_klah_ksml-clint.
    ls_class_char-class = ls_klah_ksml-class.
  ELSEIF lv_clint <> ls_klah_ksml-clint.

    ls_class_char-characteristics = lt_char_value[].
    APPEND ls_class_char TO lt_class_char.

    lv_clint = ls_klah_ksml-clint.
    ls_class_char-class = ls_klah_ksml-class.
    CLEAR: lt_char_value[].
  ENDIF.

  CLEAR: ls_cabn.
  READ TABLE lt_cabn INTO ls_cabn WITH KEY atinn = ls_values-atinn.
  ls_char_value-charact = ls_cabn-atnam. "Characteristic name
  CASE ls_cabn-atfor.
    WHEN gc_CHAR.
      ls_char_value-value = ls_values-atwrt.
    WHEN gc_NUM.
       CALL FUNCTION 'CONVERT_VALUE_FLTP_TO_CHAR'
         EXPORTING
           iv_value             = ls_values-atflv
           iv_decimals          = gc_3
        IMPORTING
          ev_value             = lv_char_value.
       SHIFT lv_char_value LEFT DELETING LEADING space.
       WRITE lv_char_value TO ls_char_value-value.
       CLEAR: lv_char_value.

    WHEN gc_CURR.
       CALL FUNCTION 'CONVERT_VALUE_FLTP_TO_CHAR'
         EXPORTING
           iv_value             = ls_values-atflv
           iv_decimals          = gc_3
        IMPORTING
          ev_value             = lv_char_value.
       SHIFT lv_char_value LEFT DELETING LEADING space.
       WRITE lv_char_value TO ls_char_value-value.
       CLEAR: lv_char_value.
  ENDCASE.
  APPEND ls_char_value TO lt_char_value.
  CLEAR: ls_char_value.

  AT END OF klart.
    ls_class_char-characteristics = lt_char_value[].
    APPEND ls_class_char TO lt_class_char.
    ls_deep_mat_class-classes = lt_class_char[].
    CLEAR:
           lt_class_char[],
           lv_clint,
           lt_char_value[].
  ENDAT.
ENDLOOP.

*-- Add expanded clauses to avoid back to back iteration.

    me->get_expanded_classes(
        EXPORTING
          im_expand                 = io_expand
          it_children               = io_expand->get_children( )
        CHANGING
          ct_expanded_tech_clauses  = et_expanded_tech_clauses ).

*-- Copy the data to the response
     copy_data_to_ref(
      EXPORTING
       is_data = ls_deep_mat_class
      CHANGING
       cr_data = er_entity ).


ELSE.
*--  Create Object Exception
      CREATE OBJECT lo_exception.
            CALL METHOD lo_exception->get_msg_container( )->add_message(
        EXPORTING
          iv_msg_type               = 'E'
          iv_msg_id                 = 'Z_MSG_CLASS_MAT'
          iv_msg_number             = '004'
          iv_add_to_response_header = abap_true
          ).

      RAISE EXCEPTION lo_exception.





ENDIF.
ENDCASE.
  endmethod.


  method /IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET.
DATA:
      lt_deep_mat_class TYPE TABLE OF ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_deep_mat,
      lt_char_value     TYPE TABLE OF ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class_char,
      lt_class_char     TYPE TABLE OF ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class,
      lo_exception      TYPE REF TO /iwbep/cx_mgw_busi_exception,
      ls_deep_mat_class TYPE ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_deep_mat,
      ls_class_char     TYPE ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class,
      ls_char_value     TYPE ZCL_ZAPI_MATERIAL_C_02_MPC=>ts_class_char,
      lv_clint          TYPE clint.



TYPES:
     BEGIN OF ty_s_values,
       matnr TYPE matnr,
       cuobj TYPE cuobj,
       klart TYPE klassenart,
       atinn TYPE atinn,
       atwrt TYPE atwrt,
       atflv TYPE atflv,
      END OF ty_s_values,

      BEGIN OF ty_s_cabn,
        atinn TYPE atinn,
        atnam TYPE atnam,
        atfor TYPE atfor,
     END OF ty_s_cabn,
     BEGIN OF ty_s_klah_ksml,
       imerk TYPE atinn,
       clint TYPE clint,
       klart TYPE	klassenart,
       class  TYPE klasse_d,
     END OF ty_s_klah_ksml.

  DATA: lt_values     TYPE TABLE OF ty_s_values,
        lt_cabn       TYPE TABLE OF ty_s_cabn,
        lt_klah_ksml  TYPE TABLE OF ty_s_klah_ksml,
        lt_selopt     TYPE TABLE OF selopt,
        ls_klah_ksml  TYPE ty_s_klah_ksml,
        ls_cabn       TYPE ty_s_cabn,
        ls_values     TYPE ty_s_values,
        lv_char_value TYPE crmt_mpk_value_visible_ui,
        ls_key        TYPE /iwbep/s_mgw_name_value_pair,
        ls_sel        TYPE selopt.


FIELD-SYMBOLS:
         <fs_header> TYPE any.

   CASE iv_entity_set_name.
     WHEN 'MaterialSet'.


       IF it_key_tab IS NOT INITIAL.
*-- Then fill filter values
         LOOP AT it_key_tab INTO ls_key WHERE name = 'material'.
           ls_sel-sign = 'I'.
           ls_sel-option = 'EQ'.
           ls_sel-low = ls_key-value.
           APPEND ls_sel TO lt_selopt.
           CLEAR ls_sel.
         ENDLOOP.
       ENDIF.

*-- Check if there are request paramters in IT_KEY_TAB


*-- Get material classification values for all Material with type FINISHED PRODUCT
  SELECT a~matnr
         b~cuobj
         b~klart
         c~atinn
         c~atwrt
         c~atflv
  INTO TABLE lt_values
  FROM mara AS a INNER JOIN
       inob AS b ON ( b~objek = a~matnr ) INNER JOIN
       ausp AS c ON ( c~objek = b~cuobj )
  WHERE a~matnr IN lt_selopt
  AND   a~mtart = gc_finished_product.


IF sy-subrc = 0.
*-- Sort by Internal Characteristic number
  SORT lt_values BY atinn.


*-- Get Characteristics name and Characteristics type
  SELECT atinn
         atnam
         atfor
  FROM cabn
  INTO TABLE lt_cabn
  FOR ALL ENTRIES IN lt_values
  WHERE atinn = lt_values-atinn.
  IF sy-subrc = 0.
*-- Sort by Charcteristic internal number
    SORT lt_cabn BY atinn.
  ENDIF.

*-- Get Class name by Characteristics.
  SELECT a~imerk
         b~clint
         b~klart
         b~class
  INTO TABLE lt_klah_ksml
  FROM ksml AS a INNER JOIN klah AS b
    ON b~clint = a~clint
  FOR ALL ENTRIES IN lt_values
  WHERE a~imerk = lt_values-atinn.

  IF sy-subrc = 0.
*-- Sort by Charcteristic internal number
    SORT lt_klah_ksml BY imerk.
  ENDIF.


*-- Create Expanded Entity set .

SORT lt_values BY matnr
                  klart
                  atinn.

LOOP AT lt_values INTO ls_values.

  AT NEW klart.
*-- Assign Material number and class_type
    ls_deep_mat_class-matnr = ls_values-matnr.
    ls_deep_mat_class-class_type = ls_values-klart.
  ENDAT.
  CLEAR: ls_klah_ksml.

  READ TABLE lt_klah_ksml INTO ls_klah_ksml
                          WITH KEY imerk = ls_values-atinn.


  IF  lv_clint IS INITIAL.
    lv_clint = ls_klah_ksml-clint.
    ls_class_char-class = ls_klah_ksml-class.
  ELSEIF lv_clint <> ls_klah_ksml-clint.

    ls_class_char-characteristics = lt_char_value[].
    APPEND ls_class_char TO lt_class_char.

    lv_clint = ls_klah_ksml-clint.
    ls_class_char-class = ls_klah_ksml-class.
    CLEAR: lt_char_value[].
  ENDIF.

  CLEAR: ls_cabn.
  READ TABLE lt_cabn INTO ls_cabn WITH KEY atinn = ls_values-atinn.
  ls_char_value-charact = ls_cabn-atnam. "Characteristic name
  CASE ls_cabn-atfor.
    WHEN gc_CHAR.
      ls_char_value-value = ls_values-atwrt.
    WHEN gc_NUM.
       CALL FUNCTION 'CONVERT_VALUE_FLTP_TO_CHAR'
         EXPORTING
           iv_value             = ls_values-atflv
           iv_decimals          = gc_3
        IMPORTING
          ev_value             = lv_char_value.
       SHIFT lv_char_value LEFT DELETING LEADING space.
       WRITE lv_char_value TO ls_char_value-value.
       CLEAR: lv_char_value.

    WHEN gc_CURR.
       CALL FUNCTION 'CONVERT_VALUE_FLTP_TO_CHAR'
         EXPORTING
           iv_value             = ls_values-atflv
           iv_decimals          = gc_3
        IMPORTING
          ev_value             = lv_char_value.
       SHIFT lv_char_value LEFT DELETING LEADING space.
       WRITE lv_char_value TO ls_char_value-value.
       CLEAR: lv_char_value.
  ENDCASE.
  APPEND ls_char_value TO lt_char_value.
  CLEAR: ls_char_value.

  AT END OF klart.
    ls_class_char-characteristics = lt_char_value[].
    APPEND ls_class_char TO lt_class_char.
    ls_deep_mat_class-classes = lt_class_char[].
    APPEND ls_deep_mat_class TO lt_deep_mat_class.
    CLEAR: ls_deep_mat_class,
           lt_class_char[],
           lv_clint,
           lt_char_value[].
  ENDAT.
ENDLOOP.

*-- Add expanded clauses to avoid back to back iteration.

    me->get_expanded_classes(
        EXPORTING
          im_expand                 = io_expand
          it_children               = io_expand->get_children( )
        CHANGING
          ct_expanded_tech_clauses  = et_expanded_tech_clauses ).



*-- Copy the data to the response
     copy_data_to_ref(
      EXPORTING
       is_data = lt_deep_mat_class
      CHANGING
       cr_data = er_entityset ).


ELSE.
*--  Create Object Exception
      CREATE OBJECT lo_exception.
            CALL METHOD lo_exception->get_msg_container( )->add_message(
        EXPORTING
          iv_msg_type               = 'E'
          iv_msg_id                 = 'Z_MSG_CLASS_MAT'
          iv_msg_number             = '004'
          iv_add_to_response_header = abap_true
          ).

      RAISE EXCEPTION lo_exception.

ENDIF.
ENDCASE.
  endmethod.


  method FORMAT_MESSAGE.
    DATA:
          ls_return TYPE bapiret2.

    SHIFT iv_msgv1 LEFT DELETING LEADING '0'.
    SHIFT iv_msgv2 LEFT DELETING LEADING '0'.
    SHIFT iv_msgv3 LEFT DELETING LEADING '0'.
    SHIFT iv_msgv4 LEFT DELETING LEADING '0'.


*-- Call Return message formatting Function module.
    CALL FUNCTION 'BALW_BAPIRETURN_GET2'
      EXPORTING
        type   = iv_message_type
        cl     = gc_Message_class
        number = iv_message_num
        par1   = iv_msgv1
        par2   = iv_msgv2
        par3   = iv_msgv3
        par4   = iv_msgv4
      IMPORTING
        return = ls_return.


* Add message to return tab
    APPEND ls_return TO ct_return.

  endmethod.


  method GET_EXPANDED_CLASSES.
     LOOP AT it_children ASSIGNING FIELD-SYMBOL(<fs_child>).
      IF im_parent IS NOT INITIAL.
        APPEND |{ im_parent-tech_nav_prop_name }/{ <fs_child>-tech_nav_prop_name }| TO ct_expanded_tech_clauses.
       ELSE.
         APPEND |{ <fs_child>-tech_nav_prop_name }| TO ct_expanded_tech_clauses.
       ENDIF.
       get_expanded_classes(
       exporting
         im_expand    = im_expand
         im_parent    = <fs_child>
         it_children  = <fs_child>-node->get_children( )
        changing
          ct_expanded_tech_clauses  = ct_expanded_tech_clauses ).
       ENDLOOP.
endmethod.


  method GET_OBJKEY.
     DATA:
      lv_matnr  TYPE matnr.

*-- Convert Material number to Object key.

    CALL FUNCTION 'CONVERSION_EXIT_MATN1_INPUT'
      EXPORTING
        input        = iv_matnr
      IMPORTING
        output       = lv_matnr
      EXCEPTIONS
        length_error = 1
        OTHERS       = 2.
    IF sy-subrc = 0.
      EX_oBJKEY = lv_matnr.
    ENDIF.

  endmethod.


  method MATERIAL_CLASS_CHAR_CREATE.
      TYPES:
    BEGIN OF ty_s_cabn,
      atnam TYPE atnam,
      atfor TYPE atfor,
    END OF ty_s_cabn.


 DATA:
      ls_deep_entity  TYPE zcl_zapi_material_char_mpc_ext=>ts_deep_mat,
      ls_class        TYPE zcl_zapi_material_char_mpc_ext=>ts_class,
      ls_class_char   TYPE zcl_zapi_material_char_mpc_ext=>ts_class_char,
      ls_curr         TYPE bapi1003_alloc_values_curr,
      ls_num          TYPE bapi1003_alloc_values_num,
      ls_return       TYPE bapiret2,
      ls_cabn         TYPE ty_s_cabn,
      ls_char         TYPE bapi1003_alloc_values_char,
      lv_objkey       TYPE objnum,
      lv_matnr        TYPE matnr,
      lv_msgv1        TYPE syst_msgv,
      lv_msgv2        TYPE syst_msgv,
      ls_mat_item     TYPE zst_material_master_class,
      lt_cabn         TYPE TABLE OF ty_s_cabn,
      lt_list         TYPE TABLE OF bapi1003_alloc_list,
      lt_char         TYPE TABLE OF bapi1003_alloc_values_char,
      lt_curr         TYPE TABLE OF bapi1003_alloc_values_curr,
      lt_num          TYPE TABLE OF bapi1003_alloc_values_num,
      lt_return       TYPE TABLE OF bapiret2.


*-- Get Deep Entity

       ls_deep_entity = im_mat_deep_entity.

*-- Get Material number, check whether material already exists

       lv_matnr = ls_deep_entity-matnr.
       SELECT SINGLE matnr
       FROM mara
       INTO lv_matnr
       WHERE matnr = lv_matnr.
       IF sy-subrc <> 0.
*-- Append error message
        lv_msgv1 = lv_matnr.
        CALL METHOD format_message
          EXPORTING
            iv_message_type = 'E'
            iv_message_num  = '000'
            iv_msgv1        = lv_msgv1
          CHANGING
            ct_return       = et_return.
        EXIT.
       ENDIF.

*-- Check Class type is not Initial
  IF ls_deep_entity-class_type IS INITIAL.
*-- Append error message

        CALL METHOD format_message
          EXPORTING
            iv_message_type = 'E'
            iv_message_num  = '001'
          CHANGING
            ct_return       = et_return.
        EXIT.
    ENDIF.


*-- Get Material Objkey
         CALL METHOD me->get_objkey
           EXPORTING
             iv_matnr  = lv_matnr
           IMPORTING
             ex_objkey = lv_objkey.

*-- Get the Type of characteristic
         SELECT c~atnam
                c~atfor
         INTO TABLE lt_cabn
         FROM klah AS a INNER JOIN ksml AS b ON b~clint = a~clint
         INNER JOIN cabn AS c ON c~atinn = b~imerk
         WHERE a~klart = ls_deep_entity-class_type.

         LOOP AT ls_deep_entity-classes INTO ls_class.
           LOOP AT ls_class-characteristics INTO ls_class_char.

             READ TABLE lt_cabn INTO ls_cabn WITH KEY atnam = ls_class_char-charact.
             IF sy-subrc <> 0.
*-- Format Message
*-- Append error message
               lv_msgv1 = ls_class_char-charact.
               CALL METHOD format_message
                EXPORTING
                   iv_message_type = 'E'
                   iv_message_num  = '003'
                   iv_msgv1        = lv_msgv1
                CHANGING
                   ct_return       = et_return.
             ELSE.
             CASE ls_cabn-atfor.
*-- If CHAR
               WHEN 'CHAR'.
                 ls_char-charact = ls_class_char-charact.
                 ls_char-value_char  = ls_class_char-Value.
                 APPEND ls_char TO lt_char.
                 CLEAR ls_char.

               WHEN 'NUM'.
*-- When NUM
                ls_num-charact          = ls_class_char-charact.
                ls_num-value_relation   = '1'.
                ls_num-value_from       = ls_class_char-value.
                APPEND ls_num TO lt_num.
                CLEAR ls_num.

               WHEN 'CURR'.
*-- When CURR
                ls_curr-charact          = ls_class_char-charact.
                ls_curr-value_relation   = '1'.
                ls_curr-value_from       = ls_class_char-value.
                APPEND ls_curr TO lt_curr.
                CLEAR ls_curr.


             ENDCASE.
             ENDIF.
             CLEAR : ls_class_char.
            ENDLOOP.



*-- Call BAPI to create Material Classification details
         CALL FUNCTION 'BAPI_OBJCL_CREATE'
           EXPORTING
             objectkeynew            = lv_objkey
             objecttablenew          = 'MARA'
             classnumnew             = ls_class-class
             classtypenew            = ls_deep_entity-class_Type
           TABLES
             allocvaluesnum          = lt_num
             allocvalueschar         = lt_char
             allocvaluescurr         = lt_curr
             return                  = lt_return.



          READ TABLE lt_return INTO ls_return WITH KEY type = 'E'.
          IF sy-subrc = 0.
            APPEND LINES OF lt_return TO et_return.

          ELSE.
*-- Call Commit to Update


              CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
              EXPORTING
                wait          = 'X'.


*-- Add Success
              lv_msgv1 = lv_matnr.
              lv_msgv2 = ls_class-class.
              CALL METHOD format_message
              EXPORTING
                iv_message_type = 'S'
                iv_message_num  = '002'
                iv_msgv1        = lv_msgv1
                iv_msgv2        = lv_msgv2
               CHANGING
                ct_return       = et_return.

           ENDIF.
           CLEAR: lt_char[],
                  lt_curr[],
                  lt_num[],
                  lt_return[],
                  ls_char.


         ENDLOOP.





  endmethod.
ENDCLASS.
