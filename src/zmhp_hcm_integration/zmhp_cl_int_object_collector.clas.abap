class ZMHP_CL_INT_OBJECT_COLLECTOR definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF s_object.
    TYPES bukrs TYPE bukrs.
    TYPES molga TYPE zmhp_int_objects-molga.
    TYPES int_run TYPE guid_32.
    TYPES object TYPE zmhp_int_objects-object.
    TYPES object_seqnr TYPE zmhp_int_objects-object_seqnr.
    TYPES folder TYPE zmhp_int_objects-folder.
    TYPES element_id_sap TYPE zmhp_int_objects-id_sap.
    TYPES element_id_cloud TYPE zmhp_int_objects-id_oracle.
    TYPES id_sap TYPE char15.
    TYPES id_cloud TYPE char15.
    TYPES pernr_cloud TYPE char15.
    TYPES xml_node TYPE REF TO if_ixml_node.
    TYPES object_handler TYPE REF TO zmhp_cl_int_object_handler.
    TYPES END OF s_object .
  types:
    t_objects TYPE TABLE OF s_object .
  types:
    t_obj_cust TYPE TABLE OF zmhp_int_objects WITH DEFAULT KEY .

  data OBJECT_CUSTOMIZING type T_OBJ_CUST .
  data OBJECTS type T_OBJECTS .
  data XML type ref to IF_IXML_DOCUMENT .
  data SEQUENCE type SEQNR .

  methods SET_FULL_XML
    importing
      !XML type ref to IF_IXML_DOCUMENT .
  methods READ_CUSTOMZING
    importing
      !MOLGA type MOLGA
      !OBJECT type OTYPE optional .
  methods READ_CUSTOMZING_OBJECTS
    importing
      !OBJECT type OTYPE optional
      !SEQUENCE type SEQNR optional
      !MOLGA type MOLGA .
  methods COLLECT_OBJECTS
    exporting
      !MESSAGE type BAPIRET1 .
  methods READ_COUNTRY_CODE
    importing
      !COUNTRY_CODE_PATH type STRING
      !CUSTOMIZING type ZMHP_INT_OBJECTS
      !NODE type ref to IF_IXML_NODE
    exporting
      !BUKRS type BUKRS
      !COUNTRY_CODE type MOLGA
      !MESSAGE type BAPIRET1 .
  methods READ_INTEGRATION_ID
    importing
      !INTEGRATION_ID_PATH type STRING
    exporting
      !MESSAGE type BAPIRET1
      !INTEGRATION_ID type GUID_32 .
  methods CONSTRUCTOR
    importing
      !SEQUENCE type SEQNR optional .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZMHP_CL_INT_OBJECT_COLLECTOR IMPLEMENTATION.


  METHOD collect_objects.
    DATA:lr_collection TYPE REF TO if_ixml_node_collection.
    DATA(processor) = NEW cl_xslt_processor( ).

    me->read_integration_id( EXPORTING integration_id_path = zmhp_cl_int_constants=>integration_run_id_path
                             IMPORTING message             = message
                                       integration_id      = DATA(int_id) ).
    IF int_id IS INITIAL.
      me->read_integration_id( EXPORTING integration_id_path = '//request_id'
                               IMPORTING message             = message
                                         integration_id      = int_id ).
    ENDIF.
*    SORT me->object_customizing ASCENDING BY molga.

    DATA(custom_objects) = me->object_customizing.
    SORT custom_objects BY object ASCENDING. DELETE ADJACENT DUPLICATES FROM custom_objects COMPARING object.

    LOOP AT custom_objects ASSIGNING FIELD-SYMBOL(<customizing>).
      processor->set_source_node( node = me->xml ).
      processor->set_expression( expression = CONV #( <customizing>-folder ) ).
      processor->run( progname = space ).

      lr_collection = processor->get_nodes( ).
      IF lr_collection IS BOUND.
        DATA(lr_iterator) = lr_collection->create_iterator( ).
        DATA(lo_node) = lr_iterator->get_next( ).

        WHILE lo_node IS BOUND.
          CLEAR message.

**JMB20211007 start insert - check for report specific seqnr
* Vor Auslieferung an DÜRR LÖSCHEN!
          DATA(country_code_path) = COND #( WHEN <customizing>-object = zmhp_cl_int_constants=>person
                                            THEN zmhp_cl_int_constants=>country_code_path
                                            ELSE zmhp_cl_int_constants=>country_code_path_dt ).
*                                            WHEN <customizing>-object = zmhp_cl_int_constants=>orgunit AND <customizing>-object_seqnr = '000'
*                                            THEN zmhp_cl_int_constants=>country_code_path_om
*                                            WHEN <customizing>-object = zmhp_cl_int_constants=>orgunit AND <customizing>-object_seqnr = '001'
*                                            THEN zmhp_cl_int_constants=>country_code_path_dt
*                                            ELSE '/Position_Record/AccountingArea' ).

          DATA(object_rpt) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>del_absence )
                                                 ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>del_attendance )
                                                 ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>balance ) ).
          DATA(person_number) = CONV string( '/PersonNumber' ).
          IF <customizing>-object IN object_rpt.
            country_code_path = zmhp_cl_int_constants=>country_code_path_rpt.
            person_number = '/PERSON_NUMBER'.
          ENDIF.
*JMB20211007 insert end

          "country code is needed for storage in IT_AEND
          me->read_country_code( EXPORTING customizing       = <customizing>
                                           country_code_path = country_code_path "'/Details/AccountingArea'
                                           node              = lo_node
                                 IMPORTING bukrs        = DATA(bukrs)
                                           country_code = DATA(molga)
                                           message      = message    ).
          "in case molga is empty go to next delivered object
          IF molga IS INITIAL OR message-type = zmhp_cl_int_constants=>error.
            FREE lo_node.
            lo_node = lr_iterator->get_next( ).
            CLEAR molga.
            CONTINUE.
          ELSE.

            "check for country specific customizing, fallback: '*'
            READ TABLE me->object_customizing WITH KEY molga = molga object = <customizing>-object TRANSPORTING NO FIELDS.
            IF sy-subrc IS NOT INITIAL.
              READ TABLE me->object_customizing WITH KEY molga = '*' object = <customizing>-object TRANSPORTING NO FIELDS.
            ENDIF.

            IF sy-subrc IS INITIAL.
              APPEND INITIAL LINE TO me->objects ASSIGNING FIELD-SYMBOL(<object>).
              <object> = CORRESPONDING #( <customizing> ).
              <object>-int_run        = int_id.
              <object>-bukrs          = bukrs.
              <object>-molga          = molga.

              "get SAP own ID of actual object
              <object>-element_id_sap = <customizing>-id_sap.
              zmhp_cl_int_xml=>get_xpath_element( EXPORTING element_xpath = CONV #( <object>-element_id_sap )
                                                            xml_node      = lo_node
                                                  IMPORTING value         = DATA(lv_value)
                                                            message       = message ).
              <object>-id_sap = lv_value.

              CHECK message-type NE zmhp_cl_int_constants=>error.
              "If the sap id has 10 characters it is not an sap id, happens only for object P and hire action
              "-> we need to set the sap id blank so pernr can be generated when booking
              IF strlen( lv_value ) > 8.
                CLEAR <object>-id_sap.
              ENDIF.
              CLEAR:lv_value.

              "get SAP own ID of actual object
              <object>-element_id_cloud = <customizing>-id_oracle.
              zmhp_cl_int_xml=>get_xpath_element( EXPORTING element_xpath = CONV #( <object>-element_id_cloud )
                                                            xml_node      = lo_node
                                                  IMPORTING value         = lv_value
                                                            message       = message ).
              <object>-id_cloud = lv_value.
              <object>-xml_node = lo_node.

              "get oracle pernr
              IF <object>-object = zmhp_cl_int_constants=>person.
                zmhp_cl_int_xml=>get_xpath_element( EXPORTING element_xpath = person_number
                                                xml_node      = lo_node
                                      IMPORTING value         = lv_value
                                                message       = message ).
                <object>-pernr_cloud = lv_value.

**JMB20210823 start insert - fallback in case SAP PERNR wasn´t provided
*
                IF <object>-pernr_cloud IS NOT INITIAL AND
                   <object>-id_sap      IS INITIAL.

                  SELECT SINGLE pernr INTO @DATA(pernr) FROM pa9400 WHERE oraclepernr EQ @lv_value AND
                                                                          begda       LE @sy-datum AND
                                                                          endda       GE @sy-datum.
                  IF <object>-id_sap NE pernr.
                    <object>-id_sap = pernr.
                    CLEAR: pernr, lv_value.
                  ENDIF.
                ENDIF.
*JMB20210823 end insert, start delete
*                IF sy-sysid = 'Q02'.
*                  SELECT SINGLE pernr INTO @DATA(pernr) FROM pa9400 WHERE oraclepernr = @lv_value.
*                  IF <object>-id_sap NE pernr.
*                    <object>-id_sap = pernr.
*                    CLEAR pernr.
*                  ENDIF.
*                ENDIF.
*JMB20210823 delete end
              ENDIF.
              FREE lo_node.

              "fallback in case SAP_ID wasn´t deliverd by the importing file (excluding Person objects)
              IF <object>-object NE zmhp_cl_int_constants=>person AND
                 <object>-id_sap IS INITIAL.
                zmhp_cl_int_infty_proc_xml=>get_sap_ids( EXPORTING otype    = <object>-object
                                                                   begda    = sy-datum
                                                                   endda    = sy-datum
                                                                   cloud_id = CONV #( <object>-id_cloud )
                                                         IMPORTING sap_id   = DATA(sap_id)
                                                                    message = message ).
                <object>-id_sap = sap_id.
              ENDIF.
            ENDIF.

            "get next object
            lo_node = lr_iterator->get_next( ).
            IF lo_node IS BOUND.
              processor->set_source_node( node = lo_node ).
            ENDIF.
          ENDIF.
          CLEAR: molga, bukrs.
        ENDWHILE.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


METHOD constructor.
  me->sequence = sequence.
ENDMETHOD.


  METHOD read_country_code.
*   Evaluate the xPath for country (PA and OM)
    zmhp_cl_int_xml=>get_xpath_element(
      EXPORTING
        element_xpath = country_code_path
        xml_node      = node
*        xml_node      = me->xml
      IMPORTING
        value         = DATA(value)
        message       = message ).

    bukrs = value.
    IF customizing-object = zmhp_cl_int_constants=>person.
      SELECT SINGLE molga FROM t500l INTO @country_code WHERE intca EQ @value.
      IF sy-subrc IS NOT INITIAL.
        zmhp_cl_int_xml=>get_xpath_element(
         EXPORTING
           element_xpath = '//CompanyCode'
           xml_node      = node
*        xml_node      = me->xml
         IMPORTING
           value         = value
           message       = message ).
        IF value IS INITIAL.
          zmhp_cl_int_xml=>get_xpath_element(
          EXPORTING
            element_xpath = '//CancelledHireData/CompanyCode'
            xml_node      = node
*        xml_node      = me->xml
          IMPORTING
            value         = value
            message       = message ).
        ENDIF.
        SELECT SINGLE land1 FROM t500p WHERE bukrs = @value INTO @DATA(country).
        IF sy-subrc IS INITIAL.
          SELECT SINGLE molga FROM t500l INTO @country_code WHERE intca EQ @country.
        ENDIF.
      ENDIF.
      IF country_code IS INITIAL.
*       Evaluate the xPath for IDs (PA)
        zmhp_cl_int_xml=>get_xpath_element(
          EXPORTING
            element_xpath = CONV #( customizing-id_sap )
            xml_node      = node
          IMPORTING
            value         = DATA(sap_id)
            message       = message ).
        zmhp_cl_int_xml=>get_xpath_element(
          EXPORTING
            element_xpath = CONV #( customizing-id_oracle )
            xml_node      = node
          IMPORTING
            value         = DATA(cloud_id)
            message       = message ).
*       Add message to returning parameter
        message = VALUE bapiret1( type = zmhp_cl_int_constants=>error
                                  id = zmhp_cl_int_constants=>msg_class_int
                                  number = zmhp_cl_int_constants=>msg_no-m7
                                  message_v1 = customizing-object
                                  message_v2 = sap_id
                                  message_v3 = cloud_id
*                                  message_v4 =
                                ).
      ENDIF.
      IF value IS INITIAL.
        zmhp_cl_int_xml=>get_xpath_element(
          EXPORTING
            element_xpath = zmhp_cl_int_constants=>company_code_path
            xml_node      = node
*        xml_node      = me->xml
          IMPORTING
            value         = value
            message       = message ).
      ENDIF.

      bukrs = value.
    ELSE.
      IF customizing-object = zmhp_cl_int_constants=>position.
        value = '010-108232'.
      ENDIF.
      "TO BE ADDED: LOGIC DUERR/HOMAG - HOMAG 3 and DUERR 4 LETTERS BUKRS
      bukrs = bukrs+0(3).
      SELECT SINGLE land1 FROM t001 WHERE bukrs = @value+0(3) INTO @DATA(land1).
      IF sy-subrc IS INITIAL.
        SELECT SINGLE molga FROM t500l INTO @country_code WHERE intca EQ @land1.
      ELSE.
*       Evaluate the xPath for IDs (OM)
        zmhp_cl_int_xml=>get_xpath_element(
          EXPORTING
            element_xpath = CONV #( customizing-id_sap )
            xml_node      = me->xml
          IMPORTING
            value         = sap_id
            message       = message ).
        zmhp_cl_int_xml=>get_xpath_element(
          EXPORTING
            element_xpath = CONV #( customizing-id_oracle )
            xml_node      = me->xml
          IMPORTING
            value         = cloud_id
            message       = message ).
*       Add message to returning parameter
        message = VALUE bapiret1( type = zmhp_cl_int_constants=>error
                                  id = zmhp_cl_int_constants=>msg_class_int
                                  number = zmhp_cl_int_constants=>msg_no-m7
                                  message_v1 = customizing-object
                                  message_v2 = sap_id
                                  message_v3 = cloud_id
*                                  message_v4 =
                                ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD READ_CUSTOMZING.
    IF object IS NOT INITIAL."Get Objects
      SELECT * FROM zmhp_int_objects INTO TABLE me->object_customizing WHERE molga = molga AND object = object.
    ELSE.
      SELECT * FROM zmhp_int_objects INTO TABLE me->object_customizing WHERE molga = molga.
    ENDIF.
  ENDMETHOD.


  METHOD read_customzing_objects.
*    IF object IS NOT INITIAL."Get Objects
*      IF scenario = abap_true.
    SELECT * FROM zmhp_int_objects INTO TABLE me->object_customizing WHERE molga = molga and object = object AND object_seqnr = sequence.
*      ELSE.
*        SELECT * FROM zmhp_int_objects INTO TABLE me->object_customizing WHERE object = object.
*      ENDIF.
*    ELSE.
*      SELECT * FROM zmhp_int_objects INTO TABLE me->object_customizing .
*    ENDIF.
  ENDMETHOD.


  METHOD READ_INTEGRATION_ID.
    DATA:
      processor  TYPE REF TO cl_xslt_processor,
      collection TYPE REF TO if_ixml_node_collection.
    CREATE OBJECT processor.
    processor->set_source_node( node = me->xml ).
    processor->set_expression( expression = CONV #( integration_id_path ) ).
    processor->run( progname = space ).
    collection = processor->get_nodes( ).
    IF collection IS BOUND.
      DATA(lr_iterator) = collection->create_iterator( ).
      DATA(lo_node) = lr_iterator->get_next( ).
      IF lo_node IS BOUND.
        integration_id = lo_node->get_value( ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD SET_FULL_XML.
    me->xml = xml.
  ENDMETHOD.
ENDCLASS.
