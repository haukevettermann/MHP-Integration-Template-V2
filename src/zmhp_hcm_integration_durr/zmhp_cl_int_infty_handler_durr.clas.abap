class ZMHP_CL_INT_INFTY_HANDLER_DURR definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF ty_field,
        infty     TYPE infty,
        subty     TYPE subty,
        field     TYPE char50,
        field_old TYPE char50,
        field_new TYPE char50,
        field4    TYPE char50,
      END OF ty_field .
  types:
    ty_fields TYPE TABLE OF ty_field .

  types:
    BEGIN OF s_time_slices .
        INCLUDE TYPE hrperiods.
        TYPES seqnr TYPE seqnr.
    TYPES folder TYPE zmhp_dd_folder.
    TYPES parent_folder TYPE zmhp_dd_folder.
    TYPES parent_folder_seqnr TYPE seqnr.
    TYPES END OF s_time_slices .
  types:
    t_time_slices TYPE TABLE OF s_time_slices .
  types:
    BEGIN OF s_folder.
    TYPES folder TYPE zmhp_dd_folder.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES node TYPE REF TO if_ixml_node.
    TYPES cust_seqnr TYPE seqnr.
    TYPES seqnr TYPE seqnr.
    TYPES ignore_timeslices type boole_d.
    TYPES parent_folder TYPE zmhp_dd_folder.
    TYPES parent_folder_seqnr TYPE seqnr.
    TYPES END OF s_folder .
  types:
    t_folders TYPE TABLE OF s_folder WITH DEFAULT KEY .
  types T_DATA type ref to DATA .
  types:
    BEGIN OF s_field.
    TYPES infty TYPE infty.
    TYPES field_sap TYPE zmhp_dd_field.
    TYPES field_oracle TYPE zmhp_dd_field.
    TYPES begda TYPE begda.
    TYPES endda TYPE endda.
    TYPES value TYPE zmhp_dd_value.
    TYPES value_mapped TYPE zmhp_dd_value.
    TYPES value_converted TYPE zmhp_dd_value.
    TYPES folder TYPE zmhp_dd_folder.
    TYPES cust_seqnr TYPE seqnr.
    TYPES seqnr TYPE seqnr.
    TYPES parent_folder TYPE zmhp_dd_folder.
    TYPES parent_folder_seqnr TYPE seqnr.
    TYPES END OF s_field .
  types:
    t_fields TYPE TABLE OF s_field WITH KEY infty field_sap field_oracle begda endda .
  types:
    BEGIN OF s_folders_aggregated.
    TYPES id TYPE seqnr.
    TYPES folders TYPE t_folders.
    TYPES fields TYPE t_fields.
    TYPES END OF s_folders_aggregated .
  types:
    t_folders_aggregated TYPE TABLE OF s_folders_aggregated WITH DEFAULT KEY .

  data XML_NODE type ref to IF_IXML_NODE .
  data FOLDERS type T_FOLDERS .
  data FOLDERS_AGGREGATED type T_FOLDERS_AGGREGATED .
  data FIELDS type T_FIELDS .
  data TIME_SLICES type T_TIME_SLICES .
  data INFTY type INFTY .
  data SEQNR type SEQNR .
  data ACTION type MASSN .
  data CUSTOMIZING type ref to ZMHP_CL_INT_CUSTOMIZING_XML .
  class-data GO_INSTANCE type ref to ZMHP_CL_INT_INFTY_PROC_XML .
  data INFOTYPE_CHANGES type ref to ZMHP_CL_INT_IT_AEND .
  data OBJECT_HANDLER type ref to ZMHP_CL_INT_OBJECT_HANDLER .
  data INT_RUN type GUID_32 .
  data MOLGA type MOLGA .
  data CUSTOMIZING_FOLDERS type ZMHP_CL_INT_CUSTOMIZING_XML=>T_CUSTOMIZING_FOLDERS .
  data HAS_MULTIPLE type BOOLE_D .
  data HAS_ERROR type BOOLE_D .
  class-data INFTY_OPERATION type ref to ZMHP_CL_INT_IT_OPERATION .

  class-methods KOPIERVORLAGE
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type ZMHP_VALUE
      !CLOUD_ID type ZMHP_VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to ZMHP_CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type ZMHP_INT_IT_AEND .
  class-methods PROCESS_0000
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type ZMHP_VALUE
      !CLOUD_ID type ZMHP_VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to ZMHP_CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type ZMHP_INT_IT_AEND .
  class-methods PROCESS_0008
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      value(SAP_ID) type ZMHP_VALUE
      !CLOUD_ID type ZMHP_VALUE
      !AEND_ID type GUID_32
      !HAS_ERROR type BOOLE_D
      !FIELDS type T_FIELDS
      !INFOTYPE_XML_PROCESSOR type ref to ZMHP_CL_INT_INFTY_PROC_XML
      value(DATA) type ANY
    exporting
      !IT_AEND type ZMHP_INT_IT_AEND .
  class-methods PROCESS_IT_SPECIFICS_AFTER_OP
    importing
      !SIMU type BOOLE_D
      !MASSN type MASSN
      !MOLGA type MOLGA
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !INFTY type INFTY
      !SUBTY type SUBTY
      !CHANGED_FIELDS type TY_FIELDS
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB
    changing
      !RECORD type ANY .
  class-methods PROCESS_IT_SPECIFICS_BEFORE_OP
    importing
      !SIMU type BOOLE_D
      !MASSN type MASSN
      !MOLGA type MOLGA
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !INFTY type INFTY
      !SUBTY type SUBTY
      !CHANGED_FIELDS type TY_FIELDS
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB
    changing
      !RECORD type ANY .
  class-methods PROCESS_0001_AFTER
    importing
      !SIMU type BOOLE_D
      !MASSN type MASSN
      !MOLGA type MOLGA
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !INFTY type INFTY
      !CHANGED_FIELDS type TY_FIELDS
      !SUBTY type SUBTY
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB
    changing
      !RECORD type ANY .
  class-methods PROCESS_0001_BEFORE
    importing
      !SIMU type BOOLE_D
      !MASSN type MASSN
      !MOLGA type MOLGA
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !INFTY type INFTY
      !CHANGED_FIELDS type TY_FIELDS
      !SUBTY type SUBTY
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB
    changing
      !RECORD type ANY .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZMHP_CL_INT_INFTY_HANDLER_DURR IMPLEMENTATION.


  METHOD KOPIERVORLAGE.
    DATA:
      classdescr        TYPE REF TO cl_abap_classdescr,
      type_descr        TYPE REF TO cl_abap_typedescr,
      custom_classdescr TYPE REF TO cl_abap_classdescr.
*   Check if molga specific method is available
    DATA(infotype_handler) = zmhp_cl_int_general_settings=>get_infotyphandler( ).
*   Get exception class description
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = infotype_handler
      RECEIVING
        p_descr_ref    = type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    custom_classdescr ?= type_descr.
*    classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMHP_CL_INT_INFTY_HANDLER' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE custom_classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Infty spefic method availlable
      CALL METHOD infotype_xml_processor->(method)
        EXPORTING
          begda     = begda
          endda     = endda
          sap_id    = sap_id
          cloud_id  = cloud_id
          aend_id   = aend_id
          has_error = has_error
          fields    = fields
          data      = data
        IMPORTING
          it_aend   = it_aend.
    ELSE." Logic if not infty specific.
      DATA: prelp_orig TYPE prelp,
            endda_term TYPE dats.
      CLEAR: endda_term.
      ASSIGN data TO FIELD-SYMBOL(<structure>).
*     Check action and set instance attribute.
      IF infotype_xml_processor->infty = zmhp_cl_int_constants=>it0000.
        READ TABLE fields WITH KEY field_sap = zmhp_cl_int_constants=>massn ASSIGNING FIELD-SYMBOL(<action>).

        "SEW specific Hire and Termination handled in customizing via infotype 0000 seqnr 001. Therefore ignore for seqnr 000
        IF infotype_xml_processor->seqnr = 000 AND <action>-value IN zmhp_cl_int_constants=>non_relevant_action( ).
*          EXIT.
        ENDIF.

        ASSIGN COMPONENT zmhp_cl_int_constants=>massn OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
        IF <value> = 'RE'.
          EXIT.
        ENDIF.

*        CHECK <value> NOT IN zmhp_cl_int_constants=>non_relevant_action( ).

*     Set action for IT0000
        infotype_xml_processor->action = <value>.
        IF <value> IS NOT INITIAL AND <value> NE zmhp_cl_int_constants=>hire_date_change.
          IF infotype_xml_processor->object_handler->action NE zmhp_cl_int_constants=>termination.
            infotype_xml_processor->object_handler->action = <value>.
          ENDIF.
*      Set termination date for delimiting
          IF infotype_xml_processor->object_handler->action = zmhp_cl_int_constants=>termination.
            ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
            endda_term = <value>.
*          IF <value> = zmhp_cl_int_constants=>highdate.
            ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <structure> TO <value>.
            infotype_xml_processor->object_handler->delimit_date = endda_term + 1.
*          ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*   Rehire special logic
      IF infotype_xml_processor->infty NE zmhp_cl_int_constants=>it0000 AND infotype_xml_processor->object_handler->action = '04'.
        ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
        IF <value> NE zmhp_cl_int_constants=>highdate.
          CLEAR it_aend.
          EXIT.
        ENDIF.
      ENDIF.
      "Assign pernr
      ASSIGN COMPONENT zmhp_cl_int_constants=>pernr OF STRUCTURE <structure> TO <value>.
      <value> = sap_id.

      "Transfer data to prelp
      infotype_xml_processor->pa_to_prelp( EXPORTING infotype = <structure>
                                 aend_id  = aend_id
                       IMPORTING prelp    = DATA(prelp) ).
      cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <structure>
                                             IMPORTING prelp = prelp_orig ).


      it_aend = CORRESPONDING zmhp_int_it_aend( prelp_orig ).
      it_aend-aend_id = aend_id.

      "Set action for infotypes other then IT0000
      it_aend-action = infotype_xml_processor->action.
      "Check for active status.
      READ TABLE fields WITH KEY field_oracle = 'AssStatusType' ASSIGNING FIELD-SYMBOL(<status>).
      IF <status> IS ASSIGNED AND <status>-value CS 'INACTIVE'.
        it_aend-active = 'I'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD process_0000.
    DATA:
      classdescr        TYPE REF TO cl_abap_classdescr,
      type_descr        TYPE REF TO cl_abap_typedescr,
      custom_classdescr TYPE REF TO cl_abap_classdescr.
*   Check if molga specific method is available
    DATA(infotype_handler) = zmhp_cl_int_general_settings=>get_infotyphandler( ).
*   Get exception class description
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = infotype_handler
      RECEIVING
        p_descr_ref    = type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    custom_classdescr ?= type_descr.
*    classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMHP_CL_INT_INFTY_HANDLER' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE custom_classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Infty spefic method availlable
      CALL METHOD infotype_xml_processor->(method)
        EXPORTING
          begda     = begda
          endda     = endda
          sap_id    = sap_id
          cloud_id  = cloud_id
          aend_id   = aend_id
          has_error = has_error
          fields    = fields
          data      = data
        IMPORTING
          it_aend   = it_aend.
    ELSE." Logic if not infty specific.
      DATA: prelp_orig TYPE prelp,
            endda_term TYPE dats.
      CLEAR: endda_term.
      ASSIGN data TO FIELD-SYMBOL(<structure>).
*     Check action and set instance attribute.
      IF infotype_xml_processor->infty = zmhp_cl_int_constants=>it0000.
        READ TABLE fields WITH KEY field_sap = zmhp_cl_int_constants=>massn ASSIGNING FIELD-SYMBOL(<action>).

        "DÜRR specific Hire and Termination handled in customizing via infotype 0000 seqnr 001. Therefore ignore for seqnr 000
        IF ( <action> IS ASSIGNED AND infotype_xml_processor->seqnr = 000 AND <action>-value IN zmhp_cl_int_constants=>non_relevant_action( ) ) or <action> is not assigned.
          EXIT.
        ENDIF.

        ASSIGN COMPONENT zmhp_cl_int_constants=>massn OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
        IF <value> = 'RE'.
          EXIT.
        ENDIF.

*        CHECK <value> NOT IN zmhp_cl_int_constants=>non_relevant_action( ).

*     Set action for IT0000
        infotype_xml_processor->action = <value>.
        IF <value> IS NOT INITIAL AND <value> NE zmhp_cl_int_constants=>hire_date_change.
          IF infotype_xml_processor->object_handler->action NE zmhp_cl_int_constants=>termination.
            infotype_xml_processor->object_handler->action = <value>.
          ENDIF.
*      Set termination date for delimiting
          IF infotype_xml_processor->object_handler->action = zmhp_cl_int_constants=>termination.
            ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
            endda_term = <value>.
*          IF <value> = zmhp_cl_int_constants=>highdate.
            ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <structure> TO <value>.
            infotype_xml_processor->object_handler->delimit_date = endda_term + 1.
*          ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
*   Rehire special logic
      IF infotype_xml_processor->infty NE zmhp_cl_int_constants=>it0000 AND infotype_xml_processor->object_handler->action = '04'.
        ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <structure> TO <value>.
        IF <value> NE zmhp_cl_int_constants=>highdate.
          CLEAR it_aend.
          EXIT.
        ENDIF.
      ENDIF.
      "Assign pernr
      ASSIGN COMPONENT zmhp_cl_int_constants=>pernr OF STRUCTURE <structure> TO <value>.
      <value> = sap_id.

      "Transfer data to prelp
      infotype_xml_processor->pa_to_prelp( EXPORTING infotype = <structure>
                                 aend_id  = aend_id
                       IMPORTING prelp    = DATA(prelp) ).
      cl_hr_pnnnn_type_cast=>pnnnn_to_prelp( EXPORTING pnnnn = <structure>
                                             IMPORTING prelp = prelp_orig ).


      it_aend = CORRESPONDING zmhp_int_it_aend( prelp_orig ).
      it_aend-aend_id = aend_id.

      "Set action for infotypes other then IT0000
      it_aend-action = infotype_xml_processor->action.
      "Check for active status.
      READ TABLE fields WITH KEY field_oracle = 'AssStatusType' ASSIGNING FIELD-SYMBOL(<status>).
      IF <status> IS ASSIGNED AND <status>-value CS 'INACTIVE'.
        it_aend-active = 'I'.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_0001_AFTER.
*    DATA: hrp_old_it     TYPE REF TO data,
*          lr_structdescr TYPE REF TO cl_abap_structdescr,
*          error_tab      TYPE hrpad_return_tab,
*          ret            TYPE hrpad_return,
*          delimit_date   TYPE dats,
*          child          TYPE objec,
*          parent         TYPE objec,
*          pos_id         TYPE hrobjid.
*
*    FIELD-SYMBOLS:  <hrp_old>     TYPE any.
*
*    ASSIGN COMPONENT zmhp_cl_int_constants=>plans OF STRUCTURE record TO FIELD-SYMBOL(<pos>).
*    ASSIGN COMPONENT zmhp_cl_int_constants=>orgeh OF STRUCTURE record TO FIELD-SYMBOL(<orgeh>).
*    CONCATENATE 'A' '008' INTO DATA(relat_holder).
*    CONCATENATE 'A' '011' INTO DATA(relat_cc).
*    CONCATENATE 'A' '003' INTO DATA(relat_pos_org).
*    CLEAR: pos_id.
*    "Get language key for molga
*    zmhp_cl_int_utility=>get_spras_by_molga( EXPORTING molga = molga
*                                             IMPORTING spras = DATA(spras) langu = DATA(langu) ).
*
*    IF simu = abap_true.
**      DATA(begda_ext) = zmhp_cl_int_utility=>get_external_date( date = <begda> ).
**      DATA(endda_ext) = zmhp_cl_int_utility=>get_external_date( date = <endda> ).
**      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
**      return = VALUE bapiret1( type = zmhp_cl_int_constants=>warning
**                                 id = zmhp_cl_int_constants=>msg_class_int
**                                 number = zmhp_cl_int_constants=>msg_no-m26
**                                 message_v1 = <objid>
**                                 message_v2 = infty
**                                 message_v3 = date
***                                 message_v4 = date
**).
*    ELSE.
*      "Check changed fields and perform operation which are specific to field changes
*      "Loop at changed fields of IT0001
*      LOOP AT changed_fields ASSIGNING FIELD-SYMBOL(<field>).
*        "If field orgeh changed we need to create new dummy position and relate it to new orgunit
*        IF <field>-field = zmhp_cl_int_constants=>orgeh.
**          DATA(infty_operation) = NEW zmhp_cl_int_it_operation( pernr = pernr molga = molga ).
*
**--------------------------------------------------------------------*
**   Decision was made that no new dummy position will be created when an employee has a change of org unit
**   The position should be moved to the new org unit so there will be no trouble with manager assignments at org level
**--------------------------------------------------------------------*
*          "delimit relation of position to old orgunit
*
*          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && zmhp_cl_int_constants=>it1001 ) ).
*          CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
*          ASSIGN hrp_old_it->* TO <hrp_old>.
*
*          zmhp_cl_int_infotypes=>infty_operation->read_hrpxxxx(
*          EXPORTING
*            begda = begda
*            endda = endda
*            infty = CONV #( zmhp_cl_int_constants=>it1001 )
*            langu = CONV #( spras )
*            otype = zmhp_cl_int_constants=>position
*            plvar = CONV #( zmhp_cl_int_constants=>plvar )
*            relat = '003'
*            rsign = 'A'
*            sap_id = <pos>
*            subty = CONV #( relat_pos_org )
*            IMPORTING
*              hrp_old = <hrp_old>
*              return = return
*                                  ).
*          ret = CORRESPONDING #( return ).
*          APPEND ret TO return_tab.
*          CLEAR: ret, return.
*
*          delimit_date = begda - 1.
*          zmhp_cl_int_infotypes=>infty_operation->delimit_hrpxxxx(
*          EXPORTING
*            record       = <hrp_old>
*            begda        = begda
*            endda        = endda
*            infty        = CONV #( zmhp_cl_int_constants=>it1001 )
*            delimit_date = delimit_date
*            IMPORTING
*              return     = return
*                              ).
*          ret = CORRESPONDING #( return ).
*          APPEND ret TO return_tab.
*
*          "create new relation between position and new org unit
*          IF return-type NE zmhp_cl_int_constants=>error.
*            CLEAR: ret, return.
*            "Build child
*            child-plvar = zmhp_cl_int_constants=>plvar.
*            child-endda = endda.
*            child-begda = begda.
*            child-objid = <field>-field_new.
*            child-otype = zmhp_cl_int_constants=>orgunit.
*            "Build parent
*            parent-plvar = zmhp_cl_int_constants=>plvar.
*            parent-endda = endda.
*            parent-begda = begda.
*            parent-objid = <pos>.
*            parent-otype = zmhp_cl_int_constants=>position.
*            zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
*              EXPORTING
*                 action = CONV #( zmhp_cl_int_constants=>insert_rel )
*                 begda = begda
*                 endda = endda
*                 relat = '003'
*                 rsign = 'A'
*                 parent = CONV #( parent ) "CONV #( orgeh )
*                 child = CONV #( child ) "CONV #( <pos_line> )
*                 spras = spras
*                 simu = simu
*                 IMPORTING
*                   return = return
*                     ).
*            IF return-type NE 'E'.
*              CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*              COMMIT WORK AND WAIT.
*            ENDIF.
*            ret = CORRESPONDING #( return ).
*            APPEND ret TO return_tab.
*            CLEAR: ret, return.
*          ENDIF.
*        ENDIF.
*      ENDLOOP.
*    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_0001_BEFORE.
*
*    DATA: hrp_old_it     TYPE REF TO data,
*          lr_structdescr TYPE REF TO cl_abap_structdescr,
*          error_tab      TYPE hrpad_return_tab,
*          ret            TYPE hrpad_return,
*          delimit_date   TYPE dats,
*          child          TYPE objec,
*          parent         TYPE objec,
*          pos_id         TYPE hrobjid.
*
*    FIELD-SYMBOLS:  <hrp_old>     TYPE any.
*
*    ASSIGN COMPONENT zmhp_cl_int_constants=>plans OF STRUCTURE record TO FIELD-SYMBOL(<pos>).
*    ASSIGN COMPONENT zmhp_cl_int_constants=>orgeh OF STRUCTURE record TO FIELD-SYMBOL(<orgeh>).
*    CONCATENATE 'A' '008' INTO DATA(relat_holder).
*    CONCATENATE 'A' '011' INTO DATA(relat_cc).
*    CONCATENATE 'A' '003' INTO DATA(relat_pos_org).
*    CLEAR: pos_id.
*    "Get language key for molga
*    zmhp_cl_int_utility=>get_spras_by_molga( EXPORTING molga = molga
*                                             IMPORTING spras = DATA(spras) langu = DATA(langu) ).
*
*    IF simu = abap_true.
**      DATA(begda_ext) = zmhp_cl_int_utility=>get_external_date( date = <begda> ).
**      DATA(endda_ext) = zmhp_cl_int_utility=>get_external_date( date = <endda> ).
**      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
**      return = VALUE bapiret1( type = zmhp_cl_int_constants=>warning
**                                 id = zmhp_cl_int_constants=>msg_class_int
**                                 number = zmhp_cl_int_constants=>msg_no-m26
**                                 message_v1 = <objid>
**                                 message_v2 = infty
**                                 message_v3 = date
***                                 message_v4 = date
**).
*    ELSE.
*
*      "If termination we need to delimit A008 relation from employee to position
*      IF massn = zmhp_cl_int_constants=>termination.
*        IF endda = zmhp_cl_int_constants=>highdate.
*          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && zmhp_cl_int_constants=>it1001 ) ).
*          CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
*          ASSIGN hrp_old_it->* TO <hrp_old>.
*
*          "Read old relation
*          zmhp_cl_int_infotypes=>infty_operation->read_hrpxxxx(
*          EXPORTING
*            begda = begda
*            endda = endda
*            infty = CONV #( zmhp_cl_int_constants=>it1001 )
*            langu = CONV #( spras )
*            otype = zmhp_cl_int_constants=>position
*            plvar = CONV #( zmhp_cl_int_constants=>plvar )
*            relat = '008'
*            rsign = 'A'
*            sap_id = <pos>
*            subty = CONV #( relat_holder )
*            IMPORTING
*              hrp_old = <hrp_old>
*              return = return
*                                  ).
*          ret = CORRESPONDING #( return ).
*          APPEND ret TO return_tab.
*          CLEAR: ret, return.
*
*          "Delimit old relation
*          delimit_date = begda - 1.
*          zmhp_cl_int_infotypes=>infty_operation->delimit_hrpxxxx(
*          EXPORTING
*            record       = <hrp_old>
*            begda        = begda
*            endda        = endda
*            infty        = CONV #( zmhp_cl_int_constants=>it1001 )
*            delimit_date = delimit_date
*            IMPORTING
*              return     = return
*                              ).
*          IF return-type NE zmhp_cl_int_constants=>error.
*            "Pernr got unlocked during delimit
*            CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
*              EXPORTING
*                number = pernr
*              IMPORTING
*                return = return.
*          ENDIF.
*          ret = CORRESPONDING #( return ).
*          APPEND ret TO return_tab.
*
*          "Set Default position
*          <pos> = '99999999'.
*        ENDIF.
*      ELSEIF massn = zmhp_cl_int_constants=>rehire.
*        SELECT SINGLE * FROM pa0000 INTO @DATA(p0000_line) WHERE massn = @zmhp_cl_int_constants=>rehire AND pernr = @pernr AND endda GE @begda AND begda LE @endda.
*        IF sy-subrc <> 0.
*          "When rehire create new dummy position and relate to orgunit
*          pos_id = zmhp_cl_int_infotypes=>infty_operation->create_dummy_pos( orgeh = CONV #( <orgeh> ) begda = begda endda = endda langu = CONV #( spras ) simu = simu ).
*          "Set new position id to record
*          IF pos_id IS NOT INITIAL.
*            <pos> = pos_id.
*
*            "Build child
*            child-plvar = zmhp_cl_int_constants=>plvar.
*            child-endda = endda.
*            child-begda = begda.
*            child-objid = <orgeh>.
*            child-otype = zmhp_cl_int_constants=>orgunit.
*            "Build parent
*            parent-plvar = zmhp_cl_int_constants=>plvar.
*            parent-endda = endda.
*            parent-begda = begda.
*            parent-objid = <pos>.
*            parent-otype = zmhp_cl_int_constants=>position.
*            zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
*              EXPORTING
*                 action = CONV #( zmhp_cl_int_constants=>insert_rel )
*                 begda = begda
*                 endda = endda
*                 relat = '003'
*                 rsign = 'A'
*                 parent = CONV #( parent ) "CONV #( orgeh )
*                 child = CONV #( child ) "CONV #( <pos_line> )
*                 spras = spras
*                 simu = simu
*                 IMPORTING
*                   return = return
*                     ).
*            IF return-type NE 'E'.
*              "Relate position with employee
*              "Build child
*              child-plvar = zmhp_cl_int_constants=>plvar.
*              child-endda = endda.
*              child-begda = begda.
*              child-objid = pernr.
*              child-otype = zmhp_cl_int_constants=>person.
*              "Build parent
*              parent-plvar = zmhp_cl_int_constants=>plvar.
*              parent-endda = endda.
*              parent-begda = begda.
*              parent-objid = <pos>.
*              parent-otype = zmhp_cl_int_constants=>position.
*              zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
*                EXPORTING
*                   action = CONV #( zmhp_cl_int_constants=>insert_rel )
*                   begda = begda
*                   endda = endda
*                   relat = '008'
*                   rsign = 'A'
*                   parent = CONV #( parent ) "CONV #( orgeh )
*                   child = CONV #( child ) "CONV #( <pos_line> )
*                   spras = spras
*                   simu = simu
*                   IMPORTING
*                     return = return
*                       ).
*              IF return-type NE 'E'.
*                CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*                COMMIT WORK AND WAIT.
*              ENDIF.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ELSEIF massn NE zmhp_cl_int_constants=>termination AND massn NE zmhp_cl_int_constants=>rehire.
*        "If current position is defaulted - add new position and relate to orgunit
*        IF <pos> = '99999999'.
*          pos_id = zmhp_cl_int_infotypes=>infty_operation->create_dummy_pos( orgeh = CONV #( <orgeh> ) begda = begda endda = endda langu = CONV #( spras ) simu = simu ).
*          "Set new position id to record
*          IF pos_id IS NOT INITIAL.
*            <pos> = pos_id.
*
*            "Build child
*            child-plvar = zmhp_cl_int_constants=>plvar.
*            child-endda = endda.
*            child-begda = begda.
*            child-objid = <orgeh>.
*            child-otype = zmhp_cl_int_constants=>orgunit.
*            "Build parent
*            parent-plvar = zmhp_cl_int_constants=>plvar.
*            parent-endda = endda.
*            parent-begda = begda.
*            parent-objid = <pos>.
*            parent-otype = zmhp_cl_int_constants=>position.
*            zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
*              EXPORTING
*                 action = CONV #( zmhp_cl_int_constants=>insert_rel )
*                 begda = begda
*                 endda = endda
*                 relat = '003'
*                 rsign = 'A'
*                 parent = CONV #( parent ) "CONV #( orgeh )
*                 child = CONV #( child ) "CONV #( <pos_line> )
*                 spras = spras
*                 simu = simu
*                 IMPORTING
*                   return = return
*                     ).
*            IF return-type NE 'E'.
*              "Relate position with employee
*              "Build child
*              child-plvar = zmhp_cl_int_constants=>plvar.
*              child-endda = endda.
*              child-begda = begda.
*              child-objid = pernr.
*              child-otype = zmhp_cl_int_constants=>person.
*              "Build parent
*              parent-plvar = zmhp_cl_int_constants=>plvar.
*              parent-endda = endda.
*              parent-begda = begda.
*              parent-objid = <pos>.
*              parent-otype = zmhp_cl_int_constants=>position.
*              zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
*                EXPORTING
*                   action = CONV #( zmhp_cl_int_constants=>insert_rel )
*                   begda = begda
*                   endda = endda
*                   relat = '008'
*                   rsign = 'A'
*                   parent = CONV #( parent ) "CONV #( orgeh )
*                   child = CONV #( child ) "CONV #( <pos_line> )
*                   spras = spras
*                   simu = simu
*                   IMPORTING
*                     return = return
*                       ).
*              CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*              COMMIT WORK AND WAIT.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*
*
*      "Check changed fields and perform operation which are specific to field changes
*      "Loop at changed fields of IT0001
*      LOOP AT changed_fields ASSIGNING FIELD-SYMBOL(<field>).
*        "If field orgeh changed we need to create new dummy position and relate it to new orgunit
*        IF <field>-field = zmhp_cl_int_constants=>orgeh.
**          DATA(infty_operation) = NEW zmhp_cl_int_it_operation( pernr = pernr molga = molga ).
*
**--------------------------------------------------------------------*
**   Decision was made that no new dummy position will be created when an employee has a change of org unit
**   The position should be moved to the new org unit so there will be no trouble with manager assignments at org level
**--------------------------------------------------------------------*
*          "delimit relation of position to old orgunit
*
*          lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && zmhp_cl_int_constants=>it1001 ) ).
*          CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
*          ASSIGN hrp_old_it->* TO <hrp_old>.
*
*          zmhp_cl_int_infotypes=>infty_operation->read_hrpxxxx(
*          EXPORTING
*            begda = begda
*            endda = endda
*            infty = CONV #( zmhp_cl_int_constants=>it1001 )
*            langu = CONV #( spras )
*            otype = zmhp_cl_int_constants=>position
*            plvar = CONV #( zmhp_cl_int_constants=>plvar )
*            relat = '003'
*            rsign = 'A'
*            sap_id = <pos>
*            subty = CONV #( relat_pos_org )
*            IMPORTING
*              hrp_old = <hrp_old>
*              return = return
*                                  ).
*          ret = CORRESPONDING #( return ).
*          APPEND ret TO return_tab.
*          CLEAR: ret, return.
*
**          DATA(global_hr) = ' '.
**          SET PARAMETER ID 'GLOB' FIELD global_hr.
*
*          delimit_date = begda - 1.
*          zmhp_cl_int_infotypes=>infty_operation->delimit_hrpxxxx(
*          EXPORTING
*            record       = <hrp_old>
*            begda        = begda
*            endda        = endda
*            infty        = CONV #( zmhp_cl_int_constants=>it1001 )
*            delimit_date = delimit_date
*            IMPORTING
*              return     = return
*                              ).
*          ret = CORRESPONDING #( return ).
*          APPEND ret TO return_tab.
**          global_hr = 'GLOB'.
**          SET PARAMETER ID 'GLOB' FIELD global_hr.
*          "create new relation between position and new org unit
*          IF return-type NE zmhp_cl_int_constants=>error.
*            CLEAR: ret, return.
*            "Build child
*            child-plvar = zmhp_cl_int_constants=>plvar.
*            child-endda = endda.
*            child-begda = begda.
*            child-objid = <field>-field_new.
*            child-otype = zmhp_cl_int_constants=>orgunit.
*            "Build parent
*            parent-plvar = zmhp_cl_int_constants=>plvar.
*            parent-endda = endda.
*            parent-begda = begda.
*            parent-objid = <pos>.
*            parent-otype = zmhp_cl_int_constants=>position.
*            zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
*              EXPORTING
*                 action = CONV #( zmhp_cl_int_constants=>insert_rel )
*                 begda = begda
*                 endda = endda
*                 relat = '003'
*                 rsign = 'A'
*                 parent = CONV #( parent ) "CONV #( orgeh )
*                 child = CONV #( child ) "CONV #( <pos_line> )
*                 spras = spras
*                 simu = simu
*                 IMPORTING
*                   return = return
*                     ).
*            IF return-type NE 'E'.
*              CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*              COMMIT WORK AND WAIT.
*              "Pernr got unlocked during delimit
*              CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
*                EXPORTING
*                  number = pernr
*                IMPORTING
*                  return = return.
*
*            ENDIF.
*            ret = CORRESPONDING #( return ).
*            APPEND ret TO return_tab.
*            CLEAR: ret, return.
*          ENDIF.
*
*          "Create dummy position and relate to new orgunit
**          IF simu = abap_false.
**            DATA(pos_id) = zmhp_cl_int_infotypes=>infty_operation->create_dummy_pos( orgeh = CONV #( <field>-field_new ) begda = begda endda = endda langu = CONV #( spras ) simu = simu ).
**            "Set new position id to record
**            IF pos_id IS NOT INITIAL.
**              "Buffer old position id
**              DATA pos_old TYPE plans.
**              pos_old = <pos>.
**              <pos> = pos_id.
**              "Add success message
**              APPEND VALUE hrpad_return( type = zmhp_cl_int_constants=>success
**                              id = zmhp_cl_int_constants=>msg_class_int
**                              number = zmhp_cl_int_constants=>msg_no-m16
**                              message_v1 = pos_id
**                              message_v2 = pernr
***                            message_v3 = <it_aend>-endda
***                            message_v4 = <it_aend>-infty
**                            ) TO return_tab.
**
**            ELSE.
**              "Add Error
**              APPEND VALUE hrpad_return( type = zmhp_cl_int_constants=>error
**                              id = zmhp_cl_int_constants=>msg_class_int
**                              number = zmhp_cl_int_constants=>msg_no-m16
**                              message_v1 = pos_id
**                              message_v2 = pernr
***                            message_v3 = <it_aend>-endda
***                            message_v4 = <it_aend>-infty
**                            ) TO return_tab.
**            ENDIF.
**          ENDIF.
*        ENDIF.
*      ENDLOOP.
*
**--------------------------------------------------------------------*
**   Decision was made that no new dummy position will be created when an employee has a change of org unit
**   The position should be moved to the new org unit so there will be no trouble with manager assignments at org level
**--------------------------------------------------------------------*
*      "If dummy position was created we need to delimit old A008 relation
**      IF pos_id IS NOT INITIAL.
**        lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && zmhp_cl_int_constants=>it1001 ) ).
**        CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
**        ASSIGN hrp_old_it->* TO <hrp_old>.
**        "Read old relation
**        zmhp_cl_int_infotypes=>infty_operation->read_hrpxxxx(
**        EXPORTING
**          begda = begda
**          endda = endda
**          infty = CONV #( zmhp_cl_int_constants=>it1001 )
**          langu = CONV #( spras )
**          otype = zmhp_cl_int_constants=>position
**          plvar = CONV #( zmhp_cl_int_constants=>plvar )
**          relat = '008'
**          rsign = 'A'
**          sap_id = pos_old
**          subty = CONV #( relat_holder )
**          IMPORTING
**            hrp_old = <hrp_old>
**            return = return
**                                ).
**        ret = CORRESPONDING #( return ).
**        APPEND ret TO return_tab.
**        CLEAR: ret, return.
**
**        "Delimit old relation
**        delimit_date = begda - 1.
**        zmhp_cl_int_infotypes=>infty_operation->delimit_hrpxxxx(
**        EXPORTING
**          record       = <hrp_old>
**          begda        = begda
**          endda        = endda
**          infty        = CONV #( zmhp_cl_int_constants=>it1001 )
**          delimit_date = delimit_date
**          IMPORTING
**            return     = return
**                            ).
**        ret = CORRESPONDING #( return ).
**        APPEND ret TO return_tab.
**
**        "If delimit was successful create new A008 to dummy position
**        IF return-type NE zmhp_cl_int_constants=>error.
**          CLEAR: ret, return.
**          "Build child
**          child-plvar = zmhp_cl_int_constants=>plvar.
**          child-endda = endda.
**          child-begda = begda.
**          child-objid = pernr.
**          child-otype = zmhp_cl_int_constants=>person.
**          "Build parent
**          parent-plvar = zmhp_cl_int_constants=>plvar.
**          parent-endda = endda.
**          parent-begda = begda.
**          parent-objid = pos_id.
**          parent-otype = zmhp_cl_int_constants=>position.
**          zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
**            EXPORTING
**               action = CONV #( zmhp_cl_int_constants=>insert_rel )
**               begda = begda
**               endda = endda
**               relat = '008'
**               rsign = 'A'
**               parent = CONV #( parent ) "CONV #( orgeh )
**               child = CONV #( child ) "CONV #( <pos_line> )
**               spras = spras
**               simu = simu
**               IMPORTING
**                 return = return
**                   ).
**          ret = CORRESPONDING #( return ).
**          APPEND ret TO return_tab.
**          CLEAR: ret, return.
**        ENDIF.
**      ENDIF.
*
*      "Check if cost center relation needs to be specified for employee
*      "Read current orgunit <-> cost center relation
*      IF <orgeh> IS NOT INITIAL.
*        lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && zmhp_cl_int_constants=>it1001 ) ).
*        CREATE DATA hrp_old_it TYPE HANDLE lr_structdescr.
*        ASSIGN hrp_old_it->* TO <hrp_old>.
*        zmhp_cl_int_infotypes=>infty_operation->read_hrpxxxx(
*        EXPORTING
*          begda = begda
*          endda = endda
*          infty = CONV #( zmhp_cl_int_constants=>it1001 )
*          langu = CONV #( spras )
*          otype = zmhp_cl_int_constants=>orgunit
*          plvar = CONV #( zmhp_cl_int_constants=>plvar )
*          relat = '011'
*          rsign = 'A'
*          sap_id = <orgeh>
*          subty = CONV #( relat_cc )
*          IMPORTING
*            hrp_old = <hrp_old>
*            return = return
*                                ).
*        ret = CORRESPONDING #( return ).
*        APPEND ret TO return_tab.
*        CLEAR: ret, return.
*        IF <hrp_old> IS NOT INITIAL.
*          "Check if there is a difference in cost center of employee and related cost center with orgunit
*          ASSIGN COMPONENT zmhp_cl_int_constants=>sobid OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<sobid>).
*          ASSIGN COMPONENT zmhp_cl_int_constants=>kostl OF STRUCTURE record TO FIELD-SYMBOL(<kostl>).
*          IF <sobid> IS ASSIGNED AND <kostl> IS ASSIGNED.
*            IF <sobid>+0(10) NE <kostl>.
*              "If the employee cost center is different we need to create new relation from position of employee to the cost center of employee
*              "Build child
*              child-plvar = zmhp_cl_int_constants=>plvar.
*              child-endda = endda.
*              child-begda = begda.
*              child-objid = <kostl>.
*              child-otype = zmhp_cl_int_constants=>costcenter.
*              "Build parent
*              parent-plvar = zmhp_cl_int_constants=>plvar.
*              parent-endda = endda.
*              parent-begda = begda.
*              parent-objid = <pos>.
*              parent-otype = zmhp_cl_int_constants=>position.
*              zmhp_cl_int_infotypes=>infty_operation->maintain_relation(
*              EXPORTING
*                 action = CONV #( zmhp_cl_int_constants=>insert_rel )
*                 begda = begda
*                 endda = endda
*                 relat = '011'
*                 rsign = 'A'
*                 simu = simu
*                 spras = spras
*                 parent = CONV #( parent ) "CONV #( orgeh )
*                 child = CONV #( child ) "CONV #( <pos_line> )
*                     ).
*              IF return-type NE 'E'.
*                CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*                COMMIT WORK AND WAIT.
*              ENDIF.
*              ret = CORRESPONDING #( return ).
*              APPEND ret TO return_tab.
*              CLEAR: ret, return.
*            ENDIF.
*          ENDIF.
*        ENDIF.
*      ENDIF.
*    ENDIF.
*    CLEAR: return.
  ENDMETHOD.


  METHOD process_0008.
    DATA:
      classdescr        TYPE REF TO cl_abap_classdescr,
      type_descr        TYPE REF TO cl_abap_typedescr,
      custom_classdescr TYPE REF TO cl_abap_classdescr.
*   Check if molga specific method is available
    DATA(infotype_handler) = zmhp_cl_int_general_settings=>get_infotyphandler( ).
*   Get exception class description
    CALL METHOD cl_abap_classdescr=>describe_by_name
      EXPORTING
        p_name         = infotype_handler
      RECEIVING
        p_descr_ref    = type_descr
      EXCEPTIONS
        type_not_found = 1
        OTHERS         = 2.
    custom_classdescr ?= type_descr.
*    classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMHP_CL_INT_INFTY_HANDLER' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE custom_classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Infty spefic method availlable
      CALL METHOD (infotype_handler)=>(method)
        EXPORTING
          begda                  = begda
          endda                  = endda
          sap_id                 = sap_id
          cloud_id               = cloud_id
          aend_id                = aend_id
          has_error              = has_error
          fields                 = fields
          data                   = data
          infotype_xml_processor = infotype_xml_processor
        IMPORTING
          it_aend                = it_aend.
    ELSE.
*     Transfer data to IT_AEND structure
      infotype_xml_processor->pa_to_prelp( EXPORTING infotype = data
                                 aend_id  = aend_id
                       IMPORTING prelp    = DATA(prelp) ).
      it_aend = CORRESPONDING zmhp_int_it_aend( prelp ).
      it_aend-aend_id = aend_id.
    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_IT_SPECIFICS_AFTER_OP.

    zmhp_cl_int_infty_handler_durr=>infty_operation = NEW zmhp_cl_int_it_operation( molga = molga pernr = pernr ).

    TRY.
        DATA(method) = CONV string( 'PROCESS_' && infty && '_AFTER' ).
        CALL METHOD zmhp_cl_int_infty_handler_durr=>(method)
          EXPORTING
            molga          = molga
            simu           = simu
            massn          = massn
            pernr          = pernr
            begda          = begda
            endda          = endda
            infty          = infty
            subty          = subty
            changed_fields = changed_fields
          IMPORTING
            return         = return
            return_tab     = return_tab
          CHANGING
            record         = record.
      CATCH cx_sy_dyn_call_illegal_method.
      CATCH cx_sy_dyn_call_illegal_type.
      CATCH cx_sy_dyn_call_param_missing.
    ENDTRY.
  ENDMETHOD.


  METHOD PROCESS_IT_SPECIFICS_BEFORE_OP.

    zmhp_cl_int_infty_handler_durr=>infty_operation = NEW zmhp_cl_int_it_operation( molga = molga pernr = pernr ).

    TRY.
        DATA(method) = CONV string( 'PROCESS_' && infty && '_BEFORE' ).
        CALL METHOD zmhp_cl_int_infty_handler_durr=>(method)
          EXPORTING
            molga          = molga
            simu           = simu
            massn          = massn
            pernr          = pernr
            begda          = begda
            endda          = endda
            infty          = infty
            subty          = subty
            changed_fields = changed_fields
          IMPORTING
            return         = return
            return_tab     = return_tab
          CHANGING
            record         = record.
      CATCH cx_sy_dyn_call_illegal_method.
      CATCH cx_sy_dyn_call_illegal_type.
      CATCH cx_sy_dyn_call_param_missing.
    ENDTRY.
  ENDMETHOD.
ENDCLASS.
