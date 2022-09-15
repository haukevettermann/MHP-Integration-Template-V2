class ZMHP_CL_INT_INFTY_HANDLER definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF s_payment .
    TYPES oracle_wagetype TYPE string.
    TYPES wagetype TYPE p0008-lga01.
    TYPES amount TYPE p0008-bet01.
    TYPES seqnr TYPE seqnr.
    TYPES END OF s_payment .
  types:
    t_payments TYPE TABLE OF s_payment WITH DEFAULT KEY .
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
    TYPES ignore_timeslices TYPE boole_d.
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

  methods CONSTRUCTOR .
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
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZMHP_CL_INT_INFTY_HANDLER IMPLEMENTATION.


  method CONSTRUCTOR.
  endmethod.


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

        "SEW specific Hire and Termination handled in customizing via infotype 0000 seqnr 001. Therefore ignore for seqnr 000
        IF infotype_xml_processor->seqnr = 000 AND <action>-value IN zmhp_cl_int_constants=>non_relevant_action( ).
*          EXIT.
        ENDIF.

        ASSIGN COMPONENT zmhp_cl_int_constants=>massn OF STRUCTURE <structure> TO FIELD-SYMBOL(<value>).
        IF <value> = 'RE'.
          EXIT.
        ENDIF.

        CHECK <value> NOT IN zmhp_cl_int_constants=>non_relevant_action( ).

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


  METHOD process_0008.
    DATA: classdescr TYPE REF TO cl_abap_classdescr,
          p0008_tab  TYPE TABLE OF p0008.
*   Check if molga specific method is available
    classdescr ?= cl_abap_typedescr=>describe_by_name( '/SEW/CL_INT_INFTY_HANDLER' ).
    DATA(method) = |PROCESS_| && infotype_xml_processor->infty && '_' && infotype_xml_processor->molga.
    READ TABLE classdescr->methods WITH KEY name = method TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
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
    ELSE." Logic if not molga specific.

*     Read current infotype with begda = begda of current entry
      DATA(lo_infotype) = NEW zmhp_cl_int_it_operation( ).
      lo_infotype->read_paxxxx_dcif(
        EXPORTING
          iv_infty        = '0008'
        IMPORTING
          record_tab      = p0008_tab ).
      IF p0008_tab IS NOT INITIAL.
        DATA(p0008) = VALUE #( p0008_tab[ 1 ] OPTIONAL ).
      ELSE.
*     Fill general fields first in case no IT0008 exists
*     Assign pernr
        p0008-pernr = sap_id.
*     Set infotype
        p0008-infty = '0008'.
      ENDIF.
*     Fill other fields
      p0008-begda = begda.
      p0008-endda = endda.
      p0008-subty = '0'.
*     Fill tarif info TRFAR, TRFGB, TRFGR
      READ TABLE fields WITH KEY field_sap = 'TRFAR' ASSIGNING FIELD-SYMBOL(<trfar>).
      IF <trfar> IS ASSIGNED.
        p0008-trfar = COND #( WHEN <trfar>-value_converted IS NOT INITIAL THEN <trfar>-value_converted
                              "WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfar>-value ).
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGB' ASSIGNING FIELD-SYMBOL(<trfgb>).
      IF <trfgb> IS ASSIGNED.
        p0008-trfgb = COND #( WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              WHEN <trfgb>-value_mapped IS NOT INITIAL THEN <trfgb>-value_mapped
                              "WHEN <trfgb>-value_converted IS NOT INITIAL THEN <trfgb>-value_converted
                              ELSE <trfgb>-value ).
*        p0008-trfgb = split2.
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFGR' ASSIGNING FIELD-SYMBOL(<trfgr>).
      IF <trfgr> IS ASSIGNED.
        p0008-trfgr = COND #( WHEN <trfgr>-value_converted IS NOT INITIAL THEN <trfgr>-value_converted
                              "WHEN <trfgr>-value_mapped IS NOT INITIAL THEN <trfgr>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfgr>-value ).
      ENDIF.
      READ TABLE fields WITH KEY field_sap = 'TRFST' ASSIGNING FIELD-SYMBOL(<trfst>).
      IF <trfst> IS ASSIGNED.
        p0008-trfst = COND #( WHEN <trfst>-value_converted IS NOT INITIAL THEN <trfst>-value_converted
                              "WHEN <trfst>-value_mapped IS NOT INITIAL THEN <trfst>-value_mapped
                              WHEN <trfar>-value_mapped IS NOT INITIAL THEN <trfar>-value_mapped
                              ELSE <trfst>-value ).
      ENDIF.
*     Collect payments in table.
      DATA(payments) = VALUE t_payments( FOR payment IN fields WHERE ( field_sap = 'LGART' ) ( oracle_wagetype = payment-value seqnr = payment-seqnr ) ).
      LOOP AT payments ASSIGNING FIELD-SYMBOL(<payment>).
        DATA(offset) = strlen( <payment>-oracle_wagetype ) - 4.
        <payment>-wagetype = <payment>-oracle_wagetype+offset(4).
        READ TABLE fields WITH KEY seqnr = <payment>-seqnr field_sap = 'BETRG' ASSIGNING FIELD-SYMBOL(<field>).
        IF <field> IS ASSIGNED.
          <payment>-amount = <field>-value.
        ENDIF.
      ENDLOOP.
*     Start processing of infotype 0008.
      DATA(continue) = abap_true.
      DATA(wage_types_done) = CONV char2( '01' ).
      DATA(comp_wage_type) = 'LGA' && wage_types_done.
      DATA(comp_amount) = 'BET' && wage_types_done.
      WHILE continue = abap_true.
        ASSIGN COMPONENT comp_wage_type OF STRUCTURE p0008 TO FIELD-SYMBOL(<wage_type>).
        ASSIGN COMPONENT comp_amount OF STRUCTURE p0008 TO FIELD-SYMBOL(<amount>).
*       Check if IT0008 is filled and
        IF ( <wage_type> IS ASSIGNED AND <wage_type> IS NOT INITIAL ) AND ( <amount> IS ASSIGNED AND <amount> IS NOT INITIAL ).
          READ TABLE payments WITH KEY wagetype = <wage_type> ASSIGNING FIELD-SYMBOL(<new_payment>).
          IF sy-subrc IS NOT INITIAL.
            <amount> = <new_payment>-amount.
            UNASSIGN <new_payment>.
            DELETE payments WHERE wagetype = <wage_type>.
          ENDIF.
        ELSE.
          READ TABLE payments ASSIGNING <new_payment> INDEX 1.
          IF <new_payment> IS ASSIGNED.
            <wage_type> = <new_payment>-wagetype.
            <amount> = <new_payment>-amount.
            UNASSIGN <new_payment>.
            DELETE payments INDEX 1.
          ELSE.
            continue = abap_false.
          ENDIF.
        ENDIF.
        wage_types_done = wage_types_done + 1.
        UNPACK wage_types_done TO wage_types_done.
        comp_wage_type = 'LGA' && wage_types_done.
        comp_amount = 'BET' && wage_types_done.
        IF wage_types_done = 41.
          continue = abap_false.
        ENDIF.
      ENDWHILE.
    ENDIF.
*     Transfer data to IT_AEND structure
    infotype_xml_processor->pa_to_prelp( EXPORTING infotype = p0008
                               aend_id  = aend_id
                     IMPORTING prelp    = DATA(prelp) ).
    it_aend = CORRESPONDING zmhp_int_it_aend( prelp ).
    it_aend-aend_id = aend_id.
  ENDMETHOD.
ENDCLASS.
