class ZMHP_CL_INT_ACTION_HANDLER_DUR definition
  public
  final
  create public .

public section.

  class-methods KOPIERVORLAGE
    importing
      !ACTION type ZMHP_INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type ZMHP_TT_IT_AEND .
  class-methods KOPIERVORLAGE_MOLGEPSPEZIFISCH
    importing
      !ACTION type ZMHP_INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type ZMHP_TT_IT_AEND .
  class-methods PROCESS_01
    importing
      !ACTION type ZMHP_INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type ZMHP_TT_IT_AEND .
  class-methods PROCESS_HI
    importing
      !ACTION type ZMHP_INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type ZMHP_TT_IT_AEND .
  class-methods PROCESS_03
    importing
      !ACTION type ZMHP_INT_IT_AEND
    exporting
      !HAS_ERROR type BOOLE_D
    changing
      !NEW_IT_AEND type ZMHP_TT_IT_AEND .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZMHP_CL_INT_ACTION_HANDLER_DUR IMPLEMENTATION.


  METHOD kopiervorlage.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMHP_CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD zmhp_cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
*   Business Logic
    ENDIF.
  ENDMETHOD.


  METHOD KOPIERVORLAGE_MOLGEPSPEZIFISCH.
*   Business Logic
  ENDMETHOD.


  METHOD PROCESS_01.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMHP_CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD zmhp_cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      DATA:
            p0001 TYPE p0001.
*   Process hire logic
*   Check if hire is delimited by Org change -> if that is the case change hire to highdate.
      READ TABLE new_it_aend WITH KEY infty = zmhp_cl_int_constants=>it0000 action = zmhp_cl_int_constants=>org_change TRANSPORTING NO FIELDS.
      IF sy-subrc IS INITIAL.
        DATA(org_change_exists) = abap_true.
        IF action-endda NE zmhp_cl_int_constants=>highdate.
          READ TABLE new_it_aend WITH KEY infty = zmhp_cl_int_constants=>it0000 action = zmhp_cl_int_constants=>hire ASSIGNING FIELD-SYMBOL(<hire_action>).
          <hire_action>-endda = zmhp_cl_int_constants=>highdate.
        ENDIF.
      ENDIF.
*   Loop at timeslices with same begda where action is initial.
      LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<infty>) WHERE begda = action-begda AND action IS INITIAL.
        READ TABLE new_it_aend WITH KEY infty = <infty>-infty action = action-action TRANSPORTING NO FIELDS.
        IF sy-subrc IS INITIAL.
          CONTINUE.
        ELSE.
          IF <infty>-infty = zmhp_cl_int_constants=>it0000 OR <infty>-infty = zmhp_cl_int_constants=>it0001.
            <infty>-action = action-action.
          ENDIF.
*       Set initial end date to end date of hire (should be highdate). End dated infotypes might otherwise cause issues when processing.
          IF <infty>-endda NE action-endda.
            <infty>-endda = action-endda.
          ENDIF.
        ENDIF.
      ENDLOOP.
*   Check if hire is delimited without further orgchange ->
      IF action-endda NE zmhp_cl_int_constants=>highdate AND org_change_exists NE abap_true.
        APPEND INITIAL LINE TO new_it_aend ASSIGNING FIELD-SYMBOL(<termination>).
        <termination>-pernr = action-pernr.
        <termination>-cloud_id = action-cloud_id.
        <termination>-cloud_pernr = action-cloud_pernr.
        <termination>-aend_id = action-aend_id.
        <termination>-begda = action-endda + 1.
        <termination>-endda = zmhp_cl_int_constants=>highdate.
        <termination>-action = zmhp_cl_int_constants=>termination.
      ENDIF.
*   SEW does not use the position interface, therefore dummy position needs to be created in case of hire.
      UNASSIGN <infty>.
      READ TABLE new_it_aend WITH KEY infty = zmhp_cl_int_constants=>it0000 action = action-action ASSIGNING <infty>.
      IF sy-subrc IS INITIAL.
        zmhp_cl_int_utility=>get_spras_by_molga( EXPORTING molga = <infty>-molga
                                                 IMPORTING spras = DATA(spras)
                                                           langu = DATA(langu) ).

        DATA(infty_operation) = NEW zmhp_cl_int_it_operation( int_run = <infty>-int_run
                                                              molga   = action-molga ).
*   Create dummy position and add to infotyp
        DATA(prelp) = CORRESPONDING zmhp_prelp( <infty> ).
        zmhp_cl_int_type_cast=>prelp_to_pnnnn(
          EXPORTING
            prelp = CONV #( prelp )
          IMPORTING
            pnnnn = p0001 ).
        p0001-plans = infty_operation->create_dummy_pos( orgeh = p0001-orgeh
                                                          begda = action-begda
                                                          endda = action-endda
                                                          langu = CONV #( spras )
                                                          simu = zmhp_cl_int_statics=>test_run ).
        "Transfer data to prelp
        zmhp_cl_int_infty_proc_xml=>pa_to_prelp( EXPORTING infotype = p0001
                                                           aend_id  = <infty>-aend_id
                                                 IMPORTING prelp    = prelp ).
        <infty>-data1 = prelp-data1.
        <infty>-data2 = prelp-data2.
        <infty>-data3 = prelp-data3.
        <infty>-data4 = prelp-data4.
        <infty>-data5 = prelp-data5.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_03.
*   Process termination
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMHP_CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefici method availlable
      CALL METHOD ZMHP_CL_INT_ACTION_HANDLER=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
      "In case of Termination the termination date must be set for each IT for delimiting
      READ TABLE new_it_aend WITH KEY action = zmhp_cl_int_constants=>termination ASSIGNING FIELD-SYMBOL(<term_action>).
      IF sy-subrc IS INITIAL.
        LOOP AT new_it_aend ASSIGNING FIELD-SYMBOL(<action>) WHERE infty = zmhp_cl_int_constants=>it0105.
*       Delimit IT0105 with begda of termination + 1 day.
          <action>-endda = <term_action>-begda + 1.
          <action>-action = zmhp_cl_int_constants=>termination.
        ENDLOOP.
        IF <term_action>-pernr IS INITIAL.
          SELECT SINGLE pernr FROM pa9400 INTO @DATA(pernr) WHERE oracleid = @<term_action>-cloud_id.
          IF sy-subrc IS INITIAL.
            LOOP AT new_it_aend ASSIGNING <action>.
              <action>-pernr = pernr.
            ENDLOOP.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD PROCESS_HI.
    DATA: classdescr TYPE REF TO cl_abap_classdescr.
    classdescr ?= cl_abap_typedescr=>describe_by_name( 'ZMHP_CL_INT_ACTION_HANDLER' ).
    DATA(method_molga) = |PROCESS_| && action-action && action-molga.
    READ TABLE classdescr->methods WITH KEY name = method_molga TRANSPORTING NO FIELDS.
    IF sy-subrc IS INITIAL."Molga spefic method availlable
      CALL METHOD zmhp_cl_int_action_handler=>(method_molga)
        EXPORTING
          action      = action
        IMPORTING
          has_error   = has_error
        CHANGING
          new_it_aend = new_it_aend.
    ELSE." Logic if not molga specific.
*   Business Logic
    ENDIF.
  ENDMETHOD.
ENDCLASS.
