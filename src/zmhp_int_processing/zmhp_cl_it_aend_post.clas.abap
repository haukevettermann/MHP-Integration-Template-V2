class ZMHP_CL_IT_AEND_POST definition
  public
  create public .

public section.

  types:
    BEGIN OF ty_field,
        infty type infty,
        subty type subty,
        field  TYPE char50,
        field_old TYPE char50,
        field_new TYPE char50,
        field4 TYPE char50,
      END OF ty_field .
  types:
    ty_fields TYPE TABLE OF ty_field .

  data DATES type RSDSSELOPT_T .
  data STATUS type RSDSSELOPT_T .
  data IT_AEND type ZMHP_TT_IT_AEND .
  data IT_AEND_ERROR type ZMHP_TT_IT_AEND .
  data IT_AEND_POST type ZMHP_TT_IT_AEND .
  data IT_AEND_NOCHANGE type ZMHP_TT_IT_AEND .
  data MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  data SKIPPED_PERNRS type HRAHQ_PERNR_TABLE .
  data SIMU type BOOLEAN .
  data STATUS_HANDLER type ref to ZMHP_CL_INT_STATUS_HANDLER .
  data IT_AEND_LOCKED type ZMHP_TT_IT_AEND .

  methods ADD_PA9400
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
      !CLOUD_ID type ZMHP_DD_ELEMENT
      !SIMU type BOOLE_D
    exporting
      !RETURN type BAPIRET1 .
  methods CHECK_PA9400_EXISTS
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !PERNR type PERNR_D
    exporting
      !RETURN type BAPIRET1
      !EXISTS type BOOLE_D .
  methods ADJUST_BEGDA_ENDDA
    importing
      !PERNR type PERNR_D
      !SIMU type BOOLE_D
    exporting
      !NO_CHANGE type BOOLE_D
    changing
      !RECORD type ANY .
  methods ADJUST_OPERATION
    importing
      !SIMU type BOOLE_D
    changing
      !IT_AEND type ZMHP_INT_IT_AEND .
  methods CHECK_ACTION_ON_HIREDATE
    importing
      !PERNR type PERNR_D
      !INT_RUN type GUID_32
      !SIMU type BOOLE_D
    exporting
      !NO_CHANGE type BOOLE_D
      !OPERATION type ACTIO
    changing
      !RECORD type ANY .
  methods HANDLE_HIRE
    importing
      !SIMU type BOOLE_D
    exporting
      !IS_OK type BOOLE_D
      !RETURN_TAB type HRPAD_RETURN_TAB
      !PERNR type PERNR_D
      !IT_AEND_ERROR type ZMHP_TT_IT_AEND
      !IT_AEND_POST type ZMHP_TT_IT_AEND
      !CONTINUE type BOOLE_D
    changing
      !IT_AEND type ZMHP_INT_IT_AEND
      !IT_AEND_TAB type ZMHP_TT_IT_AEND .
  methods HANDLE_ORG_CHANGE
    importing
      !RECORD type ANY
      !SIMU type BOOLE_D
    exporting
      !IS_OK type BOOLE_D
      !RETURN_TAB type HRPAD_RETURN_TAB
      !PERNR type PERNR_D
      !IT_AEND_ERROR type ZMHP_TT_IT_AEND
      !IT_AEND_POST type ZMHP_TT_IT_AEND
      !CONTINUE type BOOLE_D
    changing
      !IT_AEND type ZMHP_INT_IT_AEND
      !IT_AEND_TAB type ZMHP_TT_IT_AEND .
  methods HANDLE_TERMINATION
    importing
      !RECORD type ANY
      !SIMU type BOOLE_D
    exporting
      !IS_OK type BOOLE_D
      !RETURN_TAB type HRPAD_RETURN_TAB
      !PERNR type PERNR_D
      !IT_AEND_ERROR type ZMHP_TT_IT_AEND
      !IT_AEND_POST type ZMHP_TT_IT_AEND
      !CONTINUE type BOOLE_D
    changing
      !IT_AEND type ZMHP_INT_IT_AEND
      !IT_AEND_TAB type ZMHP_TT_IT_AEND .
  methods PROCESS_ACTION
    importing
      !RECORD type ANY
      !SIMU type BOOLE_D
    exporting
      !IS_OK type BOOLE_D
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RETURN type BAPIRETURN1
      !PERNR type PERNR_D
      !IT_AEND_ERROR type ZMHP_TT_IT_AEND
      !IT_AEND_POST type ZMHP_TT_IT_AEND
      !CONTINUE type BOOLE_D
    changing
      !IT_AEND type ZMHP_INT_IT_AEND
      !IT_AEND_TAB type ZMHP_TT_IT_AEND .
  methods PROCESS_CHANGES
    importing
      !SIMU type BOOLE_D
      !RECORD type ANY
    exporting
      !RETURN_TAB type HRPAD_RETURN_TAB
      !RETURN type BAPIRETURN1
      !PERNR type PERNR_D
      !IT_AEND_ERROR type ZMHP_TT_IT_AEND
      !IT_AEND_POST type ZMHP_TT_IT_AEND
      !CONTINUE type BOOLE_D
    changing
      !IS_OK type BOOLE_D
      !IT_AEND type ZMHP_INT_IT_AEND
      !IT_AEND_TAB type ZMHP_TT_IT_AEND .
  methods IT_SPECIFIC_LOGIC
    importing
      !SIMU type BOOLE_D
      !EXEC_OPTION type STRING
      !MOLGA type MOLGA
      !INFTY type INFTY
      !SUBTY type SUBTY
      !MASSN type MASSN
      !CHANGED_FIELDS type TY_FIELDS
    exporting
      !RETURN type BAPIRET1
      !RETURN_TAB type HRPAD_RETURN_TAB
    changing
      !RECORD type ANY .
  methods PERFORM_PA9400_CHECK
    importing
      !IT_AEND_POST type ZMHP_TT_IT_AEND
      !SIMU type BOOLE_D
    exporting
      !EXISTS type BOOLE_D .
  methods UPDATE_STATUS
    importing
      !SIMU type BOOLE_D .
  methods CHECK_STATUS_RANGE
    importing
      !INITIAL type BOOLEAN
      !ERROR type BOOLEAN .
  methods GET_IT_AEND_DATA
    importing
      !PERNR type RSDSSELOPT_T
      !INTRUN type RSDSSELOPT_T
      !DATES type RSDSSELOPT_T
      !STATUS type RSDSSELOPT_T
    returning
      value(IT_AEND) type ZMHP_TT_IT_AEND .
  methods PREPARE_PROTOCOL .
  methods START_POST .
  methods CONSTRUCTOR
    importing
      !ALV type BOOLEAN
      !BATCH type BOOLEAN
      !SIMU type BOOLEAN
      !DATES type RSDSSELOPT_T .
  methods POST_IT_AEND
    importing
      !SIMU type BOOLEAN
    exporting
      !IT_AEND_POST type ZMHP_TT_IT_AEND
      !IT_AEND_ERROR type ZMHP_TT_IT_AEND
      !IT_AEND_NOCHANGE type ZMHP_TT_IT_AEND
    changing
      !MESSAGE_HANDLER type ref to CL_HRPAY00_MESSAGE_HANDLER .
  PROTECTED SECTION.
private section.
ENDCLASS.



CLASS ZMHP_CL_IT_AEND_POST IMPLEMENTATION.


  METHOD add_pa9400.

    DATA: pa9400 TYPE p9400,
          prelp  TYPE prelp.

    pa9400-begda = begda.
    pa9400-endda = endda.
    pa9400-pernr = pernr.
    pa9400-oracleid = cloud_id.

*    MOVE-CORRESPONDING p9400 TO prelp.
*    cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
*      EXPORTING
*        prelp = prelp
*      IMPORTING
*        pnnnn = <struc> ).
*
*    "perform infotype operation
*
    CALL FUNCTION 'HR_INFOTYPE_OPERATION'
      EXPORTING
        infty         = '9400'
*       recordnumber  = <it_aend>-seqnr
        number        = pernr
*       subtype       = <it_aend>-subty
        validityend   = endda
        validitybegin = begda
        record        = pa9400
        operation     = zmhp_cl_int_constants=>infty_operation-pa_insert
        nocommit      = simu
      IMPORTING
        return        = return.
*        key           = key.

  ENDMETHOD.


  METHOD adjust_begda_endda.

*    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
*          record_old     TYPE REF TO data,
*          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
*          lr_table_old   TYPE REF TO data,
*          lr_table_new   TYPE REF TO data,
*          ret            LIKE LINE OF return_tab.

*    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
*                   <record_old>     TYPE any,
*                   <record_new>     TYPE any,
*                   <record_old_tab> TYPE STANDARD TABLE.
*    ASSIGN record TO <record_new>.

*    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
*    CREATE DATA record_old TYPE HANDLE lr_structdescr.
*    ASSIGN record_old->* TO <record_old>.
*    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
*    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
**    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
*    ASSIGN lr_table_old->* TO <record_old_tab>.
*    ASSIGN lr_table_new->* TO <record_new_tab>.
    no_change = abap_false.

    SELECT * FROM pa0000 INTO TABLE @DATA(it0000) WHERE pernr = @pernr.
    IF it0000 IS NOT INITIAL.
      SORT it0000 ASCENDING BY begda.
      READ TABLE it0000 ASSIGNING FIELD-SYMBOL(<s0000>) INDEX 1.
      ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE record TO FIELD-SYMBOL(<begda>).
      ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE record TO FIELD-SYMBOL(<endda>).

      IF <endda> LT <s0000>-begda.
        no_change = abap_true.
      ENDIF.
      IF no_change = abap_false.
        IF <begda> LT <s0000>-begda.
          <begda> = <s0000>-begda.
        ENDIF.
      ENDIF.
    ENDIF.
*    CALL FUNCTION 'HR_READ_INFOTYPE'
*      EXPORTING
**       TCLAS     = 'A'
*        pernr     = pernr
*        infty     = zmhp_cl_int_constants=>it0000
*        begda     = begda
*        endda     = endda
**       SPRPS     = '*'
**       BYPASS_BUFFER = ' '
**       LEGACY_MODE   = ' '
**     IMPORTING
*      TABLES
*        infty_tab = <record_old_tab>
** EXCEPTIONS
**       INFTY_NOT_FOUND       = 1
**       INVALID_INPUT = 2
**       OTHERS    = 3
*      .
*    IF sy-subrc <> 0.
*    ENDIF.
*    READ TABLE <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>) INDEX 1.
*    IF subty IS NOT INITIAL.
*      LOOP AT <record_old_tab> ASSIGNING <fs_record_old>.
*        ASSIGN COMPONENT zmhp_cl_int_constants=>subty OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_subty_old>).
*        IF <fs_subty_old> = subty.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*      IF <fs_subty_old> NE subty.
*        UNASSIGN <fs_record_old>.
*      ENDIF.
*    ENDIF.
*    LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>).
*      CLEAR: ret, return.
*      IF <fs_record_old> IS ASSIGNED.
*        ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_endda_old>).
*        IF <fs_endda_old> GT delimit_date.
*          <fs_endda_old> = delimit_date.
*          ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_begda_old>).
*          CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*            EXPORTING
*              infty         = infty
**             recordnumber  = <it_aend>-seqnr
*              number        = pernr
*              subtype       = subty
*              validityend   = <fs_endda_old>
*              validitybegin = <fs_begda_old>
*              record        = <fs_record_old>
*              operation     = zmhp_cl_int_constants=>infty_operation-pa_delimit
*              nocommit      = simu
*            IMPORTING
*              return        = return.
**        key           = key.
*          IF return-type = zmhp_cl_int_constants=>error.
*            MOVE-CORRESPONDING return TO ret.
*
*          ELSE.
*            ret = VALUE hrpad_return( type = zmhp_cl_int_constants=>success
*                     id = zmhp_cl_int_constants=>msg_class_int
*                     number = zmhp_cl_int_constants=>msg_no-m17
*                     message_v1 = pernr
*                     message_v2 = infty
*                     message_v3 = subty
*                     message_v4 = delimit_date
*                   ).
*          ENDIF.
*          APPEND ret TO return_tab.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.


  METHOD adjust_operation.
    IF it_aend-infty = zmhp_cl_int_constants=>it0001.
*      IF it_aend-endda NE zmhp_cl_int_constants=>highdate.
*        it_aend-operation = zmhp_cl_int_constants=>infty_operation-pa_delimit.
*      ENDIF.
**      SELECT * FROM pa0001 INTO TABLE @DATA(p0001) WHERE pernr = @it_aend-pernr AND begda GE @it_aend-endda.
      it_aend-operation = zmhp_cl_int_constants=>infty_operation-pa_copy.
      SELECT * FROM pa0001 INTO TABLE @DATA(p0001) WHERE pernr = @it_aend-pernr AND begda = @it_aend-begda.
      IF p0001 IS NOT INITIAL.
*        SORT p0001 BY begda ASCENDING.
*        READ TABLE p0001 INTO DATA(p0001_s) INDEX 1.
*        IF p0001_s-begda = it_aend-begda.
*          it_aend-operation = zmhp_cl_int_constants=>infty_operation-pa_copy.
*        ENDIF.
        READ TABLE p0001 INTO DATA(p0001_s) INDEX 1.
        IF p0001_s-endda = zmhp_cl_int_constants=>highdate AND it_aend-endda NE zmhp_cl_int_constants=>highdate.
          it_aend-operation = zmhp_cl_int_constants=>infty_operation-pa_delimit.
        ENDIF.
      ELSEIF p0001 IS INITIAL.
        IF it_aend-endda NE zmhp_cl_int_constants=>highdate.
          it_aend-operation = zmhp_cl_int_constants=>infty_operation-pa_delimit.
        ENDIF.
      ENDIF.
    ELSEIF it_aend-infty = zmhp_cl_int_constants=>it0000.
      IF it_aend-action NE 'INSS'.
        SELECT * FROM pa0000 INTO TABLE @DATA(p0000) WHERE pernr = @it_aend-pernr AND begda = @it_aend-begda AND endda = @it_aend-endda.
        IF p0000 IS NOT INITIAL.
          it_aend-operation = zmhp_cl_int_constants=>infty_operation-pa_copy.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_action_on_hiredate.

*    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
*          record_old     TYPE REF TO data,
*          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
*          lr_table_old   TYPE REF TO data,
*          lr_table_new   TYPE REF TO data,
*          ret            LIKE LINE OF return_tab.

*    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
*                   <record_old>     TYPE any,
*                   <record_new>     TYPE any,
*                   <record_old_tab> TYPE STANDARD TABLE.
*    ASSIGN record TO <record_new>.

*    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
*    CREATE DATA record_old TYPE HANDLE lr_structdescr.
*    ASSIGN record_old->* TO <record_old>.
*    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
*    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
**    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
*    ASSIGN lr_table_old->* TO <record_old_tab>.
*    ASSIGN lr_table_new->* TO <record_new_tab>.
    CLEAR: operation.
    no_change = abap_false.
    ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE record TO FIELD-SYMBOL(<begda>).
    ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE record TO FIELD-SYMBOL(<endda>).
    ASSIGN COMPONENT zmhp_cl_int_constants=>infty OF STRUCTURE record TO FIELD-SYMBOL(<infty>).
*    ASSIGN COMPONENT 'INT_RUN' OF STRUCTURE record TO FIELD-SYMBOL(<int_run>).
    SELECT * FROM pa0000 INTO TABLE @DATA(it0000) WHERE pernr = @pernr AND massn = @zmhp_cl_int_constants=>hire.
    IF it0000 IS NOT INITIAL.
*      SORT it0000 ASCENDING BY begda.
      READ TABLE it0000 ASSIGNING FIELD-SYMBOL(<s0000>) INDEX 1.

      IF <infty> NE zmhp_cl_int_constants=>it0000.
        IF <infty> NE zmhp_cl_int_constants=>it0105.
          IF <infty> = '0002' OR <infty> = '9400'.
          ELSE.
            IF <begda> LT <s0000>-begda.
              no_change = abap_true.
            ENDIF.
          ENDIF.
        ELSE.
        ENDIF.
      ENDIF.
    ENDIF.

    IF <infty> = zmhp_cl_int_constants=>it0000.
      ASSIGN COMPONENT zmhp_cl_int_constants=>massn OF STRUCTURE record TO FIELD-SYMBOL(<massn>).
      CLEAR: it0000.
      SELECT * FROM pa0000 INTO TABLE @it0000 WHERE pernr = @pernr AND ( massn = @zmhp_cl_int_constants=>hire OR massn = @zmhp_cl_int_constants=>termination ).
      IF it0000 IS NOT INITIAL.

        LOOP AT it0000 ASSIGNING <s0000>.
          IF <begda> EQ <s0000>-begda.
            operation = 'INSS'.
            IF <massn> = zmhp_cl_int_constants=>org_change AND <begda> NE <endda>.
              no_change = abap_true.
            ENDIF.
          ENDIF.
        ENDLOOP.
      ELSE.
        READ TABLE me->status_handler->it_aend INTO DATA(it_aend_line) WITH KEY action = zmhp_cl_int_constants=>hire pernr = pernr int_run = int_run begda = <begda>.
        IF it_aend_line IS NOT INITIAL.
          operation = 'INSS'.
          IF <massn> = zmhp_cl_int_constants=>org_change AND <begda> NE <endda>.
            IF <begda> = it_aend_line-begda.
              no_change = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

*    CALL FUNCTION 'HR_READ_INFOTYPE'
*      EXPORTING
**       TCLAS     = 'A'
*        pernr     = pernr
*        infty     = zmhp_cl_int_constants=>it0000
*        begda     = begda
*        endda     = endda
**       SPRPS     = '*'
**       BYPASS_BUFFER = ' '
**       LEGACY_MODE   = ' '
**     IMPORTING
*      TABLES
*        infty_tab = <record_old_tab>
** EXCEPTIONS
**       INFTY_NOT_FOUND       = 1
**       INVALID_INPUT = 2
**       OTHERS    = 3
*      .
*    IF sy-subrc <> 0.
*    ENDIF.
*    READ TABLE <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>) INDEX 1.
*    IF subty IS NOT INITIAL.
*      LOOP AT <record_old_tab> ASSIGNING <fs_record_old>.
*        ASSIGN COMPONENT zmhp_cl_int_constants=>subty OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_subty_old>).
*        IF <fs_subty_old> = subty.
*          EXIT.
*        ENDIF.
*      ENDLOOP.
*      IF <fs_subty_old> NE subty.
*        UNASSIGN <fs_record_old>.
*      ENDIF.
*    ENDIF.
*    LOOP AT <record_old_tab> ASSIGNING FIELD-SYMBOL(<fs_record_old>).
*      CLEAR: ret, return.
*      IF <fs_record_old> IS ASSIGNED.
*        ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_endda_old>).
*        IF <fs_endda_old> GT delimit_date.
*          <fs_endda_old> = delimit_date.
*          ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_record_old> TO FIELD-SYMBOL(<fs_begda_old>).
*          CALL FUNCTION 'HR_INFOTYPE_OPERATION'
*            EXPORTING
*              infty         = infty
**             recordnumber  = <it_aend>-seqnr
*              number        = pernr
*              subtype       = subty
*              validityend   = <fs_endda_old>
*              validitybegin = <fs_begda_old>
*              record        = <fs_record_old>
*              operation     = zmhp_cl_int_constants=>infty_operation-pa_delimit
*              nocommit      = simu
*            IMPORTING
*              return        = return.
**        key           = key.
*          IF return-type = zmhp_cl_int_constants=>error.
*            MOVE-CORRESPONDING return TO ret.
*
*          ELSE.
*            ret = VALUE hrpad_return( type = zmhp_cl_int_constants=>success
*                     id = zmhp_cl_int_constants=>msg_class_int
*                     number = zmhp_cl_int_constants=>msg_no-m17
*                     message_v1 = pernr
*                     message_v2 = infty
*                     message_v3 = subty
*                     message_v4 = delimit_date
*                   ).
*          ENDIF.
*          APPEND ret TO return_tab.
*        ENDIF.
*      ENDIF.
*    ENDLOOP.
  ENDMETHOD.


  METHOD check_pa9400_exists.

    DATA: pa9400     TYPE p9400,
          prelp      TYPE prelp,
          pa9400_tab TYPE TABLE OF p9400.

    pa9400-begda = begda.
    pa9400-endda = endda.
    pa9400-pernr = pernr.

*    "perform infotype operation

    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
*       TCLAS           = 'A'
        pernr           = pernr
        infty           = '9400'
        begda           = begda
        endda           = endda
*       SPRPS           = '*'
*       BYPASS_BUFFER   = ' '
*       LEGACY_MODE     = ' '
*     IMPORTING
      TABLES
        infty_tab       = pa9400_tab
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* Implement suitable error handling here
      return = zmhp_cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*      zmhp_cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                        IMPORTING message = return-message ).
    ENDIF.

    IF pa9400_tab IS NOT INITIAL.
      exists = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_status_range.

    "Default both status
    status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_it_aend=>status_error )
                                 ( sign = 'I' option = 'EQ' low = zmhp_cl_int_it_aend=>status_initial ) ).

    "In case only approved and canceled requests should be posted
    IF initial EQ abap_true.
      status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_it_aend=>status_initial ) ).
    ENDIF.

    "In case only failed requests should be posted
    IF error EQ abap_true.
      status = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_it_aend=>status_error ) ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    me->simu  = simu.
    me->dates = dates.

    "prepare protocol in case dialog is active and ALV is requested
    IF alv   EQ abap_true AND
       batch EQ abap_false.
      message_handler = cl_hrpay00_message_handler=>get_message_handler( ).
    ENDIF.

  ENDMETHOD.


  METHOD get_it_aend_data.

    "get infotype changes
    it_aend = NEW zmhp_cl_int_it_aend( )->get( pernr  = pernr
                                               intrun = intrun
                                               dates  = dates
                                               status = status ).

    CHECK it_aend IS NOT INITIAL.

    SORT it_aend BY pernr timestamp infty ASCENDING.

  ENDMETHOD.


  METHOD handle_hire.
*    DATA: it_aend_tmp TYPE zmhp_tt_it_aend.
*    it_aend_tmp = it_aend_tab.
    DATA: error_tab   TYPE hrpad_return_tab,
          error_hrpad LIKE LINE OF error_tab,
          return      TYPE bapiret1,
          massn_txt   TYPE mntxt,
          del_it_aend TYPE zmhp_int_it_aend.
    continue = abap_false.
    DATA(infty_operation) = NEW zmhp_cl_int_it_operation( molga = it_aend-molga int_run = it_aend-int_run ).
    zmhp_cl_int_utility=>get_spras_by_molga( EXPORTING molga = it_aend-molga
     IMPORTING spras = DATA(spras) langu = DATA(langu) ).
    "Hire logic
    IF it_aend-infty = zmhp_cl_int_constants=>it0000 AND it_aend-action = zmhp_cl_int_constants=>hire.
      IF it_aend-pernr IS INITIAL. "it_aend-endda = zmhp_cl_int_constants=>highdate.

        DATA(stat) = it_aend-status.

        infty_operation->perform_hire( EXPORTING it_aend = it_aend it_aend_tab_rel = it_aend_tab it_aend_tab = me->status_handler->it_aend simu = simu
                               IMPORTING is_ok = is_ok return_tab = return_tab pernr = it_aend-pernr ).
        APPEND LINES OF return_tab TO error_tab.

        CLEAR: return_tab, return.
        "Hire was successfull
        IF is_ok = abap_true.

          pernr = it_aend-pernr.

          it_aend-status = zmhp_cl_int_constants=>booking_status-success.
          MODIFY it_aend_tab FROM it_aend TRANSPORTING status WHERE status NE zmhp_cl_int_constants=>booking_status-success.
          MODIFY it_aend_tab FROM it_aend TRANSPORTING pernr WHERE status EQ zmhp_cl_int_constants=>booking_status-success.

          APPEND LINES OF it_aend_tab TO it_aend_post.
          del_it_aend = it_aend.
          CLEAR del_it_aend-pernr.
          APPEND del_it_aend TO me->status_handler->del_it_aend.
          DELETE it_aend_tab WHERE int_run = it_aend-int_run AND cloud_id = it_aend-cloud_id AND aend_id = it_aend-aend_id.
          "Add success message
*              me->status_handler->add_log_message( aend_id = it_aend-aend_id bapiret1 = return hrpad_return = return_tab ).
*          ENDIF.

          "Add action message
          massn_txt = zmhp_cl_int_utility=>read_massn_txt( sprsl = spras massn = it_aend-action ).
          DATA(begda_ext) = zmhp_cl_int_utility=>get_external_date( date = it_aend-begda ).
          DATA(endda_ext) = zmhp_cl_int_utility=>get_external_date( date = it_aend-endda ).
          return = VALUE bapiret1( type = zmhp_cl_int_constants=>success
                                   id = zmhp_cl_int_constants=>msg_class_int
                                   number = zmhp_cl_int_constants=>msg_no-m20
                                   message_v1 = it_aend-pernr
                                   message_v2 = begda_ext
                                   message_v3 = endda_ext
                                   message_v4 = massn_txt
                                 ).
          error_hrpad = CORRESPONDING #( return ).
          APPEND error_hrpad TO error_tab.
          CLEAR: return, error_hrpad, massn_txt.

          me->status_handler->add_log_message( aend_id = it_aend-aend_id bapiret1 = return hrpad_return = error_tab ).
          "Else there was an error when hiring
        ELSE.
          "set error status
          it_aend-status = zmhp_cl_int_constants=>booking_status-error.
          MODIFY it_aend_tab FROM it_aend TRANSPORTING status WHERE status NE zmhp_cl_int_constants=>booking_status-error.
          APPEND LINES OF it_aend_tab TO it_aend_error.
          "Add error message
          me->status_handler->add_log_message( aend_id = it_aend-aend_id hrpad_return = error_tab ).
          APPEND LINES OF  error_tab TO return_tab.
        ENDIF.
        stat = it_aend-status.
      ELSE.

        DELETE it_aend_tab WHERE int_run = it_aend-int_run AND cloud_id = it_aend-cloud_id AND aend_id = it_aend-aend_id.
        APPEND it_aend TO it_aend_nochange.

      ENDIF.
    ELSE.
      del_it_aend = it_aend.
      CLEAR del_it_aend-pernr.
      APPEND del_it_aend TO me->status_handler->del_it_aend.
      DELETE it_aend_tab WHERE int_run = it_aend-int_run AND cloud_id = it_aend-cloud_id AND aend_id = it_aend-aend_id.

      continue = abap_true.
    ENDIF.
  ENDMETHOD.


  METHOD handle_org_change.
    DATA: exception TYPE REF TO cx_root.
    FIELD-SYMBOLS: <fs_it>         TYPE any,
                   <fs_it_upd>     TYPE any,
                   <fs_it_old_upd> TYPE any,
                   <ft_it_old>     TYPE STANDARD TABLE.
    DATA(infty_operation) = NEW zmhp_cl_int_it_operation( int_run = it_aend-int_run molga = it_aend-molga pernr = it_aend-pernr ).
    ASSIGN record TO <fs_it>.

    infty_operation->create_action_dcif(
    EXPORTING
      begda = it_aend-begda
      endda = it_aend-endda
      data = <fs_it>
      infty = it_aend-infty
      pernr = it_aend-pernr
      IMPORTING
        is_ok = is_ok
        messages = DATA(messages)
    ).

    return_tab = zmhp_cl_int_utility=>map_msg_tab( messages ).
  ENDMETHOD.


  METHOD handle_termination.
    DATA: exception TYPE REF TO cx_root.
    FIELD-SYMBOLS: <fs_it>         TYPE any,
                   <fs_it_upd>     TYPE any,
                   <fs_it_old_upd> TYPE any,
                   <ft_it_old>     TYPE STANDARD TABLE.
    DATA(infty_operation) = NEW zmhp_cl_int_it_operation( int_run = it_aend-int_run molga = it_aend-molga pernr = it_aend-pernr ).
    ASSIGN record TO <fs_it>.

    infty_operation->create_action_dcif(
    EXPORTING
      begda = it_aend-begda
      endda = it_aend-endda
      data = <fs_it>
      infty = it_aend-infty
      pernr = it_aend-pernr
      IMPORTING
        is_ok = is_ok
        messages = DATA(messages)
    ).
    return_tab = zmhp_cl_int_utility=>map_msg_tab( messages ).
  ENDMETHOD.


  METHOD it_specific_logic.

    ASSIGN COMPONENT zmhp_cl_int_constants=>pernr OF STRUCTURE record TO FIELD-SYMBOL(<pernr>).
    ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE record TO FIELD-SYMBOL(<begda>).
    ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE record TO FIELD-SYMBOL(<endda>).

    IF exec_option = zmhp_cl_int_constants=>before.
*      zmhp_cl_int_infotypes=>process_it_specifics_before_op(
      zmhp_cl_int_infty_handler_durr=>process_it_specifics_before_op(
      EXPORTING
         simu           = simu
         massn          = massn
         molga          = molga
         pernr          = <pernr>
         begda          = <begda>
         endda          = <endda>
         infty          = infty
         subty          = subty
         changed_fields = changed_fields
      IMPORTING
         return         = return
         return_tab     = return_tab
      CHANGING
         record         = record ).
    ELSE.
*      zmhp_cl_int_infotypes=>process_it_specifics_after_op(
      zmhp_cl_int_infty_handler_durr=>process_it_specifics_after_op(
      EXPORTING
         simu           = simu
         massn          = massn
         molga          = molga
         pernr          = <pernr>
         begda          = <begda>
         endda          = <endda>
         infty          = infty
         subty          = subty
         changed_fields = changed_fields
      IMPORTING
         return         = return
         return_tab     = return_tab
      CHANGING
         record         = record ).
    ENDIF.
  ENDMETHOD.


  METHOD perform_pa9400_check.

    DATA(it_aend_post_tmp) = it_aend_post.
    SORT it_aend_post_tmp ASCENDING BY pernr.
    DELETE ADJACENT DUPLICATES FROM it_aend_post_tmp COMPARING pernr.

    LOOP AT it_aend_post_tmp ASSIGNING FIELD-SYMBOL(<it_aend>) WHERE pernr IS NOT INITIAL.
      "Check pa9400 existance
      me->check_pa9400_exists( EXPORTING pernr = <it_aend>-pernr begda = <it_aend>-begda endda = <it_aend>-endda
                                IMPORTING return = DATA(return) exists = exists ).
      "If entry does not exist, create it
      IF exists = abap_false.
        me->add_pa9400( EXPORTING pernr = <it_aend>-pernr cloud_id = <it_aend>-cloud_id begda = <it_aend>-begda endda = <it_aend>-endda simu = simu
                          IMPORTING return = return ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD post_it_aend.

    DATA: pernr                   TYPE pernr_d,
          count                   TYPE i VALUE 0,
          max                     TYPE i VALUE 0,
          prelp                   TYPE prelp,
          key                     TYPE bapipakey,
          return                  TYPE bapireturn1,
          it                      TYPE REF TO data,
          pernr_error             TYPE rsdsselopt_t,
          it_aend_tmp_hire        TYPE zmhp_tt_it_aend,
          it_aend_tmp_hire_err    TYPE zmhp_tt_it_aend,
          it_aend_tmp_hire_it9400 TYPE zmhp_tt_it_aend,
          it_aend_tmp             TYPE zmhp_tt_it_aend,
          it_aend_tmp_it0001      TYPE zmhp_tt_it_aend,
          it_aend_tmp_err         TYPE zmhp_tt_it_aend,
          pa9400_check            TYPE boole_d,
          no_change               TYPE boole_d,
          term_date               TYPE dats,
          term_pernr              TYPE pernr_d,
          term_cloud_id           TYPE zmhp_dd_element,
          return_tab              TYPE hrpad_return_tab,
          error_tab               TYPE hrpad_return_tab,
          error_bapi              TYPE bapiret1,
          error_hrpad             LIKE LINE OF error_tab,
          massn_txt               TYPE mntxt,
          begda_ext               TYPE zmhp_dd_date_ext,
          endda_ext               TYPE zmhp_dd_date_ext,
          it_create               TYPE boole_d,
          it_change               TYPE boole_d,
          changed_fields          TYPE ty_fields,
          is_rev_term             TYPE boole_d,
          exception               TYPE REF TO cx_root,
          has_error               TYPE boole_d,
          is_ok                   TYPE boole_d,
          masterdata_bl           TYPE REF TO  if_hrpa_masterdata_bl.

    FIELD-SYMBOLS: <struc>       TYPE any,
                   <infotype>    TYPE any,
                   <it_aend_tmp> TYPE zmhp_int_it_aend.


***-- initialize dcif masterdata
    CALL METHOD cl_hrpa_masterdata_factory=>get_business_logic
      IMPORTING
        business_logic = masterdata_bl.

    masterdata_bl->initialize( ).

    DATA(status_proceed) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '01' )
                                               ( sign = 'I' option = 'EQ' low = '08' )
                                               ( sign = 'I' option = 'EQ' low = '09' )
                                               ( sign = 'I' option = 'EQ' low = '10' ) ). "Simulated entries "JMB20210709 I

    LOOP AT me->status_handler->it_aend ASSIGNING FIELD-SYMBOL(<it_aend>) WHERE status IN status_proceed.                  "= '01' OR status = '08' OR status = '09'. "OR status = '03'.
      CLEAR: has_error.

*--------------------------------------------------------------------*
*   Check for previous errors
*--------------------------------------------------------------------*
      me->status_handler->check_for_error(
        EXPORTING
          cloud_id = CONV #( <it_aend>-cloud_id )
          sap_id = CONV #( <it_aend>-pernr )
          type = zmhp_cl_int_constants=>person
          int_run = <it_aend>-int_run
          IMPORTING
            has_error = has_error
          ).
      IF has_error = abap_true.
        CONTINUE.
      ENDIF.

*--------------------------------------------------------------------*
*   Initializing
*--------------------------------------------------------------------*
      CLEAR: no_change, it_change, it_create, changed_fields.
      "Set language
      zmhp_cl_int_utility=>get_spras_by_molga( EXPORTING molga = <it_aend>-molga
           IMPORTING spras = DATA(spras) langu = DATA(langu) ).

      "Get project
*      DATA(project) = zmhp_cl_int_utility=>check_project( <it_aend>-molga ).

      IF <it_aend>-endda = '00000000'.
        <it_aend>-endda = zmhp_cl_int_constants=>highdate.
      ENDIF.

      me->status_handler->aend_id = <it_aend>-aend_id.
      me->status_handler->begda = <it_aend>-begda.
      me->status_handler->endda = <it_aend>-endda.
      me->status_handler->molga = <it_aend>-molga.
      me->status_handler->cloud_id = <it_aend>-cloud_id.
      me->status_handler->sap_id = <it_aend>-pernr.
      me->status_handler->int_run = <it_aend>-int_run.

      DATA(infty_operation) = NEW zmhp_cl_int_it_operation( int_run = <it_aend>-int_run molga = <it_aend>-molga pernr = <it_aend>-pernr ).
      DATA(infty_delta) = NEW zmhp_cl_int_infty_delta( infty = <it_aend>-infty ).

      "if already an error for guid occurred go to next entry
      IF <it_aend>-cloud_id   IN pernr_error AND
         pernr_error        IS NOT INITIAL.
        count = count + 1.
        CONTINUE.
      ENDIF.


      "check for new content
      IF max EQ count.
        count = 0.
        CLEAR: is_rev_term.

        "get all it_aend entries with same pernr run id
        it_aend_tmp = COND zmhp_tt_it_aend( WHEN <it_aend>-action IS NOT INITIAL
                                            THEN VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( cloud_id = <it_aend>-cloud_id AND
                                                                                                                              int_run  = <it_aend>-int_run )  "AND
                                                                                                                              "action   = <it_aend>-action )
                                                                        ( ls_it_aend ) )
                                            ELSE VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( cloud_id EQ <it_aend>-cloud_id          AND
                                                                                                                              int_run  = <it_aend>-int_run )
*                                                                                                                                  action IS INITIAL AND
*                                                                                                                                ( status = zmhp_cl_int_constants=>booking_status-initial OR
*                                                                                                                                  status = zmhp_cl_int_constants=>booking_status-error_des ) )
                                                                        ( ls_it_aend ) ) ).

        max = lines( it_aend_tmp ).

      ENDIF.

*        IF <it_aend>-infty NE zmhp_cl_int_constants=>it0000 AND <it_aend>-action IS NOT INITIAL.
*          count = count + 1.
*          CONTINUE.
*        ENDIF.

      "Check if inactive process
      IF <it_aend>-infty = zmhp_cl_int_constants=>it0001 AND <it_aend>-active = 'I' AND <it_aend>-endda = zmhp_cl_int_constants=>highdate.
        it_aend_tmp_it0001 = COND zmhp_tt_it_aend( WHEN <it_aend>-action NE zmhp_cl_int_constants=>hire
                                                   THEN VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( cloud_id = <it_aend>-cloud_id            AND
                                                                                                                                     int_run  = <it_aend>-int_run             AND
                                                                                                                                     infty    = zmhp_cl_int_constants=>it0001 AND
                                                                                                                                     endda    = zmhp_cl_int_constants=>highdate )
                                                                               ( ls_it_aend ) ) ).
        DATA(count_it0001_cof) = lines( it_aend_tmp_it0001 ).
        IF count_it0001_cof = 1.
          CLEAR: <it_aend>-active.
        ENDIF.
      ENDIF.
      IF <it_aend>-infty  = zmhp_cl_int_constants=>it0001      AND
         <it_aend>-action = zmhp_cl_int_constants=>termination AND
         <it_aend>-endda  = zmhp_cl_int_constants=>highdate.
        CLEAR: <it_aend>-active.
      ENDIF.
      IF <it_aend>-active NE 'I'.
        "check if hire
*        IF <it_aend>-action NE zmhp_cl_int_constants=>hire.
        "if termination buffer termination date and pernr
        IF <it_aend>-action = zmhp_cl_int_constants=>termination AND <it_aend>-infty = zmhp_cl_int_constants=>it0000.
          IF <it_aend>-endda NE zmhp_cl_int_constants=>highdate.
            <it_aend>-begda = <it_aend>-endda + 1.
          ENDIF.
          <it_aend>-endda = zmhp_cl_int_constants=>highdate.
          IF term_date IS INITIAL.
            term_date = <it_aend>-begda.
            term_pernr = <it_aend>-pernr.
            term_cloud_id = <it_aend>-cloud_id.
          ELSE.
            IF term_cloud_id NE <it_aend>-cloud_id.
              term_date = <it_aend>-endda.
              term_cloud_id = <it_aend>-cloud_id.
              term_pernr = <it_aend>-pernr.
            ENDIF.
          ENDIF.
        ELSE.
          IF term_cloud_id NE <it_aend>-cloud_id.
            CLEAR: term_date, term_cloud_id, term_pernr.
          ENDIF.
        ENDIF.

        "default simulation status
        DATA(simu_tmp_cof) = simu.

        "create infotype structure
        CONCATENATE 'P' <it_aend>-infty INTO DATA(it_name_cof).
        CREATE DATA it TYPE (it_name_cof).
        ASSIGN it->* TO <struc>.

        "get infotype data
        MOVE-CORRESPONDING <it_aend> TO prelp.
        cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
          EXPORTING
            prelp = prelp
          IMPORTING
            pnnnn = <struc> ).


        IF no_change NE abap_true.

*--------------------------------------------------------------------*
*   Hire logic as special case seperated from normal processing logic
*--------------------------------------------------------------------*
          IF <it_aend>-action NE zmhp_cl_int_constants=>hire AND <it_aend>-infty NE zmhp_cl_int_constants=>it0000.
            CLEAR: <it_aend>-action.
          ENDIF.
          IF <it_aend>-action IS NOT INITIAL.
            count = count + 1.
            "PROCESS_ACTION
            "check for IT change
            infty_delta->check_infty_change( EXPORTING it_record     = <struc>
                                                       action        = <it_aend>-action
                                                       action_date   = term_date
                                             IMPORTING it_record_upd = <struc>
                                                       it_create     = it_create
                                                       it_change     = it_change
                                                       fields        = changed_fields ).
            "add changed fields
            status_handler->get_changed_fields( otype      = zmhp_cl_int_constants=>person
                                                fields     = changed_fields
                                                infty      = <it_aend>-infty
                                                it_record  = <struc>
                                                aend_id    = <it_aend>-aend_id
                                                simulation = simu_tmp_cof ).
            DATA(it_aend_tmp_action) = COND zmhp_tt_it_aend( WHEN <it_aend>-action IS NOT INITIAL
                                                THEN VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( cloud_id = <it_aend>-cloud_id AND
                                                                                                                                  int_run  = <it_aend>-int_run AND
                                                                                                                                  action   = <it_aend>-action )
                                                                            ( ls_it_aend ) ) ) .
            IF <it_aend>-action NE zmhp_cl_int_constants=>hire.
              "execute it specific logic before operation
              me->it_specific_logic( EXPORTING exec_option = zmhp_cl_int_constants=>before infty = <it_aend>-infty subty = <it_aend>-subty
                                               massn = <it_aend>-action changed_fields = changed_fields molga = <it_aend>-molga simu = simu
                                      IMPORTING return = return return_tab = return_tab
                                        CHANGING record = <struc> ).
            ENDIF.

            TRY.
                me->process_action( EXPORTING simu          = simu
                                              record = <struc>
                                 IMPORTING continue      = DATA(continue_cof)
                                       it_aend_error = DATA(it_aend_error_tmp_cof)
                                       it_aend_post  = DATA(it_aend_post_tmp_cof)
*                                            is_ok         = is_ok
                                           return    = return
                                           return_tab = return_tab
                                 CHANGING  it_aend       = <it_aend>
                                           it_aend_tab   = it_aend_tmp_action ). "it_aend_tmp ).
              CATCH cx_hrpa_invalid_buffer_access INTO exception.
              CATCH cx_hrpa_invalid_parameter INTO exception.
                return-type = zmhp_cl_int_constants=>error.
                return-id = 'ZMHP_HCM_INTEGRATION'.
                return-message_v1 = exception->get_text( ).
                return-number = zmhp_cl_int_constants=>msg_no-m35.

            ENDTRY.

            IF <it_aend>-action = zmhp_cl_int_constants=>hire.
              MODIFY me->status_handler->it_aend FROM <it_aend> TRANSPORTING pernr WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id.
              DATA(cloud_id_tmp) = <it_aend>-cloud_id.

              IF it_aend_error_tmp_cof IS INITIAL.
                "execute it specific logic after operation
                me->it_specific_logic( EXPORTING exec_option = zmhp_cl_int_constants=>after infty = <it_aend>-infty subty = <it_aend>-subty
                                                 massn = <it_aend>-action changed_fields = changed_fields molga = <it_aend>-molga simu = simu
                                        IMPORTING return = return return_tab = return_tab
                                          CHANGING record = <struc> ).
                DELETE me->status_handler->it_aend WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id AND pernr IS INITIAL.
                APPEND LINES OF it_aend_post_tmp_cof TO it_aend_post.
                APPEND LINES OF it_aend_tmp TO me->status_handler->del_it_aend.
                DATA(lines_hire) = lines( it_aend_tmp_action ).
                IF lines_hire EQ max.
                  COMMIT WORK AND WAIT.
                ELSEIF count EQ max.
                  IF simu = abap_false.

                    IF masterdata_bl IS BOUND.
                      TRY.
                          masterdata_bl->flush( no_commit = space ).
                          COMMIT WORK AND WAIT.
                        CATCH cx_hrpa_violated_assertion.
                      ENDTRY.
                    ENDIF.
                  ENDIF.
                ENDIF.

              ELSE.
                IF it_aend_error_tmp_cof IS NOT INITIAL.
                  ROLLBACK WORK.
                  APPEND VALUE #( sign = 'I' option = 'EQ' low = cloud_id_tmp ) TO pernr_error.
                  MODIFY me->status_handler->it_aend FROM <it_aend> TRANSPORTING status WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id.
                  APPEND LINES OF me->status_handler->it_aend TO it_aend_error.
                ENDIF.
              ENDIF.
              "Other processes then hire
            ELSE.
              READ TABLE return_tab ASSIGNING FIELD-SYMBOL(<return_check>) WITH KEY type = 'E'.
              IF sy-subrc = 0.

                error_hrpad = CORRESPONDING #( return ).
                IF error_hrpad IS NOT INITIAL.
                  APPEND error_hrpad TO error_tab.
                ENDIF.
                APPEND LINES OF return_tab TO error_tab.

                CLEAR: return.
                me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return hrpad_return = error_tab ).

                CLEAR: key, return, error_hrpad, error_tab.

                "set error status
                <it_aend>-status = zmhp_cl_int_constants=>booking_status-error.

                MODIFY it_aend_tmp FROM <it_aend> TRANSPORTING status WHERE status NE zmhp_cl_int_constants=>booking_status-error.

                APPEND VALUE #( sign = 'I' option = 'EQ' low = <it_aend>-cloud_id ) TO pernr_error.
                "COMMENTED
                it_aend_tmp_err = COND zmhp_tt_it_aend( WHEN <it_aend>-action = zmhp_cl_int_constants=>hire THEN VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( cloud_id = <it_aend>-cloud_id AND
                                                                                       int_run = <it_aend>-int_run ) ( ls_it_aend ) ) ELSE
                                                                                     VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( pernr EQ <it_aend>-pernr AND int_run = <it_aend>-int_run ) ( ls_it_aend ) ) ).
                DELETE it_aend_nochange WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id.
                DELETE it_aend_post WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id.
                APPEND LINES OF it_aend_tmp_err TO it_aend_error.
                "COMMENTED
*                    APPEND LINES OF it_aend_tmp TO it_aend_error.

                "in case several it_aend entries belong to same int_run, rollback for all infotype operations of int_run is needed
                IF max GT 1.
*                      count = max.
*                      IF <it_aend>-action IS INITIAL.
                  ROLLBACK WORK.
*                      ENDIF.
                ENDIF.
                CLEAR: changed_fields, return, return_tab, error_tab, error_hrpad, spras, langu.
                CONTINUE.
                "Success in IT Update
              ELSE.
                begda_ext = zmhp_cl_int_utility=>get_external_date( date = <it_aend>-begda ).
                endda_ext = zmhp_cl_int_utility=>get_external_date( date = <it_aend>-endda ).
                IF <it_aend>-action IS NOT INITIAL.
                  IF <it_aend>-infty = zmhp_cl_int_constants=>it0000.
                    massn_txt = zmhp_cl_int_utility=>read_massn_txt( sprsl = spras massn = <it_aend>-action ).
                    return = VALUE bapiret1( type = zmhp_cl_int_constants=>success
                                             id = zmhp_cl_int_constants=>msg_class_int
                                             number = zmhp_cl_int_constants=>msg_no-m20
                                             message_v1 = <it_aend>-pernr
                                             message_v2 =  begda_ext
                                             message_v3 = endda_ext
                                             message_v4 = massn_txt
                                           ).
                    CLEAR: massn_txt.
                  ENDIF.
                ENDIF.
                IF return-type IS INITIAL.
                  return = VALUE bapiret1( type = zmhp_cl_int_constants=>success
                                            id = zmhp_cl_int_constants=>msg_class_int
                                            number = zmhp_cl_int_constants=>msg_no-m16
                                            message_v1 = <it_aend>-pernr
                                            message_v2 = begda_ext
                                            message_v3 = endda_ext
                                            message_v4 = <it_aend>-infty
                                          ).
                ENDIF.

                error_hrpad = CORRESPONDING #( return ).
                APPEND error_hrpad TO error_tab.
                APPEND LINES OF return_tab TO error_tab.
                CLEAR: return, error_hrpad, return_tab.

                "execute it specific logic after operation
                me->it_specific_logic( EXPORTING exec_option = zmhp_cl_int_constants=>after infty = <it_aend>-infty subty = <it_aend>-subty
                                                 massn = <it_aend>-action changed_fields = changed_fields molga = <it_aend>-molga simu = simu
                                        IMPORTING return = return return_tab = return_tab
                                          CHANGING record = <struc> ).
                APPEND LINES OF return_tab TO error_tab.
                CLEAR: return_tab, return.
              ENDIF.

            ENDIF.


*--------------------------------------------------------------------*
*   Other then hire logic
*--------------------------------------------------------------------*
          ELSE.

            "in case more entries belong to actual integration run id
            IF max GT 1.
              simu_tmp_cof = abap_true.
            ENDIF.

            "enqueue pernr
            IF pernr NE <it_aend>-pernr OR
               pernr IS INITIAL.

              "dequeue old pernr
              IF pernr IS NOT INITIAL.
                CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
                  EXPORTING
                    number = pernr
                  IMPORTING
                    return = return.

                "check for enqueue pernr
                IF return-type EQ zmhp_cl_int_constants=>error.
                  me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return ).

                  CLEAR: pernr, return.
                ENDIF.
              ENDIF.

              CALL FUNCTION 'HR_EMPLOYEE_ENQUEUE'
                EXPORTING
                  number = <it_aend>-pernr
                IMPORTING
                  return = return.

              "check for enqueue pernr
              IF return-type EQ zmhp_cl_int_constants=>error.
                me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return ).

                CLEAR: pernr, return.

                "collect lines in error output table
                APPEND LINES OF it_aend_tmp TO it_aend_error.
                APPEND VALUE #( sign = 'I' option = 'EQ' low = <it_aend>-cloud_id ) TO pernr_error.
*                count = max.
                CLEAR: changed_fields, return.
                CONTINUE.
              ENDIF.

              pernr = <it_aend>-pernr.
            ENDIF.


*--------------------------------------------------------------------*
*   Start standard IT processing
*--------------------------------------------------------------------*
            count = count + 1.

            "check for IT change
            infty_delta->check_infty_change( EXPORTING it_record = <struc> action = <it_aend>-action action_date = term_date
                                             IMPORTING it_record_upd = <struc> it_create = it_create
                                                       it_change = it_change fields = changed_fields ).
            DATA(changed_fields_tmp_cof) = changed_fields.
            DATA(it_change_tmp_cof) = it_change.
            DATA(it_create_tmp_cof) = it_create.

            "execute it specific logic before operation
            TRY.
                me->it_specific_logic( EXPORTING exec_option = zmhp_cl_int_constants=>before infty = <it_aend>-infty subty = <it_aend>-subty
                                                 massn = <it_aend>-action changed_fields = changed_fields molga = <it_aend>-molga simu = simu
                                        IMPORTING return = return return_tab = return_tab
                                          CHANGING record = <struc> ).
              CATCH cx_hrpa_invalid_parameter.
                return = zmhp_cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
                error_hrpad = CORRESPONDING #( return ).
                APPEND error_hrpad TO error_tab.
            ENDTRY.
            APPEND LINES OF return_tab TO error_tab.
            CLEAR: return_tab, return, error_hrpad, changed_fields.

            "check for IT change
            infty_delta->check_infty_change( EXPORTING it_record = <struc> action = <it_aend>-action action_date = term_date
                                             IMPORTING it_record_upd = <struc> it_create = it_create
                                                       it_change = it_change fields = changed_fields ).

            IF changed_fields_tmp_cof IS NOT INITIAL AND changed_fields IS INITIAL.
              changed_fields = changed_fields_tmp_cof.
              it_change = it_change_tmp_cof.
              it_create = it_create_tmp_cof.

            ENDIF.
            CLEAR: changed_fields_tmp_cof, it_change_tmp_cof, it_create_tmp_cof.

            "add changed fields
            status_handler->get_changed_fields( otype = zmhp_cl_int_constants=>person
                                                fields = changed_fields
                                                infty = <it_aend>-infty
                                                it_record = <struc>
                                                aend_id = <it_aend>-aend_id
                                                simulation = simu_tmp_cof ).
            "execute action specific logic

            "If IT9400 and contingent becomes employee old pernr is terminated and new pernr is given -> delimit old IT9400
            IF <it_aend>-infty = '9400'.
              zmhp_cl_int_utility=>get_pernr_by_cloudid( EXPORTING cloud_id = CONV #( <it_aend>-cloud_id ) begda = <it_aend>-begda endda = <it_aend>-endda IMPORTING pernr = DATA(pernr_old_cof) ).
              IF pernr_old_cof NE <it_aend>-pernr.
                infty_operation->delimit_paxxxx(
                  EXPORTING
                    begda = <it_aend>-begda
                    endda = <it_aend>-endda
                    pernr = pernr_old_cof
                    infty = <it_aend>-infty
                    subty = <it_aend>-subty
                    record = <struc>
                    delimit_date = term_date
                    simu = simu
                    IMPORTING
                      return_tab = return_tab
                  ).
                APPEND LINES OF return_tab TO error_tab.
                CLEAR: return_tab.
              ENDIF.
            ENDIF.


            IF it_change = abap_true OR it_create = abap_true.

              "PROCESS CHANGES
              TRY.

                  me->process_changes( EXPORTING simu          = simu
                                                record = <struc>
                                   IMPORTING return    = return
                                             return_tab = return_tab
                                   CHANGING  it_aend       = <it_aend>
                                             it_aend_tab   = it_aend_tmp
                                             is_ok = is_ok ).
                CATCH cx_hrpa_invalid_buffer_access INTO exception.
                CATCH cx_hrpa_invalid_parameter INTO exception.
                  return-type = zmhp_cl_int_constants=>error.
                  return-id = 'ZMHP_HCM_INTEGRATION'.
                  return-message_v1 = exception->get_text( ).
                  return-number = zmhp_cl_int_constants=>msg_no-m35.

              ENDTRY.

              "check for operation errors
*              IF return-type EQ 'E'.
              READ TABLE return_tab ASSIGNING FIELD-SYMBOL(<return_check_process>) WITH KEY type = 'E'.
              IF sy-subrc = 0.

                error_hrpad = CORRESPONDING #( return ).
                IF error_hrpad IS NOT INITIAL.
                  APPEND error_hrpad TO error_tab.
                ENDIF.
                APPEND LINES OF return_tab TO error_tab.

                CLEAR: return.
                me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return hrpad_return = error_tab ).

                CLEAR: key, return, error_hrpad, error_tab.

                "set error status
                <it_aend>-status = zmhp_cl_int_constants=>booking_status-error.

                MODIFY it_aend_tmp FROM <it_aend> TRANSPORTING status WHERE status NE zmhp_cl_int_constants=>booking_status-error.

                APPEND VALUE #( sign = 'I' option = 'EQ' low = <it_aend>-cloud_id ) TO pernr_error.
                "COMMENTED
                it_aend_tmp_err = COND zmhp_tt_it_aend( WHEN <it_aend>-action = zmhp_cl_int_constants=>hire THEN VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( cloud_id = <it_aend>-cloud_id AND
                                                                                       int_run = <it_aend>-int_run ) ( ls_it_aend ) ) ELSE
                                                                                     VALUE zmhp_tt_it_aend( FOR ls_it_aend IN me->status_handler->it_aend WHERE ( pernr EQ <it_aend>-pernr AND int_run = <it_aend>-int_run ) ( ls_it_aend ) ) ).
                DELETE it_aend_nochange WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id.
                DELETE it_aend_post WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id.
                APPEND LINES OF it_aend_tmp_err TO it_aend_error.
                "COMMENTED
*                    APPEND LINES OF it_aend_tmp TO it_aend_error.

                "in case several it_aend entries belong to same int_run, rollback for all infotype operations of int_run is needed
                IF max GT 1.
*                      count = max.
*                      IF <it_aend>-action IS INITIAL.
                  ROLLBACK WORK.
*                      ENDIF.
                ENDIF.
                CLEAR: changed_fields, return, return_tab, error_tab, error_hrpad, spras, langu.
                CONTINUE.
                "Success in IT Update
              ELSE.
                begda_ext = zmhp_cl_int_utility=>get_external_date( date = <it_aend>-begda ).
                endda_ext = zmhp_cl_int_utility=>get_external_date( date = <it_aend>-endda ).
                IF <it_aend>-action IS NOT INITIAL.
                  IF <it_aend>-infty = zmhp_cl_int_constants=>it0000.
                    massn_txt = zmhp_cl_int_utility=>read_massn_txt( sprsl = spras massn = <it_aend>-action ).
                    return = VALUE bapiret1( type = zmhp_cl_int_constants=>success
                                             id = zmhp_cl_int_constants=>msg_class_int
                                             number = zmhp_cl_int_constants=>msg_no-m20
                                             message_v1 = <it_aend>-pernr
                                             message_v2 =  begda_ext
                                             message_v3 = endda_ext
                                             message_v4 = massn_txt
                                           ).
                    CLEAR: massn_txt.
                  ENDIF.
                ENDIF.
                IF return-type IS INITIAL.
                  return = VALUE bapiret1( type = zmhp_cl_int_constants=>success
                                            id = zmhp_cl_int_constants=>msg_class_int
                                            number = zmhp_cl_int_constants=>msg_no-m16
                                            message_v1 = <it_aend>-pernr
                                            message_v2 = begda_ext
                                            message_v3 = endda_ext
                                            message_v4 = <it_aend>-infty
                                          ).
                ENDIF.

                error_hrpad = CORRESPONDING #( return ).
                APPEND error_hrpad TO error_tab.
                CLEAR: return, error_hrpad.

                "execute it specific logic after operation
                  me->it_specific_logic( EXPORTING exec_option = zmhp_cl_int_constants=>after infty = <it_aend>-infty subty = <it_aend>-subty
                                                   massn = <it_aend>-action changed_fields = changed_fields molga = <it_aend>-molga simu = simu
                                          IMPORTING return = return return_tab = return_tab
                                            CHANGING record = <struc> ).
                APPEND LINES OF return_tab TO error_tab.
                CLEAR: return_tab, return.
              ENDIF.
              "No change to be booked
            ELSE.
*                  count = count + 1.
              no_change = abap_true.
            ENDIF.

            "in case several it_aend entries belong to same int_run, commit for all infotype operations of int_run is needed

            IF max EQ count.
              IF no_change NE abap_true.
                IF <it_aend_tmp> IS ASSIGNED.
                  CLEAR: <it_aend_tmp>.
                ENDIF.
                READ TABLE it_aend_tmp ASSIGNING <it_aend_tmp> WITH KEY int_run = <it_aend>-int_run cloud_id = <it_aend>-cloud_id aend_id = <it_aend>-aend_id.
                APPEND <it_aend> TO it_aend_post.
                <it_aend>-status = zmhp_cl_int_constants=>booking_status-success.
                DELETE it_aend_tmp WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id AND aend_id = <it_aend>-aend_id.
              ELSE.
                APPEND <it_aend> TO it_aend_nochange.
                <it_aend>-status = zmhp_cl_int_constants=>booking_status-nochange.
                DELETE it_aend_tmp WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id AND aend_id = <it_aend>-aend_id.
              ENDIF.
              "set success status
*            <it_aend>-status = zmhp_cl_int_constants=>booking_status-success.
*            MODIFY it_aend_tmp FROM <it_aend> TRANSPORTING status WHERE status NE zmhp_cl_int_constants=>booking_status-success.
*            APPEND LINES OF it_aend_tmp TO it_aend_post.

              count = max.
              me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return hrpad_return = error_tab ).
              "do commit only in case simulation is inactive
              IF simu = abap_false.

                "COMMENTED
*                CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
                IF masterdata_bl IS BOUND.
                  TRY.
                      masterdata_bl->flush( no_commit = space ).
                      COMMIT WORK AND WAIT.
                    CATCH cx_hrpa_violated_assertion.    "
                  ENDTRY.
                ENDIF.
*                  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.
                "COMMENTED
              ENDIF.
            ELSE.
              IF no_change NE abap_true.
                IF <it_aend_tmp> IS ASSIGNED.
                  CLEAR: <it_aend_tmp>.
                ENDIF.
                READ TABLE it_aend_tmp ASSIGNING <it_aend_tmp> WITH KEY int_run = <it_aend>-int_run cloud_id = <it_aend>-cloud_id aend_id = <it_aend>-aend_id.
                <it_aend>-status = zmhp_cl_int_constants=>booking_status-success.
                APPEND <it_aend> TO it_aend_post.
*                  APPEND <it_aend> TO me->status_handler->del_it_aend.
                DELETE it_aend_tmp WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id AND aend_id = <it_aend>-aend_id.
              ELSE.
                APPEND <it_aend> TO it_aend_nochange.
                <it_aend>-status = zmhp_cl_int_constants=>booking_status-nochange.
                DELETE it_aend_tmp WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id AND aend_id = <it_aend>-aend_id.
              ENDIF.
              me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return hrpad_return = error_tab ).

            ENDIF.

          ENDIF.
        ELSE.
          "endda IT < begda first IT0000 will not be handled
          APPEND <it_aend> TO it_aend_nochange.
          <it_aend>-status = zmhp_cl_int_constants=>booking_status-nochange.
          DELETE it_aend_tmp WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id AND aend_id = <it_aend>-aend_id.
          count = count + 1.
        ENDIF.
      ELSE.
        "Inactive process will not be handled
        APPEND <it_aend> TO it_aend_nochange.
        <it_aend>-status = zmhp_cl_int_constants=>booking_status-nochange.
        DELETE it_aend_tmp WHERE int_run = <it_aend>-int_run AND cloud_id = <it_aend>-cloud_id AND aend_id = <it_aend>-aend_id.
        IF max NE 0.
          count = count + 1.
        ENDIF.
      ENDIF.
      CLEAR: changed_fields, return, return_tab, error_tab, error_hrpad, spras, langu, it_aend_post_tmp_cof, cloud_id_tmp.


    ENDLOOP.

    "dequeue last pernr
    IF pernr IS NOT INITIAL.
      CALL FUNCTION 'HR_EMPLOYEE_DEQUEUE'
        EXPORTING
          number = pernr
        IMPORTING
          return = return.

      "check for enqueue pernr
      IF return-type EQ 'E'.
        me->status_handler->add_log_message( aend_id = <it_aend>-aend_id bapiret1 = return ).

      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD prepare_protocol.

    "Check message handler
    IF message_handler IS BOUND.

      "prepare layout options
      DATA(layout) = VALUE slis_layout_alv( zebra = 'X'
                                               colwidth_optimize = 'X' ).

      DATA: root_node TYPE hrpad_pal_node_key VALUE 'ROOT'.

      "Define structure of successful and failed IT_AEND entries ALV table
      message_handler->if_hrpay00_pal_services~create_fcat( EXPORTING i_structure_name = 'ZMHP_INT_IT_AEND'
                                                            IMPORTING et_fcat          = DATA(fcat_it_aend) ).

      "add post table
*      IF it_aend_post IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*          EXPORTING
*            i_parent_node_key = root_node
*            it_fcat           = fcat_it_aend
*            it_append_table   = it_aend_post
*            is_layout         = layout
*            i_node_txt        = COND string( WHEN simu EQ abap_true
*                                             THEN TEXT-005
*                                             ELSE TEXT-001 ) ).
*      ENDIF.
*
*      "add error table
*      IF it_aend_error IS NOT INITIAL.
*        message_handler->if_hrpay00_pal_services~add_table(
*          EXPORTING
*            i_parent_node_key = root_node
*            it_fcat           = fcat_it_aend
*            it_append_table   = it_aend_error
*            is_layout         = layout
*            i_node_txt        = COND string( WHEN simu EQ abap_true
*                                             THEN TEXT-006
*                                             ELSE TEXT-002 ) ).
*      ENDIF.
        message_handler->if_hrpay00_pal_services~add_table(
          EXPORTING
            i_parent_node_key = root_node
            it_fcat           = fcat_it_aend
            it_append_table   = me->status_handler->new_it_aend
            is_layout         = layout
            i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-009
                                             ELSE TEXT-010 ) ).
      "add skipped pernr's
      IF skipped_pernrs IS NOT INITIAL.
        message_handler->if_hrpay00_pal_services~add_table(
        EXPORTING
          i_parent_node_key = root_node
          it_append_table   = skipped_pernrs
          is_layout         = layout
          i_node_txt        = COND string( WHEN simu EQ abap_true
                                             THEN TEXT-008
                                             ELSE TEXT-004 ) ).
      ENDIF.

      "add message table
      message_handler->if_hrpay00_pal_services~add_messages(
        EXPORTING
          i_parent_node_key = root_node
      ).

      "show pals
      CALL METHOD message_handler->display_pal.

    ENDIF.

  ENDMETHOD.


  METHOD process_action.
    DATA: exception TYPE REF TO cx_root.
    CASE it_aend-action.
        "Handle Hire
      WHEN zmhp_cl_int_constants=>hire.
*    TRY.
        me->handle_hire( EXPORTING simu          = simu
                         IMPORTING continue      = continue
                                   it_aend_error = it_aend_error
                                   it_aend_post  = it_aend_post
*                                            is_ok         = is_ok
                                   return_tab    = return_tab
                         CHANGING  it_aend       = it_aend
                                   it_aend_tab   = it_aend_tab ).
*      CATCH cx_hrpa_invalid_buffer_access INTO exception.
*      CATCH cx_hrpa_invalid_parameter INTO exception.
*        return-type = zmhp_cl_int_constants=>error.
*        return-id = 'ZMHP_HCM_INTEGRATION'.
*        return-message_v1 = exception->get_text( ).
*        return-number = zmhp_cl_int_constants=>msg_no-m35.
*
*    ENDTRY.

        "Handle Assignment Changes
*    TRY.
      WHEN zmhp_cl_int_constants=>org_change.
        me->handle_org_change( EXPORTING simu          = simu
                                          record = record
                         IMPORTING continue      = continue
                                   it_aend_error = it_aend_error
                                   it_aend_post  = it_aend_post
*                                            is_ok         = is_ok
                                   return_tab    = return_tab
                         CHANGING  it_aend       = it_aend
                                   it_aend_tab   = it_aend_tab ).

*      catch cx_hrpa_invalid_buffer_access into exception.
*      CATCH cx_hrpa_invalid_parameter INTO exception.
*        return-type = zmhp_cl_int_constants=>error.
*        return-id = 'ZMHP_HCM_INTEGRATION'.
*        return-message_v1 = exception->get_text( ).
*        return-number = zmhp_cl_int_constants=>msg_no-m35.
*
*    ENDTRY.

        "Handle Termination not needed?
*    TRY.
      WHEN zmhp_cl_int_constants=>termination.
        me->handle_termination( EXPORTING simu          = simu
                                          record = record
                         IMPORTING continue      = continue
                                   it_aend_error = it_aend_error
                                   it_aend_post  = it_aend_post
*                                            is_ok         = is_ok
                                   return_tab    = return_tab
                         CHANGING  it_aend       = it_aend
                                   it_aend_tab   = it_aend_tab ).

*      CATCH cx_hrpa_invalid_buffer_access INTO exception.
*      CATCH cx_hrpa_invalid_parameter INTO exception.
*        return-type = zmhp_cl_int_constants=>error.
*        return-id = 'ZMHP_HCM_INTEGRATION'.
*        return-message_v1 = exception->get_text( ).
*        return-number = zmhp_cl_int_constants=>msg_no-m35.
*
*    ENDTRY.

        "Handle Rehire

        "Handle Reverse Termination


    ENDCASE.
  ENDMETHOD.


  METHOD process_changes.
    DATA: exception TYPE REF TO cx_root.
    FIELD-SYMBOLS: <fs_it>         TYPE any,
                   <fs_it_upd>     TYPE any,
                   <fs_it_old_upd> TYPE any,
                   <ft_it_old>     TYPE STANDARD TABLE.
    DATA(infty_operation) = NEW zmhp_cl_int_it_operation( int_run = it_aend-int_run molga = it_aend-molga pernr = it_aend-pernr ).
    ASSIGN record TO <fs_it>.


    infty_operation->update_paxxxx_dcif(
    EXPORTING
      begda = it_aend-begda
      endda = it_aend-endda
      data = <fs_it>
      infty = it_aend-infty
      IMPORTING
        is_ok = is_ok
        messages = DATA(messages)
    ).

    return_tab = zmhp_cl_int_utility=>map_msg_tab( messages ).
  ENDMETHOD.


  METHOD start_post.

    "call post for all infotypes, except 2011 due to special handling
    me->post_it_aend(
      EXPORTING
        simu             = simu
      IMPORTING
        it_aend_post     = it_aend_post
        it_aend_error    = it_aend_error
        it_aend_nochange = it_aend_nochange
      CHANGING
        message_handler  = message_handler ).


    "check if mapping IT exists and add mapping IT if necessary
    me->perform_pa9400_check( EXPORTING it_aend_post = it_aend_post
                                                simu = simu
                              IMPORTING exists = DATA(exists) ).
    "puffer status update
    me->update_status( EXPORTING simu = simu ).

    "persist status update
    DATA(is_ok) = me->status_handler->persist_data( source = zmhp_cl_int_constants=>poster simu = simu ).

    "Check simulation status and rollback if needed
    IF simu  EQ abap_true OR
       is_ok EQ abap_false.
      ROLLBACK WORK.
    ENDIF.

  ENDMETHOD.


  METHOD update_status.
    DATA: cloud_id_rtab   TYPE /sdf/calm_range_tt,
          cloud_id_single LIKE LINE OF cloud_id_rtab.
    "Update Success
    LOOP AT me->it_aend_post ASSIGNING FIELD-SYMBOL(<it_aend_post>).
      me->status_handler->set_status( aend_id = <it_aend_post>-aend_id status = zmhp_cl_int_constants=>booking_status-success ).
    ENDLOOP.
    "Update Error
    LOOP AT me->it_aend_error ASSIGNING FIELD-SYMBOL(<it_aend_error>).
      me->status_handler->set_status( aend_id = <it_aend_error>-aend_id status = zmhp_cl_int_constants=>booking_status-error ).
    ENDLOOP.
    "Update No Change
    LOOP AT me->it_aend_nochange ASSIGNING FIELD-SYMBOL(<it_aend_nochange>).
      me->status_handler->set_status( aend_id = <it_aend_nochange>-aend_id status = zmhp_cl_int_constants=>booking_status-nochange ).
    ENDLOOP.
    "Update Locked
    LOOP AT me->it_aend_locked ASSIGNING FIELD-SYMBOL(<it_aend_locked>).
      me->status_handler->set_status( aend_id = <it_aend_locked>-aend_id status = zmhp_cl_int_constants=>booking_status-pernr_locked ).
    ENDLOOP.

    "Set simulated status if simulation run
    IF simu = abap_true.
      me->status_handler->simu_it_aend = me->status_handler->new_it_aend.
      LOOP AT me->status_handler->simu_it_aend ASSIGNING FIELD-SYMBOL(<simu_it_aend>).
        IF <simu_it_aend>-action = zmhp_cl_int_constants=>hire.
          cloud_id_single-sign = 'I'.
          cloud_id_single-option = 'EQ'.
          cloud_id_single-low = <simu_it_aend>-cloud_pernr.
          IF line_exists( cloud_id_rtab[ table_line = cloud_id_single-low ] ).
          ELSE.
            APPEND cloud_id_single TO cloud_id_rtab.
          ENDIF.
          CLEAR: <simu_it_aend>-pernr, cloud_id_single.
        ELSEIF <simu_it_aend>-cloud_pernr IN cloud_id_rtab.
          CLEAR: <simu_it_aend>-pernr.
        ENDIF.
        <simu_it_aend>-status = zmhp_cl_int_constants=>booking_status-simulated.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
