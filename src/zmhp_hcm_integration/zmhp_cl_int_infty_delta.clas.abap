class ZMHP_CL_INT_INFTY_DELTA definition
  public
  final
  create public .

public section.

  types:
    tr_field_range TYPE RANGE OF dfies-fieldname .
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

  data BEGDA type DATS .
  data ENDDA type DATS .
  data INFTY type INFTY .
  data MOLGA type MOLGA .
  data PERNR type PERNR_D .
  data OBJID type HROBJID .
  data OTYPE type OTYPE .
  data LDATE type LDATE .
  data USER2 type HR_USRFLD .

  class-methods BUILD_IT_AEND_UP
    importing
      !TCLAS type PSPAR-TCLAS
      !INNNN type PRELP
      !PSAVE type PRELP
    exporting
      !MESSAGE type BAPIRET1 .
  methods CHECK_FIELDS
    importing
      !COMP type DDFIELDS
      !IT_RECORD_NEW type ANY
      !IT_RECORD_OLD type ANY
    exporting
      !IT_RECORD_NEW_UPD type ANY
      !IT_RECORD_OLD_UPD type ANY
      !HAS_CHANGE type BOOLEAN
      !FIELDS type TY_FIELDS .
  methods GET_FIELDS_IT_CREATE
    importing
      !IT_RECORD_NEW type ANY
    exporting
      !FIELDS type TY_FIELDS .
  methods CLEAR_FIELDS
    importing
      !COMP type DDFIELDS
      !IT_RECORD_NEW type ANY
      !IT_RECORD_OLD type ANY
    exporting
      !IT_RECORD_NEW_UPD type ANY
      !IT_RECORD_OLD_UPD type ANY .
  methods BUILD_REL_FIELDS
    returning
      value(RR_REL_FIELDS) type ZMHP_CL_INT_INFTY_DELTA=>TR_FIELD_RANGE .
  methods CONSTRUCTOR
    importing
      !INFTY type INFTY .
  methods CHECK_INFTY_CHANGE
    importing
      !ACTION_DATE type DATS optional
      !ACTION type MASSN optional
      !IT_RECORD type ANY
    exporting
      !IT_CHANGE type BOOLEAN
      !IT_CREATE type BOOLEAN
      !IT_RECORD_UPD type ANY
      !FIELDS type TY_FIELDS .
  methods CHECK_INFTY_CHANGE_OM
    importing
      !HRP_OLD type ANY
      !IT_RECORD type ANY
    exporting
      !IT_CHANGE type BOOLEAN
      !IT_CREATE type BOOLEAN
      !IT_RECORD_UPD type ANY
      !FIELDS type TY_FIELDS
      !RETURN_TAB type HRPAD_RETURN_TAB .
  methods CHECK_INFTY_CHANGE_PA
    importing
      !IT_RECORD type ANY
    exporting
      !IT_CHANGE type BOOLEAN
      !IT_CREATE type BOOLEAN
      !IT_RECORD_UPD type ANY
      !FIELDS type TY_FIELDS .
  class-methods CHECK_INFTY_CHANGE_ZXPADU02
    importing
      !IS_PRELP_NEW type PRELP
      !IS_PRELP_OLD type PRELP
    returning
      value(RS_IT_AENDUP) type ZMHP_INT_IT_AEUP .
  methods TRANSFER_INFTY_DATA
    importing
      !INFTY type INFTY
      !IT_RECORD_OLD type ANY
      !IT_RECORD_NEW type ANY
    exporting
      !IT_RECORD_NEW_UPD type ANY .
  class-methods TRANSFER_INFTY_DATA_V2
    importing
      !INFTY type INFTY
      !PERNR type PERNR_D
      !STIDAT type DATS
      !FIELDS type ZMHP_CL_INT_INFTY_PROC_XML=>T_FIELDS
    changing
      !IT_AEND type ZMHP_INT_IT_AEND .
  class-methods GET_LAST_INFOTYPE
    importing
      !PERNR type PERNR_D
      !INFTY type INFTY
      !SUBTY type SUBTY
      !STIDAT type DATS
    exporting
      value(DATA) type ANY .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CLASS-METHODS is_rel_field .
ENDCLASS.



CLASS ZMHP_CL_INT_INFTY_DELTA IMPLEMENTATION.


  method BUILD_IT_AEND_UP.
  endmethod.


  METHOD build_rel_fields.
    DATA: field_range LIKE LINE OF rr_rel_fields.
    IF me->molga IS INITIAL.
      me->molga = zmhp_cl_int_utility=>get_molga( pernr = me->pernr begda = me->begda endda = me->endda ).
    ENDIF.
    DATA(lo_cust) = NEW zmhp_cl_int_customizing( infty = me->infty molga = me->molga ).

    lo_cust->read_rel_fields( IMPORTING rel_fields = DATA(rel_fields) is_ok = DATA(is_ok) ).

    IF is_ok = abap_true.
      field_range-sign = 'I'.
      field_range-option = 'EQ'.
      LOOP AT rel_fields ASSIGNING FIELD-SYMBOL(<fs_field>).
        CLEAR: field_range-low.
        field_range-low = <fs_field>-field.
        APPEND field_range TO rr_rel_fields.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD check_fields.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data,
          ls_fields      LIKE LINE OF fields.
*    ASSIGN it_record_new TO FIELD-SYMBOL(<fs_it_new>).
*    ASSIGN it_record_old TO FIELD-SYMBOL(<fs_it_old>).
    CLEAR: has_change.
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string( 'P' && me->infty ) ).
    CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_new->* TO FIELD-SYMBOL(<fs_it_new>).
    CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_old->* TO FIELD-SYMBOL(<fs_it_old>).
    <fs_it_new> = it_record_new.
    <fs_it_old> = it_record_old.
    IF me->infty = zmhp_cl_int_constants=>it1001.
      DATA old_sobid TYPE sobid.
      ASSIGN COMPONENT 'SCLAS' OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<sclas>).
    ENDIF.
    "Processing relevant data
    DATA(rel_fields) = me->build_rel_fields( ).
    IF rel_fields IS NOT INITIAL.
      LOOP AT comp INTO DATA(ls_comp) WHERE fieldname IN rel_fields.
*   clear not needed fields and
        ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_field_new>).
        IF sy-subrc IS NOT INITIAL.
*            is_ok = abap_false.
        ENDIF.
        IF ( <fs_field_new> IS ASSIGNED )  AND ( <fs_field_new> IS NOT INITIAL ).
          ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_field_old>).
          IF sy-subrc IS NOT INITIAL.
*            is_ok = abap_false.
          ENDIF.
          IF <fs_field_old> IS ASSIGNED AND <fs_field_new> IS ASSIGNED.
            TRANSLATE <fs_field_old> TO UPPER CASE.
            TRANSLATE <fs_field_new> TO UPPER CASE.
            IF <sclas> IS ASSIGNED AND <sclas> = zmhp_cl_int_constants=>costcenter.
              old_sobid = <fs_field_old>.
              <fs_field_old> = <fs_field_old>+0(10).
            ENDIF.
            IF <fs_field_new> IS NOT INITIAL AND <fs_field_new> NE <fs_field_old>.


              has_change = abap_true.
              ls_fields-field = ls_comp-fieldname.
              ls_fields-field_old = <fs_field_old>.
              ls_fields-field_new = <fs_field_new>.
              APPEND ls_fields TO fields.
            ELSEIF  <fs_field_new> = '0.00' AND <fs_field_new> NE <fs_field_old>.
              has_change = abap_true.
              ls_fields-field = ls_comp-fieldname.
              ls_fields-field_old = <fs_field_old>.
              ls_fields-field_new = <fs_field_new>.
              APPEND ls_fields TO fields.
            ELSEIF  <fs_field_new> = '0' AND <fs_field_new> NE <fs_field_old>.
              has_change = abap_true.
              ls_fields-field = ls_comp-fieldname.
              ls_fields-field_old = <fs_field_old>.
              ls_fields-field_new = <fs_field_new>.
              APPEND ls_fields TO fields.
            ENDIF.
            IF <sclas> IS ASSIGNED AND <sclas> = zmhp_cl_int_constants=>costcenter.
              <fs_field_old> = old_sobid.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDLOOP.
    ELSE.

    ENDIF.
    it_record_new_upd = <fs_it_new>.
    it_record_old_upd = <fs_it_old>.
  ENDMETHOD.


  METHOD check_infty_change.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data,
          lv_pernr       TYPE pernr_d,
          lv_begda       TYPE dats,
          lv_endda       TYPE dats,
          subty          TYPE subty.
    FIELD-SYMBOLS: <fs_it_new>     TYPE any,
                   <fs_it_new_upd> TYPE any,
                   <fs_it_old_upd> TYPE any,
                   <ft_it_old>     TYPE STANDARD TABLE.
    CLEAR: it_create, it_change, fields.
    ASSIGN it_record TO <fs_it_new>.
    IF me->infty = '0041' AND action NE zmhp_cl_int_constants=>hire.
    ELSE.
      ASSIGN COMPONENT zmhp_cl_int_constants=>pernr OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_pernr>).
      me->pernr = <fs_pernr>.
      ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_begda>).
      me->begda = <fs_begda>.
      ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_endda>).
      me->endda = <fs_endda>.
      ASSIGN COMPONENT zmhp_cl_int_constants=>subty OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_subty>).
      subty = <fs_subty>.

      lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string( 'P' && me->infty ) ).
      lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
      CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
      ASSIGN lr_table->* TO <ft_it_old>.

      IF me->pernr IS NOT INITIAL AND action NE zmhp_cl_int_constants=>hire.
        CALL FUNCTION 'HR_READ_INFOTYPE'
          EXPORTING
*           TCLAS     = 'A'
            pernr     = me->pernr
            infty     = me->infty
            begda     = me->endda
            endda     = me->endda
*           SPRPS     = '*'
*           BYPASS_BUFFER = ' '
*           LEGACY_MODE   = ' '
*     IMPORTING
          TABLES
            infty_tab = <ft_it_old>
* EXCEPTIONS
*           INFTY_NOT_FOUND       = 1
*           INVALID_INPUT = 2
*           OTHERS    = 3
          .
        IF sy-subrc <> 0.
* Implement suitable error handling here
        ENDIF.

        READ TABLE <ft_it_old> ASSIGNING FIELD-SYMBOL(<fs_it_old>) INDEX 1.
        IF subty IS NOT INITIAL.
          LOOP AT <ft_it_old> ASSIGNING <fs_it_old>.
            ASSIGN COMPONENT zmhp_cl_int_constants=>subty OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_subty_old>).
            IF <fs_subty_old> = subty.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF <fs_subty_old> IS ASSIGNED.
            IF <fs_subty_old> NE subty.
              UNASSIGN <fs_it_old>.
            ENDIF.
          ENDIF.
*          ELSE.
*          LOOP AT <ft_it_old> ASSIGNING <fs_it_old>.
        ENDIF.
        DATA(lt_comp) = lr_structdescr->get_ddic_field_list( ).

        IF <fs_it_new> IS ASSIGNED AND <fs_it_old> IS ASSIGNED.
          CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
          ASSIGN lr_struc_new->* TO <fs_it_new_upd>.
*      <fs_it_new_upd> = <fs_it_new>.
          CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
          ASSIGN lr_struc_old->* TO <fs_it_old_upd>.
          "Clearing not relevant data
          me->clear_fields( EXPORTING comp = lt_comp it_record_new = <fs_it_new>  it_record_old = <fs_it_old>
                            IMPORTING it_record_new_upd = <fs_it_new_upd> it_record_old_upd = <fs_it_old_upd> ).
          "Check for change in relevant data
          me->check_fields( EXPORTING comp = lt_comp it_record_new = <fs_it_new>  it_record_old = <fs_it_old>
                            IMPORTING it_record_new_upd = <fs_it_new_upd> it_record_old_upd = <fs_it_old_upd> has_change = it_change
                                      fields = fields ).

          IF it_change = abap_true OR action = zmhp_cl_int_constants=>termination.
            "Fill record with old data
            IF it_change = abap_false AND action = zmhp_cl_int_constants=>termination AND me->endda NE zmhp_cl_int_constants=>highdate.
            ELSE.
*              me->transfer_infty_data( EXPORTING infty = me->infty it_record_old = <fs_it_old> it_record_new = <fs_it_new>
*                                       IMPORTING it_record_new_upd = <fs_it_new_upd> ).
              IF action = zmhp_cl_int_constants=>termination AND me->infty NE zmhp_cl_int_constants=>it0000.
*              ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_it_new_upd> TO FIELD-SYMBOL(<fs_begda_action>).
*              ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_begda_old>).
*              <fs_begda_action> = <fs_begda_old>.
*              ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <fs_it_new_upd> TO FIELD-SYMBOL(<fs_endda_action>).
*              <fs_endda_action> = action_date - 1.
              ENDIF.
              it_record_upd = <fs_it_new_upd>.
              it_change = abap_true.
            ENDIF.
          ELSEIF it_change = abap_false AND it_create = abap_false AND action NE zmhp_cl_int_constants=>termination AND me->infty = zmhp_cl_int_constants=>it0001.
*            me->transfer_infty_data( EXPORTING infty = me->infty it_record_old = <fs_it_old> it_record_new = <fs_it_new>
*                           IMPORTING it_record_new_upd = <fs_it_new_upd> ).
            it_record_upd = <fs_it_new_upd>.
          ENDIF.

          "No old record
        ELSEIF <fs_it_old> IS NOT ASSIGNED.
          it_create = abap_true.
        ENDIF.
      ELSE.
        it_create = abap_true.
      ENDIF.

      IF it_create IS INITIAL AND it_change IS INITIAL AND action = zmhp_cl_int_constants=>org_change AND me->infty = zmhp_cl_int_constants=>it0000.
        ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_begda_old>).
        IF <fs_begda_old> IS ASSIGNED.
          IF me->begda NE <fs_begda_old>.
            it_change = abap_true.
          ENDIF.
        ENDIF.
*            it_change = abap_true.
      ENDIF.

      IF it_create = abap_true.
        me->get_fields_it_create( EXPORTING it_record_new = <fs_it_new>
                           IMPORTING  fields = fields ).
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_infty_change_om.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data,
          delimit_date   TYPE dats,
          return         TYPE bapiret1,
          hrpad_return   TYPE hrpad_return,
          check_date     TYPE dats,
          langu          TYPE spras,
          subty          TYPE subty.
    FIELD-SYMBOLS: <fs_it_new>     TYPE any,
                   <fs_it_new_upd> TYPE any,
                   <fs_it_old_upd> TYPE any,
                   <hrp_old>       TYPE any,
                   <ft_it_old>     TYPE STANDARD TABLE.
    CLEAR: it_change, it_create.
    ASSIGN it_record TO <fs_it_new>.
    ASSIGN hrp_old TO <hrp_old>.

    ASSIGN COMPONENT zmhp_cl_int_constants=>objid OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_objid>).
    IF <fs_objid> IS ASSIGNED AND <fs_objid> IS INITIAL AND me->infty = zmhp_cl_int_constants=>it1000.
      ASSIGN COMPONENT zmhp_cl_int_constants=>objid OF STRUCTURE <hrp_old> TO FIELD-SYMBOL(<fs_objid_old>).
      IF <fs_objid_old> IS ASSIGNED.
        me->objid = <fs_objid_old>.
      ENDIF.
    ENDIF.
    IF me->objid IS INITIAL AND <fs_objid> IS ASSIGNED.
      me->objid = <fs_objid>.
    ENDIF.
    ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_begda>).
    me->begda = <fs_begda>.
    ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_endda>).
    me->endda = <fs_endda>.
    ASSIGN COMPONENT zmhp_cl_int_constants=>otype OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_otype>).
    me->otype = <fs_otype>.
*    ASSIGN COMPONENT zmhp_cl_int_constants=>subty OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_subty>).
*    subty = <fs_subty>.
*    ASSIGN COMPONENT zmhp_cl_int_constants=>langu OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_langu>).
*    langu = <fs_langu>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string( 'P' && me->infty ) ).
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <ft_it_old>.

    check_date = me->begda - 1.
    IF me->objid IS NOT INITIAL.
      "If there is no current record check if there is a record ending with begda - 1
*      IF <hrp_old> IS INITIAL.
*        me->read_hrpxxxx(
*        EXPORTING
*          begda = check_date
*          endda = check_date
*          infty = me->infty
*          langu = CONV #( spras )
*          otype = me->otype
*          sap_id = me->sap_id
*          subty = subty
*          plvar = CONV #( zmhp_cl_int_constants=>plvar )
*          IMPORTING
*            hrp_old = <hrp_old>
*            return = return ).
*      ENDIF.
      IF <hrp_old> IS INITIAL.
        IF me->infty = zmhp_cl_int_constants=>it1000.
          CALL FUNCTION 'RH_READ_INFTY_1000'
            EXPORTING
*             AUTHORITY        = 'DISP'
              with_stru_auth   = ' '
              plvar            = '01'
              otype            = me->otype
              objid            = me->objid
*             ISTAT            = ' '
*             EXTEND           = 'X'
              begda            = check_date
              endda            = check_date
*             CONDITION        = '00000'
*             SORT             = 'X'
            TABLES
              i1000            = <ft_it_old>
*             OBJECTS          =
            EXCEPTIONS
              nothing_found    = 1
              wrong_condition  = 2
              wrong_parameters = 3
              OTHERS           = 4.
          IF sy-subrc <> 0.
* Implement suitable error handling here
            return = zmhp_cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
            IF return IS NOT INITIAL.
              hrpad_return = CORRESPONDING #( return ).
              APPEND hrpad_return TO return_tab.
              CLEAR: return, hrpad_return.
            ENDIF.
          ENDIF.
        ELSE.
          CALL FUNCTION 'RH_READ_INFTY_NNNN'
            EXPORTING
*             AUTHORITY             = 'DISP'
              with_stru_auth        = ' '
              plvar                 = '01'
              otype                 = me->otype
              objid                 = me->objid
              infty                 = me->infty
*             ISTAT                 = ' '
*             EXTEND                = 'X'
*             SUBTY                 = ' '
              begda                 = check_date
              endda                 = check_date
*             CONDITION             = '00000'
*             INFTB                 = '1'
*             SORT                  = 'X'
            TABLES
              innnn                 = <ft_it_old>
*             OBJECTS               =
            EXCEPTIONS
              nothing_found         = 1
              wrong_condition       = 2
              infotyp_not_supported = 3
              wrong_parameters      = 4
              OTHERS                = 5.
          IF sy-subrc <> 0.
* Implement suitable error handling here
            return = zmhp_cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*          zmhp_cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                            IMPORTING message = return-message ).
            IF return IS NOT INITIAL.
              hrpad_return = CORRESPONDING #( return ).
              APPEND hrpad_return TO return_tab.
              CLEAR: return, hrpad_return.
            ENDIF.
          ENDIF.

        ENDIF.
      ENDIF.

      IF <ft_it_old> IS ASSIGNED.
        READ TABLE <ft_it_old> ASSIGNING FIELD-SYMBOL(<fs_it_old>) INDEX 1.
        DATA(lt_comp) = lr_structdescr->get_ddic_field_list( ).
        IF <fs_it_old> IS NOT ASSIGNED.
          IF <hrp_old> IS NOT INITIAL.
            ASSIGN <hrp_old> TO <fs_it_old>.
          ENDIF.
        ENDIF.
        IF <fs_it_new> IS ASSIGNED AND <fs_it_old> IS ASSIGNED.
          CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
          ASSIGN lr_struc_new->* TO <fs_it_new_upd>.
*      <fs_it_new_upd> = <fs_it_new>.
          CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
          ASSIGN lr_struc_old->* TO <fs_it_old_upd>.
          "Clearing not relevant data
          me->clear_fields( EXPORTING comp = lt_comp it_record_new = <fs_it_new>  it_record_old = <fs_it_old>
                            IMPORTING it_record_new_upd = <fs_it_new_upd> it_record_old_upd = <fs_it_old_upd> ).
          "Check for change in relevant data
          me->check_fields( EXPORTING comp = lt_comp it_record_new = <fs_it_new>  it_record_old = <fs_it_old>
                            IMPORTING it_record_new_upd = <fs_it_new_upd> it_record_old_upd = <fs_it_old_upd> has_change = it_change
                                      fields = fields ).

          IF it_change = abap_true.
            "Fill record with old data
            me->transfer_infty_data( EXPORTING infty = me->infty it_record_old = <fs_it_old> it_record_new = <fs_it_new>
                                     IMPORTING it_record_new_upd = <fs_it_new_upd> ).
            it_record_upd = <fs_it_new_upd>.

          ENDIF.

          "No old record
        ELSEIF <fs_it_old> IS NOT ASSIGNED.
          it_create = abap_true.
        ENDIF.
      ELSE.
        it_create = abap_true.
      ENDIF.
    ELSE.
      it_create = abap_true.
    ENDIF.

    IF it_create = abap_true.
      me->get_fields_it_create( EXPORTING it_record_new = <fs_it_new>
                         IMPORTING  fields = fields ).
    ENDIF.

    "Delimit old IT if available
*    IF <fs_it_old> IS NOT INITIAL.
*      delimit_date = me->begda - 1.
*      ASSIGN COMPONENT 'BEGDA' OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_begda_old>).
*      IF <fs_begda_old> NE me->begda.
*        CALL FUNCTION 'RH_CUT_INFTY'
*          EXPORTING
**           LOAD               = 'X'
*            gdate              = delimit_date
*            histo              = ' '
**           DEL_SUCC           = ' '
*            vtask              = 'D'
**           ORDER_FLG          = 'X'
**           COMMIT_FLG         = 'X'
*            authy              = ' '
**           PPPAR_IMP          =
**           KEEP_LUPD          =
**           WORKF_ACTV         = 'X'
*          TABLES
*            innnn              = <ft_it_old>
**           ILFCODE            =
*          EXCEPTIONS
*            error_during_cut   = 1
*            no_authorization   = 2
*            gdate_before_begda = 3
*            cut_of_timco_one   = 4
*            corr_exit          = 5
*            OTHERS             = 6.
*
*      ELSE.
*        CALL FUNCTION 'RH_DELETE_OBJECT'
*          EXPORTING
*            plvar                        = '01'
*            otype                        = 'O'
*            objid                        = me->objid
*            vtask                        = 'D'
**           ORDER_FLG                    = 'X'
**           COMMIT_FLG                   = 'X'
*            authy                        = ' '
**           CONFIRM                      = ' '
**           DELETE_1205_WFDID            = 'X'
**           DELETE_USER_PROFILES         = 'X'
**           DELETE_DEPENDENTS            = 'X'
**           KEEP_LUPD                    =
**           WORKF_ACTV                   = 'X'
**           NO_EXCEPT_FOREIGN_DATA       = ' '
** IMPORTING
**           CONFIRM_EXIT                 =
*          TABLES
*            del_objects                  = <ft_it_old>
**           ILFCODE                      =
*          EXCEPTIONS
*            error_during_delete          = 1
*            no_authorization             = 2
*            corr_exit                    = 3
*            buffer_upd_with_foreign_data = 4
*            OTHERS                       = 5.
*      ENDIF.
*
*
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*        "Error when delimiting old IT
*        return = zmhp_cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*        zmhp_cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                          IMPORTING message = return-message ).
*        IF return IS NOT INITIAL.
*          hrpad_return = CORRESPONDING #( return ).
*          APPEND hrpad_return TO return_tab.
*          CLEAR: return, hrpad_return.
*        ENDIF.
*      ENDIF.
*    ENDIF.

  ENDMETHOD.


  METHOD CHECK_INFTY_CHANGE_PA.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data,
          lv_pernr       TYPE pernr_d,
          lv_begda       TYPE dats,
          lv_endda       TYPE dats.
    FIELD-SYMBOLS: <fs_it_new>     TYPE any,
                   <fs_it_new_upd> TYPE any,
                   <fs_it_old_upd> TYPE any,
                   <ft_it_old>     TYPE STANDARD TABLE.

    ASSIGN it_record TO <fs_it_new>.

    ASSIGN COMPONENT zmhp_cl_int_constants=>pernr OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_pernr>).
    me->pernr = <fs_pernr>.
    ASSIGN COMPONENT zmhp_cl_int_constants=>begda OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_begda>).
    me->begda = <fs_begda>.
    ASSIGN COMPONENT zmhp_cl_int_constants=>endda OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_endda>).
    me->endda = <fs_endda>.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string( 'P' && me->infty ) ).
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <ft_it_old>.

    IF me->pernr IS NOT INITIAL.
      CALL FUNCTION 'HR_READ_INFOTYPE'
        EXPORTING
*         TCLAS     = 'A'
          pernr     = me->pernr
          infty     = me->infty
          begda     = me->begda
          endda     = me->endda
*         SPRPS     = '*'
*         BYPASS_BUFFER = ' '
*         LEGACY_MODE   = ' '
*     IMPORTING
        TABLES
          infty_tab = <ft_it_old>
* EXCEPTIONS
*         INFTY_NOT_FOUND       = 1
*         INVALID_INPUT = 2
*         OTHERS    = 3
        .
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

      READ TABLE <ft_it_old> ASSIGNING FIELD-SYMBOL(<fs_it_old>) INDEX 1.
      DATA(lt_comp) = lr_structdescr->get_ddic_field_list( ).

      IF <fs_it_new> IS ASSIGNED AND <fs_it_old> IS ASSIGNED.
        CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
        ASSIGN lr_struc_new->* TO <fs_it_new_upd>.
*      <fs_it_new_upd> = <fs_it_new>.
        CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
        ASSIGN lr_struc_old->* TO <fs_it_old_upd>.
        "Clearing not relevant data
        me->clear_fields( EXPORTING comp = lt_comp it_record_new = <fs_it_new>  it_record_old = <fs_it_old>
                          IMPORTING it_record_new_upd = <fs_it_new_upd> it_record_old_upd = <fs_it_old_upd> ).
        "Check for change in relevant data
        me->check_fields( EXPORTING comp = lt_comp it_record_new = <fs_it_new>  it_record_old = <fs_it_old>
                          IMPORTING it_record_new_upd = <fs_it_new_upd> it_record_old_upd = <fs_it_old_upd> has_change = it_change
                                    fields = fields ).

        IF it_change = abap_true.
          "Fill record with old data
          me->transfer_infty_data( EXPORTING infty = me->infty it_record_old = <fs_it_old> it_record_new = <fs_it_new>
                                   IMPORTING it_record_new_upd = <fs_it_new_upd> ).
          it_record_upd = <fs_it_new_upd>.
        ENDIF.

        "No old record
      ELSEIF <fs_it_old> IS NOT ASSIGNED.
        it_create = abap_true.
      ENDIF.
    ELSE.
      it_create = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_infty_change_zxpadu02.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_structure   TYPE REF TO data,
          it_new         TYPE REF TO data,
          it_old         TYPE REF TO data.

    FIELD-SYMBOLS: <infty_data_new> TYPE any,
                   <infty_data_old> TYPE any.

    CONCATENATE 'P' is_prelp_new-infty INTO DATA(infty).
    CREATE DATA it_new TYPE (infty).
    CREATE DATA it_old TYPE (infty).
    ASSIGN it_new->* TO <infty_data_new>.
    ASSIGN it_old->* TO <infty_data_old>.

    IF <infty_data_new> IS ASSIGNED.
      "get infotype data
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
        EXPORTING
          prelp = is_prelp_new
        IMPORTING
          pnnnn = <infty_data_new> ).
    ENDIF.

    IF <infty_data_old> IS ASSIGNED.
      "get infotype data
      cl_hr_pnnnn_type_cast=>prelp_to_pnnnn(
        EXPORTING
          prelp = is_prelp_old
        IMPORTING
          pnnnn = <infty_data_old> ).
    ENDIF.

    DATA(lo_it_delta) = NEW zmhp_cl_int_infty_delta( infty = is_prelp_new-infty ).
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( 'P' && is_prelp_new-infty ).
    DATA(components) = lr_structdescr->get_ddic_field_list( ).
    LOOP AT components ASSIGNING FIELD-SYMBOL(<component>).

      ASSIGN COMPONENT <component>-fieldname OF STRUCTURE <infty_data_new> TO FIELD-SYMBOL(<fs_field_new>).
      IF ( <fs_field_new> IS ASSIGNED ) AND
         ( <fs_field_new> IS NOT INITIAL ).

        ASSIGN COMPONENT <component>-fieldname OF STRUCTURE <infty_data_old> TO FIELD-SYMBOL(<fs_field_old>).
        IF <fs_field_old> IS ASSIGNED AND <fs_field_new> IS ASSIGNED.
**JMB20210907 start insert - in case of decimal pass translate statement
*
          DATA(type_old) = cl_abap_typedescr=>describe_by_data( <fs_field_old> ).

          IF type_old->type_kind NE 'P'.
            TRANSLATE <fs_field_old> TO UPPER CASE.
            TRANSLATE <fs_field_new> TO UPPER CASE.
          ELSE.
**JMB20210909 start insert - in case of IT2006 pass difference (only after update)
*
            CASE is_prelp_new-infty.
              WHEN '2006'.
                CASE <component>-fieldname.
                  WHEN 'ANZHL'.
                    <fs_field_new> = <fs_field_new> - <fs_field_old>.
                ENDCASE.
            ENDCASE.
*JMB20210909 end insert
          ENDIF.
*JMB20210907 insert end

          IF <fs_field_new> EQ <fs_field_old>.
            DATA(fields_static) = value rsdsselopt_t( ( sign = 'I' option = 'EQ' low = 'PERNR' )
                                                      ( sign = 'I' option = 'EQ' low = 'INFTY' )
                                                      ( sign = 'I' option = 'EQ' low = 'BEGDA' )
                                                      ( sign = 'I' option = 'EQ' low = 'ENDDA' )
                                                      ( sign = 'I' option = 'EQ' low = 'SUBTY' )    "JMB20210909 I
                                                      ( sign = 'I' option = 'EQ' low = 'DESTA' )    "JMB20210909 I
                                                      ( sign = 'I' option = 'EQ' low = 'DEEND' ) ). "JMB20210909 I

            IF <component>-fieldname NOT IN fields_static.
              CLEAR <fs_field_new>.
            ENDIF.
          ELSE.
            IF <component>-fieldname NOT IN lo_it_delta->build_rel_fields( ).
              CLEAR <fs_field_new>.
            ENDIF.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.

    cl_hr_pnnnn_type_cast=>pnnnn_to_prelp(
      EXPORTING
        pnnnn = <infty_data_new>
      IMPORTING
        prelp = DATA(prelp) ).

    DATA(lo_it_aendup) = NEW zmhp_cl_int_it_aendup( ).
    lo_it_aendup->build_it_aend_up( EXPORTING psave = prelp IMPORTING it_aendup = rs_it_aendup message = DATA(message) ).

  ENDMETHOD.


  METHOD clear_fields.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data.

*    ASSIGN it_record_new TO FIELD-SYMBOL(<fs_it_new>).
*    ASSIGN it_record_old TO FIELD-SYMBOL(<fs_it_old>).

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string( 'P' && me->infty ) ).
    CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_new->* TO FIELD-SYMBOL(<fs_it_new>).
    CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_old->* TO FIELD-SYMBOL(<fs_it_old>).
    <fs_it_new> = it_record_new.
    <fs_it_old> = it_record_old.
    "Clearing not relevant data
    LOOP AT comp INTO DATA(ls_comp) WHERE fieldname NOT IN me->build_rel_fields( ).
*   clear not needed fields and
      ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_field>).
      IF sy-subrc IS NOT INITIAL.
*          DATA(is_ok) = abap_false.
      ENDIF.
      IF <fs_field> IS ASSIGNED.
        CLEAR <fs_field>.
      ENDIF.
      ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_old> TO <fs_field>.
      IF sy-subrc IS NOT INITIAL.
*          is_ok = abap_false.
      ENDIF.
      IF <fs_field> IS ASSIGNED.
        CLEAR <fs_field>.
      ENDIF.
    ENDLOOP.
    it_record_new_upd = <fs_it_new>.
    it_record_old_upd = <fs_it_old>.
  ENDMETHOD.


  METHOD constructor.
    me->infty = infty.
  ENDMETHOD.


  METHOD get_fields_it_create.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data,
          ls_fields      LIKE LINE OF fields.
*    ASSIGN it_record_new TO FIELD-SYMBOL(<fs_it_new>).
*    ASSIGN it_record_old TO FIELD-SYMBOL(<fs_it_old>).
*    CLEAR: has_change.
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string( 'P' && me->infty ) ).
    CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_new->* TO FIELD-SYMBOL(<fs_it_new>).
*    CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
*    ASSIGN lr_struc_old->* TO FIELD-SYMBOL(<fs_it_old>).
    <fs_it_new> = it_record_new.
*    <fs_it_old> = it_record_old.

    DATA(lt_comp) = lr_structdescr->get_ddic_field_list( ).
    "Processing relevant data
*    DATA(rel_fields) = me->build_rel_fields( ).
*    IF rel_fields IS NOT INITIAL.
    LOOP AT lt_comp INTO DATA(ls_comp). "WHERE fieldname IN rel_fields.
*   clear not needed fields and
      ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_field_new>).
      IF sy-subrc IS NOT INITIAL.
*            is_ok = abap_false.
      ENDIF.
      IF ( <fs_field_new> IS ASSIGNED )  AND ( <fs_field_new> IS NOT INITIAL ).
*          ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_field_old>).
*          IF sy-subrc IS NOT INITIAL.
**            is_ok = abap_false.
*          ENDIF.
*          IF <fs_field_old> IS ASSIGNED AND <fs_field_new> IS ASSIGNED.
*            TRANSLATE <fs_field_old> TO UPPER CASE.
        IF ls_comp-datatype ne 'DEC' OR ls_comp-datatype ne 'CURR'.
          TRANSLATE <fs_field_new> TO UPPER CASE.
        ENDIF.
        IF <fs_field_new> IS NOT INITIAL. "AND <fs_field_new> NE <fs_field_old>.
*              has_change = abap_true.
          ls_fields-field = ls_comp-fieldname.
*              ls_fields-field_old = <fs_field_old>.
          ls_fields-field_new = <fs_field_new>.
          APPEND ls_fields TO fields.
        ELSEIF  <fs_field_new> = '0.00'. "AND <fs_field_new> NE <fs_field_old>.
*              has_change = abap_true.
          ls_fields-field = ls_comp-fieldname.
*              ls_fields-field_old = <fs_field_old>.
          ls_fields-field_new = <fs_field_new>.
          APPEND ls_fields TO fields.
        ELSEIF  <fs_field_new> = '0'. "AND <fs_field_new> NE <fs_field_old>.
*              has_change = abap_true.
          ls_fields-field = ls_comp-fieldname.
*              ls_fields-field_old = <fs_field_old>.
          ls_fields-field_new = <fs_field_new>.
          APPEND ls_fields TO fields.
        ENDIF.
*          ENDIF.
      ENDIF.
    ENDLOOP.
*  ELSE.
*
*  ENDIF.
*    it_record_new_upd = <fs_it_new>.
*  it_record_old_upd = <fs_it_old>.
  ENDMETHOD.


  METHOD get_last_infotype.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
          lr_table       TYPE REF TO data,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data.
    FIELD-SYMBOLS: <ft_it_old> TYPE STANDARD TABLE.
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string('P' && infty ) ).
    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
    CREATE DATA lr_table TYPE HANDLE lr_tabledescr.
    ASSIGN lr_table->* TO <ft_it_old>.
    CALL FUNCTION 'HR_READ_INFOTYPE'
      EXPORTING
        pernr           = pernr
        infty           = infty
        begda           = stidat
        endda           = stidat
      TABLES
        infty_tab       = <ft_it_old>
      EXCEPTIONS
        infty_not_found = 1
        invalid_input   = 2
        OTHERS          = 3.
    IF sy-subrc = 0.
      READ TABLE <ft_it_old> ASSIGNING FIELD-SYMBOL(<fs_it_old>) INDEX 1.
      check <fs_it_old> is ASSIGNED.
      data = <fs_it_old>.
    ENDIF.
  ENDMETHOD.


  METHOD is_rel_field.
  ENDMETHOD.


  METHOD transfer_infty_data.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data.

    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string('P' && me->infty ) ).
    CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_new->* TO FIELD-SYMBOL(<fs_it_new>).
    CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_old->* TO FIELD-SYMBOL(<fs_it_old>).
    <fs_it_new> = it_record_new.
    <fs_it_old> = it_record_old.
    DATA(lt_comp) = lr_structdescr->get_ddic_field_list( ).

    LOOP AT lt_comp INTO DATA(ls_comp).
*   clear not needed fields and
      ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_field_new>).
      IF sy-subrc IS NOT INITIAL.
*        DATA(is_ok) = abap_false.
      ENDIF.
      ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_field_old>).
      IF sy-subrc IS NOT INITIAL.
*        is_ok = abap_false.
      ENDIF.

      IF me->infty = zmhp_cl_int_constants=>it0001 AND ( ls_comp-fieldname = 'GSBER' OR ls_comp-fieldname = 'VDSK1' ).
        IF <fs_field_old> IS ASSIGNED AND <fs_field_old> IS NOT INITIAL.
          CLEAR: <fs_field_old>.
          CONTINUE.
        ENDIF.
      ENDIF.

      IF <fs_field_new> IS ASSIGNED.
        IF <fs_field_old> IS ASSIGNED.
          IF <fs_field_new> IS INITIAL.
            <fs_field_new> = <fs_field_old>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    it_record_new_upd = <fs_it_new>.
  ENDMETHOD.


  METHOD transfer_infty_data_v2.
    DATA: lr_structdescr TYPE REF TO cl_abap_structdescr,
          lr_struc_new   TYPE REF TO data,
          lr_struc_old   TYPE REF TO data.
*   Create infotype specific structure
    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV string('P' && infty ) ).
*   New data
    CREATE DATA lr_struc_new TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_new->* TO FIELD-SYMBOL(<fs_it_new>).
    DATA(prelp) = CORRESPONDING zmhp_prelp( it_aend ).
    zmhp_cl_int_type_cast=>prelp_to_pnnnn(
      EXPORTING
        prelp = CONV #( prelp )
      IMPORTING
        pnnnn = <fs_it_new> ).
*   Old data
    CREATE DATA lr_struc_old TYPE HANDLE lr_structdescr.
    ASSIGN lr_struc_old->* TO FIELD-SYMBOL(<fs_it_old>).
*   Read last infotype entry
    ASSIGN COMPONENT 'SUBTY' OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<subty>).
    zmhp_cl_int_infty_delta=>get_last_infotype(
      EXPORTING
        pernr  = pernr
        infty  = infty
        subty  = <subty>
        stidat = stidat
      IMPORTING
        data   = <fs_it_old> ).
*   Get list of fields (components) for infotype structure
    DATA(lt_comp) = lr_structdescr->get_ddic_field_list( ).

    LOOP AT lt_comp INTO DATA(ls_comp).
*     Do not transfer GSBER and VDSK1 for infotype 1
      CHECK ls_comp-fieldname NE 'GSBER' AND ls_comp-fieldname NE 'VDSK1'.
*     Assign component of structure for comparison
      ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_new> TO FIELD-SYMBOL(<fs_field_new>).
      ASSIGN COMPONENT ls_comp-fieldname OF STRUCTURE <fs_it_old> TO FIELD-SYMBOL(<fs_field_old>).
      CHECK <fs_field_new> IS ASSIGNED AND <fs_field_old> IS ASSIGNED.
*     Check if field was deleted
      READ TABLE fields WITH KEY field_sap = ls_comp-fieldname ASSIGNING FIELD-SYMBOL(<field_value>).
      IF <field_value> IS ASSIGNED AND <field_value> = 'DELETED'.
        CLEAR <fs_it_new>.
        CONTINUE.
      ENDIF.
*     Make actual change
      IF <fs_field_new> IS ASSIGNED.
        IF <fs_field_old> IS ASSIGNED.
          IF <fs_field_new> IS INITIAL.
            <fs_field_new> = <fs_field_old>.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDLOOP.
    "Transfer data back to it_aend/prelp structure.
    CLEAR prelp.
    zmhp_cl_int_infty_proc_xml=>pa_to_prelp( EXPORTING infotype = <fs_it_new>
                                                       aend_id  = it_aend-aend_id
                                             IMPORTING prelp    = prelp ).
    it_aend-data1 = prelp-data1.
    it_aend-data2 = prelp-data2.
    it_aend-data3 = prelp-data3.
    it_aend-data4 = prelp-data4.
    it_aend-data5 = prelp-data5.
  ENDMETHOD.
ENDCLASS.
