class ZMHP_CL_INT_INFOTYPES definition
  public
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

  class-data INFTY_OPERATION type ref to ZMHP_CL_INT_IT_OPERATION .

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
  class-methods PROCESS_0000_AFTER
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
  class-methods PROCESS_9402_BEFORE
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
  PRIVATE SECTION.
ENDCLASS.



CLASS ZMHP_CL_INT_INFOTYPES IMPLEMENTATION.


  METHOD process_0000_after.


  ENDMETHOD.


  METHOD process_0001_after.
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


  METHOD process_0001_before.
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


  METHOD process_9402_before.

*    DATA: error_tab      TYPE hrpad_return_tab,
*          ret            TYPE hrpad_return,
*          lr_structdescr TYPE REF TO cl_abap_structdescr,
*          record_old     TYPE REF TO data,
*          lr_tabledescr  TYPE REF TO cl_abap_tabledescr,
*          lr_table_old   TYPE REF TO data,
*          lr_table_new   TYPE REF TO data.
*
*    FIELD-SYMBOLS: <record_new_tab> TYPE STANDARD TABLE,
*                   <record_old>     TYPE any,
*                   <record_new>     TYPE any,
*                   <record_old_tab> TYPE STANDARD TABLE.
*    ASSIGN record TO <record_new>.
*
*    lr_structdescr ?= cl_abap_typedescr=>describe_by_name( CONV #( 'P' && infty ) ).
**    CREATE DATA record_old TYPE HANDLE lr_structdescr.
**    ASSIGN record_old->* TO <record_old>.
*    lr_tabledescr ?= cl_abap_tabledescr=>create( p_line_type = lr_structdescr ).
*    CREATE DATA lr_table_old TYPE HANDLE lr_tabledescr.
**    CREATE DATA lr_table_new TYPE HANDLE lr_tabledescr.
*    ASSIGN lr_table_old->* TO <record_old_tab>.
**    ASSIGN lr_table_new->* TO <record_new_tab>.
*
*
*
*    ASSIGN COMPONENT 'DIR_APPROV' OF STRUCTURE <record_new> TO FIELD-SYMBOL(<manager>).
**    ASSIGN COMPONENT zmhp_cl_int_constants=>orgeh OF STRUCTURE record TO FIELD-SYMBOL(<orgeh>).
*
*
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
*      SELECT SINGLE * FROM pa9400 INTO @DATA(p9400_line) WHERE pernr = @<manager> AND begda LE @endda AND endda GE @begda.
*      IF sy-subrc NE 0.
*        SELECT SINGLE * FROM pa9400 INTO @p9400_line WHERE oraclepernr = @<manager> AND begda LE @endda AND endda GE @begda.
*        IF p9400_line IS NOT INITIAL.
*          <manager> = p9400_line-pernr.
*        ENDIF.
*      ENDIF.
**      zmhp_cl_int_infotypes=>infty_operation->read_paxxxx(
**      EXPORTING
**        begda = begda
**        endda = endda
**        infty = infty
**        pernr = pernr
**        subty = subty
**        simu = simu
**        IMPORTING
**          return_tab = return_tab
**          record_tab = <record_old_tab>
**        ).
*
**      ret = CORRESPONDING #( return ).
**      APPEND ret TO return_tab.
**      CLEAR: ret, return.
*
*    ENDIF.
*    CLEAR: return.
  ENDMETHOD.


  METHOD process_it_specifics_after_op.

    zmhp_cl_int_infotypes=>infty_operation = NEW zmhp_cl_int_it_operation( molga = molga pernr = pernr ).

    TRY.
        DATA(method) = CONV string( 'PROCESS_' && infty && '_AFTER' ).
        CALL METHOD zmhp_cl_int_infotypes=>(method)
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


  METHOD process_it_specifics_before_op.

    zmhp_cl_int_infotypes=>infty_operation = NEW zmhp_cl_int_it_operation( molga = molga pernr = pernr ).

    TRY.
        DATA(method) = CONV string( 'PROCESS_' && infty && '_BEFORE' ).
        CALL METHOD zmhp_cl_int_infotypes=>(method)
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
