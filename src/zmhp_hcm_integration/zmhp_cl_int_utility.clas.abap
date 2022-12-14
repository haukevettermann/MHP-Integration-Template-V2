class ZMHP_CL_INT_UTILITY definition
  public
  final
  create public .

public section.

  class-data IT0105_PROCESSER type BOOLE_D value ABAP_FALSE ##NO_TEXT.

  class-methods DECRYPT_EXTRACT
    importing
      !FILENAME type STRING
    exporting
      !FILENAME_DEC type STRING .
  class-methods ENCRYPT_EXTRACT .
  class-methods CHECK_PROJECT
    importing
      !MOLGA type MOLGA
    returning
      value(PROJECT) type CHAR4 .
  class-methods BUILD_OBJECT
    importing
      !BEGDA type DATS
      !ENDDA type DATS
      !OTYPE type OTYPE
      !OBJID type HROBJID
      !BUKRS type BUKRS optional
    returning
      value(OBJECT) type OBJEC .
  class-methods CHECK_FOR_OM_ERROR
    importing
      !TYPE type ZMHP_DD_EXT_TYPE optional
    returning
      value(HAS_ERROR) type BOOLE_D .
  class-methods CREATE_GUID
    returning
      value(GUID) type GUID_32 .
  class-methods GET_EXTERNAL_DATE
    importing
      !DATE type DATS
    returning
      value(DATE_EXT) type ZMHP_DD_DATE_EXT .
  class-methods GET_MESSAGE
    importing
      !MSGID type SY-MSGID
      !MSGNO type SY-MSGNO
      !MSGV1 type SY-MSGV1 optional
      !MSGV2 type SY-MSGV2 optional
      !MSGV3 type SY-MSGV3 optional
      !MSGV4 type SY-MSGV4 optional
    exporting
      !MESSAGE type BAPI_MSG .
  class-methods DEFAULT_NATIO
    importing
      value(MOLGA) type MOLGA
    returning
      value(NATIO) type NATIO .
  class-methods GET_MOLGA
    importing
      !PERNR type PERNR_D
      !BEGDA type DATS
      !ENDDA type DATS
    returning
      value(MOLGA) type MOLGA .
  class-methods GET_OBJID_BY_CLOUDID
    importing
      !CLOUD_ID type ZMHP_DD_ELEMENT
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !OBJID type HROBJID .
  class-methods GET_PERNR_BY_CLOUDID
    importing
      !CLOUD_ID type ZMHP_DD_ELEMENT
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !PERNR type PERNR_D .
  class-methods GET_PLANS_IT0001
    importing
      !PERNR type PERNR_D
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    exporting
      !PLANS type PLANS .
  class-methods GET_MOLGA_BY_SPRAS
    importing
      !SPRAS type SPRAS
    exporting
      !MOLGA type MOLGA .
  class-methods GET_SPRAS_BY_MOLGA
    importing
      !MOLGA type MOLGA
    exporting
      value(LANGU) type LAISO
      !SPRAS type SPRAS .
  class-methods MAINTAIN_RELATION
    importing
      !ACTION type OKCODE
      !BEGDA type DATS
      !ENDDA type DATS
      !PARENT type OBJEC
      !CHILD type OBJEC
      !RSIGN type RSIGN
      !RELAT type RELAT
      !SIMU type BOOLE_D optional
      !SPRAS type SPRAS
    exporting
      !RETURN type BAPIRET1 .
  class-methods MAP_MSG_TAB
    importing
      !MESSAGES type HRPAD_MESSAGE_TAB
    returning
      value(RETURN_TAB) type HRPAD_RETURN_TAB .
  class-methods MAP_SY_MSG
    importing
      !MSGID type SY-MSGID
      !MSGTY type SY-MSGTY
      !MSGNO type SY-MSGNO
      !MSGV1 type SY-MSGV1
      !MSGV2 type SY-MSGV2
      !MSGV3 type SY-MSGV3
      !MSGV4 type SY-MSGV4
    returning
      value(RETURN) type BAPIRET1 .
  class-methods READ_MASSN_TXT
    importing
      !SPRSL type SPRAS
      !MASSN type MASSN
      !MASSG type MASSG optional
    returning
      value(MASSN_TXT) type MNTXT .
  class-methods START_INT_RUN
    importing
      !INT_RUN type GUID_32
      !SAP_ID type ZMHP_DD_VALUE
      !CLOUD_ID type ZMHP_DD_VALUE
      !TYPE type OTYPE
      !EXT_TYPE type ZMHP_DD_EXT_TYPE optional .
  class-methods GET_EXTERNAL_TIME
    importing
      !TIME type TIMS
    returning
      value(TIME_EXT) type ZMHP_DD_DATE_EXT .
protected section.
private section.
ENDCLASS.



CLASS ZMHP_CL_INT_UTILITY IMPLEMENTATION.


  METHOD build_object.
    DATA: sobid TYPE sobid,
          kostl TYPE c LENGTH 10.
*          kokrs TYPE kokrs.
    "Build child
    object-plvar = zmhp_cl_int_constants=>plvar.
    object-endda = endda.
    object-begda = begda.
    object-otype = otype.
    object-objid = objid.
    object-istat = '1'.
    IF otype = zmhp_cl_int_constants=>costcenter.
      "Get kostenrechnungskreis
*      sobid = objid.
*      CALL FUNCTION 'RH_GET_CONTROLLING_AREA'
*        EXPORTING
*          plvar         = '01'
*          otype         = 'K'
*          objid         = objid
*          find_kokrs_up = 'X'
*        IMPORTING
*          kokrs         = kokrs
*        EXCEPTIONS
*          not_found     = 1
*          OTHERS        = 2.
*
*      IF sy-subrc <> 0.
** Implement suitable error handling here
*      ENDIF.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = objid
        IMPORTING
          output = kostl.

*      SELECT SINGLE kokrs FROM csks INTO @DATA(kokrs) WHERE kostl = @k AND bukrs = @b AND datab LE @endda AND datbi GE @begda.
      SELECT SINGLE kokrs FROM csks INTO @DATA(kokrs) WHERE kostl = @kostl AND bukrs = @bukrs AND datab LE @endda AND datbi GE @begda.
*      IF kokrs IS NOT INITIAL.
*        CONCATENATE objid kokrs INTO object-realo.
*      ELSE.
      CONCATENATE kostl kokrs INTO object-realo.
*      ENDIF.
    ELSEIF otype = zmhp_cl_int_constants=>position.
      object-realo = objid.
    ENDIF.


  ENDMETHOD.


  METHOD check_for_om_error.
    DATA om_aend TYPE zmhp_tt_om_aend.
    IF type IS INITIAL.
      SELECT * FROM zmhp_int_om_aend INTO TABLE @om_aend WHERE status = '01' OR status = '03' OR status = '07' OR status = '08'.
      IF om_aend IS NOT INITIAL.
        has_error = abap_true.
      ENDIF.
    ELSEIF type = 'DT'.
      SELECT * FROM zmhp_int_om_aend INTO TABLE @om_aend WHERE ( status = '01' OR status = '03' OR status = '07' OR status = '08' ) AND ext_type = 'D'.
      IF om_aend IS NOT INITIAL.
        has_error = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD check_project.
    CASE molga.
      WHEN '01'.
        project = 'COFU'.
      WHEN '03'.
        project = 'COFU'.
      WHEN '05'.
        project = 'COFU'.
      WHEN '06'.
        project = 'COFU'.
      WHEN '15'.
        project = 'COFU'.
      WHEN OTHERS.
        project = 'COGL'.
    ENDCASE.
  ENDMETHOD.


  METHOD create_guid.
    TRY.
        guid = cl_system_uuid=>create_uuid_c32_static( ).
      CATCH cx_uuid_error INTO DATA(error).
    ENDTRY.
  ENDMETHOD.


  METHOD decrypt_extract.

    DATA: add_parameters TYPE sxpgcolist-parameters,
          target_sys     TYPE rfcdisplay-rfchost,
          result         TYPE TABLE OF btcxpm,
          output         TYPE string.

*    CONCATENATE 'gpg2 --batch' '--passphrase' zmhp_cl_int_constants=>pgp_passphrase '--no-tty' '-o' '/Oracle_HCM/EXTRACTS/test' '-d' filename INTO add_parameters SEPARATED BY space.
*sudo gpg2 --batch --passphrase Du3rr_PgPK3y#D3v1_2022#! --no-tty -o /Oracle_HCM/EXTRACTS/Testdecrypt.xml -d /Oracle_HCM/EXTRACTS/DUERR_WORKER_SAP_EXTRACT_300000071392852.20220601085148.asc
    target_sys = 'sap10e70'.
    DATA(name_length) = strlen( filename ).
    name_length = name_length - 4.
    output = filename.
    output = output+0(name_length).
    CONCATENATE zmhp_cl_int_constants=>pgp_passphrase output filename INTO add_parameters SEPARATED BY space.
    CALL FUNCTION 'SXPG_COMMAND_EXECUTE'
      EXPORTING
        commandname                   = zmhp_cl_int_constants=>decrypt_command
        additional_parameters         = add_parameters
*       OPERATINGSYSTEM               = SY-OPSYS
        targetsystem                  = target_sys
*       DESTINATION                   =
*       STDOUT                        = 'X'
*       STDERR                        = 'X'
*       TERMINATIONWAIT               = 'X'
*       TRACE                         =
*       DIALOG                        =
*      IMPORTING
*       status                        =
*       exitcode                      =
      TABLES
        exec_protocol                 = result
      EXCEPTIONS
        no_permission                 = 1
        command_not_found             = 2
        parameters_too_long           = 3
        security_risk                 = 4
        wrong_check_call_interface    = 5
        program_start_error           = 6
        program_termination_error     = 7
        x_error                       = 8
        parameter_expected            = 9
        too_many_parameters           = 10
        illegal_command               = 11
        wrong_asynchronous_parameters = 12
        cant_enq_tbtco_entry          = 13
        jobcount_generation_error     = 14
        OTHERS                        = 15.
    IF sy-subrc <> 0.
* Implement suitable error handling here
*    ELSE.
*      filename_dec = output.
    ENDIF.

    filename_dec = output.

  ENDMETHOD.


  METHOD default_natio.
    SELECT SINGLE * FROM t500l INTO @DATA(w500l) WHERE molga EQ @molga.
    IF sy-subrc EQ 0.
      SELECT SINGLE * FROM t005 INTO @DATA(w005) WHERE intca EQ @w500l-intca.
      IF sy-subrc EQ 0.
        natio = w005-land1.
      ENDIF.
      CLEAR: w005,w500l.
    ENDIF.

  ENDMETHOD.


  method ENCRYPT_EXTRACT.
  endmethod.


  METHOD get_external_date.
    CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
      EXPORTING
        date_internal            = date
      IMPORTING
        date_external            = date_ext
      EXCEPTIONS
        date_internal_is_invalid = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDMETHOD.


  METHOD get_external_time.

*    DATA(user_timefm) = cl_abap_timefm=>get_user_timefm( ).

    CALL METHOD cl_abap_timefm=>conv_time_int_to_ext
      EXPORTING
        time_int = time
*       without_seconds     = abap_false       " Flag f??r Sekunden
*       format_according_to =                  " Format gem???? ENVIRONMENT oder ISO
      IMPORTING
        time_ext = DATA(time_string).

    time_ext = time_string.

  ENDMETHOD.


  METHOD get_message.

    CALL FUNCTION 'BAPI_MESSAGE_GETDETAIL'
      EXPORTING
        id         = msgid
        number     = msgno
*       LANGUAGE   = SY-LANGU
        textformat = 'RTF'
*       LINKPATTERN        =
        message_v1 = msgv1
        message_v2 = msgv2
        message_v3 = msgv3
        message_v4 = msgv4
*       LANGUAGE_ISO       =
*       LINE_SIZE  =
      IMPORTING
        message    = message
*       RETURN     =
*           TABLES
*       TEXT       =
      .

  ENDMETHOD.


METHOD get_molga.

  CALL FUNCTION 'RH_PM_GET_MOLGA_FROM_PERNR'
    EXPORTING
*     PLVAR           =
      pernr           = pernr
      begda           = begda
      endda           = endda
    IMPORTING
      molga           = molga
*     TRFKZ           =
    EXCEPTIONS
      nothing_found   = 1
      no_active_plvar = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

ENDMETHOD.


  METHOD get_molga_by_spras.
*    SELECT SINGLE intca FROM t500l WHERE molga = @molga INTO @DATA(country).
*    SELECT SINGLE spras FROM t001 WHERE land1 = @country INTO @spras.
*    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
*      EXPORTING
*        input  = spras
*      IMPORTING
*        output = langu.
*
*    spras = 'E'.
*    langu = 'EN'.

    SELECT SINGLE land1 FROM t001 WHERE spras = @spras INTO @DATA(country).
    SELECT SINGLE molga FROM t500l WHERE intca = @country INTO @molga.

  ENDMETHOD.


  METHOD get_objid_by_cloudid.
    SELECT SINGLE objid FROM hrp9401 INTO objid WHERE oracleid = cloud_id AND begda LE endda AND endda GE begda.
  ENDMETHOD.


  METHOD get_pernr_by_cloudid.
    SELECT SINGLE pernr FROM pa9400 INTO pernr WHERE oracleid = cloud_id AND begda LE endda AND endda GE begda.
    IF pernr IS INITIAL.
      SELECT SINGLE pernr FROM pa9400 INTO pernr WHERE oraclepernr = cloud_id AND begda LE endda AND endda GE begda.
      IF pernr IS INITIAL.
        pernr = cloud_id.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD get_plans_it0001.

    SELECT SINGLE plans FROM pa0001 INTO plans WHERE pernr = pernr AND begda LE endda AND endda GE begda.

    SELECT * FROM pa0001 INTO TABLE @DATA(p0001_tab) WHERE pernr = @pernr AND begda LE @endda AND endda GE @begda.
    SORT p0001_tab DESCENDING BY endda.
    READ TABLE p0001_tab INTO DATA(p0001) INDEX 1.
    plans = p0001-plans.
  ENDMETHOD.


  METHOD get_spras_by_molga.
    SELECT SINGLE intca FROM t500l WHERE molga = @molga INTO @DATA(country).
    SELECT SINGLE spras FROM t001 WHERE land1 = @country INTO @spras.
    CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
      EXPORTING
        input  = spras
      IMPORTING
        output = langu.

*    spras = 'E'.
*    langu = 'EN'.

  ENDMETHOD.


  METHOD maintain_relation.
    DATA plvar TYPE plvar.
*    CALL FUNCTION 'RHOM_ALL_BUFFER_INIT'.

    IF simu = abap_true.
      CONCATENATE rsign relat INTO DATA(subty).
      DATA(begda_ext) = zmhp_cl_int_utility=>get_external_date( date = begda ).
      DATA(endda_ext) = zmhp_cl_int_utility=>get_external_date( date = endda ).
      CONCATENATE begda_ext '-' endda_ext INTO DATA(date) SEPARATED BY space.
      return = VALUE bapiret1( type = zmhp_cl_int_constants=>warning
                                 id = zmhp_cl_int_constants=>msg_class_int
                                 number = zmhp_cl_int_constants=>msg_no-m24
                                 message_v1 = parent
                                 message_v2 = child
                                 message_v3 = subty
                                 message_v4 = date
).

    ELSE.

      "Relation to child can be earliest with begda of child object
*      me->read_hrpxxxx(
*      EXPORTING
*        begda = begda
*        endda = endda
*        infty = zmhp_cl_int_constants=>it1000
*        langu = CONV #( spras )
*        otype = <om_aend>-otype
*        sap_id = <om_aend>-sap_id
*        subty = <om_aend>-subty
*        plvar = CONV #( zmhp_cl_int_constants=>plvar )
*        IMPORTING
*          hrp_old = <hrp_old>
*          return = return ).

      plvar = zmhp_cl_int_constants=>plvar.
      CALL FUNCTION 'RHOM_MAINTAIN_RELATION_BUFF'
        EXPORTING
          act_fcode               = action
          act_plvar               = plvar
          act_istat               = '1'
          vbegda                  = begda
          vendda                  = endda
*         DEFIBEGDA               =
*         DEFIENDDA               =
          vprozt                  = '100'
*         VPRIOX                  =
*         VSEQNR                  =
          parent_object           = parent
*         ACT_INFTY               = zmhp_cl_int_constants=>it1001
          act_rsign               = rsign
          act_relat               = relat
          child_object            = child
*         MOVE_FLAG               =
*         OLD_PARENT_OBJECT       =
*         CUT_DATE                =
*         HR_ACTION_INFO          =
          change_manager_relation = ' '
*     IMPORTING
*         VBEGDA                  =
*         VENDDA                  =
        EXCEPTIONS
          no_active_plvar         = 1
          no_authority            = 2
          write_error             = 3
          OTHERS                  = 4.
      IF sy-subrc <> 0.
* Implement suitable error handling here
        "Create Message for Error during maintain relation between S - O
        return = zmhp_cl_int_utility=>map_sy_msg( msgid = sy-msgid msgty = sy-msgty msgno = sy-msgno msgv1 = sy-msgv1 msgv2 = sy-msgv2 msgv3 = sy-msgv3 msgv4 = sy-msgv4 ).
*      zmhp_cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number
*                                        IMPORTING message = return-message ).
        IF return-number = 075.
          return-type = zmhp_cl_int_constants=>error.
        ENDIF.
      ELSE.
        "Success create relation
*      CALL FUNCTION 'RHOM_WRITE_BUFFER_TO_DB'.
*      COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD map_msg_tab.
    DATA: return LIKE LINE OF return_tab.

    LOOP AT messages ASSIGNING FIELD-SYMBOL(<message>).
      return = CORRESPONDING #( <message> ).
      return-type = <message>-msgty.
      return-id = <message>-msgid.
      return-number = <message>-msgno.
*      return-message = <message>-
      return-message_v1 = <message>-msgv1.
      return-message_v2 = <message>-msgv2.
      return-message_v3 = <message>-msgv3.
      return-message_v4 = <message>-msgv4.
      zmhp_cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number msgv1 = return-message_v1 msgv2 = return-message_v2
                                      msgv3 = return-message_v3 msgv4 = return-message_v4
                        IMPORTING message = return-message ).
      APPEND return TO return_tab.
      CLEAR: return.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_sy_msg.
    return-id = msgid.
    return-type = msgty.
    return-number = msgno.
    return-message_v1 = msgv1.
    return-message_v2 = msgv2.
    return-message_v3 = msgv3.
    return-message_v4 = msgv4.

    zmhp_cl_int_utility=>get_message( EXPORTING msgid = return-id msgno = return-number msgv1 = return-message_v1 msgv2 = return-message_v2
                                                msgv3 = return-message_v3 msgv4 = return-message_v4
                                  IMPORTING message = return-message ).
  ENDMETHOD.


  METHOD read_massn_txt.

    SELECT SINGLE mntxt FROM t529t INTO @massn_txt WHERE sprsl = @sprsl AND massn = @massn.

  ENDMETHOD.


  METHOD start_int_run.
    IF type = zmhp_cl_int_constants=>person.
      SUBMIT zmhp_rp_it_aend_post AND RETURN
        WITH pnppernr INCL sap_id
        WITH so_cldid INCL cloud_id
        WITH so_intr INCL int_run
        WITH pa_simu EQ abap_false
        WITH pa_alv EQ abap_false.
    ELSEIF type = zmhp_cl_int_constants=>position OR type = zmhp_cl_int_constants=>orgunit.
      IF ext_type = 'D'.
        SUBMIT zmhp_rp_om_aend_post AND RETURN
          WITH so_sapid INCL sap_id
          WITH so_cldid INCL cloud_id
          WITH so_intr INCL int_run
          WITH p_otype INCL type
          WITH p_etype INCL ext_type
          WITH p_test EQ abap_false
          WITH pa_alv EQ abap_false
          WITH p_manag EQ abap_false.
        SUBMIT zmhp_rp_om_aend_post AND RETURN
          WITH so_sapid INCL sap_id
          WITH so_cldid INCL cloud_id
          WITH so_intr INCL int_run
          WITH p_otype INCL type
          WITH p_etype INCL ext_type
          WITH p_test EQ abap_false
          WITH pa_alv EQ abap_false
          WITH p_manag EQ abap_true.
      ELSEIF ext_type = 'DT'.
        SUBMIT zmhp_rp_om_aend_post AND RETURN
          WITH so_sapid INCL sap_id
          WITH so_cldid INCL cloud_id
          WITH so_intr INCL int_run
          WITH p_otype INCL type
          WITH p_etype INCL ext_type
          WITH p_test EQ abap_false
          WITH pa_alv EQ abap_false
          WITH p_manag EQ abap_false.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
