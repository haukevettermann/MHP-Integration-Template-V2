class ZMHP_CL_MIG_POS_HIER definition
  public
  create public .

public section.

  data OBJECTS type OBJEC_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  constants POSITION_HIER type STRING value 'PositionHierarchy' ##NO_TEXT.
  constants POS_HIER type STRING value 'POS_HIER' ##NO_TEXT.
  data MOLGA type RSDSSELOPT_T .
  constants METADATA_HIER type STRING value 'METADATA_HIER' ##NO_TEXT.

  methods CONSTRUCTOR
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !OBJEC type OBJEC_T .
  methods PROCEED_POS_DATA
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA_HIER
    returning
      value(METADATA_HIER) type STRING .
  methods DOWNLOAD_FILES_PH
    importing
      !FILES_PH type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  methods PROCEED_FILE_CONSTRUCTION
    importing
      !METADATA_HIER type STRING
      !DATA_HIER type STRING
    returning
      value(CONTENT_HIER) type STRING .
  PROTECTED SECTION.
private section.

  data LEADING_POS type ZMHP_HRP1000_T .
  data IT1008 type ZMHP_HRP1008_T .
  data ORGUNITS type ZMHP_HRP1000_T .
  data CONNECTIONS_TO_JOBS type HRP1001_T .
  data CONNECTIONS_TO_ORGUNITS type HRP1001_T .
  data POSITIONS type ZMHP_HRP1000_T .
  data JOBS type ZMHP_HRP1000_T .
  data BUKRS_TEXTS type T_T001 .
  data MAPPING_FIELDS_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data STRUC_T type STRUC_T .

  methods GET_KOSTL_OF_ORGID
    importing
      !OBJID type REALO
      !BEGDA type BEGDA
      !PLVAR type PLVAR
    returning
      value(BUKRS_TXT1) type BUTXT .
  methods GET_BUKRS_TXT
    importing
      !BUKRS type BUKRS
    returning
      value(BUKRS_TXT) type BUTXT .
  methods GET_BTRTL
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
    returning
      value(BTRTL) type BTRTL .
  methods GET_PERSA
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
    returning
      value(PERSA) type WERKS_D .
  methods MAP_POS_DATA
    returning
      value(DATA_HIER) type STRING .
  methods GET_POS_DATA .
  methods GET_BUKRS
    importing
      !POSITION_OBJECT_ID type REALO
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PLVAR type PLVAR
    returning
      value(BUKRS) type BUKRS .
  methods GET_ORGUNIT_DATA
    importing
      !OBJECT_ID type HROBJID
    exporting
      value(ORGUNIT_NAME) type STEXT
      value(ORGUNIT_OBJECT_ID) type HROBJID .
  methods GET_SET_CODE
    returning
      value(SET_CODE) type STRING .
  methods GET_JOB_DATA
    importing
      !OBJECT_ID type HROBJID
    returning
      value(JOB_NAME) type STEXT .
  methods GET_EFFECTIVE_START_DATE
    importing
      !OBJECT_ID type REALO
    returning
      value(EFFECTIVE_START_DATE) type BEGDA .
  methods GET_WERKS
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
    returning
      value(WERKS) type WERKS_D .
  methods GET_BUKRS_TEXT
    importing
      !BUKRS type BUKRS
    returning
      value(BUKRS_TEXT) type BUTXT .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_MIG_VALUES
    importing
      !BUKRS type BUKRS
    exporting
      !BUSINESS_UNIT_NAME type ZMHP_DD_VALUE
      !WORKING_HOURS type ZMHP_DD_VALUE
      !FREQUENCY type ZMHP_DD_VALUE .
  methods GET_EFFECTIVE_START_DATE_HIER .
ENDCLASS.



CLASS ZMHP_CL_MIG_POS_HIER IMPLEMENTATION.


  METHOD CONSTRUCTOR.

    me->begda = begda.
    me->endda = endda.
    me->objects = objec.

  ENDMETHOD.


  METHOD CREATE_METADATA_HIER.

    CONCATENATE zmhp_cl_mig_utils=>metadata_hier
                position_hier
                'BusinessUnitId'
                'PositionCode'
                'BusinessUnitName'
                'ParentBusinessUnitId'
                'ParentPositionCode'
                'ParentBusinessUnitName'
                'EffectiveEndDate'
                'EffectiveStartDate'
                'ParentPositionId'
                'PositionHierarchyId'
                'PositionId'
                'GUID'
                'SourceSystemId'
                'SourceSystemOwner'
   INTO metadata_hier SEPARATED BY zmhp_cl_mig_utils=>separator.

  ENDMETHOD.


  METHOD DOWNLOAD_FILES_PH.

    DATA: content   TYPE stringtab,
          filename  TYPE string,
          content_x TYPE xstring,
          zip_file  TYPE string,
          zip_tab   TYPE swxmlcont.



    LOOP AT files_ph ASSIGNING FIELD-SYMBOL(<files>).
      APPEND <files>-value TO content.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename = <files>-name
          filetype = 'DAT'
          codepage = '4110' "UTF-8
        TABLES
          data_tab = content.
      CLEAR: content.
    ENDLOOP.



  ENDMETHOD.


  method GET_BTRTL.


    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      btrtl = <it1008>-btrtl.
    ENDLOOP.

  endmethod.


  METHOD get_bukrs.

*    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008_position>) WHERE objid = position_object_id AND otype = 'S'.
*      bukrs = <it1008_position>-bukrs.
*    ENDLOOP.
*
*    IF sy-subrc <> 0.
*      LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008_orgunit>) WHERE objid = orgunit_object_id AND otype = 'O'.
*        bukrs = <it1008_orgunit>-bukrs.
*      ENDLOOP.
*    ENDIF.


    DATA: lt_holder TYPE TABLE OF hrwpc_rc_pd_holders.
    DATA: ls_holder TYPE hrwpc_rc_pd_holders.
    DATA: ls_pa0001 TYPE pa0001.
    DATA: objektid TYPE objektid.


    objektid = position_object_id.

    CALL FUNCTION 'HRMSS_RFC_PP_HOLDERS'
      EXPORTING
        plvar  = plvar
        otype  = 'S'
        objid  = objektid
        begda  = begda
        endda  = endda
* IMPORTING
*       RETURN =
      TABLES
        holder = lt_holder.


    IF lt_holder IS NOT INITIAL.
      LOOP AT lt_holder INTO ls_holder.
        SELECT * FROM pa0001 INTO CORRESPONDING FIELDS OF ls_pa0001 WHERE pernr = ls_holder-pernr AND begda LE endda AND
                                                                            endda GE begda.
        ENDSELECT.
        bukrs = ls_pa0001-bukrs.
      ENDLOOP.
    ENDIF.


  ENDMETHOD.


  METHOD GET_BUKRS_TEXT.

    LOOP AT bukrs_texts ASSIGNING FIELD-SYMBOL(<bukrs>) WHERE bukrs = bukrs.
      bukrs_text = <bukrs>-butxt.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_bukrs_txt.

    DATA: ls_t001 TYPE t001.
    SELECT * FROM t001 INTO CORRESPONDING FIELDS OF ls_t001
      WHERE bukrs = bukrs.
    ENDSELECT.


    bukrs_txt = ls_t001-butxt.
  ENDMETHOD.


  METHOD GET_EFFECTIVE_START_DATE.

    DATA: leading_pos_begda   TYPE begda,
          connection_hier_begda TYPE begda.

    LOOP AT struc_t ASSIGNING FIELD-SYMBOL(<struct>) WHERE objid = object_id.
      leading_pos_begda = <struct>-vbegda.
    ENDLOOP.

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      connection_hier_begda = <connection_to_orgunit>-begda.
    ENDLOOP.

    IF connection_hier_begda > leading_pos_begda.
      effective_start_date = connection_hier_begda.
    ELSE.
      effective_start_date = leading_pos_begda.
    ENDIF.

  ENDMETHOD.


  method GET_EFFECTIVE_START_DATE_HIER.


  endmethod.


  METHOD GET_JOB_DATA.

    DATA: job_object_id TYPE hrobjid.

    DELETE connections_to_jobs WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT connections_to_jobs ASSIGNING FIELD-SYMBOL(<connection_to_job>) WHERE objid = object_id.
      job_object_id = <connection_to_job>-sobid.
    ENDLOOP.

    DELETE jobs WHERE objid = job_object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT jobs ASSIGNING FIELD-SYMBOL(<job>) WHERE objid = job_object_id.
      job_name = <job>-stext.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_kostl_of_orgid.

    DATA: ls_csks TYPE csks.
    DATA: ls_hrp1001 TYPE hrp1001.
    DATA: ls_t001 TYPE t001.
    DATA: bukrs      TYPE bukrs,
          manager_id TYPE realo,
          ls_pa0001  TYPE pa0001.


    LOOP AT struc_t ASSIGNING FIELD-SYMBOL(<struct>) WHERE otype = 'O'.

      SELECT * FROM hrp1001 INTO ls_hrp1001
        WHERE objid = <struct>-objid
        AND sclas = 'K'
        AND begda LE sy-datum
        AND endda GE sy-datum.
      ENDSELECT.


      CLEAR: ls_csks.
      SELECT * FROM csks INTO CORRESPONDING FIELDS OF ls_csks
        WHERE kostl = ls_hrp1001-sobid
        AND datab LE sy-datum
        AND datbi GE sy-datum.
      ENDSELECT.

      IF ls_csks IS NOT INITIAL.
        bukrs = ls_csks-bukrs.
      ENDIF.


      IF bukrs IS NOT INITIAL.

        SELECT * FROM t001 INTO CORRESPONDING FIELDS OF ls_t001
          WHERE bukrs = bukrs.
        ENDSELECT.

        bukrs_txt1 = ls_t001-butxt.

      ENDIF.

      IF bukrs_txt1 IS INITIAL.

        CALL FUNCTION 'RH_GET_LEADER'
          EXPORTING
            plvar                     = plvar
            keydate                   = begda
            otype                     = 'O'
            objid                     = <struct>-objid
            consider_vac_pos          = 'X'
          IMPORTING
            leader_id                 = manager_id
          EXCEPTIONS
            no_leader_found           = 1
            no_leading_position_found = 2
            OTHERS                    = 3.

        IF sy-subrc EQ 0.

          SELECT * FROM pa0001 INTO CORRESPONDING FIELDS OF ls_pa0001
            WHERE pernr = manager_id
            AND begda LE sy-datum
            AND endda GE sy-datum.
          ENDSELECT.

          IF ls_pa0001 IS NOT INITIAL.
            bukrs = ls_pa0001-bukrs.

            SELECT * FROM t001 INTO CORRESPONDING FIELDS OF ls_t001
               WHERE bukrs = bukrs.
            ENDSELECT.

            bukrs_txt1 = ls_t001-butxt.

          ENDIF.

        ENDIF.
      ENDIF.


    ENDLOOP.
  ENDMETHOD.


  METHOD GET_MAPPING_FIELDS.

    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0001'
                                                     sap_field    = 'BUKRS'
                                                     oracle_field = 'BUSINESSUNITSHORTCODE'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs ).




    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0001'
                                                     sap_field    = 'BUKRS'
                                                     oracle_field = 'NORMALHOURS'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs ).

  ENDMETHOD.


  METHOD GET_MAPPING_VALUES.

    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0001'
                                                   sap_field    = 'BUKRS'
                                                   oracle_field = 'BUSINESSUNITSHORTCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs ).




    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                  infty        = '0001'
                                                  sap_field    = 'BUKRS'
                                                  oracle_field = 'NORMALHOURS'
                                                  export       = abap_true
                                        IMPORTING mapping_values = mapping_values_bukrs ).

  ENDMETHOD.


  METHOD GET_ORGUNIT_DATA.

    DELETE connections_to_orgunits WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      orgunit_object_id = <connection_to_orgunit>-sobid.
    ENDLOOP.

    DELETE orgunits WHERE objid = orgunit_object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT orgunits ASSIGNING FIELD-SYMBOL(<orgunit>) WHERE objid = orgunit_object_id.
      orgunit_name = <orgunit>-objid.
    ENDLOOP.

  ENDMETHOD.


  METHOD GET_PERSA.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      persa = <it1008>-persa.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_pos_data.


    READ TABLE objects ASSIGNING FIELD-SYMBOL(<object>) index 1.


    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype    = 'O'
        act_objid    = <object>-objid
        act_wegid    = 'O-O-S'
      TABLES
*       RESULT_TAB   =
*       RESULT_OBJEC =
        result_struc = struc_t
 EXCEPTIONS
       NO_PLVAR_FOUND         = 1
       NO_ENTRY_FOUND         = 2
       OTHERS       = 3
      .
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.



  ENDMETHOD.


  METHOD GET_SET_CODE.

*DEBIEAKSA auskommentiert, da nicht nötig.
*    CASE sy-mandt.
*      WHEN '102'.
*        set_code = 'DE_SET'.
*      WHEN '005'.
*        set_code = 'FR_SET'.
*      WHEN OTHERS.
*        set_code = ''.
*    ENDCASE.


  ENDMETHOD.


  METHOD GET_WERKS.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      werks = <it1008>-werks.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_mig_values.

    DATA: value_tmp TYPE zmhp_dd_value.

    "Instantiate mapping class
    DATA(mapping_handler) = NEW zmhp_cl_int_mapping( ).

    value_tmp = CONV #( bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0001'
        field_sap      = 'BUKRS'
        field_oracle   = 'BUSINESSUNITSHORTCODE'
        mapping_fields = CONV #( mapping_fields_bukrs )
        mapping_values = CONV #( mapping_values_bukrs )
     CHANGING
       value           = value_tmp ).

    business_unit_name             = value_tmp.



    value_tmp = CONV #( bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0001'
        field_sap      = 'BUKRS'
        field_oracle   = 'NORMALHOURS'
        mapping_fields = CONV #( mapping_fields_bukrs )
        mapping_values = CONV #( mapping_values_bukrs )
     CHANGING
       value           = value_tmp ).

    working_hours             = value_tmp.


*DEBIEAKSA: Je nachdem wie working_hours gefüllt ist, soll es auch gefüllt sein
    IF working_hours IS INITIAL.
      frequency = ''.
    ELSE.
      frequency = 'W'.
    ENDIF.

  ENDMETHOD.


  METHOD map_pos_data.

    DATA: src_id                        TYPE string,
          sys_id                        TYPE string,
          hr_periods                    TYPE hrperiods_tab,
          hr_periods_line               TYPE hrperiods,
          bukrs                         TYPE bukrs,
          werks                         TYPE werks_d,
          persa                         TYPE persa,
          btrtl                         TYPE btrtl,
          orgunit_name                  TYPE stext,
          orgunit_object_id             TYPE hrobjid,
          job_name                      TYPE stext,
          oldest_dataset_begda          TYPE begda,
          count                         TYPE i,
          is_deleted                    TYPE bool,
          set_code                      TYPE string,
          leading_pos                   TYPE hrobject_t,
          parentpositioncode            TYPE objid,
          klammer1(1)                   TYPE c,
          klammer2(1)                   TYPE c,
          business_code2(40)            TYPE c,
          business_code1(40)            TYPE c,
          business_unit_name(70)        TYPE c,
          parent_business_unit_name(70) TYPE c,
          plvar                         TYPE plvar.


    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT struc_t ASSIGNING FIELD-SYMBOL(<struct>) WHERE otype = 'S'.
      CLEAR: leading_pos,
             parentpositioncode.

      CALL FUNCTION 'RH_GET_LEADING_POSITION'
        EXPORTING
          plvar             = '01'
          otype             = 'S'
          sobid             = <struct>-objid
          date              = sy-datum "<struct>-vbegda
*         BUFFER_MODE       = ' '
*         consider_vac_pos  = 'X'
        TABLES
          leading_pos       = leading_pos
        EXCEPTIONS
          no_lead_pos_found = 1
          OTHERS            = 2.
      IF sy-subrc EQ 0.
        READ TABLE leading_pos ASSIGNING FIELD-SYMBOL(<struc>) INDEX 1.
        parentpositioncode = <struc>-objid.
      ENDIF.


      src_id = pos_hier && '_' && <struct>-objid.
      DATA(positioncode) = <struct>-objid.

      DATA(effective_start_date) = zmhp_cl_mig_utils=>convert_date( get_effective_start_date( <struct>-objid ) ).
      DATA(effective_end_date) = zmhp_cl_mig_utils=>convert_date( <struct>-vendda ).

*DEBIEAKSA 20220727 hinzugefügt. Buchungskreisermittlung hat die Logik über den Inhaber der Planstelle
      bukrs = get_bukrs( position_object_id = <struct>-objid
                         begda = <struct>-vbegda
                         endda = <struct>-vendda
                         plvar = plvar ).

      DATA(bukrs_txt) = get_bukrs_txt( bukrs = bukrs ).

      IF bukrs_txt IS INITIAL.

        DATA(bukrs_txt1) = get_kostl_of_orgid( objid = <struct>-objid
                                               begda = <struct>-vbegda
                                               plvar = plvar ).

        bukrs_txt = bukrs_txt1.

      ENDIF.

      business_code1 = 'WMS'.
      klammer1 = '('.
      klammer2 = ')'.

      CLEAR: business_code2.
      CONCATENATE klammer1 business_code1 klammer2 INTO business_code2.
      CLEAR: business_unit_name,
             parent_business_unit_name.
      CONCATENATE bukrs_txt business_code2 INTO business_unit_name SEPARATED BY space.
      CONCATENATE bukrs_txt business_code2 INTO parent_business_unit_name SEPARATED BY space.


      CONCATENATE zmhp_cl_mig_utils=>merge
              position_hier
              ''                      "businessunitid
              positioncode            "PositionCode
              business_unit_name                  "Bei Homag immer WMS -> BusinessUnitName
              ''                      "parentbusinessunitid
              parentpositioncode
              parent_business_unit_name                 "Bei Homag immer WMS ->ParentBusinessUnitName
              effective_end_date      "EffectiveEndDate
              effective_start_date    "EffectiveStartDate
              ''                      "ParentPositionId
              ''                      "PositionHierarchyId
              ''                      "PositionId
              ''                      "GUID
              src_id
              sys_id
      INTO DATA(data_tmp_hier) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data_hier cl_abap_char_utilities=>newline data_tmp_hier INTO data_hier.

    ENDLOOP.

  ENDMETHOD.


  METHOD PROCEED_FILE_CONSTRUCTION.

    CONCATENATE metadata_hier data_hier INTO content_hier SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD PROCEED_POS_DATA.

    get_pos_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    data = map_pos_data( ).

  ENDMETHOD.
ENDCLASS.
