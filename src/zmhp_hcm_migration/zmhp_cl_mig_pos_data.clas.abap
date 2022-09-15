CLASS zmhp_cl_mig_pos_data DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    DATA objects TYPE objec_t .
    DATA begda TYPE begda .
    DATA endda TYPE endda .
    CONSTANTS position TYPE string VALUE 'Position' ##NO_TEXT.
    CONSTANTS pos TYPE string VALUE 'POS_' ##NO_TEXT.
    DATA molga TYPE rsdsselopt_t .
    DATA p0001 TYPE p0001_tab .
    data FTE_LE type ZMHP_TT_FTE_LE .

    METHODS constructor
      IMPORTING
        !begda TYPE begda
        !endda TYPE endda
        !objec TYPE objec_t .
    METHODS proceed_pos_data
      RETURNING
        VALUE(data) TYPE string .
    METHODS create_metadata
      RETURNING
        VALUE(metadata) TYPE string .
    METHODS download_files
      IMPORTING
        !files TYPE /iwbep/t_mgw_name_value_pair .
    METHODS proceed_file_construction
      IMPORTING
        !metadata      TYPE string
        !data          TYPE string
      RETURNING
        VALUE(content) TYPE string .
  PROTECTED SECTION.
private section.

  data COST_CENTERS type HRBPREP_COST_DIST_IT .
  data COST_CENTERS_TMP type HRBPREP_COST_DIST_IT .
  data IT1008 type ZMHP_HRP1008_T .
  data ORGUNITS type ZMHP_HRP1000_T .
  data CONNECTIONS_TO_JOBS type HRP1001_T .
  data CONNECTIONS_TO_ORGUNITS type HRP1001_T .
  data POSITIONS type ZMHP_HRP1000_T .
  data JOBS type ZMHP_HRP1000_T .
  data BUKRS_TEXTS type T_T001 .
  data MAPPING_FIELDS_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data CONNECTIONS_TO_KOSTL type HRP1001_T .
  data MAPPING_VALUES_HFM type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_HFM type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_FIELDS_LEGAL type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_LEGAL type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .

  methods SET_WORKING_HOURS .
  methods SET_OVERLAP
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
      !POSITION_OBJECT_ID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PLVAR type PLVAR
      !STEXT type STEXT
    returning
      value(OVERLAP) type STRING .
  methods GET_KOSTL_OF_POSITION
    importing
      !OBJID type HROBJID
    returning
      value(BUKRS_TXT1) type BUTXT .
  methods GET_KOSTL_OF_ORGID .
  methods GET_KOSTL_DATA
    importing
      !OBJECT_ID type HROBJID
    exporting
      value(ORGUNIT_NAME) type STEXT
      value(ORGUNIT_OBJECT_ID) type HROBJID
    returning
      value(GET_KOSTL_DATA) type KOSTL .
  methods GET_BUKRS_TXT
    importing
      !BUKRS type BUKRS
    returning
      value(BUKRS_TXT) type BUTXT .
  methods GET_BUKRS_DATA
    importing
      !KOSTL type KOSTL
    returning
      value(GET_BUKRS) type BUKRS .
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
  methods GET_KOSTL_OF_FIRST_EMP
    importing
      !OBJID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
    returning
      value(KOSTL) type KOSTL .
  methods MAP_POS_DATA
    returning
      value(DATA) type STRING .
  methods GET_POS_DATA .
  methods GET_WORKING_HOURS
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
      !POSITION_OBJECT_ID type HROBJID
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PLVAR type PLVAR
    returning
      value(WORKING_HOURS) type WOSTD .
  methods GET_BUKRS
    importing
      !ORGUNIT_OBJECT_ID type HROBJID
      !POSITION_OBJECT_ID type HROBJID
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
      !OBJECT_ID type HROBJID
      !DATUM type BEGDA
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
  methods GET_POSITION_SHORT
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PLVAR type PLVAR
      !POSITION_OBJECT_ID type HROBJID
    returning
      value(SHORT) type HRP1000-SHORT .
  methods GET_HFM_CODE
    importing
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !PLVAR type PLVAR
      !POSITION_OBJECT_ID type HROBJID
    returning
      value(HFM_CODE) type STRING .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_VALUES .
  methods MAP_MIG_VALUES
    importing
      !BUKRS type BUKRS
    exporting
      !BUSINESS_UNIT_NAME type ZMHP_DD_VALUE
      !WORKING_HOURS type ZMHP_DD_VALUE
      !FREQUENCY type ZMHP_DD_VALUE
      !BUKRS_TXT type ZMHP_DD_VALUE .
ENDCLASS.



CLASS ZMHP_CL_MIG_POS_DATA IMPLEMENTATION.


  METHOD constructor.

    me->begda = begda.
    me->endda = endda.
    me->objects = objec.

    me->set_working_hours( ).
  ENDMETHOD.


  METHOD create_metadata.

    CONCATENATE zmhp_cl_mig_utils=>metadata
                position
                'SourceSystemOwner'
                'EffectiveStartDate'
                'EffectiveEndDate'
                'BusinessUnitName'
                'Name'
*                'NameShort'
                'PositionCode'
                'ActionReasonCode'
                'ActiveStatus'
                'DepartmentId(SourceSystemId)'
                'JobCode'
                'JobSetCode'
                'LocationCode'
                'LocationSetCode'
                'SupervisorPersonNumber'
                'FullPartTime'
                'RegularTemporary'
                'HiringStatus'
                'PositionType'
                'FTE'
                'HeadCount'
                'SecurityClearance'
                'ProbationPeriod'
                'ProbationPeriodUnitCd'
                'BargainingUnitCd'
                'CollectiveAgreementCode'
                'OverlapAllowedFlag'
                'SeasonalFlag'
                'GradeLadderName'
                'EntryGradeCode'
                'EntryGradeSetCode'
                'EntryStepName'
                'StandardWorkingHours'
                'StandardWorkingFrequency'
                'WorkingHours'
                'Frequency'
                'StartTime'
                'EndTime'
                'SourceSystemId'
                'UnionName'
                'UnionClassificationCode'
                'SeasonalStartDate'
                'SeasonalEndDate'
                'RequisitionNumber'
                'AssignmentCategory'
                'BudgetAmount'
                'BudgetAmountCurrency'
                'BudgetedPositionFlag'
                'CostCenterName'
                'DelegatePositionCode'
                'DelegatePositionBusinessUnitName'
                'FundedByExistingPositionFlag'
                'FLEX:PER_POSITIONS_DFF'
                'accountingArea(PER_POSITIONS_DFF=Global Data Elements)' "'Accountingarea'
                'costCenter(PER_POSITIONS_DFF=Global Data Elements)' "'CostCenter'
                'XXHfmCode(PER_POSITIONS_DFF=Global Data Elements)'
                'XXPositionShortText(PER_POSITIONS_DFF=Global Data Elements)'
                'duSapId(PER_POSITIONS_DFF=Global Data Elements)'
   INTO metadata SEPARATED BY zmhp_cl_mig_utils=>separator.

  ENDMETHOD.


  METHOD download_files.

    DATA: content   TYPE stringtab,
          filename  TYPE string,
          content_x TYPE xstring,
          zip_file  TYPE string,
          zip_tab   TYPE swxmlcont.



    LOOP AT files ASSIGNING FIELD-SYMBOL(<files>).
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


  METHOD get_btrtl.


    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      btrtl = <it1008>-btrtl.
    ENDLOOP.

  ENDMETHOD.


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

    CALL FUNCTION 'HRMSS_RFC_PP_HOLDERS'
      EXPORTING
        plvar  = plvar
        otype  = 'S'
        objid  = position_object_id
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


  METHOD get_bukrs_data.

    DATA: ls_csks TYPE csks.

    SELECT * FROM csks
      INTO CORRESPONDING FIELDS OF ls_csks
      WHERE kostl = kostl
      AND datab LE sy-datum
      AND datbi GE sy-datum.
    ENDSELECT.

    IF ls_csks IS NOT INITIAL.
      get_bukrs = ls_csks-bukrs.
    ENDIF.


  ENDMETHOD.


  METHOD get_bukrs_text.

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


  METHOD get_effective_start_date.

    DATA: position_begda   TYPE begda,
          connection_begda TYPE begda.


    LOOP AT positions ASSIGNING FIELD-SYMBOL(<position>) WHERE objid = object_id AND begda = datum.
      position_begda = <position>-begda.
    ENDLOOP.

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      connection_begda = <connection_to_orgunit>-begda.
    ENDLOOP.

    IF connection_begda > position_begda.
      effective_start_date = connection_begda.
    ELSE.
      effective_start_date = position_begda.
    ENDIF.

  ENDMETHOD.


  METHOD get_hfm_code.

*  READ TABLE me->connections_to_jobs WITH KEY objid = position_object_id begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<connection_to_job>).
*  IF <connection_to_job> IS ASSIGNED.
*    DATA(jobs) = VALUE zmhp_hrp1000_t( FOR job IN me->jobs WHERE ( objid = <connection_to_job>-sobid AND endda BETWEEN begda AND endda ) ( job ) ).
*    SORT jobs BY begda endda DESCENDING."In case of multiple consider current.
*    READ TABLE jobs ASSIGNING FIELD-SYMBOL(<job>) INDEX 1.
*    IF <job> IS ASSIGNED.
*      CONCATENATE <job>-short ' ' <job>-stext INTO hfm_code RESPECTING BLANKS.
*      CONDENSE hfm_code.
*    ENDIF.
*  ENDIF.
*  HFM Code
    hfm_code = 'DUMMY'.

  ENDMETHOD.


  METHOD get_job_data.

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


  METHOD get_kostl_data.

    DATA: ls_connections_to_kostl TYPE HRP1001_T.

    DELETE connections_to_kostl WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries


    LOOP AT connections_to_kostl ASSIGNING FIELD-SYMBOL(<connection_to_kostl>) WHERE objid = object_id.
      get_kostl_data = <connection_to_kostl>-sobid.

    ENDLOOP.



  ENDMETHOD.


  METHOD get_kostl_of_first_emp.
    DATA: plvar        TYPE plvar,
          result_objec TYPE objec_t,
          ls_pa0001 TYPE pa0001.

    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    "get only employees in same OrgUnit
    CALL FUNCTION 'RH_STRUC_GET'
      EXPORTING
        act_otype      = 'S'
        act_objid      = objid
        act_wegid      = 'S_NACH_P'
        act_plvar      = plvar
        act_begda      = begda
        act_endda      = endda
      TABLES
        result_objec   = result_objec
      EXCEPTIONS
        no_plvar_found = 1
        no_entry_found = 2
        OTHERS         = 3.

    "keep only employees
    DELETE result_objec WHERE otype NE 'P'.

    CHECK result_objec IS NOT INITIAL.

    "get first employee
    READ TABLE result_objec ASSIGNING FIELD-SYMBOL(<employee>) INDEX 1.

    CHECK <employee> IS ASSIGNED.

    SELECT * FROM pa0001 INTO CORRESPONDING FIELDS OF ls_pa0001
      WHERE pernr =  <employee>-objid
      AND begda LE sy-datum
      AND endda GE sy-datum.
    ENDSELECT.

    IF ls_pa0001 IS NOT INITIAL.
      kostl = ls_pa0001-kostl.
    ENDIF.

*    "get cost center of employee
*    SELECT SINGLE kostl INTO @kostl FROM pa0001 WHERE pernr EQ @<employee>-objid AND
*                                                      begda LE @endda            AND
*                                                      endda GE @begda.
  ENDMETHOD.


  method GET_KOSTL_OF_ORGID.

*      DATA: ls_csks TYPE csks.
*    DATA: ls_hrp1001 TYPE hrp1001.
*    DATA: ls_t001 TYPE t001.
*    DATA: bukrs TYPE bukrs.
*
*
*    LOOP AT positions ASSIGNING FIELD-SYMBOL(<pos>) WHERE otype = 'S'.
*
*      SELECT * FROM hrp1001 INTO ls_hrp1001
*        WHERE objid = <pos>-objid
*        AND sclas = 'K'
*        AND begda LE sy-datum
*        AND endda GE sy-datum.
*      ENDSELECT.
*
*
*      CLEAR: ls_csks.
*      SELECT * FROM csks INTO CORRESPONDING FIELDS OF ls_csks
*        WHERE kostl = ls_hrp1001-sobid
*        AND datab LE sy-datum
*        AND datbi GE sy-datum.
*      ENDSELECT.
*
*      IF ls_csks IS NOT INITIAL.
*        bukrs = ls_csks-bukrs.
*      ENDIF.
*
*
*      IF bukrs IS NOT INITIAL.
*
*        SELECT * FROM t001 INTO CORRESPONDING FIELDS OF ls_t001
*          WHERE bukrs = bukrs.
*        ENDSELECT.
*
*        bukrs_txt1 = ls_t001-butxt.
*
*      ENDIF.
*
*
*    ENDLOOP.

  endmethod.


  method GET_KOSTL_OF_POSITION.

          DATA: ls_csks TYPE csks.
    DATA: ls_hrp1001 TYPE hrp1001.
    DATA: ls_t001 TYPE t001.
    DATA: bukrs TYPE bukrs.


    LOOP AT positions ASSIGNING FIELD-SYMBOL(<pos>) WHERE otype = 'S'.

      SELECT * FROM hrp1001 INTO ls_hrp1001
        WHERE objid = <pos>-objid
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


    ENDLOOP.

  endmethod.


  METHOD get_mapping_fields.

    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0001'
                                                     sap_field    = 'BUKRS'
                                                     oracle_field = 'BUSINESSUNITSHORTCODE'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs ).


    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                 infty        = '0001'
                                                 sap_field    = 'BUKRS'
                                                 oracle_field = 'LEGALEMPLOYERNAME'
                                                 export       = abap_true
                                       IMPORTING mapping_fields = mapping_fields_legal ).



    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0001'
                                                     sap_field    = 'BUKRS'
                                                     oracle_field = 'NORMALHOURS'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs ).

    "get mapping fields HFMCODE
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0033
                                                     sap_field    = 'AUS01'
                                                     oracle_field = zmhp_cl_mig_utils=>hfmcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_hfm ).

  ENDMETHOD.


  METHOD get_mapping_values.

    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                   infty        = '0001'
                                                   sap_field    = 'BUKRS'
                                                   oracle_field = 'BUSINESSUNITSHORTCODE'
                                                   export       = abap_true
                                         IMPORTING mapping_values = mapping_values_bukrs ).


    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                               infty        = '0001'
                                               sap_field    = 'BUKRS'
                                               oracle_field = 'LEGALEMPLOYERNAME'
                                               export       = abap_true
                                     IMPORTING mapping_values = mapping_values_legal ).



    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                  infty        = '0001'
                                                  sap_field    = 'BUKRS'
                                                  oracle_field = 'NORMALHOURS'
                                                  export       = abap_true
                                        IMPORTING mapping_values = mapping_values_bukrs ).


    "get mapping fields for HFMCODE
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0033
                                                     sap_field    = 'AUS01'
                                                     oracle_field = zmhp_cl_mig_utils=>hfmcode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_hfm ).

  ENDMETHOD.


  METHOD get_orgunit_data.

    DELETE connections_to_orgunits WHERE objid = object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT connections_to_orgunits ASSIGNING FIELD-SYMBOL(<connection_to_orgunit>) WHERE objid = object_id.
      orgunit_object_id = <connection_to_orgunit>-sobid.
    ENDLOOP.

    DELETE orgunits WHERE objid = orgunit_object_id AND endda LT sy-datum. "Keep only the actual and future entries

    LOOP AT orgunits ASSIGNING FIELD-SYMBOL(<orgunit>) WHERE objid = orgunit_object_id.
      orgunit_name = <orgunit>-objid.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_persa.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      persa = <it1008>-persa.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_position_short.

    READ TABLE me->positions WITH KEY objid = position_object_id begda = begda endda = endda ASSIGNING FIELD-SYMBOL(<position>).
    IF <position> IS ASSIGNED.
      short = <position>-short.
    ENDIF.

  ENDMETHOD.


  METHOD get_pos_data.

    DATA: system_language TYPE langu,
          plvar           TYPE plvar,
          start           TYPE i VALUE 1,
          end             TYPE i VALUE 500,
          objid           TYPE rsdsselopt_t,
          positions_tmp   TYPE zmhp_hrp1000_t.

    system_language = sy-langu.

    "get all objid from class attribute
    DATA(count_lines) = lines( objects ).
    DATA(all) = abap_false.


    WHILE end <= count_lines.

      LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>) FROM start TO end.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <object>-objid ) TO objid.
      ENDLOOP.

      start = end + 1.
      end = end + 500.
      IF end = count_lines.
        end = count_lines + 1.
      ELSEIF all = abap_false AND end > count_lines.
        end = count_lines.
        all = abap_true.
      ENDIF.

      "get all revelant positions from IT1000, the language should be equal to the system language
      SELECT objid,
             otype,
             begda,
             endda,
             langu,
             stext,
             short INTO CORRESPONDING FIELDS OF TABLE @positions_tmp FROM hrp1000 WHERE objid IN @objid AND
                                                                                    begda LE @endda AND
                                                                                    endda GE @begda AND
                                                                                    otype EQ 'S'    AND
                                                                                    plvar EQ '01'   AND
                                                                                    langu EQ @system_language.
      IF positions_tmp IS INITIAL.
        SELECT objid,
              otype,
              begda,
              endda,
              langu,
              stext,
              short INTO CORRESPONDING FIELDS OF TABLE @positions_tmp FROM hrp1000 WHERE objid IN @objid AND
                                                                                     begda LE @endda AND
                                                                                     endda GE @begda AND
                                                                                     otype EQ 'S'    AND
                                                                                     plvar EQ '01'.   "AND
*                                                                                    langu EQ @system_language.
      ENDIF.
      APPEND LINES OF positions_tmp TO positions.
      CLEAR: objid,
      positions_tmp.
    ENDWHILE.
    IF sy-subrc NE 0.
      EXIT.
    ENDIF.

    IF positions IS INITIAL.
      LOOP AT objects ASSIGNING FIELD-SYMBOL(<object_single>).
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <object_single>-objid ) TO objid.
      ENDLOOP.
      "get all revelant positions from IT1000, the language should be equal to the system language
      SELECT objid,
             otype,
             begda,
             endda,
             langu,
             stext,
             short INTO CORRESPONDING FIELDS OF TABLE @positions_tmp FROM hrp1000 WHERE objid IN @objid AND
                                                                                    begda LE @endda AND
                                                                                    endda GE @begda AND
                                                                                    otype EQ 'S'    AND
                                                                                    plvar EQ '01' AND
                                                                                    langu EQ @system_language.
      IF positions_tmp IS INITIAL.
        SELECT objid,
              otype,
              begda,
              endda,
              langu,
              stext,
              short INTO CORRESPONDING FIELDS OF TABLE @positions_tmp FROM hrp1000 WHERE objid IN @objid AND
                                                                                     begda LE @endda AND
                                                                                     endda GE @begda AND
                                                                                     otype EQ 'S'    AND
                                                                                     plvar EQ '01'. "AND
*                                                                                    langu EQ @system_language.
      ENDIF.
      APPEND LINES OF positions_tmp TO positions.

    ENDIF.

    "get orgunits
    SELECT objid,
           plvar,
           otype,
           begda,
           endda,
           stext INTO CORRESPONDING FIELDS OF TABLE @orgunits FROM hrp1000 WHERE begda LE @endda AND
                                                                                 endda GE @begda AND
                                                                                 otype EQ 'O'    AND
                                                                                 plvar EQ '01'.
*                                                                                 langu EQ @system_language.

    "get jobs
    SELECT objid,
           begda,
           endda,
           short,
           stext INTO CORRESPONDING FIELDS OF TABLE @jobs FROM hrp1000 WHERE begda LE @endda AND
                                                                             endda GE @begda AND
                                                                             otype EQ 'C'    AND
                                                                             plvar EQ '01'.
*                                                                                 langu EQ @system_language.

    "get connections to orgunits
    SELECT objid,
           begda,
           endda,
           sobid INTO CORRESPONDING FIELDS OF TABLE @connections_to_orgunits FROM hrp1001 WHERE objid IN @objid AND
                                                                                                begda LE @endda AND
                                                                                                endda GE @begda AND
                                                                                                otype EQ 'S'    AND
                                                                                                plvar EQ '01'   AND
                                                                                                relat EQ '003'  AND
                                                                                                sclas EQ 'O'.

    "get connections to jobs
    SELECT objid,
           begda,
           endda,
           sobid INTO CORRESPONDING FIELDS OF TABLE @connections_to_jobs FROM hrp1001 WHERE objid IN @objid AND
                                                                                            begda LE @endda AND
                                                                                            endda GE @begda AND
                                                                                            otype EQ 'S'    AND
                                                                                            plvar EQ '01'   AND
                                                                                            relat EQ '007'  AND
                                                                                            sclas EQ 'C'.

    "get connections to jobs
    SELECT objid,
           begda,
           endda,
           sobid INTO CORRESPONDING FIELDS OF TABLE @connections_to_kostl FROM hrp1001 WHERE objid IN @objid AND
                                                                                            begda LE @endda AND
                                                                                            endda GE @begda AND
                                                                                            otype EQ 'S'    AND
                                                                                            plvar EQ '01'   AND
                                                                                            relat EQ '011'  AND
                                                                                            sclas EQ 'K'.

    "get IT1008 for bukrs
    SELECT objid,
           otype,
           begda,
           endda,
           bukrs,
           persa,
           btrtl INTO CORRESPONDING FIELDS OF TABLE @it1008 FROM hrp1008 WHERE "objid IN @objid AND
                                                                               begda LE @endda AND
                                                                               endda GE @begda.

    "get bukrs text
    SELECT bukrs,
           butxt INTO CORRESPONDING FIELDS OF TABLE @bukrs_texts FROM t001.

  ENDMETHOD.


  METHOD get_set_code.

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


  METHOD get_werks.

    LOOP AT it1008 ASSIGNING FIELD-SYMBOL(<it1008>) WHERE objid = orgunit_object_id.
      werks = <it1008>-werks.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_working_hours.

    DATA: lt_holder TYPE TABLE OF hrwpc_rc_pd_holders.
    DATA: ls_holder TYPE hrwpc_rc_pd_holders.
    DATA: ls_pa0007 TYPE pa0007.

    CLEAR: lt_holder,
           ls_holder.
    CALL FUNCTION 'HRMSS_RFC_PP_HOLDERS'
      EXPORTING
        plvar  = plvar
        otype  = 'S'
        objid  = position_object_id
        begda  = begda
        endda  = endda
* IMPORTING
*       RETURN =
      TABLES
        holder = lt_holder.


    IF lt_holder IS NOT INITIAL.
      LOOP AT lt_holder INTO ls_holder.
        SELECT * FROM pa0007 INTO CORRESPONDING FIELDS OF ls_pa0007 WHERE pernr = ls_holder-pernr AND begda LE endda AND
                                                                            endda GE begda.
          IF working_hours IS INITIAL.
            working_hours = ls_pa0007-wostd.
          ELSEIF working_hours < ls_pa0007-wostd.
            working_hours = ls_pa0007-wostd.
          ENDIF.
        ENDSELECT.
*        working_hours = ls_pa0007-wostd.
      ENDLOOP.
    ENDIF.

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
        field_oracle   = 'LEGALEMPLOYERNAME'
        mapping_fields = CONV #( mapping_fields_legal )
        mapping_values = CONV #( mapping_values_legal )
     CHANGING
       value           = value_tmp ).

       bukrs_txt             = value_tmp.



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

    DATA: src_id               TYPE string,
          sys_id               TYPE string,
          hr_periods           TYPE hrperiods_tab,
          hr_periods_line      TYPE hrperiods,
          bukrs                TYPE bukrs,
          werks                TYPE werks_d,
          persa                TYPE persa,
          btrtl                TYPE btrtl,
          orgunit_name         TYPE stext,
          orgunit_object_id    TYPE hrobjid,
          job_name             TYPE stext,
          oldest_dataset_begda TYPE begda,
          count                TYPE i,
          is_deleted           TYPE bool,
          set_code             TYPE string,
          plvar                TYPE plvar,
          ls_p0001             TYPE p0001,
          ls_p0033             TYPE p0033,
          v_sobid              TYPE hrp1001-sobid,
          zv_sobid             TYPE hrp1001-sobid,
          austx                TYPE t543d-austx,
          klammer1(1)          TYPE c,
          klammer2(1)          TYPE c,
          business_code1       TYPE zdivision,
          value_tmp            TYPE zmhp_dd_value,
          accountingarea       TYPE bukrs.

    "Instantiate mapping class
    DATA(mapping_handler) = NEW zmhp_cl_int_mapping( ).

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    SORT positions ASCENDING.

    LOOP AT positions ASSIGNING FIELD-SYMBOL(<position>).

      IF is_deleted = abap_true.
        is_deleted = abap_false.
        CONTINUE.
      ENDIF.

      LOOP AT positions ASSIGNING FIELD-SYMBOL(<pos>) WHERE objid = <position>-objid.



        count = count + 1.
        IF count = 1.
          oldest_dataset_begda = <pos>-begda.
          DELETE positions WHERE objid = <pos>-objid AND endda LT sy-datum. "Keep only the actual and future entries
          IF <pos> IS NOT ASSIGNED.
            is_deleted = abap_true.
            CONTINUE.
          ENDIF.
        ELSE.
          <pos>-begda = oldest_dataset_begda.
        ENDIF.

        CONCATENATE pos <pos>-objid INTO src_id.

        get_orgunit_data( EXPORTING object_id = <pos>-objid
                          IMPORTING orgunit_name = orgunit_name
                                    orgunit_object_id = orgunit_object_id ).

        "MS202220719 Decision was made to not export positions with no current active relation to any orgunit
        CHECK orgunit_object_id IS NOT INITIAL.

        DELETE it1008 WHERE objid = <pos>-objid AND endda LT sy-datum. "Keep only the actual and future entries
*DEBIEAKSA 20220208 Mapping für Location_Code zusammengesetzt aus BUKRS + PERSA
*        CLEAR: bukrs,
*               persa.
        CLEAR: bukrs,
               accountingarea.
        bukrs = get_bukrs( position_object_id = <pos>-objid
                           orgunit_object_id = orgunit_object_id
                           begda = <pos>-begda
                           endda = <pos>-endda
                           plvar = plvar ).
**DEBIEAKSA auskommentiert, da Zusammensetzung aus PERSA UND BTRTL
**        werks = get_werks( orgunit_object_id = orgunit_object_id ).
**DEBIEAKSA hinzugefügt, da Zusammensetzung aus PERSA
*        persa = get_persa( orgunit_object_id = orgunit_object_id ).
**DEBIEAKSA hinzugefügt, da Zusammensetzung aus BTRTL
*        btrtl = get_btrtl( orgunit_object_id = orgunit_object_id ).


        DATA(overlap) = set_overlap( position_object_id = <pos>-objid
                                   stext = <pos>-stext
                                   orgunit_object_id = orgunit_object_id
                                   begda = <pos>-begda
                                   endda = <pos>-endda
                                   plvar = plvar ).


*DEBIEAKSA: Buchungskreis, Personalbereich, Personalteilbereich Selektion über Personalstammdaten

        DATA(kostl) = get_kostl_of_first_emp( objid = <pos>-objid
                                     begda = <pos>-begda
                                     endda = <pos>-endda ).



        IF kostl IS INITIAL.

          DATA(get_kostl_data) = get_kostl_data(  object_id = <pos>-objid ).

          kostl = get_kostl_data.

          IF sy-subrc NE 0.
            CLEAR: kostl.
          ENDIF.
        ENDIF.
*
        IF bukrs IS INITIAL.
          DATA(get_bukrs) = get_bukrs_data( kostl = kostl ).

          bukrs = get_bukrs.

        ENDIF.


        map_mig_values( EXPORTING bukrs = bukrs
                        IMPORTING bukrs_txt = DATA(bukrs_txt)
*                          business_unit_name = DATA(business_unit_name)
                          working_hours = DATA(working_hours)
*DEBIEAKSA hinzugefügt -> soll nämlich je nachdem wie working_hours gefüllt ist, auch dementsprechend gefüllt sein
                          frequency = DATA(frequency) ).





*    "get cost center of employee
*    SELECT SINGLE kostl INTO @kostl FROM pa0001 WHERE pernr EQ @<employee>-objid AND
*                                                      begda LE @endda            AND
*                                                      endda GE @begda.


        IF <pos>-otype = 'S'.
          SELECT sobid  FROM  hrp1001 INTO zv_sobid
            WHERE  otype  = 'S'
            AND    objid  = <pos>-objid
            AND    plvar  = '01'
            AND    begda  <= sy-datum
            AND    endda  >= sy-datum
            AND    subty  = 'A008'
            AND    sclas  = 'P'.
          ENDSELECT.


          "Get IT0001, um KOSTL, WERKS und BTRTl zu holen
          SELECT *
            FROM pa0001 INTO CORRESPONDING FIELDS OF ls_p0001 WHERE pernr = zv_sobid AND begda <= sy-datum AND endda >= sy-datum.
          ENDSELECT.

          "Get IT0033, um HFM Code zu holen
          SELECT *
            FROM pa0033 INTO CORRESPONDING FIELDS OF ls_p0033 WHERE pernr = zv_sobid AND begda <= sy-datum AND endda >= sy-datum AND subty = 'ZHFM'.
          ENDSELECT.
          IF ls_p0033-aus01  IS NOT INITIAL.
            value_tmp = CONV #( ls_p0033-aus01 ).
            mapping_handler->process_mapping(
              EXPORTING
                import         = abap_false
                export         = abap_true
                infty          = zmhp_cl_mig_utils=>it0033
                field_sap      = 'AUS01'
                field_oracle   = zmhp_cl_mig_utils=>hfmcode
                mapping_fields = CONV #( mapping_fields_hfm )
                mapping_values = CONV #( mapping_values_hfm )
*        fields         = dontknow
              CHANGING
                value          = value_tmp ).

            DATA(hfm_code) = value_tmp.
            CLEAR value_tmp.
          ENDIF.
        ENDIF.

        "prüfen, ob bukrs gefüllt ist, wenn ja, accountingarea mit dem Wert füllen
*        IF ls_p0001-bukrs IS NOT INITIAL.

        IF bukrs IS NOT INITIAL.
          accountingarea = bukrs.
*        ENDIF.

*          DATA(bukrs_txt) = get_bukrs_txt( bukrs = accountingarea ).
        ENDIF.

*        IF bukrs_txt IS INITIAL.
*
*          DATA(bukrs_txt1) = get_kostl_of_position( objid = <pos>-objid ).
*
*          bukrs_txt = bukrs_txt1.
*
*        ENDIF.


        business_code1 = 'WMS'.
        klammer1 = '('.
        klammer2 = ')'.


        CONCATENATE klammer1 business_code1 klammer2 INTO DATA(business_code2).
        CONCATENATE bukrs_txt business_code2 INTO DATA(business_unit_name) SEPARATED BY space.


        "prüfen, ob WERKS und BTRTL gefüllt sind, wenn ja, location_code mi beiden Werten füllen
        IF ls_p0001-werks IS NOT INITIAL AND ls_p0001-btrtl IS NOT INITIAL.
          CONCATENATE ls_p0001-werks ls_p0001-btrtl INTO DATA(location_code).
        ENDIF.

        IF location_code IS NOT INITIAL.
          set_code = 'COMMON'.
        ELSE.
          set_code = ''.
        ENDIF.




*JMB20210526 end insert

*DEBIEAKSA Costcenter setzt sich bei Oracle aus AccountingArea und kostl zusammen.
*        DATA(costcenter) = accountingarea && '-' && kostl.
        DATA(costcenter) = kostl.




*DEBIEAKSA auskommentiert -> sofern Workbook noch nicht gefüllt worden ist, soll dieses Feld nicht übergeben werden.
*        job_name = get_job_data( object_id = <pos>-objid ).

*DEBIEAKSA auskommentiert, da Zeile 69 abgefragt wird. Somit wird dieser Wert weiter oben übergeben
*        set_code = get_set_code( ).



        DATA(effective_start_date) = zmhp_cl_mig_utils=>convert_date( get_effective_start_date( datum = <pos>-begda
                                                                                                object_id = <pos>-objid ) ).
        DATA(effective_end_date) = zmhp_cl_mig_utils=>convert_date( <pos>-endda ).


*        bukrs = get_bukrs( position_object_id = <pos>-objid
*                           orgunit_object_id = orgunit_object_id ).

        CALL FUNCTION 'RH_GET_PLVAR'
          IMPORTING
            plvar = plvar.


**JMB20210526 start insert - in case no cost center was found

*        DATA(kostl) = get_kostl_of_first_emp( objid = <pos>-objid
*                                              begda = <pos>-begda
*                                              endda = <pos>-endda ).


        "MS20220912 -> FTE Fehler
        READ TABLE me->fte_le ASSIGNING FIELD-SYMBOL(<fte_entry>) WITH KEY legal_entity = accountingarea.
        DATA(whours) = get_working_hours( position_object_id = <pos>-objid
                         orgunit_object_id = orgunit_object_id
                         begda = <pos>-begda
                         endda = <pos>-endda
                         plvar = plvar ).
        IF <fte_entry> IS ASSIGNED.
          IF whours > <fte_entry>-working_hours.
            working_hours = whours.
            CONDENSE working_hours.
            frequency = 'W'.
          ELSE.
            working_hours = ''.
            frequency = ''.
          ENDIF.
        ELSE.
          working_hours = ''.
          frequency = ''.
        ENDIF.

*DEBIE2HAVE Read new fields position short code
        DATA(short) = get_position_short(
                                   position_object_id = <pos>-objid
                                   begda = <pos>-begda
                                   endda = <pos>-endda
                                   plvar = plvar ).


        CONCATENATE zmhp_cl_mig_utils=>merge
                          position
                          sys_id
                          effective_start_date
                          effective_end_date
*DEBIEAKSA bei Homag soll immer 'WMS' übergeben werden
                          business_unit_name
                          <pos>-stext "Name
*                          <pos>-short "DEBIEAKSA: Shorttext zur Positionsbezeichnung
                          <pos>-objid "PositionCode
                          '' "ActionReasonCode
                          'A' "ActiveStatus *DEBIEAKSA: Active soll zu "A" werden.
                          orgunit_name "DepartmentName
                          'DU_DEFAULT_JOB' "job_name "JobCode
                          set_code "JobSetCode
                          location_code "LocationCode
                          set_code "LocationSetCode
                          '' "SupervisorPersonNumber
                          '' "FullPartTime
                          '' "RegularTemporary
                          'Approved' "HiringStatus
                          'SINGLE' "PositionType
                          '' "FTE
                          '1' "HeadCount
                          '' "SecurityClearance
                          '' "ProbationPeriod
                          '' "ProbationPeriodUnitCd
                          '' "BargainingUnitCd
                          '' "CollectiveAgreementCode
                          overlap "OverlapAllowedFlag
                          '' "SeasonalFlag
                          '' "GradeLadderName
                          '' "EntryGradeCode
                          '' "EntryGradeSetCode
                          '' "set_code "EntryStepName
                          working_hours "'' "StandardWorkingHours
                          frequency "'' "StandardWorkingFrequency
                          '' "working_hours "WorkingHours
                          '' "frequency "Frequency
                          '' "StartTime
                          '' "EndTime
                          src_id "SourceSystemId
                          '' "UnionName
                          '' "UnionClassificationCode
                          '' "SeasonalStartDate
                          '' "SeasonalEndDate
                          '' "RequisitionNumber
                          '' "AssignmentCategory
                          '' "BudgetAmount
                          '' "BudgetAmountCurrency
                          '' "BudgetedPositionFlag
                          '' "CostCenterName
                          '' "DelegatePositionCode
                          '' "DelegatePositionBusinessUnitName
                          '' "FundedByExistingPositionFlag
                          'Global Data Elements' "PER_POSITIONS_DFF
                          accountingarea "DEBIEAKSA: Buchungskreis hinzugefügt
                          costcenter "DEBIEAKSA: costcenter hinzugefügt
                          hfm_code "XXHfmCode
                          short "XXPositionShortText
                          <pos>-objid
              INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

        CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
        CLEAR: working_hours, frequency, zv_sobid, hfm_code, ls_p0001, ls_p0033.
      ENDLOOP.
      count = 0.

    ENDLOOP.

  ENDMETHOD.


  METHOD proceed_file_construction.

    CONCATENATE metadata data INTO content SEPARATED BY cl_abap_char_utilities=>newline.

  ENDMETHOD.


  METHOD proceed_pos_data.

    get_pos_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    data = map_pos_data( ).

  ENDMETHOD.


  METHOD set_overlap.

    DATA: lt_holder TYPE TABLE OF hrwpc_rc_pd_holders.
    DATA: ls_holder TYPE hrwpc_rc_pd_holders.
    DATA: ls_pa0001 TYPE pa0001.
    DATA: stext1 TYPE string.

    CALL FUNCTION 'HRMSS_RFC_PP_HOLDERS'
      EXPORTING
        plvar  = plvar
        otype  = 'S'
        objid  = position_object_id
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

      ENDLOOP.


      IF ls_pa0001-persg = '9'.
        overlap = 'Y'.
      ELSE.
        overlap = 'N'.
      ENDIF.
    ELSE.
      stext1 = stext.
      TRANSLATE stext1 TO UPPER CASE.
      FIND 'EXTERN' IN stext1.
      IF sy-subrc EQ 0.
        overlap = 'Y'.
      ELSE.
        overlap = 'N'.
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD set_working_hours.

    DATA: fte_le_entry TYPE zmhp_s_fte_le.
    fte_le_entry-legal_entity = 5000.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 1199.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '0001'.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '0100'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '0175'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '0185'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 1010.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 1100.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 1500.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 2120.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 2700.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3000.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3170.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3310.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3370.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3380.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3400.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3700.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3800.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 3970.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4210.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4400.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4510.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4600.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4800.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4915.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4925.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4935.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 4950.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 5210.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 5510.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 6000.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 6200.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 6500.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 6600.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 8110.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 8200.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 8400.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 8670.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 9010.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = 9170.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '010'.
    fte_le_entry-working_hours = 38.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '002'.
    fte_le_entry-working_hours = 38.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '168'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '183'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '174'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '061'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '193'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '182'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '195'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '198'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '020'.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '027'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '060'.
    fte_le_entry-working_hours = 35.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '028'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '070'.
    fte_le_entry-working_hours = 38.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '075'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '080'.
    fte_le_entry-working_hours = 39.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '171'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '180'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '188'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '189'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '191'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '192'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '197'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

    fte_le_entry-legal_entity = '870'.
    fte_le_entry-working_hours = 40.
    APPEND fte_le_entry TO me->fte_le.

  ENDMETHOD.
ENDCLASS.
