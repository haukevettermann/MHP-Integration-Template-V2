class ZMHP_CL_MIG_ASSIGNMENT definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data P0001 type P0001_TAB .
  data MOLGA type RSDSSELOPT_T .
  data ASSIGNMENT_STRUCTURE type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants ASSIGNMENT type STRING value 'Assignment' ##NO_TEXT.
  constants ASSIGN type STRING value 'ASN_' ##NO_TEXT.
  data P0000 type P0000_TAB .
  constants WT type STRING value '_WT' ##NO_TEXT.
  constants E type STRING value 'E' ##NO_TEXT.
  data P0105 type ZMHP_TT_P0105_TB .
  constants STAT2 type ZMHP_DD_FIELD value 'STAT2' ##NO_TEXT.
  constants ASSIGNMENTSTATUSTYPECODE type ZMHP_DD_FIELD value 'ASSIGNMENTSTATUSTYPECODE' ##NO_TEXT.
  constants ASSIGNMENTTYPE type ZMHP_DD_FIELD value 'ASSIGNMENTTYPE' ##NO_TEXT.
  constants WORKERCATEGORY type ZMHP_DD_FIELD value 'WORKERCATEGORY' ##NO_TEXT.
  constants ASSIGNMENTCATEGORY type ZMHP_DD_FIELD value 'ASSIGNMENTCATEGORY' ##NO_TEXT.
  constants PERSONTYPECODE type ZMHP_DD_FIELD value 'PERSONTYPECODE' ##NO_TEXT.
  constants ASSIGNMENTPERSONTYPE type ZMHP_DD_FIELD value 'ASSIGNMENTPERSONTYPE' ##NO_TEXT.
  constants PERSK type ZMHP_DD_FIELD value 'PERSK' ##NO_TEXT.
  data HRP1000_ORGEH type HRTNM_P1000_TAB .
  data BUKRS_TXT type T_T001 .
  data HRP1000_STELL type HRTNM_P1000_TAB .
  data BU_COUNTRY type ZMHP_TT_T005T .
  constants SYSTEMPERSONTYPE type ZMHP_DD_FIELD value 'SYSTEMPERSONTYPE' ##NO_TEXT.
  constants NORMAL_HOURS type ZMHP_DD_FIELD value 'NORMALHOURS' ##NO_TEXT.
  data P0016 type P0016_TAB .
  data P0006 type P0006_TAB .
  data P0008 type P0008_TAB .
  data HR_PERIODS type HRPERIODS_TAB .
  constants CTTYP type ZMHP_DD_FIELD value 'CTTYP' ##NO_TEXT.
  data P0007 type P0007_TAB .
  data P0033 type P0033_TAB .

  methods PROCEED_COGU_ASSIGNMENT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_WKR_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !VP_WTERM_ID type ZMHP_CL_MIG_WORK_TERMS=>VP_WTERM_ID_T
      !WORKER type ref to ZMHP_CL_MIG_WORKER
      !WORKTERMS type ref to ZMHP_CL_MIG_WORK_TERMS
    exporting
      !WORK_MEASURE type STRING
    returning
      value(DATA) type STRING .
  methods PROCEED_COGL_ASSIGNMENT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_WKR_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !VP_WTERM_ID type ZMHP_CL_MIG_WORK_TERMS=>VP_WTERM_ID_T
      !WORKER type ref to ZMHP_CL_MIG_WORKER
      !WORKTERMS type ref to ZMHP_CL_MIG_WORK_TERMS
    exporting
      !WORK_MEASURE type STRING
    returning
      value(DATA) type STRING .
  methods PROCEED_COFU_ASSIGNMENT
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !VP_WKR_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T
      !VP_WTERM_ID type ZMHP_CL_MIG_WORK_TERMS=>VP_WTERM_ID_T
      !WORKER type ref to ZMHP_CL_MIG_WORKER
    exporting
      !DATA_ASSIGN_MAN type STRING
    returning
      value(DATA) type STRING .
  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  PROTECTED SECTION.
private section.

  types:
    T_T543D TYPE TABLE OF T543D .

  data MAPPING_FIELDS_PERSG_WT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSG_WT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_MASSN type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_MASSN type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_MASSG type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_MASSG type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_STAT2 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_STAT2 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSG_AT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSG_AT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_WC type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_WC type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_AC type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_AC type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_PTC type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_PTC type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_PERSK_SPT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_SPT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_BTRTL type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_FIELDS_HFM type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_HFM type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_VALUES_BTRTL type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  data VP_WKR_ID type ZMHP_CL_MIG_WORK_RELATION=>VP_WKR_ID_T .
  data VP_WTERM_ID type ZMHP_CL_MIG_WORK_TERMS=>VP_WTERM_ID_T .
  data MAPPING_FIELDS_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data PERNR_HISTORY type RSDSSELOPT_T .
  data MAPPING_FIELDS_BUKRS_BU type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_BUKRS_BU type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_DEPARTMENT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_DEPARTMENT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_HOURS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_HOURS type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  constants WEEKLY type STRING value 'Weekly' ##NO_TEXT.
  data COGU type BOOLEAN .
  data MAPPING_FIELDS_PERSK_APT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PERSK_APT type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_JOB type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_JOB type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_PRBEH type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_PRBEH type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_KDGF2 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_KDGF2 type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data MAPPING_FIELDS_CTTYP type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPP_FI .
  data MAPPING_VALUES_CTTYP type ZMHP_CL_MIG_UTILS=>ZMHP_TT_INT_MAPPING .
  data T543D type T_T543D .

  methods COLLECT_HR_PERIODS
    importing
      !PERNR type PERNR_D .
  methods CREATE_HIRE_COFU_ENTRY
    importing
      !PERIOD type HRPERIODS
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
  methods CREATE_HIRE_COGU_ENTRY
    returning
      value(DATA) type STRING .
  methods CREATE_HIRE_ENTRY
    returning
      value(DATA) type STRING .
  methods GET_BUSINESS_CODE
    importing
      !T001 type T001
      !ENDDA type ENDDA
    returning
      value(BUSINESS_CODE) type STRING .
  methods MAP_MIG_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
      !HIRE type BOOLEAN optional
      !P0016 type P0016
    exporting
      !MASSN type ZMHP_DD_VALUE
      !MASSG type ZMHP_DD_VALUE
      !WORKER_TYPE type ZMHP_DD_VALUE
      !ASSIGNMENT_TYPE type ZMHP_DD_VALUE
      !PERSON_TYPE_CODE type ZMHP_DD_VALUE
      !SYSTEM_PERSON_TYPE type ZMHP_DD_VALUE
      !ASSIGN_STATUS type ZMHP_DD_VALUE
      !WORKER_CATEGORY type ZMHP_DD_VALUE
      !ASSIGNMENT_CATEGORY type ZMHP_DD_VALUE
      !LOCATION_CODE type ZMHP_DD_VALUE
      !LEGAL_EMPLOYER_NAME type ZMHP_DD_VALUE
      !BUSINESS_UNIT_CODE type ZMHP_DD_VALUE
      !DEPARTMENT_NAME type ZMHP_DD_VALUE
      !NORMALHOURS type ZMHP_DD_VALUE .
  methods MAP_MIG_COGU_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
      !HIRE type BOOLEAN optional
      !JOB type STRING
      !P0016 type P0016
    exporting
      !MASSN type ZMHP_DD_VALUE
      !MASSG type ZMHP_DD_VALUE
      !WORKER_TYPE type ZMHP_DD_VALUE
      !ASSIGNMENT_TYPE type ZMHP_DD_VALUE
      !PERSON_TYPE_CODE type ZMHP_DD_VALUE
      !SYSTEM_PERSON_TYPE type ZMHP_DD_VALUE
      !ASSIGN_STATUS type ZMHP_DD_VALUE
      !WORKER_CATEGORY type ZMHP_DD_VALUE
      !ASSIGNMENT_CATEGORY type ZMHP_DD_VALUE
      !LOCATION_CODE type ZMHP_DD_VALUE
      !LEGAL_EMPLOYER_NAME type ZMHP_DD_VALUE
      !BUSINESS_UNIT_CODE type ZMHP_DD_VALUE
      !DEPARTMENT_NAME type ZMHP_DD_VALUE
      !NORMALHOURS type ZMHP_DD_VALUE
      !ASSIGNMENT_PERSON_TYPE_CODE type ZMHP_DD_VALUE
      !JOB_CODE type ZMHP_DD_VALUE .
  methods MAP_MIG_COFU_VALUES
    importing
      !P0001 type P0001
      !P0000 type P0000
      !HIRE type BOOLEAN optional
      !JOB type STRING
      !P0016 type P0016
      !P0033 type P0033 optional
    exporting
      !MASSN type ZMHP_DD_VALUE
      !MASSG type ZMHP_DD_VALUE
      !WORKER_TYPE type ZMHP_DD_VALUE
      !ASSIGNMENT_TYPE type ZMHP_DD_VALUE
      !PERSON_TYPE_CODE type ZMHP_DD_VALUE
      !SYSTEM_PERSON_TYPE type ZMHP_DD_VALUE
      !ASSIGN_STATUS type ZMHP_DD_VALUE
      !WORKER_CATEGORY type ZMHP_DD_VALUE
      !ASSIGNMENT_CATEGORY type ZMHP_DD_VALUE
      !LOCATION_CODE type ZMHP_DD_VALUE
      !LEGAL_EMPLOYER_NAME type ZMHP_DD_VALUE
      !BUSINESS_UNIT_CODE type ZMHP_DD_VALUE
      !DEPARTMENT_ID type ZMHP_DD_VALUE
      !NORMALHOURS type ZMHP_DD_VALUE
      !ASSIGNMENT_PERSON_TYPE_CODE type ZMHP_DD_VALUE
      !JOB_CODE type ZMHP_DD_VALUE
      !PROBATIONUNIT type ZMHP_DD_VALUE
      !NOTICEUOM type ZMHP_DD_VALUE
      !CONTRACTTYPE type ZMHP_DD_VALUE
      !HFMCODE type ZMHP_DD_VALUE .
  methods GET_MAPPING_VALUES .
  methods GET_MAPPING_FIELDS .
  methods GET_MAPPING_COGU_VALUES .
  methods GET_MAPPING_COGU_FIELDS .
  methods GET_MAPPING_COFU_VALUES .
  methods GET_MAPPING_COFU_FIELDS .
  methods GET_COGL_DATA .
  methods GET_COFU_DATA .
  methods MAP_COFU_DATA
    importing
      !PERNR type PERNR_D
    changing
      !PERIODS type HRPERIODS_TAB
    returning
      value(DATA) type STRING .
  methods MAP_COGL_DATA
    exporting
      !WORK_MEASURE type STRING
    returning
      value(DATA) type STRING .
  methods MAP_COGU_DATA
    exporting
      !WORK_MEASURE type STRING
    returning
      value(DATA) type STRING .
ENDCLASS.



CLASS ZMHP_CL_MIG_ASSIGNMENT IMPLEMENTATION.


  METHOD collect_hr_periods.

    DATA(p0000_pernr) = p0000.
    DATA(p0001_pernr) = p0001.
    DATA(p0006_pernr) = p0006.
    DATA(p0007_pernr) = p0007.
    DATA(p0008_pernr) = p0008.
    DATA(p0016_pernr) = p0016.
    DATA(p0105_pernr) = p0105.

    DELETE p0000_pernr WHERE pernr NE pernr.
    DELETE p0001_pernr WHERE pernr NE pernr.
    DELETE p0006_pernr WHERE pernr NE pernr.
    DELETE p0007_pernr WHERE pernr NE pernr.
    DELETE p0008_pernr WHERE pernr NE pernr.
    DELETE p0016_pernr WHERE pernr NE pernr.
    DELETE p0105_pernr WHERE pernr NE pernr.

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0000_pernr
                                       CHANGING  hr_periods = hr_periods ).

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0001_pernr
                                       CHANGING  hr_periods = hr_periods ).

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0006_pernr
                                       CHANGING  hr_periods = hr_periods ).

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0007_pernr
                                       CHANGING  hr_periods = hr_periods ).

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0008_pernr
                                       CHANGING  hr_periods = hr_periods ).

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0016_pernr
                                       CHANGING  hr_periods = hr_periods ).

    zmhp_cl_mig_utils=>get_hr_periods( EXPORTING table      = p0105_pernr
                                       CHANGING  hr_periods = hr_periods ).

  ENDMETHOD.


  METHOD constructor.

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cogl EQ abap_true OR
       cogu EQ abap_true.
      assignment_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                      ( name = 2  value = assignment )
                                      ( name = 3  value = 'SourceSystemOwner' )
                                      ( name = 4  value = 'SourceSystemId' )
                                      ( name = 6  value = 'ActionCode' )
                                      ( name = 7  value = 'EffectiveStartDate' )
                                      ( name = 8  value = 'EffectiveEndDate' )
                                      ( name = 9  value = 'EffectiveSequence' )
                                      ( name = 10 value = 'EffectiveLatestChange' )
                                      ( name = 14 value = 'WorkTermsAssignmentId(SourceSystemId)' )
                                      ( name = 15 value = 'AssignmentType' )
                                      ( name = 11 value = 'AssignmentName' )
                                      ( name = 12 value = 'AssignmentNumber' )
                                      ( name = 13 value = 'AssignmentStatusTypeCode' )
                                      ( name = 16 value = 'BusinessUnitShortCode' )
                                      ( name = 17 value = 'WorkerCategory' )
                                      ( name = 18 value = 'AssignmentCategory' )
                                      ( name = 19 value = 'JobCode' )
                                      ( name = 20 value = 'LocationCode' )
                                      ( name = 21 value = 'ManagerFlag' )
                                      ( name = 22 value = 'OrganizationId(SourceSystemId)' )
                                      ( name = 23 value = 'PeriodOfServiceId(SourceSystemId)' )
                                      ( name = 5  value = 'PersonId(SourceSystemId)' )
                                      ( name = 24 value = 'DateStart' )
                                      ( name = 25 value = 'WorkerType' )
                                      ( name = 32 value = 'LegalEmployerName' )
                                      ( name = 26 value = 'PersonTypeCode' )
                                      ( name = 27 value = 'SystemPersonType' )
                                      ( name = 28 value = 'PrimaryAssignmentFlag' )
                                      ( name = 29 value = 'PrimaryFlag' )
                                      ( name = 30 value = 'ReasonCode' )
                                      ( name = 31 value = 'InternalOfficeNumber' )
                                      ( name = 32 value = 'NormalHours' )
                                      ( name = 33 value = 'Frequency' ) ).

    ELSEIF cofu EQ abap_true.
      assignment_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                      ( name = 2  value = assignment )
                                      ( name = 3  value = 'SourceSystemOwner' )
                                      ( name = 4  value = 'SourceSystemId' )
                                      ( name = 6  value = 'ActionCode' )
                                      ( name = 7  value = 'EffectiveStartDate' )
                                      ( name = 8  value = 'EffectiveEndDate' )
                                      ( name = 9  value = 'EffectiveSequence' )
                                      ( name = 10 value = 'EffectiveLatestChange' )
                                      ( name = 14 value = 'WorkTermsAssignmentId(SourceSystemId)' )
                                      ( name = 15 value = 'AssignmentType' )
                                      ( name = 11 value = 'AssignmentName' )
                                      ( name = 12 value = 'AssignmentNumber' )
                                      ( name = 13 value = 'AssignmentStatusTypeCode' )
                                      ( name = 16 value = 'BusinessUnitShortCode' )
                                      ( name = 17 value = 'WorkerCategory' )
                                      ( name = 18 value = 'AssignmentCategory' )
                                      ( name = 19 value = 'JobCode' )
                                      ( name = 20 value = 'LocationCode' )
                                      ( name = 21 value = 'ManagerFlag' )
                                      ( name = 22 value = 'OrganizationId(SourceSystemId)' )
                                      ( name = 23 value = 'PeriodOfServiceId(SourceSystemId)' )
                                      ( name = 5  value = 'PersonId(SourceSystemId)' )
                                      ( name = 24 value = 'DateStart' )
                                      ( name = 25 value = 'WorkerType' )
                                      ( name = 32 value = 'LegalEmployerName' )
                                      ( name = 26 value = 'PersonTypeCode' )
                                      ( name = 27 value = 'SystemPersonType' )
                                      ( name = 28 value = 'PrimaryAssignmentFlag' )
                                      ( name = 29 value = 'PrimaryFlag' )
                                      ( name = 30 value = 'ReasonCode' )
                                      ( name = 31 value = 'InternalOfficeNumber' )
                                      ( name = 32 value = 'NormalHours' )
                                      ( name = 33 value = 'Frequency' )
                                      ( name = 34 value = 'DateProbationEnd' )
                                      ( name = 35 value = 'ReportingEstablishment' )
                                      ( name = 36 value = 'GradeCode' )
                                      ( name = 39 value = 'PositionCode' )
                                      ( name = 40 value = 'ProbationPeriod' )
                                      ( name = 41 value = 'ProbationUnit' )
                                      ( name = 42 value = 'GradeId' )
*                                      ( name = 42 value = 'CompanyCar' )
*                                      ( name = 42 value = 'HomeDistance' )
                                      ( name = 49 value = 'NoticePeriodUOM' )
*                                      ( name = 42 value = 'Contracttype' )
*                                      ( name = 42 value = 'Contracttypeenddate' )
                                      ( name = 43 value = 'duLegalEntity(PER_ASG_DF=Global Data Elements)' )
                                      ( name = 44 value = 'duCostCenter(PER_ASG_DF=Global Data Elements)' )
                                      ( name = 45 value = 'duAssgnControllingArea(PER_ASG_DF=Global Data Elements)' )
                                      ( name = 46 value = 'CollectiveAgreementIdCode' )
                                      ( name = 47 value = 'FullPartTime' )
*                                      ( name = 47 value = 'DefaultExpenseAccount' )
                                      ( name = 48 value = 'FLEX:PER_ASG_DF' )
                                      ( name = 50 value = 'durrAreaHomag(PER_ASG_DF=Global Data Elements)' )
                                      ( name = 51 value = 'durrSubareaHomag(PER_ASG_DF=Global Data Elements)' )
                                      ( name = 52 value = 'durrHfmCode(PER_ASG_DF=Global Data Elements)' ) ).
    ENDIF.
  ENDMETHOD.


  METHOD create_hire_cofu_entry.

    DATA: plvar                TYPE plvar,
          job_short            TYPE string,
          internal_number      TYPE string,
          frequency            TYPE string,
          normalhours          TYPE string,
          dateprob_end         TYPE string,
          rpt_est              TYPE string,
          grade_code           TYPE string,
          notice_per           TYPE string,
          pos_code             TYPE string,
          prob_period          TYPE string,
          grade_id             TYPE string,
          company_car          TYPE string,
          home_distance        TYPE string,
          cofu_notice_per      TYPE string,
          du_legal             TYPE string,
          du_costcenter        TYPE string,
          du_controlling       TYPE string,
          defaultexpense       TYPE string,
          collective_agreement TYPE string,
          prob_unit            TYPE string,
          flex_field           TYPE string,
          klammer1(1)          TYPE c,
          klammer2(1)          TYPE c,
          business_code1       TYPE zdivision,
          parttime             TYPE string,
          austx                TYPE t543d-austx.


    "get active plvar
    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

**JMB20211129 start insert - build dummy entry
*
    DELETE p0001 WHERE endda LE sy-datum.
*    DELETE p0000 WHERE endda LE sy-datum.
    DELETE p0016 WHERE endda LE sy-datum.

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ pernr    AND
                                                               begda LE sy-datum AND
                                                               endda GE sy-datum.
      EXIT.
    ENDLOOP.

    "get earliest entry
    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ pernr       AND
*                                                        begda LE period-begda AND
                                                        endda GE sy-datum.

      "Decision 06.08.2021: pass only actual legal employer due to import problems in Oracle
      <p0001>-bukrs = <p0001_actual>-bukrs. "JMB20210806 I

      ""pass only actual worker/assignment type due to worker type in work relationship
      <p0001>-persg = <p0001_actual>-persg.
      <p0001>-persk = <p0001_actual>-persk.
      DATA(area) = <p0001_actual>-zz_bu.
      DATA(subarea) = <p0001_actual>-zz_lcs.
      EXIT.
    ENDLOOP.

    "get relevant P0000
    LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE period-endda AND
                                                        endda GE period-begda AND
                                                        pernr EQ pernr.
      EXIT.
    ENDLOOP.

    "get P0007
    LOOP AT p0007 ASSIGNING FIELD-SYMBOL(<p0007>) WHERE begda LE sy-datum AND "JMB20210910 I
                                                        endda GE sy-datum AND "JMB20210910 I
                                                        pernr EQ pernr.
      normalhours = <p0007>-wostd.
      parttime = COND #( WHEN <p0007>-teilk EQ 'X' THEN 'Part time'
            ELSE 'Full time' ).
      CONDENSE normalhours.
      EXIT.
    ENDLOOP.

    "get P0016
    LOOP AT p0016 INTO DATA(p0016) WHERE        "ASSIGNING FIELD-SYMBOL(<p0016>) WHERE
                                                        endda GE sy-datum AND
                                                        pernr EQ <p0001>-pernr.
*      cofu_notice_per = cofu_notice_per = notice_per = prob_period = <p0016>-prbzt.
*      CONDENSE: cofu_notice_per, cofu_notice_per, notice_per, prob_period.

*
      EXIT.
    ENDLOOP.

*DEBIE2HAVE 20220509 Ausgabe HFM Code
    LOOP AT p0033 INTO DATA(p0033) WHERE begda LE period-endda AND
                                                        endda GE period-begda AND
                                                        pernr EQ pernr.
      EXIT.
    ENDLOOP.
*    IF <p0033> IS ASSIGNED.
*      SELECT SINGLE austx FROM t543d INTO austx
*              WHERE subty = <p0033>-subty
*              AND staus = <p0033>-aus01.
*      CONCATENATE '45' <p0033>-aus01 ' ' austx INTO DATA(hfm_code) RESPECTING BLANKS.
*      CONDENSE hfm_code.
*    ENDIF.

    map_mig_cofu_values( EXPORTING p0001 = <p0001>
                                   p0000 = <p0000>
                                   p0016 = p0016            "<p0016>
                                   p0033 = p0033
                                   hire  = abap_true
                                   job   = job_short
                         IMPORTING massn               = DATA(massn)
                                   massg               = DATA(massg)
                                   worker_type         = DATA(worker_type)
                                   assignment_type     = DATA(assignment_type)
                                   person_type_code    = DATA(person_type_code)
                                   system_person_type  = DATA(system_person_type)
                                   assign_status       = DATA(assign_status)
                                   worker_category     = DATA(worker_category)
                                   assignment_person_type_code = DATA(assignment_person_type_code)
                                   assignment_category = DATA(assignment_category)
                                   location_code       = DATA(location_code)
                                   legal_employer_name = DATA(legal_employer_name)
*                                   business_unit_code  = DATA(business_code)
                                   department_id     = DATA(department_id)
                                   job_code            = DATA(job)
                                   noticeuom           = DATA(notice_per_uom)
                                   hfmcode             = DATA(hfm_code)
                                   normalhours         = DATA(bukrs_hours) ). "JMB20210928 I - C400129651-5642
*    CLEAR: <p0016_tmp>.

    business_code1 = 'WMS'.
    klammer1 = '('.
    klammer2 = ')'.

    CONCATENATE klammer1 business_code1 klammer2 INTO DATA(business_code2).
    CONCATENATE legal_employer_name business_code2 INTO DATA(business_code) SEPARATED BY space.


**JMB20210928 start insert - in case no IT0007 was maintained retrieve hours from CompanyCode (C400129651-5642)
*
    normalhours = COND #( WHEN normalhours IS INITIAL THEN bukrs_hours
                          ELSE normalhours ).
*JMB20210928 insert end

    DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( period-begda ).
    DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( period-endda ).
    CONCATENATE assign <p0001>-pernr INTO DATA(src_id).
*DEBIEAKSA 20220504 auskommentiert, da Oracle diese Nummer automatisch generiert
*    CONCATENATE e <p0001>-pernr  INTO DATA(assign_num).

    "get source id
    DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = pernr
                                                      begda = period-begda
                                                      endda = period-endda
                                                      vp_src_id = vp_src_id ).

    DATA(is_manager) = zmhp_cl_mig_utils=>is_manager( <p0001> ).

    IF business_code NE zmhp_cl_mig_utils=>default_business_unit.
      "get orgunit text
      LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-orgeh AND
                                                                    begda LE <p0001>-endda AND
                                                                    endda GE <p0001>-begda.

        department_id = <hrp1000>-objid.

*        DATA(stext_length) = strlen( <hrp1000>-stext ) - 2.
*        CHECK <hrp1000>-stext+stext_length(2) NE business_code+0(2).
*        CONCATENATE <hrp1000>-stext '-' business_code+0(2) INTO department_id.
        EXIT.
      ENDLOOP.
    ENDIF.

    "Get relevant work relationship
    DATA(period_of_ser) = 'WR_NOT_FOUND'.
    LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                begda LE <p0001>-endda AND
                                                                endda GE <p0001>-begda.
      period_of_ser = <vp_wkr_id>-wkr_id.
      EXIT.
    ENDLOOP.

    "Get relevant work terms
    DATA(workterm) = 'WT_NOT_FOUND'.
    LOOP AT vp_wterm_id ASSIGNING FIELD-SYMBOL(<vp_wterm_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                    begda LE <p0001>-endda AND
                                                                    endda GE <p0001>-begda.
      workterm = <vp_wterm_id>-wterm_id.
      EXIT.
    ENDLOOP.

    DATA(default) = 'WT_'.

    IF workterm = 'WT_NOT_FOUND' OR workterm = 'WR_NOT_FOUND'.
      CONCATENATE default pernr INTO workterm.
    ENDIF.

    DATA(frequen) = weekly.
    IF normalhours IS INITIAL.
      CLEAR: frequen.
    ENDIF.

    massn = 'HIRE'.

    CONCATENATE zmhp_cl_mig_utils=>merge
                assignment
                sys_id
                src_id
                massn
                begda_tmp
                endda_tmp
                '1'
                zmhp_cl_mig_utils=>yes
                workterm
                assignment_type
                job
                '' "assign_num
                assign_status
                business_code
                '' "worker_category
                assignment_category
                ''
                location_code
                is_manager
                department_id
                period_of_ser
                src_sys_id
                begda_tmp
                worker_type
                legal_employer_name
                person_type_code
                system_person_type
                zmhp_cl_mig_utils=>yes
                zmhp_cl_mig_utils=>yes
                massg
                internal_number
                normalhours
                frequen  "JMB20210705 Pass frequency and normal hours in all assignment entries
                dateprob_end
                rpt_est
                grade_code
*                notice_per
                pos_code
                prob_period
                prob_unit
                grade_id
                notice_per
*               contracttype
*               contracttypeenddate
                du_legal
                du_costcenter
                du_controlling
                '' "collective_agreement - DEBIEAKSA: is not relevant for DUERR
*               defaultexpense
                parttime
                'Global Data Elements'
*               flex_field
                area
                subarea
                hfm_code
    INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

    CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
*    CLEAR: p0016, p0000.
  ENDMETHOD.


  METHOD create_hire_cogu_entry.

    DATA: plvar           TYPE plvar,
          job_short       TYPE string,
          internal_number TYPE string,
          frequency       TYPE string,
          normalhours     TYPE string,
          klammer1(1)     TYPE c,
          klammer2(1)     TYPE c,
          business_code1  TYPE zdivision.


    "get history assignment
    DATA(p0001_history) = p0001.

    DELETE p0001_history WHERE begda GT sy-datum.
    DELETE p0001_history WHERE begda LE sy-datum AND
                               endda GE sy-datum.

    "delete history from main table
    LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001_history>).
      DELETE TABLE p0001 FROM <p0001_history>.
    ENDLOOP.

    "only first assignment relevant
    SORT p0001_history BY pernr begda ASCENDING.
    DELETE ADJACENT DUPLICATES FROM p0001_history COMPARING pernr.

    "get actual assignment
    DATA(p0001_actual) = p0001.
    DELETE p0001_actual WHERE endda LT sy-datum OR
                              begda GT sy-datum.

    DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                               ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '34' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                               ( sign = 'I' option = 'EQ' low = '25' ) )
                                            WHEN zmhp_cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '25' ) )
                                            "default Italy/Austria
                                            ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                          ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).
    "get active plvar
    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.

      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "get relevant P0000
      LOOP AT p0016 ASSIGNING FIELD-SYMBOL(<p0016>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "get actual assignment and set enddate of history entry to actual begda - 1
      LOOP AT p0001_actual ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr.
        <p0001>-endda = <p0001_actual>-begda - 1.

        "Decision 06.08.2021: pass only actual legal employer due to import problems in Oracle
        <p0001>-bukrs = <p0001_actual>-bukrs. "JMB20210806 I

        ""pass only actual worker/assignment type due to worker type in work relationship
        <p0001>-persg = <p0001_actual>-persg.
        <p0001>-persk = <p0001_actual>-persk.
        DATA(area) = <p0001_actual>-zz_bu.
        DATA(subarea) = <p0001_actual>-zz_lcs.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "get P0007
      LOOP AT p0007 ASSIGNING FIELD-SYMBOL(<p0007>) WHERE begda LE sy-datum AND "JMB20210910 I
                                                          endda GE sy-datum AND "JMB20210910 I
                                                          pernr EQ <p0001>-pernr.
        normalhours = <p0007>-wostd.
        CONDENSE normalhours.
        EXIT.
      ENDLOOP.

      "in case of termination ignore entry
*    CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it??s provided in WorkRelationship

      CLEAR job_short.
      "get job text
      LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<stell>) WHERE objid EQ <p0001>-stell AND
                                                                  begda LE <p0001>-endda AND
                                                                  endda GE <p0001>-begda.
*DEBIEAKSA 20220824 JobCode soll leer ??bergeben werden
        job_short = ''.
        EXIT.
      ENDLOOP.

      map_mig_cogu_values( EXPORTING p0001 = <p0001>
                                     p0000 = <p0000>
                                     p0016 = <p0016>
                                     hire  = abap_true
                                     job   = job_short
                           IMPORTING massn               = DATA(massn)
                                     massg               = DATA(massg)
                                     worker_type         = DATA(worker_type)
                                     assignment_type     = DATA(assignment_type)
                                     person_type_code    = DATA(person_type_code)
                                     system_person_type  = DATA(system_person_type)
                                     assign_status       = DATA(assign_status)
                                     worker_category     = DATA(worker_category)
                                     assignment_person_type_code = DATA(assignment_person_type_code)
                                     assignment_category = DATA(assignment_category)
                                     location_code       = DATA(location_code)
                                     legal_employer_name = DATA(legal_employer_name)
*                                     business_unit_code  = DATA(business_code)
                                     department_name     = DATA(department_name)
                                     job_code            = DATA(job)
                                     normalhours         = DATA(bukrs_hours) ). "JMB20210928 I - C400129651-5642

      business_code1 = 'WMS'.
      klammer1 = '('.
      klammer2 = ')'.

      CONCATENATE klammer1 business_code1 klammer2 INTO DATA(business_code2).
      CONCATENATE legal_employer_name business_code2 INTO DATA(business_code) SEPARATED BY space.


**JMB20210928 start insert - in case no IT0007 was maintained retrieve hours from CompanyCode (C400129651-5642)
*
      normalhours = COND #( WHEN normalhours IS INITIAL THEN bukrs_hours
                            ELSE normalhours ).
*JMB20210928 insert end


      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-endda ).
      CONCATENATE assign <p0001>-pernr INTO DATA(src_id).
      CONCATENATE e <p0001>-pernr  INTO DATA(assign_num).

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(is_manager) = zmhp_cl_mig_utils=>is_manager( <p0001> ).

      IF business_code NE zmhp_cl_mig_utils=>default_business_unit.
        "get orgunit text
        LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-orgeh AND
                                                                      begda LE <p0001>-endda AND
                                                                      endda GE <p0001>-begda.
        department_name = <hrp1000>-objid.

*        DATA(stext_length) = strlen( <hrp1000>-stext ) - 2.
*        CHECK <hrp1000>-stext+stext_length(2) NE business_code+0(2).
*        CONCATENATE <hrp1000>-stext '-' business_code+0(2) INTO department_id.
          EXIT.
        ENDLOOP.
      ENDIF.

      "Get relevant work relationship
      DATA(period_of_ser) = 'WR_NOT_FOUND'.
      LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                  begda LE <p0001>-endda AND
                                                                  endda GE <p0001>-begda.
        period_of_ser = <vp_wkr_id>-wkr_id.
        EXIT.
      ENDLOOP.

      "Get relevant work terms
      DATA(workterm) = 'WT_NOT_FOUND'.
      LOOP AT vp_wterm_id ASSIGNING FIELD-SYMBOL(<vp_wterm_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                      begda LE <p0001>-endda AND
                                                                      endda GE <p0001>-begda.
        workterm = <vp_wterm_id>-wterm_id.
        EXIT.
      ENDLOOP.

      DATA(frequen) = weekly.
      IF normalhours IS INITIAL.
        CLEAR: frequen.
      ENDIF.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment
                  sys_id
                  src_id
                  massn
                  begda_tmp
                  endda_tmp
                  '1'
                  zmhp_cl_mig_utils=>yes
                  workterm
                  assignment_type
                  job
                  assign_num
                  assign_status
                  business_code
                  worker_category
                  assignment_category
                  ''
                  location_code
                  is_manager
                  department_name
                  period_of_ser
                  src_sys_id
                  begda_tmp
                  worker_type
                  legal_employer_name
                  person_type_code
                  system_person_type
                  zmhp_cl_mig_utils=>yes
                  zmhp_cl_mig_utils=>yes
                  massg
                  internal_number
                  normalhours
                  frequen  "JMB20210705 Pass frequency and normal hours in all assignment entries
                  area
                  subarea
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: endda, department_name, job, internal_number, normalhours.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_hire_entry.

    DATA: plvar           TYPE plvar,
          job             TYPE string,
          internal_number TYPE string,
          frequency       TYPE string,
          department_name TYPE string,
          klammer1(1)     TYPE c,
          klammer2(1)     TYPE c,
          business_code1  TYPE zdivision.


    "get history assignment
    DATA(p0001_history) = p0001.

    DELETE p0001_history WHERE begda GT sy-datum.
    DELETE p0001_history WHERE begda LE sy-datum AND
                               endda GE sy-datum.

    "delete history from main table
    LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001_history>).
      DELETE TABLE p0001 FROM <p0001_history>.
    ENDLOOP.

    "only first assignment relevant
    SORT p0001_history BY pernr begda ASCENDING.
    DELETE ADJACENT DUPLICATES FROM p0001_history COMPARING pernr.

    "get actual assignment
    DATA(p0001_actual) = p0001.
    DELETE p0001_actual WHERE endda LT sy-datum OR
                              begda GT sy-datum.

    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

    "get active plvar
    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT p0001_history ASSIGNING FIELD-SYMBOL(<p0001>).
      APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.

      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.



      CHECK sy-subrc EQ 0.

      "get actual assignment and set enddate of history entry to actual begda - 1
      LOOP AT p0001_actual ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr.
        <p0001>-endda = <p0001_actual>-begda - 1.

        ""pass only actual worker/assignment type due to worker type in work relationship
        <p0001>-persg = <p0001_actual>-persg.
        <p0001>-persk = <p0001_actual>-persk.

        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

*DEBIEAKSA hinzugef??gt, da Assignmenttype ??ber IT16 gemapped werden soll
      "get relevant P0000
      LOOP AT p0016 ASSIGNING FIELD-SYMBOL(<p0016>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination ignore entry
*    CHECK <p0000>-massn NOT IN massn_term. "JMB20210312 D: Provide entry even if it??s provided in WorkRelationship

      map_mig_values( EXPORTING p0001 = <p0001>
                                p0000 = <p0000>
                                p0016 = <p0016>
                                hire  = abap_true
                      IMPORTING massn               = DATA(massn)
                                massg               = DATA(massg)
                                worker_type         = DATA(worker_type)
                                assignment_type     = DATA(assignment_type)
                                person_type_code    = DATA(person_type_code)
                                system_person_type  = DATA(system_person_type)
                                assign_status       = DATA(assign_status)
                                worker_category     = DATA(worker_category)
                                assignment_category = DATA(assignment_category)
                                location_code       = DATA(location_code)
                                legal_employer_name = DATA(legal_employer_name)
*                                business_unit_code  = DATA(business_code)
                                normalhours        = DATA(normalhours) ).


      business_code1 = 'WMS'.
      klammer1 = '('.
      klammer2 = ')'.

      CONCATENATE klammer1 business_code1 klammer2 INTO DATA(business_code2).
      CONCATENATE legal_employer_name business_code2 INTO DATA(business_code) SEPARATED BY space.


      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-endda ).
      CONCATENATE assign <p0001>-pernr INTO DATA(src_id).
*DEBIEAKSA 20220504 auskommentiert, da Oracle diese Nummer automatisch generiert
*      CONCATENATE e <p0001>-pernr  INTO DATA(assign_num).

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(is_manager) = zmhp_cl_mig_utils=>is_manager( <p0001> ).

      IF business_code NE zmhp_cl_mig_utils=>default_business_unit.
        "get orgunit text
        LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-orgeh AND
                                                                      begda LE <p0001>-endda AND
                                                                      endda GE <p0001>-begda.
        department_name = <hrp1000>-objid.

*        DATA(stext_length) = strlen( <hrp1000>-stext ) - 2.
*        CHECK <hrp1000>-stext+stext_length(2) NE business_code+0(2).
*        CONCATENATE <hrp1000>-stext '-' business_code+0(2) INTO department_id.
          EXIT.
        ENDLOOP.

        "get job text
        LOOP AT hrp1000_stell ASSIGNING <hrp1000> WHERE objid EQ <p0001>-stell AND
                                                        begda LE <p0001>-endda AND
                                                        endda GE <p0001>-begda.
          job = ''..
          EXIT.
        ENDLOOP.
      ENDIF.

      "Get relevant work relationship
      DATA(period_of_ser) = 'WR_NOT_FOUND'.
      LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                  begda LE <p0001>-endda AND
                                                                  endda GE <p0001>-begda.
        period_of_ser = <vp_wkr_id>-wkr_id.
        EXIT.
      ENDLOOP.

      "Get relevant work terms
      DATA(workterm) = 'WT_NOT_FOUND'.
      LOOP AT vp_wterm_id ASSIGNING FIELD-SYMBOL(<vp_wterm_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                      begda LE <p0001>-endda AND
                                                                      endda GE <p0001>-begda.
        workterm = <vp_wterm_id>-wterm_id.
        EXIT.
      ENDLOOP.

      DATA(default) = 'WT_'.

      IF workterm = 'WT_NOT_FOUND' OR workterm = 'WR_NOT_FOUND'.
        CONCATENATE default <p0001>-pernr INTO workterm.
      ENDIF.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment
                  sys_id
                  src_id
                  massn
                  begda_tmp
                  endda_tmp
                  '1'
                  zmhp_cl_mig_utils=>yes
                  workterm
                  assignment_type
                  job
                  '' "assign_num
                  assign_status
                  business_code
                  worker_category
                  '' "assignment_category "JMB20210408 D - Only pass space as AssignmentCategory
                  ''
                  location_code
                  is_manager
                  department_name
                  period_of_ser
                  src_sys_id
                  begda_tmp
                  worker_type
                  legal_employer_name
                  person_type_code
                  system_person_type
                  zmhp_cl_mig_utils=>yes
                  zmhp_cl_mig_utils=>yes
                  massg
                  internal_number
                  normalhours
                  weekly  "JMB20210705 Pass frequency and normal hours in all assignment entries
               INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: endda, department_name, job, internal_number.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE assignment_structure LINES DATA(length).

    LOOP AT assignment_structure ASSIGNING FIELD-SYMBOL(<assign_struc>).

      "set METADATA title
      CASE <assign_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <assign_struc>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_business_code.

    "get country to bukrs
    READ TABLE bu_country INTO DATA(country) WITH KEY land1 = t001-land1.

    "concatenate BusinessUnitShortCode
    CONCATENATE t001-land1 t001-bukrs country-landx INTO business_code SEPARATED BY space.

    "01.03.2021 - Final decision: for history provide default value
    CHECK endda LT sy-datum.

    business_code = 'Data Migration BU'.

  ENDMETHOD.


  METHOD get_cofu_data.

    "Get IT0000
    IF p0000 IS INITIAL.
      SELECT pernr,
             begda,
             endda,
             massn,
             massg,
             stat1,
             stat2 INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                               begda LE @endda AND
                                                                               endda GE @begda.
    ENDIF.

    "Get IT0001
    SELECT pernr,
           begda,
           endda,
           bukrs,
           abkrs,
           persg,
           persk,
           plans,
           stell,
           werks,
           btrtl,
           orgeh,
           ansvh,
           zz_bu,
           zz_lcs,
           kostl INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

**JMB20210824 start insert - only pass office room number for Germany
*
    IF sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-germany.
      "Get IT0105
      SELECT pernr,
             begda,
             endda,
             subty,
             usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                               begda LE @endda AND
                                                                               endda GE @begda AND
                                                                               subty EQ @zmhp_cl_mig_utils=>it0105_9001.
    ENDIF.
*JMB20210824 end insert

    "get IT0006
    SELECT pernr,
           begda,
           endda,
           entkm,  "DE/AT/IT
           entk2  "FR
*           zzentkm "NL
      FROM pa0006 INTO CORRESPONDING FIELDS OF TABLE @p0006 WHERE pernr IN @pernr AND
                                                                  begda LE @endda AND
                                                                  endda GE @begda.

    "get IT0008
    SELECT pernr,
           begda,
           endda,
           trfar,
           trfgb,
           trfgr,
           trfst
      FROM pa0008 INTO CORRESPONDING FIELDS OF TABLE @p0008 WHERE pernr IN @pernr AND
                                                                  begda LE @endda AND
                                                                  endda GE @begda.

    "Get IT0007
    SELECT pernr,
           begda,
           endda,
           wostd FROM pa0007 INTO CORRESPONDING FIELDS OF TABLE @p0007 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

    "Get IT0016
    SELECT pernr,
           begda,
           endda,
           prbzt,
           prbeh,
           cttyp,
           ctedt,
           kdgf2
           FROM pa0016 INTO CORRESPONDING FIELDS OF TABLE @p0016 WHERE pernr IN @pernr AND
                                                                       begda LE @endda AND
                                                                       endda GE @begda.

    "get orgunit text
    DATA(orgeh) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-orgeh ) ).
    SORT orgeh BY low.
    DELETE ADJACENT DUPLICATES FROM orgeh COMPARING low.

    "get actual department name
    SELECT objid, begda, endda, stext FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_orgeh WHERE objid IN @orgeh    AND
                                                                                                           otype EQ 'O'       AND
                                                                                                           langu EQ @sy-langu AND
                                                                                                           begda LE @sy-datum AND
                                                                                                           begda LE @sy-datum.
    "begda LE @endda AND "JMB20211014 D
    "endda GE @begda.    "JMB20211014 D

    "get job name
    DATA(stell) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-stell ) ).
    SORT stell BY low.
    DELETE ADJACENT DUPLICATES FROM stell COMPARING low.

    SELECT objid, begda, endda, short FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_stell WHERE objid IN @stell    AND
                                                                                                           otype EQ 'C'       AND
                                                                                                           langu EQ @sy-langu AND
                                                                                                           begda LE @endda    AND
                                                                                                           endda GE @begda.

    "get bukrs text
    DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
    SORT bukrs BY low.
    DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.

    SELECT bukrs, butxt, land1 FROM t001 INTO CORRESPONDING FIELDS OF TABLE @bukrs_txt WHERE bukrs IN @bukrs.

    "get country to bukrs
    DATA(land1) = VALUE rsdsselopt_t( FOR <t001> IN bukrs_txt ( sign = 'I' option = 'EQ' low = <t001>-land1 ) ).
    SORT land1 BY low.
    DELETE ADJACENT DUPLICATES FROM land1 COMPARING low.

    SELECT land1, landx FROM t005t INTO CORRESPONDING FIELDS OF TABLE @bu_country WHERE land1 IN @land1    AND
                                                                                        spras EQ @sy-langu.

    "Get IT0033, um HFM Code zu holen
    SELECT *
      FROM pa0033 INTO CORRESPONDING FIELDS OF TABLE @p0033 WHERE pernr in @pernr AND begda <= @sy-datum AND endda >= @sy-datum AND subty = 'ZHFM'.

  ENDMETHOD.


  METHOD get_cogl_data.

    "Get IT0000
    IF p0000 IS INITIAL.
      SELECT pernr,
             begda,
             endda,
             massn,
             massg,
             stat2 INTO CORRESPONDING FIELDS OF TABLE @p0000 FROM pa0000 WHERE pernr IN @pernr AND
                                                                               begda LE @endda AND
                                                                               endda GE @begda.
    ENDIF.

    "Get IT0001
    SELECT pernr,
           begda,
           endda,
           bukrs,
           persg,
           persk,
           plans,
           stell,
           werks,
           btrtl,
           orgeh INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.

    "Get IT0105
    SELECT pernr,
           begda,
           endda,
           subty,
           usrid FROM pa0105 INTO CORRESPONDING FIELDS OF TABLE @p0105 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda AND
                                                                             subty EQ @zmhp_cl_mig_utils=>it0105_9001.

    "get orgunit text
    DATA(orgeh) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-orgeh ) ).
    SORT orgeh BY low.
    DELETE ADJACENT DUPLICATES FROM orgeh COMPARING low.

    SELECT objid, begda, endda, stext FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_orgeh WHERE objid IN @orgeh AND
                                                                                                           otype EQ 'O'   AND
                                                                                                           begda LE @endda AND
                                                                                                           endda GE @begda.

    "get job name
    DATA(stell) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-stell ) ).
    SORT stell BY low.
    DELETE ADJACENT DUPLICATES FROM stell COMPARING low.

    SELECT objid, begda, endda, short FROM hrp1000 INTO CORRESPONDING FIELDS OF TABLE @hrp1000_stell WHERE objid IN @stell AND
                                                                                                           otype EQ 'C'   AND
                                                                                                           begda LE @endda AND
                                                                                                           endda GE @begda.

    "get bukrs text
    DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
    SORT bukrs BY low.
    DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.

    SELECT bukrs, butxt, land1 FROM t001 INTO CORRESPONDING FIELDS OF TABLE @bukrs_txt WHERE bukrs IN @bukrs.

    "get country to bukrs
    DATA(land1) = VALUE rsdsselopt_t( FOR <t001> IN bukrs_txt ( sign = 'I' option = 'EQ' low = <t001>-land1 ) ).
    SORT land1 BY low.
    DELETE ADJACENT DUPLICATES FROM land1 COMPARING low.

    SELECT land1, landx FROM t005t INTO CORRESPONDING FIELDS OF TABLE @bu_country WHERE land1 IN @land1    AND
                                                                                        spras EQ @sy-langu.
  ENDMETHOD.


  METHOD get_mapping_cofu_fields.

    DATA(worker_sap) = persk.

    "for Austria employee group is relevant for workertype
    IF '03' IN molga OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-australia OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-newzealand.
      worker_sap = zmhp_cl_mig_utils=>persg.
    ENDIF.

    "get mapping fields for workertype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = zmhp_cl_mig_utils=>workertype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persg_wt ).

    "get mapping fields for locationcode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>btrtl
                                                     oracle_field = zmhp_cl_mig_utils=>locationcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_btrtl ).

    worker_sap = COND #( WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-newzealand THEN persk
                         ELSE worker_sap ).

    "get mapping fields for assignmenttype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = assignmenttype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persg_at ).



    "get mapping fields for actioncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_massn ).




    "get mapping fields for reasoncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massg
                                                     oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_massg ).


    "get mapping fields for AssignmentStatusTypeCode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = stat2
                                                     oracle_field = assignmentstatustypecode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_stat2 ).

    DATA(worker_cat) = persk.

    DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-germany )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-australia )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-newzealand ) ).
    "in case of Germany pass ABKRS
    IF sy-mandt IN abkrs_clients .
      worker_cat = 'ABKRS'.
    ENDIF.

    "get mapping fields for workercategory
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_cat
                                                     oracle_field = workercategory
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_persk_wc ).

    "get mapping fields for assignmentcategory
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentcategory
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_ac ).

    "get mapping fields for assignmentpersontype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentpersontype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_apt ).

    "get mapping fields for persontypecode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = persontypecode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_ptc ).

    "get mapping fields for systempersontype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = systempersontype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_spt ).

    "get mapping fields for legal employer
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_bukrs ).

*DEBIEAKSA auskommentiert, da Business Unit bei Homag immer WMS ist
*    "get mapping fields for business unit short
*    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
*                                                     infty        = zmhp_cl_mig_utils=>it0001
*                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
*                                                     oracle_field = zmhp_cl_mig_utils=>businessunitshortcode
*                                                     export       = abap_true
*                                           IMPORTING mapping_fields = mapping_fields_bukrs_bu ).



    "get mapping fields for normal hours
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = normal_hours
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_hours ).

    "get mapping fields for departmentname
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>orgeh
                                                     oracle_field = zmhp_cl_mig_utils=>departmentname
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_department ).

    "get mapping fields for jobcode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = 'STELL'
                                                     oracle_field = 'JOBCODE'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_job ).

    "get mapping fields for PRBEH
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0016'
                                                     sap_field    = 'PRBEH'
                                                     oracle_field = 'PROBATIONUNIT'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_prbeh ).

    "get mapping fields for KDGF2
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0016'
                                                     sap_field    = 'KDGF2'
                                                     oracle_field = 'NOTICEPERIODUOM'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_kdgf2 ).

    "get mapping fields for PRBEH
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = '0016'
                                                     sap_field    = 'CTTYP'
                                                     oracle_field = 'CONTRACTTYPE'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_cttyp ).

    "get mapping fields HFMCODE
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0033
                                                     sap_field    = 'AUS01'
                                                     oracle_field = zmhp_cl_mig_utils=>hfmcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_hfm ).

  ENDMETHOD.


  METHOD get_mapping_cofu_values.

    DATA(worker_sap) = persk.

    "for Austria employee group is relevant for workertype
    IF '03' IN molga OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-australia OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-newzealand.
      worker_sap = zmhp_cl_mig_utils=>persg.
    ENDIF.

    "get mapping values for workertype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = zmhp_cl_mig_utils=>workertype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persg_wt ).

    "get mapping values for locationcode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>btrtl
                                                     oracle_field = zmhp_cl_mig_utils=>locationcode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_btrtl ).

    worker_sap = COND #( WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-newzealand THEN persk
                         ELSE worker_sap ).



   " get mapping values for assignmenttype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = assignmenttype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persg_at ).


"    get mapping values for actioncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                          IMPORTING mapping_values = mapping_values_massn ).




    "get mapping values for reasoncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massg
                                                     oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_massg ).

    "get mapping values for AssignmentStatusTypeCode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = stat2
                                                     oracle_field = assignmentstatustypecode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_stat2 ).

    DATA(worker_cat) = persk.

    DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-germany )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-australia )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-newzealand ) ).
    "in case of Germany pass ABKRS
    IF sy-mandt IN abkrs_clients .
      worker_cat = 'ABKRS'.
    ENDIF.

    "get mapping fields for workercategory
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_cat
                                                     oracle_field = workercategory
                                                     export       = abap_true
                                            IMPORTING mapping_values = mapping_values_persk_wc ).

    "get mapping values for assignmentcategory
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentcategory
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_ac ).

    "get mapping fields for assignmentpersontype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentpersontype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_apt ).

    "get mapping values for persontypecode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = persontypecode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_ptc ).

    "get mapping values for systempersontype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = systempersontype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_spt ).

    "get mapping values for legal employer
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                     export       = abap_true
                                            IMPORTING mapping_values = mapping_values_bukrs ).

*DEBIEAKSA auskommentiert, da BusinessUnit immer WMS ist
*    "get mapping values for business unit short
*    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
*                                                     infty        = zmhp_cl_mig_utils=>it0001
*                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
*                                                     oracle_field = zmhp_cl_mig_utils=>businessunitshortcode
*                                                     export       = abap_true
*                                           IMPORTING mapping_values = mapping_values_bukrs_bu ).



    "get mapping values for normal hours
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = normal_hours
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_hours ).

    "get mapping values for departmentname
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>orgeh
                                                     oracle_field = zmhp_cl_mig_utils=>departmentname
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_department ).

    "get mapping values for jobcode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = 'STELL'
                                                     oracle_field = 'JOBCODE'
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_job ).

    "get mapping values for PRBEH
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = '0016'
                                                     sap_field    = 'PRBEH'
                                                     oracle_field = 'PROBATIONUNIT'
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_prbeh ).

    "get mapping values for KDGF2
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = '0016'
                                                     sap_field    = 'KDGF2'
                                                     oracle_field = 'NOTICEPERIODUOM'
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_kdgf2 ).

    "get mapping fields for PRBEH
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = '0016'
                                                     sap_field    = 'CTTYP'
                                                     oracle_field = 'CONTRACTTYPE'
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_cttyp ).

        "get mapping fields for HFMCODE
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0033
                                                     sap_field    = 'AUS01'
                                                     oracle_field = zmhp_cl_mig_utils=>hfmcode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_hfm ).
  ENDMETHOD.


  METHOD get_mapping_cogu_fields.

    DATA(worker_sap) = persk.

    "for Austria employee group is relevant for workertype
    IF '03' IN molga OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-australia OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-newzealand.
      worker_sap = zmhp_cl_mig_utils=>persg.
    ENDIF.

    "get mapping fields for workertype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = zmhp_cl_mig_utils=>workertype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persg_wt ).

    "get mapping fields for locationcode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>btrtl
                                                     oracle_field = zmhp_cl_mig_utils=>locationcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_btrtl ).

    worker_sap = COND #( WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-newzealand THEN persk
                         ELSE worker_sap ).


    "get mapping fields for assignmenttype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = assignmenttype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persg_at ).


    "get mapping fields for actioncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_massn ).


    "get mapping fields for reasoncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massg
                                                     oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_massg ).

    "get mapping fields for AssignmentStatusTypeCode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = stat2
                                                     oracle_field = assignmentstatustypecode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_stat2 ).

    DATA(worker_cat) = persk.

    DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-germany )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-australia )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-newzealand ) ).
    "in case of Germany pass ABKRS
    IF sy-mandt IN abkrs_clients .
      worker_cat = 'ABKRS'.
    ENDIF.

    "get mapping fields for workercategory
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_cat
                                                     oracle_field = workercategory
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_persk_wc ).

    "get mapping fields for assignmentcategory
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentcategory
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_ac ).

    "get mapping fields for assignmentpersontype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentpersontype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_apt ).

    "get mapping fields for persontypecode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = persontypecode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_ptc ).

    "get mapping fields for systempersontype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = systempersontype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_spt ).

    "get mapping fields for legal employer
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_bukrs ).

    "get mapping fields for business unit short
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>businessunitshortcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs_bu ).

    "get mapping fields for normal hours
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = normal_hours
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_hours ).

    "get mapping fields for departmentname
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>orgeh
                                                     oracle_field = zmhp_cl_mig_utils=>departmentname
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_department ).

    "get mapping fields for jobcode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = 'STELL'
                                                     oracle_field = 'JOBCODE'
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_job ).
  ENDMETHOD.


  METHOD get_mapping_cogu_values.

    DATA(worker_sap) = persk.

    "for Austria employee group is relevant for workertype
    IF '03' IN molga OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-australia OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-newzealand.
      worker_sap = zmhp_cl_mig_utils=>persg.
    ENDIF.

    "get mapping values for workertype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = zmhp_cl_mig_utils=>workertype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persg_wt ).

    "get mapping values for locationcode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>btrtl
                                                     oracle_field = zmhp_cl_mig_utils=>locationcode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_btrtl ).

    worker_sap = COND #( WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-newzealand THEN persk
                         ELSE worker_sap ).


    "get mapping values for assignmenttype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_sap
                                                     oracle_field = assignmenttype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persg_at ).



    "get mapping values for actioncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                          IMPORTING mapping_values = mapping_values_massn ).


    "get mapping values for reasoncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massg
                                                     oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_massg ).

    "get mapping values for AssignmentStatusTypeCode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = stat2
                                                     oracle_field = assignmentstatustypecode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_stat2 ).

    DATA(worker_cat) = persk.

    DATA(abkrs_clients) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-germany )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-australia )
                                              ( sign = 'I' option = 'EQ' low = zmhp_cl_int_constants=>cofu_mandant-newzealand ) ).
    "in case of Germany pass ABKRS
    IF sy-mandt IN abkrs_clients .
      worker_cat = 'ABKRS'.
    ENDIF.

    "get mapping fields for workercategory
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = worker_cat
                                                     oracle_field = workercategory
                                                     export       = abap_true
                                            IMPORTING mapping_values = mapping_values_persk_wc ).

    "get mapping values for assignmentcategory
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentcategory
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_ac ).

    "get mapping fields for assignmentpersontype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentpersontype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_apt ).

    "get mapping values for persontypecode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = persontypecode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_ptc ).

    "get mapping values for systempersontype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = systempersontype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_spt ).

    "get mapping values for legal employer
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                     export       = abap_true
                                            IMPORTING mapping_values = mapping_values_bukrs ).

    "get mapping values for business unit short
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>businessunitshortcode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_bukrs_bu ).

    "get mapping values for normal hours
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = normal_hours
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_hours ).

    "get mapping values for departmentname
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>orgeh
                                                     oracle_field = zmhp_cl_mig_utils=>departmentname
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_department ).

    "get mapping values for jobcode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = 'STELL'
                                                     oracle_field = 'JOBCODE'
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_job ).
  ENDMETHOD.


  METHOD get_mapping_fields.

    "get mapping fields for workertype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>persg
                                                     oracle_field = zmhp_cl_mig_utils=>workertype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persg_wt ).

    "get mapping fields for locationcode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>btrtl
                                                     oracle_field = zmhp_cl_mig_utils=>locationcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_btrtl ).

    "get mapping fields for assignmenttype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>persg
                                                     oracle_field = assignmenttype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persg_at ).


    "get mapping fields for actioncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                          IMPORTING mapping_fields = mapping_fields_massn ).


    "get mapping fields for reasoncode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massg
                                                     oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_massg ).

    "get mapping fields for AssignmentStatusTypeCode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = stat2
                                                     oracle_field = assignmentstatustypecode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_stat2 ).

    "get mapping fields for workercategory
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = workercategory
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_persk_wc ).

    "get mapping fields for assignmentcategory
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentcategory
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_ac ).

    "get mapping fields for persontypecode
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentpersontype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_ptc ).

    "get mapping fields for systempersontype
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = systempersontype
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_persk_spt ).

    "get mapping fields for legal employer
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_bukrs ).

    "get mapping fields for business unit short
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>businessunitshortcode
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_bukrs_bu ).

    "get mapping fields for normal hours
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = normal_hours
                                                     export       = abap_true
                                            IMPORTING mapping_fields = mapping_fields_hours ).

    "get mapping fields for departmentname
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>orgeh
                                                     oracle_field = zmhp_cl_mig_utils=>departmentname
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_department ).
  ENDMETHOD.


  METHOD get_mapping_values.

    "get mapping values for workertype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>persg
                                                     oracle_field = zmhp_cl_mig_utils=>workertype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persg_wt ).

    "get mapping values for locationcode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>btrtl
                                                     oracle_field = zmhp_cl_mig_utils=>locationcode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_btrtl ).

    "get mapping values for assignmenttype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>persg
                                                     oracle_field = assignmenttype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persg_at ).


    "get mapping values for actioncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massn
                                                     oracle_field = zmhp_cl_mig_utils=>actioncode
                                                     export       = abap_true
                                          IMPORTING mapping_values = mapping_values_massn ).


    "get mapping values for reasoncode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = zmhp_cl_mig_utils=>massg
                                                     oracle_field = zmhp_cl_mig_utils=>reasoncode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_massg ).

    "get mapping values for AssignmentStatusTypeCode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0000
                                                     sap_field    = stat2
                                                     oracle_field = assignmentstatustypecode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_stat2 ).

    "get mapping values for workercategory
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = workercategory
                                                     export       = abap_true
                                            IMPORTING mapping_values = mapping_values_persk_wc ).

    "get mapping values for assignmentcategory
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentcategory
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_ac ).

    "get mapping values for persontypecode
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = assignmentpersontype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_ptc ).

    "get mapping values for systempersontype
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = persk
                                                     oracle_field = systempersontype
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_persk_spt ).

    "get mapping values for legal employer
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>legalemployername
                                                     export       = abap_true
                                            IMPORTING mapping_values = mapping_values_bukrs ).

    "get mapping values for business unit short
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = zmhp_cl_mig_utils=>businessunitshortcode
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_bukrs_bu ).

    "get mapping values for normal hours
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>bukrs
                                                     oracle_field = normal_hours
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_hours ).

    "get mapping values for departmentname
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0001
                                                     sap_field    = zmhp_cl_mig_utils=>orgeh
                                                     oracle_field = zmhp_cl_mig_utils=>departmentname
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_department ).
  ENDMETHOD.


  METHOD map_cofu_data.
    DATA: plvar                TYPE plvar,
          pernr_tmp            TYPE pernr_d,
          pernr_old            TYPE pernr_d,
          massn_tmp            TYPE massn,
          dateprob_end         TYPE string,
          rpt_est              TYPE string,
          grade_code           TYPE string,
          notice_per           TYPE string,
          pos_code             TYPE string,
          dep_code             TYPE string,
          job_short            TYPE string,
          prob_period          TYPE string,
          normalhours          TYPE string,
          internal_number      TYPE string,
          grade_id             TYPE string,
          company_car          TYPE string,
          home_distance        TYPE string,
          cofu_notice_per      TYPE string,
          cofu_notice_per_dis  TYPE string,
          defaultexpense       TYPE string,
          contracttypeenddate  TYPE string,
          collective_agreement TYPE string,
          flex_field           TYPE string,
          dulegalentity        TYPE string,
          ducostcenter         TYPE string,
          duassgncontrarea     TYPE string,
          klammer1(1)          TYPE c,
          klammer2(1)          TYPE c,
          business_code1       TYPE zdivision,
          parttime             TYPE string,
          austx                TYPE t543d-austx.


    DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                         WHEN zmhp_cl_int_constants=>mandant-durr     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '21' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '22' )
                                                                                                            ( sign = 'I' option = 'EQ' low = '27' ) )
                                         WHEN zmhp_cl_int_constants=>mandant-homag      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                            ( sign = 'I' option = 'EQ' low = 'Z3' )
                                                                                                            ( sign = 'I' option = 'EQ' low = 'Z4' ) ) ).

    "get active plvar
    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).

      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        EXIT.
      ENDLOOP.

      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        dep_code = <p0001>-orgeh.
        pos_code = <p0001>-plans.
        ducostcenter = <p0001>-kostl.
        dulegalentity = <p0001>-bukrs.

        DATA(area) = <p0001>-zz_bu.
        DATA(subarea) = <p0001>-zz_lcs.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination in future
      CHECK <p0000>-massn NOT IN massn_term.

      "check for terminations right after actual record
      DATA(datum) = CONV datum( <period>-endda + 1 ).
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                               endda GE datum         AND
                                                               pernr EQ pernr         AND
                                                               massn IN massn_term.
        <period>-endda = <p0000_term>-endda.
      ENDLOOP.

      "get P0105
      LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        internal_number = <p0105>-usrid.
        EXIT.
      ENDLOOP.

      "get P0006
      LOOP AT p0006 ASSIGNING FIELD-SYMBOL(<p0006>) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        DATA(entkm) = SWITCH anzhl( sy-mandt
*                                    WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN <p0006>-zzentkm
                                    WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN <p0006>-entk2
                                    ELSE <p0006>-entkm ).

        home_distance = round( val = entkm
                               dec = 0 ).

        CONDENSE home_distance.
        EXIT.
      ENDLOOP.

      "get P0007
      LOOP AT p0007 ASSIGNING FIELD-SYMBOL(<p0007>) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        normalhours = <p0007>-wostd.
        parttime = COND #( WHEN <p0007>-teilk EQ 'X' THEN 'Part time'
              ELSE 'Full time' ).

        CONDENSE normalhours.
        EXIT.
      ENDLOOP.

      LOOP AT p0016 INTO DATA(p0016_entry) WHERE begda LE <period>-endda AND
                                                 endda GE <period>-begda AND
                                                 pernr EQ pernr.
*        cofu_notice_per = cofu_notice_per_dis = notice_per = prob_period = p0016_entry-prbzt.
*        CONDENSE: cofu_notice_per, cofu_notice_per_dis, notice_per, prob_period.

        IF p0016_entry-ctedt IS NOT INITIAL.
          contracttypeenddate = zmhp_cl_mig_utils=>convert_date( p0016_entry-ctedt ).
        ENDIF.
        EXIT.
      ENDLOOP.

      CLEAR job_short.
      "get job text
      LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-stell AND
                                                                    begda LE <period>-endda AND
                                                                    endda GE <period>-begda.
        job_short = ''.
        EXIT.
      ENDLOOP.

      "get P0008
      LOOP AT p0008 ASSIGNING FIELD-SYMBOL(<p0008>) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        collective_agreement = <p0008>-trfar && '_' && <p0008>-trfgb.
        grade_code = <p0008>-trfgr.

        grade_code = SWITCH #( <p0008>-trfst
                             WHEN '95' OR '10' THEN grade_code && '/' && <p0008>-trfst
                             ELSE grade_code ).

        CONDENSE: collective_agreement, grade_code.
        EXIT.
      ENDLOOP.

*DEBIE2HAVE 20220509 Ausgabe HFM Code
      LOOP AT p0033 INTO DATA(p0033) WHERE begda LE <period>-endda AND
                                                          endda GE <period>-begda AND
                                                          pernr EQ pernr.
        EXIT.
      ENDLOOP.
*      IF <p0033> IS ASSIGNED.
*        SELECT SINGLE austx FROM t543d INTO austx
*                WHERE subty = <p0033>-subty
*                AND staus = <p0033>-aus01.
*        CONCATENATE '45' <p0033>-aus01 ' ' austx INTO DATA(hfm_code) RESPECTING BLANKS.
*        CONDENSE hfm_code.
*      ENDIF.

      map_mig_cofu_values( EXPORTING p0001 = <p0001>
                                     p0000 = <p0000>
                                     p0016 = p0016_entry
                                     p0033 = p0033
                                     job   = job_short
                           IMPORTING massn               = DATA(massn)
                                     massg               = DATA(massg)
                                     worker_type         = DATA(worker_type)
                                     assignment_type     = DATA(assignment_type)
                                     person_type_code    = DATA(person_type_code)
                                     system_person_type  = DATA(system_person_type)
                                     assign_status       = DATA(assign_status)
                                     worker_category     = DATA(worker_category)
                                     assignment_person_type_code = DATA(assignment_person_type_code)
                                     assignment_category = DATA(assignment_category)
                                     location_code       = DATA(location_code)
                                     legal_employer_name = DATA(legal_employer_name)
*                                     business_unit_code  = DATA(business_code)
                                     department_id     = DATA(department_id)
                                     job_code            = DATA(job)
                                     noticeuom           = DATA(notice_per_uom)
                                     probationunit       = DATA(prob_unit)
                                     contracttype        = DATA(contracttype)
                                     hfmcode             = DATA(hfm_code)
                                     normalhours         = DATA(bukrs_hours) ). "JMB20210928 I - C400129651-5642

      business_code1 = 'WMS'.
      klammer1 = '('.
      klammer2 = ')'.

      CONCATENATE klammer1 business_code1 klammer2 INTO DATA(business_code2).
      CONCATENATE legal_employer_name business_code2 INTO DATA(business_code) SEPARATED BY space.


**JMB20210928 start insert - in case no IT0007 was maintained retrieve hours from CompanyCode (C400129651-5642)
*
      normalhours = COND #( WHEN normalhours IS INITIAL THEN bukrs_hours
                            ELSE normalhours ).
*JMB20210928 insert end

      "check hire entry for employee
      IF  pernr         IN pernr_history AND
          pernr_history IS NOT INITIAL.
        "actioncode for these entries will be ASC_CHANGE
        massn = 'ASG_CHANGE'.
      ELSE.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = pernr ) TO pernr_history.
      ENDIF.

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <period>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( COND #( WHEN <p0000>-massn IN massn_term AND
                                                                      <p0000>-stat2 NE '1'  "JMB20210911 I
                                                                 THEN zmhp_cl_mig_utils=>oracle_hd
                                                                 ELSE <period>-endda ) ).
      CONCATENATE assign pernr INTO DATA(src_id).
*DEBIEAKSA 20220504 auskommentiert, da Oracle diese Nummer automatisch generiert
*      CONCATENATE e pernr  INTO DATA(assign_num).

      IF sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-germany.
        CONCATENATE 'POS' '_' <p0001>-plans INTO pos_code.
      ENDIF.

      pernr_tmp = pernr.
      massn_tmp = <p0000>-massn.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(is_manager) = zmhp_cl_mig_utils=>is_manager( <p0001> ).

      "Get relevant work relationship
      DATA(period_of_ser) = 'WR_NOT_FOUND'.
      LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                  begda LE <p0001>-endda AND
                                                                  endda GE <p0001>-begda.
        period_of_ser = <vp_wkr_id>-wkr_id.
        EXIT.
      ENDLOOP.

      "Get relevant work terms
      DATA(workterm) = 'WT_NOT_FOUND'.
      LOOP AT vp_wterm_id ASSIGNING FIELD-SYMBOL(<vp_wterm_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                      begda LE sy-datum AND
                                                                      endda GE <p0001>-endda.
        workterm = <vp_wterm_id>-wterm_id.
        EXIT.
      ENDLOOP.


      DATA(default) = 'WT_'.

      IF workterm = 'WT_NOT_FOUND' OR workterm = 'WR_NOT_FOUND'.
        CONCATENATE default <p0001>-pernr INTO workterm.
      ENDIF.

      DATA(frequen) = weekly.
      IF normalhours IS INITIAL.
        CLEAR: frequen.
      ENDIF.

      DATA(expense_account) = SWITCH string( <p0001>-bukrs
                                             WHEN '9500' THEN '629501'
                                             WHEN '4000' THEN '635141'
                                             WHEN '9000' THEN '685400' ).

      defaultexpense = <p0001>-bukrs && '.' && <p0001>-bukrs && '-' && <p0001>-kostl && '.' && expense_account.

      "set country for flex fields
      flex_field = SWITCH #( sy-mandt
                             WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN 'NL'
                             WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN 'DE'
                             WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN 'FR' ).

      IF '15' IN molga.
        flex_field = 'IT'.
      ELSEIF '03' IN molga.
        flex_field = 'AT'.
      ENDIF.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment
                  sys_id
                  src_id
                  massn
                  begda_tmp
                  endda_tmp
                  '1'
                  zmhp_cl_mig_utils=>yes
                  workterm
                  assignment_type
                  '.'   "JMB20210714 I - Pass as default a point to AssignmentName
                  '' "assign_num
                  assign_status
                  business_code "business_code
                  '' "worker_category
                  assignment_category
                  job
                  location_code
                  is_manager
                  dep_code            "department_id
                  period_of_ser
                  src_sys_id
                  begda_tmp
                  worker_type
                  legal_employer_name
                  person_type_code
                  system_person_type
                  zmhp_cl_mig_utils=>yes
                  zmhp_cl_mig_utils=>yes
                  massg
                  internal_number
                  normalhours
                  frequen
                  dateprob_end
                  rpt_est
                  '' "grade_code - DEBIEAKSA: not relevant for Duerr
                  pos_code
                  notice_per
                  prob_unit
                  grade_id
*                  company_car
*                  home_distance
                  notice_per_uom "cofu_notice_per
*                  contracttype
*                  contracttypeenddate
                  dulegalentity
                  ducostcenter
                  duassgncontrarea
                  '' "collective_agreement - DEBIEAKSA: is not relevant for DUERR
*                  defaultexpense
                  parttime
                  'Global Data Elements'
*                  flex_field
                  area
                  subarea
                  hfm_code
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: internal_number, job, department_id, normalhours, prob_period, pos_code, grade_id,
             p0016_entry,
             company_car,
             home_distance,
             cofu_notice_per,
             cofu_notice_per_dis,
             collective_agreement.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_cogl_data.

    DATA: plvar           TYPE plvar,
          pernr_tmp       TYPE pernr_d,
          pernr_old       TYPE pernr_d,
          massn_tmp       TYPE massn,
          job             TYPE string,
          internal_number TYPE string,
          klammer1(1)     TYPE c,
          klammer2(1)     TYPE c,
          business_code1  TYPE zdivision.


    DATA(massn_term) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '03' ) ).

    "get active plvar
    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).


      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination in future
      CHECK <p0000>-massn NOT IN massn_term.

      "check for terminations right after actual record
      DATA(datum) = CONV datum( <p0001>-endda + 1 ).
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                               endda GE datum         AND
                                                               pernr EQ <p0001>-pernr AND
                                                               massn IN massn_term.
        <p0001>-endda = <p0000_term>-endda.
      ENDLOOP.

      "get P0105
      LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        internal_number = <p0105>-usrid.
        EXIT.
      ENDLOOP.

      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr AND
                                                                 begda LE sy-datum      AND
                                                                 endda GE sy-datum.
        "pass only actual worker/assignment type due to worker type in work relationship
        <p0001>-persg = <p0001_actual>-persg.
        <p0001>-persk = <p0001_actual>-persk.

        EXIT.
      ENDLOOP.


*DEBIEAKSA hinzugef??gt, da Assignmenttype ??ber IT16 gemapped werden soll
      "get relevant P0000
      LOOP AT p0016 ASSIGNING FIELD-SYMBOL(<p0016>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.


      map_mig_values( EXPORTING p0001 = <p0001>
                                p0000 = <p0000>
                                p0016 = <p0016>
                      IMPORTING massn               = DATA(massn)
                                massg               = DATA(massg)
                                worker_type         = DATA(worker_type)
                                assignment_type     = DATA(assignment_type)
                                person_type_code    = DATA(person_type_code)
                                system_person_type  = DATA(system_person_type)
                                assign_status       = DATA(assign_status)
                                worker_category     = DATA(worker_category)
                                assignment_category = DATA(assignment_category)
                                location_code       = DATA(location_code)
                                legal_employer_name = DATA(legal_employer_name)
*                                business_unit_code  = DATA(business_code)
                                department_name     = DATA(department_name)
                                normalhours         = DATA(normalhours) ).


      business_code1 = 'WMS'.
      klammer1 = '('.
      klammer2 = ')'.

      CONCATENATE klammer1 business_code1 klammer2 INTO DATA(business_code2).
      CONCATENATE legal_employer_name business_code2 INTO DATA(business_code) SEPARATED BY space.


      "check hire entry for employee
      IF <p0001>-pernr  IN pernr_history AND
          pernr_history IS NOT INITIAL.
        "actioncode for these entries will be ASC_CHANGE
        massn = 'ASG_CHANGE'.
      ELSE.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
      ENDIF.

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( COND #( WHEN <p0000>-massn IN massn_term
                                                                 THEN zmhp_cl_mig_utils=>oracle_hd
                                                                 ELSE <p0001>-endda ) ).
      CONCATENATE assign <p0001>-pernr INTO DATA(src_id).
      CONCATENATE e <p0001>-pernr  INTO DATA(assign_num).

      IF massn_tmp     IN     massn_term    AND
         <p0000>-massn NOT IN massn_term    AND
         pernr_tmp     EQ     <p0001>-pernr.
        CONCATENATE assign_num '_' massn INTO assign_num.
        CONCATENATE src_id     '_' massn INTO src_id.
      ENDIF.

      pernr_tmp = <p0001>-pernr.
      massn_tmp = <p0000>-massn.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(is_manager) = zmhp_cl_mig_utils=>is_manager( <p0001> ).

      IF business_code NE zmhp_cl_mig_utils=>default_business_unit.
*      job = 'MG_SEW_0001'.   "JMB20210720 D - Not needed
        "get job text
        LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-stell AND
                                                                      begda LE <p0001>-endda AND
                                                                      endda GE <p0001>-begda.
          job = ''.
          EXIT.
        ENDLOOP.
      ENDIF.

      "Get relevant work relationship
      DATA(period_of_ser) = 'WR_NOT_FOUND'.
      LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                  begda LE <p0001>-endda AND
                                                                  endda GE <p0001>-begda.
        period_of_ser = <vp_wkr_id>-wkr_id.
        EXIT.
      ENDLOOP.

      "Get relevant work terms
      DATA(workterm) = 'WT_NOT_FOUND'.
      LOOP AT vp_wterm_id ASSIGNING FIELD-SYMBOL(<vp_wterm_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                      begda LE <p0001>-endda AND
                                                                      endda GE <p0001>-begda.
        workterm = <vp_wterm_id>-wterm_id.
        EXIT.
      ENDLOOP.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment
                  sys_id
                  src_id
                  massn
                  begda_tmp
                  endda_tmp
                  '1'
                  zmhp_cl_mig_utils=>yes
                  workterm
                  assignment_type
                  '.'   "JMB20210714 I - Pass as default a point to AssignmentName
                  assign_num
                  assign_status
                  business_code
                  worker_category
                  '' "assignment_category "JMB20210408 D - Only pass space as AssignmentCategory
                  job
                  location_code
                  is_manager
                  department_name
                  period_of_ser
                  src_sys_id
                  begda_tmp
                  worker_type
                  legal_employer_name
                  person_type_code
                  system_person_type
                  zmhp_cl_mig_utils=>yes
                  zmhp_cl_mig_utils=>yes
                  massg
                  internal_number
                  normalhours
                  weekly
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: internal_number, job, department_name.

**JMB20210722 start insert - create work measure entity
*
      CHECK <p0001>-pernr NE pernr_old.
      zmhp_cl_mig_work_measure=>map_cogl_data( EXPORTING action               = CONV #( massn )
                                                         assignment_source_id = src_id
                                                         begda                = begda_tmp
                                                         source_system_owner  = sys_id
                                                CHANGING work_measure         = work_measure ).
      pernr_old = <p0001>-pernr.
*JMB20210722 insert end


    ENDLOOP.
  ENDMETHOD.


  METHOD map_cogu_data.

    DATA: plvar           TYPE plvar,
          pernr_tmp       TYPE pernr_d,
          pernr_old       TYPE pernr_d,
          massn_tmp       TYPE massn,
          job_short       TYPE string,
          normalhours     TYPE string,
          internal_number TYPE string,
          klammer1(1)     TYPE c,
          klammer2(1)     TYPE c,
          business_code1  TYPE zdivision.


    DATA(massn_term) = SWITCH rsdsselopt_t( sy-mandt
                                          WHEN zmhp_cl_int_constants=>cofu_mandant-germany     THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '13' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '35' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '99' )
                                                                                                             ( sign = 'I' option = 'EQ' low = 'ZZ' ) )
                                          WHEN zmhp_cl_int_constants=>cofu_mandant-france      THEN VALUE #( ( sign = 'I' option = 'EQ' low = '09' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '11' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '18' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '32' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '34' ) )
                                          WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands THEN VALUE #( ( sign = 'I' option = 'EQ' low = 'Z4' ) )
                                          WHEN zmhp_cl_int_constants=>cofu_mandant-australia   THEN VALUE #( ( sign = 'I' option = 'EQ' low = '10' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '20' )
                                                                                                             ( sign = 'I' option = 'EQ' low = '25' ) )
                                          WHEN zmhp_cl_int_constants=>cofu_mandant-newzealand  THEN VALUE #( ( sign = 'I' option = 'EQ' low = '25' ) )
                                          "default Italy/Austria
                                          ELSE VALUE #( ( sign = 'I' option = 'EQ' low = '03' )
                                                        ( sign = 'I' option = 'EQ' low = 'ZZ' ) ) ).

    "get active plvar
    CALL FUNCTION 'RH_GET_PLVAR'
      IMPORTING
        plvar = plvar.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO DATA(sys_id).

    LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>).

      "get relevant P0000
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.


      LOOP AT p0016 ASSIGNING FIELD-SYMBOL(<p0016>) WHERE begda LE <p0001>-endda AND
                                                    endda GE <p0001>-begda AND
                                                    pernr EQ <p0001>-pernr.
        EXIT.
      ENDLOOP.

      CHECK sy-subrc EQ 0.

      "in case of termination in future
      CHECK <p0000>-massn NOT IN massn_term.

      "check for terminations right after actual record
      DATA(datum) = CONV datum( <p0001>-endda + 1 ).
      LOOP AT p0000 ASSIGNING FIELD-SYMBOL(<p0000_term>) WHERE begda LE datum         AND
                                                               endda GE datum         AND
                                                               pernr EQ <p0001>-pernr AND
                                                               massn IN massn_term.
        <p0001>-endda = <p0000_term>-endda.
      ENDLOOP.

      "get P0105
      LOOP AT p0105 ASSIGNING FIELD-SYMBOL(<p0105>) WHERE begda LE <p0001>-endda AND
                                                          endda GE <p0001>-begda AND
                                                          pernr EQ <p0001>-pernr.
        internal_number = <p0105>-usrid.
        EXIT.
      ENDLOOP.

      "get P0007
      LOOP AT p0007 ASSIGNING FIELD-SYMBOL(<p0007>) WHERE begda LE sy-datum AND "JMB20210910 I
                                                          endda GE sy-datum AND "JMB20210910 I
                                                          pernr EQ <p0001>-pernr.
        normalhours = <p0007>-wostd.
        CONDENSE normalhours.
        EXIT.
      ENDLOOP.

      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001_actual>) WHERE pernr EQ <p0001>-pernr AND
                                                                 begda LE sy-datum      AND
                                                                 endda GE sy-datum.

        "Decision 06.08.2021: pass only actual legal employer due to import problems in Oracle
        <p0001>-bukrs = <p0001_actual>-bukrs. "JMB20210806 I

        "pass only actual worker/assignment type due to worker type in work relationship
        <p0001>-persg = <p0001_actual>-persg.
        <p0001>-persk = <p0001_actual>-persk.

        DATA(area) = <p0001_actual>-zz_bu.
        DATA(subarea) = <p0001_actual>-zz_lcs.
        EXIT.
      ENDLOOP.

      CLEAR job_short.
**JMB20210811 start delete - JobCode not needed for CoGu
*
*    "get job text
*    LOOP AT hrp1000_stell ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE objid EQ <p0001>-stell AND
*                                                                  begda LE <p0001>-endda AND
*                                                                  endda GE <p0001>-begda.
*      job_short = <hrp1000>-short.
*      EXIT.
*    ENDLOOP.
*JMB20210811 end delete

      map_mig_cogu_values( EXPORTING p0001 = <p0001>
                                     p0000 = <p0000>
                                     p0016 = <p0016>
                                     job   = job_short
                           IMPORTING massn               = DATA(massn)
                                     massg               = DATA(massg)
                                     worker_type         = DATA(worker_type)
                                     assignment_type     = DATA(assignment_type)
                                     person_type_code    = DATA(person_type_code)
                                     system_person_type  = DATA(system_person_type)
                                     assign_status       = DATA(assign_status)
                                     worker_category     = DATA(worker_category)
                                     assignment_person_type_code = DATA(assignment_person_type_code)
                                     assignment_category = DATA(assignment_category)
                                     location_code       = DATA(location_code)
                                     legal_employer_name = DATA(legal_employer_name)
*                                     business_unit_code  = DATA(business_code)
                                     department_name     = DATA(department_name)
                                     job_code            = DATA(job)
                                     normalhours         = DATA(bukrs_hours) ). "JMB20210928 I - C400129651-5642

      business_code1 = 'WMS'.
      klammer1 = '('.
      klammer2 = ')'.

      CONCATENATE klammer1 business_code1 klammer2 INTO DATA(business_code2).
      CONCATENATE legal_employer_name business_code2 INTO DATA(business_code) SEPARATED BY space.


**JMB20210928 start insert - in case no IT0007 was maintained retrieve hours from CompanyCode (C400129651-5642)
*
      normalhours = COND #( WHEN normalhours IS INITIAL THEN bukrs_hours
                            ELSE normalhours ).
*JMB20210928 insert end

      "check hire entry for employee
      IF <p0001>-pernr  IN pernr_history AND
          pernr_history IS NOT INITIAL.
        "actioncode for these entries will be ASC_CHANGE
        massn = 'ASG_CHANGE'.
      ELSE.
        APPEND VALUE #( sign = 'I' option = 'EQ' low = <p0001>-pernr ) TO pernr_history.
      ENDIF.

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <p0001>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( COND #( WHEN <p0000>-massn IN massn_term AND
                                                                      <p0000>-stat2 NE '1'  "JMB20210911 I
                                                                 THEN zmhp_cl_mig_utils=>oracle_hd
                                                                 ELSE <p0001>-endda ) ).
      CONCATENATE assign <p0001>-pernr INTO DATA(src_id).
      CONCATENATE e <p0001>-pernr  INTO DATA(assign_num).

**JMB20210911 start delete
*
*    IF massn_tmp     IN     massn_term    AND
*       <p0000>-massn NOT IN massn_term    AND
*       pernr_tmp     EQ     <p0001>-pernr.
*      CONCATENATE assign_num '_' massn INTO assign_num.
*      CONCATENATE src_id     '_' massn INTO src_id.
*    ENDIF.
*JMB20210911 end delete

      pernr_tmp = <p0001>-pernr.
      massn_tmp = <p0000>-massn.

      "get source id
      DATA(src_sys_id) = zmhp_cl_mig_utils=>get_src_id( pernr = <p0001>-pernr
                                                        begda = <p0001>-begda
                                                        endda = <p0001>-endda
                                                        vp_src_id = vp_src_id ).

      DATA(is_manager) = zmhp_cl_mig_utils=>is_manager( <p0001> ).

      "Get relevant work relationship
      DATA(period_of_ser) = 'WR_NOT_FOUND'.
      LOOP AT vp_wkr_id ASSIGNING FIELD-SYMBOL(<vp_wkr_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                  begda LE <p0001>-endda AND
                                                                  endda GE <p0001>-begda.
        period_of_ser = <vp_wkr_id>-wkr_id.
        EXIT.
      ENDLOOP.

      "Get relevant work terms
      DATA(workterm) = 'WT_NOT_FOUND'.
      LOOP AT vp_wterm_id ASSIGNING FIELD-SYMBOL(<vp_wterm_id>) WHERE pernr EQ <p0001>-pernr AND
                                                                      begda LE <p0001>-endda AND
                                                                      endda GE <p0001>-begda.
        workterm = <vp_wterm_id>-wterm_id.
        EXIT.
      ENDLOOP.

      DATA(frequen) = weekly.
      IF normalhours IS INITIAL.
        CLEAR: frequen.
      ENDIF.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assignment
                  sys_id
                  src_id
                  massn
                  begda_tmp
                  endda_tmp
                  '1'
                  zmhp_cl_mig_utils=>yes
                  workterm
                  assignment_type
                  '.'   "JMB20210714 I - Pass as default a point to AssignmentName
                  assign_num
                  assign_status
                  business_code
                  worker_category
                  assignment_category
                  job
                  location_code
                  is_manager
                  department_name
                  period_of_ser
                  src_sys_id
                  begda_tmp
                  worker_type
                  legal_employer_name
                  person_type_code
                  system_person_type
                  zmhp_cl_mig_utils=>yes
                  zmhp_cl_mig_utils=>yes
                  massg
                  internal_number
                  normalhours
                  frequen
                  area
                  subarea
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
      CLEAR: internal_number, job, department_name, normalhours.

**JMB20210722 start insert - create work measure entity
*
      CHECK <p0001>-pernr NE pernr_old.
      zmhp_cl_mig_work_measure=>map_cogl_data( EXPORTING action               = CONV #( massn )
                                                         assignment_source_id = src_id
                                                         begda                = begda_tmp
                                                         source_system_owner  = sys_id
                                                CHANGING work_measure         = work_measure ).
      pernr_old = <p0001>-pernr.
*JMB20210722 insert end

    ENDLOOP.
  ENDMETHOD.


  METHOD map_mig_cofu_values.
    DATA: value_tmp TYPE zmhp_dd_value.
    DATA(fields) = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty = zmhp_cl_mig_utils=>it0001
                                                                 field_sap = zmhp_cl_mig_utils=>btrtl
                                                                 value = p0001-btrtl )
                                                               ( infty = zmhp_cl_mig_utils=>it0001
                                                                 field_sap = zmhp_cl_mig_utils=>werks
                                                                 value = p0001-werks ) ).
    "Instantiate mapping class
    DATA(mapping_handler) = NEW zmhp_cl_int_mapping( ).

    "Process WERKS/BTRTL mapping (LocationCode)
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>btrtl
        field_oracle   = zmhp_cl_mig_utils=>locationcode
        mapping_fields = CONV #( mapping_fields_btrtl )
        fields         = fields
      CHANGING
        value          = location_code ).

    "Process MASSN mapping
    value_tmp       = CONV #( p0000-massn ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = zmhp_cl_mig_utils=>massn
        field_oracle   = zmhp_cl_mig_utils=>actioncode
        mapping_fields = CONV #( mapping_fields_massn )
        mapping_values = CONV #( mapping_values_massn )
      CHANGING
        value          = value_tmp ).

    massn = value_tmp.

    "Process MASSG mapping
    value_tmp = CONV #( p0000-massg ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = zmhp_cl_mig_utils=>massg
        field_oracle   = zmhp_cl_mig_utils=>reasoncode
        mapping_fields = CONV #( mapping_fields_massg )
        mapping_values = CONV #( mapping_values_massg )
      CHANGING
        value          = value_tmp ).

    massg = value_tmp.

    "Get molga for IT/AT
    READ TABLE molga INTO DATA(molga_country) INDEX 1.

    DATA(fields_gk) = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = zmhp_cl_mig_utils=>persg
                                                                    value     = p0001-persg )
                                                                  ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = persk
                                                                    value     = p0001-persk )
                                                                  ( infty     = zmhp_cl_mig_utils=>it0016
                                                                    field_sap = cttyp
                                                                    value     = p0016-cttyp )
                                                                  ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = 'MOLGA'
                                                                    value     = molga_country-low )
                                                                  ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = 'ANSVH'
                                                                    value     = p0001-ansvh ) ).

    DATA(worker_sap) = persk.
    value_tmp = CONV #( p0001-persk ).
    "for Austria employee group is relevant for workertype
*    IF '03'    IN molga OR
*      sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-australia  OR
*       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-newzealand.
*      worker_sap = zmhp_cl_mig_utils=>persg.
*      value_tmp = CONV #( p0001-persg ).
*    ENDIF.

    "Process workertype mapping
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = worker_sap
        field_oracle   = zmhp_cl_mig_utils=>workertype
        mapping_fields = CONV #( mapping_fields_persg_wt )
        mapping_values = CONV #( mapping_values_persg_wt )
        fields         = fields_gk
      CHANGING
        value          = value_tmp ).

*    worker_type = value_tmp.

*DEBIEAKSA: Wenn Wert leer -> Dann Default to "E"
    IF value_tmp IS NOT INITIAL.
      worker_type = value_tmp.
    ELSE.
      worker_type = 'E'.
    ENDIF.

    "Process assignmenttype mapping
    "for Austria/Australie employee group is relevant for assignmenttype
    value_tmp = COND #( WHEN '03'     IN molga                                         THEN p0001-persg
                        WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-australia THEN p0001-persg
                        ELSE p0001-persk ).

    worker_sap = COND #( WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-newzealand THEN persk
                         ELSE worker_sap ).


    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = worker_sap
        field_oracle   = assignmenttype
        mapping_fields = CONV #( mapping_fields_persg_at )
        mapping_values = CONV #( mapping_values_persg_at )
      CHANGING
        value          = value_tmp ).
*    assignment_type    = value_tmp.

*DEBIEAKSA: Wenn Wert leer -> Dann Default to "E"
    IF value_tmp IS NOT INITIAL.
      assignment_type = value_tmp.
    ELSE.
      assignment_type = 'E'.
    ENDIF.


    CLEAR: value_tmp.
    "Process workercategory mapping
*    value_tmp = SWITCH #( sy-mandt
*                          WHEN zmhp_cl_int_constants=>cofu_mandant-germany THEN p0001-abkrs
*                          WHEN zmhp_cl_int_constants=>cofu_mandant-austria THEN p0001-persk
*                          WHEN zmhp_cl_int_constants=>cofu_mandant-italy   THEN p0001-persk ).

*    worker_sap = SWITCH zmhp_dd_field( sy-mandt
*                                       WHEN zmhp_cl_int_constants=>cofu_mandant-germany    THEN 'ABKRS'
*                                       WHEN zmhp_cl_int_constants=>cofu_mandant-australia  THEN 'ABKRS'
*                                       WHEN zmhp_cl_int_constants=>cofu_mandant-newzealand THEN 'ABKRS'
*                                       WHEN zmhp_cl_int_constants=>cofu_mandant-austria    THEN persk
*                                       WHEN zmhp_cl_int_constants=>cofu_mandant-italy      THEN persk ).
    value_tmp = p0001-persk.
    worker_sap = persk.

    IF value_tmp  IS NOT INITIAL AND
       worker_sap IS NOT INITIAL.
      mapping_handler->process_mapping(
        EXPORTING
          import         = abap_false
          export         = abap_true
          infty          = zmhp_cl_mig_utils=>it0001
          field_sap      = worker_sap
          field_oracle   = workercategory
          mapping_fields = CONV #( mapping_fields_persk_wc )
          mapping_values = CONV #( mapping_values_persk_wc )
        CHANGING
          value          = value_tmp ).

      worker_category    = value_tmp.
    ENDIF.

    "Process assignmentcategory mapping
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = assignmentcategory
        mapping_fields = CONV #( mapping_fields_persk_ac )
        fields         = fields_gk
      CHANGING
        value          = assignment_category ).

    "Process assignmentpersontype mapping
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = assignmentpersontype
        mapping_fields = CONV #( mapping_fields_persk_apt )
        fields         = fields_gk
      CHANGING
        value          = assignment_person_type_code ).

    "Process hfm code mapping
    IF p0033-aus01 IS NOT INITIAL.
      value_tmp = CONV #( p0033-aus01 ).
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

      hfmcode = value_tmp.
      CLEAR value_tmp.
    ENDIF.
    "Process persontypecode mapping
    value_tmp = p0001-persk.
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = persontypecode
        mapping_fields = CONV #( mapping_fields_persk_ptc )
        mapping_values = CONV #( mapping_values_persk_ptc )
        fields         = fields_gk
      CHANGING
        value          = value_tmp ).

    person_type_code = value_tmp.

**JMB20211031 start insert - pass assignment person type instead of person type code (C400129651-5748)
*
    IF assignment_person_type_code IS NOT INITIAL.
      person_type_code = assignment_person_type_code.
    ENDIF.
*JMB20211031 insert end

    "Process systempersontype mapping
    value_tmp = CONV #( p0001-persk ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = systempersontype
        mapping_fields = CONV #( mapping_fields_persk_spt )
        mapping_values = CONV #( mapping_values_persk_spt )
        fields         = fields_gk
     CHANGING
        value          = value_tmp ).

*    system_person_type = value_tmp.

*DEBIEAKSA: Wenn Wert leer dann Default to "EMP"
    IF value_tmp IS NOT INITIAL.
      system_person_type = value_tmp.
    ELSE.
      system_person_type = 'EMP'.
    ENDIF.

    fields = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty     = zmhp_cl_mig_utils=>it0000
                                                           field_sap = stat2
                                                           value     = p0000-stat2 )
                                                         ( infty     = zmhp_cl_mig_utils=>it0001
                                                           field_sap = 'KOSTL'
                                                           value     = p0001-kostl )
                                                         ( infty     = zmhp_cl_mig_utils=>it0000
                                                           field_sap = 'STAT1'
                                                           value     = p0000-stat1 ) ).

    "Process STAT2 mapping
    value_tmp = CONV #( p0000-stat2 ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = stat2
        field_oracle   = assignmentstatustypecode
        mapping_fields = CONV #( mapping_fields_stat2 )
        mapping_values = CONV #( mapping_values_stat2 )
        fields         = fields
      CHANGING
        value          = value_tmp ).

    assign_status      = value_tmp.

    "Process BUKRS Mapping
    value_tmp = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = zmhp_cl_mig_utils=>legalemployername
        mapping_fields = CONV #( mapping_fields_bukrs )
        mapping_values = CONV #( mapping_values_bukrs )
      CHANGING
        value          = value_tmp ).

    legal_employer_name = value_tmp.



    fields = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = zmhp_cl_int_constants=>bukrs
                                                                    value = p0001-bukrs )
                                                                  ( infty = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = zmhp_cl_int_constants=>kostl
                                                                    value = p0001-kostl ) ).
    "Process WERKS/BTRTL mapping (LocationCode)
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = CONV #( zmhp_cl_int_constants=>bukrs )
        field_oracle   = zmhp_cl_mig_utils=>businessunitshortcode
        mapping_fields = CONV #( mapping_fields_bukrs_bu )
        fields         = fields
      CHANGING
        value          = business_unit_code ).



**JMB20211011 start delete - Dominik have changed name of OrgUnits on Prod, so no mapping is needed
*
*  "Process department Mapping
*  value_tmp = CONV #( p0001-orgeh ).
*  mapping_handler->process_mapping(
*    EXPORTING
*      import         = abap_false
*      export         = abap_true
*      infty          = ZMHP_cl_mig_utils=>it0001
*      field_sap      = ZMHP_cl_mig_utils=>orgeh
*      field_oracle   = ZMHP_cl_mig_utils=>departmentname
*      mapping_fields = CONV #( mapping_fields_department )
*      mapping_values = CONV #( mapping_values_department )
*    CHANGING
*      value          = value_tmp ).
*
*  department_name = value_tmp.
*JMB20211011 delete end, start insert
    LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE begda LE sy-datum AND
                                                                  endda GE sy-datum AND
                                                                  objid EQ p0001-orgeh.
      department_id = <hrp1000>-objid.
      EXIT.
    ENDLOOP.
*JMB20211011 insert end


    "Process BUKRS/NormalHours mapping
    value_tmp       = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = normal_hours
        mapping_fields = CONV #( mapping_fields_hours )
        mapping_values = CONV #( mapping_values_hours )
      CHANGING
        value          = value_tmp ).
    IF p0001-bukrs = value_tmp.
      CLEAR: value_tmp.
    ENDIF.
    normalhours = value_tmp.

    value_tmp       = CONV #( p0016-prbeh ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0016'
        field_sap      = 'PRBEH'
        field_oracle   = 'PROBATIONUNIT'
        mapping_fields = CONV #( mapping_fields_prbeh )
        mapping_values = CONV #( mapping_values_prbeh )
      CHANGING
        value          = value_tmp ).

    probationunit = value_tmp.

    value_tmp       = CONV #( p0016-kdgf2 ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0016'
        field_sap      = 'KDGF2'
        field_oracle   = 'NOTICEPERIODUOM'
        mapping_fields = CONV #( mapping_fields_kdgf2 )
        mapping_values = CONV #( mapping_values_kdgf2 )
      CHANGING
        value          = value_tmp ).

    noticeuom = value_tmp.

**JMB20210811 start insert - check jobCode mapping
*
    IF job IS NOT INITIAL.
      value_tmp       = CONV #( job ).
      mapping_handler->process_mapping(
        EXPORTING
          import         = abap_false
          export         = abap_true
          infty          = zmhp_cl_mig_utils=>it0001
          field_sap      = 'STELL'
          field_oracle   = 'JOBCODE'
          mapping_fields = CONV #( mapping_fields_job )
          mapping_values = CONV #( mapping_values_job )
        CHANGING
          value          = value_tmp ).
      job_code = value_tmp.
    ENDIF.
*JMB20210811 end insert

    value_tmp       = CONV #( p0016-cttyp ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = '0016'
        field_sap      = 'CTTYP'
        field_oracle   = 'CONTRACTTYPE'
        mapping_fields = CONV #( mapping_fields_cttyp )
        mapping_values = CONV #( mapping_values_cttyp )
      CHANGING
        value          = value_tmp ).

    contracttype = value_tmp.

    "01.03.2021 - Final decision: for history provide default value
*  CHECK p0001-endda LT sy-datum.
    CHECK hire EQ abap_true.

    business_unit_code = zmhp_cl_mig_utils=>default_business_unit.
    CLEAR: location_code, department_id, job_code. "JMB20210705 D - Don??t clear normalhours (Ticket 4999).
  ENDMETHOD.


  METHOD map_mig_cogu_values.
    DATA: value_tmp TYPE zmhp_dd_value.
    DATA(fields) = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty = zmhp_cl_mig_utils=>it0001
                                                                 field_sap = zmhp_cl_mig_utils=>btrtl
                                                                 value = p0001-btrtl )
                                                               ( infty = zmhp_cl_mig_utils=>it0001
                                                                 field_sap = zmhp_cl_mig_utils=>werks
                                                                 value = p0001-werks ) ).
    "Instantiate mapping class
    DATA(mapping_handler) = NEW zmhp_cl_int_mapping( ).

    "Process WERKS/BTRTL mapping (LocationCode)
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>btrtl
        field_oracle   = zmhp_cl_mig_utils=>locationcode
        mapping_fields = CONV #( mapping_fields_btrtl )
        fields         = fields
      CHANGING
        value          = location_code ).

    "Process MASSN mapping
    value_tmp       = CONV #( p0000-massn ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = zmhp_cl_mig_utils=>massn
        field_oracle   = zmhp_cl_mig_utils=>actioncode
        mapping_fields = CONV #( mapping_fields_massn )
        mapping_values = CONV #( mapping_values_massn )
      CHANGING
        value          = value_tmp ).

    massn = value_tmp.

    "Process MASSG mapping
    value_tmp = CONV #( p0000-massg ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = zmhp_cl_mig_utils=>massg
        field_oracle   = zmhp_cl_mig_utils=>reasoncode
        mapping_fields = CONV #( mapping_fields_massg )
        mapping_values = CONV #( mapping_values_massg )
      CHANGING
        value          = value_tmp ).

    massg = value_tmp.

    "Get molga for IT/AT
    READ TABLE molga INTO DATA(molga_country) INDEX 1.

    DATA(fields_gk) = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = zmhp_cl_mig_utils=>persg
                                                                    value     = p0001-persg )
                                                                  ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = persk
                                                                    value     = p0001-persk )
                                                                  ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = 'MOLGA'
                                                                    value     = molga_country-low )
                                                                  ( infty     = zmhp_cl_mig_utils=>it0001
                                                                    field_sap = 'ANSVH'
                                                                    value     = p0001-ansvh ) ).

    DATA(worker_sap) = persk.
    value_tmp = CONV #( p0001-persk ).
    "for Austria employee group is relevant for workertype
    IF '03'    IN molga OR
      sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-australia  OR
       sy-mandt = zmhp_cl_int_constants=>cofu_mandant-newzealand.
      worker_sap = zmhp_cl_mig_utils=>persg.
      value_tmp = CONV #( p0001-persg ).
    ENDIF.

    "Process workertype mapping
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = worker_sap
        field_oracle   = zmhp_cl_mig_utils=>workertype
        mapping_fields = CONV #( mapping_fields_persg_wt )
        mapping_values = CONV #( mapping_values_persg_wt )
        fields         = fields_gk
      CHANGING
        value          = value_tmp ).

    worker_type = value_tmp.

    "Process assignmenttype mapping
    "for Austria/Australie employee group is relevant for assignmenttype
    value_tmp = COND #( WHEN '03'     IN molga                                         THEN p0001-persg
                        WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-australia THEN p0001-persg
                        ELSE p0001-persk ).

    worker_sap = COND #( WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-newzealand THEN persk
                         ELSE worker_sap ).


    mapping_handler->process_mapping(
    EXPORTING
      import         = abap_false
      export         = abap_true
      infty          = zmhp_cl_mig_utils=>it0001
      field_sap      = worker_sap
      field_oracle   = assignmenttype
      mapping_fields = CONV #( mapping_fields_persg_at )
      mapping_values = CONV #( mapping_values_persg_at )
      fields         = fields_gk
      CHANGING
        value          = value_tmp ).
    assignment_type    = value_tmp.

    CLEAR: value_tmp.
    "Process workercategory mapping
    value_tmp = SWITCH #( sy-mandt
                          WHEN zmhp_cl_int_constants=>cofu_mandant-germany THEN p0001-abkrs
                          WHEN zmhp_cl_int_constants=>cofu_mandant-austria THEN p0001-persk
                          WHEN zmhp_cl_int_constants=>cofu_mandant-italy   THEN p0001-persk ).

    worker_sap = SWITCH zmhp_dd_field( sy-mandt
                                       WHEN zmhp_cl_int_constants=>cofu_mandant-germany    THEN 'ABKRS'
                                       WHEN zmhp_cl_int_constants=>cofu_mandant-australia  THEN 'ABKRS'
                                       WHEN zmhp_cl_int_constants=>cofu_mandant-newzealand THEN 'ABKRS'
                                       WHEN zmhp_cl_int_constants=>cofu_mandant-austria    THEN persk
                                       WHEN zmhp_cl_int_constants=>cofu_mandant-italy      THEN persk ).
    IF value_tmp  IS NOT INITIAL AND
       worker_sap IS NOT INITIAL.
      mapping_handler->process_mapping(
        EXPORTING
          import         = abap_false
          export         = abap_true
          infty          = zmhp_cl_mig_utils=>it0001
          field_sap      = worker_sap
          field_oracle   = workercategory
          mapping_fields = CONV #( mapping_fields_persk_wc )
          mapping_values = CONV #( mapping_values_persk_wc )
        CHANGING
          value          = value_tmp ).

      worker_category    = value_tmp.
    ENDIF.

    "Process assignmentcategory mapping
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = assignmentcategory
        mapping_fields = CONV #( mapping_fields_persk_ac )
        fields         = fields_gk
      CHANGING
        value          = assignment_category ).

    "Process assignmentpersontype mapping
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = assignmentpersontype
        mapping_fields = CONV #( mapping_fields_persk_apt )
        fields         = fields_gk
      CHANGING
        value          = assignment_person_type_code ).

    "Process persontypecode mapping
    value_tmp = p0001-persk.
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = persontypecode
        mapping_fields = CONV #( mapping_fields_persk_ptc )
        mapping_values = CONV #( mapping_values_persk_ptc )
        fields         = fields_gk
      CHANGING
        value          = value_tmp ).

    person_type_code = value_tmp.

**JMB20211031 start insert - pass assignment person type instead of person type code (C400129651-5748)
*
    IF assignment_person_type_code IS NOT INITIAL.
      person_type_code = assignment_person_type_code.
    ENDIF.
*JMB20211031 insert end

    "Process systempersontype mapping
    value_tmp = CONV #( p0001-persk ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = systempersontype
        mapping_fields = CONV #( mapping_fields_persk_spt )
        mapping_values = CONV #( mapping_values_persk_spt )
        fields         = fields_gk
     CHANGING
        value          = value_tmp ).

    system_person_type = value_tmp.

    fields = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty     = zmhp_cl_mig_utils=>it0000
                                                           field_sap = stat2
                                                           value     = p0000-stat2 )
                                                         ( infty     = zmhp_cl_mig_utils=>it0001
                                                           field_sap = 'KOSTL'
                                                           value     = p0001-kostl )
                                                         ( infty     = zmhp_cl_mig_utils=>it0000
                                                           field_sap = 'STAT1'
                                                           value     = p0000-stat1 ) ).

    "Process STAT2 mapping
    value_tmp = CONV #( p0000-stat2 ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = stat2
        field_oracle   = assignmentstatustypecode
        mapping_fields = CONV #( mapping_fields_stat2 )
        mapping_values = CONV #( mapping_values_stat2 )
        fields         = fields
      CHANGING
        value          = value_tmp ).

    assign_status      = value_tmp.

    "Process BUKRS Mapping
    value_tmp = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = zmhp_cl_mig_utils=>legalemployername
        mapping_fields = CONV #( mapping_fields_bukrs )
        mapping_values = CONV #( mapping_values_bukrs )
      CHANGING
        value          = value_tmp ).

    legal_employer_name = value_tmp.

**JMB20211011 start delete - Dominik have changed name of OrgUnits on Prod, so no mapping is needed
*
*  "Process department Mapping
*  value_tmp = CONV #( p0001-orgeh ).
*  mapping_handler->process_mapping(
*    EXPORTING
*      import         = abap_false
*      export         = abap_true
*      infty          = ZMHP_cl_mig_utils=>it0001
*      field_sap      = ZMHP_cl_mig_utils=>orgeh
*      field_oracle   = ZMHP_cl_mig_utils=>departmentname
*      mapping_fields = CONV #( mapping_fields_department )
*      mapping_values = CONV #( mapping_values_department )
*    CHANGING
*      value          = value_tmp ).
*
*  department_name = value_tmp.
*JMB20211011 delete end, start insert
    LOOP AT hrp1000_orgeh ASSIGNING FIELD-SYMBOL(<hrp1000>) WHERE begda LE sy-datum AND
                                                                  endda GE sy-datum AND
                                                                  objid EQ p0001-orgeh.
      department_name = <hrp1000>-stext.
      EXIT.
    ENDLOOP.
*JMB20211011 insert end

    "Process BUKRS Mapping
    value_tmp = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = zmhp_cl_mig_utils=>businessunitshortcode
        mapping_fields = CONV #( mapping_fields_bukrs_bu )
        mapping_values = CONV #( mapping_values_bukrs_bu )
      CHANGING
        value          = value_tmp ).

    business_unit_code = value_tmp.

    "Process BUKRS/NormalHours mapping
    value_tmp       = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = normal_hours
        mapping_fields = CONV #( mapping_fields_hours )
        mapping_values = CONV #( mapping_values_hours )
      CHANGING
        value          = value_tmp ).

    normalhours = value_tmp.

**JMB20210811 start insert - check jobCode mapping
*
    IF job IS NOT INITIAL.
      value_tmp       = CONV #( job ).
      mapping_handler->process_mapping(
        EXPORTING
          import         = abap_false
          export         = abap_true
          infty          = zmhp_cl_mig_utils=>it0001
          field_sap      = 'STELL'
          field_oracle   = 'JOBCODE'
          mapping_fields = CONV #( mapping_fields_job )
          mapping_values = CONV #( mapping_values_job )
        CHANGING
          value          = value_tmp ).
      job_code = value_tmp.
    ENDIF.
*JMB20210811 end insert

    "01.03.2021 - Final decision: for history provide default value
*  CHECK p0001-endda LT sy-datum.
    CHECK hire EQ abap_true.

    business_unit_code = zmhp_cl_mig_utils=>default_business_unit.
    CLEAR: location_code, department_name, job_code. "JMB20210705 D - Don??t clear normalhours (Ticket 4999).
  ENDMETHOD.


  METHOD map_mig_values.
    DATA: value_tmp TYPE zmhp_dd_value.
    DATA(fields) = VALUE zmhp_cl_int_infty_proc_xml=>t_fields( ( infty = zmhp_cl_mig_utils=>it0001
                                                                 field_sap = zmhp_cl_mig_utils=>btrtl
                                                                 value = p0001-btrtl )
                                                               ( infty = zmhp_cl_mig_utils=>it0016
                                                                 field_sap = zmhp_cl_mig_utils=>cttyp
                                                                 value = p0016-cttyp )
                                                               ( infty = zmhp_cl_mig_utils=>it0001
                                                                 field_sap = zmhp_cl_mig_utils=>werks
                                                                 value = p0001-werks ) ).
    "Instantiate mapping class
    DATA(mapping_handler) = NEW zmhp_cl_int_mapping( ).

    "Process WERKS/BTRTL mapping (LocationCode)
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>btrtl
        field_oracle   = zmhp_cl_mig_utils=>locationcode
        mapping_fields = CONV #( mapping_fields_btrtl )
        fields         = fields
      CHANGING
        value          = location_code ).

    "Process MASSN mapping
    value_tmp       = CONV #( p0000-massn ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = zmhp_cl_mig_utils=>massn
        field_oracle   = zmhp_cl_mig_utils=>actioncode
        mapping_fields = CONV #( mapping_fields_massn )
        mapping_values = CONV #( mapping_values_massn )
      CHANGING
        value          = value_tmp ).

    massn = value_tmp.

    "Process MASSG mapping
    value_tmp = CONV #( p0000-massg ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = zmhp_cl_mig_utils=>massg
        field_oracle   = zmhp_cl_mig_utils=>reasoncode
        mapping_fields = CONV #( mapping_fields_massg )
        mapping_values = CONV #( mapping_values_massg )
      CHANGING
        value          = value_tmp ).

    massg = value_tmp.

    "Process workertype mapping
    value_tmp = CONV #( p0001-persg ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>persg
        field_oracle   = zmhp_cl_mig_utils=>workertype
        mapping_fields = CONV #( mapping_fields_persg_wt )
        mapping_values = CONV #( mapping_values_persg_wt )
      CHANGING
        value          = value_tmp ).

    worker_type = value_tmp.


    "Process assignmenttype mapping
    value_tmp = CONV #( p0001-persk ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>persg
        field_oracle   = assignmenttype
        mapping_fields = CONV #( mapping_fields_cttyp )
        mapping_values = CONV #( mapping_values_cttyp )
      CHANGING
        value          = value_tmp ).

    assignment_type    = value_tmp.

    "Process workercategory mapping
    value_tmp = CONV #( p0001-persk ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = workercategory
        mapping_fields = CONV #( mapping_fields_persk_wc )
        mapping_values = CONV #( mapping_values_persk_wc )
      CHANGING
        value          = value_tmp ).

    worker_category    = value_tmp.

    "Process assignmentcategory mapping
    value_tmp = CONV #( p0001-persk ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = assignmentcategory
        mapping_fields = CONV #( mapping_fields_persk_ac )
        mapping_values = CONV #( mapping_values_persk_ac )
      CHANGING
        value          = value_tmp ).

    assignment_category = value_tmp.

    "Process assignmentcategory mapping
    value_tmp = CONV #( p0001-persk ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = assignmentpersontype
        mapping_fields = CONV #( mapping_fields_persk_ptc )
        mapping_values = CONV #( mapping_values_persk_ptc )
      CHANGING
        value          = value_tmp ).

    person_type_code = value_tmp.

    "Process assignmentcategory mapping
    value_tmp = CONV #( p0001-persk ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = persk
        field_oracle   = systempersontype
        mapping_fields = CONV #( mapping_fields_persk_spt )
        mapping_values = CONV #( mapping_values_persk_spt )
     CHANGING
        value          = value_tmp ).

*    system_person_type = value_tmp.

*DEBIEAKSA: Wenn Wert leer -> Dann Default to "Home"
    IF value_tmp IS NOT INITIAL.
      system_person_type = value_tmp.
    ELSE.
      system_person_type = 'EMP'.
    ENDIF.

    "Process STAT2 mapping
    value_tmp = CONV #( p0000-stat2 ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0000
        field_sap      = stat2
        field_oracle   = assignmentstatustypecode
        mapping_fields = CONV #( mapping_fields_stat2 )
        mapping_values = CONV #( mapping_values_stat2 )
      CHANGING
        value          = value_tmp ).

    assign_status      = value_tmp.

    "Process BUKRS Mapping
    value_tmp = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = zmhp_cl_mig_utils=>legalemployername
        mapping_fields = CONV #( mapping_fields_bukrs )
        mapping_values = CONV #( mapping_values_bukrs )
      CHANGING
        value          = value_tmp ).

    legal_employer_name = value_tmp.

    "Process department Mapping
    value_tmp = CONV #( p0001-orgeh ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>orgeh
        field_oracle   = zmhp_cl_mig_utils=>departmentname
        mapping_fields = CONV #( mapping_fields_department )
        mapping_values = CONV #( mapping_values_department )
      CHANGING
        value          = value_tmp ).

    department_name = value_tmp.

    "Process BUKRS Mapping
    value_tmp = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = zmhp_cl_mig_utils=>businessunitshortcode
        mapping_fields = CONV #( mapping_fields_bukrs_bu )
        mapping_values = CONV #( mapping_values_bukrs_bu )
      CHANGING
        value          = value_tmp ).


    business_unit_code = value_tmp.

    "Process BUKRS/NormalHours mapping
    value_tmp       = CONV #( p0001-bukrs ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0001
        field_sap      = zmhp_cl_mig_utils=>bukrs
        field_oracle   = normal_hours
        mapping_fields = CONV #( mapping_fields_hours )
        mapping_values = CONV #( mapping_values_hours )
      CHANGING
        value          = value_tmp ).

    normalhours = value_tmp.

    "01.03.2021 - Final decision: for history provide default value
*  CHECK p0001-endda LT sy-datum.
    CHECK hire EQ abap_true.

    business_unit_code = zmhp_cl_mig_utils=>default_business_unit.
    CLEAR: location_code, department_name. "JMB20210705 D - Don??t clear normalhours (Ticket 4999).
  ENDMETHOD.


  METHOD proceed_cofu_assignment.
    DATA: hire_data    TYPE string,
          data_asg_ext TYPE string,
          hire_wt_data TYPE string,
          data_wt_all  TYPE string.


    "build workterms, assignment, assignmentExtraInfo and assignmentSupervisor in same method (time ranges)
    DATA(workterms) = NEW zmhp_cl_mig_work_terms( pernr = pernr
                                                  begda = begda
                                                  endda = endda
                                                  cogl  = cogl
                                                  cofu  = cofu
                                                  cogu  = cogu
                                                  molga = molga ).

    "build workterms, assignment, assignmentExtraInfo and assignmentSupervisor in same method (time ranges)
    DATA(manager) = NEW zmhp_cl_mig_assignment_man( pernr = pernr
                                                    begda = begda
                                                    endda = endda
                                                    cogl  = cogl
                                                    cofu  = cofu
                                                    cogu  = cogu
                                                    molga = molga ).

*    "build workterms, assignment, assignmentExtraInfo and assignmentSupervisor in same method (time ranges)
*    DATA(asg_ext_info) = NEW zmhp_cl_mig_assign_extra_info( pernr = pernr
*                                                            begda = begda
*                                                            endda = endda
*                                                            cogl  = cogl
*                                                            cofu  = cofu
*                                                            cogu  = cogu
*                                                            molga = molga ).
    me->vp_src_id   = vp_src_id.
    me->vp_wkr_id   = vp_wkr_id.
    p0000           = worker->p0000.

    get_cofu_data( ).
    get_mapping_cofu_fields( ).
    get_mapping_cofu_values( ).
*    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000       = worker->p0000
*                                                    create_hire = abap_true
*                                           CHANGING p0001 = p0001 ).

    zmhp_cl_mig_utils=>check_assign_supervisor( EXPORTING cofu        = me->cofu
                                                          all_periods = abap_true
                                                 CHANGING p0001       = p0001 ).

    workterms->proceed_cofu_work_terms( vp_src_id  = vp_src_id
                                        vp_wkr_id  = vp_wkr_id
                                        worker     = worker
                                        assignment = me ).

    manager->proceed_cofu_assign_manager( vp_src_id  = vp_src_id
                                          worker     = worker ).

*    asg_ext_info->proceed_cofu_extra_info( vp_src_id  = vp_src_id
*                                           worker     = worker ).

    LOOP AT pernr ASSIGNING FIELD-SYMBOL(<pernr>).
      CLEAR: hr_periods.
      collect_hr_periods( CONV #( <pernr>-low ) ).

      zmhp_cl_mig_utils=>summarize_past( CHANGING hr_periods = hr_periods ).

      "check if creation of dummy hire entry is needed
      IF lines( hr_periods ) GT 1.
        READ TABLE hr_periods INTO DATA(period) INDEX 1.

        "dummy hire entry should be create
        IF period-endda LT sy-datum.
          DATA(hire_data_wt)    = workterms->create_hire_cofu_entry( pernr  = CONV #( <pernr>-low )
                                                                     period = period ).

          me->vp_wterm_id  = workterms->vp_wterm_col.

          DATA(hire_data_pernr) = create_hire_cofu_entry( pernr  = CONV #( <pernr>-low )
                                                          period = period ).

          DELETE TABLE hr_periods FROM period.
          APPEND VALUE #( sign = 'I' option = 'EQ' low = <pernr>-low ) TO pernr_history.

          CONCATENATE hire_data hire_data_pernr INTO hire_data.
          CONCATENATE hire_wt_data hire_data_wt INTO hire_wt_data.
        ENDIF.

      ENDIF.

      DATA(data_wt)    = workterms->map_cofu_data( EXPORTING pernr   = CONV #( <pernr>-low )
                                                   CHANGING periods = hr_periods ).
      me->vp_wterm_id  = workterms->vp_wterm_col.

      DATA(data_pernr) = map_cofu_data( EXPORTING pernr   = CONV #( <pernr>-low )
                                        CHANGING periods = hr_periods ).

      "get supervisor
      manager->get_cofu_managers_of_emp( periods = hr_periods
                                         pernr   = CONV #( <pernr>-low ) ).

      DATA(manager_data) = manager->map_cofu_data( periods = hr_periods
                                                   pernr   = CONV #( <pernr>-low ) ).

*      DATA(data_asg_ext_pernr) = asg_ext_info->map_cofu_data( periods = hr_periods
*                                                              pernr   = CONV #( <pernr>-low ) ).

*      CONCATENATE data_asg_ext data_asg_ext_pernr INTO data_asg_ext.
      CONCATENATE data_wt_all data_wt          INTO data_wt_all.
      CONCATENATE data data_pernr              INTO data.
      CONCATENATE data_assign_man manager_data INTO data_assign_man.
    ENDLOOP.

    CONCATENATE hire_wt_data
                data_wt_all
                cl_abap_char_utilities=>newline
                hire_data
                data
                cl_abap_char_utilities=>newline
                data_asg_ext INTO data.
  ENDMETHOD.


  METHOD proceed_cogl_assignment.
    me->vp_src_id   = vp_src_id.
    me->vp_wkr_id   = vp_wkr_id.
    me->vp_wterm_id = vp_wterm_id.
    p0000           = worker->p0000.
    get_cogl_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000       = worker->p0000
                                                    create_hire = abap_true
                                           CHANGING p0001 = p0001 ).
    DATA(hire_data) = create_hire_entry( ).

    IF workterms->p0001 IS INITIAL.
      zmhp_cl_mig_utils=>check_assign_supervisor( EXPORTING all_periods = abap_true
                                                   CHANGING p0001       = p0001 ).
    ELSE.
      p0001 = workterms->p0001.
    ENDIF.

    data = map_cogl_data( IMPORTING work_measure = work_measure ).

    CONCATENATE hire_data data INTO data.
  ENDMETHOD.


  METHOD proceed_cogu_assignment.
    me->vp_src_id   = vp_src_id.
    me->vp_wkr_id   = vp_wkr_id.
    me->vp_wterm_id = vp_wterm_id.
    p0000           = worker->p0000.

    get_cofu_data( ).
    get_mapping_cogu_fields( ).
    get_mapping_cogu_values( ).
    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000       = worker->p0000
                                                    create_hire = abap_true
                                           CHANGING p0001 = p0001 ).

    zmhp_cl_mig_utils=>check_assign_supervisor( EXPORTING all_periods = abap_true
                                                 CHANGING p0001       = p0001 ).

    DATA(hire_data) = create_hire_cogu_entry( ).
    data = map_cogu_data( IMPORTING work_measure = work_measure ).

    CONCATENATE hire_data data INTO data.
  ENDMETHOD.
ENDCLASS.
