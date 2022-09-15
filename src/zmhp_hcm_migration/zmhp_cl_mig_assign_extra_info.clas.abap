class ZMHP_CL_MIG_ASSIGN_EXTRA_INFO definition
  public
  create public .

public section.

  data PERNR type RSDSSELOPT_T .
  data BEGDA type BEGDA .
  data ENDDA type ENDDA .
  data COFU type BOOLEAN .
  data COGL type BOOLEAN .
  data MOLGA type RSDSSELOPT_T .
  data VP_EXTRA_INFO type /IWBEP/T_MGW_NAME_VALUE_PAIR .
  constants ASSIGN_EXTRA_INFO type STRING value 'AssignmentExtraInfo' ##NO_TEXT.
  constants NL_TMGT type STRING value 'SEW_NL_TMGT' ##NO_TEXT.
  data P0050 type PTT_P0050 .
  constants SRC_ID_NL_PREFIX type STRING value 'PER_ASG_EIT' ##NO_TEXT.

  methods PROCEED_COFU_EXTRA_INFO
    importing
      !VP_SRC_ID type /IWBEP/T_MGW_NAME_VALUE_PAIR
      !WORKER type ref to ZMHP_CL_MIG_WORKER .
  methods CONSTRUCTOR
    importing
      !PERNR type RSDSSELOPT_T
      !BEGDA type BEGDA
      !ENDDA type ENDDA
      !COFU type BOOLEAN
      !COGL type BOOLEAN
      !MOLGA type RSDSSELOPT_T
      !COGU type BOOLEAN .
  methods MAP_COFU_DATA
    importing
      !PERIODS type HRPERIODS_TAB
      !PERNR type PERNR_D
    returning
      value(DATA) type STRING .
  methods CREATE_METADATA
    returning
      value(METADATA) type STRING .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA vp_src_id TYPE /iwbep/t_mgw_name_value_pair .
    DATA cogu TYPE boolean .
    DATA de_tmgt TYPE string VALUE 'SEW_DE_TMGT' ##NO_TEXT.

    METHODS get_cofu_data .
ENDCLASS.



CLASS ZMHP_CL_MIG_ASSIGN_EXTRA_INFO IMPLEMENTATION.


  METHOD constructor.

    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogu  = cogu.
    me->cogl  = cogl.
    me->molga = molga.

    IF cofu EQ abap_true.
      vp_extra_info = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                               ( name = 2  value = assign_extra_info )
                               ( name = 3  value = 'AssignmentId(SourceSystemId)' )
                               ( name = 6  value = 'FLEX:PER_ASSIGNMENT_EIT_EFF' )
                               ( name = 7  value = 'EFF_CATEGORY_CODE' )
                               ( name = 8  value = 'AeiInformationCategory' )
                               ( name = 9  value = 'EffectiveStartDate' )
                               ( name = 10 value = 'EffectiveEndDate' )
                               ( name = 11 value = 'EffectiveLatestChange' )
                               ( name = 11 value = 'EffectiveSequence' )
                               ( name = 11 value = 'InformationType' )
*                               ( name = 11 value = 'sewNlTmgtStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
*                               ( name = 11 value = 'sewNlTmgtEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
*                               ( name = 11 value = 'sewNlTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_NL_TMGT)' )
*                               ( name = 11 value = 'sewDeTmgtStartDt(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
*                               ( name = 11 value = 'sewDeTmgtEndDt(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
*                               ( name = 11 value = 'sewDeTmgtRole(PER_ASSIGNMENT_EIT_EFF=SEW_DE_TMGT)' )
                               ( name = 4  value = 'SourceSystemId' )
                               ( name = 4  value = 'SourceSystemOwner' ) ).
    ENDIF.
  ENDMETHOD.


  METHOD create_metadata.
    DESCRIBE TABLE vp_extra_info LINES DATA(length).

    LOOP AT vp_extra_info ASSIGNING FIELD-SYMBOL(<ext_info_data>).

      "set METADATA title
      CASE <ext_info_data>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <ext_info_data>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_cofu_data.

    "Get IT0050
    SELECT pernr,
           begda,
           endda,
           bdegr FROM pa0050 INTO CORRESPONDING FIELDS OF TABLE @p0050 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id        TYPE string,
          sys_id        TYPE string,
          pernr_counter TYPE i VALUE 0,
          pernr_old     TYPE pernr_d,
          src_sys_id    TYPE string,
          begda_tmp_nl  TYPE string,
          endda_tmp_nl  TYPE string,
          bdegr_nl      TYPE string,
          begda_tmp_de  TYPE string,
          endda_tmp_de  TYPE string,
          bdegr_de      TYPE string,
          data_tmp      TYPE string,
          old_0050      TYPE rsdsselopt_t.

    CONCATENATE zmhp_cl_mig_utils=>sap sy-mandt INTO sys_id.

    LOOP AT periods ASSIGNING FIELD-SYMBOL(<period>).
      LOOP AT p0050 ASSIGNING FIELD-SYMBOL(<p0050>) WHERE pernr EQ pernr          AND
                                                          begda LE <period>-endda AND
                                                          endda GE <period>-begda.
        EXIT.
      ENDLOOP.

      CHECK <p0050> IS ASSIGNED.

      CHECK <p0050>-bdegr IS NOT INITIAL.

      IF pernr_old NE pernr.
        pernr_old = pernr.
        pernr_counter = 0.
      ENDIF.

      pernr_counter = pernr_counter + 1.

      DATA(xx_tmgt) = COND string( WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-germany     THEN de_tmgt
                                   WHEN sy-mandt EQ zmhp_cl_int_constants=>cofu_mandant-netherlands THEN nl_tmgt
                                   ELSE '' ).

      CHECK xx_tmgt IS NOT INITIAL.

      DATA(begda_tmp) = zmhp_cl_mig_utils=>convert_date( <period>-begda ).
      DATA(endda_tmp) = zmhp_cl_mig_utils=>convert_date( <period>-endda ).

      CLEAR: begda_tmp_nl, endda_tmp_nl, bdegr_nl, begda_tmp_de, endda_tmp_de, bdegr_de.

      CASE sy-mandt.
        WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands.
          begda_tmp_nl = begda_tmp.
          endda_tmp_nl = endda_tmp.
          bdegr_nl     = 'PR00000' && <p0050>-bdegr.
        WHEN zmhp_cl_int_constants=>cofu_mandant-germany.
          begda_tmp_de = begda_tmp.
          endda_tmp_de = endda_tmp.
          bdegr_de     = 'PR00000' && <p0050>-bdegr.
      ENDCASE.

      DATA(latestchange) = COND string( WHEN sy-datum BETWEEN <p0050>-begda AND <p0050>-endda THEN zmhp_cl_mig_utils=>yes
                                        ELSE '' ).

      src_id = CONV #( pernr_counter ).
      CONCATENATE zmhp_cl_mig_utils=>assign pernr INTO DATA(asn_id).

      "get source id
      src_sys_id = zmhp_cl_mig_utils=>get_src_id( pernr = pernr
                                                  begda = <period>-begda
                                                  endda = <period>-endda
                                                  vp_src_id = vp_src_id ).
      DATA(counter) = src_id.
      CONDENSE counter.

      "ID needs to be unique
      CONCATENATE xx_tmgt '_' asn_id '_' src_id INTO src_id.
      CONDENSE src_id.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  assign_extra_info
                  asn_id
                  xx_tmgt
                  'PER_ASG_EIT'
                  xx_tmgt
                  begda_tmp
                  endda_tmp
                  latestchange
                  counter
                  xx_tmgt
                  src_id
                  sys_id
*                  begda_tmp_nl
*                  endda_tmp_nl
*                  bdegr_nl
*                  begda_tmp_de
*                  endda_tmp_de
*                  bdegr_de

      INTO data_tmp SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.
  ENDMETHOD.


  METHOD proceed_cofu_extra_info.
    me->vp_src_id = vp_src_id.
    get_cofu_data( ).
  ENDMETHOD.
ENDCLASS.
