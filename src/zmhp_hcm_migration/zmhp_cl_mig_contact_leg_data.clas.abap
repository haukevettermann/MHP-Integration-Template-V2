CLASS zmhp_cl_mig_contact_leg_data DEFINITION
*  PUBLIC
*  CREATE PUBLIC .
*
*  PUBLIC SECTION.
*
*    DATA pernr TYPE rsdsselopt_t .
*    DATA begda TYPE begda .
*    DATA endda TYPE endda .
*    DATA cofu TYPE boolean .
*    DATA cogl TYPE boolean .
*    DATA cogu TYPE boolean .
*    DATA molga TYPE rsdsselopt_t .
*    DATA p0002 TYPE p0002_tab .
*    DATA vp_con_leg_data_structure TYPE /iwbep/t_mgw_name_value_pair .
*    CONSTANTS contact_leg_data TYPE string VALUE 'ContactLegislativeData' ##NO_TEXT.
*    CONSTANTS per TYPE string VALUE 'PER' ##NO_TEXT.
*    CONSTANTS cont TYPE string VALUE 'CONT' ##NO_TEXT.
*    CONSTANTS leg TYPE string VALUE 'LEG' ##NO_TEXT.
*    CONSTANTS sex TYPE zmhp_dd_field VALUE 'SEX' ##NO_TEXT.
*    CONSTANTS gesch TYPE zmhp_dd_field VALUE 'GESCH' ##NO_TEXT.
*
*    METHODS constructor
*      IMPORTING
*        !pernr TYPE rsdsselopt_t
*        !begda TYPE begda
*        !endda TYPE endda
*        !cofu  TYPE boolean
*        !cogl  TYPE boolean
*        !molga TYPE rsdsselopt_t
*        !cogu  TYPE boolean .
*    METHODS proceed_cofu_con_leg_data
*      IMPORTING
*        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
*        !worker     TYPE REF TO zmhp_cl_mig_worker
*      RETURNING
*        VALUE(data) TYPE string .
*    METHODS create_metadata
*      RETURNING
*        VALUE(metadata) TYPE string .
*  PROTECTED SECTION.
*  PRIVATE SECTION.
*
*    DATA p0021 TYPE p0021_tab .
*    DATA p0001 TYPE p0001_tab .
*    DATA land1_map TYPE /iwbep/t_mgw_name_value_pair .
*    DATA mapping_fields_gesch TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapp_fi .
*    DATA mapping_values_gesch TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapping .
*
*    METHODS get_cofu_data .
*    METHODS map_cofu_data
*      IMPORTING
*        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
*      RETURNING
*        VALUE(data) TYPE string .
*    METHODS map_mig_values
*      IMPORTING
*        !p0002 TYPE p0002
*      EXPORTING
*        !gesch TYPE zmhp_dd_value .
*    METHODS get_mapping_fields .
*    METHODS get_mapping_values .
*ENDCLASS.
*
*
*
*CLASS zmhp_cl_mig_contact_leg_data IMPLEMENTATION.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Public Method ZMHP_CL_MIG_CONTACT_LEG_DATA->CONSTRUCTOR
** +-------------------------------------------------------------------------------------------------+
** | [--->] PERNR                          TYPE        RSDSSELOPT_T
** | [--->] BEGDA                          TYPE        BEGDA
** | [--->] ENDDA                          TYPE        ENDDA
** | [--->] COFU                           TYPE        BOOLEAN
** | [--->] COGL                           TYPE        BOOLEAN
** | [--->] MOLGA                          TYPE        RSDSSELOPT_T
** | [--->] COGU                           TYPE        BOOLEAN
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD constructor.
*    me->pernr = pernr.
*    me->begda = begda.
*    me->endda = endda.
*    me->cofu  = cofu.
*    me->cogl  = cogl.
*    me->cogu  = cogu.
*    me->molga = molga.
*
*    IF cogu EQ abap_true.
*
*    ELSEIF cofu EQ abap_true.
*      vp_con_leg_data_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
*                                           ( name = 2  value = contact_leg_data )
*                                           ( name = 3  value = 'EffectiveStartDate' )
*                                           ( name = 4  value = 'EffectiveEndDate' )
*                                           ( name = 5  value = 'PersonId(SourceSystemId)' )
*                                           ( name = 6  value = 'LegislationCode' )
*                                           ( name = 7  value = 'Sex' )
*                                           ( name = 8  value = 'SourceSystemOwner' )
*                                           ( name = 9  value = 'SourceSystemId' ) ).
*    ELSEIF cogl EQ abap_true.
*
*    ENDIF.
*
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Public Method ZMHP_CL_MIG_CONTACT_LEG_DATA->CREATE_METADATA
** +-------------------------------------------------------------------------------------------------+
** | [<-()] METADATA                       TYPE        STRING
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD create_metadata.
*
*    DESCRIBE TABLE vp_con_leg_data_structure LINES DATA(length).
*
*    LOOP AT vp_con_leg_data_structure ASSIGNING FIELD-SYMBOL(<contact_leg_data>).
*
*      "set METADATA title
*      CASE <contact_leg_data>-name.
*        WHEN 1.
*          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
*          CONTINUE.
*      ENDCASE.
*
*      CONCATENATE metadata <contact_leg_data>-value INTO metadata.
*
*      "set separator
*      CHECK length NE sy-tabix.
*      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
*    ENDLOOP.
*
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Private Method ZMHP_CL_MIG_CONTACT_LEG_DATA->GET_COFU_DATA
** +-------------------------------------------------------------------------------------------------+
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD get_cofu_data.
*
*    "Get IT0002
*    SELECT pernr,
*           begda,
*           endda,
*           sprsl,
*           gesch,
*           famst,
*           famdt INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr AND
*                                                                             begda LE @endda AND
*                                                                             endda GE @begda.
*
*    "Get BUKRS for LegislationCode
*    SELECT pernr,
*           begda,
*           endda,
*           bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
*                                                                             begda LE @endda AND
*                                                                             endda GE @begda.
*
*
*
*    DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
*    SORT bukrs BY low.
*    DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
*    land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Private Method ZMHP_CL_MIG_CONTACT_LEG_DATA->GET_MAPPING_FIELDS
** +-------------------------------------------------------------------------------------------------+
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD get_mapping_fields.
*
*    "get mapping fields for sex
*    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
*                                                     infty        = zmhp_cl_mig_utils=>it0002
*                                                     sap_field    = gesch
*                                                     oracle_field = sex
*                                                     export       = abap_true
*                                           IMPORTING mapping_fields = mapping_fields_gesch ).
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Private Method ZMHP_CL_MIG_CONTACT_LEG_DATA->GET_MAPPING_VALUES
** +-------------------------------------------------------------------------------------------------+
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD get_mapping_values.
*    "get mapping values for sex
*    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
*                                                     infty        = zmhp_cl_mig_utils=>it0002
*                                                     sap_field    = gesch
*                                                     oracle_field = sex
*                                                     export       = abap_true
*                                           IMPORTING mapping_values = mapping_values_gesch ).
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Private Method ZMHP_CL_MIG_CONTACT_LEG_DATA->MAP_COFU_DATA
** +-------------------------------------------------------------------------------------------------+
** | [--->] VP_SRC_ID                      TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
** | [<-()] DATA                           TYPE        STRING
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD map_cofu_data.
*
*    DATA: src_id TYPE string,
*          sys_id TYPE string,
*          land1  TYPE /iwbep/s_mgw_name_value_pair.
*
*
*    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).
*      CLEAR land1.
*      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0002>-pernr AND
*                                                          begda LE <p0002>-endda AND
*                                                          endda GE <p0002>-begda.
*
*        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
*        EXIT.
*      ENDLOOP.
*
*      DATA(eff_start_date) = zmhp_cl_mig_utils=>convert_date( <p0002>-begda ).
*
*      CONCATENATE per
*                  cont
*                  <p0002>-pernr
*      INTO DATA(per_id) SEPARATED BY '_'.
*
*      sys_id = 'SAP_' && sy-mandt.
*
*      CONCATENATE per
*                  cont
*                  leg
*                  <p0002>-pernr
*      INTO src_id SEPARATED BY '_'.
*
*      map_mig_values( EXPORTING p0002 = <p0002>
*                      IMPORTING gesch = DATA(gesch_tmp) ).
*
*
*      CONCATENATE zmhp_cl_mig_utils=>merge
*                  contact_leg_data
*                  eff_start_date
*                  ''
*                  per_id
*                  land1-value
*                  gesch_tmp
*                  sys_id
*                  src_id
*      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.
*
*      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
*    ENDLOOP.
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Private Method ZMHP_CL_MIG_CONTACT_LEG_DATA->MAP_MIG_VALUES
** +-------------------------------------------------------------------------------------------------+
** | [--->] P0002                          TYPE        P0002
** | [<---] GESCH                          TYPE        ZMHP_DD_VALUE
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD map_mig_values.
*    DATA: value_tmp TYPE zmhp_dd_value.
*
*    "Instantiate mapping class
*    DATA(mapping_handler) = NEW zmhp_cl_int_mapping( ).
*
*    "Process GESCH mapping
*    value_tmp = CONV #( p0002-gesch ).
*    mapping_handler->process_mapping(
*      EXPORTING
*        import         = abap_false
*        export         = abap_true
*        infty          = zmhp_cl_mig_utils=>it0002
*        field_sap      = zmhp_cl_mig_person_leg_data=>gesch
*        field_oracle   = sex
*        mapping_fields = CONV #( mapping_fields_gesch )
*        mapping_values = CONV #( mapping_values_gesch )
*     CHANGING
*       value           = value_tmp ).
*
*    gesch              = value_tmp.
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Public Method ZMHP_CL_MIG_CONTACT_LEG_DATA->PROCEED_COFU_CON_LEG_DATA
** +-------------------------------------------------------------------------------------------------+
** | [--->] VP_SRC_ID                      TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
** | [--->] WORKER                         TYPE REF TO ZMHP_CL_MIG_WORKER
** | [<-()] DATA                           TYPE        STRING
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD proceed_cofu_con_leg_data.
*
*    get_cofu_data( ).
*    get_mapping_fields( ).
*    get_mapping_values( ).
**    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
**                                           CHANGING p0002 = p0002 ).
*
*    data =  map_cofu_data( vp_src_id ).
*
*  ENDMETHOD.
*ENDCLASS.


PUBLIC
 CREATE PUBLIC .

  PUBLIC SECTION.

    DATA pernr TYPE rsdsselopt_t .
    DATA begda TYPE begda .
    DATA endda TYPE endda .
    DATA cofu TYPE boolean .
    DATA cogl TYPE boolean .
    DATA cogu TYPE boolean .
    DATA molga TYPE rsdsselopt_t .
    DATA vp_con_leg_data_structure TYPE /iwbep/t_mgw_name_value_pair .
    CONSTANTS contact_leg_data TYPE string VALUE 'ContactLegislativeData' ##NO_TEXT.
    CONSTANTS per TYPE string VALUE 'PER' ##NO_TEXT.
    CONSTANTS cont TYPE string VALUE 'CONT' ##NO_TEXT.
    CONSTANTS leg TYPE string VALUE 'LEG' ##NO_TEXT.
    CONSTANTS sex TYPE zmhp_dd_field VALUE 'SEX' ##NO_TEXT.
    CONSTANTS gesch TYPE zmhp_dd_field VALUE 'GESCH' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !pernr TYPE rsdsselopt_t
        !begda TYPE begda
        !endda TYPE endda
        !cofu  TYPE boolean
        !cogl  TYPE boolean
        !molga TYPE rsdsselopt_t
        !cogu  TYPE boolean .
    METHODS proceed_cofu_con_leg_data
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
        !worker     TYPE REF TO zmhp_cl_mig_worker
      RETURNING
        VALUE(data) TYPE string .
    METHODS create_metadata
      RETURNING
        VALUE(metadata) TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA p0021 TYPE p0021_tab .
    DATA p0001 TYPE p0001_tab .
    DATA land1_map TYPE /iwbep/t_mgw_name_value_pair .
    DATA mapping_fields_gesch TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapp_fi .
    DATA mapping_values_gesch TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapping .

    METHODS get_cofu_data .
    METHODS map_cofu_data
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
      RETURNING
        VALUE(data) TYPE string .
    METHODS map_mig_values
      IMPORTING
        !p0021 TYPE p0021
      EXPORTING
        !gesch TYPE zmhp_dd_value .
    METHODS get_mapping_fields .
    METHODS get_mapping_values .
ENDCLASS.



CLASS ZMHP_CL_MIG_CONTACT_LEG_DATA IMPLEMENTATION.


  METHOD constructor.
    me->pernr = pernr.
    me->begda = begda.
    me->endda = endda.
    me->cofu  = cofu.
    me->cogl  = cogl.
    me->cogu  = cogu.
    me->molga = molga.

    IF cogu EQ abap_true.

    ELSEIF cofu EQ abap_true.
      vp_con_leg_data_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                           ( name = 2  value = contact_leg_data )
                                           ( name = 3  value = 'EffectiveStartDate' )
                                           ( name = 4  value = 'EffectiveEndDate' )
                                           ( name = 5  value = 'PersonId(SourceSystemId)' )
                                           ( name = 6  value = 'LegislationCode' )
                                           ( name = 7  value = 'Sex' )
                                           ( name = 8  value = 'SourceSystemOwner' )
                                           ( name = 9  value = 'SourceSystemId' ) ).
    ELSEIF cogl EQ abap_true.

    ENDIF.


  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_con_leg_data_structure LINES DATA(length).

    LOOP AT vp_con_leg_data_structure ASSIGNING FIELD-SYMBOL(<contact_leg_data>).

      "set METADATA title
      CASE <contact_leg_data>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_leg_data>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.


  ENDMETHOD.


  METHOD get_cofu_data.
**JMB20211312 start insert - select only specific subtypes
*
    DATA: famsa TYPE rsdsselopt_t.
*    CASE sy-mandt.
*      WHEN zmhp_cl_int_constants=>cofu_mandant-netherlands.
    famsa = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '1' )
                                ( sign = 'I' option = 'EQ' low = '13' )
                                ( sign = 'I' option = 'EQ' low = '2' )
                                ( sign = 'I' option = 'EQ' low = '6' )
                                ( sign = 'I' option = 'EQ' low = '11' )
                                ( sign = 'I' option = 'EQ' low = '12' )
                                ( sign = 'I' option = 'EQ' low = '14' ) ).
*    ENDCASE.
*JMB20211312 insert end

    "Get IT0021
    SELECT pernr,
           begda,
           endda,
           fasex
    INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                            AND famsa IN @famsa
                                                            AND begda LE @endda
                                                            AND endda GE @begda.

    "Get BUKRS for LegislationCode
    SELECT pernr,
           begda,
           endda,
           bukrs INTO CORRESPONDING FIELDS OF TABLE @p0001 FROM pa0001 WHERE pernr IN @pernr AND
                                                                             begda LE @endda AND
                                                                             endda GE @begda.



    DATA(bukrs) = VALUE rsdsselopt_t( FOR <p0001> IN p0001 ( sign = 'I' option = 'EQ' low = <p0001>-bukrs ) ).
    SORT bukrs BY low.
    DELETE ADJACENT DUPLICATES FROM bukrs COMPARING low.
    land1_map = zmhp_cl_mig_utils=>get_legislation_codes( bukrs ).

  ENDMETHOD.


  METHOD get_mapping_fields.

    "get mapping fields for sex
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0002
                                                     sap_field    = gesch
                                                     oracle_field = sex
                                                     export       = abap_true
                                           IMPORTING mapping_fields = mapping_fields_gesch ).

  ENDMETHOD.


  METHOD get_mapping_values.
    "get mapping values for sex
    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga        = molga
                                                     infty        = zmhp_cl_mig_utils=>it0002
                                                     sap_field    = gesch
                                                     oracle_field = sex
                                                     export       = abap_true
                                           IMPORTING mapping_values = mapping_values_gesch ).

  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string,
          land1  TYPE /iwbep/s_mgw_name_value_pair.

    CHECK p0021 IS NOT INITIAL. "IFT20211207 I, due to dump
    SORT p0021 BY pernr ASCENDING begda ASCENDING.

    DATA(check_pernr) = p0021[ 1 ]-pernr.
    DATA(count) = 0.

    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

      IF <p0021>-pernr NE check_pernr.
        count = 1.
        check_pernr = <p0021>-pernr.
      ELSE.
        count = count + 1.
      ENDIF.

      "get legislationcode
      CLEAR land1.
      LOOP AT p0001 ASSIGNING FIELD-SYMBOL(<p0001>) WHERE pernr EQ <p0021>-pernr AND
                                                          begda LE <p0021>-endda AND
                                                          endda GE <p0021>-begda.
        READ TABLE land1_map INTO land1 WITH KEY name = <p0001>-bukrs.
        EXIT.
      ENDLOOP.

      DATA(eff_start_date) = zmhp_cl_mig_utils=>convert_date( <p0021>-begda ).
      DATA(eff_end_date) = zmhp_cl_mig_utils=>convert_date( <p0021>-endda ).

      DATA(count_str) = CONV string( count ).
      CONDENSE count_str.

      CONCATENATE per
                  cont
                  <p0021>-pernr
                  count_str
      INTO DATA(per_id) SEPARATED BY '_'.

      sys_id = 'SAP_' && sy-mandt.

      CONCATENATE per
                  cont
                  leg
                  <p0021>-pernr
                  count_str
      INTO src_id SEPARATED BY '_'.
      map_mig_values( EXPORTING p0021 = <p0021>
                      IMPORTING gesch = DATA(gesch_tmp) ).

      CONCATENATE zmhp_cl_mig_utils=>merge
                  contact_leg_data
                  eff_start_date
                  eff_end_date
                  per_id
                  land1-value
                  gesch_tmp
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
    ENDLOOP.
  ENDMETHOD.


  METHOD map_mig_values.
    DATA: value_tmp TYPE zmhp_dd_value.

    "Process FASEX/GESCH mapping
    value_tmp = CONV #( p0021-fasex ).
    zmhp_cl_int_mapping=>process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0002
        field_sap      = zmhp_cl_mig_person_leg_data=>gesch
        field_oracle   = sex
        mapping_fields = CONV #( mapping_fields_gesch )
        mapping_values = CONV #( mapping_values_gesch )
     CHANGING
       value           = value_tmp ).

    gesch              = value_tmp.


  ENDMETHOD.


  METHOD proceed_cofu_con_leg_data.

    get_cofu_data( ).
    get_mapping_fields( ).
    get_mapping_values( ).
    data =  map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
