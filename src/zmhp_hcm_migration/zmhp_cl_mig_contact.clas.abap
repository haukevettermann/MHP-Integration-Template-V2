CLASS zmhp_cl_mig_contact DEFINITION
*  PUBLIC
*  CREATE PUBLIC .
*
*  PUBLIC SECTION.
*
*    DATA p0021 TYPE p0021_tab .
*    DATA pernr TYPE rsdsselopt_t .
*    DATA begda TYPE begda .
*    DATA endda TYPE endda .
*    DATA cofu TYPE boolean .
*    DATA cogl TYPE boolean .
*    DATA cogu TYPE boolean .
*    DATA molga TYPE rsdsselopt_t .
*    DATA p0002 TYPE p0002_tab .
*    DATA vp_per_contact_structure TYPE /iwbep/t_mgw_name_value_pair .
*    CONSTANTS contact TYPE string VALUE 'Contact' ##NO_TEXT.
*    CONSTANTS per TYPE string VALUE 'PER' ##NO_TEXT.
*    CONSTANTS cont TYPE string VALUE 'CONT' ##NO_TEXT.
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
*    METHODS proceed_cofu_per_contact
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
*    METHODS get_cofu_data .
*    METHODS map_cofu_data
*      IMPORTING
*        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
*      RETURNING
*        VALUE(data) TYPE string .
*ENDCLASS.
*
*
*
*CLASS zmhp_cl_mig_contact IMPLEMENTATION.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Public Method ZMHP_CL_MIG_CONTACT->CONSTRUCTOR
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
*      vp_per_contact_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
*                                          ( name = 2  value =  contact )
*                                          ( name = 3  value = 'EffectiveStartDate' )
*                                          ( name = 4  value = 'EffectiveEndDate' )
*                                          ( name = 5  value = 'PersonNumber' )
*                                          ( name = 6  value = 'StartDate' )
*                                          ( name = 7  value = 'DateOfBirth' )
*                                          ( name = 8  value = 'SourceSystemOwner' )
*                                          ( name = 9  value = 'SourceSystemId' ) ).
*    ELSEIF cogl EQ abap_true.
*
*
*    ENDIF.
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Public Method ZMHP_CL_MIG_CONTACT->CREATE_METADATA
** +-------------------------------------------------------------------------------------------------+
** | [<-()] METADATA                       TYPE        STRING
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD create_metadata.
*
*    DESCRIBE TABLE vp_per_contact_structure LINES DATA(length).
*
*    LOOP AT vp_per_contact_structure ASSIGNING FIELD-SYMBOL(<person_contact_struc>).
*
*      "set METADATA title
*      CASE <person_contact_struc>-name.
*        WHEN 1.
*          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
*          CONTINUE.
*      ENDCASE.
*
*      CONCATENATE metadata <person_contact_struc>-value INTO metadata.
*
*      "set separator
*      CHECK length NE sy-tabix.
*      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
*
*    ENDLOOP.
*
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Private Method ZMHP_CL_MIG_CONTACT->GET_COFU_DATA
** +-------------------------------------------------------------------------------------------------+
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD get_cofu_data.
*
*    DATA: famsa TYPE rsdsselopt_t.
*    " Read infotype 0002
*    SELECT pernr,
*           begda,
*           endda,
*           fgbdt
*           INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
*                                                                   AND famsa IN @famsa
*                                                                   AND begda LE @endda
*                                                                   AND endda GE @begda.
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Private Method ZMHP_CL_MIG_CONTACT->MAP_COFU_DATA
** +-------------------------------------------------------------------------------------------------+
** | [--->] VP_SRC_ID                      TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
** | [<-()] DATA                           TYPE        STRING
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD map_cofu_data.
*
*    DATA: src_id TYPE string,
*          sys_id TYPE string.
*
*    LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>).
*      DATA(eff_start_date) = COND #( WHEN <p0002>-begda IS NOT INITIAL THEN  zmhp_cl_mig_utils=>convert_date( <p0002>-begda )
*                                     ELSE '' ).
*      DATA(start_date) = COND #( WHEN <p0002>-begda IS NOT INITIAL THEN  zmhp_cl_mig_utils=>convert_date( <p0002>-begda )
*                                 ELSE '' ).
*      DATA(date_of_birth) = COND #( WHEN <p0002>-gbdat IS NOT INITIAL THEN  zmhp_cl_mig_utils=>convert_date( <p0002>-gbdat )
*                                    ELSE '' ).
*
*      sys_id = 'SAP_' && sy-mandt.
*      CONCATENATE per
*                  cont
*                  <p0002>-pernr
*                  INTO src_id SEPARATED BY '_'. "PER_CONT_00200518
*
*      CONCATENATE per
*                  <p0002>-pernr
*                  INTO DATA(tmp_pernr).
*
*      CONCATENATE zmhp_cl_mig_utils=>merge
*                  contact
*                  eff_start_date
*                  ''
*                  tmp_pernr
*                  start_date
*                  date_of_birth
*                  sys_id
*                  src_id
*      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.
*
*      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.
*
*    ENDLOOP.
*
*
*  ENDMETHOD.
*
*
** <SIGNATURE>---------------------------------------------------------------------------------------+
** | Instance Public Method ZMHP_CL_MIG_CONTACT->PROCEED_COFU_PER_CONTACT
** +-------------------------------------------------------------------------------------------------+
** | [--->] VP_SRC_ID                      TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
** | [--->] WORKER                         TYPE REF TO ZMHP_CL_MIG_WORKER
** | [<-()] DATA                           TYPE        STRING
** +--------------------------------------------------------------------------------------</SIGNATURE>
*  METHOD proceed_cofu_per_contact.
*
*    get_cofu_data( ).
**    zmhp_cl_mig_utils=>update_begin_date( EXPORTING p0000 = worker->p0000
**                                           CHANGING p0002 = p0002 ).
*
*    data = map_cofu_data( vp_src_id ).
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
    DATA vp_per_contact_structure TYPE /iwbep/t_mgw_name_value_pair .
    CONSTANTS contact TYPE string VALUE 'Contact' ##NO_TEXT.
    CONSTANTS per TYPE string VALUE 'PER' ##NO_TEXT.
    CONSTANTS cont TYPE string VALUE 'CONT' ##NO_TEXT.

    METHODS constructor
      IMPORTING
        !pernr TYPE rsdsselopt_t
        !begda TYPE begda
        !endda TYPE endda
        !cofu  TYPE boolean
        !cogl  TYPE boolean
        !molga TYPE rsdsselopt_t
        !cogu  TYPE boolean .
    METHODS proceed_cofu_per_contact
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

    METHODS get_cofu_data .
    METHODS map_cofu_data
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
      RETURNING
        VALUE(data) TYPE string .
ENDCLASS.



CLASS ZMHP_CL_MIG_CONTACT IMPLEMENTATION.


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
      vp_per_contact_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                          ( name = 2  value =  contact )
                                          ( name = 3  value = 'EffectiveStartDate' )
                                          ( name = 4  value = 'EffectiveEndDate' )
                                          ( name = 5  value = 'PersonNumber' )
                                          ( name = 6  value = 'StartDate' )
                                          ( name = 7  value = 'DateOfBirth' )
                                          ( name = 8  value = 'SourceSystemOwner' )
                                          ( name = 9  value = 'SourceSystemId' ) ).
    ELSEIF cogl EQ abap_true.


    ENDIF.

  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_per_contact_structure LINES DATA(length).

    LOOP AT vp_per_contact_structure ASSIGNING FIELD-SYMBOL(<person_contact_struc>).

      "set METADATA title
      CASE <person_contact_struc>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <person_contact_struc>-value INTO metadata.

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


    "Read Infotype 0021
    SELECT pernr,
           begda,
           endda,
           fgbdt
    INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr AND
                                                                famsa IN @famsa AND
                                                                begda LE @endda AND
                                                                endda GE @begda.
  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id TYPE string,
          sys_id TYPE string.

    CHECK p0021 IS NOT INITIAL.
    SORT p0021 BY pernr ASCENDING begda ASCENDING.

    DATA(check_pernr) = p0021[ 1 ]-pernr.
    DATA(count) = 0.

*    SELECT * FROM pa0000 INTO TABLE @DATA(p0000_tab) WHERE pernr IN @pernr.
*    SORT p0000_tab ASCENDING BY begda.
*    DATA(earliest_begda) = p0000_tab[ 1 ]-begda.

    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

      IF <p0021>-pernr NE check_pernr.
        count = 1.
        check_pernr = <p0021>-pernr.
      ELSE.
        count = count + 1.
      ENDIF.


      DATA(eff_start_date) = COND #( WHEN <p0021>-begda IS NOT INITIAL THEN  zmhp_cl_mig_utils=>convert_date( <p0021>-begda )
                                     ELSE '' ).
      DATA(start_date) = COND #( WHEN <p0021>-begda IS NOT INITIAL THEN  zmhp_cl_mig_utils=>convert_date( <p0021>-begda )
                                 ELSE '' ).

      DATA(date_of_birth) = COND #( WHEN <p0021>-fgbdt IS NOT INITIAL THEN  zmhp_cl_mig_utils=>convert_date( <p0021>-fgbdt )
                                    ELSE '' ).

      DATA(eff_end_date) = COND #( WHEN <p0021>-endda IS NOT INITIAL THEN  zmhp_cl_mig_utils=>convert_date( <p0021>-endda )
                                     ELSE '' ).
      DATA(count_str) = CONV string( count ).
      CONDENSE count_str.

      sys_id = 'SAP_' && sy-mandt.

      CONCATENATE per
                  cont
                  <p0021>-pernr
                  count_str
                  INTO src_id SEPARATED BY '_'. "PER_CONT_00200518_1

      CONCATENATE per
                  <p0021>-erbnr
                  INTO DATA(tmp_pernr) SEPARATED BY '_'.

      IF <p0021>-erbnr IS INITIAL.
        CLEAR tmp_pernr.
      ENDIF.

      CONCATENATE zmhp_cl_mig_utils=>merge
                  contact
                  eff_start_date
                  eff_end_date
                  tmp_pernr
                  start_date
                  date_of_birth
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.

      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.

    ENDLOOP.


  ENDMETHOD.


  METHOD proceed_cofu_per_contact.

    get_cofu_data( ).
    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.
ENDCLASS.
