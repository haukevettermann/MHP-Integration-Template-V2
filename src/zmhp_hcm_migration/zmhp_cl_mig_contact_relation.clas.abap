CLASS zmhp_cl_mig_contact_relation DEFINITION
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
    DATA p0002 TYPE p0002_tab .
    DATA vp_con_relationship_structure TYPE /iwbep/t_mgw_name_value_pair .
    CONSTANTS contact_relationship TYPE string VALUE 'ContactRelationship' ##NO_TEXT.
    DATA p0021 TYPE p0021_tab .
    CONSTANTS per TYPE string VALUE 'PER' ##NO_TEXT.
    CONSTANTS cont TYPE string VALUE 'CONT' ##NO_TEXT.
    CONSTANTS rel TYPE string VALUE 'REL' ##NO_TEXT.
    CONSTANTS famsa TYPE zmhp_dd_field VALUE 'FAMSA' ##NO_TEXT.
    DATA mapping_fields_famsa TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapp_fi .
    DATA mapping_values_famsa TYPE zmhp_cl_mig_utils=>zmhp_tt_int_mapping .

    METHODS constructor
      IMPORTING
        !pernr TYPE rsdsselopt_t
        !begda TYPE begda
        !endda TYPE endda
        !cofu  TYPE boolean
        !cogl  TYPE boolean
        !molga TYPE rsdsselopt_t
        !cogu  TYPE boolean .
    METHODS proceed_cofu_con_relationship
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

    METHODS get_cofu_data .
    METHODS map_cofu_data
      IMPORTING
        !vp_src_id  TYPE /iwbep/t_mgw_name_value_pair
      RETURNING
        VALUE(data) TYPE string .
    METHODS update_begin_date
      IMPORTING
        !p0002 TYPE p0002_tab
      CHANGING
        !p0021 TYPE p0021_tab .
    METHODS get_mapping_cofu_fields .
    METHODS get_mapping_cofu_values .
    METHODS map_mig_values
      IMPORTING
        !p0021 TYPE p0021
      EXPORTING
        !famsa TYPE zmhp_dd_value .
ENDCLASS.



CLASS ZMHP_CL_MIG_CONTACT_RELATION IMPLEMENTATION.


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
      vp_con_relationship_structure = VALUE #( ( name = 1  value = zmhp_cl_mig_utils=>merge )
                                               ( name = 2  value = contact_relationship )
                                               ( name = 3  value = 'EffectiveStartDate' )
                                               ( name = 4  value = 'EffectiveEndDate' )
                                               ( name = 5  value = 'PersonId(SourceSystemId)' )
                                               ( name = 6  value = 'PersonNumber' )
                                               ( name = 7  value = 'RelatedPersonId(SourceSystemId)' )
                                               ( name = 8  value = 'RelatedPersonNumber' )
                                               ( name = 9  value = 'ContactType' )
                                               ( name = 10 value = 'StatutoryDependent' )
                                               ( name = 11 value = 'EmergencyContactFlag' )
                                               ( name = 12 value = 'ExistingPerson' )
                                               ( name = 13 value = 'PersonalFlag' )
                                               ( name = 14 value = 'PrimaryContactFlag' )
                                               ( name = 15 value = 'SequenceNumber' )
                                               ( name = 16 value = 'SourceSystemOwner' )
                                               ( name = 17 value = 'SourceSystemId' ) ).
    ELSEIF cogl EQ abap_true.

    ENDIF.

  ENDMETHOD.


  METHOD create_metadata.

    DESCRIBE TABLE vp_con_relationship_structure LINES DATA(length).

    LOOP AT vp_con_relationship_structure ASSIGNING FIELD-SYMBOL(<contact_relation_data>).

      "set METADATA title
      CASE <contact_relation_data>-name.
        WHEN 1.
          CONCATENATE zmhp_cl_mig_utils=>metadata zmhp_cl_mig_utils=>separator INTO metadata.
          CONTINUE.
      ENDCASE.

      CONCATENATE metadata <contact_relation_data>-value INTO metadata.

      "set separator
      CHECK length NE sy-tabix.
      CONCATENATE metadata zmhp_cl_mig_utils=>separator INTO metadata.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_cofu_data.
    DATA: famsa TYPE rsdsselopt_t.

    famsa = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '1' )
                                ( sign = 'I' option = 'EQ' low = '13' )
                                ( sign = 'I' option = 'EQ' low = '2' )
                                ( sign = 'I' option = 'EQ' low = '6' )
                                ( sign = 'I' option = 'EQ' low = '11' )
                                ( sign = 'I' option = 'EQ' low = '12' )
                                ( sign = 'I' option = 'EQ' low = '14' ) ).

    " Read Infotype 0002
    SELECT pernr,
           begda,
           endda
    INTO CORRESPONDING FIELDS OF TABLE @p0002 FROM pa0002 WHERE pernr IN @pernr
                                                            AND begda LE @endda
                                                            AND endda GE @begda.


    " Read Infotype 0021
    SELECT pernr,
           begda,
           endda,
           famsa,
           seqnr,
           emrgn,
           subty,
           objps,
           fgbdt
*           zz_telnr,
*           zz_telnr2,
*           zz_art
    INTO CORRESPONDING FIELDS OF TABLE @p0021 FROM pa0021 WHERE pernr IN @pernr
                                                            AND famsa IN @famsa
                                                            AND begda LE @endda
                                                            AND endda GE @begda.

  ENDMETHOD.


  METHOD get_mapping_cofu_fields.


    " get mapping fields for ContactRelation
    zmhp_cl_mig_utils=>get_mapping_fields( EXPORTING molga          = molga
                                                     infty          = zmhp_cl_mig_utils=>it0021
                                                     sap_field      = zmhp_cl_mig_contact_relation=>famsa
                                                     oracle_field   = zmhp_cl_mig_utils=>contact_type
                                                     export         = abap_true
                                           IMPORTING mapping_fields = mapping_fields_famsa ).


  ENDMETHOD.


  METHOD get_mapping_cofu_values.

    zmhp_cl_mig_utils=>get_mapping_values( EXPORTING molga          = molga
                                                     infty          = zmhp_cl_mig_utils=>it0021
                                                     sap_field      = zmhp_cl_mig_contact_relation=>famsa
                                                     oracle_field   = zmhp_cl_mig_utils=>contact_type
                                                     export         = abap_true
                                           IMPORTING mapping_values = mapping_values_famsa ).


  ENDMETHOD.


  METHOD map_cofu_data.

    DATA: src_id    TYPE string,
          sys_id    TYPE string,
          tmp_pernr TYPE pernr_d,
          count     TYPE i.


*    DATA(tmp_pernr) = p0021[ 1 ]-pernr.
*    DATA(count) = 0.
    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).

      IF <p0021>-pernr NE tmp_pernr OR tmp_pernr IS INITIAL. " multiple entries for one pernr possible, reset counter if pernr changes
        count = 0.
      ELSE.
        count = count + 1.
      ENDIF.

      DATA(eff_start_date) = zmhp_cl_mig_utils=>convert_date( <p0021>-begda ).
      DATA(eff_end_date) = zmhp_cl_mig_utils=>convert_date( <p0021>-endda ).
*      DATA(count_str) = COND string( WHEN count EQ 0 THEN ''
*                                     ELSE CONV string( count ) ).

      CONCATENATE per
                  cont
                  <p0021>-pernr
*                  count_str
      INTO DATA(per_id) SEPARATED BY '_'.

      DATA(rel_per_id) = per && '_' && <p0021>-pernr.

      DATA(emergency_contact) = COND #( WHEN <p0021>-emrgn IS NOT INITIAL THEN 'Y'
                                        ELSE 'N' ).

      DATA(existing_person) = 'Y'. " TODO

      map_mig_values( EXPORTING p0021 = <p0021>
                      IMPORTING famsa  = DATA(contact_type) ).


      sys_id = 'SAP_' && sy-mandt.
      CONCATENATE per
                  cont
                  rel
                  <p0021>-pernr
      INTO src_id SEPARATED BY '_'.


      CONCATENATE zmhp_cl_mig_utils=>merge
                  contact_relationship
                  eff_start_date
                  eff_end_date                      " EffectiveEndDate
                  per_id
                  ''                      "
                  rel_per_id
                  ''                      " RelatedPersonNumber
                  contact_type            " Contact Type
                  ''                      " StatuoryDependent
                  emergency_contact
                  existing_person
                  'Y'
                  'N'
                  <p0021>-seqnr " TODO or count?
                  sys_id
                  src_id
      INTO DATA(data_tmp) SEPARATED BY zmhp_cl_mig_utils=>separator.


      CONCATENATE data cl_abap_char_utilities=>newline data_tmp INTO data.


*      tmp_pernr = <p0021>-pernr.
    ENDLOOP.

  ENDMETHOD.


  METHOD map_mig_values.

    DATA: value_tmp TYPE zmhp_dd_value.

    "Instantiate mapping class
    DATA(mapping_handler) = NEW zmhp_cl_int_mapping( ).

    " Process FAMSA mapping
    value_tmp = CONV #( p0021-famsa ).
    mapping_handler->process_mapping(
      EXPORTING
        import         = abap_false
        export         = abap_true
        infty          = zmhp_cl_mig_utils=>it0021
        field_sap      = zmhp_cl_mig_contact_relation=>famsa
        field_oracle   = zmhp_cl_mig_utils=>contact_type
        mapping_fields = CONV #( mapping_fields_famsa )
        mapping_values = CONV #( mapping_values_famsa )
      CHANGING
        value          = value_tmp ).

    famsa = value_tmp.



  ENDMETHOD.


  METHOD proceed_cofu_con_relationship.

    get_cofu_data( ).
    update_begin_date( EXPORTING p0002 = p0002
                       CHANGING  p0021 = p0021 ).
    get_mapping_cofu_fields( ).
    get_mapping_cofu_values( ).

    data = map_cofu_data( vp_src_id ).

  ENDMETHOD.


  METHOD update_begin_date.

    LOOP AT p0021 ASSIGNING FIELD-SYMBOL(<p0021>).
      LOOP AT p0002 ASSIGNING FIELD-SYMBOL(<p0002>) WHERE pernr EQ <p0021>-pernr
                                                      AND begda LE endda
                                                      AND endda GE begda.
        EXIT.
      ENDLOOP.
      CHECK <p0002> IS ASSIGNED.

      <p0021>-begda = <p0002>-begda.
      UNASSIGN <p0002>.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
