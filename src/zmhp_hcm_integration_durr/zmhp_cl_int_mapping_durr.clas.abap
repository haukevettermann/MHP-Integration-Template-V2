class ZMHP_CL_INT_MAPPING_DURR definition
  public
  inheriting from ZMHP_CL_INT_MAPPING
  final
  create public .

public section.

  types:
    BEGIN OF t_mapping_fieldlist_t.
    TYPES field TYPE zmhp_dd_value.
    TYPES END OF t_mapping_fieldlist_t .
  types:
    BEGIN OF t_mapping_valuelist_t.
    TYPES field TYPE zmhp_dd_value.
    TYPES value TYPE zmhp_dd_value.
    TYPES END OF t_mapping_valuelist_t .

  class-methods KOPIERVORLAGE
    importing
      !PERNR type PERNR_D optional
      !OBJID type HROBJID optional
      !BEGDA type DATS optional
      !ENDDA type DATS optional
      !INFTY type INFTY optional
      !SEQNR type SEQNR default 000
      !FIELD_SAP type ZMHP_DD_FIELD optional
      !FIELD_ORACLE type ZMHP_DD_FIELD optional
      !IMPORT type BOOLE_D
      value(EXPORT) type BOOLE_D
      !FIELDS type ZMHP_CL_INT_INFTY_PROC_XML=>T_FIELDS
    exporting
      value(VALUE_OUT) type ZMHP_DD_VALUE
      !MESSAGE type BAPIRET1
      !DO_SIMPLE type BOOLE_D .

  methods GET_ENAME
    redefinition .
  methods GET_STAT2
    redefinition .
  methods GET_USRID
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZMHP_CL_INT_MAPPING_DURR IMPLEMENTATION.


  METHOD get_ename.
    DATA(firstname) = VALUE #( fields[ field_sap = 'ENAME' ]-value OPTIONAL ).
    DATA(lastname) = VALUE #( fields[ field_sap = 'ENAME_LASTNAME'  ]-value OPTIONAL ).
    DATA(title) = VALUE #( fields[ field_sap = 'ENAME_TITLE'  ]-value OPTIONAL ).
    value_out = title && | | && firstname && | | && lastname.
  ENDMETHOD.


  method GET_STAT2.
**JMB20210712 start insert - differentiate between import and export logic
*
    IF import EQ abap_true.
      CASE infty.
        WHEN zmhp_cl_int_constants=>it0000.
          IF seqnr = 001.

            READ TABLE fields WITH KEY field_oracle = zmhp_cl_int_constants=>fields-ass_stat ASSIGNING FIELD-SYMBOL(<ass_stat>).
            READ TABLE fields WITH KEY field_oracle = zmhp_cl_int_constants=>fields-action_code ASSIGNING FIELD-SYMBOL(<action_code>).
            IF <action_code>-value NE 'HIRE'.
              IF <ass_stat>-value = 'SUSPEND_PROCESS'.
                value_out = '1'.
              ELSE.
                do_simple = abap_true.
              ENDIF.
            ELSE.
              do_simple = abap_true.
            ENDIF.
          ENDIF.
      ENDCASE.
    ELSEIF export EQ abap_true.

      "differentiate between SAP mandants
      CASE sy-mandt.

        WHEN zmhp_cl_int_constants=>cofu_mandant-germany. "DE
          READ TABLE fields WITH KEY field_sap = zmhp_cl_int_constants=>kostl  ASSIGNING FIELD-SYMBOL(<kostl>).
          READ TABLE fields WITH KEY field_sap = zmhp_cl_mig_assignment=>stat2 ASSIGNING FIELD-SYMBOL(<stat2>).
          READ TABLE fields WITH KEY field_sap = 'STAT1'                       ASSIGNING FIELD-SYMBOL(<stat1>).
          DATA(kostl_inactive) = VALUE rsdsselopt_t( ( sign = 'I' option = 'EQ' low = '0019990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0029990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0039990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0049990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0059990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0069990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0079990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0089990000' )
                                                     ( sign = 'I' option = 'EQ' low = '0099990000' ) ).

          "check employee status
          CASE <stat2>-value.
            WHEN '0' OR '2'.
              value_out = zmhp_cl_int_constants=>oracle_assign_status_code-inactive_process.
            WHEN '1'.
              value_out = zmhp_cl_int_constants=>oracle_assign_status_code-suspend_process.
            WHEN '3'.
              value_out = zmhp_cl_int_constants=>oracle_assign_status_code-active_process.
          ENDCASE.

**JMB20210901 start insert - in case of ATZ II pass suspend process C400129651-5611
*
          CASE <stat1>-value.
            WHEN '2'.
              value_out = zmhp_cl_int_constants=>oracle_assign_status_code-suspend_process.
          ENDCASE.
*JMB20210901 end insert

          "JMB20210914 I - check inactive cost center (C400129651-5610)
          CHECK <kostl>-value IN kostl_inactive.
          value_out = zmhp_cl_int_constants=>oracle_assign_status_code-suspend_process.

      ENDCASE.

    ENDIF.
*JMB20210712 insert end
  endmethod.


  METHOD get_usrid.
    IF export = abap_true.

    ELSEIF import = abap_true.
      READ TABLE fields INTO DATA(field_number) WITH KEY field_sap = field_sap.
      READ TABLE fields INTO DATA(field_ext) WITH KEY field_sap = 'USRID_EXT'.
      READ TABLE fields INTO DATA(field_acode) WITH KEY field_sap = 'USRID_ACODE'.
      READ TABLE fields INTO DATA(field_ccode) WITH KEY field_sap = 'USRID_CCODE'.
      IF seqnr = 005.
        value_out = field_number.
      ELSEIF seqnr = 002.
        DATA: list   TYPE TABLE OF abaplist,
              userid TYPE sysid.
        READ TABLE fields INTO DATA(field_kostl) WITH KEY field_sap = 'USRID_KOSTL'.
        SPLIT field_number-value AT '.' INTO DATA(vorna) DATA(nachn).
        "Logic for username creation needs to be added
        SUBMIT zhr_userid_pruefen WITH p_vorna = vorna
                                  WITH p_nachn = nachn
                                  WITH p_batch = abap_true
                                  WITH p_kostl = field_kostl-value AND RETURN.
        IMPORT ev_userid_int TO userid FROM MEMORY ID 'INT_USER'.
        value_out = userid.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD KOPIERVORLAGE.
    IF 1 = 0.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
