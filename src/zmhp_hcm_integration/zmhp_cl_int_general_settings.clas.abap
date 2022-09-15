class ZMHP_CL_INT_GENERAL_SETTINGS definition
  public
  final
  create public .

public section.

  class-methods GET_ACTIONHANDLER
    returning
      value(VALUE) type STRING .
  class-methods GET_AL11_ARCHIVE
    returning
      value(VALUE) type STRING .
  class-methods GET_AL11_DOWN
    returning
      value(VALUE) type STRING .
  class-methods GET_AL11_UP
    returning
      value(VALUE) type STRING .
  class-methods GET_CONVERSION_HANDLER
    returning
      value(VALUE) type STRING .
  class-methods GET_INFOTYPHANDLER
    returning
      value(VALUE) type STRING .
  class-methods GET_MAPPINGHANDLER
    returning
      value(VALUE) type STRING .
  class-methods GET_XPATH_BEGDA
    returning
      value(VALUE) type STRING .
  class-methods GET_XPATH_ENDDA
    returning
      value(VALUE) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZMHP_CL_INT_GENERAL_SETTINGS IMPLEMENTATION.


  METHOD get_actionhandler.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '05' INTO @value.
    IF sy-subrc IS NOT INITIAL.
      value = 'ZMHP_CL_INT_ACTION_HANDLER'.
    ENDIF.
  ENDMETHOD.


  method GET_AL11_ARCHIVE.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '03' INTO @value.
  endmethod.


  METHOD GET_AL11_DOWN.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '01' INTO @value.
  ENDMETHOD.


  method GET_AL11_UP.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '02' INTO @value.
  endmethod.


  METHOD GET_CONVERSION_HANDLER.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '07' INTO @value.
    IF sy-subrc IS NOT INITIAL.
      value = 'ZMHP_CL_INT_CONVERSION'.
    ENDIF.
  ENDMETHOD.


  METHOD get_infotyphandler.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '04' INTO @value.
    IF sy-subrc IS NOT INITIAL.
      value = 'ZMHP_CL_INT_INFTY_HANDLER'.
    ENDIF.
  ENDMETHOD.


  METHOD GET_MAPPINGHANDLER.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '06' INTO @value.
    IF sy-subrc IS NOT INITIAL.
      value = 'ZMHP_CL_INT_MAPPING'.
    ENDIF.
  ENDMETHOD.


  METHOD GET_XPATH_BEGDA.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '08' INTO @value.
    IF sy-subrc IS NOT INITIAL.
      value = '/StartDate'.
    ENDIF.
  ENDMETHOD.


  METHOD GET_XPATH_ENDDA.
    SELECT SINGLE path FROM zmhp_int_general WHERE field = '09' INTO @value.
    IF sy-subrc IS NOT INITIAL.
      value = '/EndDate'.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
