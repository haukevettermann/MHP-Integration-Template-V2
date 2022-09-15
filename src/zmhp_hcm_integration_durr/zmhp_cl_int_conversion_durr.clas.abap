class ZMHP_CL_INT_CONVERSION_DURR definition
  public
  inheriting from ZMHP_CL_INT_CONVERSION
  final
  create public .

public section.

  class-methods CONVERT_BUKRS
    importing
      !VALUE_IN type ZMHP_DD_VALUE
    returning
      value(VALUE_OUT) type ZMHP_DD_VALUE .
  class-methods CONVERT_PERSG
    importing
      !VALUE_IN type ZMHP_DD_VALUE
    returning
      value(VALUE_OUT) type ZMHP_DD_VALUE .
  class-methods CONVERT_PERSK
    importing
      !VALUE_IN type ZMHP_DD_VALUE
    returning
      value(VALUE_OUT) type ZMHP_DD_VALUE .
  class-methods KOPIERVORLAGE
    importing
      !VALUE_IN type ZMHP_DD_VALUE
    returning
      value(VALUE_OUT) type ZMHP_DD_VALUE .
protected section.
private section.

  class-data INSTANCE type ref to zmhp_cl_int_conversion_durr .
ENDCLASS.



CLASS ZMHP_CL_INT_CONVERSION_DURR IMPLEMENTATION.


  method CONVERT_BUKRS.
    value_out = value_in+3(3).
  endmethod.


  method CONVERT_PERSG.
    value_out = value_in+3(1).
  endmethod.


  method CONVERT_PERSK.
    value_out = value_in+5(2).
  endmethod.


  method KOPIERVORLAGE.
  endmethod.
ENDCLASS.
