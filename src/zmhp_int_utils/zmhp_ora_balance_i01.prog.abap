*----------------------------------------------------------------------*
***INCLUDE ZMHP_ORA_BALANCE_I01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9000 INPUT.
  CASE sy-ucomm.
    WHEN OTHERS.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.
