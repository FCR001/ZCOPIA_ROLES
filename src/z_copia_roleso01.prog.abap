*----------------------------------------------------------------------*
***INCLUDE Z_COPIA_ROLESO01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module TC_200_CHANGE_TC_ATTR OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE tc_200_change_tc_attr OUTPUT.
 CLEAR ok_code.
  DESCRIBE TABLE zincltab_act LINES tc_200-lines.
ENDMODULE.

MODULE tc_202_get_lines OUTPUT.
  loopc = sy-loopc.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module SET_STATUS OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE set_status OUTPUT.
 SET PF-STATUS 'INCLTAB'.
 SET TITLEBAR 'T02'.
ENDMODULE.