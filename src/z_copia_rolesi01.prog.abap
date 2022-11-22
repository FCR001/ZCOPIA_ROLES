*----------------------------------------------------------------------*
***INCLUDE Z_COPIA_ROLESI01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  OK_CODE_0202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ok_code_0202 INPUT.
CASE ok_code.
    WHEN  'UPDA'.
      LEAVE TO SCREEN 0.
    WHEN  'PF21'.
      tc_200-top_line = 1.
    WHEN  'PF22'.
      tc_200-top_line = tc_200-current_line - loopc.
      IF tc_200-top_line <= 0.
        tc_200-top_line = 1.
      ENDIF.
    WHEN  'PF23'.
      DESCRIBE TABLE zincltab LINES line_global.
      tc_200-top_line = tc_200-current_line + loopc.
      IF tc_200-top_line GE line_global.
        tc_200-top_line = line_global - loopc.
      ENDIF.
    WHEN  'PF24'.
      DESCRIBE TABLE zincltab LINES line_global.
      tc_200-top_line = line_global - loopc.
  ENDCASE.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  EXIT_200  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE exit_200 INPUT.

*  MESSAGE s017 .
  ucomm = sy-ucomm.
  SET SCREEN 0.
  LEAVE SCREEN.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  MODIFY_INCLTAB_0202  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE modify_incltab_0202 INPUT.

   MODIFY zincltab_act FROM zincltable
         INDEX tc_200-current_line.

ENDMODULE.
