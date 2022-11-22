*&---------------------------------------------------------------------*
*& Include          Z_COPIA_ROLES_SEL
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
select-options: so_user for SY-UNAME no INTERVALS DEFAULT sy-uname.
SELECT-OPTIONS: so_rol  for AGR_DEFINE-AGR_NAME.
parameters: pa_sufix type char4 DEFAULT '_CPY'.
*PARAMETERS: pa_trans type char1 AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b1.
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-019.
PARAMETERS: ck_rfc AS CHECKBOX USER-COMMAND RFC.
parameters: pa_rfc type RFCDEST MODIF ID DST MATCHCODE OBJECT F4_RFCDES3.
*parameters: pa_rfc type BDBAPIDST MODIF ID DST.
SELECTION-SCREEN END OF BLOCK b2.
