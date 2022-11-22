*&---------------------------------------------------------------------*
*& Include          Z_COPIA_ROLES_TOP
*&---------------------------------------------------------------------*
TABLES: agr_define, zincltable.

CONTROLS tc_200 TYPE TABLEVIEW USING SCREEN 202.

TYPES: BEGIN OF ty_salida,
         estado    TYPE icon_d,
         rol       TYPE agr_name,
         rol_copia TYPE agr_name,
         sin_act   TYPE char1,
         sin_tcode TYPE char1,
         text      TYPE bapiret2-message,
       END OF ty_salida.

TYPES: BEGIN OF ty_tcode,
         rol  TYPE agr_name,
         code TYPE tcode,
         text TYPE bapiret2-message,
       END OF ty_tcode.

DATA:"  go_alv      TYPE REF TO cl_salv_table,
  go_selec  TYPE REF TO cl_salv_selections,
  go_selec2 TYPE REF TO cl_salv_selections.


DATA: gt_salida TYPE TABLE OF ty_salida,
      gs_salida LIKE LINE OF gt_salida.

DATA: gt_tcode TYPE TABLE OF ty_tcode,
      gs_tcode LIKE LINE OF gt_tcode.


"Data for output
DATA: gr_container TYPE REF TO cl_gui_docking_container.   "The carrier for the split container
DATA: lv_splitter TYPE REF TO cl_gui_splitter_container.  "The splitter itself
DATA: lv_parent1 TYPE REF TO cl_gui_container.           "parent 1 and 2
DATA: lv_parent2 TYPE REF TO cl_gui_container.

DATA ref_grid1 TYPE REF TO cl_gui_alv_grid.
DATA ref_grid2 TYPE REF TO cl_gui_alv_grid.
DATA: gr_table1 TYPE REF TO cl_salv_table.
DATA: gr_table2 TYPE REF TO cl_salv_table.


DATA zincltab TYPE STANDARD TABLE OF zincltable WITH HEADER LINE.      " includes to be renamed
DATA zincltab_act TYPE STANDARD TABLE OF zincltable WITH HEADER LINE.  " active source
DATA zincltab_temp TYPE STANDARD TABLE OF zincltable WITH HEADER LINE. " inactive source

DATA: ok_code                TYPE sy-ucomm.    " screen ok_code.

* list fields must be global
DATA:
  inputx      TYPE c LENGTH 1,
  loopc       LIKE sy-loopc,             " max. loop counter

  line_global LIKE sy-tabix.      "amount of rows per int. table

DATA: coltop TYPE p VALUE 5,
      rowtop TYPE p VALUE 5,
      ucomm  LIKE sy-ucomm.            " helpfield for exit
