*&---------------------------------------------------------------------*
*& Include          Z_CARGA_TCODES_FORMS
*&---------------------------------------------------------------------*

CONSTANTS: lc_crea      TYPE char4 VALUE 'Crea',
           lc_modif     TYPE char4 VALUE 'Modi',
           lc_visua     TYPE char3 VALUE 'Vis',
           lc_monitor   TYPE char7 VALUE 'Monitor',
           lc_monitor_m TYPE char7 VALUE 'MONITOR'.

TYPES: BEGIN OF ty_salida,
         estado    TYPE icon_d,
         tcode     TYPE tcode,
         tcode_vis TYPE tcode,
         text      TYPE bapiret2-message,
       END OF ty_salida.

DATA: lt_salida TYPE TABLE OF ty_salida,
      ls_salida LIKE LINE OF lt_salida.

DATA: lt_tstc_ori TYPE TABLE OF tstct,
      lt_tstc_vis TYPE TABLE OF tstct,
      lt_ZTSTC    TYPE TABLE OF ztstc,
      ls_tstc_ori LIKE LINE OF lt_tstc_ori,
      ls_tstc_vis LIKE LINE OF lt_tstc_vis,
      ls_ztstc    LIKE LINE OF lt_ztstc.


DATA: lv_index_from TYPE sy-tabix,
      lv_index_to   TYPE sy-tabix,
      lv_total_reg  TYPE i.

DATA: lv_tcode_aux TYPE tcode,
      lv_answer    TYPE c,
      lv_carga_ok  TYPE char1.
DATA: ls_ztstc_val TYPE ztstc.

DATA: lv_carga_inicial TYPE char1.


DATA: lv_porcentaje TYPE i,
      lv_lines type i.

* Verificamos si la tabla de destino ZTSTC
* esta rellena


SELECT * UP TO 1 ROWS FROM ztstc INTO ls_ztstc.
ENDSELECT.
IF sy-subrc NE 0.
  lv_carga_inicial = 'X'.
ENDIF.
*
*IF lv_carga_ok = 'X'.

* Cargamos la tabla con todos los TCODES
SELECT * INTO TABLE lt_tstc_ori FROM tstct
WHERE sprsl = 'S'.
*  order by tcode.

SORT lt_tstc_ori BY tcode.

DESCRIBE TABLE lt_tstc_ori LINES lv_total_reg.

IF sy-subrc = 0.

  lt_tstc_vis[] = lt_tstc_ori[].

  DESCRIBE TABLE lt_tstc_vis LINES lv_lines.

* Recorremos la tabla con todos los tcodes.
  LOOP AT lt_tstc_ori INTO ls_tstc_ori.

* Mostramos indicador de proceso

  lv_porcentaje = ( sy-tabix * 100 ) / lv_lines.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = lv_porcentaje
      text       = text-005.

* Nos quedamos con el TCODE
    IF ls_tstc_ori-tcode CS lc_monitor_m AND ls_ztstc-tcode(1) NE 'Z'.
      ls_ztstc-tcode = ls_tstc_ori-tcode.
      ls_ztstc-tcode_vis = ls_ztstc-tcode.
      SELECT SINGLE * FROM ztstc INTO ls_ztstc_val
             WHERE tcode = ls_ztstc-tcode.
      IF sy-subrc NE 0.
        APPEND ls_ztstc TO lt_ztstc.
        IF lv_carga_inicial IS INITIAL.
          ls_salida-tcode = ls_ztstc-tcode.
          ls_Salida-tcode_vis = ls_Ztstc-tcode_vis.
          ls_salida-estado = '@08@'.
          ls_salida-text = TEXT-003.
          APPEND ls_salida TO lt_salida.
        ENDIF.
      ENDIF.
      CLEAR ls_ztstc.
    ELSEIF ls_tstc_ori-ttext(3) NE lc_visua.
      ls_ztstc-tcode = ls_tstc_ori-tcode.
* Calculamos 5 abajo
      lv_index_from = sy-tabix - 10.
      IF lv_index_from < 0.
        lv_index_from = sy-tabix.
      ENDIF.
* Calculamos 5 arriba
      lv_index_to = sy-tabix + 10.
      IF lv_index_to > lv_total_reg.
        lv_index_to = lv_total_reg.
      ENDIF.
* Recorremos 15 arriba y 15 abajo buscando además que las primeras tres letras del TCODE de visualizacion sea igual al que estamos tratando
      LOOP AT lt_tstc_vis INTO ls_tstc_vis  FROM lv_index_from TO lv_index_to WHERE tcode(3) = ls_tstc_ori-tcode(3) .
        IF ls_tstc_vis-ttext(3) = lc_visua.
          CONCATENATE ls_tstc_vis-tcode 'N' INTO lv_tcode_aux.
          READ TABLE lt_tstc_vis TRANSPORTING NO FIELDS WITH KEY tcode = lv_tcode_aux.
          IF sy-subrc = 0.
            ls_ztstc-tcode_vis = lv_tcode_aux.  CLEAR lv_tcode_aux.
          ELSE.
            ls_ztstc-tcode_vis = ls_tstc_vis-tcode.
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
* Si hemos encontrado TCODE de visulalización (descartamos los que son iguales)
      IF ls_ztstc-tcode NE ls_ztstc-tcode_vis AND ls_ztstc-tcode_vis IS NOT INITIAL AND ls_ztstc-tcode(1) NE 'Z'.
*      insert ztstc from ls_ztstc.
* Validamos si ya se ha introducido ese TCODE en la tabla Z
        SELECT SINGLE * FROM ztstc INTO ls_ztstc_val
        WHERE tcode = ls_ztstc-tcode.
        IF sy-subrc NE 0.
          APPEND ls_ztstc TO lt_ztstc.
          IF lv_carga_inicial IS INITIAL.
            ls_salida-tcode = ls_ztstc-tcode.
            ls_Salida-tcode_vis = ls_Ztstc-tcode_vis.
            ls_salida-estado = '@08@'.
            ls_salida-text = TEXT-003.
            APPEND ls_salida TO lt_salida.
          ENDIF.
        ENDIF.

        CLEAR ls_ztstc.
      ENDIF.
    ENDIF.
  ENDLOOP.


  IF lt_ztstc[] IS NOT INITIAL.
    INSERT ztstc FROM TABLE lt_ZTSTC.
    COMMIT WORK AND WAIT.
    IF lv_carga_inicial IS NOT INITIAL.
      MESSAGE TEXT-001 TYPE 'S'.
    ELSE.
      PERFORM fotr_mostrar_alv.
    ENDIF.
  ELSE.
    MESSAGE TEXT-002 TYPE 'I'.
  ENDIF.
ENDIF.

* endif.
*&---------------------------------------------------------------------*
*& Form fotr_mostrar_alv
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fotr_mostrar_alv .

  DATA: go_alv   TYPE REF TO cl_salv_table.

  DATA : lo_header TYPE REF TO cl_salv_form_layout_grid.

  DATA: lr_column  TYPE REF TO cl_salv_column_table,
        lr_columns TYPE REF TO cl_salv_columns_table.

  DATA lv_text_S TYPE scrtext_s.
  DATA lv_text_m TYPE scrtext_m.
  DATA lv_text_l TYPE scrtext_l.

   IF lo_header IS NOT BOUND.
      CREATE OBJECT lo_header.
    ENDIF.

    lo_header->create_header_information( row = 1 column = 1 text = text-004 ).  "Report Header Text


  IF lt_salida[] IS NOT INITIAL.
*Instanciamos
    TRY.
        cl_salv_table=>factory(
          IMPORTING
            r_salv_table   =  go_alv   " Basis Class Simple ALV Tables
          CHANGING
            t_table        = lt_salida
        ).
      CATCH cx_salv_msg.    "

    ENDTRY.

*    go_alv->set_screen_status(
*    pfstatus      =  'SALV_STANDARD'
*    report        =  sy-repid
*    set_functions = go_Alv->c_functions_all ).


*    go_selec = go_alv->get_selections( ).
*    go_alv->get_functions( )->set_all( abap_true ).
*    go_selec->set_selection_mode( value = if_salv_c_selection_mode=>multiple ).

    lr_columns = go_alv->get_columns( ).
*    lr_columns->set_optimize( abap_true ).

    lv_text_S = lv_text_m = lv_text_l = TEXT-t01.

    TRY.
        lr_column ?= lr_columns->get_column( 'ESTADO' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lv_text_S = lv_text_m = lv_text_l = TEXT-t02.

    TRY.
        lr_column ?= lr_columns->get_column( 'TCODE_VIS' ).
        lr_column->SET_OUTPUT_LENGTH( 25   ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
*         lr_column->SET_optimized( ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lv_text_S = lv_text_m = lv_text_l = TEXT-t03.

    TRY.
        lr_column ?= lr_columns->get_column( 'TEXT' ).
        lr_column->SET_OUTPUT_LENGTH( 50   ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
*         lr_column->SET_optimized( ).
      CATCH cx_salv_not_found.
    ENDTRY.


    go_alv->set_top_of_list( lo_header ).

    go_alv->display( ).


  ENDIF.
ENDFORM.
