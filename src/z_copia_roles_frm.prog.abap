*----------------------------------------------------------------------*
***INCLUDE Z_COPIA_ROLES_FRM.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form copiar_rol
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM fabd_copiar_rol TABLES t_salida STRUCTURE gs_salida
                           t_tcode STRUCTURE gs_tcode.


*  TYPES: BEGIN OF ty_rol,
*           agr_name TYPE agr_name,
*         END OF ty_rol.

  DATA: ls_salida LIKE LINE OF t_salida.
  DATA: ls_tcode LIKE LINE OF t_tcode.
  DATA: lt_rol      TYPE TABLE OF ztl_tiporol,
        ls_rol      LIKE LINE OF lt_Rol,
        lt_messages TYPE sprot_u_tab.

  DATA: lt_act      TYPE TABLE OF tvordata,
        ls_act      LIKE LINE OF lt_act,
        lt_values2  TYPE STANDARD TABLE OF tpr01,
        lt_values_f TYPE STANDARD TABLE OF tpr01.

  DATA: lt_values TYPE TABLE OF pt1251,
        ls_values LIKE LINE OF lt_Values.

  DATA: lt_1250   TYPE TABLE OF pt1250,
        lt_1251   TYPE TABLE OF pt1251,
        ls_1251   LIKE LINE OF lt_1251,
        lt_1252   TYPE TABLE OF pt1252,
        lt_return TYPE TABLE OF bapiret2.

  DATA: lv_count         TYPE i,
        lv_control_actv  TYPE char1,
        lv_control_tcode TYPE char1,
        lv_con_tcode     TYPE char1,
        lv_tabix         TYPE sy-tabix,
        lv_agr_name      TYPE agr_name.

  DATA: lt_usobt      TYPE TABLE OF usobt,
        ls_usobt      LIKE LINE OF lt_usobt,
        lv_retc       TYPE char01,
        ls_ztstc      TYPE ztstc,
        ls_zincltable TYPE zincltable,
        lv_parent     TYPE  par_agr,
        lt_usuario    TYPE ztt_user,
        ls_usuario    LIKE LINE OF lt_usuario.

  DATA: lv_dest   TYPE bdbapidst,
        lv_answer TYPE c.

  DATA: lt_tcodes TYPE TABLE OF tstc,
        ls_tcodes LIKE LINE OF lt_tcodes.


  RANGES: ra_tcodes FOR ztstc-tcode.


* Evaluamos si se introduce usuario o un rol individiual.

  IF so_user IS NOT INITIAL.

    IF ck_rfc IS INITIAL.
* Si no está marcado el check de RFC, se buscan los roles por usuario en el sistema local
      SELECT agr_name FROM agr_users
      INTO TABLE lt_rol
      WHERE  agr_name IN so_rol AND
             uname IN so_user.                        "#EC CI_SGLSELECT

    ELSE.

* Miramos si el sistema destino está activo
      CALL FUNCTION 'RFC_PING' DESTINATION pa_rfc.
      IF sy-subrc = 0.
*Cargamos la tabla con los usuario de la pantalla de selección
* para llamar a la función remota
        LOOP AT so_user.
          ls_usuario-uname = so_user-low.
          APPEND ls_usuario TO lt_usuario.
        ENDLOOP.
* Llamamos a la función remota para traer los roles de producción
        CALL FUNCTION 'Z_GET_ROL_FROM_USER' DESTINATION pa_rfc
          EXPORTING
            it_usuario = lt_usuario
          IMPORTING
            t_roles    = lt_rol.

        IF lt_rol IS INITIAL.
* Si no encuentra datos: Pantalla pop up para pedir buscar datos en local.
          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar       = TEXT-021
*             DIAGNOSE_OBJECT             = ' '
              text_question  = TEXT-022
              text_button_1  = TEXT-023
*             ICON_BUTTON_1  = ' '
              text_button_2  = TEXT-024
            IMPORTING
              answer         = lv_answer
            EXCEPTIONS
              text_not_found = 1
              OTHERS         = 2.
          IF sy-subrc = 0.

          ENDIF.

        ENDIF.

      ELSE.
* Si el ping al sistema remoto falla:
* Pantalla pop up para pedir buscar datos en local.
        CALL FUNCTION 'POPUP_TO_CONFIRM'
          EXPORTING
            titlebar       = TEXT-020
*           DIAGNOSE_OBJECT             = ' '
            text_question  = TEXT-022
            text_button_1  = TEXT-023
*           ICON_BUTTON_1  = ' '
            text_button_2  = TEXT-024
          IMPORTING
            answer         = lv_answer
          EXCEPTIONS
            text_not_found = 1
            OTHERS         = 2.
        IF sy-subrc = 0.

        ENDIF.
      ENDIF.
    ENDIF.
  ELSE.
* Si se ha introducido un rol individual en la pantalla de selección
    SELECT agr_name FROM agr_define
  INTO TABLE lt_rol
  WHERE agr_name IN so_rol.                           "#EC CI_SGLSELECT

  ENDIF.
* No continuamos si no se han obtenido datos.
  IF lt_rol[] IS NOT INITIAL.
* ORdenamos y borramos duplicados.
    SORT lt_rol BY agr_name.
    DELETE ADJACENT DUPLICATES FROM lt_rol.
* Pantalla de confirmación de nombres.
    PERFORM carga_tabla_conf USING lt_rol.
    CALL SCREEN 202 STARTING AT coltop rowtop.
* Comprobamos que se pulsa el botón COPIAR de la pantalla de confirmación
    IF ok_code = 'UPDA'.
* Recorremos los roles introducidos en la pantalla de selección
      LOOP AT zincltab_act INTO ls_zincltable.

        MOVE ls_zincltable-name TO ls_salida-rol.
        MOVE ls_zincltable-new_name TO ls_salida-rol_copia.
        IF ls_Salida-rol_copia IS NOT INITIAL. "si se borra el Rol Copia de la pantalla de confirmación
          " se omite ese rol para la copia.
* Validamos que el rol original tenga ACT o T_CODE
          SELECT agr_name UP TO 1 ROWS FROM agr_1251
          INTO lv_agr_name
          WHERE agr_name = ls_salida-rol AND
               ( field = 'ACTV' OR field = 'TCD' ).     "#EC CI_NOORDER
          ENDSELECT.
          IF sy-subrc NE 0.
            CLEAR ls_salida-rol_copia.
            ls_salida-text = TEXT-011.
            ls_salida-estado = '@0A@'.
            APPEND ls_salida TO t_salida.
            CONTINUE.
          ENDIF.

* VAlidamos que no exista como rol_padre.ç
          SELECT parent_agr UP TO 1 ROWS FROM agr_define
          INTO lv_parent
          WHERE parent_agr = ls_salida-rol.
          ENDSELECT.
          IF sy-subrc = 0.
            CLEAR ls_salida-rol_copia.
            ls_salida-text = TEXT-018.
            ls_salida-estado = '@0A@'.
            APPEND ls_salida TO t_salida.
            CONTINUE.
          ENDIF.


* Bloqueamos el nuevo rol
          CALL FUNCTION 'PRGN_ACTIVITY_GROUP_ENQUEUE'
            EXPORTING
              activity_group          = ls_salida-rol_copia
*             COLL_ROLE               =
*             INH_ROLE                =
*             CHK_SETTING             = 'X'
*             NO_DIALOG               =
*             REQUEST                 =
* IMPORTING
*             NEW_REQUEST             =
* TABLES
*             ENQ_RESULT              =
*             RETURN                  =
            EXCEPTIONS
              foreign_lock            = 1
              system_error            = 2
              transport_check_problem = 3
              OTHERS                  = 4.
          IF sy-subrc <> 0.
* Implement suitable error handling here
          ENDIF.

* Mostramos indicador de proceso
          PERFORM progress_indicator USING ls_salida-rol_copia.
* Esta función hace una copia exacta de los roles
          CALL FUNCTION 'PRGN_COPY_AGR'
            EXPORTING
              source_agr                     = ls_salida-rol
              target_agr                     = ls_salida-rol_copia
**       SELECTIVE_COPY                 = ' '
*             tcodes_inherit                 = 'X'
              hr_assignments                 = ' '
              user_assinement                = ' '
*             pers_objects                   = 'X'
*             with_distribution              = 'X'
*             no_distrib_popup               = ' '
*             distribute_single_role         = ' '
*             dialog                         = ' '
              display_messages               = ' '
*             REQUEST                        =
            IMPORTING
              messages                       = lt_messages
* TABLES
*             SELECTED_OBJECTS               =
            EXCEPTIONS
              no_recording                   = 1
              target_agrname_not_free        = 2
              source_agr_not_exists          = 3
              no_authority_for_creation      = 4
              no_authority_for_user_insert   = 5
              no_authority_for_tcodes_insert = 6
              no_authority_for_object_insert = 7
              no_authority_for_srole_insert  = 8
              no_authority_for_srole_show    = 9
              flag_not_existing              = 10
              action_cancelled               = 11
              no_auth_for_objects_and_users  = 12
              no_auth_for_sroles_and_users   = 13
              enqueue_failure                = 14
              hr_incomplete                  = 15
              dist_incomplete                = 16
              OTHERS                         = 17.
          IF lt_messages IS INITIAL OR sy-subrc NE 0.
* Cuando la tabla de mensajes vienen vacia es que no se ha copiado el rol
*      ls_salida-text = TEXT-002.
* Recogemos el mensaje de la excepción
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_salida-text  .
            ls_salida-estado = '@0A@'.
            CLEAR ls_salida-rol_copia.
            APPEND ls_salida TO t_salida.
            CONTINUE.
          ENDIF.

* Estas funciones hacen el commit del cambio

          CALL FUNCTION 'PRGN_UPDATE_DATABASE'
            EXCEPTIONS
              OTHERS = 1.

          CALL FUNCTION 'PRGN_CLEAR_BUFFER'
            EXCEPTIONS
              OTHERS = 1.

** Funcion para activar los roles

          CALL FUNCTION 'SUPRN_PROFILE_BATCH'
            EXPORTING
              act_objid        = ls_salida-rol_copia
*             no_usr_upd       = 'X'
*             ENQUEUE          = 'X'
            EXCEPTIONS
              objid_not_found  = 1
              no_authorization = 2
              enqueue_failed   = 3
              not_generated    = 4
              OTHERS           = 5.

          IF sy-subrc = 0.
* Esta función lee todos los valores de la tabla T1251 con TCODES y Actividades
            CALL FUNCTION 'PRGN_1251_READ_FIELD_VALUES'
              EXPORTING
                activity_group    = ls_salida-rol_copia
              TABLES
                field_values      = lt_1251
              EXCEPTIONS
                no_data_available = 1
                OTHERS            = 2.
            IF sy-subrc <> 0.
*       Implement suitable error handling here
            ENDIF.


* Editamos las actividades y los TCODES, solo dejaremos 03 - Visualizar
* F4 - Visualizar ayudas
            LOOP AT lt_1251 INTO ls_1251.
              lv_tabix = sy-tabix.
              IF ls_1251-object = 'S_TCODE'." AND lv_control_tcode IS INITIAL.

                IF ls_1251-high IS INITIAL.
                  SELECT * INTO TABLE lt_usobt
                  FROM usobt
                  WHERE name = ls_1251-low.
                  LOOP AT lt_usobt INTO ls_usobt.
                    READ TABLE lt_1251 TRANSPORTING NO FIELDS WITH KEY ls_usobt-object.
                    IF sy-subrc = 0.
                      lv_control_tcode = 'X'.
                      EXIT.
                    ENDIF.
                  ENDLOOP.
                  IF lv_control_tcode IS INITIAL.
* Si la transacción no se controla por actividad, buscamos en nuestra tabla
* Z (se carga con Z_CARGA_TCODES) si hay una transacción equivalente de
* visualización.
                    SELECT SINGLE * INTO ls_ztstc FROM ztstc
                           WHERE tcode = ls_1251-low.
                    "                      and actua = 'X'.
                    IF sy-subrc = 0.
                      ls_tcode-rol = ls_salida-rol_copia.
                      ls_tcode-code = ls_ztstc-tcode_vis.
                      ls_1251-low = ls_ztstc-tcode_vis.
                      ls_tcode-text = TEXT-026.
                      APPEND ls_tcode TO t_tcode.
* Modificamos la línea correspondiente de la 1251 con el tcode de visualización.
                      MODIFY lt_1251 FROM ls_1251 INDEX lv_tabix.
                      lv_con_tcode = 'X'.
                    ELSE.
                      ls_tcode-rol = ls_salida-rol_copia.
                      ls_tcode-code = ls_1251-low.
                      ls_tcode-text = TEXT-012.
                      APPEND ls_tcode TO t_tcode.
                      DELETE lt_1251 INDEX lv_tabix.
                    ENDIF.
                  ELSE.
* Se ha encontrado que el TCODE se controla por actividad.
*                    ls_tcode-rol = ls_salida-rol_copia.
*                    ls_tcode-code = ls_1251-low.
*                    ls_tcode-text = TEXT-016.

                    SELECT SINGLE * INTO ls_ztstc FROM ztstc
                    WHERE tcode = ls_1251-low.

                    IF sy-subrc = 0.
*          Cambiamos TCDE
                     ls_tcode-rol = ls_salida-rol_copia.
                      ls_tcode-code = ls_ztstc-tcode_vis.
                      ls_1251-low = ls_ztstc-tcode_vis.
                      ls_tcode-text = TEXT-026.
                      APPEND ls_tcode TO t_tcode.
* Modificamos la línea correspondiente de la 1251 con el tcode de visualización.
                      MODIFY lt_1251 FROM ls_1251 INDEX lv_tabix.
                      lv_con_tcode = 'X'.
                      CLEAR lv_control_tcode.
                    ELSE.
                      ls_tcode-rol = ls_salida-rol_copia.
                      ls_tcode-code = ls_1251-low.
                      ls_tcode-text = TEXT-028.
                      APPEND ls_tcode TO t_tcode.
*                      DELETE lt_1251 INDEX lv_tabix.
                    ENDIF.
                  ENDIF.

                ELSE.
* Necesitamos evaluar el LOW y el HIGH

                  IF ls_1251-high IS NOT INITIAL.
                    ra_tcodes-option = 'BT'.
                    ra_tcodes-sign = 'I'.
                    ra_tcodes-low = ls_1251-low.
                    ra_tcodes-high = ls_1251-high.
                  ELSE.
                    ra_tcodes-option = 'EQ'.
                    ra_tcodes-sign = 'I'.
                    ra_tcodes-low = ls_1251-low.
                  ENDIF.

* Conseguimos todos los TCODES incluidos en el rango.
                  SELECT * FROM tstc INTO TABLE lt_tcodes
                  WHERE tcode IN ra_tcodes.
                  IF sy-subrc = 0.
* Vamos a sustituir el listado de TCODES obtenido por valores individuales.
                    LOOP AT lt_tcodes INTO ls_tcodes.

                      SELECT * INTO TABLE lt_usobt
                        FROM usobt
                        WHERE name = ls_tcodes-tcode.
                      LOOP AT lt_usobt INTO ls_usobt.
                        READ TABLE lt_tcodes TRANSPORTING NO FIELDS WITH KEY ls_usobt-object.
                        IF sy-subrc = 0.
                          lv_control_tcode = 'X'.
                          EXIT.
                        ENDIF.
                      ENDLOOP.
                      IF lv_control_tcode IS INITIAL.
* Si la transacción no se controla por actividad, buscamos en nuestra tabla
* Z (se carga con Z_CARGA_TCODES) si hay una transacción equivalente de
* visualización.
                        SELECT SINGLE * INTO ls_ztstc FROM ztstc
                               WHERE tcode = ls_tcodes-tcode.
                        IF sy-subrc = 0.
                          ls_tcode-rol = ls_salida-rol_copia.
                          ls_tcode-code = ls_ztstc-tcode_vis.
                          ls_1251-low = ls_ztstc-tcode_vis.
                          ls_tcode-text = TEXT-026.
                          APPEND ls_tcode TO t_tcode.
                          lv_con_tcode = 'X'.
* Modificamos la línea correspondiente de la 1251 con el tcode de visualización.
*                    modify lt_1251 FROM ls_1251 INDEX lv_tabix.
                          APPEND ls_1251 TO lt_1251.
                        ELSE.
                          ls_tcode-rol = ls_salida-rol_copia.
                          ls_tcode-code = ls_tcodes-tcode.
                          ls_tcode-text = TEXT-012.
                          APPEND ls_tcode TO t_tcode.
*                    DELETE lt_1251 INDEX lv_tabix.
                        ENDIF.
                      ELSE.
* Se ha encontrado que el TCODE se controla por actividad.
                        ls_tcode-rol = ls_salida-rol_copia.
                        ls_tcode-code = ls_tcodes-tcode.
                        ls_tcode-text = TEXT-016.

                        APPEND ls_tcode TO t_tcode.
                        APPEND ls_1251 TO lt_1251.
                        CLEAR lv_control_tcode.
                        lv_con_tcode = 'X'.

                      ENDIF.
                      REFRESH: t_tcode, lt_tcodes, lt_usobt.
                    ENDLOOP.
* borramos la línea de la tabla 1251 ya que la hemos sustituido por valores individuales.
                    DELETE lt_1251 INDEX lv_tabix.
                  ENDIF.
                ENDIF.
              ENDIF.

              IF ls_1251-field = 'ACTVT' AND ( ls_1251-low NE '03' AND ls_1251-low NE 'F4').
                ls_1251-low = '03'.
                lv_control_actv = 'X'.
                MODIFY lt_1251 FROM ls_1251 INDEX sy-tabix.
              ENDIF.

            ENDLOOP.


* Si el rol solo tienen Tcodes pero no actividades, borramos la copia
*            IF lv_con_tcode = ' '.
*              READ TABLE lt_1251 TRANSPORTING NO FIELDS WITH KEY object = 'S_TCODE'.
*              IF sy-subrc NE 0.
*                CALL FUNCTION 'PRGN_ACTIVITY_GROUP_DELETE'
*                  EXPORTING
*                    activity_group                = ls_salida-rol_copia
**                   ENQUEUE_AND_TRANSPORT         = 'X'
*                    show_dialog                   = ' '
**                   DISTRIBUTE                    = 'X'
**                   REQUEST                       =
*                  IMPORTING
*                    error_flag                    = lv_retc
**                   NEW_REQUEST                   =
**       TABLES
**                   MESSAGES                      =
*                  EXCEPTIONS
*                    not_authorized                = 1
*                    transport_check_problem       = 2
*                    transport_canceled_or_problem = 3
*                    one_or_more_users_enqueued    = 4
*                    foreign_lock                  = 5
*                    user_cancels_action           = 6
*                    child_agr_exists              = 7
*                    deletion_in_target_cancelled  = 8
*                    tech_error                    = 9
*                    hr_error                      = 10
*                    OTHERS                        = 11.
*
*                IF lv_retc IS INITIAL AND sy-subrc = 0.
*
*                  CLEAR ls_salida-rol_copia.
*                  ls_salida-text = TEXT-013.
*                  ls_salida-estado = '@0A@'.
*                  APPEND ls_salida TO t_salida.
*                  CONTINUE.
*
*                ENDIF.
*              ENDIF.
*
*            ENDIF.

            IF lv_con_tcode IS INITIAL."miramos si se ha añadido alguna transacción.
* Si no hay transacción porque la hemos borrad, borramos la línea de TCODE
              CALL FUNCTION 'PRGN_1250_READ_AUTH_DATA'
                EXPORTING
                  activity_group    = ls_salida-rol_copia
                TABLES
                  auth_data         = lt_1250
                EXCEPTIONS
                  no_data_available = 1
                  OTHERS            = 2.

              DELETE lt_1250 WHERE object = 'S_TCODE'.

              CALL FUNCTION 'PRGN_1250_SAVE_AUTH_DATA'
                EXPORTING
                  activity_group = ls_salida-rol_copia
                TABLES
                  auth_data      = lt_1250.


            ENDIF.

            CLEAR: lv_tabix, lv_control_actv , lv_control_tcode, lv_con_Tcode.
            DELETE ADJACENT DUPLICATES FROM lt_1251.

            CALL FUNCTION 'PRGN_1251_SAVE_FIELD_VALUES'
              EXPORTING
                activity_group = ls_salida-rol_copia
              TABLES
                field_values   = lt_1251.

* Grabamos las actividades que hemos editado para dejar solo visualización.
*      CALL FUNCTION 'SUPRN_SAVE_AUTH_DATA'
*        EXPORTING
*          role        = ls_salida-rol_copia
*        IMPORTING
*          return      = lt_return
*        TABLES
*          auths       = lt_1250
*          auth_values = lt_1251
*          org_values  = lt_1252.



            IF NOT lt_return IS INITIAL.

              ls_salida-text = TEXT-002.
              ls_salida-estado = '@0A@'.
              APPEND ls_salida TO t_salida.
              CONTINUE.
            ENDIF.

            CALL FUNCTION 'PRGN_UPDATE_DATABASE'
              EXCEPTIONS
                OTHERS = 1.

            CALL FUNCTION 'PRGN_CLEAR_BUFFER'
              EXCEPTIONS
                OTHERS = 1.

** Funcion para activar los roles

            CALL FUNCTION 'SUPRN_PROFILE_BATCH'
              EXPORTING
                act_objid        = ls_salida-rol_copia
*               no_usr_upd       = 'X'
              EXCEPTIONS
                objid_not_found  = 1
                no_authorization = 2
                enqueue_failed   = 3
                not_generated    = 4
                OTHERS           = 5.

            IF sy-subrc = 0.

* Desbloqueamos el Rol
              CALL FUNCTION 'PRGN_ACTIVITY_GROUP_DEQUEUE'
                EXPORTING
                  activity_group = ls_salida-rol_copia.

* Añadimos mensaje de
              ls_salida-text = TEXT-003.
              ls_salida-estado = '@08@'.
              APPEND ls_Salida TO t_salida.

            ELSE.

              MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_salida-text  .
              ls_salida-estado = '@0A@'.
              APPEND ls_salida TO t_salida.
              CONTINUE.

            ENDIF.

          ELSE.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 INTO ls_salida-text  .
            ls_salida-estado = '@0A@'.
            APPEND ls_salida TO t_salida.
            CONTINUE.

          ENDIF.

        ELSE.

          CLEAR ls_salida-rol_copia.
          ls_salida-text = TEXT-025.
          ls_salida-estado = '@0A@'.
          APPEND ls_salida TO t_salida.
          CONTINUE.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form progress_indicator
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_
*&---------------------------------------------------------------------*
FORM progress_indicator USING p_rol TYPE agr_name .
  DATA:
    ld_tx TYPE sy-msgv1.
*----------------------------------------------------------------------*

  ld_tx = 'Actualizando datos de cabecera del rol'(s03).    "#EC *


  IF sy-batch IS INITIAL AND sy-binpt IS INITIAL.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = 1
        text       = ld_tx.
  ELSE.
    MESSAGE i208(00) WITH ld_tx.
    COMMIT WORK.
  ENDIF.

ENDFORM.                    " progress_indicator


*&---------------------------------------------------------------------*
*& Form copiar_rol
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_display.

  DATA: lr_column  TYPE REF TO cl_salv_column_table,
        lr_columns TYPE REF TO cl_salv_columns_table.

  DATA lv_text_S TYPE scrtext_s.
  DATA lv_text_m TYPE scrtext_m.
  DATA lv_text_l TYPE scrtext_l.

  DATA: lr_functions TYPE REF TO cl_salv_functions.

  DATA: lr_display TYPE REF TO cl_salv_display_settings,

        lv_header  TYPE lvc_title.

  IF gt_salida[] IS NOT INITIAL.
*Instanciamos
    TRY.
        cl_salv_table=>factory(
        EXPORTING
          r_container  = lv_parent1
          IMPORTING
            r_salv_table   =  gr_table1   " Basis Class Simple ALV Tables
          CHANGING
            t_table        = gt_salida
        ).
      CATCH cx_salv_msg.    "

    ENDTRY.

    lr_display = gr_table1->get_display_settings( ).
    lr_display->set_striped_pattern( 'X' ).

    lv_header = TEXT-014.

    IF lv_header IS NOT INITIAL.

      lr_display->set_list_header( lv_header ).

    ENDIF.

    go_selec = gr_table1->get_selections( ).
    gr_table1->get_functions( )->set_all( abap_true ).
    go_selec->set_selection_mode( value = if_salv_c_selection_mode=>multiple ).
*
    lr_columns = gr_table1->get_columns( ).
    lr_columns->set_optimize( abap_true ).


* try.
*    lr_column ?= lr_columns->get_column( 'ROL' ).
**    lr_column->set_short_text( 'Rol'(t01) ).
**    lr_column->set_medium_text( 'Rol'(t01) ).
**    lr_column->set_long_text( 'Rol'(t01) ).


    lv_text_S = lv_text_m = lv_text_l = TEXT-t01.
    TRY.
        lr_column ?= lr_columns->get_column( 'ROL' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.
    lv_text_S = lv_text_m = lv_text_l = TEXT-t02.
    TRY.
        lr_column ?= lr_columns->get_column( 'ROL_COPIA' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.
    lv_text_S = lv_text_m = lv_text_l = TEXT-t03.

    TRY.
*
        lr_column ?= lr_columns->get_column( 'ESTADO' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.
    lv_text_S = lv_text_m = lv_text_l = TEXT-t04.
    TRY.
        lr_column ?= lr_columns->get_column( 'TEXT' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.
    TRY.
        lr_column ?= lr_columns->get_column( 'SIN_ACT' ).
        lr_column->set_visible( value  = if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found.
    ENDTRY.
    TRY.
        lr_column ?= lr_columns->get_column( 'SIN_TCODE' ).
        lr_column->set_visible( value  = if_salv_c_bool_sap=>false ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lr_functions = gr_table1->get_functions( ).
    lr_functions->set_all( if_salv_c_bool_sap=>true ).

    gr_table1->display( ).

  ELSE.
*    MESSAGE i000(z72fi) WITH TEXT-009.
  ENDIF.

*


ENDFORM.
*&---------------------------------------------------------------------*
*& Form fotr_crear_transpote
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> gt_salida
*&---------------------------------------------------------------------*
*FORM fotr_crear_transpote  TABLES t_salida STRUCTURE gs_salida.
*
*  DATA: id_role TYPE agr_name.
*  DATA: ls_Salida LIKE LINE OF t_salida.
*
*
*  CONSTANTS: lc_var_sgl TYPE raldb_vari VALUE 'SAP&SINGLE_AGR',
*             lc_var_col TYPE raldb_vari VALUE 'SAP&COMP_AGR'.
*
*  DATA: lt_hier   TYPE smenagrdeftyp,
*        lt_return TYPE bapirettab.
*
*  DATA: ls_return TYPE bapiret2,
*        ls_msg    TYPE symsg.
*
*  DATA: ld_request  TYPE trkorr,
*        ld_role_rec TYPE agr_name,
*        ld_retc     TYPE sysubrc,
*        ld_variant  TYPE raldb_vari.
*
*  DATA: lf_coll_role TYPE char01,
*        lf_result    TYPE char01.
*
*  DATA: answer TYPE c .
*
*  FIELD-SYMBOLS: <role> TYPE agr_define.
** Leemos un registro correcto de la tabla de salida.
*  LOOP AT t_Salida INTO ls_salida WHERE estado = '@08@'.
*    id_role = ls_salida-rol_copia.
*  ENDLOOP.
*
*  CHECK sy-subrc = 0.
*
** Checking role type
*  CALL FUNCTION 'PRGN_GET_COLLECTIVE_AGR_FLAG'
*    EXPORTING
*      activity_group      = id_role
*    IMPORTING
*      collective_agr_flag = lf_coll_role
*    EXCEPTIONS
*      agr_does_not_exist  = 1
*      flag_not_available  = 2
*      OTHERS              = 3.
*  IF sy-subrc NE 0.
*    MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
*            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    RETURN.
*  ENDIF.
*
*  CLEAR: lt_hier, ld_role_rec, lt_return.
** Checking SCC4 setting
*  CALL FUNCTION 'PRGN_CHK_CLIENT_CUST_SETTING'
*    EXCEPTIONS
*      automatic_recording    = 1
*      no_automatic_recording = 1
*      no_changes_allowed     = 1
*      no_transport_allowed   = 2
*      OTHERS                 = 2.
*  CASE sy-subrc.
*    WHEN 1.
*      IF lf_coll_role EQ space.
*        CALL FUNCTION 'PRGN_GET_AGR_HIERARCHY'
*          EXPORTING
*            agr_name              = id_role
*            up                    = 'X'
*            down                  = space
*          TABLES
*            relation              = lt_hier
*          EXCEPTIONS
*            cycle_detected        = 1
*            parent_does_not_exist = 2
*            OTHERS                = 3.
*        IF sy-subrc NE 0.
*          MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
*                  WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*          RETURN.
*        ENDIF.
*      ELSE.
*        ld_role_rec = id_role.
*        APPEND INITIAL LINE TO lt_hier ASSIGNING <role>.
*        <role>-agr_name = id_role.
*      ENDIF.
**     Checking existing records
*      CLEAR answer.
*      PERFORM chk_recs_1_role IN PROGRAM saplprgn
*                              USING    lt_hier
*                              CHANGING lf_result
*                                       ld_request ld_role_rec lt_return.
*      IF ld_request NE space.
*        ls_msg-msgty = 'S'.
*        ls_msg-msgid = 'S#'.
*        ls_msg-msgv1 = id_role.
*        IF ld_role_rec EQ id_role.
*          ls_msg-msgno = 267.
*          ls_msg-msgv2 = ld_request.
*        ELSE.
*          ls_msg-msgno = 277.
*          ls_msg-msgv2 = ld_role_rec.
*          ls_msg-msgv3 = ld_request.
*        ENDIF.
*        IF 1 = 0.
*          MESSAGE s267(s#) WITH ls_msg-msgv1 ls_msg-msgv2.
*          MESSAGE s277(s#) WITH ls_msg-msgv1 ls_msg-msgv2 ls_msg-msgv3.
*        ENDIF.
*        IF lf_result EQ 'R'.
**         New record optional: record exists on own task.
*          PERFORM add_msg_to_return(saplprgn_auth) USING    ls_msg
*                                                   CHANGING lt_return.
*          READ TABLE lt_return INTO ls_return INDEX 1.
*          CALL FUNCTION 'POPUP_TO_CONFIRM'
*            EXPORTING
*              titlebar              = 'Own record'(120)
*              text_question         = ls_return-message
*              icon_button_1         = 'ICON_OKAY'
*              text_button_1         = 'Continue'(124)
*              icon_button_2         = 'ICON_CANCEL'
*              text_button_2         = 'Cancel'(125)
*              display_cancel_button = space
*            IMPORTING
*              answer                = answer
*            EXCEPTIONS
*              OTHERS                = 0.
*        ELSE.
**         New record required: role hierarchy incompletely recorded or
**         on foreign request
*          MESSAGE ID 'S#' TYPE 'I' NUMBER ls_msg-msgno
*                                   WITH   ls_msg-msgv1 ls_msg-msgv2
*                                          ls_msg-msgv3.
*        ENDIF.
*      ENDIF.
**     Initializing record buffer for avoiding wrong check results during
**     next call
*      PERFORM init_rec_buffer IN PROGRAM saplprgn USING lt_hier.
*      IF lf_result EQ 'E'.
*        PERFORM display_return(saplprgn_auth) USING lt_return.
*        MESSAGE s323(s#) DISPLAY LIKE 'E'. RETURN.
*      ENDIF.
*      IF answer EQ '2'.  "Cancel
*        MESSAGE s232(s#). RETURN.
*      ENDIF.
*    WHEN 2.
*      MESSAGE ID sy-msgid TYPE 'E' NUMBER sy-msgno
*              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*      RETURN.
*  ENDCASE.
*
*  IF lf_coll_role EQ 'X'.
*    ld_variant = lc_var_col.
*  ELSE.
*    ld_variant = lc_var_sgl.
*  ENDIF.
*  id_role = 'Z*CPY'.
** Checking the existence of the variant
*  CALL FUNCTION 'RS_VARIANT_EXISTS'
*    EXPORTING
*      report              = 'PFCG_MASS_TRANSPORT'
*      variant             = ld_variant
*    IMPORTING
*      r_c                 = ld_retc
*    EXCEPTIONS
*      not_authorized      = 1
*      no_report           = 2
*      report_not_existent = 3
*      OTHERS              = 4.
*  IF sy-subrc EQ 0 AND ld_retc EQ 0.
*    SUBMIT pfcg_mass_transport WITH agr_name = id_role
*           VIA SELECTION-SCREEN USING SELECTION-SET ld_variant
*           AND RETURN.
*  ELSE.
*    SUBMIT pfcg_mass_transport WITH agr_name = id_role
*           VIA SELECTION-SCREEN
*           AND RETURN.
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*& Form fotr_get_nombre_copia
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- LS_SALIDA
*&---------------------------------------------------------------------*
FORM fotr_get_nombre_copia  USING p_name TYPE agr_name
                            CHANGING p_salida STRUCTURE gs_Salida.


  DATA: lv_count TYPE i.
  CLEAR lv_count.
  p_salida-rol = p_name.
* Contruimos en nombre del rol copia. (Es como el que entra + CPY)
* Comprobamos si el nombre original + CPY < 30 (long. max. nombre rol)
  lv_count = strlen( p_salida-rol ).
  IF lv_count < 26.
    CONCATENATE p_name pa_sufix INTO p_salida-rol_copia.
  ELSE.
    CONCATENATE p_name(26) pa_sufix INTO p_salida-rol_copia.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'STANDARD'.
  SET TITLEBAR 'T01'.

  CREATE OBJECT gr_container
    EXPORTING
*     parent                      = g_grid_main
      repid                       = sy-repid "needs REPORT id
      dynnr                       = sy-dynnr "need dynpro number
      side                        = cl_gui_docking_container=>dock_at_bottom "we want to ADD the docking ON the bottom of the screen 2000
      extension                   = cl_gui_docking_container=>ws_maximizebox "The Dockingcontainer should use the hole screen
*     style                       =
*     lifetime                    = lifetime_default
*     caption                     =
*     metric                      = 0
*     ratio                       = 70
*     no_autodef_progid_dynnr     =
*     name                        =
    EXCEPTIONS
      cntl_error                  = 1
      cntl_system_error           = 2
      create_error                = 3
      lifetime_error              = 4
      lifetime_dynpro_dynpro_link = 5
      OTHERS                      = 6.
  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgnr TYPE sy-smgty NUMBER sy–msgno
*    WITH sy–msgv1 sy–msgv2 sy–msgv3 sy–msgv4.
  ENDIF.

**   Creamos el splitter para poner los dos ALVS
  CREATE OBJECT lv_splitter
    EXPORTING
      parent  = gr_container
      rows    = 2
      columns = 1
      align   = 15.
**   get part of splitter container for 1st table
  CALL METHOD lv_splitter->get_container
    EXPORTING
      row       = 1
      column    = 1
    RECEIVING
      container = lv_parent1.
**   get part of splitter container for 2nd table
  CALL METHOD lv_splitter->get_container
    EXPORTING
      row       = 2
      column    = 1
    RECEIVING
      container = lv_parent2.


***  Display first ALV
  PERFORM set_display.
***  Display second ALV
  PERFORM set_display1.

ENDMODULE.
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  IF sy-ucomm = '&F12' OR sy-ucomm = '&F15'.
    LEAVE PROGRAM.
  ELSEIF sy-ucomm = '&F03'.
    SET SCREEN 0.
  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form set_display1
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM set_display1 .

  DATA: lr_column  TYPE REF TO cl_salv_column_table,
        lr_columns TYPE REF TO cl_salv_columns_table.

  DATA lv_text_S TYPE scrtext_s.
  DATA lv_text_m TYPE scrtext_m.
  DATA lv_text_l TYPE scrtext_l.

  DATA: lr_functions TYPE REF TO cl_salv_functions.


  DATA: lr_display TYPE REF TO cl_salv_display_settings,
        lv_header  TYPE lvc_title.

  IF gt_tcode[] IS NOT INITIAL.
*Instanciamos
    TRY.
        cl_salv_table=>factory(
        EXPORTING
          r_container  = lv_parent2
          IMPORTING
            r_salv_table   =  gr_table2
          CHANGING
            t_table        = gt_tcode
        ).
      CATCH cx_salv_msg.    "

    ENDTRY.

    lr_display = gr_table2->get_display_settings( ).
    lr_display->set_striped_pattern( 'X' ).

    lv_header = TEXT-015.

    IF lv_header IS NOT INITIAL.

      lr_display->set_list_header( lv_header ).

    ENDIF.
*
    go_selec2 = gr_table2->get_selections( ).
    gr_table2->get_functions( )->set_all( abap_true ).
    go_selec2->set_selection_mode( value = if_salv_c_selection_mode=>multiple ).
*
    lr_columns = gr_table2->get_columns( ).
    lr_columns->set_optimize( abap_true ).


    lv_text_S = lv_text_m = lv_text_l = TEXT-t01.
    TRY.
        lr_column ?= lr_columns->get_column( 'ROL' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lv_text_S = lv_text_m = lv_text_l = TEXT-t05.
    TRY.
        lr_column ?= lr_columns->get_column( 'CODE' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.
    lv_text_S = lv_text_m = lv_text_l = TEXT-t06.
    TRY.
        lr_column ?= lr_columns->get_column( 'TEXT' ).
        lr_column->set_short_text( lv_text_S ).
        lr_column->set_medium_text( lv_text_m ).
        lr_column->set_long_text( lv_text_l ).
      CATCH cx_salv_not_found.
    ENDTRY.

    lr_functions = gr_table1->get_functions( ).
    lr_functions->set_all( if_salv_c_bool_sap=>true ).

    gr_table2->display( ).

  ELSE.
*    MESSAGE i000(z72fi) WITH TEXT-009.
  ENDIF.


ENDFORM.
*&---------------------------------------------------------------------*
*& Form carga_tabla_conf
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LT_ROL
*&---------------------------------------------------------------------*
FORM carga_tabla_conf  USING pt_rol TYPE ztt_tiporol.


  DATA: ls_rol TYPE ztl_tiporol.
  DATA: ls_salida   TYPE ty_salida,
        ls_zincltab LIKE LINE OF zincltab.


  LOOP AT pt_rol INTO ls_rol.



    PERFORM fotr_get_nombre_copia  USING ls_rol-agr_name
                                   CHANGING ls_salida.


    MOVE ls_salida-rol TO ls_ZINCLTAB-name.
    MOVE ls_salida-rol_copia TO ls_ZINCLTAB-new_name.
    APPEND ls_ZINCLTAB TO ZINCLTAB_act.

*  move corresponding


  ENDLOOP.


ENDFORM.
