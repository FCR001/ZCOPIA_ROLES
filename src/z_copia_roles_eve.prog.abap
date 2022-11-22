AT SELECTION-SCREEN OUTPUT.

  if ck_rfc is not initial.

    loop at screen.
      if screen-group1 = 'DST'.
        screen-input = 1.
        screen-invisible = 0.
        screen-required = 1.
        MODIFY SCREEN.
      endif.
    endloop.

 else.

   loop at screen.
      if screen-group1 = 'DST'.
        screen-input = 0.
        screen-invisible = 1.
        screen-required = 0.
        MODIFY SCREEN.
      endif.
    endloop.
    clear pa_rfc.

 endif.



*&---------------------------------------------------------------------*
*& Include          Z_COPIA_ROLES_EVE
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  IF so_user IS INITIAL AND so_rol IS INITIAL.
    MESSAGE TEXT-009 TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  PERFORM fabd_copiar_rol TABLES gt_salida gt_tcode .

*if pa_trans is not initial.
** Incluimos los roles copiados en una orden de transporte.
* perform fotr_crear_transpote tables gt_salida.
*endif.

  IF gt_salida IS INITIAL.
    IF ok_code IS INITIAL.
      MESSAGE TEXT-010 TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      MESSAGE TEXT-017 TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.
    EXIT.
  ENDIF.

  IF gt_salida[] IS NOT INITIAL.
*  perform fotr_mostrar_alv.
    CALL SCREEN 2000.
  ENDIF.
