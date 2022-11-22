FUNCTION Z_GET_ROL_FROM_USER.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(IT_USUARIO) TYPE  ZTT_USER
*"  EXPORTING
*"     VALUE(T_ROLES) TYPE  ZTT_TIPOROL
*"----------------------------------------------------------------------



    SELECT agr_name FROM agr_users
    INTO TABLE t_roles
    FOR ALL ENTRIES IN it_usuario
    WHERE uname = it_usuario-uname.



ENDFUNCTION.
