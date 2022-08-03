*&---------------------------------------------------------------------*
*& Report  ZFI_CUENTAS_COBRAR_PAGAR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfi_cuentas_cobrar_pagar.

TYPE-POOLS: slis.
TABLES: bsik, bsid, bkpf, lfa1, kna1, adrc.

DATA: g_repid TYPE sy-repid.
DATA: ok_code LIKE sy-ucomm.

*///// Structures & Interal Tables
*TYPES: BEGIN OF st_alv,
*    lifnr TYPE lfa1-lifnr , " Acreedor
*    kunnr TYPE kna1-kunnr , " Cliente
*    budat TYPE bkpf-budat , " Fecha de contabilizacion
*    bktxt TYPE bkpf-bktxt , " NCF
*    xblnr TYPE bsid-xblnr , " Referencia
*    waers TYPE bkpf-waers , " Moneda
*    zbd1t TYPE bsak-zbd1t , " Dias
*    dmbtr TYPE bsak-dmbtr , " Importe
*    badoc TYPE bsak-dmbtr , " Balance de Documentos
*END OF st_alv.


*///////////////////////////////      Internal Tables
DATA: it_alv  TYPE zst_estado, "st_alv,
      it_bsik TYPE TABLE OF bsik,
      it_bsid TYPE TABLE OF bsid,
      it_bkpf TYPE TABLE OF bkpf,
      it_lfa1 TYPE TABLE OF lfa1,
      it_kna1 TYPE TABLE OF kna1,
      it_adrc TYPE TABLE OF adrc,
      it_docncf TYPE TABLE OF ztb_fi_rg_docncf,
      it_nro_ctrol TYPE TABLE OF zsd_nro_ctrl.


*///// Field Symbols
FIELD-SYMBOLS: <fs_alv>  TYPE zst_estado_cuentas, "ZTT_ESTADO_CUENTAS, "st_alv,
               <fs_bsik> TYPE bsik,
               <fs_bsid> TYPE bsid,
               <fs_bkpf> TYPE bkpf,
               <fs_lfa1> TYPE lfa1,
               <fs_kna1> TYPE kna1,
               <fs_adrc> TYPE adrc,
               <fs_nro_ctrol> TYPE zsd_nro_ctrl,
               <fs_docncf> TYPE ztb_fi_rg_docncf.

DATA: l_funcion TYPE rs38l_fnam.
DATA:  v_tdname.
DATA: flag TYPE char10.


*///////////////////////////////      Entry-Screen
SELECTION-SCREEN BEGIN OF BLOCK block_1 WITH FRAME TITLE text-001.
PARAMETERS:     p_bukrs LIKE bkpf-bukrs DEFAULT 'DO01' OBLIGATORY.
SELECT-OPTIONS: s_budat FOR  bkpf-budat DEFAULT sy-datum OBLIGATORY,
                lifnr   FOR  lfa1-lifnr,
                kunnr   FOR  kna1-kunnr.
SELECTION-SCREEN END OF BLOCK block_1.

SELECTION-SCREEN BEGIN OF BLOCK block_2 WITH FRAME TITLE text-002.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(17) text-003 FOR FIELD r_lifnr.
SELECTION-SCREEN POSITION 20.
PARAMETERS: r_lifnr RADIOBUTTON GROUP rad DEFAULT 'X' USER-COMMAND rad.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(17) text-004 FOR FIELD r_kunnr.
SELECTION-SCREEN POSITION 20.
PARAMETERS: r_kunnr RADIOBUTTON GROUP rad.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK block_2.

SELECTION-SCREEN BEGIN OF BLOCK block_3 WITH FRAME TITLE text-005.
PARAMETERS: c_imp AS CHECKBOX,
            c_pdf AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK block_3.


AT SELECTION-SCREEN OUTPUT.
  IF r_lifnr EQ 'X'.
    LOOP AT SCREEN.
      REFRESH: kunnr.
      IF ( screen-name(5) EQ 'LIFNR' OR screen-name(7) EQ '%_LIFNR' ).
        screen-input = 1.
        screen-invisible = 0.
        MODIFY SCREEN.
      ELSEIF ( screen-name(5) EQ 'KUNNR' OR screen-name(7) EQ '%_KUNNR' ) .
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF .
    ENDLOOP.

  ELSEIF r_kunnr EQ 'X'.
    REFRESH: lifnr.
    LOOP AT SCREEN.
      IF ( screen-name(5) EQ 'KUNNR' OR screen-name(7) EQ '%_KUNNR' ) .
        screen-input = 1.
        screen-invisible = 0.
        MODIFY SCREEN.
      ELSEIF ( screen-name(5) EQ 'LIFNR' OR screen-name(7) EQ '%_LIFNR' ).
        screen-input = 0.
        screen-invisible = 1.
        MODIFY SCREEN.
      ENDIF .
    ENDLOOP.

  ENDIF.

*///////////////////////////////      Queries F01
START-OF-SELECTION.

  IF r_lifnr EQ 'X'.

    SELECT *
      FROM bsik
      INTO TABLE it_bsik
      WHERE bukrs EQ p_bukrs
        AND budat IN s_budat
        AND lifnr IN lifnr.
*        AND blart IN ('KB', 'KC', 'KD').

    IF it_bsik IS NOT INITIAL.
      SELECT *
        FROM bkpf
        INTO CORRESPONDING FIELDS OF TABLE it_bkpf
        FOR ALL ENTRIES IN it_bsik
        WHERE bukrs EQ it_bsik-bukrs
          AND budat EQ it_bsik-budat
          AND belnr EQ it_bsik-belnr.

      SELECT *
        FROM ztb_fi_rg_docncf
        INTO TABLE it_docncf
        FOR ALL ENTRIES IN it_bsik
        WHERE belnr EQ it_bsik-belnr.
    ENDIF.


  ELSEIF r_kunnr EQ 'X'.

    SELECT *
     FROM bsid
     INTO TABLE it_bsid
     WHERE bukrs EQ p_bukrs
       AND budat IN s_budat
       AND kunnr IN kunnr.

    IF it_bsid IS NOT INITIAL.
      SELECT *
        FROM bkpf
        INTO TABLE it_bkpf
        FOR ALL ENTRIES IN it_bsid
        WHERE bukrs EQ it_bsid-bukrs
          AND budat EQ it_bsid-budat
          AND belnr EQ it_bsid-belnr.

      SELECT *
        FROM zsd_nro_ctrl
        INTO TABLE it_nro_ctrol
        FOR ALL ENTRIES IN it_bsid
        WHERE vbeln EQ it_bsid-vbeln.
    ENDIF.

  ENDIF.

  SORT it_bsik[] BY lifnr budat ASCENDING.
  SORT it_bsid[] BY kunnr budat ASCENDING.

  DATA: gv_dmbtr TYPE dmbtr,
        gv_lifnr TYPE lifnr,
        gv_kunnr TYPE kunnr,
        gv_total TYPE dmbtr.


  IF r_lifnr EQ 'X'.

    LOOP AT it_bsik ASSIGNING <fs_bsik>.

      APPEND INITIAL LINE TO it_alv-cuentas ASSIGNING <fs_alv>.

      <fs_alv>-lifnr = <fs_bsik>-lifnr.      "Acreedor
      <fs_alv>-budat = <fs_bsik>-budat.      "Fecha de Contabilizacion
      <fs_alv>-xblnr = <fs_bsik>-xblnr.      "Referencia
      <fs_alv>-zbd1t = <fs_bsik>-zbd1t.      "Dias
      <fs_alv>-dmbtr = <fs_bsik>-dmbtr.      "Importe
*      <fs_alv>-waers = <fs_bsik>-waers.      "Moneda

      IF <fs_bsik>-shkzg EQ 'H'. "Suma

        gv_dmbtr = gv_dmbtr + <fs_bsik>-dmbtr.

      ELSEIF <fs_bsik>-shkzg EQ 'S'. "Resta

        gv_dmbtr = gv_dmbtr - <fs_bsik>-dmbtr.

      ENDIF.

      IF sy-tabix EQ '1'.
        <fs_alv>-badoc = gv_dmbtr.
      ELSEIF gv_lifnr EQ <fs_bsik>-lifnr.
        <fs_alv>-badoc =  gv_dmbtr.
      ELSEIF gv_lifnr NE <fs_bsik>-lifnr.
        <fs_alv>-badoc = <fs_bsik>-dmbtr.
        gv_dmbtr = <fs_bsik>-dmbtr.
      ENDIF.

      gv_lifnr = <fs_bsik>-lifnr.      "Acreedor


      CASE  <fs_bsik>-blart.
        WHEN 'KB' OR 'KC' OR 'KD'.
          READ TABLE it_docncf ASSIGNING <fs_docncf> WITH KEY belnr = <fs_bsik>-belnr.
          IF  sy-subrc EQ 0.
            <fs_alv>-ncf = <fs_docncf>-ncf.
            <fs_alv>-waers = <fs_bsik>-waers.
          ENDIF.
        WHEN OTHERS.
          READ TABLE it_bkpf ASSIGNING <fs_bkpf> WITH KEY bukrs = <fs_bsik>-bukrs
                                                          budat = <fs_bsik>-budat
                                                          belnr = <fs_bsik>-belnr
                                                          blart = <fs_bsik>-blart.
          IF sy-subrc EQ 0.
            <fs_alv>-ncf   = <fs_bkpf>-bktxt.      "NFC
            <fs_alv>-waers = <fs_bkpf>-hwaer.      "Moneda
          ENDIF.

      ENDCASE.



      "Saldo final formulario
      CLEAR: gv_total.
      gv_total = <fs_alv>-badoc.

    ENDLOOP.

    SORT it_alv-cuentas[] BY lifnr budat ASCENDING.

  ELSEIF r_kunnr EQ 'X'.

    LOOP AT it_bsid ASSIGNING <fs_bsid>.

      APPEND INITIAL LINE TO it_alv-cuentas ASSIGNING <fs_alv>.

      <fs_alv>-kunnr = <fs_bsid>-kunnr.      "Cliente
      <fs_alv>-budat = <fs_bsid>-budat.      "Fecha de Contabilizacion
      <fs_alv>-xblnr = <fs_bsid>-xblnr.      "Referencia
      <fs_alv>-zbd1t = <fs_bsid>-zbd1t.      "Dias
*      <fs_alv>-waers = <fs_bsid>-waers.     "Moneda

      IF <fs_bsid>-bukrs EQ 'DO03'.
        <fs_alv>-waers = 'USD'.              "Moneda Fija para DO03
        <fs_alv>-dmbtr = <fs_bsid>-dmbe2.    "Importe

        IF <fs_bsid>-shkzg EQ 'H'. "Suma
          gv_dmbtr = gv_dmbtr + <fs_bsid>-dmbe2.
        ELSEIF <fs_bsid>-shkzg EQ 'S'. "Resta
          gv_dmbtr = gv_dmbtr - <fs_bsid>-dmbe2..
        ENDIF.

      ELSE.
*        <fs_alv>-waers = <fs_bsid>-waers.    "Moneda
        <fs_alv>-dmbtr = <fs_bsid>-dmbtr.    "Importe

        IF <fs_bsid>-shkzg EQ 'H'. "Suma
          gv_dmbtr = gv_dmbtr + <fs_bsid>-dmbtr.
        ELSEIF <fs_bsid>-shkzg EQ 'S'. "Resta
          gv_dmbtr = gv_dmbtr - <fs_bsid>-dmbtr.
        ENDIF.
      ENDIF.

      IF sy-tabix EQ '1'.
        <fs_alv>-badoc = gv_dmbtr.
      ELSEIF gv_kunnr EQ <fs_bsid>-kunnr.
        <fs_alv>-badoc =  gv_dmbtr.
      ELSEIF gv_kunnr NE <fs_bsid>-kunnr.
        <fs_alv>-badoc = <fs_bsid>-dmbtr.
        gv_dmbtr = <fs_bsid>-dmbtr.
      ENDIF.

      gv_kunnr = <fs_bsid>-kunnr.      "Cliente

      READ TABLE it_bkpf ASSIGNING <fs_bkpf> WITH KEY bukrs = <fs_bsid>-bukrs
                                                      budat = <fs_bsid>-budat
                                                      belnr = <fs_bsid>-belnr.
      IF sy-subrc EQ 0.
        IF <fs_bsid>-bukrs <> 'DO03'.
*          <fs_alv>-waers = <fs_bsid>-waers.    "Moneda

*        ELSE.
*          <fs_alv>-bktxt = <fs_bkpf>-bktxt.      "NFC
          <fs_alv>-waers = <fs_bkpf>-hwaer.      "Moneda
        ENDIF.
      ENDIF.

      READ TABLE it_nro_ctrol ASSIGNING <fs_nro_ctrol> WITH KEY vbeln = <fs_bsid>-vbeln.
      IF  sy-subrc EQ 0.
        <fs_alv>-ncf = <fs_nro_ctrol>-ncf.
      ENDIF.

      "Saldo final formulario
      CLEAR: gv_total.
      gv_total = <fs_alv>-badoc.

    ENDLOOP.

    SORT it_alv-cuentas[] BY kunnr budat ASCENDING.

  ENDIF.


*///////////////////////////////      ALV
  IF r_lifnr EQ 'X'.
    flag = 'PROVEEDOR'.
  ELSEIF r_kunnr EQ 'X'.
    flag = 'DEUDOR'.
  ENDIF.

  IF it_alv-cuentas[] IS NOT INITIAL.
    IF c_imp EQ 'X'.
      PERFORM imprimir.
    ELSEIF c_pdf EQ 'X'.
      PERFORM pdf.
    ELSE.
      PERFORM alv_report USING it_alv-cuentas[].
    ENDIF.

  ELSE.
    MESSAGE i162(00) WITH 'No existen datos para su selección'.
    STOP.
  ENDIF.

*  TYPE-POOLS: slis.
  DATA: lf_sp_group   TYPE slis_t_sp_group_alv,                       "Grupos de campos
        lf_layout     TYPE slis_layout_alv.                         "Diseño de layout

*ALV Header
  DATA: lt_header       TYPE slis_t_listheader,                       "Header del rep
        ls_header       TYPE slis_listheader,                         "Linea del header
        lt_line         LIKE ls_header-info,
        lv_lines        TYPE i,
        lv_linesc(10)   TYPE c.

* Eventos
  DATA: i_events            TYPE slis_t_event,
        w_events            TYPE slis_alv_event.

  DATA: p_status TYPE slis_t_extab.                                   "ALV Status Button
  DATA: alv_git_fieldcat TYPE slis_t_fieldcat_alv WITH HEADER LINE.   "Parametros del catalogo




*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV
FORM alv_report  USING  pp_itab LIKE it_alv-cuentas[].

  PERFORM sp_group_build USING lf_sp_group[].         " ALV PERFORM_1
  PERFORM alv_ini_fieldcat.                           " ALV PERFORM_2
  PERFORM layout_build USING lf_layout.               " ALV PERFORM_3
  PERFORM do_events.
  PERFORM alv_listado USING pp_itab[].                " ALV PERFORM_4

ENDFORM.                    "alv_report

*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV PERFORM_1
FORM sp_group_build USING u_lf_sp_group TYPE slis_t_sp_group_alv.

  DATA: ls_sp_group TYPE slis_sp_group_alv.
  CLEAR  ls_sp_group.
  ls_sp_group-sp_group = 'A'.
  ls_sp_group-text     = text-010.
  APPEND ls_sp_group TO u_lf_sp_group.

ENDFORM.                    "sp_group_build


*///////////////////////////////      ALV PERFORM_2
FORM alv_ini_fieldcat.

  IF r_lifnr EQ 'X'.
    CLEAR alv_git_fieldcat.
    alv_git_fieldcat-fieldname   = 'LIFNR'.
    alv_git_fieldcat-seltext_m   = 'Acreedor'.
    alv_git_fieldcat-seltext_l   = 'Acreedor'.
    alv_git_fieldcat-col_pos     = 0.
    alv_git_fieldcat-sp_group    = 'A'.
    alv_git_fieldcat-outputlen   = '15'.
    APPEND alv_git_fieldcat TO alv_git_fieldcat.

  ELSEIF r_kunnr EQ 'X'.

    CLEAR alv_git_fieldcat.
    alv_git_fieldcat-fieldname   = 'KUNNR'.
    alv_git_fieldcat-seltext_m   = 'Cliente'.
    alv_git_fieldcat-seltext_l   = 'Cliente'.
    alv_git_fieldcat-col_pos     = 0.
    alv_git_fieldcat-sp_group    = 'A'.
    alv_git_fieldcat-outputlen   = '15'.
    APPEND alv_git_fieldcat TO alv_git_fieldcat.
  ENDIF.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BUDAT'.
  alv_git_fieldcat-seltext_m   = 'Fecha Cont.'.
  alv_git_fieldcat-seltext_l   = 'Fecha de Contabilizacion'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'NCF'.
  alv_git_fieldcat-seltext_m   = 'NCF'.
  alv_git_fieldcat-seltext_l   = 'NCF'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'XBLNR'.
  alv_git_fieldcat-seltext_m   = 'Referencia'.
  alv_git_fieldcat-seltext_l   = 'Referencia'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '16'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'WAERS'.
  alv_git_fieldcat-seltext_m   = 'Mon.'.
  alv_git_fieldcat-seltext_l   = 'Moneda'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'ZBD1T'.
  alv_git_fieldcat-seltext_m   = 'Días'.
  alv_git_fieldcat-seltext_l   = 'Días'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '10'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'DMBTR'.
  alv_git_fieldcat-seltext_m   = 'Importe'.
  alv_git_fieldcat-seltext_l   = 'Importe'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-fieldname   = 'BADOC'.
  alv_git_fieldcat-seltext_m   = 'Balance Doc.'.
  alv_git_fieldcat-seltext_l   = 'Balance de Documentos'.
  alv_git_fieldcat-col_pos     = 0.
  alv_git_fieldcat-sp_group    = 'A'.
  alv_git_fieldcat-outputlen   = '20'.
  APPEND alv_git_fieldcat TO alv_git_fieldcat.

  CLEAR alv_git_fieldcat.
  alv_git_fieldcat-sp_group = 'A'.
*  MODIFY alv_git_fieldcat FROM alv_git_fieldcat
*  TRANSPORTING sp_group WHERE fieldname = 'VBTYP'.

ENDFORM.                    "alv_ini_fieldcat

*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV PERFORM_3
FORM layout_build USING    u_lf_layout TYPE slis_layout_alv.

*  u_lf_layout-box_fieldname       = 'CHECK'.  "Checkbox
  u_lf_layout-zebra               = 'X'.      "Streifenmuster
*  u_lf_layout-get_selinfos        = 'X'.
*  u_lf_layout-f2code              = 'BEAN' .  "Doppelklickfunktion
*  u_lf_layout-confirmation_prompt = 'X'.      "Sicherheitsabfrage
*  u_lf_layout-key_hotspot         = 'X'.      "Schlüssel als Hotspot
*  u_lf_layout-info_fieldname      = 'COL'.    "Zeilenfarbe

ENDFORM.                    "layout_build

*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV PERFORM_4
FORM alv_listado  USING ppp_itab LIKE it_alv-cuentas[].

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
       EXPORTING
            i_callback_program        = sy-repid
            i_buffer_active           = 'X'
            i_callback_top_of_page    = 'TOP_OF_PAGE'                   "ALV Header
*            i_callback_pf_status_set  = 'STATUS_GUI'                    "Status Bar
*            i_callback_user_command   = 'USER_COMMAND_9001'             "Comandos de usuario
            is_layout                 = lf_layout
            it_fieldcat               = alv_git_fieldcat[]
            i_save                    = 'X'
            it_events                 = i_events
       TABLES
            t_outtab                  = ppp_itab.

ENDFORM.                    "alv_listado



*≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈≈     ALV HEADER
FORM top_of_page.

  CLEAR lt_header[].                                               " Limpia la tabla y no repite el header.

* Titulo
  ls_header-typ = 'H'.
  ls_header-info = 'Reporte Cuentas por Cobrar y Pagar'.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header.

* Fecha
  ls_header-typ = 'S'.
  ls_header-key = 'Fecha: '.
  CONCATENATE sy-datum+6(2) '.'
              sy-datum+4(2) '.'
              sy-datum(4)
              INTO ls_header-info.                                 "Fecha de hoy concatenada y separada por "."
  APPEND ls_header TO lt_header.
  CLEAR: ls_header.

*No. Registros en el Reporte
  DESCRIBE TABLE it_alv-cuentas LINES lv_lines.
  lv_linesc = lv_lines.
  CONCATENATE 'No. Registros: ' lv_linesc        "Concatenamos Cant. de Registros
  INTO lt_line SEPARATED BY space.
  ls_header-typ = 'A'.
  ls_header-info = lt_line.
  APPEND ls_header TO lt_header.
  CLEAR: ls_header, lt_line.


  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      i_logo             = 'LOGO_MOLANCA'
      it_list_commentary = lt_header.

ENDFORM.                    "top-of-page



*&---------------------------------------------------------------------*
*&      Form  do_events
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM do_events.

  REFRESH i_events.
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type     = 0
    IMPORTING
      et_events       = i_events
    EXCEPTIONS
      list_type_wrong = 1
      OTHERS          = 2.

* Leer los eventos que me interesan
  CLEAR w_events.

  READ TABLE i_events INTO w_events
  WITH KEY name = slis_ev_top_of_page.

  IF sy-subrc = 0.
    MOVE 'TOP_OF_PAGE' TO w_events-form.
    MODIFY i_events FROM w_events INDEX sy-tabix.
  ENDIF.

ENDFORM.                    " do_events                " EXTRAERDAT



*----------------------------------------------------------------------*
*  MODULE status_9001 OUTPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
*MODULE status_gui OUTPUT.
*  SET PF-STATUS 'ZSTANDARD'.
*
*ENDMODULE.                 " STATUS_9001  OUTPUT

*----------------------------------------------------------------------*
*  MODULE user_command_9001 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE user_command_9001 INPUT.
  CASE ok_code.
    WHEN 'BACK' OR 'SALIR' OR 'CANCELAR'.
      LEAVE TO LIST-PROCESSING AND RETURN TO SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.

ENDMODULE.                    "user_command_9001 INPUT



*&---------------------------------------------------------------------*
*&      Form  IMPRIMIR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM imprimir.

  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
    EXPORTING
      formname           = 'ZSF_FI_ESTADO_CUENTAS'
    IMPORTING
      fm_name            = l_funcion
    EXCEPTIONS
      no_form            = 1
      no_function_module = 2
      OTHERS             = 3.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.


  CALL FUNCTION l_funcion
    EXPORTING
      tdname           = v_tdname
      lifnr            = lifnr-low
      kunnr            = kunnr-low
      flag             = flag
      bukrs            = p_bukrs
      total            = gv_total
      data             = it_alv
*    TABLES
*      it_data          = it_alv
    EXCEPTIONS
      formatting_error = 1
      internal_error   = 2
      send_error       = 3
      user_canceled    = 4
      OTHERS           = 5.


ENDFORM.                    " IMPRIMIR


*----------------------------------------------------------------------*
FORM pdf .
  DATA: carr_id TYPE sbook-carrid,
        cparam TYPE ssfctrlop,
        outop TYPE ssfcompop,
        fm_name TYPE rs38l_fnam.

  DATA: tab_otf_data TYPE ssfcrescl,
        pdf_tab LIKE tline OCCURS 0 WITH HEADER LINE,
        tab_otf_final TYPE itcoo OCCURS 0 WITH HEADER LINE,
        file_size TYPE i,
        bin_filesize TYPE i,
        file_name TYPE string,
        file_path TYPE string,
        full_path TYPE string.

*  outop-tddest = 'LP01'.
  cparam-no_dialog = 'X'.
*  cparam-preview = space.
  cparam-getotf = 'X'.


  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
       EXPORTING  formname           = 'ZSF_FI_ESTADO_CUENTAS'
*                 variant            = ' '
*                 direct_call        = ' '
       IMPORTING  fm_name            = fm_name
       EXCEPTIONS no_form            = 1
                  no_function_module = 2
                  OTHERS             = 3.

  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    EXIT.
  ENDIF.


* calling the generated function module
  CALL FUNCTION fm_name
    EXPORTING
      control_parameters = cparam
      output_options     = outop
      user_settings      = space
      lifnr              = lifnr-low
      kunnr              = kunnr-low
      flag               = flag
      bukrs              = p_bukrs
      total              = gv_total
      data               = it_alv
    IMPORTING
      job_output_info    = tab_otf_data
    EXCEPTIONS
      formatting_error   = 1
      internal_error     = 2
      send_error         = 3
      user_canceled      = 4
      OTHERS             = 5.

  IF sy-subrc <> 0.
*   error handling
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


  tab_otf_final[] = tab_otf_data-otfdata[].


  CALL FUNCTION 'CONVERT_OTF'
 EXPORTING
   format                      = 'PDF'
   max_linewidth               = 132
*   ARCHIVE_INDEX               = ' '
*   COPYNUMBER                  = 0
*   ASCII_BIDI_VIS2LOG          = ' '
 IMPORTING
   bin_filesize                = bin_filesize
*   BIN_FILE                    =
  TABLES
    otf                         = tab_otf_final
    lines                       = pdf_tab
 EXCEPTIONS
   err_max_linewidth           = 1
   err_format                  = 2
   err_conv_not_possible       = 3
   err_bad_otf                 = 4
   OTHERS                      = 5
          .
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.



  IF lifnr-low IS NOT INITIAL.
    CONCATENATE lifnr-low '.PDF' INTO file_name.
  ELSEIF kunnr-low IS NOT INITIAL.
    CONCATENATE kunnr-low '.PDF' INTO file_name.
  ENDIF.

  CALL METHOD cl_gui_frontend_services=>file_save_dialog
  EXPORTING
*    WINDOW_TITLE         =
      default_extension  = 'PDF'
      default_file_name    = file_name
*    FILE_FILTER          =
*    INITIAL_DIRECTORY    =
*    WITH_ENCODING        =
*    PROMPT_ON_OVERWRITE  = 'X'
    CHANGING
      filename             = file_name
      path                 = file_path
      fullpath             = full_path
*    USER_ACTION          =
*    FILE_ENCODING        =
*  EXCEPTIONS
*    CNTL_ERROR           = 1
*    ERROR_NO_GUI         = 2
*    NOT_SUPPORTED_BY_GUI = 3
*    others               = 4
          .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.




*************downloading the converted PDF data to your local PC********

  CALL FUNCTION 'GUI_DOWNLOAD'
    EXPORTING
     bin_filesize                    = bin_filesize
     filename                        = full_path
     filetype                        = 'BIN'
*   APPEND                          = ' '
*   WRITE_FIELD_SEPARATOR           = ' '
*   HEADER                          = '00'
*   TRUNC_TRAILING_BLANKS           = ' '
*   WRITE_LF                        = 'X'
*   COL_SELECT                      = ' '
*   COL_SELECT_MASK                 = ' '
*   DAT_MODE                        = ' '
*   CONFIRM_OVERWRITE               = ' '
*   NO_AUTH_CHECK                   = ' '
*   CODEPAGE                        = ' '
*   IGNORE_CERR                     = ABAP_TRUE
*   REPLACEMENT                     = '#'
*   WRITE_BOM                       = ' '
*   TRUNC_TRAILING_BLANKS_EOL       = 'X'
   IMPORTING
     filelength                      = file_size
    TABLES
      data_tab                        = pdf_tab
*   FIELDNAMES                      =
   EXCEPTIONS
     file_write_error                = 1
     no_batch                        = 2
     gui_refuse_filetransfer         = 3
     invalid_type                    = 4
     no_authority                    = 5
     unknown_error                   = 6
     header_not_allowed              = 7
     separator_not_allowed           = 8
     filesize_not_allowed            = 9
     header_too_long                 = 10
     dp_error_create                 = 11
     dp_error_send                   = 12
     dp_error_write                  = 13
     unknown_dp_error                = 14
     access_denied                   = 15
     dp_out_of_memory                = 16
     disk_full                       = 17
     dp_timeout                      = 18
     file_not_found                  = 19
     dataprovider_exception          = 20
     control_flush_error             = 21
     OTHERS                          = 22
            .
  IF sy-subrc <> 0.

  ENDIF.



ENDFORM.                    " PDF
