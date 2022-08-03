*&---------------------------------------------------------------------*
*& Report  ZFI_ACT_NIVEL_TESORERIA
*&
*&---------------------------------------------------------------------*
*&  Desarrollo para actualización de Niveles de tesorería(FDLEV)
*&  en la BSIS Y BSAS para aquellos docuemntos que contienen niveles de
*&  tesorería en la BSEG.
*&  N° sol:      10604
*&  Autor:       JGP
*&  Empresa:     C.1.C. Consulting C,A.
*&  fecha:       22.04.2022
*&  Transacción: N/A
*&---------------------------------------------------------------------*

REPORT  zfi_act_nivel_tesoreria.

INCLUDE zfi_act_nivel_tesoreria_top.
INCLUDE zfi_act_nivel_tesoreria_f01.


AT SELECTION-SCREEN ON p_bukrs.
  PERFORM valid_param.

AT SELECTION-SCREEN ON p_gjahr.
  PERFORM valid_param.

AT SELECTION-SCREEN ON s_monat.
  PERFORM valid_param.

START-OF-SELECTION.
  IMPORT p_bukrs p_gjahr s_monat s_belnr s_hkont  FROM MEMORY ID 'AJUST_FDLEV_BSIS_BSAS'.

  IF p_bukrs IS NOT INITIAL AND p_gjahr IS NOT INITIAL AND s_monat IS NOT INITIAL.
    PERFORM get_data.
  ENDIF.

END-OF-SELECTION.
