"Name: \PR:SAPMSPAD\FO:UPDATE_DEVICES\SE:BEGIN\EI
ENHANCEMENT 0 ZAF_OUT_MSPADF21.
IF RSPOSEL-DEVICE(1) = 'Z'.
 CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
   EXPORTING
    TITEL              = '--- ATTENTION ---'
    textline1          = '!!ATTENTION!! Please do not lock or delete this device. !!ATTENTION!!'.
ENDIF.
ENDENHANCEMENT.