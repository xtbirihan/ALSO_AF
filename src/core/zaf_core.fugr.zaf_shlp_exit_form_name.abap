FUNCTION zaf_shlp_exit_form_name .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCT
*"      RECORD_TAB STRUCTURE  SEAHLPRES
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_form_name,
           form_name TYPE zaf_form_name,
         END OF ty_form_name.

  DATA: l_field_name    TYPE dfies-lfieldname,
        l_it_form_names TYPE TABLE OF ty_form_name.

* EXIT immediately, if you do not want to handle this step
  IF callcontrol-step <> 'SELONE' AND
     callcontrol-step <> 'SELECT' AND
     " AND SO ON
     callcontrol-step <> 'DISP'.
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP SELONE  (Select one of the elementary searchhelps)
*"----------------------------------------------------------------------
* This step is only called for collective searchhelps. It may be used
* to reduce the amount of elementary searchhelps given in SHLP_TAB.
* The compound searchhelp is given in SHLP.
* If you do not change CALLCONTROL-STEP, the next step is the
* dialog, to select one of the elementary searchhelps.
* If you want to skip this dialog, you have to return the selected
* elementary searchhelp in SHLP and to change CALLCONTROL-STEP to
* either to 'PRESEL' or to 'SELECT'.
  IF callcontrol-step = 'SELONE'.
*   PERFORM SELONE .........
    EXIT.
  ENDIF.

*"----------------------------------------------------------------------
* STEP PRESEL  (Enter selection conditions)
*"----------------------------------------------------------------------
* This step allows you, to influence the selection conditions either
* before they are displayed or in order to skip the dialog completely.
* If you want to skip the dialog, you should change CALLCONTROL-STEP
* to 'SELECT'.
* Normaly only SHLP-SELOPT should be changed in this step.
  IF callcontrol-step = 'PRESEL'.
*   PERFORM PRESEL ..........
    EXIT.
  ENDIF.
*"----------------------------------------------------------------------
* STEP SELECT    (Select values)
*"----------------------------------------------------------------------
* This step may be used to overtake the data selection completely.
* To skip the standard seletion, you should return 'DISP' as following
* step in CALLCONTROL-STEP.
* Normally RECORD_TAB should be filled after this step.
* Standard function module F4UT_RESULTS_MAP may be very helpfull in this
* step.
  IF callcontrol-step = 'SELECT'.
    "finde die eindeutige Forumlarnamen in ZAF_DOCUMENT_ID
    SELECT DISTINCT formname AS form_name
      FROM zaf_document_id
      INTO CORRESPONDING FIELDS OF TABLE l_it_form_names.

    READ TABLE shlp_tab INTO DATA(l_wa_shlp) INDEX 1.
    IF sy-subrc = 0.
      LOOP AT l_wa_shlp-fielddescr INTO DATA(l_wa_field).
        l_field_name = l_wa_field-fieldname.

        CALL FUNCTION 'F4UT_PARAMETER_RESULTS_PUT'
          EXPORTING
            parameter         = l_wa_field-fieldname
            fieldname         = l_field_name
          TABLES
            shlp_tab          = shlp_tab
            record_tab        = record_tab
            source_tab        = l_it_form_names
          CHANGING
            shlp              = shlp
            callcontrol       = callcontrol
          EXCEPTIONS
            parameter_unknown = 1
            OTHERS            = 2.
      ENDLOOP.
    ENDIF.

    IF lines( record_tab ) > 0.
      callcontrol-step = 'DISP'.
    ELSE.
      callcontrol-step = 'EXIT'.
    ENDIF.
  ENDIF.
*"----------------------------------------------------------------------
* STEP DISP     (Display values)
*"----------------------------------------------------------------------
* This step is called, before the selected data is displayed.
* You can e.g. modify or reduce the data in RECORD_TAB
* according to the users authority.
* If you want to get the standard display dialog afterwards, you
* should not change CALLCONTROL-STEP.
* If you want to overtake the dialog on you own, you must return
* the following values in CALLCONTROL-STEP:
* - "RETURN" if one line was selected. The selected line must be
*   the only record left in RECORD_TAB. The corresponding fields of
*   this line are entered into the screen.
* - "EXIT" if the values request should be aborted
* - "PRESEL" if you want to return to the selection dialog
* Standard function modules F4UT_PARAMETER_VALUE_GET and
* F4UT_PARAMETER_RESULTS_PUT may be very helpfull in this step.
  IF callcontrol-step = 'DISP'.
*   PERFORM AUTHORITY_CHECK TABLES RECORD_TAB SHLP_TAB
*                           CHANGING SHLP CALLCONTROL.
    EXIT.
  ENDIF.
ENDFUNCTION.
