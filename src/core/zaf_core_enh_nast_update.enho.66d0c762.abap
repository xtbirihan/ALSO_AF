"Name: \PR:RSNAST00\FO:CHECK_KAPPLS_NOT_TO_PROCESS\SE:END\EI
ENHANCEMENT 0 ZAF_CORE_ENH_NAST_UPDATE.
*TS-180802 180116-143713-AR - Adobe Forms CORE Entwicklung
* An dieser Stelle kann gesteuert werden, ob ein NAST Satz verarbeitet werden soll oder nicht.
* In der "Vorverarbeitung" für den Schweizer Rechnungsprozess wollen wir nur Nachrichten verarbeien,
* die auf einem Rechnungsdrucker ausgegeben werden (Steuertabelle ZSD_INVPRT_LDEST).

  DATA: zz_l_ldest TYPE ldest.

  IF p_suff2 = 'ZAF_PREPARE'.
    CLEAR zz_l_ldest.
    SELECT SINGLE ldest
      FROM ZSD_INVPRT_LDEST
      INTO zz_l_ldest
      WHERE ldest = nast-ldest.
    IF sy-subrc <> 0.
      p_subrc = 4.    "NAST Satz wird nicht verarbeitet
*->TS-200402
      ELSE.
* Wir wollen nicht ZAF_PREPARE im NAST Satz speichern, denn das führt bei "Nachricht wiederholen"
* zu einer erneuten Vorverarbeitung, die wir nicht wollen. Daher wird beim letzten Aufruf dieser
* Formroutine das Kennzeichen entfernt. Der letzte Aufruf ist aus NAST_UPDATE.
        IF zcl_actebis_util=>is_call_from_program( im_program = 'RSNAST00' im_eventname = 'NAST_UPDATE') = abap_true.
          clear nast-dsuf2.
        ENDIF.
*<-TS-200402
    ENDIF.
  ENDIF.

ENDENHANCEMENT.
