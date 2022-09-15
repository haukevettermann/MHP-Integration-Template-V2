FUNCTION zmhp_hromfb_get_abteilung_new .
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     REFERENCE(PLVAR) TYPE  PLOG-PLVAR OPTIONAL
*"     REFERENCE(OTYPE) TYPE  PLOG-OTYPE DEFAULT 'O'
*"     REFERENCE(OBJID) TYPE  PLOG-OBJID OPTIONAL
*"     REFERENCE(BEGDA) TYPE  P1000-BEGDA DEFAULT SY-DATUM
*"     REFERENCE(ENDDA) TYPE  P1000-ENDDA DEFAULT SY-DATUM
*"     REFERENCE(STATUS) TYPE  P1000-ISTAT DEFAULT '1'
*"     REFERENCE(CHECK_AUTH) TYPE  FLAG OPTIONAL
*"     REFERENCE(HISTORICAL) TYPE  FLAG OPTIONAL
*"  EXPORTING
*"     REFERENCE(GESELLSCHAFTER) TYPE  PLOG-OBJID
*"     REFERENCE(GESCHAEFTSFUEHRUNG) TYPE  PLOG-OBJID
*"     REFERENCE(HAUPTABTEILUNG) TYPE  PLOG-OBJID
*"     REFERENCE(ABTEILUNG) TYPE  PLOG-OBJID
*"     REFERENCE(GRUPPE) TYPE  PLOG-OBJID
*"     REFERENCE(TEAM) TYPE  PLOG-OBJID
*"     REFERENCE(ORG_LVL7) TYPE  PLOG-OBJID
*"     REFERENCE(ORG_LVL8) TYPE  PLOG-OBJID
*"     REFERENCE(ORG_LVL9) TYPE  PLOG-OBJID
*"     REFERENCE(TX_GESELLSCHAFTER) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_GESCHAEFTSFUEHRUNG) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_HAUPTABTEILUNG) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_ABTEILUNG) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_GRUPPE) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_ORGEH) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_TEAM) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_LVL7) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_LVL8) TYPE  OBJEC-SHORT
*"     REFERENCE(TX_LVL9) TYPE  OBJEC-SHORT
*"     REFERENCE(TXLG_GESELLSCHAFTER) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_GESCHAEFTSFUEHRUNG) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_HAUPTABTEILUNG) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_ABTEILUNG) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_GRUPPE) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_ORGEH) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_TEAM) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_LVL7) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_LVL8) TYPE  OBJEC-STEXT
*"     REFERENCE(TXLG_LVL9) TYPE  OBJEC-STEXT
*"     REFERENCE(STABS) TYPE  CHAR1
*"     REFERENCE(LEVEL) TYPE  OBJEC-STEXT
*"     REFERENCE(DIFF_LEVEL) TYPE  OBJEC-STEXT
*"  EXCEPTIONS
*"      OBJID_INITIAL
*"      NOT_FOUND
*"      ERR_INFTY1003
*"      INTERNAL_ERROR
*"      NO_PLVAR
*"----------------------------------------------------------------------

  TYPES: BEGIN OF ty_objec1.
           INCLUDE STRUCTURE objec.
           TYPES:   level_new   TYPE slevel_d,
           teamisgroup TYPE c,
         END OF ty_objec1.

  DATA: lwa_objec1 TYPE ty_objec1,
        ls_objec1  TYPE ty_objec1,
        lit_objec1 TYPE TABLE OF ty_objec1.
* DESCHUWE 04.06.2004

* DESCHUWE 04.06.2004 genutztes altes Coding
  DATA: l_plvar       TYPE plog-plvar
      , lt_struc_tab  TYPE TABLE OF struc
      , lt_objec_tab  TYPE TABLE OF objec
      , l_levelausn   TYPE numc2
      , l_level       TYPE numc2
      , l_i1003       TYPE p1003 OCCURS 1
      , l_p1003       TYPE p1003
      , l_loops       TYPE i
      , l_objid       TYPE plog-objid
      , l_short       TYPE objec-short
      , l_ausnahme    TYPE i
      , l_stabs       TYPE p1003-stabs
      , l_offset      TYPE i
      , l_index       TYPE i
      , l_teamisgroup TYPE flag
      , lt_hrp1000    TYPE TABLE OF hrp1000                "DEPRIFRI_20161111
      , lwa_hrp1000   TYPE hrp1000
      .

*DEMANPAS 09.11.2010 Stabstellen auf Abteilungsebene richtig anzeigen
  DATA: l_stab_index TYPE i.

  FIELD-SYMBOLS: <struc> TYPE struc
               , <objec> TYPE objec
               .

* Rückgabewerte löschen
  CLEAR: hauptabteilung, abteilung, gruppe.
  CLEAR: tx_hauptabteilung
       , tx_abteilung
       , tx_gruppe
       , tx_orgeh
       .

  l_plvar = plvar.
  IF l_plvar IS INITIAL.
*   Aktive Planvariante ermitteln
    CALL FUNCTION 'RH_GET_PLVAR'
      EXPORTING
        no_message = 'X'
      IMPORTING
        plvar      = l_plvar
*       WFPLVAR    =
      EXCEPTIONS
        no_plvar   = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
*     MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*             WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      RAISE no_plvar.
    ENDIF.

  ENDIF.

  IF objid IS INITIAL.
*   Org-Einheit muß übergeben werden
    RAISE objid_initial.
  ENDIF.

  SELECT SINGLE *
    FROM hrp1000
    INTO lwa_hrp1000
   WHERE plvar = l_plvar
     AND otype = 'O'
     AND objid = objid
     AND endda GE begda
     AND begda LE endda.

  IF sy-subrc NE 0.
*    RAISE not_found.                         "DEPRIFRI_20161111
    IF historical IS NOT INITIAL.             "DEPRIFRI_20161111 ->>
      CLEAR: lt_hrp1000[], lwa_hrp1000.
      SELECT *
        FROM hrp1000
        INTO TABLE lt_hrp1000
       WHERE plvar = l_plvar
         AND otype = 'O'
         AND objid = objid.
      SORT lt_hrp1000 BY endda DESCENDING.
      READ TABLE lt_hrp1000 INTO lwa_hrp1000 INDEX 1.
      IF sy-subrc NE 0.
        RAISE not_found.
      ENDIF.
    ELSE.
      RAISE not_found.
    ENDIF.                                    "DEPRIFRI_20161111 -<<
  ENDIF.

* Organisationsstruktur lesen
* Baum mit Organisationseinheiten ab aktueller OE aufwärts
  CALL FUNCTION 'RH_PM_GET_STRUCTURE'
    EXPORTING
      plvar           = l_plvar
      otype           = otype
      objid           = objid
      begda           = begda
      endda           = endda
      status          = status
      wegid           = 'O-O'
*     77AW_INT        = ' '
      authy           = check_auth
*     DEPTH           = 0
*     CHECK_OBJECT    = ' '
*     PROGRESS_INDICATOR = ' '
*     SVECT           =
*     ACTIV           = 'X'
*     BUFFER_MODE     =
    TABLES
      objec_tab       = lt_objec_tab
      struc_tab       = lt_struc_tab
*     GDSTR_TAB       =
    EXCEPTIONS
      not_found       = 1
      ppway_not_found = 2
      OTHERS          = 3.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    RAISE not_found.
  ENDIF.

* Level ermitteln (Standardfall)
  DESCRIBE TABLE lt_struc_tab LINES l_level.
* Standardfall ist:
*  6 = Team
*  5 = Gruppe
*  4 = Abteilung
*  3 = Hauptabteilung

* DESCHUWE 04.06.2004 - Ende altes Coding

* DESCHUWE 04.06.2004 - Coding entfernt
*  l_offset = 0.
*  l_ausnahme = 0.
* DESCHUWE 04.06.2004 - Coding entfernt

* DESCHUWE 04.06.2004 - Beginn neues Coding
  lit_objec1[] = lt_objec_tab[].

  LOOP AT lit_objec1 INTO lwa_objec1.

* Ermitteln, ob eine Abweichung in der Strukturzuordnung vorliegt
    CALL FUNCTION 'RH_READ_INFTY'
      EXPORTING
        with_stru_auth       = check_auth
        plvar                = l_plvar
        otype                = otype
        objid                = lwa_objec1-objid
        infty                = '1003'
        istat                = status
*       EXTEND               = 'X'
*       SUBTY                = ' '
        begda                = begda
        endda                = endda
*       CONDITION            = '00000'
*       INFTB                = '1'
*       SORT                 = 'X'
*       VIA_T777D            = ' '
      TABLES
        innnn                = l_i1003
*       OBJECTS              =
      EXCEPTIONS
        all_infty_with_subty = 1
        nothing_found        = 2
        no_objects           = 3
        wrong_condition      = 4
        wrong_parameters     = 5
        OTHERS               = 6.
    IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      IF sy-subrc = 2.
*     keine Ausnahme gefunden - Standard annehmen
        lwa_objec1-level_new = l_level.
      ELSE.
        RAISE err_infty1003.
      ENDIF.
    ENDIF.

    l_level = l_level - 1.

    MODIFY lit_objec1 FROM lwa_objec1.

  ENDLOOP.

* Bezeichnung Orgeinheit ermitteln.
  READ TABLE lt_objec_tab ASSIGNING <objec>
                          WITH KEY objid = objid.
  IF sy-subrc = 0.
    l_short = tx_orgeh = <objec>-short.
    txlg_orgeh = <objec>-stext.
  ENDIF.


  LOOP AT lit_objec1 INTO lwa_objec1.
* Sonderfall (Ausnahme von der Ausnahme)
    IF lwa_objec1-teamisgroup = 'X'.
      gruppe    = lwa_objec1-objid.
      tx_gruppe = lwa_objec1-short.
    ENDIF.
    CASE lwa_objec1-level_new.
      WHEN 9.
        org_lvl9 = lwa_objec1-objid.
        tx_lvl9 = lwa_objec1-short.
        txlg_lvl9 = lwa_objec1-stext.
      WHEN 8.
        org_lvl8 = lwa_objec1-objid.
        tx_lvl8 = lwa_objec1-short.
        txlg_lvl8 = lwa_objec1-stext.
      WHEN 7.
        org_lvl7 = lwa_objec1-objid.
        tx_lvl7 = lwa_objec1-short.
        txlg_lvl7 = lwa_objec1-stext.
      WHEN 6.
        team      = lwa_objec1-objid.
        tx_team   = lwa_objec1-short.
        txlg_team = lwa_objec1-stext.
      WHEN 5.
        gruppe      = lwa_objec1-objid.
        tx_gruppe   = lwa_objec1-short.
        txlg_gruppe = lwa_objec1-stext.
      WHEN 4.
        abteilung      = lwa_objec1-objid.
        tx_abteilung   = lwa_objec1-short.
        txlg_abteilung = lwa_objec1-stext.
      WHEN 3.
        hauptabteilung      = lwa_objec1-objid.
        tx_hauptabteilung   = lwa_objec1-short.
        txlg_hauptabteilung = lwa_objec1-stext.
      WHEN 2.
        geschaeftsfuehrung      = lwa_objec1-objid.
        tx_geschaeftsfuehrung   = lwa_objec1-short.
        txlg_geschaeftsfuehrung = lwa_objec1-stext.
      WHEN 1.
        gesellschafter      = lwa_objec1-objid.
        tx_gesellschafter   = lwa_objec1-short.
        txlg_gesellschafter = lwa_objec1-stext.
      WHEN OTHERS.
*       RAISE internal_error.
    ENDCASE.

  ENDLOOP.

*dewischa(20160725)
* Über die lit_object1 lesen und bei der gesuchten OrgID das Level mit ausgeben -> Exportparaemter lv_level
  LOOP AT lit_objec1 INTO ls_objec1 WHERE objid = objid.
    IF sy-subrc EQ 0.
      level = ls_objec1-level_new.
    ENDIF.
  ENDLOOP.
  CLEAR ls_objec1.
*dewischa(20160725)

* Bei Kennzeichnung als Stabsstelle, sollen die        " PECTBA20040308
* Personen direkt an der Hauptabteilung angezeigt
* werden
*DEMANPAS 09.11.2010 Wenn Gruppe = Stabstelle dann ORGEH zur Abteilung anzeigen
*  DESCRIBE TABLE lit_objec1 LINES l_stab_index.
*
*  IF l_stabs = 'X'.
*    IF l_stab_index >= 5.
*      CLEAR: gruppe,    tx_gruppe.
*    ELSE.
*      CLEAR: gruppe,    tx_gruppe.
*      CLEAR: abteilung, tx_abteilung.
*      CLEAR: hauptabteilung, tx_hauptabteilung.
*    ENDIF.
*  ENDIF.

  CLEAR lwa_objec1.
  READ TABLE lit_objec1 INTO lwa_objec1 INDEX 1.

  IF l_stabs = 'X'.
    stabs = 'X'.
*    CASE lwa_objec1-level_new.
*      WHEN 1.
*      WHEN 2.
*        CLEAR: team,      tx_team.
*        CLEAR: gruppe,    tx_gruppe.
*        CLEAR: abteilung, tx_abteilung.
*        CLEAR: hauptabteilung, tx_hauptabteilung.
*        CLEAR: geschaeftsfuehrung, tx_geschaeftsfuehrung.
*      WHEN 3.
*        CLEAR: team,      tx_team.
*        CLEAR: gruppe,    tx_gruppe.
*        CLEAR: abteilung, tx_abteilung.
*        CLEAR: hauptabteilung, tx_hauptabteilung.
*      WHEN 4.
*        CLEAR: team,      tx_team.
*        CLEAR: gruppe,    tx_gruppe.
*        CLEAR: abteilung, tx_abteilung.
*      WHEN 5.
*        CLEAR: team,      tx_team.
*        CLEAR: gruppe,    tx_gruppe.
*      WHEN 6.
*        CLEAR: team,      tx_team.
*    ENDCASE.
  ENDIF.

* DESCHUWE 04.06.2004 - Ende neues Coding

************************************************************************
* DESCHUWE 04.06.2004 altes Coding - komplett entfernt                 *
************************************************************************


** Ermitteln, ob eine Abweichung in der Strukturzuordnung vorliegt
*  CALL FUNCTION 'RH_READ_INFTY'
*    EXPORTING
*      with_stru_auth             = check_auth
*      plvar                      = l_plvar
*      otype                      = otype
*      objid                      = objid
*      infty                      = '1003'
*      istat                      = status
**     EXTEND                     = 'X'
**     SUBTY                      = ' '
*      begda                      = begda
*      endda                      = endda
**     CONDITION                  = '00000'
**     INFTB                      = '1'
**     SORT                       = 'X'
**     VIA_T777D                  = ' '
*    TABLES
*      innnn                      = l_i1003
**     OBJECTS                    =
*    EXCEPTIONS
*      all_infty_with_subty       = 1
*      nothing_found              = 2
*      no_objects                 = 3
*      wrong_condition            = 4
*      wrong_parameters           = 5
*      OTHERS                     = 6.
*  IF sy-subrc <> 0.
**   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**           WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*    IF sy-subrc = 2.
**     keine Ausnahme gefunden - Standard annehmen
*    ELSE.
*      RAISE err_infty1003.
*    ENDIF.
*  ELSE.
*    READ TABLE l_i1003 INTO l_p1003 INDEX 1.
*    IF sy-subrc = 0.
*      IF l_p1003-yyhier IS NOT INITIAL.
*        l_levelausn = l_p1003-yyhier.
*        IF l_levelausn = l_level.
**         keine Abweichung
*          IF l_levelausn > 5.
*            l_offset = l_levelausn - 5.
*          ENDIF.
*        ELSE.
*          IF l_levelausn > l_level.
*            l_ausnahme = l_levelausn - l_level.
*            IF l_levelausn = 6 AND l_level = 5.
*              l_teamisgroup = 'X'.
*            ENDIF.
*          ELSE.
**           l_levelausn < l_level
*          ENDIF.
*          l_level = l_levelausn.
*        ENDIF.
*      ELSE.
*        l_stabs = l_p1003-stabs.
*        DESCRIBE TABLE lt_struc_tab LINES l_level.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
** Jetzt steht l_level fest mit folgenden Bedeutungen:
**  6 = Team
**  5 = Gruppe
**  4 = Abteilung
**  3 = Hauptabteilung
*
*
*  l_objid = objid.
*  READ TABLE lt_objec_tab ASSIGNING <objec>
*                          WITH KEY objid = l_objid.
*  IF sy-subrc = 0.
*    l_short = tx_orgeh = <objec>-short.
*  ENDIF.
*
** Sonderfall (Ausnahme von der Ausnahme)
*  IF l_teamisgroup = 'X'.
*    gruppe    = l_objid.
*    tx_gruppe = l_short.
*  ENDIF.
*
** Level passt nicht
*  IF l_level < 3.
*    EXIT.
*  ENDIF.
*
*
*
*  l_loops = l_level - 2 - l_ausnahme.
*  DO l_loops TIMES.
*    l_index = sy-index + l_offset.
*    IF l_level >= 3 AND l_level <= 5.
*      READ TABLE lt_struc_tab ASSIGNING <struc> INDEX l_index.
*      IF sy-subrc <> 0.
*        RAISE not_found.
*      ENDIF.
*      l_objid = <struc>-objid.
*      READ TABLE lt_objec_tab ASSIGNING <objec>
*                              WITH KEY objid = l_objid.
*      IF sy-subrc = 0.
*        l_short = <objec>-short.
*      ENDIF.
*    ENDIF.
*    CASE l_level.
*      WHEN 5.
*        gruppe    = l_objid.
*        tx_gruppe = l_short.
*      WHEN 4.
*        abteilung    = l_objid.
*        tx_abteilung = l_short.
*      WHEN 3.
*        hauptabteilung    = l_objid.
*        tx_hauptabteilung = l_short.
*      WHEN OTHERS.
**       RAISE internal_error.
*    ENDCASE.
*    l_level = l_level - 1 - l_ausnahme.
*    IF l_ausnahme > 0.
*      l_ausnahme = l_ausnahme - 1.
*    ENDIF.
*  ENDDO.
*
** Bei Kennzeichnung als Stabsstelle, sollen die        " PECTBA20040308
** Personen direkt an der Hauptabteilung angezeigt
** werden
*  IF l_stabs = 'X'.
*    CLEAR: gruppe,    tx_gruppe.
*    CLEAR: abteilung, tx_abteilung.
*  ENDIF.
* DESCHUWE 04.06.2004

ENDFUNCTION.
