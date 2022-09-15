FUNCTION zmhp_int_extract_receiver.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  IMPORTING
*"     VALUE(XML_STRING) TYPE  XSTRING
*"     VALUE(FILE_NAME) TYPE  CHAR70
*"  EXPORTING
*"     VALUE(RESPONSE) TYPE  BAPIRET1
*"----------------------------------------------------------------------

  DATA:
    filename TYPE string,
    message  TYPE bapiret1,
    xml      TYPE REF TO if_ixml_document,
    file     TYPE string VALUE 'Worker_Extract.xml'.
  zmhp_cl_int_xml=>create_xml_interface(
  EXPORTING
    xstring      = xml_string
  IMPORTING
    message      = DATA(ls_message)
    xml_document = xml ).
  DATA(object_collector) = NEW zmhp_cl_int_object_collector( ).
  object_collector->set_full_xml( xml = xml ).
  object_collector->read_integration_id(
    EXPORTING
      integration_id_path = zmhp_cl_int_constants=>integration_run_id_path
    IMPORTING
     message             = message
      integration_id      = DATA(int_id) ).
  file = file_name. "Duerr_Worker_SAP_Extract_20220228_120632.xml
  CONDENSE file.
  SPLIT file AT '_' INTO DATA(duerr) DATA(worker) DATA(sap) DATA(extract) DATA(date).
  REPLACE ALL OCCURRENCES OF '_' IN date WITH space.
  REPLACE ALL OCCURRENCES OF '.xml' IN date WITH space.
  CONDENSE date NO-GAPS.
  CLEAR file.
  file = duerr && '_' && worker && '_' && sap && '_' && extract && '_' && int_id  && '.' && date.
  TRANSLATE file TO UPPER CASE.
  CONDENSE file.
  IF xml_string IS NOT INITIAL.
    DATA(logical_filename) = CONV filename-fileintern( zmhp_cl_int_general_settings=>get_al11_down( ) ).
    CALL FUNCTION 'FILE_GET_NAME'
      EXPORTING
        logical_filename = logical_filename
        parameter_1      = file
      IMPORTING
        file_name        = filename
      EXCEPTIONS
        file_not_found   = 1
        OTHERS           = 2.
*    CALL FUNCTION 'AUTHORITY_CHECK_DATASET'
*      EXPORTING
**       PROGRAM          =
*        activity         = '36'
*        filename         = filename
*      EXCEPTIONS
*        no_authority     = 1
*        activity_unknown = 2
*        OTHERS           = 3.
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.
*    TRANSLATE filename TO UPPER CASE.
    TRY.
        OPEN DATASET filename FOR OUTPUT IN BINARY MODE.

        IF sy-subrc IS INITIAL.
          TRANSFER xml_string TO filename.
          CLOSE DATASET filename.
        ELSE.
          response-type = 'E'.
          response-message = 'E070 Receiverflow hat Fehler beim Speichern des Files'.
        ENDIF.
      CATCH cx_sy_file_authority INTO DATA(exception).
        response-type = 'E'.
        response-message = 'E070 Receiverflow hat Fehler beim Speichern des Files'.
    ENDTRY.
    IF sy-subrc IS INITIAL.
      IF exception IS NOT INITIAL.
        response-type = 'E'.
        response-message = 'E070 Receiverflow hat Fehler beim Speichern des Files'.
      ELSE.
        response-type = 'S'.
        response-message = 'E070 Receiverflow hat File erfolgreich gespeichert'.
*      MESSAGE 'D002/400 Receiverflow hat File erfolgreich gespeichert' TYPE 'S'.
      ENDIF.
    ELSE.
      response-type = 'E'.
      response-message = 'E070 Receiverflow hat Fehler beim Speichern des Files'.
    ENDIF.
  ELSE.
    response-type = 'W'.
    response-message = 'D002/400 XML ist leer'.
  ENDIF.






ENDFUNCTION.
