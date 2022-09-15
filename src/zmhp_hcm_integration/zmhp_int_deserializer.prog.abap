*&---------------------------------------------------------------------*
*& Report ZMHP_INT_TEST_XML
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zmhp_int_deserializer.
TYPES BEGIN OF file_list_sorted.
TYPES: file_name TYPE string.
TYPES timestamp TYPE timestamp.
TYPES END OF file_list_sorted.
DATA:
  xstring          TYPE xstring,
  directory        TYPE epsf-epsdirnam,
  filename         TYPE string,
  filename_archive TYPE string,
*  filename         TYPE string,
  xml              TYPE REF TO if_ixml_document,
  file_list        TYPE TABLE OF zmhp_file_list,
  file_list_sorted TYPE TABLE OF file_list_sorted,
  timestamp        TYPE c LENGTH 13,
  id               TYPE vrm_id,
  value_list       TYPE vrm_values,
  molga            TYPE molga,
  otype            TYPE hrotype,
  sequence         TYPE seqnr,
  pernr            TYPE pernr_d,
  oracle_id        TYPE zmhp_dd_element.
* Begin of selection Screen
SELECTION-SCREEN BEGIN OF BLOCK frame1.
PARAMETERS: p_file   TYPE text200 OBLIGATORY,
            p_full   RADIOBUTTON GROUP r2,
            p_list   RADIOBUTTON GROUP r2,
            p_rarch  TYPE boole_d,
            p_scenar TYPE zmhp_dd_scenario AS LISTBOX VISIBLE LENGTH 30 OBLIGATORY.
*            p_otype  TYPE hrotype OBLIGATORY, "ZMHP_DD_EXT_TYPE
*            p_dt     TYPE boole_d.
SELECTION-SCREEN   END OF BLOCK frame1.
SELECTION-SCREEN BEGIN OF BLOCK frame2.
SELECT-OPTIONS:
p_sid  FOR pernr,
p_cid  FOR oracle_id. "hrobjid
PARAMETERS:
  p_arch TYPE boole_d,
  p_test TYPE boole_d.
SELECTION-SCREEN   END OF BLOCK frame2.

AT SELECTION-SCREEN OUTPUT.
  SELECT * FROM zmhp_int_objects INTO TABLE @DATA(objects).
  LOOP AT objects ASSIGNING FIELD-SYMBOL(<object>).
    APPEND INITIAL LINE TO value_list ASSIGNING FIELD-SYMBOL(<value>).
    <value>-key = <object>-molga && '-' && <object>-object && '-' && <object>-object_seqnr.
    <value>-text = <object>-scenario && ' | ' && <object>-molga.
  ENDLOOP.
  id = 'P_SCENAR'.
  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id     = id
      values = value_list.

CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    CLASS-METHODS:
      handle_double_click
                  FOR EVENT double_click OF cl_gui_alv_grid
        IMPORTING e_row     "Zeile
                  e_column. "Spalte                    "#EC NEEDED

ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.
  METHOD handle_double_click.
    PERFORM handle_double_click
      USING e_row               "Zeile
            e_column.           "Spalte
  ENDMETHOD.                    "handle_double_click
ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

START-OF-SELECTION.
* Set otype and sequence in order to read customizing.
  IF p_scenar+0(1) = '*'.
    molga = p_scenar+0(1).
    otype = p_scenar+2(2).
    IF otype+1 = '-'.
      otype = p_scenar+2(1).
      sequence = p_scenar+4(3).
    ELSE.
      sequence = p_scenar+5(3).
    ENDIF.
  ELSE.
    molga = p_scenar+0(2).
    otype = p_scenar+3(2).
    IF otype+1 = '-'.
      otype = p_scenar+3(1).
      sequence = p_scenar+4(3).
    ELSE.
      sequence = p_scenar+6(3).
    ENDIF.
  ENDIF.
* End of selection Screen
*  TRANSLATE p_file TO UPPER CASE.
* Set static attributes
  zmhp_cl_int_statics=>read_archive = p_rarch.
  zmhp_cl_int_statics=>archive = p_arch.
  zmhp_cl_int_statics=>test_run = p_test.
** Get physical path
  DATA(logical_filename) = COND filename-fileintern( WHEN p_rarch = abap_false THEN zmhp_cl_int_general_settings=>get_al11_down( )
                                                   WHEN p_rarch = abap_true THEN zmhp_cl_int_general_settings=>get_al11_archive( ) ).
  CALL FUNCTION 'FILE_GET_NAME'
    EXPORTING
      logical_filename = logical_filename
*     parameter_1      = p_file
    IMPORTING
      file_name        = directory
    EXCEPTIONS
      file_not_found   = 1
      OTHERS           = 2.
* Get list of files
  IF p_list = abap_true.
    DATA(mask) = CONV epsf-epsfilnam( COND #( WHEN strlen( p_file ) > 40 THEN p_file(40) ELSE p_file ) && '*' ).
    CALL FUNCTION 'ZMHP_INT_GET_DIRECTORY_LISTING'
      EXPORTING
        dir_name               = directory
        file_mask              = mask
      TABLES
        dir_list               = file_list
      EXCEPTIONS
        invalid_eps_subdir     = 1
        sapgparam_failed       = 2
        build_directory_failed = 3
        no_authorization       = 4
        read_directory_failed  = 5
        too_many_read_errors   = 6
        empty_directory_list   = 7
        OTHERS                 = 8.
  ELSE.
    APPEND INITIAL LINE TO file_list ASSIGNING FIELD-SYMBOL(<new_file>).
    <new_file>-name = p_file.
  ENDIF.

  IF file_list IS NOT INITIAL.
*  build sortable list with timestamp.  EXTRACT timestamp from name.
    LOOP AT file_list ASSIGNING FIELD-SYMBOL(<file>).
      APPEND INITIAL LINE TO file_list_sorted ASSIGNING FIELD-SYMBOL(<file_sorted>).
      <file_sorted>-file_name = <file>-name.
      SPLIT <file>-name AT '_' INTO DATA(dont_care) DATA(dont_care2).
      SPLIT dont_care2 AT '.' INTO DATA(dont_care3) timestamp.
      <file_sorted>-timestamp = timestamp.
      CLEAR timestamp.
    ENDLOOP.
* Sort list by timestamp oldest to newest and ignore files that have no timestamp
    SORT file_list_sorted BY timestamp ASCENDING.
*  DELETE file_list_sorted WHERE timestamp IS INITIAL OR timestamp = 0.
    LOOP AT file_list_sorted ASSIGNING <file_sorted>.
* get physical path
      CALL FUNCTION 'FILE_GET_NAME'
        EXPORTING
          logical_filename = logical_filename
          parameter_1      = <file_sorted>-file_name
        IMPORTING
          file_name        = filename
        EXCEPTIONS
          file_not_found   = 1
          OTHERS           = 2.
      IF filename IS INITIAL.
        filename = '/SAPtmp/' && p_file.
      ENDIF.

* decrypt and go on with decrypted file
*      zmhp_cl_int_utility=>decrypt_extract( EXPORTING filename = filename IMPORTING filename_dec = filename ).

      IF filename IS INITIAL.
        MESSAGE 'Fehler bei der Entschlüsselung' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ELSE.
        OPEN DATASET filename FOR INPUT IN BINARY MODE.
        IF sy-subrc IS INITIAL.
          READ DATASET filename INTO xstring.
          CLOSE DATASET filename.
*          DELETE DATASET filename.
          MESSAGE 'Erfolgreich heruntergeladen' TYPE 'S'.
        ELSE.
          MESSAGE 'Fehler beim Herunterladen' TYPE 'S' DISPLAY LIKE 'E'.
          EXIT.
        ENDIF.
      ENDIF.

      zmhp_cl_int_xml=>create_xml_interface(
        EXPORTING
          xstring      = xstring
        IMPORTING
          message      = DATA(message)
          xml_document = xml ).
      IF message IS NOT INITIAL.
        IF sy-batch IS INITIAL.
          MESSAGE ID message-id TYPE 'S' NUMBER message-number DISPLAY LIKE 'E'.
        ELSE.
          WRITE message-id(message-number).
        ENDIF.
        EXIT.
      ENDIF.
      DATA(object_collector) = NEW zmhp_cl_int_object_collector( sequence = sequence ).
      object_collector->set_full_xml( xml = xml ).
      object_collector->read_customzing_objects( molga = molga object = CONV #( otype ) sequence = sequence ).
* Collect objects based on object customizing
      object_collector->collect_objects( ).
      LOOP AT object_collector->objects ASSIGNING FIELD-SYMBOL(<object>). "WHERE id_sap IS NOT INITIAL. "JMB20211018 I - Proceed only employees with a SAP PERNR
        IF ( <object>-object_seqnr = 001 OR <object>-object_seqnr = 002 OR <object>-object_seqnr = 003 OR <object>-object_seqnr = 004
          OR <object>-object_seqnr = 005 OR <object>-object_seqnr = 006 OR <object>-object_seqnr = 007 ) AND <object>-object = zmhp_cl_int_constants=>person.
          IF <object>-id_sap IS INITIAL.
            CONTINUE.
          ENDIF.
        ENDIF.
        IF p_sid IS NOT INITIAL.
          CHECK <object>-id_sap IN p_sid.
        ENDIF.
        IF p_cid IS NOT INITIAL.
          CHECK <object>-id_cloud IN p_cid.
        ENDIF.
* Instantiate Object Handler
        <object>-object_handler = NEW zmhp_cl_int_object_handler( molga       = <object>-molga
                                                                  bukrs       = <object>-bukrs
                                                                  object_type = <object>-object
                                                                  xml_node    = <object>-xml_node
                                                                  cloud_id    = CONV #( <object>-id_cloud )
                                                                  cloud_pernr = <object>-pernr_cloud
                                                                  sap_id      = CONV #( <object>-id_sap )
                                                                  int_run     = <object>-int_run
                                                                  seqnr       = <object>-object_seqnr ).

        "Read customizing (customizing is a singleton object and will only be read once per object and molga
        <object>-object_handler->read_customizing( object_type = <object>-object object_seqnr = <object>-object_seqnr ).

        "Start of processing the customizing.
        <object>-object_handler->process_customzing( ).
      ENDLOOP.

      "Archive file
      IF p_test = abap_false AND p_arch = abap_true AND p_rarch IS INITIAL.

* Get physical path
        DATA(logical_filename2) = CONV filename-fileintern( 'ZMHP_HCM_ORACLE_EXTRACTS_ARCHIVE' ).
*        DATA(logical_filename2) = COND filename-fileintern( WHEN sy-sysid = 'D02' THEN 'ZMHP_HCM_ORACLE_EXTRACTS_ARCHIVE_DEV'
*                                                            WHEN sy-sysid = 'Q02' THEN 'ZMHP_HCM_ORACLE_EXTRACTS_ARCHIVE_QS'
*                                                            WHEN sy-sysid = 'P02' THEN 'ZMHP_HCM_ORACLE_EXTRACTS_ARCHIVE_PROD' ).
        CALL FUNCTION 'FILE_GET_NAME'
          EXPORTING
            logical_filename = logical_filename2
            parameter_1      = <file_sorted>-file_name
          IMPORTING
            file_name        = filename_archive
          EXCEPTIONS
            file_not_found   = 1
            OTHERS           = 2.
        TRY.
            OPEN DATASET filename_archive FOR OUTPUT IN BINARY MODE.
            IF sy-subrc IS INITIAL.
              TRANSFER xstring TO filename_archive.
              CLOSE DATASET filename_archive.
              IF sy-subrc IS INITIAL.
                DELETE DATASET filename.
              ENDIF.
            ELSE.
              MESSAGE 'Fehler beim Archivieren' TYPE 'S' DISPLAY LIKE 'E'.
            ENDIF.
          CATCH cx_sy_file_authority INTO DATA(exception).
        ENDTRY.
      ENDIF.
    ENDLOOP.
  ELSE.
    IF sy-batch IS INITIAL.
      MESSAGE 'No files found' TYPE 'S' DISPLAY LIKE 'E'.
    ELSE.
      WRITE 'No files found'.
    ENDIF.
  ENDIF.
  DATA(alv) = NEW zmhp_cl_int_alv( testrun = p_test ).
  IF sy-batch IS INITIAL.
*    DATA: lo_gui_splitter TYPE REF TO cl_gui_splitter_container.
*    DATA: lo_alv_grid_1 TYPE REF TO cl_gui_alv_grid.
*    DATA: lo_alv_grid_2 TYPE REF TO cl_gui_alv_grid.
*    DATA: lo_alv_grid_3 TYPE REF TO cl_gui_alv_grid.
*    DATA: lo_container_1 TYPE REF TO cl_gui_container.
*    DATA: lo_container_2 TYPE REF TO cl_gui_container.
*    DATA: lo_container_3 TYPE REF TO cl_gui_container.
*    DATA: layout TYPE lvc_s_layo.
** Create splitter object
*    CREATE OBJECT lo_gui_splitter
*      EXPORTING
*        parent                  = cl_gui_custom_container=>default_screen
*        no_autodef_progid_dynnr = abap_true
*        rows                    = 3
*        columns                 = 1
*      EXCEPTIONS
*        cntl_error              = 1
*        cntl_system_error       = 2
*        OTHERS                  = 3.
*    IF sy-subrc <> 0.
*      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*      WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*    ENDIF.
** Instantiate Container
*    lo_container_1 = lo_gui_splitter->get_container( row = 1 column = 1 ).
*    lo_container_2 = lo_gui_splitter->get_container( row = 2 column = 1 ).
*    lo_container_3 = lo_gui_splitter->get_container( row = 3 column = 1 ).
** Set width
*    lo_gui_splitter->set_column_width( id = 1 width = 30 ).
** Instanciate ALV
*    CREATE OBJECT lo_alv_grid_1
*      EXPORTING
*        i_parent          = lo_container_1
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.
*    CREATE OBJECT lo_alv_grid_2
*      EXPORTING
*        i_parent          = lo_container_2
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.
*    CREATE OBJECT lo_alv_grid_3
*      EXPORTING
*        i_parent          = lo_container_3
*      EXCEPTIONS
*        error_cntl_create = 1
*        error_cntl_init   = 2
*        error_cntl_link   = 3
*        error_dp_create   = 4
*        OTHERS            = 5.
** Create ALV and set for display
*    DATA(successful_entries) = VALUE zmhp_cl_int_statics=>t_it_aend( FOR it IN zmhp_cl_int_statics=>it_aend WHERE ( status NE zmhp_cl_int_constants=>booking_status-error_des ) ( it ) ).
*    layout-zebra = abap_true.
*    layout-grid_title = 'Erfolgreich Deserialisiert'.
*    lo_alv_grid_1->set_table_for_first_display(
*      EXPORTING
*        i_structure_name = 'ZMHP_INT_IT_AEND'
*        is_layout = layout
*      CHANGING
*        it_outtab = successful_entries
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error = 2
*        too_many_lines = 3 ).
*    DATA(error_entries) = VALUE zmhp_cl_int_statics=>t_it_aend( FOR it IN zmhp_cl_int_statics=>it_aend WHERE ( status EQ zmhp_cl_int_constants=>booking_status-error_des ) ( it ) ).
*    layout-grid_title = 'Fehler bei Deserialisierung'.
*    lo_alv_grid_2->set_table_for_first_display(
*      EXPORTING
*        i_structure_name = 'ZMHP_INT_IT_AEND'
*        is_layout = layout
*      CHANGING
*        it_outtab = error_entries
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error = 2
*        too_many_lines = 3 ).
*    layout-grid_title = 'Messages'.
*    lo_alv_grid_3->set_table_for_first_display(
*      EXPORTING
*        i_structure_name = 'ZMHP_INT_MSG_L'
*        is_layout = layout
*      CHANGING
*        it_outtab = zmhp_cl_int_statics=>messages
*      EXCEPTIONS
*        invalid_parameter_combination = 1
*        program_error = 2
*        too_many_lines = 3 ).
** Create event handler
*    DATA gref_event_receiver TYPE REF TO lcl_event_receiver.
*    CREATE OBJECT gref_event_receiver.
** Register events
**    SET HANDLER gref_event_receiver->handle_user_command FOR lo_alv_grid_2.
*    SET HANDLER lcl_event_receiver=>handle_double_click   FOR lo_alv_grid_1.
*    SET HANDLER lcl_event_receiver=>handle_double_click   FOR lo_alv_grid_2.
    alv->build_alv_structure( ).
* Show dynpro
    WRITE: space.
  ELSE.
    alv->build_log( ).
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  HANDLE_DOUBLE_CLICK
*&---------------------------------------------------------------------*
*       Doppelklick
*----------------------------------------------------------------------*
FORM handle_double_click
  USING pis_row    TYPE lvc_s_row   "Zeile = PIS_ROW-INDEX
        pis_column TYPE lvc_s_col.  "Feldname
*  DATA:
*    rows      TYPE lvc_t_roid,
*    prelp     TYPE prelp,
*    prelp_tab TYPE TABLE OF prelp,
*    it        TYPE REF TO data.
*  FIELD-SYMBOLS: <table>       TYPE STANDARD TABLE.
*  lo_alv_grid_1->get_selected_rows(
*    IMPORTING
*      et_row_no     = rows ).
*  IF rows IS NOT INITIAL.
*    DATA(row) = rows[ 1 ].
*    DATA(success_clicked) = abap_true.
*    lo_alv_grid_1->refresh_table_display( ).
*  ELSE.
*    lo_alv_grid_2->get_selected_rows(
*      IMPORTING
*        et_row_no     = rows ).
*    IF rows IS NOT INITIAL.
*      row = rows[ 1 ].
*      DATA(error_clicked) = abap_true.
*      lo_alv_grid_2->refresh_table_display( ).
*    ENDIF.
*  ENDIF.
*  DATA(selected_entry) = COND zmhp_int_it_aend(
*                                  WHEN success_clicked = abap_true THEN VALUE #( successful_entries[ row-row_id ] OPTIONAL )
*                                  WHEN error_clicked = abap_true THEN VALUE #( error_entries[ row-row_id ] OPTIONAL ) ).
*  IF selected_entry IS INITIAL.
*    MESSAGE 'Kein Eintrag gefunden' TYPE 'S' DISPLAY LIKE 'E'.
*    EXIT.
*  ENDIF.
*  CLEAR: rows, row.
*  MOVE-CORRESPONDING selected_entry TO prelp.
*  APPEND prelp TO prelp_tab.        "
** Create infotype table
*  CONCATENATE 'P' selected_entry-infty INTO DATA(infotype).
*  CREATE DATA it TYPE TABLE OF (infotype).
*  ASSIGN it->* TO <table>.
*  cl_hr_pnnnn_type_cast=>prelp_to_pnnnn_tab(
*    EXPORTING
*      prelp_tab = prelp_tab
*    IMPORTING
*      pnnnn_tab = <table> ).
*  DATA go_alv TYPE REF TO cl_salv_table.
*  TRY.
*      cl_salv_table=>factory(
*        IMPORTING
*          r_salv_table = go_alv
*        CHANGING
*          t_table      = <table> ).
*    CATCH cx_salv_msg.
*  ENDTRY.
*  DATA: lr_functions TYPE REF TO cl_salv_functions_list.
*  lr_functions = go_alv->get_functions( ).
*  lr_functions->set_all( 'X' ).
*  IF go_alv IS BOUND.
*    go_alv->set_screen_popup(
*      start_column = 25
*      end_column  = 200
*      start_line  = 1
*      end_line    = 10 ).
*    go_alv->display( ).
*  ENDIF.
  IF 1 = 0.
  ENDIF.
ENDFORM.                    " HANDLE_DOUBLE_CLICK
