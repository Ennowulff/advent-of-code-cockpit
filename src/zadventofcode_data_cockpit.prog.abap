*&---------------------------------------------------------------------*
*& Report ZADVENTOFCODE_DATA_COCKPIT
*&---------------------------------------------------------------------*
*& Cockpit to manage puzzle input
*&---------------------------------------------------------------------*
REPORT zadventofcode_data_cockpit.

DATA no_input TYPE c LENGTH 1.

SELECTION-SCREEN COMMENT /1(50) TEXT-in1.
PARAMETERS p_year TYPE jahr1 DEFAULT sy-datum(4) MODIF ID dis.
PARAMETERS p_user TYPE sy-uname DEFAULT sy-uname MODIF ID dis.


CLASS main DEFINITION.
  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        container TYPE REF TO cl_gui_container.
    METHODS load
      IMPORTING
        user TYPE syuname
        year TYPE jahr1.

  PROTECTED SECTION.
    DATA main_container TYPE REF TO cl_gui_container.
    DATA day_container TYPE REF TO cl_gui_container.
    DATA pi_container TYPE REF TO cl_gui_container.
    DATA value_container TYPE REF TO cl_gui_container.

    DATA days_grid TYPE REF TO cl_salv_table.
    DATA year TYPE jahr1.
    DATA user TYPE syuname.
    TYPES:
      BEGIN OF _entry,
        day    TYPE i,
        input1 TYPE string,
        input2 TYPE string,
        input3 TYPE string,
      END OF _entry,
      _list TYPE STANDARD TABLE OF _entry WITH DEFAULT KEY.
    DATA days TYPE _list.
    DATA pi_text_edit TYPE REF TO cl_gui_textedit.
    DATA value_text_edit TYPE REF TO cl_gui_textedit.
    DATA current_entry TYPE i.
    DATA current_column TYPE string.

    METHODS read_puzzle_input.
    METHODS display.
    METHODS check_text_changes.
    METHODS on_link_click
        FOR EVENT link_click OF cl_salv_events_table
      IMPORTING
        row
        column.
    METHODS create_grid.
    METHODS create_text.
ENDCLASS.

CLASS main IMPLEMENTATION.
  METHOD constructor.
    me->main_container = container.
  ENDMETHOD.

  METHOD load.

    me->year = year.
    me->user = user.

    read_puzzle_input( ).
    display( ).
    on_link_click( row = 1 column = 'INPUT1' ).

  ENDMETHOD.

  METHOD read_puzzle_input.
    DATA entry TYPE _entry.
    DATA day TYPE i.

    DO 24 TIMES.
      ADD 1 TO day.
      APPEND VALUE #(
        day = day
        input1 = zcl_advent_of_code=>get_puzzle_input( year = year user = user day  = day part = 1 )
        input2 = zcl_advent_of_code=>get_puzzle_input( year = year user = user day  = day part = 2 )
        input3 = zcl_advent_of_code=>get_puzzle_input( year = year user = user day  = day part = 3 ) )
      TO days.

    ENDDO.

  ENDMETHOD.

  METHOD display.

    DATA(splitter) = NEW cl_gui_splitter_container( parent = main_container rows = 1 columns = 3 ).
    splitter->set_column_width(
        id                = 1
        width             = 50 ).
    day_container = splitter->get_container( row = 1 column = 1 ).
    pi_container = splitter->get_container( row = 1 column = 2 ).
    value_container = splitter->get_container( row = 1 column = 3 ).

    create_grid( ).
    create_text( ).

  ENDMETHOD.

  METHOD create_text.

    pi_text_edit = NEW #( parent = pi_container ).
    value_text_edit = NEW #( parent = value_container ).
    value_text_edit->set_readonly_mode( 1 ).

  ENDMETHOD.

  METHOD create_grid.

    TRY.
        cl_salv_table=>factory(
          EXPORTING
            r_container    = day_container
          IMPORTING
            r_salv_table   = days_grid
          CHANGING
            t_table        = days ).
        days_grid->get_columns( )->get_column( 'DAY' )->set_medium_text( 'Day' ).
        days_grid->get_columns( )->get_column( 'INPUT1' )->set_medium_text( 'Part1' ).
        days_grid->get_columns( )->get_column( 'INPUT2' )->set_medium_text( 'Part2' ).
        days_grid->get_columns( )->get_column( 'INPUT3' )->set_medium_text( 'Part3' ).
        CAST cl_salv_column_table( days_grid->get_columns( )->get_column( 'INPUT1' ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
        CAST cl_salv_column_table( days_grid->get_columns( )->get_column( 'INPUT2' ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).
        CAST cl_salv_column_table( days_grid->get_columns( )->get_column( 'INPUT3' ) )->set_cell_type( if_salv_c_cell_type=>hotspot ).

        CAST cl_salv_column_table( days_grid->get_columns( )->get_column( 'INPUT1' ) )->set_output_length( 20 ).
        CAST cl_salv_column_table( days_grid->get_columns( )->get_column( 'INPUT2' ) )->set_output_length( 20 ).
        CAST cl_salv_column_table( days_grid->get_columns( )->get_column( 'INPUT3' ) )->set_output_length( 20 ).

        days_grid->display( ).
        SET HANDLER on_link_click FOR days_grid->get_event( ).
      CATCH cx_salv_msg cx_salv_not_found.
    ENDTRY.
  ENDMETHOD.

  METHOD on_link_click.

    check_text_changes( ).

    current_entry = row.
    current_column = column.

    CASE column.
      WHEN 'INPUT1'.
        DATA(entry) = days[ day = row ]-input1.
      WHEN 'INPUT2'.
        entry = days[ day = row ]-input2.
      WHEN 'INPUT3'.
        entry = days[ day = row ]-input3.
    ENDCASE.

    pi_text_edit->set_status_text( |Day { row }, { column }| ).
    value_text_edit->set_status_text( |Day { row }, { column }| ).

    pi_text_edit->set_textstream( text = entry ).
    value_text_edit->set_textstream( text = zcl_advent_of_code=>table_to_String( zcl_advent_of_code=>value_declaration( entry ) ) ).


  ENDMETHOD.

  METHOD check_text_changes.

    DATA db_pi TYPE zadventofcodepi.

    CHECK current_entry > 0.

    pi_text_edit->get_textstream(
      EXPORTING
        only_when_modified     = 1
      IMPORTING
        text                   = DATA(text)
        is_modified            = DATA(is_modified) ).
    cl_gui_cfw=>flush( ).
    IF is_modified = 1.
      CASE current_column.
        WHEN 'INPUT1'.
          days[ current_entry ]-input1 = text.
        WHEN 'INPUT2'.
          days[ current_entry ]-input2 = text.
        WHEN 'INPUT3'.
          days[ current_entry ]-input3 = text.
      ENDCASE.

      DATA(entry) = days[ current_entry ].
      db_pi-zyear        = year.
      db_pi-username     = user.
      db_pi-zday         = entry-day.
      db_pi-zpart        = SWITCH #( current_column WHEN 'INPUT1' THEN 1 WHEN 'INPUT2' THEN 2 WHEN 'INPUT3' THEN 3 ).
      db_pi-puzzle_input = text.

      MODIFY zadventofcodepi FROM db_pi.
      IF sy-subrc = 0.
        MESSAGE |puzzle input saved| TYPE 'S'.
      ENDIF.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

INITIALIZATION.
  DATA(docker) = NEW cl_gui_docking_container( side = cl_gui_docking_container=>dock_at_bottom ratio = 90 ).
  DATA(app) = NEW main( docker ).

AT SELECTION-SCREEN OUTPUT.
  DATA gt_excl TYPE STANDARD TABLE OF syucomm.
  APPEND 'ONLI' TO gt_excl.
  CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
    EXPORTING
      p_status  = sy-pfkey
    TABLES
      p_exclude = gt_excl.

  IF no_input = abap_true.
    LOOP AT SCREEN.
      IF screen-group1 = 'DIS'.
        screen-input = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

AT SELECTION-SCREEN.
  app->load(
    user = p_user
    year = p_year ).

  no_input = abap_true.
