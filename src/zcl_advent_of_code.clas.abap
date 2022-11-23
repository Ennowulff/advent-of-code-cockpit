class ZCL_ADVENT_OF_CODE definition
  public
  final
  create public .

public section.

  class-methods GET_PUZZLE_INPUT
    importing
      !YEAR type JAHR1
      !USER type XUBNAME default SY-UNAME
      !DAY type I
      !PART type I
    returning
      value(DATA) type STRING .
  class-methods GET_PUZZLE_INPUT_WEB
    importing
      !YEAR type CLIKE
      !DAY type I
      !ADDITION type CLIKE optional
    returning
      value(DATA) type STRING_TABLE .
  class-methods STRING_TO_TABLE
    importing
      !INPUT type STRING
    returning
      value(TABLE) type STRING_TABLE .
  class-methods VALUE_DECLARATION
    importing
      !INPUT type STRING
    returning
      value(CODE) type STRING_TABLE .
  class-methods TABLE_TO_STRING
    importing
      !INPUT type STRING_TABLE
    returning
      value(STRING) type STRING .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ADVENT_OF_CODE IMPLEMENTATION.


  METHOD get_puzzle_input.

    SELECT SINGLE puzzle_input
      FROM zadventofcodepi
      INTO @DATA(puzzle_input)
     WHERE username = @user
       AND zyear    = @year
       AND zday     = @day
       AND zpart    = @part.
    IF sy-subrc = 0.
      data = puzzle_input.
    ELSE.
      data = VALUE #( ).
    ENDIF.

  ENDMETHOD.


  METHOD GET_PUZZLE_INPUT_WEB.

* Response from automatic request:
*
*  Oh, hello!  Funny seeing you here.
*
*  I appreciate your enthusiasm, but you aren't going to find much down here.
*  There certainly aren't clues to any of the puzzles.  The best surprises don't
*  even appear in the source until you unlock them for real.
*
*  Please be careful with automated requests; I'm not a massive company, and I can
*  only take so much traffic.  Please be considerate so that everyone gets to play.
*
*  If you're curious about how Advent of Code works, it's running on some custom
*  Perl code. Other than a few integrations (auth, analytics, ads, social media),
*  I built the whole thing myself, including the design, animations, prose, and
*  all of the puzzles.
*
*  The puzzles are most of the work; preparing a new calendar and a new set of
*  puzzles each year takes all of my free time for 4-5 months. A lot of effort
*  went into building this thing - I hope you're enjoying playing it as much as I
*  enjoyed making it for you!
*
*  If you'd like to hang out, I'm @ericwastl on Twitter.
*
*  - Eric Wastl

    DATA(lv_url) = |https://adventofcode.com/{ year }/day/{ day }/input|.

    cl_http_client=>create_by_url(
        EXPORTING
          url           = lv_url
          ssl_id        = 'ANONYM'
        IMPORTING
          client        = DATA(client) ).
    cl_http_utility=>set_request_uri( request = client->request uri = 'https://adventofcode.com/' ).
    client->send( ).
    client->receive(
      EXCEPTIONS
        http_communication_failure = 1
        http_invalid_state         = 2
        http_processing_failed     = 3
        OTHERS                     = 4 ).
    IF sy-subrc > 0.
      client->response->get_status( IMPORTING code = DATA(lv_http_rc) ).
    ELSE.
      DATA(lv_xstring) = client->response->get_data( ).
      DATA lv_string TYPE string.

* xstring (binary) -> string (UTF-8)
      DATA(conv_r) = cl_abap_conv_in_ce=>create( input = lv_xstring encoding = 'UTF-8' ).
      conv_r->read( IMPORTING data = lv_string ).
      SPLIT lv_string AT cl_abap_char_utilities=>newline INTO TABLE data.
    ENDIF.

  ENDMETHOD.


  METHOD string_to_table.
    SPLIT input AT cl_abap_char_utilities=>cr_lf INTO TABLE table.
  ENDMETHOD.


  METHOD table_to_string.
    LOOP AT input INTO DATA(line).
      string = string && line && cl_abap_char_utilities=>cr_lf.
    ENDLOOP.
  ENDMETHOD.


  METHOD value_declaration.

    CHECK input IS NOT INITIAL.

    DATA(content) = string_to_table( input ).

    code = VALUE #( ( `DATA(input) = VALUE string_table(` ) ).

    LOOP AT content INTO DATA(line).
      APPEND |  ( '{ line }' )| to code.
    ENDLOOP.
    APPEND '  ).' TO code.

  ENDMETHOD.
ENDCLASS.
