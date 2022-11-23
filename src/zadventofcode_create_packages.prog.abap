REPORT zadventofcode_create_packages.

PARAMETERS p_devc TYPE parentcl OBLIGATORY.
PARAMETERS p_devs TYPE devclass OBLIGATORY.
PARAMETERS p_txt1 TYPE c LENGTH 40 LOWER CASE OBLIGATORY.
PARAMETERS p_txt2 TYPE c LENGTH 40 LOWER CASE DEFAULT 'Day $' OBLIGATORY.
PARAMETERS p_days TYPE i DEFAULT 24.
PARAMETERS p_test AS CHECKBOX DEFAULT 'X'.


INITIALIZATION.
  p_devc = |$ADVENTOFCODE_{ sy-uname }_{ sy-datum(4) }|.
  p_devs = |$AOC_{ sy-uname }_{ sy-datum(4) }_DAY$|.
  data address type BAPIADDR3.
  data return type bapiret2_tab.
  call function 'BAPI_USER_GET_DETAIL'
    EXPORTING
      username       = sy-uname
    IMPORTING
      address        = address
    TABLES
      return         = return.
  p_txt1 = |Advent Of Code { address-fullname } { sy-datum(4) }|.


START-OF-SELECTION.
  DATA text     TYPE c LENGTH 80.
  DATA devclass TYPE trdevclass.
  DATA changed  TYPE c LENGTH 1.
  DATA name TYPE devclass.
  data dayn type n length 2.

  PERFORM create_devclass USING p_devc p_txt1.

  DO p_days TIMES.

    dayn = sy-index.
    name = p_devs.
    replace 'DAY$' with dayn into name.
    text = |{ p_txt1 } - { p_txt2 }|.
    REPLACE '$' WITH dayn INTO text.

    PERFORM create_devclass USING name text.
  ENDDO.

FORM create_devclass USING name text.

  IF p_test = abap_true.
    WRITE: / name, text.
    RETURN.
  ENDIF.

  devclass-devclass  = name.
  devclass-ctext     = text.
  devclass-as4user   = sy-uname.
  devclass-pdevclass = ''.
  devclass-dlvunit   = 'LOCAL'.
  devclass-component = space.
  devclass-comp_appr = space.
  devclass-comp_text = space.
  devclass-korrflag  = 'X'.
  devclass-namespace = space.
  devclass-parentcl   = p_devc.
  devclass-tpclass    = space.
  devclass-type       = 'N'.
  devclass-target     = space.
  devclass-packtype   = space.
  devclass-restricted = space.
  devclass-mainpack   = space.
  devclass-created_by = sy-uname.
  devclass-created_on = sy-datum.


  CALL FUNCTION 'TRINT_MODIFY_DEVCLASS'
    EXPORTING
      iv_action            = 'CREA'
      iv_dialog            = space
      is_devclass          = devclass
      iv_request           = space
    IMPORTING
      es_devclass          = devclass
      ev_something_changed = changed
    EXCEPTIONS
      OTHERS               = 1.
  IF sy-subrc > 0.
    MESSAGE i000(oo) WITH 'Error creating package' name.
    STOP.
  ELSE.
    WRITE: / 'Pacakge created:', name, text.
  ENDIF.
ENDFORM.                    "create_devclass
