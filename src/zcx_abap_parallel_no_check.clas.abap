CLASS zcx_abap_parallel_no_check DEFINITION
  PUBLIC
  INHERITING FROM cx_no_check FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING iv_cascade_system_message TYPE abap_bool DEFAULT abap_false.

    METHODS get_text     REDEFINITION.
    METHODS get_longtext REDEFINITION.

  PROTECTED SECTION.

  PRIVATE SECTION.
    DATA v_message TYPE string.
ENDCLASS.


CLASS zcx_abap_parallel_no_check IMPLEMENTATION.
  METHOD constructor ##ADT_SUPPRESS_GENERATION.
    super->constructor( ).
    IF iv_cascade_system_message = abap_true.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
              WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
              INTO v_message.
    ENDIF.
  ENDMETHOD.

  METHOD get_longtext.
    result = v_message.
  ENDMETHOD.

  METHOD get_text.
    result = v_message.
  ENDMETHOD.
ENDCLASS.
