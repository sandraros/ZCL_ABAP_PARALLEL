CLASS zcl_abap_parallel_tools DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tt_component_name TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    CLASS-METHODS get_where_for_interval
      IMPORTING VALUE(it_component_name) TYPE tt_component_name OPTIONAL
                is_from                  TYPE any
                is_to                    TYPE any
      RETURNING VALUE(rv_string)         TYPE string.

  PRIVATE SECTION.
    CLASS-METHODS get_field_value
      IMPORTING iany_value       TYPE any
      RETURNING VALUE(rv_string) TYPE string.
ENDCLASS.


CLASS zcl_abap_parallel_tools IMPLEMENTATION.
  METHOD get_field_value.
    rv_string = |'{ iany_value }'|.
  ENDMETHOD.

  METHOD get_where_for_interval.
    DATA lo_typedescr                  TYPE REF TO cl_abap_typedescr.
    DATA lo_structdescr                TYPE REF TO cl_abap_structdescr.
    "! Last component when looping on a partial list of components
    DATA lt_component                  TYPE cl_abap_structdescr=>component_table.
    DATA ls_component                  TYPE REF TO abap_componentdescr.
    DATA lv_number_of_components       TYPE i.
    "! Last component
    DATA lv_last_component             TYPE REF TO string.
    DATA lv_component                  TYPE REF TO string.
    DATA lv_nb_of_equal_fields         TYPE i.
    DATA lv_from_equals_to_except_last TYPE abap_bool.
    DATA lv_tabix_component            TYPE sytabix.
    " TODO: variable is assigned but never used (ABAP cleaner)
    DATA lv_component_2                TYPE REF TO string.
    DATA lv_string                     TYPE string.
    DATA lt_string_for_or              TYPE string_table.
    DATA lt_string                     TYPE string_table.
    DATA lv_counter_components         TYPE i.
    DATA lv_contextual_last_component  TYPE REF TO string.

    FIELD-SYMBOLS <lv_from_value> TYPE any.
    FIELD-SYMBOLS <lv_to_value>   TYPE any.

    IF it_component_name IS INITIAL.
      lo_typedescr = cl_abap_typedescr=>describe_by_data( is_from ).
      lo_structdescr ?= lo_typedescr.
      lt_component = lo_structdescr->get_components( ).
      LOOP AT lt_component REFERENCE INTO ls_component.
        INSERT ls_component->name INTO TABLE it_component_name.
      ENDLOOP.
    ENDIF.

    lv_number_of_components = lines( it_component_name ).
    READ TABLE it_component_name INDEX lv_number_of_components REFERENCE INTO lv_last_component.

    " If TO > FROM, return a contradiction (always false) in order the query to return nothing.
    " NB: this special logic is done here, otherwise the logic after would select data.
    LOOP AT it_component_name REFERENCE INTO lv_component.
      ASSIGN COMPONENT lv_component->* OF STRUCTURE is_from TO <lv_from_value>.
      ASSIGN COMPONENT lv_component->* OF STRUCTURE is_to TO <lv_to_value>.
      IF <lv_from_value> > <lv_to_value>.
        rv_string = |{ lv_component->* } BETWEEN { get_field_value( <lv_from_value> ) } AND { get_field_value( <lv_to_value> ) }|.
        RETURN.
      ENDIF.
      IF <lv_from_value> < <lv_to_value>.
        EXIT.
      ENDIF.
    ENDLOOP.

    " Build a simplified AND condition if possible, to make it legible, just in case.
    " E.g.       ( DATE = '20250608' AND TIME BETWEEN '000000' AND '235959' )
    " instead of ( ( DATE = '20250608' AND TIME >= '000000' ) OR ( DATE > '20250608' AND DATE < '20250608' ) OR ( DATE = '20250608' AND TIME <= '235959' ) )
    lv_nb_of_equal_fields = 0.
    lv_from_equals_to_except_last = abap_true.
    LOOP AT it_component_name REFERENCE INTO lv_component
         WHERE table_line <> lv_last_component->*.
      ASSIGN COMPONENT lv_component->* OF STRUCTURE is_from TO <lv_from_value>.
      ASSIGN COMPONENT lv_component->* OF STRUCTURE is_to TO <lv_to_value>.
      IF <lv_from_value> <> <lv_to_value>.
        lv_from_equals_to_except_last = abap_false.
        EXIT.
      ENDIF.
      lv_nb_of_equal_fields = lv_nb_of_equal_fields + 1.
    ENDLOOP.

    LOOP AT it_component_name REFERENCE INTO lv_component.
      lv_tabix_component = sy-tabix.
      LOOP AT it_component_name REFERENCE INTO lv_component_2
           TO lv_tabix_component.
        ASSIGN COMPONENT lv_component->* OF STRUCTURE is_from TO <lv_from_value>.
        ASSIGN COMPONENT lv_component->* OF STRUCTURE is_to TO <lv_to_value>.
        IF     lv_tabix_component  > lv_nb_of_equal_fields
           AND <lv_from_value>    <> <lv_to_value>.
          lv_string = |{ lv_component->* } < { get_field_value( <lv_from_value> )
                      } AND { lv_component->* } > { get_field_value( <lv_to_value> ) }|.
          INSERT lv_string INTO TABLE lt_string_for_or.
        ELSEIF lv_tabix_component <= lv_nb_of_equal_fields.
          lv_string = |{ lv_component->* } = { get_field_value( <lv_from_value> ) }|.
        ELSE.
          lv_string = |{ lv_component->* } BETWEEN { get_field_value( <lv_from_value> )
                      } AND { get_field_value( <lv_to_value> ) }|.
        ENDIF.
        INSERT lv_string INTO TABLE lt_string.
      ENDLOOP.
      lv_string = `( `
               && concat_lines_of( table = lt_string
                                   sep   = ` AND ` )
               && ` )`.
      INSERT lv_string INTO TABLE lt_string_for_or.
    ENDLOOP.
    " Join all "top" conditions by " OR "
    rv_string = `( `
             && concat_lines_of( table = lt_string_for_or
                                 sep   = ` OR ` )
             && ` )`.
    RETURN.
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    "                 THE END
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    IF lv_from_equals_to_except_last = abap_true.
      LOOP AT it_component_name REFERENCE INTO lv_component.
        ASSIGN COMPONENT lv_component->* OF STRUCTURE is_from TO <lv_from_value>.
        ASSIGN COMPONENT lv_component->* OF STRUCTURE is_to TO <lv_to_value>.
        IF lv_component->* <> lv_last_component->*.
          lv_string = |{ lv_component->* } = { get_field_value( <lv_from_value> ) }|.
        ELSEIF <lv_from_value> = <lv_to_value>.
          lv_string = |{ lv_component->* } = { get_field_value( <lv_from_value> ) }|.
        ELSE.
          lv_string = |{ lv_component->* } BETWEEN { get_field_value( <lv_from_value> ) } AND { get_field_value( <lv_to_value> ) }|.
        ENDIF.
        INSERT lv_string INTO TABLE lt_string.
      ENDLOOP.
      rv_string = `( `
               && concat_lines_of( table = lt_string
                                   sep   = ` AND ` )
               && ` )`.
      RETURN.
    ENDIF.

    " Build the conditions around the lower boundary
    lv_counter_components = lv_number_of_components.
    WHILE lv_counter_components >= 2.
      READ TABLE it_component_name INDEX lv_counter_components REFERENCE INTO lv_contextual_last_component.
      CLEAR lt_string.
      LOOP AT it_component_name REFERENCE INTO lv_component
           FROM 1 TO lv_counter_components.
        ASSIGN COMPONENT lv_component->* OF STRUCTURE is_from TO <lv_from_value>.
        CASE lv_component->*.
          WHEN lv_last_component->*.
            lv_string = |{ lv_component->* } >= '{ <lv_from_value> }'|.
          WHEN lv_contextual_last_component->*.
            lv_string = |{ lv_component->* } > '{ <lv_from_value> }'|.
          WHEN OTHERS.
            lv_string = |{ lv_component->* } = '{ <lv_from_value> }'|.
        ENDCASE.
        INSERT lv_string INTO TABLE lt_string.
      ENDLOOP.
      lv_string = `( `
               && concat_lines_of( table = lt_string
                                   sep   = ` AND ` )
               && ` )`.
      INSERT lv_string INTO TABLE lt_string_for_or.
      lv_counter_components = lv_counter_components - 1.
    ENDWHILE.

    " Build the conditions for the first field, between the lower and upper boundaries
    READ TABLE it_component_name INDEX 1 REFERENCE INTO lv_component.
    ASSIGN COMPONENT lv_component->* OF STRUCTURE is_from TO <lv_from_value>.
    ASSIGN COMPONENT lv_component->* OF STRUCTURE is_to TO <lv_to_value>.
    IF <lv_from_value> <> <lv_to_value>.
      lv_string = |( { lv_component->* } > '{ <lv_from_value> }' AND { lv_component->* } < '{ <lv_to_value> }' )|.
      INSERT lv_string INTO TABLE lt_string_for_or.
    ENDIF.

    " Build the conditions around the upper boundary, omit the first field because it was considered when processing the lower boundary.
    lv_counter_components = 2.
    WHILE lv_counter_components <= lv_number_of_components.
      READ TABLE it_component_name INDEX lv_counter_components REFERENCE INTO lv_contextual_last_component.
      CLEAR lt_string.
      LOOP AT it_component_name REFERENCE INTO lv_component
           FROM 1 TO lv_counter_components.
        ASSIGN COMPONENT lv_component->* OF STRUCTURE is_to TO <lv_to_value>.
        CASE lv_component->*.
          WHEN lv_last_component->*.
            lv_string = |{ lv_component->* } <= '{ <lv_to_value> }'|.
          WHEN lv_contextual_last_component->*.
            lv_string = |{ lv_component->* } < '{ <lv_to_value> }'|.
          WHEN OTHERS.
            lv_string = |{ lv_component->* } = '{ <lv_to_value> }'|.
        ENDCASE.
        INSERT lv_string INTO TABLE lt_string.
      ENDLOOP.
      lv_string = `( `
               && concat_lines_of( table = lt_string
                                   sep   = ` AND ` )
               && ` )`.
      INSERT lv_string INTO TABLE lt_string_for_or.
      lv_counter_components = lv_counter_components + 1.
    ENDWHILE.

    " Join all "top" conditions by " OR "
    rv_string = `( `
             && concat_lines_of( table = lt_string_for_or
                                 sep   = ` OR ` )
             && ` )`.
  ENDMETHOD.
ENDCLASS.
