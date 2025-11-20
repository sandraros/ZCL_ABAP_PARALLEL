*"* use this source file for your ABAP unit test classes

CLASS ltc_equiv_fields_vs_one_field DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS all_same_value_except_last     FOR TESTING RAISING cx_static_check.
    METHODS first_field_lower_others_great FOR TESTING RAISING cx_static_check.
    METHODS all                            FOR TESTING RAISING cx_static_check.
    METHODS lower_greater_upper_boundary   FOR TESTING RAISING cx_static_check.
    METHODS lower_equals_to_upper_boundary FOR TESTING RAISING cx_static_check.
    METHODS mmmmmm                         FOR TESTING RAISING cx_static_check.

    TYPES:
      BEGIN OF ty_data,
        x TYPE c LENGTH 1,
        y TYPE c LENGTH 1,
        z TYPE c LENGTH 1,
      END OF ty_data.
    TYPES tt_data   TYPE SORTED TABLE OF ty_data WITH UNIQUE KEY x y z.
    TYPES ty_data_c TYPE c LENGTH 3.

    DATA t_data           TYPE tt_data.
    DATA t_component_name TYPE zcl_abap_parallel_tools=>tt_component_name.

    METHODS setup.

    METHODS test
      IMPORTING iv_from TYPE ty_data_c
                iv_to   TYPE ty_data_c.
ENDCLASS.


CLASS ltc_where DEFINITION FINAL
  FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.

  PRIVATE SECTION.
    METHODS test_1 FOR TESTING RAISING cx_static_check.
    METHODS test_2 FOR TESTING RAISING cx_static_check.
    METHODS test_3 FOR TESTING RAISING cx_static_check.
    METHODS test_4 FOR TESTING RAISING cx_static_check.

    TYPES:
      BEGIN OF ty_date_time,
        date TYPE d,
        time TYPE t,
      END OF ty_date_time.

    DATA s_from   TYPE ty_date_time.
    DATA s_to     TYPE ty_date_time.
    DATA v_string TYPE string.
ENDCLASS.


CLASS ltc_equiv_fields_vs_one_field IMPLEMENTATION.
  METHOD all.
    " GIVEN the lines to select are to go from the values in the first line to the values in the second line:
    "      | field_1 | field_2 | field_3 |
    "      | 1       | 1       | 1       |
    "      | 3       | 3       | 3       |
    "  AND there are 27 lines of 3 fields representing all unique combinations where each field has the values 1, 2 and 3
    "      | field_1 | field_2 | field_3 |
    "      | 1       | 1       | 1       |
    "      | 1       | 1       | 2       |
    "      | 1       | 1       | 3       |
    "      | 1       | 2       | 1       |
    "      | 1       | 2       | 2       |
    "      | ...     | ...     | ...     |
    "      | 1       | 3       | 2       |
    "      | 1       | 3       | 3       |
    "      | 2       | 1       | 1       |
    "      | 2       | 1       | 2       |
    "      | ...     | ...     | ...     |
    "      | 3       | 3       | 2       |
    "      | 3       | 3       | 3       |
    " THEN the selected lines should be all 27 lines
    test( iv_from = '111'
          iv_to   = '333' ).
  ENDMETHOD.

  METHOD all_same_value_except_last.
    " GIVEN the lines to select are to go from the values in the first line to the values in the second line:
    "      | field_1 | field_2 | field_3 |
    "      | 1       | 1       | 1       |
    "      | 1       | 1       | 3       |
    "  AND there are 27 lines of 3 fields representing all unique combinations where each field has the values 1, 2 and 3
    " THEN the selected lines should be:
    "      | field_1 | field_2 | field_3 |
    "      | 1       | 1       | 1       |
    "      | 1       | 1       | 2       |
    "      | 1       | 1       | 3       |
    test( iv_from = '111'
          iv_to   = '113' ).
  ENDMETHOD.

  METHOD first_field_lower_others_great.
    " GIVEN the lines to select are to go from the values in the first line to the values in the second line:
    "  AND there are 27 lines of 3 fields representing all unique combinations where each field has the values 1, 2 and 3
    " THEN the selected lines should be:
    "      | field_1 | field_2 | field_3 |
    "      | 1       | 3       | 3       |
    "      | 2       | 1       | 1       |
    test( iv_from = '133'
          iv_to   = '211' ).
  ENDMETHOD.

  METHOD lower_greater_upper_boundary.
    test( iv_from = '211'
          iv_to   = '111' ).
  ENDMETHOD.

  METHOD lower_equals_to_upper_boundary.
    test( iv_from = '111'
          iv_to   = '111' ).
  ENDMETHOD.

  METHOD mmmmmm.
    test( iv_from = '113'
          iv_to   = '131' ).
  ENDMETHOD.

  METHOD setup.
    DATA ls_data TYPE ty_data.

    DO 3 TIMES.
      ls_data-x = sy-index.
      DO 3 TIMES.
        ls_data-y = sy-index.
        DO 3 TIMES.
          ls_data-z = sy-index.
          INSERT ls_data INTO TABLE t_data.
        ENDDO.
      ENDDO.
    ENDDO.

    INSERT `X` INTO TABLE t_component_name.
    INSERT `Y` INTO TABLE t_component_name.
    INSERT `Z` INTO TABLE t_component_name.
  ENDMETHOD.

  METHOD test.
    DATA ls_from                       TYPE ty_data.
    DATA ls_to                         TYPE ty_data.
    DATA lv_string                     TYPE string.
    DATA ls_data                       TYPE REF TO ty_data.
    DATA lt_data_via_champ_par_champ   TYPE tt_data.
    DATA lt_data_via_champs_concatenes TYPE tt_data.

    ls_from = iv_from.
    ls_to = iv_to.

    lv_string = zcl_abap_parallel_tools=>get_where_for_interval( it_component_name = t_component_name
                                                                 is_from           = ls_from
                                                                 is_to             = ls_to ).
    LOOP AT t_data REFERENCE INTO ls_data
         WHERE (lv_string).
      INSERT ls_data->* INTO TABLE lt_data_via_champ_par_champ.
    ENDLOOP.
    LOOP AT t_data REFERENCE INTO ls_data
         WHERE table_line BETWEEN ls_from AND ls_to.
      INSERT ls_data->* INTO TABLE lt_data_via_champs_concatenes.
    ENDLOOP.

    IF lt_data_via_champ_par_champ <> lt_data_via_champs_concatenes.
      cl_abap_unit_assert=>assert_equals( act = lt_data_via_champ_par_champ
                                          exp = lt_data_via_champs_concatenes ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


CLASS ltc_where IMPLEMENTATION.
  METHOD test_1.
    s_from-date = '20250608'.
    s_from-time = '214500'.
    s_to-date = '20250609'.
    s_to-time = '042211'.
    v_string = zcl_abap_parallel_tools=>get_where_for_interval( is_from = s_from
                                                                is_to   = s_to ).
    cl_abap_unit_assert=>assert_equals(
        act = v_string
        exp = `( ( DATE = '20250608' AND TIME >= '214500' ) OR ( DATE > '20250608' AND DATE < '20250609' ) OR ( DATE = '20250609' AND TIME <= '042211' ) )` ).
  ENDMETHOD.

  METHOD test_2.
    s_from-date = '20250608'.
    s_from-time = '214500'.
    s_to-date = '20250608'.
    s_to-time = '202211'.
    v_string = zcl_abap_parallel_tools=>get_where_for_interval( is_from = s_from
                                                                is_to   = s_to ).
    cl_abap_unit_assert=>assert_equals( act = v_string
                                        exp = `TIME BETWEEN '214500' AND '202211'` ).
  ENDMETHOD.

  METHOD test_3.
    s_from-date = '20250608'.
    s_from-time = '214500'.
    s_to-date = '20250607'.
    s_to-time = '042211'.
    v_string = zcl_abap_parallel_tools=>get_where_for_interval( is_from = s_from
                                                                is_to   = s_to ).
    cl_abap_unit_assert=>assert_equals( act = v_string
                                        exp = `DATE BETWEEN '20250608' AND '20250607'` ).
  ENDMETHOD.

  METHOD test_4.
    TYPES:
      BEGIN OF ty_test,
        dummy TYPE i,
        date  TYPE d,
        time  TYPE t,
      END OF ty_test.

    DATA lt_component_name TYPE zcl_abap_parallel_tools=>tt_component_name.
    DATA ls_from           TYPE ty_test.
    DATA ls_to             TYPE ty_test.

    INSERT `DATE` INTO TABLE lt_component_name.
    INSERT `TIME` INTO TABLE lt_component_name.
    ls_from-date = '20250608'.
    ls_from-time = '000000'.
    ls_to-date = '20250608'.
    ls_to-time = '235959'.
    v_string = zcl_abap_parallel_tools=>get_where_for_interval( it_component_name = lt_component_name
                                                                is_from           = ls_from
                                                                is_to             = ls_to ).
    cl_abap_unit_assert=>assert_equals( act = v_string
                                        exp = `( DATE = '20250608' AND TIME BETWEEN '000000' AND '235959' )` ).
  ENDMETHOD.
ENDCLASS.
