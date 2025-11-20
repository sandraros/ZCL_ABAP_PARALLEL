REPORT z_abap_parallel_demo.

PARAMETERS numtasks TYPE i DEFAULT 20.

START-OF-SELECTION.
  DATA go_error TYPE REF TO cx_root.

  TRY.
      CALL METHOD ('LCL_APP')=>('MAIN').
    CATCH cx_root INTO go_error.
      MESSAGE go_error TYPE 'S' DISPLAY LIKE 'E'.
  ENDTRY.

CLASS lcl_app DEFINITION FINAL CREATE PRIVATE.
  PUBLIC SECTION.
    CLASS-METHODS main
      RAISING zcx_abap_parallel.
ENDCLASS.


CLASS lcl_task DEFINITION FINAL.
  PUBLIC SECTION.
    INTERFACES zif_abap_parallel.

    METHODS constructor
      IMPORTING iv_task_number TYPE i.

    METHODS delay_seconds_without_wait IMPORTING delay_secs TYPE numeric.

    DATA v_task_number TYPE i.
    DATA v_start_time  TYPE t.
ENDCLASS.


CLASS lcl_app IMPLEMENTATION.
  METHOD main.
    DATA lv_task_number TYPE i.
    DATA lo_task        TYPE REF TO lcl_task.
    DATA ls_task_in     TYPE zcl_abap_parallel=>ts_task_in.
    DATA lt_task_in     TYPE zcl_abap_parallel=>tt_task_in.
    DATA lt_task_out    TYPE zcl_abap_parallel=>tt_task_out.
    DATA ls_task_out    TYPE REF TO zcl_abap_parallel=>ts_task_out.
    DATA lo_parallel    TYPE REF TO zcl_abap_parallel.

    DO numtasks TIMES.
      lv_task_number = sy-index.
      CREATE OBJECT lo_task
        EXPORTING iv_task_number = lv_task_number.

      ls_task_in-name   = |{ lv_task_number }|.
      ls_task_in-o_task = lo_task.
      INSERT ls_task_in INTO TABLE lt_task_in.
    ENDDO.

    lo_parallel = zcl_abap_parallel=>create_by_rfc_group( iv_rfc_group = 'parallel_generators' ).

    lt_task_out = lo_parallel->run_inst( it_task_in = lt_task_in ).

    LOOP AT lt_task_out REFERENCE INTO ls_task_out.
      lo_task ?= ls_task_out->o_task.
      WRITE : / lo_task->v_start_time, ls_task_out->name, ls_task_out->error_code, lo_task->v_task_number.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.


CLASS lcl_task IMPLEMENTATION.
  METHOD constructor.
    v_task_number = iv_task_number.
  ENDMETHOD.

  METHOD delay_seconds_without_wait.
    DATA tstmp1    TYPE tzntstmpl.
    DATA tstmp2    TYPE tzntstmpl.
    DATA diff_secs TYPE tzntstmpl.

    GET TIME STAMP FIELD tstmp1.
    DO.
      DO 1000000 TIMES. ENDDO.
      GET TIME STAMP FIELD tstmp2.
      diff_secs = cl_abap_tstmp=>subtract( tstmp1 = tstmp2
                                           tstmp2 = tstmp1 ).
      IF diff_secs >= delay_secs.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

  METHOD zif_abap_parallel~do.
    v_start_time = sy-uzeit.
    delay_seconds_without_wait( 10 ).
  ENDMETHOD.
ENDCLASS.
