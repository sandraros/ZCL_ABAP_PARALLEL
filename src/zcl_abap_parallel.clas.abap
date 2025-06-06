CLASS zcl_abap_parallel DEFINITION
  PUBLIC FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES tv_task_name TYPE c LENGTH 30.
    TYPES:
      BEGIN OF ts_task_in,
        name       TYPE tv_task_name,
        o_task     TYPE REF TO zif_abap_parallel,
      END OF ts_task_in.
    TYPES tt_task_in TYPE SORTED TABLE OF ts_task_in WITH UNIQUE KEY name.
    TYPES:
      BEGIN OF ts_task_out,
        name       TYPE tv_task_name,
        o_task     TYPE REF TO zif_abap_parallel,
        error_code TYPE sysubrc,
      END OF ts_task_out.
    TYPES tt_task_out TYPE SORTED TABLE OF ts_task_out WITH UNIQUE KEY name.

    "!
    "! @parameter it_task_in | List of RFC calls to be executed in parallel
    "! @parameter iv_wait_between_each_rfc_call | Number of wait seconds. Zero by défaut.
    "!                                            One second may be used to simulate a sequential execution of tasks
    "!                                            (but it's not guaranteed, RFC queues should be used to guarantee it).
    "! @parameter rt_task_out | List of executed RFC calls, with potentially the results stored in each instance.
    METHODS run_inst
      IMPORTING it_task_in                      TYPE tt_task_in
                iv_wait_between_each_rfc_call TYPE i DEFAULT 0
      RETURNING VALUE(rt_task_out)               TYPE tt_task_out.

    METHODS on_end_of_rfc_task IMPORTING p_task TYPE c.

  PRIVATE SECTION.
    DATA gt_task_out TYPE tt_task_out.
ENDCLASS.


CLASS zcl_abap_parallel IMPLEMENTATION.
  METHOD on_end_of_rfc_task.
    TYPES to_task TYPE REF TO zif_abap_parallel.

    DATA(lv_task_asxml) = VALUE xstring( ).

    RECEIVE RESULTS FROM FUNCTION 'Z_ABAP_PARALLEL'
      IMPORTING  ev_task_asxml         = lv_task_asxml
      EXCEPTIONS communication_failure = 1
                 system_failure        = 2
                 OTHERS                = 3.

    DATA(lv_sy_subrc) = sy-subrc.

    IF sy-subrc = 0.
      DATA(lo_task) = VALUE to_task( ).
      CALL TRANSFORMATION id
           SOURCE XML lv_task_asxml
           RESULT data = lo_task.
    ENDIF.

    INSERT VALUE #( name       = p_task
                    o_task     = lo_task
                    error_code = lv_sy_subrc )
           INTO TABLE gt_task_out.
  ENDMETHOD.

  METHOD run_inst.
    gt_task_out = VALUE #( ).

    LOOP AT it_task_in REFERENCE INTO DATA(ls_task).

      IF sy-tabix > 1.
        WAIT UP TO iv_wait_between_each_rfc_call SECONDS.
      ENDIF.

      CALL TRANSFORMATION id
           SOURCE data = ls_task->o_task
           RESULT XML DATA(lv_task_asxml).

      CALL FUNCTION 'Z_ABAP_PARALLEL'
          STARTING NEW TASK ls_task->name
          CALLING on_end_of_rfc_task ON END OF TASK
        EXPORTING iv_task_asxml = lv_task_asxml.

    ENDLOOP.

    WAIT FOR ASYNCHRONOUS TASKS UNTIL lines( gt_task_out ) = lines( it_task_in ).

    rt_task_out = gt_task_out.
  ENDMETHOD.
ENDCLASS.
