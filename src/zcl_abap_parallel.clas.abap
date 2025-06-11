CLASS zcl_abap_parallel DEFINITION
  PUBLIC FINAL
  CREATE PRIVATE.

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
        "! <ul>
        "! <li>0 : technical success</li>
        "! <li>1 : COMMUNICATION_FAILURE</li>
        "! <li>2 : SYSTEM_FAILURE</li>
        "! </ul>
        error_code TYPE sysubrc,
        "! Error text
        error_text TYPE c LENGTH 255,
      END OF ts_task_out.
    TYPES tt_task_out TYPE SORTED TABLE OF ts_task_out WITH UNIQUE KEY name.

    CLASS-METHODS create_by_rfc_group
      IMPORTING iv_rfc_group     TYPE rfcgr
      RETURNING VALUE(ro_result) TYPE REF TO zcl_abap_parallel.

    "! For information, the method RUN_INST may also trigger an exception of type ZCX_ABAP_PARALLEL_NO_CHECK.
    "! @parameter it_task_in | List of RFC calls to be executed in parallel
    "! @parameter iv_wait_between_each_rfc_call | Number of wait seconds. Zero by défaut.
    "!                                            One second may be used to simulate a sequential execution of tasks
    "!                                            (but it's not guaranteed, RFC queues should be used to guarantee it).
    "! @parameter rt_task_out | List of executed RFC calls, with potentially the results stored in each instance.
    METHODS run_inst
      IMPORTING it_task_in                    TYPE tt_task_in
                iv_debug                      TYPE abap_bool DEFAULT abap_false
                iv_wait_between_each_rfc_call TYPE i         DEFAULT 0
      RETURNING VALUE(rt_task_out)            TYPE tt_task_out
      RAISING   zcx_abap_parallel.

    METHODS on_end_of_rfc_task IMPORTING p_task TYPE c.

  PRIVATE SECTION.
    DATA gt_task_out        TYPE tt_task_out.
    DATA gv_rfc_group       TYPE rfcgr.
    DATA gv_free_pbt_wps    TYPE i.
ENDCLASS.


CLASS zcl_abap_parallel IMPLEMENTATION.
  METHOD create_by_rfc_group.
    CREATE OBJECT ro_result TYPE zcl_abap_parallel.
    ro_result->gv_rfc_group = iv_rfc_group.
  ENDMETHOD.

  METHOD on_end_of_rfc_task.
    TYPES to_task TYPE REF TO zif_abap_parallel.

    DATA lv_task_asxml   TYPE xstring.
    DATA lv_message      TYPE ts_task_out-error_text.
    DATA lv_sy_subrc     TYPE sysubrc.
    DATA lo_task         TYPE to_task.
    DATA ls_task_out     TYPE ts_task_out.

    RECEIVE RESULTS FROM FUNCTION 'Z_ABAP_PARALLEL'
      IMPORTING  ev_task_asxml         = lv_task_asxml
      EXCEPTIONS communication_failure = 1 MESSAGE lv_message
                 system_failure        = 2 MESSAGE lv_message
                 OTHERS                = 3.

    lv_sy_subrc = sy-subrc.

    IF sy-subrc = 0.
      CALL TRANSFORMATION id
           SOURCE XML lv_task_asxml
           RESULT data = lo_task.
    ENDIF.

    ls_task_out-name       = p_task.
    ls_task_out-o_task     = lo_task.
    ls_task_out-error_code = lv_sy_subrc.
    ls_task_out-error_text = lv_message.
    INSERT ls_task_out INTO TABLE gt_task_out.

    CALL FUNCTION 'SPBT_GET_CURR_RESOURCE_INFO'
      IMPORTING
        free_pbt_wps                = gv_free_pbt_wps
      EXCEPTIONS
        internal_error              = 1
        pbt_env_not_initialized_yet = 2
        OTHERS                      = 3.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE zcx_abap_parallel_no_check
        EXPORTING
          iv_cascade_system_message = abap_true.
    ENDIF.
  ENDMETHOD.

  METHOD run_inst.
    DATA ls_task       TYPE REF TO ts_task_in.
    DATA lv_task_asxml TYPE xstring.

    IF iv_debug = abap_false.
      CALL FUNCTION 'SPBT_INITIALIZE'
        EXPORTING
          group_name                     = gv_rfc_group
        IMPORTING
          free_pbt_wps                   = gv_free_pbt_wps
        EXCEPTIONS
          invalid_group_name             = 1
          internal_error                 = 2
          pbt_env_already_initialized    = 3
          currently_no_resources_avail   = 4
          no_pbt_resources_found         = 5
          cant_init_different_pbt_groups = 6
          OTHERS                         = 7.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE zcx_abap_parallel
          EXPORTING
            iv_cascade_system_message = abap_true.
      ENDIF.
    ENDIF.

    IF gv_free_pbt_wps = 0.
      return.
    ENDIF.

    LOOP AT it_task_in REFERENCE INTO ls_task.

      IF sy-tabix > 1.
        WAIT UP TO iv_wait_between_each_rfc_call SECONDS.
      ENDIF.

      CALL TRANSFORMATION id
           SOURCE data = ls_task->o_task
           RESULT XML lv_task_asxml.

      IF iv_debug = abap_false.
        " If the limit is reached, wait till one RFC ends/one workprocess is free.
        IF     gv_free_pbt_wps = 0
           AND iv_debug        = abap_false.
          WAIT UNTIL gv_free_pbt_wps > 0.
        ENDIF.

        DO.
          CALL FUNCTION 'Z_ABAP_PARALLEL'
            STARTING NEW TASK ls_task->name
            DESTINATION IN GROUP gv_rfc_group
            CALLING on_end_of_rfc_task ON END OF TASK
            EXPORTING  iv_task_asxml    = lv_task_asxml
            EXCEPTIONS resource_failure = 3.
          IF sy-subrc = 0.
            gv_free_pbt_wps = gv_free_pbt_wps - 1.
            EXIT.
          ENDIF.

          " If RESOURCE_FAILURE happened, wait till one RFC ends/one workprocess is free, and try again.
          WAIT UNTIL gv_free_pbt_wps > 0.
        ENDDO.
      ELSE.
        ls_task->o_task->do( ).
      ENDIF.
    ENDLOOP.

    IF iv_debug = abap_false.
      WAIT UNTIL lines( gt_task_out ) = lines( it_task_in ).
    ENDIF.

    rt_task_out = gt_task_out.
  ENDMETHOD.
ENDCLASS.
