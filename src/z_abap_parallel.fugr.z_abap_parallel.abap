FUNCTION Z_ABAP_PARALLEL.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_TASK_ASXML) TYPE  XSTRING
*"  EXPORTING
*"     VALUE(EV_TASK_ASXML) TYPE  XSTRING
*"----------------------------------------------------------------------
  DATA lo_task TYPE REF TO zif_abap_parallel.

  TRY.
      CALL TRANSFORMATION id SOURCE XML iv_task_asxml
           RESULT data = lo_task.
    CATCH cx_root INTO DATA(lo_erreur) ##NEEDED.
      ASSERT 1 = 1. " Debug helper to set a break-point
  ENDTRY.

  lo_task->do( ).

  CALL TRANSFORMATION id SOURCE data = lo_task
       RESULT XML ev_task_asxml.
ENDFUNCTION.
