open OUnit

let _ =
  run_test_tt_main ("COW" >::: (Render.suite @ Extension.suite))
