let () =
    [Test_suite.js_api_tests; Test_suite.idb_lwt_tests]
    |> List.iter (Webtest_js.Runner.run ~with_colors:true)
