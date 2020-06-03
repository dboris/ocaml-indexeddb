let () =
    Webtest_js.Runner.run
        ~with_colors:true
        Test_suite.js_api_tests