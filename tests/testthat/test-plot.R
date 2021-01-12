test_that("plot", {

  test_that("plot graph of edibble variables", {
    des <- start_design("An awesome design") %>%
      set_units(mainplot = 4,
                subplot = nested_in(mainplot, 2))

    expect_doppelganger("plot design: low-view",
                        function() plot(des, view = "low"))
    expect_doppelganger("plot design: high-view",
                        function() plot(des, view = "high"))
    expect_doppelganger("plot graph: low-view",
                        function() plot(des$graph, view = "low"))
    expect_doppelganger("plot graph: high-view",
                        function() plot(des$graph, view = "high"))
    expect_doppelganger("plot table: low-view",
                        function() plot(serve_table(des)), view = "low")
    expect_doppelganger("plot table: high-view",
                        function() plot(serve_table(des)), view = "high")

    skip("Interactive plots")


    iplot(des, view = "low")
    iplot(des, view = "high")
    iplot(des$graph)
    iplot(serve_table(des))

  })

})
