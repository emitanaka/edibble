test_that("graph-utils", {
  des1 <- start_design() %>%
    set_units(site = 3,
              plot = nested_in(site, 10),
              sample = nested_in(plot, 3))
  expect_equal(vid(des1$vgraph, "plot"), 2)
  expect_equal(vparent(des1$vgraph, vid(des1$vgraph, "plot")), 1)
  expect_equal(vancestor(des1$vgraph, vid(des1$vgraph, "plot")), c(2, 1))
  expect_equal(vancestor(des1$vgraph, vid(des1$vgraph, "sample")), c(3, 2, 1))
})
