test_that("epsg_from_cog", {
  expect_equal(epsg_from_cog("35145"), 2154)
})
