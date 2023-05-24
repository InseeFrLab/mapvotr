addr <- data.frame(
  cog = c("C1", "C1", "C1", "C2", "C2", "C2") ,
  id_bv = c("bv1", "bv1", "bv2", "bv1", "bv1", "bv1")
)
res <- mapvotr:::fget_multiBV(addr,var_cog = "cog", var_bv = "id_bv")

test_that("fget_multiBV works", {
  expect_true(identical(res, "C1"))
})
