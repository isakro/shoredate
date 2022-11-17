test_that("returns list", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_type(target_date, "list")
})
