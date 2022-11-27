test_that("Undefined CRS throws error", {
  target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)))
  err <- expect_error(interpolate_curve(target_pt))
  expect_equal(err$message, "Undefined coordinate reference system. This needs to be set to WGS84 UTM32N (EPSG: 32632).")
})

test_that("Check that wrong CRS throws error and that this is printed", {
  target_pt <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 4326)
  err <- expect_error(interpolate_curve(target_pt))
  expect_equal(err$message, paste0("Target has coordinate reference system with EPSG ",
                                   sf::st_crs(target_pt)$epsg,
                                   ". This needs to be set to WGS84 UTM32N (EPSG: 32632)."))
})
