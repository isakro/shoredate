test_that("returns expected plot when no parameters are passed", {
  p <- displacement_plot()
  vdiffr::expect_doppelganger("bare displacement plot", p)
})

test_that("returns expected plot when interpolated curve is passed", {
  target_point <- sf::st_sfc(sf::st_point(c(522623, 6526182)), crs = 32632)
  target_curve <- interpolate_curve(target_point)
  p <- displacement_plot(target_curve)
  vdiffr::expect_doppelganger("plot with interpolated curve", p)
})

test_that("returns expected plot with greyscale = TRUE", {
  target_point <- sf::st_sfc(sf::st_point(c(522623, 6526182)), crs = 32632)
  target_curve <- interpolate_curve(target_point)
  p <- displacement_plot(target_curve, greyscale = TRUE)
  vdiffr::expect_doppelganger("greyscale plot", p)
})
