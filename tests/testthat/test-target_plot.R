test_that("returns expected plot no targets are passed", {
  p <- target_plot()
  vdiffr::expect_doppelganger("bare study area plot", p)
})

test_that("returns expected plot when a target is passed", {
  set.seed(123) # For label placement
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  p <- target_plot(target_point)
  vdiffr::expect_doppelganger("plot with a target point", p)
})

test_that("gives warning if one or more points are located outside the study area", {
    target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
    warn <- expect_warning(target_plot(target_point))
    expect_equal(warn$message, "Target location is not within the study area for which the method was derived.")
})
