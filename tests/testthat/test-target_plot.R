test_that("returns expected plot no targets are passed", {
  skip_on_cran()
  skip_on_os("linux")
  p <- target_plot()
  vdiffr::expect_doppelganger("bare study area plot", p)
})

test_that("returns expected plot when a target is passed", {
  skip_on_cran()
  skip_on_os("linux")
  set.seed(123) # For label placement
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  p <- target_plot(target_point)
  vdiffr::expect_doppelganger("plot with a target point", p)
})

test_that("returns expected plot in greyscale when a target is passed", {
  skip_on_cran()
  skip_on_os("linux")
  set.seed(123) # For label placement
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  p <- target_plot(target_point, greyscale = TRUE)
  vdiffr::expect_doppelganger("greyscale plot", p)
})

test_that("gives warning if one or more points are located outside the study area", {
    skip_on_cran()
    skip_on_os("linux")
    target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
    warn <- expect_warning(target_plot(target_point))
    expect_equal(warn$message, "Target location is not within the study area for which the method was derived.")
})
