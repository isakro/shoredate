test_that("skips the plotting of dates that are out of bounds and warns how many have been skipped", {
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)

  # shoreline_date() gives an expected warning here, which is already tested in
  # test-shoreline_date.R. This is therefore suppressed.
  target_dates <- suppressWarnings(shoreline_date(sites = target_points,
                                 elevation = c(46, 100, 200)))
  expect_warning(shoredate_plot(target_dates))
})

test_that("multiple isobases causes error with multiplot", {
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)
  target_dates <- shoreline_date(sites = target_points,
                                 elevation = c(46, 70, 100),
                                 isobase_direction = c(327, 333))
  err <- expect_error(shoredate_plot(target_dates, multiplot = TRUE))
  expect_equal(err$message, "The parameter setting multiplot = TRUE is not compatible with more than one isobase direction." )
})

test_that("dates omitted as out of bounds throws warning", {
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)
  # shoreline_date() gives an expected warning here, which is already tested in
  # test-shoreline_date.R.
  target_dates <- suppressWarnings(shoreline_date(sites = target_points,
                                 elevation = c(70, 46, 86, 200)))
  warn <- expect_warning(shoredate_plot(target_dates, multiplot = TRUE))
  expect_equal(warn$message, "Skipped one date that was out of bounds.")
})
