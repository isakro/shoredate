test_that("skips the plotting of dates that are out of bounds and warns how many have been skipped", {
  target_points <- sf::st_sfc(sf::st_point(c(538310, 65442551)),
                              sf::st_point(c(538300, 65442500)),
                              sf::st_point(c(538250, 65442500)))
  target_points <- sf::st_set_crs(target_points, 32632)

  # shoreline_date gives an expected warning here, which is already tested in
  # test-shoreline_date.R. This is therefore suppressed.
  target_dates <- suppressWarnings(shoreline_date(sites = target_points,
                                 elevation = c(46, 100, 200)))
  expect_warning(shoredate_plot(target_dates))
})
