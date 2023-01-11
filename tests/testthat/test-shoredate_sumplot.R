test_that("expected sumplot is produced", {
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                               sf::st_point(c(538300, 6544250)))
  target_points <- sf::st_as_sf(target_points, crs = 32632)

  target_dates <- shoreline_date(target_points,
                                 elevation = c(65, 70),
                                 cal_reso = 100)

  target_sum <- sum_shoredates(target_dates)
  p <- shoredate_sumplot(target_sum)
  vdiffr::expect_doppelganger("Plot of sum of multiple dates", p)
})
