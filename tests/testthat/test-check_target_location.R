test_that("gives warning if a site is located outside the limit of the study area", {
  target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
  expect_warning(check_target_location(target_point))
})
