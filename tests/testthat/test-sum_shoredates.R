target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                            sf::st_point(c(538300, 6544250)))
target_points <- sf::st_as_sf(target_points, crs = 32632)

test_that("summing sparse dates", {
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62), sparse = TRUE)
  target_sum <- sum_shoredates(target_dates)
  expect_equal(target_sum$dates_n, 2)
})

test_that("summing dates with multiple isobase directions", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62),
                                 isobase_direction = c(327, 338))
  target_sum <- sum_shoredates(target_dates)
  expect_equal(target_sum$dates_n, 4)
})

test_that("summing sparse dates with multiple isobase directions", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62),
                                 isobase_direction = c(327, 338),
                                 sparse = TRUE)
  target_sum <- sum_shoredates(target_dates)
  expect_equal(target_sum$dates_n, 4)
})

test_that("date with more than 50% prob mass above 2500 BCE is excluded", {
  target_dates <- shoreline_date(target_points, elevation = c(19, 62))
  target_sum <- sum_shoredates(target_dates, cut_off_level = 0.5)
  expect_equal(target_sum$dates_n, 1)
})

