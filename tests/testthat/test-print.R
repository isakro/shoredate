target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)

test_that("A NA date is printed correctly", {
  target_date <- suppressWarnings(shoreline_date(site = target_point,
                                                 elevation = 200))
  expect_equal(print(target_date),
               cat(
                 "===============
Site:  1
Elevation:  200

95% HDR:
NA"))
})

test_that("A date with non-default isobase direction is printed correctly", {
  target_date <- shoreline_date(site = target_point, elevation = 30,
                                isobase_direction = 338)
  expect_equal(print(target_date),
               cat(
                 "===============
Site:  1
Elevation:  30

Isobase direction:  338

95% HDR:
4460 BCE-460 BCE"))
})

test_that("A date that extends into CE is printed correctly", {
  target_date <- shoreline_date(site = target_point, elevation = 25)
  expect_equal(print(target_date),
               cat(
                 "===============
Site:  1
Elevation:  25

95% HDR:
3630 BCE-120 CE"))
})

test_that("Dates that extend into CE with multiple isobase directions are printed correctly", {
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(572985, 6563115)))
  target_points <- sf::st_set_crs(target_points, 32632)
  target_dates <- shoreline_date(site = target_points, elevation = c(25, 60),
                                 isobase_direction = c(325, 338))
  expect_equal(print(target_dates),
               cat(
                 "===============
Site:  1
Elevation:  25

Isobase direction:  325

95% HDR:
3990 BCE-460 BCE
430 BCE-400 BCE
290 BCE-80 BCE
10 BCE-200 CE

Isobase direction:  338

95% HDR:
3990 BCE-460 BCE
470 BCE-400 BCE
290 BCE-80 BCE
10 CE-200 CE
===============
Site:  2
Elevation:  60

Isobase direction:  325

95% HDR:
7440 BCE-4990 BCE
4970 BCE-4960 BCE

Isobase direction:  338

95% HDR:
7610 BCE-4990 BCE"))
})

test_that("A date consisting of summed isobase directions is printed correctly", {
  target_date <- shoreline_date(sites = target_point, elevation = 25,
                                isobase_direction = c(327, 338),
                                sum_isobase_directions = TRUE)
  expect_equal(print(target_date),
               cat(
                 "===============
Site:  1
Elevation:  25

Sum of isobase directions:  327 338

95% HDR:
3620 BCE-0 CE"))
})
