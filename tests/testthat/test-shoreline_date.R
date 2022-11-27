test_that("returns list", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_type(target_date, "list")
})

test_that("throws error if resolution is not a power of ten", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
  err <- expect_error(shoreline_date(site = target_point, elevation = 46,
                                cal_reso = 5))
  expect_equal(err$message, "Resolution on calendar scale must be powers of 10 (including 1).")
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds,
          and specifies isobase direction if others than the default are provided", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 65442551)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150, isobase_direction = 338))
})
