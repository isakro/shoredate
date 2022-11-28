test_that("returns list", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_type(target_date, "list")
})

test_that("throws error if resolution is not a power of ten", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  err <- expect_error(shoreline_date(site = target_point, elevation = 46,
                                cal_reso = 5))
  expect_equal(err$message, "Resolution on calendar scale must be powers of 10 (including 1).")
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds,
          and specifies isobase direction if others than the default are provided", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150, isobase_direction = 338))
})

test_that("undefined CRS throws error", {
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)))
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, "Undefined coordinate reference system. This needs to be set to WGS84 UTM32N (EPSG: 32632).")
})

test_that("wrong CRS throws error and that this is printed", {
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 4326)
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, paste0("Target has coordinate reference system with EPSG ",
                                   sf::st_crs(target_point)$epsg,
                                   ". This needs to be set to WGS84 UTM32N (EPSG: 32632)."))
})

test_that("gives warning if a site is located outside the limit of the study area", {
  target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
  err <- expect_warning(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, "Target location is not located within the study area for which the method was derived.")
})
