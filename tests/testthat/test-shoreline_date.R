test_that("returns list of class shoreline_date", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_equal(class(target_date), c("shoreline_date", "list"))
})

test_that("progress is printed with verbose = TRUE", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_snapshot(shoreline_date(site = target_point,
                                 elevation = 46, verbose = TRUE ))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds,
          and specifies isobase direction if others than the default are provided", {
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150,
                                isobase_direction = 338))
})

test_that("undefined CRS throws error", {
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)))
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, "Undefined coordinate reference system. This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632).")
})

test_that("wrong CRS throws error and that this is printed", {
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 4326)
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, paste0("Target has coordinate reference system with EPSG ",
                                   sf::st_crs(target_point)$epsg,
                                   ". This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632)."))
})

test_that("gives warning if a site is located outside the limit of the study area", {
  target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 46))
  expect_equal(warn$message, "Target location is not within the study area for which the method was derived.")
})

test_that("gives warning and returns NA if date has a later start than 2500 BCE", {
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 17))
  expect_equal(warn$message, "Site 1 has a younger possible start date than 2500 BCE and is returned as NA.")
})

test_that("gives warning and returns NA if date has a later start than 2500 BCE and non-default isobase direction", {
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 17, isobase_direction = 338))
  expect_equal(warn$message, "Site 1 has a younger possible start date than 2500 BCE with an isobase direction of 338 and is returned as NA.")
})

test_that("lack of both elevation value and elevation raster throws error", {
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  err <- expect_error(shoreline_date(site = target_point))
  expect_equal(err$message, "A numeric value specifying the site elevation or an elevation raster must be provided.")
})

