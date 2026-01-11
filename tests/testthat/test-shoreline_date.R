test_that("returns list of class shoreline_date", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_equal(class(target_date), c("shoreline_date", "list"))
})

test_that("first column is returned as site name", {
  skip_on_cran()
  target_point <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(538310, 6544255)),
                                          crs = 32632))
  target_point$name <- "Example"
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_equal(target_point$name, target_date[[1]][[1]]$site_name)
})

test_that("progress is printed with verbose = TRUE", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_snapshot(shoreline_date(site = target_point,
                                 elevation = 46, verbose = TRUE ))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds,
          and specifies isobase direction if others than the default are provided", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150,
                                isobase_direction = 338))
})

test_that("undefined CRS throws error", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)))
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, "Undefined coordinate reference system. This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632).")
})

test_that("wrong CRS throws error and that this is printed", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 4326)
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, paste0("Target has coordinate reference system with EPSG ",
                                   sf::st_crs(target_point)$epsg,
                                   ". This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632)."))
})

test_that("gives warning if displacement is to be interpolated to a site located outside the limit of the study area", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 46))
  expect_equal(warn$message, "Target location is not within the study area for which the interpolation method was derived.")
})

test_that("gives warning and returns NA if date has a later start than 2500 BCE", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 17))
  expect_equal(warn$message, "Site 1 has a younger possible start date than 2500 BCE and is returned as NA.")
})

test_that("gives warning and returns NA if date has a later start than 2500 BCE and non-default isobase direction", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 17, isobase_direction = 338))
  expect_equal(warn$message, "Site 1 has a younger possible start date than 2500 BCE with an isobase direction of 338 and is returned as NA.")
})

test_that("lack of both elevation value and elevation raster throws error", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  err <- expect_error(shoreline_date(site = target_point))
  expect_equal(err$message, "Numeric values specifying the site elevations or an elevation raster must be provided.")
})

test_that("providing different number of elevation values than number of sites throws error", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  err <- expect_error(shoreline_date(site = target_point, elevation = c(56, 64)))
  expect_equal(err$message, "Specify one elevation value per site. 2 elevation values and 1 sites were provided.")
})

test_that("using no model instead of the gamma works", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  expect_snapshot(shoreline_date(site = target_point,
                                 elevation = 60,
                                 model = "none"))
})

test_that("summing multiple isobase directions works", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  expect_snapshot(shoreline_date(site = target_point, elevation = 60,
                      isobase_direction = c(327, 338),
                      sum_isobase_directions = TRUE))
})

test_that("finding site elevation from a raster works", {
  skip_on_cran()
  # Getting error for macos:
  # "[project] cannot get output boundaries for the target crs"
  # Does not help yo set use_gdal = FALSE
  # as suggested here https://github.com/rspatial/terra/issues/653
  skip_on_os("mac")
  # Having issues with R CMD check on GitHub with progress
  skip_if_not_installed("progress")
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  target_wgs84 <- sf::st_transform(target_point, crs = 4326)
  elev_raster <- suppressWarnings(elevatr::get_elev_raster(target_wgs84,
                                                         z = 14,  src = "aws"))
  elev_raster <- terra::project(terra::rast(elev_raster),
                                "epsg:32632")
  expect_snapshot(shoreline_date(target_point, elevation = elev_raster))
})

test_that("precomputing interpolation and passing site as a site name", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  precompiled_curve <- interpolate_curve(target_point)
  expect_snapshot(shoreline_date(site = "Example site", elevation = 60,
                                 target_curve = precompiled_curve))
})

test_that("passing displacement curve with different time interval and no isobase", {
  skip_on_cran()
  orland_disp <- get(load(system.file("extdata/orland_displacement_curve.rda",
                                      package = "shoredate")))
  expect_snapshot(shoreline_date(site = "Example site", elevation = 17,
                                 target_curve = orland_disp))
})

test_that("NA date with no isobase direction above elevation limit", {
  skip_on_cran()
  orland_disp <- get(load(system.file("extdata/orland_displacement_curve.rda",
                                      package = "shoredate")))
  warn <- expect_warning(shoreline_date(site = "Example site", elevation = 40,
                                 target_curve = orland_disp))
  expect_equal(warn$message, "The elevation of site Example site implies an earliest possible date older than -4050 BCE and is out of bounds. The date is returned as NA.")
})


test_that("site precisely on an isobase can be dated", {
  skip_on_cran()
  target_poly <- sf::st_as_sf(
    sf::st_sfc(sf::st_polygon(
      list(cbind(c(492361.7, 492361, 492359.8, 492358.7, 492357.1, 492355.2,
                   492354.5, 492354.7, 492355, 492355.4, 492355.6, 492356,
                   492356.5, 492357.1, 492357.8, 492357.7, 492358, 492358,
                   492357.2, 492356.2, 492355.2, 492354.3, 492353.3, 492352.3,
                   492351.4, 492350.7, 492349.7, 492348.5, 492345.3, 492344.1,
                   492343, 492342.2, 492341.8, 492341, 492340.3, 492339.5,
                   492339.1, 492338, 492338, 492339.2, 492339.3, 492339.4,
                   492339.8, 492340.7, 492341.8, 492343.1, 492344.6, 492345.3,
                   492346.1, 492346.9, 492348.1, 492348.8, 492349.4, 492349.6,
                   492350.5, 492350.7, 492350.2, 492350.2, 492349.9, 492349.4,
                   492350.1, 492350.2, 492350.9, 492351.7, 492352.9, 492353.5,
                   492353.9, 492354.4, 492355, 492355.4, 492356.3, 492357.2,
                   492358.1, 492359.3, 492359.6, 492360, 492360.7, 492361.1,
                   492361.1, 492361.6, 492362, 492362.3, 492363.9, 492364.9,
                   492365.6, 492366, 492366.4, 492366.8, 492367.2, 492368.1,
                   492369.5, 492370.9, 492372.2, 492373.4, 492374.3, 492376.3,
                   492378.2, 492379.3, 492380.9, 492381.7, 492383.2, 492384.8,
                   492385.9, 492386.9, 492387.7, 492388.3, 492389.4, 492390.2,
                   492391.4, 492392.3, 492393.4, 492394.5, 492395.4, 492396.2,
                   492397, 492397.2, 492397.7, 492398.1, 492398.3, 492398.2,
                   492396.5, 492394.8, 492392.7, 492391.4, 492389.6, 492387.5,
                   492385.2, 492383.7, 492382.7, 492381.5, 492380.3, 492378.5,
                   492377.7, 492376.7, 492375.7, 492375, 492374.4, 492373.9,
                   492373.1, 492372.5, 492371.6, 492371.3, 492371, 492370.5,
                   492369.9, 492369.1, 492368.5, 492367.9, 492367.5, 492366.9,
                   492366.3, 492365.6, 492364.9, 492364.3, 492363.3, 492362.5,
                   492361.8, 492361.7), c(6480250, 6480251, 6480251, 6480251,
                   6480250, 6480250, 6480250, 6480249, 6480249, 6480249,
                   6480249, 6480248, 6480248, 6480248, 6480248, 6480247,
                   6480247, 6480246, 6480247, 6480246, 6480247, 6480247,
                   6480247, 6480248, 6480249, 6480249, 6480249, 6480250,
                   6480250, 6480251, 6480251, 6480252, 6480252, 6480252,
                   6480253, 6480254, 6480254, 6480254, 6480256, 6480256,
                   6480257, 6480258, 6480259, 6480261, 6480262, 6480264,
                   6480265, 6480265, 6480266, 6480265, 6480265, 6480263,
                   6480262, 6480262, 6480261, 6480260, 6480261, 6480263,
                   6480264, 6480265, 6480266, 6480266, 6480267, 6480268,
                   6480268, 6480268, 6480267, 6480267, 6480266, 6480265,
                   6480264, 6480263, 6480261, 6480260, 6480260, 6480259,
                   6480257, 6480256, 6480255, 6480254, 6480254, 6480254,
                   6480253, 6480254, 6480254, 6480254, 6480254, 6480254,
                   6480254, 6480255, 6480255, 6480255, 6480255, 6480254,
                   6480254, 6480253, 6480252, 6480252, 6480252, 6480252,
                   6480252, 6480252, 6480252, 6480252, 6480252, 6480252,
                   6480252, 6480251, 6480251, 6480251, 6480251, 6480251,
                   6480251, 6480250, 6480249, 6480249, 6480249, 6480248,
                   6480248, 6480247, 6480247, 6480247, 6480247, 6480246,
                   6480246, 6480246, 6480245, 6480245, 6480245, 6480245,
                   6480245, 6480245, 6480245, 6480245, 6480244, 6480245,
                   6480245, 6480244, 6480244, 6480243, 6480243, 6480244,
                   6480244, 6480245, 6480245, 6480245, 6480246, 6480246,
                   6480247, 6480247, 6480248, 6480248, 6480248, 6480249,
                   6480249, 6480250, 6480250, 6480250)))), crs = 32632))
  expect_snapshot(shoreline_date(sites = target_poly, elevation = 15.4895))
})
