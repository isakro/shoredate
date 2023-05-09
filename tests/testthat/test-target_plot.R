test_that("returns expected plot when no targets are passed", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot()
  vdiffr::expect_doppelganger("no targets", p)
})

# For label placement
set.seed(123)

# To be reused
target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)

test_that("returns expected plot when a target is passed", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point)
  vdiffr::expect_doppelganger("target point", p)
})

test_that("returns expected plot when isobases are excluded", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, isobases = NA)
  vdiffr::expect_doppelganger("exclude isobases", p)
})

test_that("returns expected plot when basemap is excluded", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, basemap = NA)
  vdiffr::expect_doppelganger("exclude basemap", p)
})

test_that("returns expected plot when only targets are plotted", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, isobases = NA, basemap = NA)
  vdiffr::expect_doppelganger("only targets", p)
})

test_that("returns expected plot when only isobases are plotted", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(targets = NA, basemap = NA)
  vdiffr::expect_doppelganger("only isobases", p)
})

test_that("returns expected plot when only basemap is plotted", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(targets = NA, isobases = NA)
  vdiffr::expect_doppelganger("only basemap", p)
})

test_that("returns expected plot when graphical parameters are adjusted", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point,
                   target_shape = 24,
                   target_col = "chartreuse",
                   target_fill = "black",
                   target_size = 2.25,
                   isobase_line = c("Horten" = "dashed",
                                    "Porsgrunn" = "dashed",
                                    "Tvedestrand" = "dashed",
                                    "Arendal" = "dashed"),
                   isobase_col = c("Arendal" = "gold",
                                   "Porsgrunn" = "hotpink",
                                   "Tvedestrand" = "firebrick2",
                                   "Horten" = "thistle1"))
  vdiffr::expect_doppelganger("graphical parameters", p)
})

test_that("returns expected plot in greyscale when a target is passed", {
  # skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, greyscale = TRUE)
  vdiffr::expect_doppelganger("greyscale", p)
})

test_that("plotting with naturalearthdata works", {
  set.seed(123)
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(532719, 7065723)), crs = 32632)
  p <- target_plot(target_point, naturalearth_basemap = TRUE)
  vdiffr::expect_doppelganger("naturalearth basemap", p)
})


test_that("gives warning targets are located outside the basemap", {
    skip_on_cran()
    target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
    warn <- expect_warning(target_plot(target_point))
    expect_equal(warn$message, "Basemap and targets do not intersect.")
})



