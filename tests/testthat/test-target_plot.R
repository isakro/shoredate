test_that("gives warning if one or more points are located outside the study area", {
    target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
    warn <- expect_warning(target_plot(target_point))
    expect_equal(warn$message, "Target location is not within the study area for which the method was derived.")
})
