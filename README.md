
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shoredate

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R-CMD-check](https://github.com/isakro/shoredate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/isakro/shoredate/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/isakro/shoredate/branch/master/graph/badge.svg)](https://app.codecov.io/gh/isakro/shoredate?branch=master)
<!-- badges: end -->

The goal of *shoredate* is to offer methods to shoreline date Stone Age
sites located along the Norwegian Skagerrak coast based on their
present-day elevation and the trajectory of past relative sea-level
change. The method is based on the likely elevation of the sites above
the contemporaneous sea-level when they were in use (see @roalkvam2023
for details).

## Installation

You can install the development version of *shoredate* from
[GitHub](https://github.com/isakro/shoredate) with:

``` r
# install.packages("devtools")
devtools::install_github("isakro/shoredate")
```

## Geographical and temporal coverage

As the method of shoreline dating is contingent on relative sea-level
change, it is dependent on good geological reconstructions of this
development. At present, the method as outlined here is therefore
limited to being applicable in the region between Horten in the north
east to Arendal in the south west. This region has recently compiled
shoreline displacement curves for. The region also formed the study area
for @roalkvam2023, in which the method and its parameters were derived.
The spatial coverage is indicated in the maps below:

    #> ℹ Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.
    #> ℹ Map tiles by Stamen Design, under CC BY 3.0. Data by OpenStreetMap, under ODbL.

<img src="man/figures/README-unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

Furthermore, as human occupation in the region only occurred some time
after the retreat of the ice, the currently oldest known sites are from
around 9300 BCE. The oldest possible age to achieve with *shoredate* is
9460 BCE, although no sites are yet known to be that old. A warning is
given if a site location is outside the spatial extent outlined above,
but the dating procedure is still performed. If a site has an elevation
that implies a date older than 9460 BCE the date is returned as NA and a
warning is given.

In @roalkvam2023 it was found that sites tend to be located on or close
to the shoreline up until around the transition to the Late Neolithic,
c. 2500 BCE, which thus marks the upper limit for the applicability of
the method. A date that has a later start date than this is therefore
returned as NA with a warning. Additionally, the geological displacement
curves are reported as years cal BP. If a date extends beyond 1950 CE
(which equates to 0 cal BP), thus indicating a site location below the
present sea-level, this overshooting probability is cut off and the date
is normalised to sum to unity.

## Interpolating shoreline displacement curve

To date a site a reconstruction of local shoreline displacement is
necessary. There are currently four reliable geological displacement
curves available from within the study area. Each of these is associated
with a shoreline isobase, along which the trajectory of relative
sea-level change has been the same. To find the local displacement
curve, the curves are interpolated to the site location using inverse
distance weighting—weighting the distances by the square of the inverse
distance between site and isobases.

``` r
library(shoredate)

# Create example point using the required coordinate system WGS84 UTM32N (EPSG: 32632).
target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)

target_curve <- interpolate_curve(target_point)

displacement_plot(target_curve)
```

<img src="man/figures/README-interpolate_curve-1.png" style="display: block; margin: auto;" />

This interpolation procedure is performed under the hood for each site
when calling shoreline_date().

## Example of shoreline dating a site

Below is a basic example outlining how to date a single site by manually
specifying the site elevation using the default setting for the dating
procedure and for plotting the date.

``` r
# Using the example point from above and specifying it's elevation.
target_date <- shoreline_date(site = target_point, elevation = 70)

# Call to plot.
shoredate_plot(target_date)
```

<img src="man/figures/README-date-1.png" style="display: block; margin: auto;" />

The blue gamma distribution on the y-axis indicates the likely elevation
of the site above sea-level when it was in use which is described by an
empirically derived gamma distribution with the parameters of shape
(![\\alpha](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Calpha "\alpha"))
= 0.286 and scale
(![\\sigma](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Csigma "\sigma"))
= 0.048. The red envelope is the shoreline displacement curve as
interpolated to the site location. Transferring the probability from the
gamma distribution to the calendar scale using the displacement curve
gives the resulting shoreline date in grey which is underlined with the
95% highest density region in black. By default, the shoreline date is
normalised to sum to unity. The default resolution on the calendar scale
is 10 years, but this can be adjusted to any power of 10 (including 1).

It is also possible to plot a more sparse version of the date by
specifying what elements are to be excluded:

``` r
shoredate_plot(target_date, elevation_distribution = FALSE, 
               displacement_curve = FALSE, highest_density_region = FALSE)
```

<img src="man/figures/README-sparse-1.png" style="display: block; margin: auto;" />

It is also possible to date multiple sites at once. The default
behaviour when providing multiple shoreline dates is to plot a series of
individual plots for each date. However, setting `multiplot = TRUE` when
calling shoredate_plot() collapses the dates on a single plot that is
more sparse, ordering the sites from earliest to latest possible start
date for the occupation of the sites.

``` r
# Creating multiple points to be dated.
target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(517491, 6511426)),
                              sf::st_point(c(502059, 6495402)))

# Specifying the correct CRS and making the points a sf data frame.
target_points <- sf::st_as_sf(target_points, crs = 32632)

# If a column is added to the data frame, this will be used as the site names.
# If not, the sites will simply be numbered as they are passed to
# shoreline_date().
target_points$names <- c("Example 1", "Example 2", "Example 3", "Example 4")

# Performing shoreline dating, specifying site elevations. 
target_dates <- shoreline_date(sites = target_points, 
                               elevation = c(70, 46, 62, 72))

# Plot the dates with 95% HDRs (these can be removed by setting 
# highest_density_region = FALSE).
shoredate_plot(target_dates, multiplot = TRUE)
```

<img src="man/figures/README-multiplot-1.png" style="display: block; margin: auto;" />
