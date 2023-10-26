# shoredate (development version)

# shoredate 1.1.1 (2023-10-23)
- Added the parameter `date_probability_scale` to `shoredate_plot()` to make
 it possible to adjust the scaling of the probability distribution of the 
 shoreline date to better fit the plot.
 - Removed dependency on `rgdal` (@rsbivand, #9)

## shoredate 1.1.0 (2023-05-22)
- Added a vignette that demonstrates how the package can be applied to other
 regions outside south-eastern Norway (@benmarwick, #6).
- Removed the function `check_target_location()` and set up more specific and
 appropriate checks within `interpolate_curve()` and `target_plot()`.
- Added the package `rnaturalearth` to suggests in DESCRIPTION. This can be used
 with `target_plot()` to create a map for any area of the world, extending the 
 transferability of the package (@benmarwick, #6).
- Made most graphical parameters in `shoredate_plot()`, `displacement_plot()` 
 and `target_plot()` variables that be adjusted by the user (@kanishkan91, #5).
- Adjusted how the `geom` column of `sf` objects passed to `target_plot()` are 
 evaluated (@kanishkan91, #2).
- Cleaned up code to reduce the amount of `@Import` and `@ImportFrom` in 
 NAMESPACE, opting instead for the `package::function()` syntax
 (@kanishkan91, #4).
- Set up a pkgdown site for the package, served through GitHub at 
  https://isakro.github.io/shoredate/ (@kanishkan91, #3).

## shoredate 1.0.2 (2023-05-02)
 - `shoredate_hdr()` and `shoreline_date()` now returns the weighted mean date.
 - `shoreline_date()` now has the parameter model, which accepts either "none" 
 or "gamma". If "none" is specified then the distance between site and shoreline
 is assumed to be zero.
 - Now using the package `ggspatial` for scale bar in maps rather than `ggsn`, 
 due  to the retiring of r-spatial packages (@rsbivand, #1).

## shoredate 1.0.1 (2023-03-06)

 - `sum_shoredates()` can now handle NA dates.
 - `shoreline_date()` now accepts a vector of site names for the argument 
 `sites` if interpolated displacement curves are also provided.

## shoredate 1.0.0 (2023-01-19)
First release.
