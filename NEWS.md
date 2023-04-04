## shoredate 1.0.1.9000 (development version)
 - `shoredate_hdr()` and `shoreline_date()` now returns the weighted mean date.
 - `shoreline_date()` now has the parameter model, which accepts either "none" or "gamma". If "none" is specified then the distance between site and shoreline is assumed to be zero.

## shoredate 1.0.1 (2023-03-06)

 - `sum_shoredates()` can now handle NA dates.
 - `shoreline_date()` now accepts a vector of site names for the argument 
 `sites` if interpolated displacement curves are also provided.

## shoredate 1.0.0 (2023-01-19)
First release.
