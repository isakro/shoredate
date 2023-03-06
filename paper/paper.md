---
title: 'shoredate: An R package for shoreline dating coastal Stone Age sites in south-eastern Norway'
tags:
  - R
  - archaeology
  - shoreline dating
  - relative sea-level change
  - south-eastern Norway
authors:
  - name: Isak Roalkvam
    orcid: 0000-0001-6974-1374
    affiliation: 1
affiliations:
 - name: University of Oslo, Institute of Archaeology, Conservation and History
   index: 1
# date: 6 March 2023 
bibliography: paper.bib
output: 
  pdf_document
---

# Summary

As a result of glacio-isostatic rebound, large regions of Fennoscandia have 
undergone a process of relative sea-level fall following the 
retreat of the Fennoscandian Ice Sheet. Furthermore, coastal Stone Age sites 
in the region appear to have been predominantly located on or close to the 
shoreline when they were in use. This can be combined with a reconstruction of 
past relative sea-level change to assign an approximate date to when the sites 
were in use, based on their altitude relative to the present-day sea-level. 
This method, called shoreline dating, has been used in the region since the 
early 1900s [e.g. @brogger1905] and is still widely applied today [e.g.
@solheim2018; @manninen2021].

# Statement of need

`shoredate` is an R package for shoreline dating Stone Age sites on the
coast of south-eastern Norway, based on local geological reconstructions
of past relative sea-level change. Drawing on an empirically derived
estimate of the likely elevation of the sites above sea-level when they
were in use, the method for shoreline dating implemented in the package was 
recently published in @roalkvam2023. No open-source software with which to
perform shoreline dating exists. The only
closed-source software available is `sealev` from the University of
Tromsø, Tromsø Geophysical Observatory
[<https://www.tgo.uit.no/sealev/>, see @moller2003], which provides
non-probabilistic point estimates of shoreline dates based on data last
updated in 2002.

`shoredate` is aimed at providing researchers and students dealing with
the coastal Stone Age of south-eastern Norway with tools for
performing and handling shoreline dates. This complements
software for handling radiocarbon dates and other sources of
temporal data, such as the R packages `rcarbon` [@crema2021], `bchron`
[@haslett2008], `oxcAAR` [@hinz2021], `kairos` [@frerebeau2022] and
`ArchaeoPhases` [@philippe2020], as well as proprietary software such as
`OxCal` [@bronkramsey2009].

Shoreline dating is frequently applied in the research and cultural
resource management sectors in Norway, both to plan archaeological
investigations and for establishing temporal frameworks with which to
analyse the archaeological material. Case-studies employing `shoredate`
are currently being undertaken. Furthermore, future archaeological
material can be drawn on to further test the method as it is implemented here, 
and potentially lead to adjustments in how it could be applied in a given 
setting.

# Spatial and temporal coverage

As the method of shoreline dating is dependent on reliable
reconstructions of relative sea-level change, the package is at present
limited to being applicable in the coastal region between Horten in the
north east to Arendal in the south west (\autoref{fig:coverage}).
Geologically derived displacement curves from this region have recently
been compiled for Skoppum in Horten [@romundset2021], Gunnarsrød in
Porsgrunn [@sorensen2023], Hanto in Tvedestrand [@romundset2018] and Bjørnebu in
Arendal [@romundset2018b]. The spatial coverage of `shoredate`  will be extended
to surrounding regions as forthcoming data on shoreline displacement
becomes available. 

Following from the latest start date among the displacement curves, 
9469 BCE marks the lower temporal limit of the package. The oldest verified 
anthropogenic activity in Norway currently dates to around 9300 BCE 
[@glorstad2016]. In @roalkvam2023 it was found that sites tend to be located at 
more variable distances from the shoreline after c. 2500 BCE. This therefore 
marks the upper temporal limit of the package.

![The spatial and temporal coverage of the package. The first figure
displays the location of the spatial extent in south-eastern Norway. The
second figure displays the location of the isobases, representing contours
along which the shoreline displacement has followed the same
trajectory. The isobases correspond to the displacement curves in the
third figure, where the temporal limits are marked with dashed
lines.\label{fig:coverage}](../man/figures/coverage.png)

# Example of base functionality

To shoreline date a site, this has to provided as a spatial object of
class `sf` from the `sf` package [@pebesma2018], and be set to the coordinate 
reference system WGS 84 / UTM zone 32N (EPSG:32632). The elevation of the site
above present sea-level must be provided when running `shoreline_date()`. This
can be done by either manually specifying the site elevation, or by providing an 
elevation raster of class `SpatRaster` from the `terra` package [@hijmans2022], 
from where this is derived. When calling `shoreline_date()`, the trajectory of 
shoreline displacement at the location of the site is interpolated under the 
hood with the function `interpolate_curve()`, using inverse distance
weighting. This is based on the distance between the site and the isobases of
the displacement curves.

\autoref{fig:example_site} shows the location of an example site,
plotted by passing it to `target_plot()`. \autoref{fig:example_curve} 
displays the result of running the command `interpolate_curve()` on the example
site, and plotting the resulting interpolated displacement curve with
`displacement_plot()`. Finally, \autoref{fig:example_date} shows
the result of dating the example site with `shoreline_date()` when
manually specifying that the site is situated at 58.8m above present sea-level. 
The resulting date is plotted with the function `shoredate_plot()`.

![The location of the example site relative to the isobases of the
displacement curves. The base map is a simplified and lightweight map of
the region.\label{fig:example_site}](example_site.png) 

![The curve interpolated to the example site by means of inverse distance 
weighting. This is based on the
distance between the site and the isobases of the geological
displacement curves.\label{fig:example_curve}](example_curve.png) 

![The resulting shoreline date
for the example site. The blue gamma distribution on the y-axis
indicates the likely elevation of the site above sea-level when it was
occupied. The red envelope is the interpolated shoreline displacement
curve for the site location. The resulting shoreline date in grey is the
result of transferring the probability from the gamma distribution to
the calendar scale by coupling it with the displacement curve. The date is
underlined with the 95% highest density region (HDR) in black.
\label{fig:example_date}](example_date.png)

# Acknowledgements

I owe great thanks to David Wright, Anders Romundset, Ingrid Fuglestvedt, Per 
Persson, Steinar Solheim and Hallvard Bruvoll for valuable feedback during
work with this project.

# References
