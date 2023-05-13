---
title: 'shoredate: An R package for shoreline dating coastal Stone Age sites'
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
date: 6 March 2023 
bibliography: paper.bib
output: word_document
---

# Summary

As a result of glacio-isostatic rebound, large regions of Fennoscandia have 
undergone a process of relative sea-level fall following the 
retreat of the Fennoscandian Ice Sheet. Furthermore, coastal Stone Age sites 
in the region appear to have been predominantly located on or close to the 
shoreline when they were in use. Based on their altitude relative to the
present-day sea-level, this can be combined with a reconstruction of 
past relative sea-level change to assign an approximate date to when the sites 
were in use. This method, called shoreline dating, has been used in the region 
since the early 1900s [e.g. @brogger1905] and is still widely applied today 
[e.g. @manninen2021; @solheim2018].

# Statement of need

`shoredate` is an R package developed for shoreline dating Stone Age sites on 
the coast of south-eastern Norway, based on local geological reconstructions
of past relative sea-level change. Drawing on an empirically derived
estimate of the likely elevation of the sites above sea-level when they
were in use, the method for shoreline dating implemented in the package was 
recently published in @roalkvam2023. No open-source software with which to
perform shoreline dating currently exists. The only
closed-source software available is `sealev` from the University of
Tromsø, Tromsø Geophysical Observatory
[<https://www.tgo.uit.no/sealev/>, see @moller2003], which can provide
non-probabilistic point estimates of shoreline dates based on data 
last updated in 2002.

`shoredate` is aimed at providing researchers and students dealing with
the coastal Stone Age of the region with tools for
performing and handling shoreline dates. This complements
software for handling radiocarbon dates and other sources of
temporal data, such as the R packages `rcarbon` [@crema2021], `bchron`
[@haslett2008], `oxcAAR` [@hinz2021], `kairos` [@frerebeau2022] and
`ArchaeoPhases` [@philippe2020], as well as closed-source software such as
`OxCal` [@bronkramsey2009].

Shoreline dating is frequently applied in the research and cultural
resource management sectors in Norway, both to plan archaeological
investigations and for establishing temporal frameworks with which to
analyse the archaeological material. Case-studies employing `shoredate`
are currently being undertaken. Furthermore, future archaeological
material can be drawn on to further test the method as it is implemented here, 
and potentially lead to adjustments in how it should be applied in a given 
setting.

# Spatial and temporal coverage

As the method of shoreline dating is dependent on reliable
reconstructions of relative sea-level change, the package was developed to be 
applied in the coastal region between Horten in the north east to Arendal in the
south west (\autoref{fig:coverage}). Geologically derived displacement curves 
from this region have recently been published for Skoppum in Horten 
[@romundset2021], Gunnarsrød in Porsgrunn [@sorensen2023], Hanto in Tvedestrand
[@romundset2018] and Bjørnebu in Arendal [@romundset2018b]. The spatial coverage
of `shoredate`  will be extended to surrounding areas as forthcoming data on 
shoreline displacement becomes available. Furthermore, although the direct 
applicability of the method in other regions remains undetermined,
suggestions and examples of how such extensions can be achieved is included in
the documentation for the package.

Following from the latest start date among the geological displacement curves, 
9469 BCE marks the lower temporal limit of the package within the spatial limit 
in south-eastern Norway. The oldest verified anthropogenic activity in Norway 
currently dates to around 9300 BCE [@glorstad2016]. In @roalkvam2023 it was 
found that sites in the region tend to be located at more variable distances 
from the shoreline after c. 2500 BCE. This therefore marks the upper temporal 
limit for shoreline dating in the region.

![The spatial and temporal coverage for which the package was developed. A) The 
location of the spatial coverage in south-eastern Norway. B) The location of the
isobases that represent contours along which the shoreline displacement has 
followed the same trajectory. C) The displacement curves corresponding to the 
isobases, where the temporal limits are marked with dashed lines.
\label{fig:coverage}](../man/figures/coverage_annotated.png)

# Example of base functionality

To date a site, its elevation above present sea-level must be provided when 
running the function `shoreline_date()`. This can be done by either manually
specifying the site elevation, or by providing an elevation raster of class 
`SpatRaster` from the `terra` package [@hijmans2022] from where this is derived.
Unless a pre-compiled curve is provided, the trajectory of shoreline 
displacement at the location of the site is then interpolated under the hood 
with the function  `interpolate_curve()`, using inverse distance weighting when 
`shoreline_date()` is called. This is based on the distance between the site and 
the isobases of the displacement curves. To perform this interpolation, the site
geometry has to provided as a spatial object of class `sf` from the `sf` 
package [@pebesma2018].

\autoref{fig:example_site} shows the location of an example site,
plotted by passing it to `target_plot()`. \autoref{fig:example_curve} 
displays the result of running the command `interpolate_curve()` on the example
site, and plotting the resulting interpolated displacement curve with
`displacement_plot()`. Finally, \autoref{fig:example_date} shows
the result of dating the example site with `shoreline_date()` when
manually specifying that the site is situated at 58.8m above present sea-level. 
The resulting date is plotted with the function `shoredate_plot()`.

![The location of the example site relative to the isobases of the
displacement curves. The base map is a simplified light-weight map of
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
