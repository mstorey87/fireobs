# fireobs package
This a package for use at the University of Wollongong's Centre for Environmental Risk Management of Bushfires, but others may find the functions useful. It contains functions to search for and download satellite images, fire line scans and progressions polygons for fires in Australia. Contact mstorey@uow.edu.au.

## Main tools

**Satellite imagery and hotspots**

- `fire_himawari_search()` / `fire_himawari_download()`: Himawari-8/9 geostationary images (every 10 minutes) from the NCI archive and near-real-time catalogues, clipped to the fire area.
- `fire_stac_search()` / `fire_stac_download()`: Landsat and Sentinel-2 false-colour images from Digital Earth Australia.
- `fire_hotspots_search()` / `fire_hotspots_map()`: NASA FIRMS hotspots (VIIRS, MODIS) and daily NASA GIBS images with the hotspots drawn on.

**CERMB fire database** (requires UOW network access)

- `fire_search_images()`, `fire_search_progs()`, `fire_search_ros()`: linescans, fire progression polygons and rate-of-spread lines for a fire area and time window.

**Fire spread analysis**

- `fire_max_spread_line()`: maximum spread lines between successive progression polygons, with distance, rate of spread and bearing.
- `fire_SAM()`: maps fire boundaries from images using the Segment Anything Model (SAM2).

**Weather, fuel and vegetation**

- `fire_barra_sample_all()`: BARRA reanalysis weather (e.g. wind, temperature, humidity) for points, lines or polygons over time.
- `fire_sample_agcd()`, `fire_anu_sample()`, `fire_stac_sample_veg()`: rainfall (AGCD), fuel moisture and flammability (ANU), and vegetation and elevation (DEA).

**Utilities**

- `fire_bbox_polygon()`, `fire_get_timezone()`, `fire_uv2ds()` and others: bounding boxes, local time zones and wind conversion.

## Installation

```r
remotes::install_github("mstorey87/fireobs")
