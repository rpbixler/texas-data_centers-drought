# Texas Drought and Data Centers: app folder

Built November 13, 2026. A companion to Texas Water SHiNE and the Texas Drought Explorer.
Lives in `drought_mapper/data_centers/`.

## To run the app right now

Set the working directory to this folder in RStudio, then:

    shiny::runApp()

`app.R` reads only `data/drought_datacenters.rds`, which is included (built November 13).
Packages: shiny, bslib, leaflet, sf, DT.

## What the map carries

| | Layer | Comes from |
|---|---|---|
| 1 | Texas House districts, groundwater conservation districts, groundwater management areas, major aquifers, river basins, regional flood planning regions, regional water planning areas | Water Atlas `atlas_house.rds`, `atlas_layers.rds`, `house_members.csv` (`__[V2_minimal_shiny]_SHiNE/data`) |
| 2 | 4,452 water utilities in eight kinds (4,384 have a location) | SHiNE `twnet_nodes.rds` (`__[shiny]_SHiNE/data`) |
| 3 | 5,216 links from utilities to their sources and sellers, 2024 (5,034 can be drawn) | SHiNE `twnet_edges.rds` |
| 4 | 261 Texas data centers | `datacenters_dchub_map_im3_complete.csv` in this folder |
| 5 | Texas Social Vulnerability Index by tract; Drought Monitor for Nov 3, 2026, Sep 20, 2022 and Oct 4, 2011 | `../data/drought_planning.rds` |

For each data center the build also records the seven districts it sits in, its tract's
social vulnerability, the Drought Monitor class at its point on each date, and the water
service area it sits in. Those are in the popup, the table page and the CSV download.

## To rebuild the data

    source("prepare_datacenter_data.R")

It reads the SHiNE project folder in Box and `../data/drought_planning.rds`, and writes
only inside this folder. If SHiNE lives somewhere else: `Sys.setenv(SHINE_ROOT = "<path>")`.
The source links use SHiNE's 2024 survey year (`REF_YEAR` at the top of the script).

## Hand file: geocode_overrides.csv

19 Texas rows in the data center file have no coordinates, among them the Abilene Stargate
buildings. They are placed at the centre of their town and marked "approximate" on the map
and in the table. To fix one, replace `lat`, `lon` and `method` on its row (rows are matched
on `file_row`, the row number in the csv) and rebuild. The script never overwrites this file.
Any row with no coordinates and no entry here is written to `datacenters_unmapped.csv`.

## To publish

    source("deploy.R")      # lbj-sspg, app name Texas-Drought-Data-Centers

## Things to know

- The data center file is not valid UTF-8 throughout; the build reads it as bytes and repairs the text.
- The file merges several public lists. One building can appear more than once under different tenants.
- A data center inside a service area is not shown to be that utility's customer.
- Aquifer and reservoir points sit at the centre of the counties the source lies in, so a link shows the tie, not a route.
- The Drought Monitor layers are trimmed to the state outline for display.
