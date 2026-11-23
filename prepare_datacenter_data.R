# =============================================================================
# prepare_datacenter_data.R
# Texas Drought and Data Centers: build data/drought_datacenters.rds
#
# R. Patrick Bixler, PhD | LBJ School of Public Affairs | The University of Texas at Austin
#
# Joins five things that already exist into the one file app.R reads. Nothing is
# downloaded, and nothing outside this folder is written to.
#
#   (1) Boundaries       Texas House districts, groundwater conservation districts,
#                        groundwater management areas, major aquifers, river basins,
#                        regional flood planning regions, regional water planning areas
#                        <- the Water Atlas files atlas_house.rds and atlas_layers.rds
#   (2) Utilities        the eight kinds of water utility in SHiNE (UTILITY_CLASSES)
#                        <- SHiNE twnet_nodes.rds
#   (3) Source links     each utility's link to the sources it draws on, and to the
#                        sellers it buys from, in the reference year
#                        <- SHiNE twnet_edges.rds
#   (4) Data centers     the Texas rows of the data center file in this folder
#   (5) Drought frames   the Texas Social Vulnerability Index by census tract and the
#                        stored U.S. Drought Monitor maps
#                        <- ../data/drought_planning.rds (Texas Drought Explorer)
#
# Run from this folder (drought_mapper/data_centers):   source("prepare_datacenter_data.R")
# Needs: sf. Takes well under a minute.
#
# WHAT IS IN drought_datacenters.rds
#   boundaries    named list of seven sf layers, each with a `label` (House districts
#                 also carry the member's name and party when house_members.csv is there)
#   utilities     one row per utility: name, kind, place, population, its tract-based
#                 social vulnerability, Drought Monitor class, and largest source
#   links         one row per utility x source (or seller) in the reference year:
#                 acre-feet, share of the utility's supply, and both end points
#   sources       the source and seller points that the links run to
#   datacenters   one row per Texas data center, with every boundary it sits in, its
#                 tract's social vulnerability, the Drought Monitor class at its
#                 location on each stored date, and the utility whose service area
#                 it sits in
#   tracts        census tracts with the social vulnerability score
#   usdm          Drought Monitor polygons: this week and the stored past weeks
#   meta          build date, reference year, counts, notes on anything left out
#
# Data centers with no coordinates in the file are placed from
# geocode_overrides.csv (a hand file; never overwritten). Rows it does not cover
# are written to datacenters_unmapped.csv and left off the map.
# =============================================================================

suppressPackageStartupMessages(library(sf))
sf_use_s2(FALSE)

# ---- SETTINGS ----------------------------------------------------------------
SHINE_ROOT <- Sys.getenv("SHINE_ROOT", unset = path.expand(
  "~/Library/CloudStorage/Box-Box/Texas Water Foundation/03_Water_Atlas/Texas Water Source-Use Network"))
SHINE_DATA <- Sys.getenv("SHINE_APP_DATA", unset = file.path(SHINE_ROOT, "__[shiny]_SHiNE", "data"))
ATLAS_DATA <- Sys.getenv("ATLAS_DATA",     unset = file.path(SHINE_ROOT, "__[V2_minimal_shiny]_SHiNE", "data"))
DROUGHT    <- Sys.getenv("DROUGHT_RDS",    unset = file.path("..", "data", "drought_planning.rds"))
DC_FILE    <- "datacenters_dchub_map_im3_complete.csv"
OVERRIDES  <- "geocode_overrides.csv"
OUT        <- file.path("data", "drought_datacenters.rds")

REF_YEAR   <- 2024          # the SHiNE survey year the source links are drawn from
GAL_PER_ACFT <- 325851

# The utilities to carry, exactly as SHiNE names them (org_class)
UTILITY_CLASSES <- c(
  "Private / investor-owned or contract-operated utility",
  "City / municipal utility",
  "Water supply corporation (WSC)",
  "Utility district (MUD / UD / PUD)",
  "Small community system (owner not evident from name)",
  "Other water district (WCID / FWSD / irrigation / improvement / management)",
  "Special utility district (SUD)",
  "River authority / regional water authority / wholesale district")

# The boundary layers, in the order the app lists them
BOUNDARY_LAYERS <- c("Texas House districts", "Groundwater conservation districts", "Groundwater management areas",
                     "Major aquifers", "River basins", "Regional flood planning regions", "Regional water planning areas")

notes <- character(0)
note  <- function(...) { m <- paste0(...); notes <<- c(notes, m); message("  NOTE: ", m) }
need  <- function(f) if (!file.exists(f)) stop("Not found: ", f, call. = FALSE) else f
blank <- function(x) is.na(x) | trimws(as.character(x)) == ""
clean <- function(x) { x <- trimws(as.character(x)); x[x == ""] <- NA_character_; x }
# Read a csv whatever its encoding: a byte-order mark is dropped and stray bytes never stop the read
read_csv_any <- function(f) {
  x <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE)
  names(x)[1] <- sub("^[^A-Za-z0-9_]+", "", names(x)[1], useBytes = TRUE)   # byte-order mark
  for (j in names(x)) if (is.character(x[[j]])) {
    v <- x[[j]]; bad <- !validUTF8(v)
    v[bad] <- iconv(v[bad], "latin1", "UTF-8"); Encoding(v) <- "UTF-8"; x[[j]] <- v
  }
  x
}
make_valid <- function(x) suppressWarnings(suppressMessages(st_make_valid(x)))

# For each point, the label of the polygon it falls in (the smallest one, where several overlap)
label_at <- function(pts, polys, field = "label") {
  hit  <- suppressMessages(st_intersects(pts, polys))
  area <- suppressWarnings(as.numeric(st_area(polys)))
  vapply(hit, function(i) if (!length(i)) NA_character_ else as.character(polys[[field]][i[which.min(area[i])]]), character(1))
}

# =============================================================================
# 1. BOUNDARIES
# =============================================================================
message("1. Boundaries")
atlas <- readRDS(need(file.path(ATLAS_DATA, "atlas_layers.rds")))
house <- readRDS(need(file.path(ATLAS_DATA, "atlas_house.rds")))

f_mem <- file.path(ATLAS_DATA, "house_members.csv")
house$member <- NA_character_; house$party <- NA_character_; house$member_url <- NA_character_
members_as_of <- NA_character_
if (file.exists(f_mem)) {
  mem <- read_csv_any(f_mem)
  m <- match(house$district, mem$district)
  house$member <- clean(mem$name[m]); house$party <- clean(mem$party[m]); house$member_url <- clean(mem$url[m])
  members_as_of <- max(mem$retrieved, na.rm = TRUE)
} else note("house_members.csv not found; House districts carry no member names.")

boundaries <- c(list("Texas House districts" = house[order(house$district), ]),
                atlas$layers[intersect(BOUNDARY_LAYERS, names(atlas$layers))])
missing_layers <- setdiff(BOUNDARY_LAYERS, names(boundaries))
if (length(missing_layers)) note("Boundary layers not in the Atlas file: ", paste(missing_layers, collapse = "; "))
boundaries <- lapply(boundaries[intersect(BOUNDARY_LAYERS, names(boundaries))], function(b) make_valid(st_transform(b, 4326)))
for (n in names(boundaries)) message(sprintf("   %-40s %4d", n, nrow(boundaries[[n]])))

# =============================================================================
# 2. UTILITIES (SHiNE) and 3. THEIR SOURCE LINKS
# =============================================================================
message("2. Utilities")
N  <- readRDS(need(file.path(SHINE_DATA, "twnet_nodes.rds")))
E  <- readRDS(need(file.path(SHINE_DATA, "twnet_edges.rds")))
TM <- readRDS(need(file.path(SHINE_DATA, "twnet_meta.rds")))
DX <- readRDS(need(DROUGHT))

is_util <- N$org_class %in% UTILITY_CLASSES
U <- N[is_util, c("node_id", "name", "type", "org_class", "county", "region", "basin", "lat", "lon", "geo_quality",
                  "pop", "connections", "tceq_pws_id", "website", "owner")]
U$org_class <- factor(U$org_class, levels = UTILITY_CLASSES)
U$row <- which(is_util)                                   # position in SHiNE's node table (what the edges point to)

# The drought explorer's system measures, where it has the system
sp <- st_drop_geometry(DX$system_points)
carry <- intersect(c("svi_pct", "svi_quintile", "usdm_now", "usdm_20220920", "usdm_20111004", "top_source", "top_source_share",
                     "share_groundwater", "share_surface_water", "share_purchased", "sole_source", "res_pct_full_now",
                     "tceq_stage"), names(sp))
U <- cbind(U, sp[match(U$node_id, sp$node_id), carry, drop = FALSE])
rownames(U) <- NULL
message(sprintf("   %d utilities in the eight kinds; %d have no location and are listed but not mapped", nrow(U), sum(is.na(U$lat))))
print(table(U$org_class))

message("3. Source links, ", REF_YEAR)
if (!REF_YEAR %in% E$year) stop("SHiNE has no edges for ", REF_YEAR, call. = FALSE)
e <- E[E$year == REF_YEAR & E$tgt %in% U$row & !is.na(E$gal) & E$gal > 0, ]
e$water_name <- TM$water_levels[e$water]
# one line per utility x source: volumes added across water types, the largest type kept as the line's type
key <- paste(e$src, e$tgt)
big <- e[order(key, -e$gal), ]; big <- big[!duplicated(paste(big$src, big$tgt)), ]
L <- data.frame(src = big$src, tgt = big$tgt, flow = TM$flow_levels[big$flow], water = big$water_name, stringsAsFactors = FALSE)
L$acft  <- as.numeric(tapply(e$gal, key, sum)[paste(L$src, L$tgt)]) / GAL_PER_ACFT
L$share <- L$acft / as.numeric(tapply(L$acft, L$tgt, sum)[as.character(L$tgt)])
L$utility_id  <- N$node_id[L$tgt]
L$source_id   <- N$node_id[L$src]
L$source_name <- N$name[L$src]
L$source_type <- N$type[L$src]
L$kind <- ifelse(L$flow == "System-to-System Transfer", "Purchased from another system", "Own intake")
L$lon1 <- N$lon[L$src]; L$lat1 <- N$lat[L$src]
L$lon2 <- N$lon[L$tgt]; L$lat2 <- N$lat[L$tgt]
# SHiNE keeps self-supplied reuse as one statewide node with no real location: listed, never drawn
own_reuse <- L$source_id == "RU_REUSE_UNSPECIFIED"
L$drawn <- !own_reuse & !is.na(L$lon1) & !is.na(L$lon2)
L <- L[order(L$utility_id, -L$acft), setdiff(names(L), c("src", "tgt", "flow"))]
rownames(L) <- NULL
message(sprintf("   %d links for %d utilities; %d can be drawn", nrow(L), length(unique(L$utility_id)), sum(L$drawn)))

S <- N[N$node_id %in% L$source_id[L$drawn], c("node_id", "name", "type", "org_class", "lat", "lon", "geo_quality")]
S$n_utilities <- as.integer(table(L$source_id)[S$node_id])
S$acft <- as.numeric(tapply(L$acft, L$source_id, sum)[S$node_id])
rownames(S) <- NULL
U$n_links <- as.integer(table(factor(L$utility_id, levels = U$node_id)))
U$row <- NULL

# =============================================================================
# 4. DATA CENTERS
# =============================================================================
message("4. Data centers")
d <- read_csv_any(need(DC_FILE))
d$file_row <- seq_len(nrow(d)) + 1L                       # the row as it appears in the csv (header is row 1)
d <- d[toupper(trimws(d$state)) %in% c("TX", "TEXAS"), ]
num <- function(x) suppressWarnings(as.numeric(gsub(",", "", x)))
DC <- data.frame(
  dc_id     = paste0("dc", d$file_row),
  file_row  = d$file_row,
  site      = clean(d$site_name),
  operator  = clean(d$operator_name),
  address   = clean(d$address),
  city      = clean(d$city),
  county    = clean(sub("\\s+County$", "", d$county, ignore.case = TRUE)),
  lat       = num(d$latitude), lon = num(d$longitude),
  year_built = suppressWarnings(as.integer(d$year_built)),
  mw        = num(d[["Capacity (MW)"]]),
  size_note = clean(d$square_feet),                       # square feet in most rows, acres ("393 ac") in some
  status_raw = clean(d$Status),
  listed_by = clean(d$source),
  reference = if (ncol(d) >= 17) clean(d[[17]]) else NA_character_,
  stringsAsFactors = FALSE)
st <- tolower(ifelse(is.na(DC$status_raw), "", DC$status_raw))
DC$status <- factor(ifelse(st == "operational", "Operational",
                    ifelse(st == "under construction", "Under construction",
                    ifelse(st %in% c("planned", "approved", "under development"), "Planned or approved", "Status not given"))),
                    levels = c("Operational", "Under construction", "Planned or approved", "Status not given"))
DC$name <- ifelse(!is.na(DC$site), DC$site, ifelse(!is.na(DC$operator), DC$operator,
           ifelse(!is.na(DC$city), paste("Data center,", DC$city), "Data center (unnamed)")))
DC$located <- ifelse(is.na(DC$lat) | is.na(DC$lon), NA_character_, "coordinates in the file")

# Rows with no coordinates: placed from the hand file, matched on the csv row number
if (file.exists(OVERRIDES)) {
  ov <- read_csv_any(OVERRIDES)
  ov <- ov[!is.na(ov$lat) & !is.na(ov$lon), ]
  m  <- match(DC$file_row, ov$file_row)
  use <- !is.na(m) & is.na(DC$located)
  DC$lat[use] <- ov$lat[m[use]]; DC$lon[use] <- ov$lon[m[use]]; DC$located[use] <- ov$method[m[use]]
  message(sprintf("   %d placed from %s", sum(use), OVERRIDES))
}
unmapped <- DC[is.na(DC$located), c("file_row", "name", "operator", "address", "city", "county", "mw", "status_raw")]
if (nrow(unmapped)) {
  write.csv(unmapped, "datacenters_unmapped.csv", row.names = FALSE, na = "")
  note(nrow(unmapped), " Texas data center(s) have no coordinates and are not in ", OVERRIDES,
       "; listed in datacenters_unmapped.csv and left off the map.")
}
DC$approximate <- !is.na(DC$located) & DC$located != "coordinates in the file"
message(sprintf("   %d Texas data centers; %d mapped (%d approximately)", nrow(DC), sum(!is.na(DC$located)), sum(DC$approximate)))

# ---- where each data center sits ---------------------------------------------
ok  <- !is.na(DC$located)
pts <- st_as_sf(DC[ok, ], coords = c("lon", "lat"), crs = 4326, remove = FALSE)
put <- function(v) { out <- rep(NA, nrow(DC)); out[ok] <- v; out }

FIELD <- c("Texas House districts" = "house_district", "Groundwater conservation districts" = "gcd",
           "Groundwater management areas" = "gma", "Major aquifers" = "major_aquifer", "River basins" = "river_basin",
           "Regional flood planning regions" = "flood_region", "Regional water planning areas" = "water_region")
for (n in names(boundaries)) DC[[FIELD[[n]]]] <- put(label_at(pts, boundaries[[n]]))

tracts <- DX$units[, intersect(c("GEOID", "svi", "svi_quintile"), names(DX$units))]
hit <- suppressMessages(st_intersects(pts, tracts))
ti  <- vapply(hit, function(i) if (length(i)) i[1] else NA_integer_, integer(1))
DC$tract        <- put(tracts$GEOID[ti])
DC$svi          <- put(tracts$svi[ti])
DC$svi_quintile <- put(tracts$svi_quintile[ti])

usdm <- DX$usdm
for (k in names(usdm)) {                                  # -1 none, 0..4 = D0..D4
  u <- make_valid(usdm[[k]])
  h <- suppressMessages(st_intersects(pts, u))
  DC[[paste0("usdm_", k)]] <- put(vapply(h, function(i) if (length(i)) max(u$DM[i]) else -1L, numeric(1)))
}

# The utility whose service area the data center sits in (the smallest, where service areas overlap)
f_lay <- file.path(SHINE_DATA, "twnet_layers.rds")
DC$utility_id <- NA_character_
if (file.exists(f_lay)) {
  sa <- readRDS(f_lay)$layers[["Public water system service areas"]]
  sa <- make_valid(sa[!is.na(sa$node), ])
  sa$node_id <- N$node_id[sa$node]
  DC$utility_id <- put(label_at(pts, sa, "node_id"))
} else note("twnet_layers.rds not found; data centers are not tied to a service area.")
m <- match(DC$utility_id, N$node_id)
DC$utility_name  <- N$name[m]
DC$utility_class <- as.character(N$org_class[m])
message(sprintf("   %d of %d mapped data centers sit inside a mapped service area", sum(!is.na(DC$utility_id)), sum(ok)))

# =============================================================================
# 5. WRITE
# =============================================================================
usdm_dates <- vapply(usdm, function(u) as.character(max(as.Date(u$MapDate))), character(1))
# The Drought Monitor files cover the box around Texas; trim them to the state for display
if ("Regional water planning areas" %in% names(boundaries)) {
  tx <- make_valid(st_union(st_buffer(boundaries[["Regional water planning areas"]], 0.02)))
  usdm <- lapply(usdm, function(u) {
    u <- suppressWarnings(suppressMessages(st_intersection(make_valid(u), tx)))
    u <- suppressWarnings(st_collection_extract(u, "POLYGON"))
    u <- stats::aggregate(u["DM"], by = list(DM_ = u$DM, class = u$class, MapDate = u$MapDate), FUN = function(x) x[1])
    st_cast(u[, c("DM", "class", "MapDate")], "MULTIPOLYGON")
  })
}
out <- list(
  boundaries  = boundaries,
  utilities   = U,
  links       = L,
  sources     = S,
  datacenters = DC,
  tracts      = tracts,
  usdm        = usdm,
  meta = list(
    built = Sys.time(), ref_year = REF_YEAR, usdm_dates = usdm_dates, members_as_of = members_as_of,
    utility_classes = UTILITY_CLASSES, boundary_fields = FIELD[names(boundaries)],
    shine_built = TM$built, drought_built = DX$meta$built, atlas_built = atlas$built,
    datacenter_file = DC_FILE, datacenter_file_date = as.Date(file.info(DC_FILE)$mtime),
    n = c(datacenters = nrow(DC), mapped = sum(ok), approximate = sum(DC$approximate),
          utilities = nrow(U), links = nrow(L), links_drawn = sum(L$drawn)),
    notes = notes))
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
saveRDS(out, OUT, compress = "xz")
message(sprintf("Wrote %s (%.1f MB)", OUT, file.info(OUT)$size / 1e6))
if (length(notes)) message(length(notes), " note(s) above; they are stored in meta$notes.") else message("Nothing left out.")
