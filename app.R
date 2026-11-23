# =============================================================================
# Texas Drought and Data Centers (Shiny)
# A companion to Texas Water SHiNE and the Texas Drought Explorer
#
# R. Patrick Bixler, PhD | LBJ School of Public Affairs | The University of Texas at Austin
#
# Pages
#   Map | Data centers (table) | About the data
#
# What the map carries
#   (1) Boundaries        Texas House districts, groundwater conservation districts,
#                         groundwater management areas, major aquifers, river basins,
#                         regional flood planning regions, regional water planning areas
#   (2) Water utilities   the eight kinds of utility in SHiNE, one point each
#   (3) Source links      each utility's links to its sources and sellers (SHiNE)
#   (4) Data centers      the Texas rows of the data center file
#   (5) Drought frames    U.S. Drought Monitor (this week and two stored droughts)
#                         and the Texas Social Vulnerability Index by census tract
#
# Files (paths are relative to this file):
#   data/drought_datacenters.rds   built by prepare_datacenter_data.R. The app reads
#                                  nothing else.
#
# To refresh the data:   source("prepare_datacenter_data.R")
#
# Where to edit
#   * utility kinds, short names, colors .... UTIL
#   * boundary line colors .................. BOUNDARY_COLORS
#   * data center status colors ............. STATUS_COLORS
#   * source link colors .................... WATER_COLORS
#   * page text ............................. page_about()
# =============================================================================

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(DT)
suppressMessages(sf_use_s2(FALSE))     # shapes are simplified for display; treat them as flat

# =============================================================================
# DATA (read once at app start, shared by every visitor session)
# =============================================================================

DATA_FILE <- "data/drought_datacenters.rds"
if (!file.exists(DATA_FILE)) stop("data/drought_datacenters.rds not found. Run prepare_datacenter_data.R.")
DD   <- readRDS(DATA_FILE)
META <- DD$meta

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
NDASH <- intToUtf8(8211); EMDASH <- intToUtf8(8212); MDOT <- intToUtf8(183)

# Coordinates to four decimals (about 11 m): halves what travels to the browser
round_geom <- function(x) {
  g <- st_geometry(x)
  st_geometry(x) <- st_sfc(lapply(g, function(s) structure(rapply(unclass(s), function(m) round(m, 4), how = "replace"),
                                                           class = class(s))), crs = st_crs(g))
  x
}

BND    <- lapply(DD$boundaries, round_geom)
UTIL_D <- DD$utilities
LINKS  <- DD$links
SRC    <- DD$sources
DC     <- DD$datacenters
TRACTS <- round_geom(DD$tracts)
USDM   <- DD$usdm
REF_YEAR <- META$ref_year
TX <- list(west = -106.7, south = 25.8, east = -93.4, north = 36.6)

DC_MAP <- DC[!is.na(DC$lat) & !is.na(DC$lon), ]
DC_MAP <- DC_MAP[order(-ifelse(is.na(DC_MAP$mw), 0, DC_MAP$mw)), ]      # large first, so small ones stay clickable on top
UT_MAP <- UTIL_D[!is.na(UTIL_D$lat) & !is.na(UTIL_D$lon), ]
UT_MAP <- UT_MAP[order(-ifelse(is.na(UT_MAP$pop), 0, UT_MAP$pop)), ]

# Source links as lines, built once
LK <- LINKS[LINKS$drawn, ]
LK_GEOM <- st_sfc(lapply(seq_len(nrow(LK)), function(i)
  st_linestring(matrix(c(LK$lon1[i], LK$lon2[i], LK$lat1[i], LK$lat2[i]), ncol = 2))), crs = 4326)

# =============================================================================
# COLORS AND SETTINGS
# =============================================================================

UT <- list(orange = "#bf5700", orange_dk = "#9d4700", charcoal = "#333f48", limestone = "#d6d2c4",
           lime_lt = "#f3f1ea", shade = "#9cadb7", bluebonnet = "#005f86")

# The eight kinds of utility, as SHiNE names them (class), with a short name and a color
UTIL <- data.frame(
  class = c("Private / investor-owned or contract-operated utility",
            "City / municipal utility",
            "Water supply corporation (WSC)",
            "Utility district (MUD / UD / PUD)",
            "Small community system (owner not evident from name)",
            "Other water district (WCID / FWSD / irrigation / improvement / management)",
            "Special utility district (SUD)",
            "River authority / regional water authority / wholesale district"),
  short = c("Private / investor-owned", "City / municipal", "Water supply corporation", "Utility district (MUD / UD / PUD)",
            "Small community system", "Other water district", "Special utility district", "River authority / wholesale"),
  color = c("#7b4fa3", "#005f86", "#579d42", "#c2417f", "#9cadb7", "#00a9b7", "#a6cd57", "#8a4b2a"),
  stringsAsFactors = FALSE)
UTIL$n <- as.integer(table(factor(as.character(UTIL_D$org_class), levels = UTIL$class)))
util_color <- function(cls) UTIL$color[match(as.character(cls), UTIL$class)]

BOUNDARY_COLORS <- c("Texas House districts" = "#333f48", "Groundwater conservation districts" = "#7a3600",
                     "Groundwater management areas" = "#bf5700", "Major aquifers" = "#00a9b7", "River basins" = "#005f86",
                     "Regional flood planning regions" = "#6250d6", "Regional water planning areas" = "#579d42")
BOUNDARY_COLORS <- BOUNDARY_COLORS[names(BND)]

STATUS_COLORS <- c("Operational" = "#1b1b1b", "Under construction" = "#6250d6",
                   "Planned or approved" = "#ffffff", "Status not given" = "#8d8d8d")
WATER_COLORS  <- c("Ground Water" = "#9a6b2f", "Surface Water" = "#1f7a9c", "Reuse" = "#579d42", "Water Type Not Known" = "#7d8a92")
SOURCE_COLORS <- c("Groundwater (Aquifer)" = "#9a6b2f", "Surface Water" = "#1f7a9c")      # sellers and systems: charcoal

RAMP_SV <- c("#fbeee0", "#f3cf9f", "#e59a4e", "#bf5700", "#7a3600")   # social vulnerability: fifths of the tract scores
SV_NAMES <- c("Least vulnerable fifth", "Second fifth", "Middle fifth", "Fourth fifth", "Most vulnerable fifth")
NA_COLOR <- "#e3e3e0"
# The U.S. Drought Monitor's own colors, so its map reads the way people know it
USDM_COLORS <- c(None = "#f4f4f0", D0 = "#ffff00", D1 = "#fcd37f", D2 = "#ffaa00", D3 = "#e60000", D4 = "#730000")
USDM_NAMES  <- c(None = "No drought", D0 = "D0 abnormally dry", D1 = "D1 moderate", D2 = "D2 severe", D3 = "D3 extreme", D4 = "D4 exceptional")
usdm_name <- function(v) ifelse(is.na(v), EMDASH, unname(USDM_NAMES)[v + 2])       # -1 none, 0..4 = D0..D4

usdm_label <- function(tag) {
  d <- trimws(format(as.Date(META$usdm_dates[[tag]]), "%b %e, %Y"))
  if (identical(tag, "now")) paste0("This week (", d, ")") else d
}
USDM_KEYS    <- names(USDM)[order(names(USDM) != "now", -as.numeric(suppressWarnings(as.integer(names(USDM)))))]
USDM_CHOICES <- c(stats::setNames(USDM_KEYS, vapply(USDM_KEYS, usdm_label, character(1))), "Off" = "none")

fmt_int <- function(x) ifelse(is.na(x), EMDASH, formatC(round(x), format = "d", big.mark = ","))
fmt_num <- function(x, d = 0) ifelse(is.na(x), EMDASH, formatC(x, format = "f", digits = d, big.mark = ","))
fmt_pct <- function(x) ifelse(is.na(x), EMDASH, ifelse(x > 0 & x < 0.005, "<1%", sprintf("%.0f%%", 100 * x)))
fmt_mw  <- function(x) ifelse(is.na(x), "not given", paste(formatC(x, format = "fg", digits = 4, big.mark = ","), "MW"))
esc     <- function(x) htmltools::htmlEscape(as.character(x))
blank   <- function(x) is.na(x) | trimws(as.character(x)) == ""
or_dash <- function(x) ifelse(blank(x), EMDASH, as.character(x))
ascii_punct <- function(x) {
  x <- gsub(paste0("[", intToUtf8(c(8216, 8217)), "]"), "'", as.character(x))
  x <- gsub(paste0("[", intToUtf8(c(8220, 8221)), "]"), "\"", x)
  x <- gsub(paste0("[", intToUtf8(c(8211, 8212, 8722)), "]"), "-", x)
  gsub(intToUtf8(8230), "...", x, fixed = TRUE)
}

# Marker sizes
dc_radius   <- function(mw, scaled) if (!scaled) rep(6, length(mw)) else ifelse(is.na(mw), 5, pmin(22, 5 + sqrt(mw) / 2.8))
util_radius <- function(pop) ifelse(is.na(pop), 3, pmin(9, 3 + sqrt(pop) / 180))
link_weight <- function(acft) pmin(6, 0.8 + sqrt(pmax(acft, 0)) / 60)

# =============================================================================
# POPUPS
# =============================================================================

row2 <- function(k, v) sprintf("<tr><th>%s</th><td>%s</td></tr>", k, v)
ptable <- function(...) paste0("<table class='dd-pt'>", paste0(c(...), collapse = ""), "</table>")
phead  <- function(kicker, title) sprintf("<div class='dd-pk'>%s</div><div class='dd-ph'>%s</div>", esc(kicker), esc(title))
psub   <- function(x) sprintf("<div class='dd-ps'>%s</div>", x)

# A utility's sources in the reference year, largest first
sources_html <- function(node_id, max_rows = 6) {
  l <- LINKS[LINKS$utility_id == node_id, ]
  if (!nrow(l)) return(sprintf("<div class='dd-note'>SHiNE records no supply for this utility in %d.</div>", REF_YEAR))
  more <- nrow(l) - max_rows
  l <- utils::head(l, max_rows)
  rows <- sprintf("<tr><td>%s<div class='dd-sm'>%s%s</div></td><td class='dd-r'>%s</td><td class='dd-r'>%s</td></tr>",
                  esc(l$source_name),
                  ifelse(l$kind == "Own intake", esc(l$water), paste0("purchased ", MDOT, " ", esc(tolower(l$water)))),
                  ifelse(l$drawn, "", paste0(" ", MDOT, " not drawn")),
                  fmt_int(l$acft), fmt_pct(l$share))
  paste0("<table class='dd-st'><tr><th>Source or seller</th><th class='dd-r'>Acre-feet</th><th class='dd-r'>Share</th></tr>",
         paste0(rows, collapse = ""), "</table>",
         if (more > 0) sprintf("<div class='dd-note'>and %d smaller source%s</div>", more, ifelse(more == 1, "", "s")) else "")
}

popup_utility <- function(u) {
  paste0(
    phead("Water utility", u$name), psub(esc(u$org_class)),
    ptable(row2("County", esc(or_dash(u$county))),
           row2("Planning region", esc(or_dash(u$region))),
           row2("People served", fmt_int(u$pop)),
           row2("TCEQ system ID", esc(or_dash(u$tceq_pws_id))),
           row2("Drought this week", esc(usdm_name(u$usdm_now))),
           row2("Social vulnerability", if (is.na(u$svi_quintile)) EMDASH else esc(SV_NAMES[u$svi_quintile])),
           if (!blank(u$tceq_stage)) row2("Drought stage (TCEQ)", esc(u$tceq_stage))),
    sprintf("<div class='dd-sec'>Where its water came from, %d</div>", REF_YEAR), sources_html(u$node_id),
    if (!blank(u$website)) sprintf("<div class='dd-note'><a href='%s' target='_blank' rel='noopener'>Utility website</a></div>", esc(u$website)) else "")
}

popup_datacenter <- function(d) {
  where <- vapply(names(BND), function(n) {
    f <- META$boundary_fields[[n]]
    row2(esc(sub("s$", "", sub("^Texas ", "", n))), esc(or_dash(d[[f]])))
  }, character(1))
  drought <- vapply(USDM_KEYS, function(k) row2(esc(usdm_label(k)), esc(usdm_name(d[[paste0("usdm_", k)]]))), character(1))
  util <- if (blank(d$utility_id)) "<div class='dd-note'>Not inside a mapped water service area.</div>" else paste0(
    sprintf("<div class='dd-b'>%s</div><div class='dd-sm'>%s</div>", esc(d$utility_name), esc(or_dash(d$utility_class))),
    sources_html(d$utility_id, 4),
    "<div class='dd-note'>Sitting in a service area does not show the utility serves the site; some data centers supply themselves.</div>")
  paste0(
    phead("Data center", d$name),
    psub(paste(c(if (!blank(d$operator) && !identical(d$operator, d$name)) esc(d$operator),
                 esc(as.character(d$status))), collapse = paste0(" ", MDOT, " "))),
    ptable(row2("Capacity", esc(fmt_mw(d$mw))),
           if (!is.na(d$year_built)) row2("Year", d$year_built),
           if (!blank(d$size_note)) row2("Size as listed", esc(d$size_note)),
           row2("Address", esc(or_dash(ifelse(blank(d$address), d$city, d$address)))),
           row2("Location", esc(d$located)),
           row2("Listed by", esc(or_dash(d$listed_by)))),
    "<div class='dd-sec'>Drought Monitor at the site</div>", ptable(drought),
    "<div class='dd-sec'>Social vulnerability of its census tract</div>",
    sprintf("<div>%s</div>", if (is.na(d$svi_quintile)) EMDASH else esc(SV_NAMES[d$svi_quintile])),
    "<div class='dd-sec'>Water service area it sits in</div>", util,
    "<div class='dd-sec'>Where it sits</div>", ptable(where),
    if (!blank(d$reference)) sprintf("<div class='dd-note'><a href='%s' target='_blank' rel='noopener'>Reference</a></div>", esc(d$reference)) else "")
}

popup_source <- function(s) {
  paste0(phead(if (s$type %in% names(SOURCE_COLORS)) "Water source" else "Seller", s$name), psub(esc(s$type)),
         ptable(row2("Utilities drawing on it", fmt_int(s$n_utilities)),
                row2(sprintf("Acre-feet to them, %d", REF_YEAR), fmt_int(s$acft)),
                row2("Point location", esc(or_dash(s$geo_quality)))))
}

# Everything at one spot on the map
popup_here <- function(lng, lat) {
  pt <- st_sfc(st_point(c(lng, lat)), crs = 4326)
  at <- function(poly, field) {
    i <- suppressMessages(st_intersects(pt, poly))[[1]]
    if (length(i)) paste(unique(as.character(poly[[field]][i])), collapse = "; ") else NA_character_
  }
  where <- vapply(names(BND), function(n) {
    v <- at(BND[[n]], "label")
    if (n == "Texas House districts" && !is.na(v)) {
      b <- BND[[n]][match(v, BND[[n]]$label), ]
      if (!blank(b$member)) v <- paste0(v, " (", b$member, if (!blank(b$party)) paste0(", ", substr(b$party, 1, 1)) else "", ")")
    }
    row2(esc(sub("s$", "", sub("^Texas ", "", n))), esc(or_dash(v)))
  }, character(1))
  q <- suppressWarnings(as.integer(at(TRACTS, "svi_quintile")))
  drought <- vapply(USDM_KEYS, function(k) {
    i <- suppressMessages(st_intersects(pt, USDM[[k]]))[[1]]
    row2(esc(usdm_label(k)), esc(usdm_name(if (length(i)) max(USDM[[k]]$DM[i]) else -1L)))
  }, character(1))
  paste0(phead("At this spot", sprintf("%.4f, %.4f", lat, lng)),
         ptable(where),
         "<div class='dd-sec'>Drought Monitor</div>", ptable(drought),
         "<div class='dd-sec'>Social vulnerability of the census tract</div>",
         sprintf("<div>%s</div>", if (is.na(q)) EMDASH else esc(SV_NAMES[q])))
}

# =============================================================================
# PAGES
# =============================================================================

swatch <- function(color, shape = "dot", border = NULL) tags$span(class = paste0("dd-sw dd-", shape),
  style = if (shape == "line") paste0("border-top-color:", color, ";")
          else paste0("background:", color, ";", if (!is.null(border)) paste0("border-color:", border, ";")))

util_choice_names <- lapply(seq_len(nrow(UTIL)), function(i)
  tags$span(title = UTIL$class[i], swatch(UTIL$color[i]), UTIL$short[i], tags$span(class = "dd-n", fmt_int(UTIL$n[i]))))
status_choice_names <- lapply(names(STATUS_COLORS), function(s)
  tags$span(swatch(STATUS_COLORS[[s]], border = if (s == "Planned or approved") "#1b1b1b"), s,
            tags$span(class = "dd-n", fmt_int(sum(DC$status == s)))))
boundary_choice_names <- lapply(names(BND), function(n)
  tags$span(swatch(BOUNDARY_COLORS[[n]], "line"), n, tags$span(class = "dd-n", fmt_int(nrow(BND[[n]])))))

sidebar_controls <- function() {
  accordion(
    open = c("Data centers", "Water utilities", "Drought Monitor"), multiple = TRUE,
    accordion_panel(
      "Data centers",
      checkboxInput("show_dc", "Show data centers", TRUE),
      checkboxGroupInput("dc_status", NULL, choiceNames = status_choice_names, choiceValues = names(STATUS_COLORS),
                         selected = names(STATUS_COLORS)),
      checkboxInput("dc_size", "Size the points by capacity (MW)", TRUE),
      tags$div(class = "dd-sm", "The smallest points give no capacity.")),
    accordion_panel(
      "Water utilities",
      checkboxInput("show_util", "Show utilities", TRUE),
      checkboxGroupInput("util_class", NULL, choiceNames = util_choice_names, choiceValues = UTIL$class, selected = UTIL$class),
      radioButtons("link_mode", sprintf("Links to each utility's sources (%d)", REF_YEAR),
                   c("For the utility or data center I click" = "click", "For every utility shown" = "all", "Off" = "none"),
                   selected = "click"),
      tags$div(class = "dd-sm", "Larger utility points serve more people.")),
    accordion_panel(
      "Drought Monitor",
      radioButtons("usdm", NULL, USDM_CHOICES, selected = USDM_KEYS[1])),
    accordion_panel(
      "Social vulnerability",
      checkboxInput("show_svi", "Shade census tracts by the Texas Social Vulnerability Index", FALSE)),
    accordion_panel(
      "Boundaries",
      checkboxGroupInput("boundaries", NULL, choiceNames = boundary_choice_names, choiceValues = names(BND), selected = character(0))),
    tags$p(class = "dd-hint", "Click a data center, a utility or a source for its details. Click anywhere else for the districts, ",
           "drought class and social vulnerability at that spot.")
  )
}

page_about <- function() {
  n <- META$n
  tags$div(
    class = "dd-about",
    tags$h2("About the data"),
    tags$p("This map sets Texas data centers beside the water utilities around them, the sources those utilities draw on, ",
           "drought, and social vulnerability. It is a screening view: it shows where to look, not what a site uses."),
    tags$h3("What is on the map"),
    tags$ul(
      tags$li(tags$b("Data centers. "), sprintf("%s Texas sites from the data center file (%s). ", fmt_int(n[["datacenters"]]), META$datacenter_file),
              sprintf("%s report a capacity in megawatts. %s had no coordinates in the file and are placed at the centre of their town, marked approximate; ",
                      fmt_int(sum(!is.na(DC$mw))), fmt_int(n[["approximate"]])),
              "replace those in geocode_overrides.csv as exact locations are confirmed. The file combines several public lists, ",
              "so one building can appear more than once under different tenants."),
      tags$li(tags$b("Water utilities. "), sprintf("%s utilities of eight kinds from Texas Water SHiNE, built on the TWDB Water Use Survey. ", fmt_int(n[["utilities"]])),
              sprintf("%s have no location and are not mapped. ", fmt_int(n[["utilities"]] - nrow(UT_MAP))),
              "The kind of utility is SHiNE's own classification."),
      tags$li(tags$b("Source links. "), sprintf("Each line joins a utility to a source it took water from, or a seller it bought from, in %d. ", REF_YEAR),
              "Solid lines are the utility's own intake; dashed lines are purchases. Aquifer and reservoir points are placed approximately, ",
              "at the centre of the counties the source lies in, so a line shows the tie, not a pipeline route. ",
              "Self-supplied reuse is listed in each utility's table but not drawn."),
      tags$li(tags$b("Drought Monitor. "), paste0("The U.S. Drought Monitor map for ", paste(vapply(USDM_KEYS, usdm_label, character(1)), collapse = "; "),
              ". The two past weeks are the ones stored with the Texas Drought Explorer, from the 2011 and 2022 droughts. ",
              "The class shown for a data center is the one at its point on the map.")),
      tags$li(tags$b("Social vulnerability. "), paste0("The Texas Social Vulnerability Index by census tract (Bixler and Tenison 2022; 2016", NDASH,
              "2020 American Community Survey, 2020 boundaries), in fifths of the tract scores.")),
      tags$li(tags$b("Boundaries. "), paste0("From the Texas Water Atlas layers: ", paste(names(BND), collapse = ", "), "."),
              if (!is.na(META$members_as_of %||% NA)) sprintf("House members are as listed on %s.", META$members_as_of))),
    tags$h3("Reading it with care"),
    tags$ul(
      tags$li("A data center inside a utility's service area is not shown to be that utility's customer. Some sites supply themselves or buy from another provider."),
      tags$li("The Drought Monitor describes weather-driven dryness where the site sits. A utility's exposure depends on where its sources are, which is why the links are drawn."),
      tags$li("Listed capacity is power, not water use. Water use depends on the cooling design and is not in the file.")),
    tags$p(class = "dd-note", sprintf("Data prepared %s. SHiNE build %s; drought layers built %s. Not an official product of any agency.",
                                      format(META$built, "%B %e, %Y"), format(as.Date(META$shine_built), "%b %e, %Y"),
                                      format(as.Date(META$drought_built), "%b %e, %Y")))
  )
}

dd_css <- "
:root { --ut-orange:#bf5700; --ut-charcoal:#333f48; --ut-limestone:#d6d2c4; }
.navbar { border-bottom:3px solid var(--ut-orange); }
.navbar-brand { font-weight:700; letter-spacing:.01em; }
.dd-tiles { display:grid; grid-template-columns:repeat(4, minmax(0, 1fr)); gap:10px; margin-bottom:10px; }
.dd-tile { background:#fff; border:1px solid #e2e0d8; border-left:4px solid var(--ut-orange); border-radius:6px; padding:8px 12px; }
.dd-tile .v { font-size:1.45rem; font-weight:700; color:var(--ut-charcoal); line-height:1.15; }
.dd-tile .l { font-size:.8rem; color:#5d6970; }
@media (max-width: 900px) { .dd-tiles { grid-template-columns:repeat(2, minmax(0, 1fr)); } }
.dd-mapwrap { position:relative; flex:1 1 auto; min-height:480px; }
.dd-mapwrap #map { position:absolute !important; top:0; right:0; bottom:0; left:0; height:auto !important; }
.dd-mapwrap .leaflet-container { border:1px solid #e2e0d8; border-radius:6px; }
.dd-legend { position:absolute; left:12px; bottom:22px; z-index:500; background:rgba(255,255,255,.95); border:1px solid #d6d2c4;
             border-radius:6px; padding:8px 10px; font-size:.76rem; max-width:250px; max-height:62%; overflow:auto; line-height:1.35; }
.dd-legend h6 { font-size:.7rem; text-transform:uppercase; letter-spacing:.06em; color:#5d6970; margin:8px 0 3px; }
.dd-legend h6:first-child { margin-top:0; }
.dd-legend .it { display:flex; align-items:center; gap:6px; }
.dd-sw { display:inline-block; flex:0 0 auto; margin-right:6px; vertical-align:middle; border:1.5px solid #fff; box-shadow:0 0 0 1px rgba(0,0,0,.25); }
.dd-dot { width:11px; height:11px; border-radius:50%; }
.dd-box { width:14px; height:11px; border-radius:2px; }
.dd-line { width:18px; height:0; border:0; border-top:3px solid; box-shadow:none; background:transparent !important; }
.dd-n { color:#7d8a92; font-size:.78em; margin-left:6px; }
.dd-hint { font-size:.8rem; color:#5d6970; margin:12px 14px 0; }
.shiny-input-container .checkbox, .shiny-input-container .radio { margin-bottom:2px; font-size:.86rem; }
.accordion-button { font-weight:600; padding:.6rem .9rem; }
.accordion-body { padding:.5rem .9rem .7rem; }
.leaflet-popup-content { margin:12px 14px; font-size:.82rem; line-height:1.35; }
.dd-pk { font-size:.68rem; text-transform:uppercase; letter-spacing:.07em; color:var(--ut-orange); font-weight:700; }
.dd-ph { font-size:1.02rem; font-weight:700; color:var(--ut-charcoal); }
.dd-ps { color:#5d6970; margin-bottom:6px; }
.dd-sec { font-size:.68rem; text-transform:uppercase; letter-spacing:.06em; color:#5d6970; font-weight:700;
          border-top:1px solid #e2e0d8; margin-top:8px; padding-top:6px; }
.dd-pt, .dd-st { width:100%; border-collapse:collapse; }
.dd-pt th { font-weight:400; color:#5d6970; text-align:left; padding:1px 10px 1px 0; vertical-align:top; white-space:nowrap; }
.dd-pt td { padding:1px 0; }
.dd-st th { font-weight:600; color:#5d6970; font-size:.72rem; border-bottom:1px solid #e2e0d8; padding:2px 0; }
.dd-st td { padding:3px 0; border-bottom:1px solid #f0eee8; vertical-align:top; }
.dd-r { text-align:right; padding-left:10px !important; white-space:nowrap; }
.dd-sm, .dd-note { font-size:.74rem; color:#7d8a92; }
.dd-note { margin-top:5px; }
.dd-b { font-weight:600; }
.dd-tip { font-size:.78rem; }
.dd-about { max-width:860px; margin:0 auto; padding:10px 6px 40px; }
.dd-about h2 { color:var(--ut-charcoal); font-weight:700; }
.dd-about h3 { font-size:1.05rem; margin-top:1.4rem; color:var(--ut-orange); font-weight:700; }
.dd-about li { margin-bottom:.5rem; }
"

dd_theme <- bs_theme(version = 5, primary = UT$orange, secondary = UT$charcoal, "navbar-bg" = "#ffffff",
                     "body-bg" = "#faf9f6", "font-size-base" = "0.95rem")

ui <- page_navbar(
  title = "Texas Drought and Data Centers", id = "page", theme = dd_theme, fillable = "Map",
  header = tags$head(tags$style(HTML(dd_css))),
  nav_panel(
    "Map",
    layout_sidebar(
      sidebar = sidebar(width = 340, sidebar_controls()),
      uiOutput("tiles"),
      tags$div(class = "dd-mapwrap html-fill-item", leafletOutput("map", height = "100%"), uiOutput("legend")))),
  nav_panel(
    "Data centers",
    tags$div(style = "padding:6px 4px 30px;",
             tags$div(style = "display:flex; justify-content:space-between; align-items:end; gap:16px; flex-wrap:wrap; margin-bottom:8px;",
                      tags$p(style = "margin:0; max-width:760px;",
                             "Every Texas data center in the file, with the drought class at its site, the social vulnerability of its census tract, ",
                             "the water service area it sits in and that utility's largest source. Click a row to find it on the map."),
                      downloadButton("dl", "Download this table (CSV)", class = "btn-sm btn-outline-secondary")),
             DTOutput("table"))),
  nav_panel("About the data", page_about()),
  nav_spacer(),
  nav_item(tags$span(style = "font-size:.8rem; color:#5d6970;", "LBJ School of Public Affairs ", MDOT, " UT Austin"))
)

# =============================================================================
# THE TABLE (built once)
# =============================================================================

top_source <- function(id) {                          # a utility's largest source, with its share
  l <- LINKS[match(id, LINKS$utility_id), ]           # LINKS is sorted largest first within each utility
  ifelse(is.na(l$utility_id), NA_character_, paste0(l$source_name, " (", fmt_pct(l$share), ")"))
}
TABLE <- data.frame(
  "Data center"        = DC$name,
  "Operator"           = DC$operator,
  "Status"             = as.character(DC$status),
  "Capacity (MW)"      = DC$mw,
  "City"               = DC$city,
  "Service area it sits in" = DC$utility_name,
  "Kind of utility"    = UTIL$short[match(DC$utility_class, UTIL$class)],
  "Utility's largest source" = top_source(DC$utility_id),
  check.names = FALSE, stringsAsFactors = FALSE)
for (k in USDM_KEYS) TABLE[[paste("Drought,", sub("^This week \\((.*)\\)$", "\\1", usdm_label(k)))]] <- usdm_name(DC[[paste0("usdm_", k)]])
TABLE[["Social vulnerability (tract)"]] <- ifelse(is.na(DC$svi_quintile), NA, paste0(DC$svi_quintile, ": ", SV_NAMES[DC$svi_quintile]))
for (n in names(BND)) TABLE[[sub("s$", "", sub("^Texas ", "", n))]] <- DC[[META$boundary_fields[[n]]]]
TABLE[["Location"]] <- DC$located

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  rv <- reactiveValues(util = NULL, dc = NULL)         # the utility whose links are drawn; the data center picked

  # ---- headline tiles ---------------------------------------------------------
  output$tiles <- renderUI({
    k <- if (is.null(input$usdm) || identical(input$usdm, "none")) USDM_KEYS[1] else input$usdm
    v <- DC[[paste0("usdm_", k)]]
    tile <- function(value, label) tags$div(class = "dd-tile", tags$div(class = "v", value), tags$div(class = "l", label))
    tags$div(class = "dd-tiles",
             tile(fmt_int(nrow(DC)), sprintf("Texas data centers in the file (%s placed approximately)", fmt_int(sum(DC$approximate)))),
             tile(paste(fmt_int(sum(DC$mw, na.rm = TRUE)), "MW"), sprintf("listed capacity, from the %s sites that report it", fmt_int(sum(!is.na(DC$mw))))),
             tile(fmt_int(sum(v >= 2, na.rm = TRUE)), paste0("in severe drought or worse (D2+), ", sub("^This week \\((.*)\\)$", "\\1", usdm_label(k)))),
             tile(fmt_int(sum(DC$svi_quintile %in% 5)), "in the most socially vulnerable fifth of census tracts"))
  })

  # ---- the base map -----------------------------------------------------------
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta = 0.5, minZoom = 4, maxZoom = 16, preferCanvas = FALSE)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas, group = "Light") |>
      addProviderTiles(providers$Esri.WorldStreetMap,  group = "Streets") |>
      addProviderTiles(providers$Esri.WorldImagery,    group = "Satellite") |>
      addMapPane("tracts",     zIndex = 405) |>
      addMapPane("usdm",       zIndex = 410) |>
      addMapPane("boundaries", zIndex = 415) |>
      addMapPane("links",      zIndex = 420) |>
      addMapPane("utilities",  zIndex = 425) |>
      addMapPane("sources",    zIndex = 430) |>
      addMapPane("dc",         zIndex = 435) |>
      addMapPane("selected",   zIndex = 440) |>
      addLayersControl(baseGroups = c("Light", "Streets", "Satellite"), position = "topright",
                       options = layersControlOptions(collapsed = TRUE)) |>
      addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = TRUE, metric = FALSE)) |>
      fitBounds(TX$west, TX$south, TX$east, TX$north) |>
      # fit Texas again once the page has settled into its final size
      htmlwidgets::onRender(sprintf(
        "function(el, x) { var m = this; setTimeout(function() { m.invalidateSize(); m.fitBounds([[%s, %s], [%s, %s]]); }, 400); }",
        TX$south, TX$west, TX$north, TX$east))
  })
  outputOptions(output, "map", suspendWhenHidden = FALSE)

  # ---- (5) social vulnerability and the Drought Monitor --------------------------
  observe({
    proxy <- leafletProxy("map") |> clearGroup("tracts")
    if (isTRUE(input$show_svi)) {
      fill <- RAMP_SV[TRACTS$svi_quintile]; fill[is.na(fill)] <- NA_COLOR
      proxy |> addPolygons(data = TRACTS, group = "tracts", stroke = FALSE, fillColor = fill, fillOpacity = 0.7,
                           options = pathOptions(pane = "tracts", interactive = FALSE))
    }
  })

  observe({
    k <- input$usdm %||% "none"
    proxy <- leafletProxy("map") |> clearGroup("usdm")
    if (!identical(k, "none") && !is.null(USDM[[k]])) {
      p <- USDM[[k]]
      proxy |> addPolygons(data = p, group = "usdm", stroke = FALSE, fillOpacity = if (isTRUE(input$show_svi)) 0.4 else 0.55,
                           fillColor = unname(USDM_COLORS[as.character(p$class)]),
                           options = pathOptions(pane = "usdm", interactive = FALSE))
    }
  })

  # ---- (1) boundaries: each layer is sent the first time it is switched on, then shown or hidden ----
  sent <- character(0)
  observe({
    on <- input$boundaries %||% character(0)
    proxy <- leafletProxy("map")
    for (n in names(BND)) {
      g <- paste0("b:", n)
      if (n %in% on && !(n %in% sent)) {
        b <- BND[[n]]
        lab <- b$label
        if (n == "Texas House districts") lab <- ifelse(blank(b$member), lab, paste0(lab, " ", MDOT, " ", b$member))
        proxy <- proxy |> addPolygons(
          data = b, group = g, color = BOUNDARY_COLORS[[n]], weight = 1.6, opacity = 0.9,
          fillColor = BOUNDARY_COLORS[[n]], fillOpacity = if (n == "Major aquifers") 0.12 else 0.02,
          label = lab, labelOptions = labelOptions(className = "dd-tip", sticky = TRUE),
          highlightOptions = highlightOptions(weight = 3, opacity = 1, bringToFront = FALSE),
          options = pathOptions(pane = "boundaries"))
        sent <<- c(sent, n)
      }
      proxy <- if (n %in% on) showGroup(proxy, g) else hideGroup(proxy, g)
    }
  })

  # ---- (2) utilities ------------------------------------------------------------
  utils_shown <- reactive({
    if (!isTRUE(input$show_util)) return(UT_MAP[0, ])
    UT_MAP[as.character(UT_MAP$org_class) %in% (input$util_class %||% character(0)), ]
  })

  observe({
    u <- utils_shown()
    proxy <- leafletProxy("map") |> clearGroup("utilities")
    if (nrow(u)) proxy |> addCircleMarkers(
      lng = u$lon, lat = u$lat, layerId = paste0("u:", u$node_id), group = "utilities",
      radius = util_radius(u$pop), stroke = TRUE, color = "#ffffff", weight = 0.8, opacity = 0.9,
      fillColor = util_color(u$org_class), fillOpacity = 0.85,
      label = u$name, labelOptions = labelOptions(className = "dd-tip"),
      options = pathOptions(pane = "utilities", bubblingMouseEvents = FALSE))
  })

  # ---- (3) links from utilities to their sources -----------------------------------
  links_shown <- reactive({
    mode <- input$link_mode %||% "click"
    if (identical(mode, "none")) return(integer(0))
    if (identical(mode, "all")) return(which(LK$utility_id %in% utils_shown()$node_id))
    if (is.null(rv$util)) return(integer(0))
    which(LK$utility_id == rv$util)
  })

  observe({
    i <- links_shown()
    proxy <- leafletProxy("map") |> clearGroup("links") |> clearGroup("sources")
    if (!length(i)) return()
    all_mode <- identical(input$link_mode, "all")
    for (purchased in c(FALSE, TRUE)) {                 # own intake solid, purchases dashed
      j <- i[(LK$kind[i] != "Own intake") == purchased]
      if (length(j)) proxy <- proxy |> addPolylines(
        data = LK_GEOM[j], group = "links", color = unname(WATER_COLORS[LK$water[j]]),
        weight = link_weight(LK$acft[j]), opacity = if (all_mode) 0.45 else 0.9, dashArray = if (purchased) "6 5" else NULL,
        label = sprintf("%s to %s: %s acre-feet", LK$source_name[j], UTIL_D$name[match(LK$utility_id[j], UTIL_D$node_id)], fmt_int(LK$acft[j])),
        labelOptions = labelOptions(className = "dd-tip", sticky = TRUE),
        options = pathOptions(pane = "links"))
    }
    s <- SRC[SRC$node_id %in% LK$source_id[i], ]
    col <- unname(SOURCE_COLORS[s$type]); col[is.na(col)] <- UT$charcoal
    proxy |> addCircleMarkers(
      lng = s$lon, lat = s$lat, layerId = paste0("s:", s$node_id), group = "sources",
      radius = if (all_mode) 3 else 6, stroke = TRUE, color = col, weight = if (all_mode) 1.2 else 2, opacity = 1, fillColor = "#ffffff", fillOpacity = 1,
      label = paste0(s$name, " (", s$type, ")"), labelOptions = labelOptions(className = "dd-tip"),
      options = pathOptions(pane = "sources", bubblingMouseEvents = FALSE))
  })

  # ---- (4) data centers ----------------------------------------------------------
  dc_shown <- reactive({
    if (!isTRUE(input$show_dc)) return(DC_MAP[0, ])
    DC_MAP[as.character(DC_MAP$status) %in% (input$dc_status %||% character(0)), ]
  })

  observe({
    d <- dc_shown()
    proxy <- leafletProxy("map") |> clearGroup("dc")
    if (nrow(d)) proxy |> addCircleMarkers(
      lng = d$lon, lat = d$lat, layerId = paste0("d:", d$dc_id), group = "dc",
      radius = dc_radius(d$mw, isTRUE(input$dc_size)), stroke = TRUE,
      color = ifelse(d$status == "Planned or approved", "#1b1b1b", "#ffffff"), weight = 1.6, opacity = 1,
      fillColor = unname(STATUS_COLORS[as.character(d$status)]), fillOpacity = 0.85,
      label = paste0(d$name, ifelse(is.na(d$mw), "", paste0(" ", MDOT, " ", fmt_mw(d$mw))), ifelse(d$approximate, " (approximate location)", "")),
      labelOptions = labelOptions(className = "dd-tip"),
      options = pathOptions(pane = "dc", bubblingMouseEvents = FALSE))
  })

  # ---- clicks ----------------------------------------------------------------------
  popup_at <- function(lng, lat, html) leafletProxy("map") |> clearPopups() |>
    addPopups(lng, lat, html, options = popupOptions(maxWidth = 360, minWidth = 280, maxHeight = 360))

  ring <- function(lng, lat, r = 11) leafletProxy("map") |> clearGroup("selected") |>
    addCircleMarkers(lng = lng, lat = lat, group = "selected", radius = r, fill = FALSE, color = UT$orange, weight = 3, opacity = 1,
                     options = pathOptions(pane = "selected", interactive = FALSE))

  pick_datacenter <- function(id) {
    d <- DC[match(id, DC$dc_id), ]
    if (is.na(d$dc_id) || is.na(d$lat)) return()
    rv$dc <- id
    rv$util <- if (blank(d$utility_id)) NULL else d$utility_id
    ring(d$lon, d$lat, dc_radius(d$mw, isTRUE(input$dc_size)) + 5)
    popup_at(d$lon, d$lat, popup_datacenter(d))
  }

  observeEvent(input$map_marker_click, {
    ck <- input$map_marker_click
    id <- ck$id %||% ""
    key <- substring(id, 3)
    if (startsWith(id, "d:")) {
      pick_datacenter(key)
    } else if (startsWith(id, "u:")) {
      u <- UTIL_D[match(key, UTIL_D$node_id), ]
      rv$util <- key; rv$dc <- NULL
      ring(u$lon, u$lat, util_radius(u$pop) + 5)
      popup_at(u$lon, u$lat, popup_utility(u))
    } else if (startsWith(id, "s:")) {
      s <- SRC[match(key, SRC$node_id), ]
      popup_at(s$lon, s$lat, popup_source(s))
    }
  })

  observeEvent(input$map_click, {                       # anywhere that is not a point: what is at this spot
    ck <- input$map_click
    popup_at(ck$lng, ck$lat, popup_here(ck$lng, ck$lat))
  })

  # ---- legend (the side panel already keys the points and boundaries) ---------------------
  output$legend <- renderUI({
    item <- function(sw, text) tags$div(class = "it", sw, tags$span(text))
    parts <- list()
    if (!identical(input$link_mode, "none") && length(links_shown())) parts <- c(parts, list(
      tags$h6(sprintf("Source links, %d", REF_YEAR)),
      lapply(names(WATER_COLORS)[1:3], function(w) item(swatch(WATER_COLORS[[w]], "line"), w)),
      tags$div(class = "dd-sm", "Solid: own intake. Dashed: purchased. Thicker: more water. Ringed points are sources and sellers.")))
    if (!is.null(input$usdm) && !identical(input$usdm, "none")) parts <- c(parts, list(
      tags$h6(paste("Drought Monitor,", sub("^This week \\((.*)\\)$", "\\1", usdm_label(input$usdm)))),
      lapply(names(USDM_COLORS)[-1], function(k) item(swatch(USDM_COLORS[[k]], "box"), USDM_NAMES[[k]]))))
    if (isTRUE(input$show_svi)) parts <- c(parts, list(
      tags$h6("Social vulnerability (tracts)"),
      lapply(5:1, function(q) item(swatch(RAMP_SV[q], "box"), SV_NAMES[q]))))
    if (!length(parts)) return(NULL)
    tags$div(class = "dd-legend", parts)
  })

  # ---- the table -----------------------------------------------------------------------
  output$table <- renderDT({
    datatable(TABLE, rownames = FALSE, selection = "single",
              options = list(pageLength = 25, scrollX = TRUE, order = list(list(3, "desc")),
                             columnDefs = list(list(className = "dt-nowrap", targets = "_all"))),
              class = "compact stripe hover nowrap") |>
      formatRound("Capacity (MW)", digits = 1)
  })

  observeEvent(input$table_rows_selected, {
    i <- input$table_rows_selected
    if (!length(i) || is.na(DC$lat[i])) return()
    nav_select("page", "Map")
    if (!isTRUE(input$show_dc)) updateCheckboxInput(session, "show_dc", value = TRUE)
    if (!(as.character(DC$status[i]) %in% input$dc_status))
      updateCheckboxGroupInput(session, "dc_status", selected = union(input$dc_status, as.character(DC$status[i])))
    leafletProxy("map") |> setView(DC$lon[i], DC$lat[i], zoom = 11)
    pick_datacenter(DC$dc_id[i])
  })

  output$dl <- downloadHandler(
    filename = function() sprintf("texas_data_centers_drought_%s.csv", format(Sys.Date(), "%Y%m%d")),
    content = function(file) {
      out <- TABLE
      out[] <- lapply(out, function(col) if (is.character(col)) ascii_punct(col) else col)
      names(out) <- ascii_punct(names(out))
      con <- file(file, open = "wb"); on.exit(close(con))
      writeBin(as.raw(c(0xef, 0xbb, 0xbf)), con)        # byte-order mark, so Excel reads it as UTF-8
      write.csv(out, con, row.names = FALSE, na = "")
    })
}

shinyApp(ui, server)
