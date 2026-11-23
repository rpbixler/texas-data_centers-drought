# =============================================================================
# deploy.R  --  publish Texas Drought and Data Centers to shinyapps.io (lbj-sspg)
# -----------------------------------------------------------------------------
# Run from the app folder (drought_mapper/data_centers, the one holding app.R).
# Authentication uses the rsconnect token already stored on this machine; no
# credentials belong here.
#
# Before the first deploy:
#   1. source("prepare_datacenter_data.R")     # writes data/drought_datacenters.rds
#   2. shiny::runApp() and confirm the map draws, the links follow a click, and
#      the table fills in.
#
# Re-running with the same appName overwrites in place and keeps the URL.
# =============================================================================
library(rsconnect)
options(rsconnect.timeout = 300)

DATA_FILE <- "data/drought_datacenters.rds"

# Set to TRUE to publish even though the data build reports something left out
DEPLOY_ANYWAY <- FALSE

stopifnot("Run this from the app folder" = file.exists("app.R"),
          "Run prepare_datacenter_data.R first to create data/drought_datacenters.rds" = file.exists(DATA_FILE))

notes <- readRDS(DATA_FILE)$meta$notes
if (length(notes)) {
  message("The data build reports ", length(notes), " item(s) left out or incomplete:")
  for (n in notes) message("  - ", n)
  if (!DEPLOY_ANYWAY) stop("Not published. Finish the build, or set DEPLOY_ANYWAY <- TRUE above to publish as is.", call. = FALSE)
  message("DEPLOY_ANYWAY is TRUE: publishing as is.")
}

# Only the app and its prepared data file are published. The data center csv,
# the hand file and the scripts stay on this machine.
app_files <- c("app.R", DATA_FILE)

message(sprintf("Publishing %d file(s), %.1f MB (data prepared %s)",
                length(app_files), sum(file.info(app_files)$size) / 1e6,
                format(file.info(DATA_FILE)$mtime, "%Y-%m-%d %H:%M")))

deployApp(
  appDir      = ".",
  appFiles    = app_files,
  appName     = "Texas-Drought-Data-Centers",   # no periods allowed in app names
  appTitle    = "Texas Drought and Data Centers",
  account     = "lbj-sspg",
  server      = "shinyapps.io",
  forceUpdate = TRUE
)
