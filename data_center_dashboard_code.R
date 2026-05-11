# =============================================================================
# RECKONING WITH THE DATA CENTER BOOM IN THE US
# A Geographic & Media Analysis
# Danielle Stemper | Pratt MSDAV | INFO 609: Spatial Thinking & GIS – 2026
# =============================================================================
# DASHBOARD OVERVIEW:
#   This Shiny dashboard is an interactive, narrative-driven data journalism
#   project exploring the socioeconomic and geographic implications of the
#   U.S. data center boom – including social vulnerability, rising energy
#   costs, and proposed infrastructure in Indiana.
#
# TAB STRUCTURE:
#   Home      – Narrative framework / data storytelling introduction
#   Tab 1     – Data center locations by estimated power usage
#   Tab 2     – Electricity price changes & volatility vs. data centers
#   Tab 3     – Social Vulnerability Index vs. data center locations
#   Tab 4     – Indiana case study: proposed data centers + SVI
#   References – Sources, methodology notes, GitHub link
# =============================================================================


# =============================================================================
# SECTION 1: LIBRARY LOADING
# =============================================================================

library(shiny)           # Core Shiny framework for reactive web apps
library(shinydashboard)  # Dashboard layout components (sidebar, boxes, tabs)
library(leaflet)         # Interactive map rendering via Leaflet.js
library(leaflet.extras)  # Extended Leaflet plugins (heatmaps, fullscreen, etc.)
library(sf)              # Simple Features: reading/handling spatial vector data (.gpkg)
library(dplyr)           # Data manipulation (filter, mutate, join, select)
library(readr)           # Fast CSV reading (read_csv)
library(ggplot2)         # Static chart/plot creation for homepage visuals
library(plotly)          # Converts ggplot2 objects to interactive charts
library(scales)          # Formatting helpers for axes, labels (percent, comma)
library(RColorBrewer)    # Color palettes for choropleth maps and charts
library(htmltools)       # HTML tag construction for custom Leaflet popups/labels
library(bslib)           # Bootstrap theming utilities for custom CSS integration
library(stringr)         # String manipulation for data cleaning
library(tigris)          # Download U.S. Census TIGER shapefiles (county polygons)


# =============================================================================
# SECTION 2: CONFIGURATION & GLOBAL SETTINGS
# =============================================================================

# Set working directory – all data files should be in this folder
setwd("/Users/daniellestemper/Desktop/696 dash files/")

# Explicitly register the www/ folder so Shiny serves static files
# (images, etc.) correctly when setwd() is used
addResourcePath("www", "/Users/daniellestemper/Desktop/696 dash files/www")

# ---- Color Palette (Dark Theme – consistent across all tabs) ----------------
# Primary accent:     electric violet / indigo for highlights and selections
# Secondary accent:   warm amber for data center point markers
# Map choropleth:     diverging purple-to-yellow for SVI; red sequential for prices
COLORS <- list(
  bg_primary    = "#1a1625",   # Deep dark purple-black (main background)
  bg_secondary  = "#221e30",   # Slightly lighter panel background
  bg_card       = "#2a2540",   # Card/box backgrounds
  bg_sidebar    = "#160f2b",   # Darkest: sidebar
  accent_violet = "#7c3aed",   # Primary accent (interactive elements, headers)
  accent_indigo = "#4f46e5",   # Secondary accent (hover states, borders)
  accent_amber  = "#f59e0b",   # Data center marker color (warm contrast)
  accent_teal   = "#14b8a6",   # Tertiary accent (highlights, links)
  text_primary  = "#f1f0f5",   # Near-white body text
  text_muted    = "#a09ab8",   # Muted/secondary text
  text_heading  = "#e8e0ff",   # Slightly warm white for headings
  border        = "#3d3560"    # Subtle border color for cards and dividers
)

# ---- Map tile provider (dark basemap) ----------------------------------------
DARK_TILE_URL <- "https://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}{r}.png"
DARK_TILE_ATTR <- paste0(
  '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>',
  ' contributors &copy; <a href="https://carto.com/attributions">CARTO</a>'
)


# =============================================================================
# SECTION 3: DATA LOADING & PREPROCESSING
# =============================================================================
# All data is loaded once at startup (not inside server) for performance.
# Errors during load are caught and surfaced gracefully in the UI.

# ---- 3a. Data Centers (geocoded point layer) ---------------------------------
dc_points <- tryCatch({
  st_read("us_data_centers_geocoded.gpkg", quiet = TRUE) |>
    st_transform(4326)   # Ensure WGS 84 CRS for Leaflet compatibility
}, error = function(e) {
  message("ERROR loading us_data_centers_geocoded.gpkg: ", e$message)
  NULL
})

# ---- 3b. Electricity data (geocoded polygon/point layer) ---------------------
# Raw file contains avg_rate_2020 through avg_rate_2025.
# pct_change and volatility are derived here at load time:
#   pct_change = % increase from 2020 to 2025
#   volatility = standard deviation across all 6 yearly averages (price stability)
electricity <- tryCatch({
  st_read("us_electricity_geocoded.gpkg", quiet = TRUE) |>
    st_transform(4326) |>
    mutate(
      pct_change = round(((avg_rate_2025 - avg_rate_2020) / avg_rate_2020) * 100, 1),
      volatility = round(
        apply(
          st_drop_geometry(pick(avg_rate_2020, avg_rate_2021, avg_rate_2022,
                                avg_rate_2023, avg_rate_2024, avg_rate_2025)),
          1, sd, na.rm = TRUE
        ),
        2
      )
    )
}, error = function(e) {
  message("ERROR loading us_electricity_geocoded.gpkg: ", e$message)
  NULL
})

# ---- 3c. CDC Social Vulnerability Index 2022 (county-level CSV) --------------
svi_raw <- tryCatch({
  read_csv("SVI_2022_US_county.csv", show_col_types = FALSE)
}, error = function(e) {
  message("ERROR loading SVI_2022_US_county.csv: ", e$message)
  NULL
})

# ---- 3d. Indiana Proposed Data Center Locations (CSV with lat/lon) -----------
indiana_dc_raw <- tryCatch({
  read_csv("indiana_proposed_dc_2025_-_locs.csv", show_col_types = FALSE)
}, error = function(e) {
  message("ERROR loading indiana_proposed_dc_2025_-_locs.csv: ", e$message)
  NULL
})

# ---- 3e. Preprocessing: SVI – clean columns and join to county polygons ------
# SVI is tabular only; we use tigris::counties() to get the county geometries,
# then join by FIPS so we can draw choropleth polygons in Leaflet.
if (!is.null(svi_raw)) {

  # Clean and select key SVI columns
  svi <- svi_raw |>
    select(
      FIPS      = FIPS,
      county    = COUNTY,
      state     = STATE,
      st_abbr   = ST_ABBR,
      svi_rank  = RPL_THEMES,   # Overall SVI percentile rank (0–1); primary variable
      svi_rank1 = RPL_THEME1,   # Theme 1: Socioeconomic Status
      svi_rank2 = RPL_THEME2,   # Theme 2: Household Characteristics
      svi_rank3 = RPL_THEME3,   # Theme 3: Racial & Ethnic Minority Status
      svi_rank4 = RPL_THEME4    # Theme 4: Housing Type & Transportation
    ) |>
    mutate(
      FIPS     = str_pad(as.character(FIPS), 5, pad = "0"),
      svi_rank = ifelse(svi_rank < 0, NA_real_, svi_rank),  # CDC flags missing as -999
      high_svi = svi_rank >= 0.80
    )

  # Download national county polygons from Census TIGER via tigris
  # options(tigris_use_cache = TRUE) caches the download so it only runs once
  options(tigris_use_cache = TRUE)
  county_sf <- tryCatch({
    tigris::counties(cb = TRUE, resolution = "5m", year = 2022, progress_bar = FALSE) |>
      st_transform(4326) |>
      select(GEOID, geometry)
  }, error = function(e) {
    message("ERROR downloading county polygons via tigris: ", e$message)
    NULL
  })

  # Join SVI data to county polygons by FIPS / GEOID
  if (!is.null(county_sf)) {
    svi_sf <- county_sf |>
      left_join(svi, by = c("GEOID" = "FIPS")) |>
      filter(!is.na(svi_rank))   # drop counties with no SVI data
  } else {
    svi_sf <- NULL
  }

  # Indiana-only subset for Tab 4
  svi_indiana_sf <- if (!is.null(svi_sf)) {
    svi_sf |> filter(st_abbr == "IN")
  } else NULL

} else {
  svi        <- NULL
  svi_sf     <- NULL
  svi_indiana_sf <- NULL
}

# ---- 3f. Preprocessing: Indiana proposed data centers -----------------------
# Confirmed columns: name, owner, city, county, latitude, longitude,
#                    project_status, electric_utility, iso,
#                    anticipated_pwr_demand_mw, acres
if (!is.null(indiana_dc_raw)) {
  indiana_dc <- indiana_dc_raw |>
    filter(!is.na(latitude), !is.na(longitude))
} else {
  indiana_dc <- NULL
}

# ---- 3g. Preprocessing: extract DC coordinates for Leaflet ------------------
# Leaflet requires coordinates as numeric vectors, not sf geometry
if (!is.null(dc_points)) {
  dc_coords <- st_coordinates(dc_points)
  dc_df <- dc_points |>
    st_drop_geometry() |>
    mutate(
      lon          = dc_coords[, 1],
      lat          = dc_coords[, 2],
      # Pad county_fips to 5 chars so it matches SVI FIPS key
      county_fips  = str_pad(as.character(county_fips), 5, pad = "0"),
      # Clamp power usage for symbol sizing; handle NAs with base R ifelse
      pwr_clean    = pmax(0, ifelse(is.na(est_pwr_use_high), 0, est_pwr_use_high)),
      # Scale radius: min 4px, max 20px
      pwr_radius = scales::rescale(pwr_clean, to = c(4, 20))
    )
}


# =============================================================================
# SECTION 4: HELPER FUNCTIONS
# =============================================================================

#' Build a consistent dark-themed Leaflet map base
#' @param ... Additional leaflet() arguments
make_base_map <- function(...) {
  leaflet(...) |>
    addTiles(
      urlTemplate  = DARK_TILE_URL,
      attribution  = DARK_TILE_ATTR,
      options      = tileOptions(maxZoom = 18)
    ) |>
    setView(lng = -98.5, lat = 39.5, zoom = 4) |>   # Continental US
    addFullscreenControl() |>
    addScaleBar(position = "bottomleft")
}

#' Generate a Leaflet color palette function for SVI percentile
make_svi_palette <- function() {
  colorNumeric(
    palette = "YlOrRd",     # Yellow → Orange → Red: low to high vulnerability
    domain  = c(0, 1),
    na.color = "#444444"
  )
}

#' Standard data center circle marker (amber, semi-transparent)
#' @param map       A Leaflet map object
#' @param data      Data frame with lat, lon columns
#' @param radius    Numeric radius or column name for proportional symbols
#' @param group     Layer group name (for layer control)
#' @param label_col Column name for popup label text
add_dc_markers <- function(map, data, radius = 6, group = "Data Centers",
                           label_col = NULL) {
  popup_text <- if (!is.null(label_col) && label_col %in% names(data)) {
    data[[label_col]]
  } else {
    "Data Center"
  }

  addCircleMarkers(
    map,
    lng         = data$lon,
    lat         = data$lat,
    radius      = radius,
    color       = COLORS$accent_amber,
    fillColor   = COLORS$accent_amber,
    fillOpacity = 0.75,
    weight      = 1,
    opacity     = 0.9,
    popup       = popup_text,
    group       = group,
    clusterOptions = NULL   # No clustering – show all individual points
  )
}


# =============================================================================
# SECTION 5: CUSTOM CSS (Dark Theme)
# =============================================================================

custom_css <- tags$style(HTML(paste0("
  /* ---- Root Variables (values interpolated from COLORS list at startup) ---- */
  :root {
    --bg-primary:    ", COLORS$bg_primary, ";
    --bg-secondary:  ", COLORS$bg_secondary, ";
    --bg-card:       ", COLORS$bg_card, ";
    --bg-sidebar:    ", COLORS$bg_sidebar, ";
    --accent-violet: ", COLORS$accent_violet, ";
    --accent-indigo: ", COLORS$accent_indigo, ";
    --accent-amber:  ", COLORS$accent_amber, ";
    --accent-teal:   ", COLORS$accent_teal, ";
    --text-primary:  ", COLORS$text_primary, ";
    --text-muted:    ", COLORS$text_muted, ";
    --text-heading:  ", COLORS$text_heading, ";
    --border:        ", COLORS$border, ";
    --font-body:     'IBM Plex Mono', monospace;
    --font-display:  'Space Grotesk', sans-serif;
  }

  /* ---- Google Fonts import ---- */
  @import url('https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500&family=Space+Grotesk:wght@400;500;700&display=swap');

  /* ---- Global Overrides ---- */
  body, .wrapper, .content-wrapper, .main-header {
    background-color: var(--bg-primary) !important;
    color: var(--text-primary) !important;
    font-family: var(--font-body) !important;
  }

  /* ---- Header / Navbar ---- */
  .main-header {
    border-bottom: 2px solid #ffffff !important;
  }
  .main-header .navbar,
  .main-header .logo {
    background-color: var(--bg-sidebar) !important;
    border-bottom: none !important;
    font-family: var(--font-display) !important;
    color: #ffffff !important;
    font-weight: 700 !important;
    letter-spacing: 0.03em;
  }
  .main-header .logo {
    font-size: 13px !important;
    padding: 0 12px !important;
    line-height: 50px;
  }
  /* Sidebar toggle (hamburger) lines white */
  .main-header .navbar .sidebar-toggle,
  .main-header .navbar .sidebar-toggle:hover {
    background-color: transparent !important;
  }
  .main-header .navbar .sidebar-toggle .icon-bar {
    background-color: #ffffff !important;
  }

  /* ---- Sidebar ---- */
  .main-sidebar, .left-side {
    background-color: var(--bg-sidebar) !important;
    border-right: 1px solid var(--border) !important;
  }
  .sidebar-menu > li > a {
    color: var(--text-muted) !important;
    font-family: var(--font-body) !important;
    font-size: 12px;
    letter-spacing: 0.04em;
    border-left: 3px solid transparent;
    transition: all 0.2s ease;
  }
  .sidebar-menu > li.active > a,
  .sidebar-menu > li > a:hover {
    color: var(--text-heading) !important;
    background-color: rgba(124, 58, 237, 0.15) !important;
    border-left: 3px solid var(--accent-violet) !important;
  }
  .sidebar-menu > li > a > .fa {
    color: var(--accent-violet) !important;
  }

  /* ---- Content / Tab Panes ---- */
  .tab-content, .tab-pane {
    background-color: var(--bg-primary) !important;
  }

  /* ---- Boxes / Cards ---- */
  .box {
    background-color: var(--bg-card) !important;
    border: 1px solid var(--border) !important;
    border-radius: 8px !important;
    box-shadow: 0 4px 20px rgba(0,0,0,0.4) !important;
  }
  .box-header {
    background-color: var(--bg-card) !important;
    color: var(--text-heading) !important;
    font-family: var(--font-display) !important;
    font-weight: 700;
    border-bottom: 1px solid var(--border) !important;
    font-size: 15px;
    letter-spacing: 0.02em;
  }
  .box-body {
    background-color: var(--bg-card) !important;
    color: var(--text-primary) !important;
  }

  /* ---- Map containers ---- */
  .leaflet-container {
    border-radius: 6px;
    border: 1px solid var(--border);
  }
  .leaflet-control-layers {
    background: var(--bg-card) !important;
    color: var(--text-primary) !important;
    border: 1px solid var(--border) !important;
    border-radius: 6px;
    font-family: var(--font-body);
    font-size: 12px;
  }
  .leaflet-control-layers-base label,
  .leaflet-control-layers-overlays label {
    color: var(--text-primary) !important;
  }

  /* ---- Homepage / Article Styles ---- */
  .article-container {
    max-width: 820px;
    margin: 0 auto;
    padding: 32px 24px;
    line-height: 1.85;
    font-size: 14px;
    color: var(--text-primary);
  }
  .article-eyebrow {
    font-family: var(--font-body);
    font-size: 11px;
    letter-spacing: 0.15em;
    text-transform: uppercase;
    color: var(--accent-violet);
    margin-bottom: 12px;
  }
  .article-title {
    font-family: var(--font-display);
    font-size: 38px;
    font-weight: 700;
    color: #ffffff;
    line-height: 1.15;
    margin-bottom: 8px;
  }
  .article-subtitle {
    font-size: 16px;
    color: var(--text-muted);
    margin-bottom: 28px;
    font-style: italic;
  }
  .article-byline {
    font-size: 11px;
    letter-spacing: 0.08em;
    color: var(--text-muted);
    border-top: 1px solid var(--border);
    border-bottom: 1px solid var(--border);
    padding: 10px 0;
    margin-bottom: 32px;
    text-transform: uppercase;
  }
  .article-body p {
    margin-bottom: 20px;
    font-size: 14px;
    color: var(--text-primary);
    font-family: var(--font-display);
  }
  .article-body a {
    color: var(--text-primary) !important;
    text-decoration: underline !important;
  }
  .article-body a:hover {
    color: var(--text-heading) !important;
    text-decoration: underline !important;
  }
  .article-body h2 {
    font-family: var(--font-display);
    font-size: 22px;
    font-weight: 700;
    color: var(--text-heading);
    margin: 36px 0 14px 0;
    border-left: 4px solid var(--accent-violet);
    padding-left: 14px;
  }
  .article-body h3 {
    font-family: var(--font-display);
    font-size: 16px;
    font-weight: 600;
    color: var(--accent-teal);
    margin: 24px 0 10px 0;
  }
  .article-body blockquote {
    border-left: 3px solid var(--accent-amber);
    background: rgba(245, 158, 11, 0.06);
    margin: 24px 0;
    padding: 14px 20px;
    color: var(--text-muted);
    font-style: italic;
    border-radius: 0 6px 6px 0;
  }

  /* ---- Stat Callout Cards (Homepage) ---- */
  .stat-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(160px, 1fr));
    gap: 16px;
    margin: 28px 0;
  }
  .stat-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 20px 16px;
    text-align: center;
  }
  .stat-number {
    font-family: var(--font-display);
    font-size: 32px;
    font-weight: 700;
    color: var(--accent_amber);
    line-height: 1;
    margin-bottom: 6px;
  }
  .stat-label {
    font-size: 11px;
    color: var(--text-muted);
    text-transform: uppercase;
    letter-spacing: 0.08em;
    line-height: 1.4;
  }

  /* ---- Tab Map Page Layout ---- */
  .map-page-wrapper {
    padding: 24px;
  }
  .map-title {
    font-family: var(--font-display);
    font-size: 22px;
    font-weight: 700;
    color: var(--text-heading);
    margin-bottom: 6px;
  }
  .map-subtitle {
    font-size: 12px;
    color: var(--text-muted);
    margin-bottom: 20px;
    letter-spacing: 0.04em;
  }
  .map-writeup {
    font-size: 13px;
    color: var(--text-muted);
    line-height: 1.8;
    margin-top: 20px;
    padding: 16px 20px;
    border-left: 3px solid var(--border);
    font-style: italic;
  }

  /* ---- References Tab ---- */
  .references-container {
    max-width: 760px;
    margin: 0 auto;
    padding: 32px 24px;
  }
  .ref-section-title {
    font-family: var(--font-display);
    font-size: 20px;
    font-weight: 700;
    color: var(--text-heading);
    margin: 28px 0 14px;
    border-bottom: 1px solid var(--border);
    padding-bottom: 8px;
  }
  .ref-item {
    margin-bottom: 16px;
    font-size: 13px;
    color: var(--text-primary);
    line-height: 1.7;
    padding-left: 20px;
    text-indent: -20px;
  }
  .ref-item a {
    color: var(--accent-teal) !important;
    text-decoration: none;
  }
  .ref-item a:hover {
    text-decoration: underline;
    color: var(--accent-violet) !important;
  }
  .github-link-box {
    background: linear-gradient(135deg, rgba(124,58,237,0.15), rgba(79,70,229,0.1));
    border: 1px solid var(--accent-violet);
    border-radius: 8px;
    padding: 20px 24px;
    margin: 24px 0;
    display: flex;
    align-items: center;
    gap: 16px;
  }
  .github-link-box a {
    color: var(--accent-violet) !important;
    font-family: var(--font-display);
    font-weight: 600;
    font-size: 15px;
    text-decoration: none;
  }

  /* ---- Scrollbar ---- */
  ::-webkit-scrollbar { width: 6px; height: 6px; }
  ::-webkit-scrollbar-track { background: var(--bg-primary); }
  ::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }
  ::-webkit-scrollbar-thumb:hover { background: var(--accent-violet); }

  /* ---- Checkbox / Input Controls ---- */
  .shiny-input-container label {
    color: var(--text-primary) !important;
    font-family: var(--font-body) !important;
    font-size: 12px;
  }
  input[type='checkbox']:checked { accent-color: var(--accent-violet); }

  /* ---- Write-up prose (below each map) ---- */
  .writeup-section {
    max-width: 820px;
    margin: 28px auto 0 auto;
    padding: 0 4px;
    font-family: 'IBM Plex Mono', monospace;
    font-size: 13px;
    color: var(--text-primary);
    line-height: 1.85;
  }
  .writeup-section p {
    margin-bottom: 16px;
  }
  .writeup-section .pull-quote {
    border-left: 4px solid var(--accent-amber);
    background: rgba(245,158,11,0.06);
    padding: 14px 20px;
    margin: 20px 0;
    font-style: italic;
    color: var(--text-muted);
    border-radius: 0 6px 6px 0;
    font-size: 13px;
    line-height: 1.8;
  }
  .writeup-section .pull-quote strong {
    color: var(--text-primary);
    font-style: normal;
  }

  /* ---- Infographic stat cards (reusable) ---- */
  .info-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(150px, 1fr));
    gap: 14px;
    margin: 24px 0;
  }
  .info-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 18px 14px;
    text-align: center;
  }
  .info-card .info-number {
    font-family: 'Space Grotesk', sans-serif;
    font-size: 30px;
    font-weight: 700;
    color: var(--accent_amber);
    line-height: 1.1;
    margin-bottom: 6px;
  }
  .info-card .info-label {
    font-size: 11px;
    color: var(--text-muted);
    text-transform: uppercase;
    letter-spacing: 0.08em;
    line-height: 1.4;
  }

  /* ---- Link-list infographic (policy tracker cards) ---- */
  .link-list {
    display: flex;
    flex-direction: column;
    gap: 10px;
    margin: 20px 0;
  }
  .link-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 6px;
    padding: 12px 16px;
    font-size: 12px;
    line-height: 1.6;
  }
  .link-card a {
    color: var(--accent-teal) !important;
    text-decoration: none;
    font-weight: 500;
  }
  .link-card a:hover { text-decoration: underline; }
  .link-card .link-author {
    color: var(--text-muted);
    font-size: 11px;
  }

  /* ---- Indiana factor list ---- */
  .factor-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(260px, 1fr));
    gap: 12px;
    margin: 20px 0;
  }
  .factor-card {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 14px 16px;
  }
  .factor-card .factor-title {
    font-family: 'Space Grotesk', sans-serif;
    font-size: 13px;
    font-weight: 600;
    color: var(--accent_amber);
    margin-bottom: 4px;
  }
  .factor-card .factor-body {
    font-size: 12px;
    color: var(--text-muted);
    line-height: 1.6;
  }

  /* ---- Infographic box wrapper (titled panels) ---- */
  .info-box-panel {
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 20px 22px;
    margin: 24px 0;
  }
  .info-box-panel .info-box-title {
    font-family: 'Space Grotesk', sans-serif;
    font-size: 15px;
    font-weight: 700;
    color: var(--text-heading);
    margin-bottom: 4px;
  }
  .info-box-panel .info-box-subtitle {
    font-size: 12px;
    color: var(--text-muted);
    margin-bottom: 16px;
  }

  /* ---- Footer bar ---- */
  .dashboard-footer {
    text-align: center;
    padding: 20px 0 8px 0;
    font-size: 11px;
    color: var(--text-muted);
    letter-spacing: 0.06em;
    border-top: 1px solid var(--border);
    margin-top: 32px;
  }
")))


# =============================================================================
# SECTION 6: UI DEFINITION
# =============================================================================

ui <- dashboardPage(
  skin = "black",   # Base AdminLTE skin (will be overridden by custom CSS)

  # ---- 6a. Header -----------------------------------------------------------
  dashboardHeader(
    title = tags$span(
      style = "font-family:'Space Grotesk',sans-serif; font-size:13px; font-weight:700; letter-spacing:0.02em; color:#ffffff;",
      "EXPLORING THE DATA CENTER BOOM"
    ),
    titleWidth = 380
  ),

  # ---- 6b. Sidebar ----------------------------------------------------------
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Home",           tabName = "home",       icon = icon("newspaper")),
      menuItem("Power Usage",    tabName = "tab_power",  icon = icon("bolt")),
      menuItem("Energy Costs",   tabName = "tab_energy", icon = icon("chart-line")),
      menuItem("Social Vulnerability", tabName = "tab_svi", icon = icon("map-marked-alt")),
      menuItem("Indiana Case Study",   tabName = "tab_indiana", icon = icon("search-location")),
      menuItem("About",     tabName = "references", icon = icon("book-open"))
    ),

    # Sidebar footer: byline
    tags$div(
      style = paste0(
        "position:absolute; bottom:16px; left:0; right:0; padding:0 16px;",
        "font-size:10px; color:", COLORS$text_muted, "; line-height:1.6;",
        "font-family:'IBM Plex Mono',monospace;"
      ),
      tags$p("Danielle Stemper"),
      tags$p("Pratt MSDAV"),
      tags$p("INFO 609 · 2026")
    )
  ),

  # ---- 6c. Body -------------------------------------------------------------
  dashboardBody(
    custom_css,

    # Google Fonts (loaded in head)
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(
        rel  = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@300;400;500&family=Space+Grotesk:wght@400;500;600;700&display=swap"
      )
    ),

    tabItems(

      # ===========================================================
      # TAB: HOME — Narrative / Data Storytelling
      # ===========================================================
      tabItem(
        tabName = "home",
        tags$div(
          class = "article-container",

          # Eyebrow + Title
          tags$p(class = "article-eyebrow", "PRATT MSDAV · INFO 696 · 2026"),
          tags$h1(class = "article-title", "Exploring the Data Center Boom in the U.S."),
          tags$p(
            class = "article-byline",
            "Author: Danielle Stemper"
          ),

          tags$div(
            class = "article-body",

            tags$h2("Data centers – the physical infrastructure of everything digital. Every search query, every streamed video, every AI-generated image passes through a data center. These massive warehouses filled with racks of servers humming at near-constant capacity have become the seemingly substrate of human life – seemingly unseen, and even overlooked, until recently."),

            tags$p("Big Tech’s AI push has propelled data centers and data center construction into the spotlight. Now, in an election year, data center regulation could be the hottest issue on the ballot for voters. The data center boom has accelerated to a pace that is drawing scrutiny from utility regulators, environmental advocates, and local governments alike."),

            tags$p("The relationship between data centers and Americans is complex, and regulating the industry means addressing the AI equation as well: it seems more and more Americans are beginning to understand the direct connection between Big Tech’s AI push and the data center construction boom."),

            tags$p("The push for AI adoption has resulted in explosive demand for cloud computing, generative AI models, and streaming media. But as companies such as Oracle, Amazon, and Microsoft conduct massive layoffs in order to invest in AI, Americans are wary of the proclaimed societal benefits of AI and, by extension, further data center construction."),

            # ---- Water consumption bar chart infographic ----
            tags$div(
              style = "margin: 28px 0;",
              tags$div(
                style = paste0(
                  "background:", "var(--bg-card)", ";",
                  "border:1px solid var(--border);",
                  "border-radius:8px;",
                  "padding:20px 22px 14px 22px;"
                ),
                tags$p(
                  style = "font-family:'Space Grotesk',sans-serif; font-size:16px; font-weight:700; color:#ffffff; margin-bottom:4px;",
                  "Google’s 10 Thirstiest U.S. Data Centers"
                ),
                tags$p(
                  style = "font-family:'Space Grotesk',sans-serif; font-size:12px; color:var(--text-muted); margin-bottom:16px; line-height:1.6;",
                  "One medium-sized data center can consume 100 million gallons of water per year for cooling. If the average American household uses 300 gallons per day, a mid-size would require the equivalent of ~1,000 U.S. households annually."
                ),
                plotlyOutput("chart_water", height = "340px"),
                tags$p(
                  style = "font-family:'IBM Plex Mono',monospace; font-size:10px; color:var(--text-muted); margin-top:10px;",
                  "Source: ", tags$em("Google 2025 Environmental Report")
                )
              )
            ),

            tags$p("Every bit of information online is stored in data centers, but information around data center locations themselves—existing locations, construction plans, even the businesses behind them—is precarious and sporadic at best. These companies seem to follow our every move online, but who’s tracking them?"),

            tags$p("Amidst the data center construction push, efforts to fight back have been spreading at municipal levels: neighborhood groups coming together to vote against data center proposals, local governments passing construction moratoriums, and increased federal-level activity to develop better AI regulation overall. Regulation and policy-tracking projects by independent organizations and research groups have also been fighting to understand and uncover data center activity."),

            tags$p("The goal of this project is to capture how some Americans are reacting to the data center boom as well as offer a high-level introduction to the key concerns and socioeconomic factors surrounding it."),

            tags$h2("“Who captures value from American innovation, and on what terms?”"),

            tags$p("That’s one of the questions guiding the mission of the ",
              tags$a(href = "https://mapping-ai.org/", target = "_blank", "Mapping AI Working Group"),
              ", a collection of researchers, policy experts, and practitioners, who have set out to identify the who, when, what, and why of AI governance as a foundation for a coordinated policy agenda ahead of the 2028 presidential election cycle."
            ),

            # ---- Mapping AI screenshot image ----
            tags$div(
              style = "margin: 20px 0;",
              tags$div(
                style = "background:var(--bg-card); border:1px solid var(--border); border-radius:8px; padding:16px 18px 12px 18px;",
                tags$img(
                  src   = "www/mapping_ai_screenshot.png",
                  id    = "mapping_ai_img",
                  style = "width:100%; border-radius:4px; display:block;",
                  alt   = "Mapping AI product interface showing a network visualization of AI governance entities"
                ),
                tags$p(
                  style = "font-family:'IBM Plex Mono',monospace; font-size:11px; color:var(--text-muted); margin-top:10px; margin-bottom:0;",
                  tags$em("Mapping AI product, Mapping AI Working Group.")
                )
              )
            ),

            tags$p("The Working Group says that the landscape of AI policy in the U.S. is fragmented, and the question unifying “labor, safety, national security, and institutional design is the same: ",
              tags$strong(tags$em("who captures value from American innovation, and on what terms?")),
              "”"
            ),

            tags$h2("Distribution as a Design Criterion for how AI’s Benefits Flow"),

            tags$p("According to the Working Group’s ",
              tags$a(href = "https://www.mapping-ai.org/about", target = "_blank", "website"),
              ", “good governance will be critical to ensure human flourishing.” Its organizing principle is ",
              tags$strong("distribution"),
              " as a design criterion for how AI’s benefits flow."
            ),

            tags$h2("In an election year, data center regulation could be the hottest issue on the ballot."),

            tags$p("Just as organizations like the Working Group are looking toward the 2028 presidential election cycle, major movement is happening at the state and local levels ahead of the 2026 midterm elections."),

            tags$p("In Virginia, a hotspot for tech infrastructure known as “Data Center Alley,” data centers secured bipartisan support as recently as 2023. But growing resistance, even across political parties, is evident: a recent ",
              tags$a(href = "http://washingtonpost.com/tablet/2026/04/03/march-26-31-2026-washington-post-schar-school-virginia-poll/?itid=lk_inline_manual_2", target = "_blank", "Washington Post-Schar School poll"),
              " found that Virginia voters have turned sharply against data centers. 59 percent of voters said they would be uncomfortable if a new data center was built in their community, up from just 24 percent in 2023."
            ),

            # ---- Virginia poll diverging bar chart ----
            tags$div(
              style = "margin: 24px 0;",
              tags$div(
                style = "background:var(--bg-card); border:1px solid var(--border); border-radius:8px; padding:20px 22px 14px 22px;",
                tags$p(
                  style = "font-family:'Space Grotesk',sans-serif; font-size:15px; font-weight:700; color:#ffffff; margin-bottom:6px; line-height:1.3;",
                  "Virginia voters have turned against data centers in their community since 2023"
                ),
                tags$p(
                  style = "font-family:'Space Grotesk',sans-serif; font-size:12px; color:var(--text-muted); margin-bottom:4px;",
                  "Q: Would you be comfortable or uncomfortable if a new data center were built in your community?"
                ),
                plotlyOutput("chart_virginia_poll", height = "160px"),
                tags$p(
                  style = "font-family:'IBM Plex Mono',monospace; font-size:10px; color:var(--text-muted); margin-top:8px; line-height:1.5;",
                  "Source: Washington Post-Schar School poll conducted March 26–31, 2026, among 1,101 Virginia registered voters with an error margin of +/- 3.4 percentage points."
                )
              )
            ),

            tags$h2("Wisconsin voters already have shown that ballot measures could be a key tool for those seeking to block data center projects."),

            tags$p("Residents in Port Washington, Wisconsin, living near a $15 billion data center construction site complained about 24/7 work for months before the city set boundaries. The data center would serve companies Vantage, Oracle, and OpenAI. The construction, starting in December 2025, ",
              tags$a(href = "https://www.jsonline.com/story/communities/north/2026/03/26/hellish-24-hour-data-center-construction-in-port-washington-to-end/89289886007/", target = "_blank", "caused major day-to-day disruptions for residents"),
              " in the form of road closures, heavy traffic, loud machinery, and “a sea of floodlights” (Levins, 2026)."
            ),

            tags$p("Resident Dean Wiegert felt the impact of construction inside his home: “he constantly hears beeping from equipment and feels incessant vibrations that sporadically escalate into more intense rumblings” (Levins, 2026). Finally, after a major push by residents, Port Washington’s Plan Commission unanimously voted to limit the construction hours at the site."),

            tags$h2("But the momentum in Port Washington hasn’t ended there."),

            tags$p("Concerned about transparency, noise pollution, freshwater use, and energy costs, residents voted overwhelmingly on Tuesday, April 7 to ",
              tags$a(href = "https://www.politico.com/news/2026/04/08/wisconsin-city-passes-nations-first-anti-data-center-referendum-00863432", target = "_blank", "restrict future data centers"),
              " in the region (Katzenberger, 2026). This “first-of-its-kind” referendum – which requires city leaders to obtain voter approval before awarding developers lucrative tax incentives – could offer a blueprint for opponents of AI infrastructure across the country."
            ),

            tags$p("The Port Washington referendum doesn’t interfere with the ongoing construction of the Vantage, Oracle, and OpenAI data center campus, but targets all future development projects. It won’t be the last time we see ",
              tags$a(href = "https://www.politico.com/news/2026/04/08/wisconsin-city-passes-nations-first-anti-data-center-referendum-00863432", target = "_blank", "voters weigh-in"),
              " on data center-related policy:"
            ),

            # Upcoming ballot measures
            tags$div(
              class = "info-box-panel",
              tags$div(class = "info-box-title", "Upcoming Data Center Ballot Measures"),
              tags$div(
                style = "display:flex; flex-direction:column; gap:10px;",
                tags$div(class = "link-card",
                  tags$strong("Monterey Park, California"),
                  tags$span(style = "color:var(--text-muted);", " — June 2026"),
                  tags$br(),
                  "Residents will decide on a measure seeking to ban new data construction within city limits indefinitely."
                ),
                tags$div(class = "link-card",
                  tags$strong("Augusta Township, Michigan"),
                  tags$span(style = "color:var(--text-muted);", " — August 2026"),
                  tags$br(),
                  "Residents will vote whether to override a local ordinance that paved the way for a data center project."
                ),
                tags$div(class = "link-card",
                  tags$strong("Janesville, Wisconsin"),
                  tags$span(style = "color:var(--text-muted);", " — November 2026"),
                  tags$br(),
                  "Residents are slated to vote on a measure that could sink plans to turn a former assembly plant into an AI factory."
                )
              )
            ),

            tags$h2("It’s clear that the current push to regulate data center construction isn’t coming from federal authorities, it’s driven by communities, localities, and the efforts of independent creators."),

            # Policy tracking projects infographic
            tags$div(
              class = "info-box-panel",
              tags$div(class = "info-box-title", "Data Center and AI Policy Tracking Projects"),
              tags$div(class = "info-box-subtitle", "Just a sampling of recent projects, dashboards, and more that were created with the data center and AI boom in mind."),
              tags$div(
                class = "link-list",
                tags$div(class = "link-card",
                  tags$a(href = "https://mapping-ai.org/", target = "_blank", "Mapping the U.S. AI Policy Landscape"),
                  tags$span(class = "link-author", " — "),
                  tags$a(href = "https://mapping-ai.org/about", target = "_blank", class = "link-author", "Mapping AI Working Group")
                ),
                tags$div(class = "link-card",
                  tags$a(href = "http://trackpolicy.org", target = "_blank", "TrackPolicy.org"),
                  tags$span(class = "link-author", " — "),
                  tags$a(href = "https://x.com/isareksopuro", target = "_blank", class = "link-author", "Isabelle Reksopuro")
                ),
                tags$div(class = "link-card",
                  tags$a(href = "https://ai-policy-tracker-tracker.vercel.app/", target = "_blank", "AI Policy Tracker Tracker"),
                  tags$span(class = "link-author", " — "),
                  tags$a(href = "https://x.com/Daniel_Kalish_", target = "_blank", class = "link-author", "Daniel Kalish")
                ),
                tags$div(class = "link-card",
                  tags$a(href = "http://datacenterbans.com", target = "_blank", "DataCenterBans.com"),
                  tags$span(class = "link-author", " — "),
                  tags$a(href = "https://x.com/willmanidis", target = "_blank", class = "link-author", "Will Manidis")
                ),
                tags$div(class = "link-card",
                  tags$a(href = "https://www.pecva.org/region/loudoun/existing-and-proposed-data-centers-a-web-map/", target = "_blank", "Virginia Existing and Proposed Data Centers"),
                  tags$span(class = "link-author", " — "),
                  tags$a(href = "https://www.pecva.org/", target = "_blank", class = "link-author", "Piedmont Environmental Council")
                ),
                tags$div(class = "link-card",
                  tags$a(href = "https://www.datacenterwatch.org/", target = "_blank", "Data Center Watch")
                )
              )
            ),

            tags$h2("Ultimately, the battle between public opinion and Big Tech around the need for AI infrastructure will continue as governmental interventions—including efforts to regulate—vary at all levels."),

            # Footer
            tags$div(class = "dashboard-footer", "Copyright Danielle Stemper 2026  ·  Pratt MSDAV")
          )
        )
      ),  # end home tabItem


      # ===========================================================
      # TAB 1: DATA CENTER POWER USAGE MAP
      # ===========================================================
      tabItem(
        tabName = "tab_power",
        tags$div(
          class = "map-page-wrapper",
          tags$h2(class = "map-title", "Data Center Estimated Average Power Usage"),
          tags$p(class = "map-subtitle",
            "Circle size proportional to estimated high-end power usage (MW). Hover for details."
          ),
          box(
            width = 12,
            solidHeader = TRUE,
            leafletOutput("map_power", height = "560px")
          ),
          tags$div(
            class = "writeup-section",
            tags$p("According to Business Insider, by the end of 2024, 1,024 data centers in America were already built or approved for construction. Those locations are mapped here according to their estimated annual power usage."),
            tags$p("In an extensive investigation into America’s data centers, the publication also found that 40% of locations were situated in areas of high or extremely high water stress. Companies tend to build data centers in clusters near a power supply with access to water sources.")
          )
        )
      ),  # end tab_power


      # ===========================================================
      # TAB 2: ELECTRICITY PRICE CHANGES & VOLATILITY
      # ===========================================================
      tabItem(
        tabName = "tab_energy",
        tags$div(
          class = "map-page-wrapper",
          tags$h2(class = "map-title", "Are Americans Bearing the Brunt of Rising Energy Costs?"),
          tags$p(class = "map-subtitle",
            "Toggle layers to compare average residential electricity price change (2020–2025), price volatility, and data center locations."
          ),

          box(
            width = 12,
            solidHeader = TRUE,
            leafletOutput("map_energy", height = "520px")
          ),

          fluidRow(
            column(12,
              tags$div(
                class = "writeup-section",
                tags$p("There is a clear regional pattern to the data: northeastern states tend to be more volatile in electricity pricing than midwestern and southern states."),
                tags$p("Interestingly, high volatility states also tended to have high average prices \u2013 consumers there tend to face both expensive and predictable electricity costs. However, high volatility didn\u2019t always mean high percent change in price. For example, Vermont had moderate volatility but only a 17% increase in price, while DC had both high volatility and relatively extreme percent change.")
              ),
              tags$div(
                style = "padding: 0 4px;",
                tags$div(
                  class = "info-grid",
                  tags$div(class = "info-card",
                    tags$div(class = "info-number", "+73%"),
                    tags$div(class = "info-label", "Highest % Price Increase 2020\u201325 (Washington D.C.)")
                  ),
                  tags$div(class = "info-card",
                    tags$div(class = "info-number", "+10%"),
                    tags$div(class = "info-label", "Lowest % Price Increase 2020\u201325 (Iowa)")
                  ),
                  tags$div(class = "info-card",
                    tags$div(class = "info-number", "~29%"),
                    tags$div(class = "info-label", "National Average % Price Increase 2020\u201325")
                  ),
                  tags$div(class = "info-card",
                    tags$div(class = "info-number", "HI & CA"),
                    tags$div(class = "info-label", "Most Volatile Price Changes 2020\u201325")
                  ),
                  tags$div(class = "info-card",
                    tags$div(class = "info-number", "336"),
                    tags$div(class = "info-label", "Data Centers in States w/ Price Increases Above National Average")
                  )
                )
              )
            )
          )
        )
      ),  # end tab_energy


      # ===========================================================
      # TAB 3: SOCIAL VULNERABILITY INDEX
      # ===========================================================
      tabItem(
        tabName = "tab_svi",
        tags$div(
          class = "map-page-wrapper",
          tags$h2(class = "map-title",
            "Are Data Centers Disproportionately Located in Socially Vulnerable Areas?"
          ),
          tags$p(class = "map-subtitle",
            "County-level CDC Social Vulnerability Index (overall index percentile) overlaid with national data center locations. Darker red = higher vulnerability."
          ),
          box(
            width = 12,
            solidHeader = TRUE,
            leafletOutput("map_svi", height = "560px")
          ),
          tags$div(
            class = "writeup-section",

            tags$p("One of the questions driving the geographical analysis portion of the research: are data centers disproportionately located in socially vulnerable areas?"),
            tags$p("Using the CDC/ATSDR Social Vulnerability Index (SVI) – a county-level measure that captures socioeconomic status, household characteristics, racial and ethnic minority status, and housing and transportation factors – this page maps data center locations against vulnerability percentiles nationwide."),
            tags$p("A holistic approach to the study of long term impact of data centers is imperative. Despite heavy existing data center concentration in counties with low social vulnerability, it’s proved challenging to map ongoing, proposed, and planned data center construction – efforts to do so are largely siloed by locality and municipality, which makes studying national trends and forecasting long-term effects difficult."),

            # Stat infographic
            tags$div(
              class = "info-grid",
              tags$div(class = "info-card",
                tags$div(class = "info-number", "204"),
                tags$div(class = "info-label", "Data Centers in the most socially vulnerable counties")
              ),
              tags$div(class = "info-card",
                tags$div(class = "info-number", "636"),
                tags$div(class = "info-label", "# of counties within larger socially vulnerable regions")
              ),
              tags$div(class = "info-card",
                tags$div(class = "info-number", "99"),
                tags$div(class = "info-label", "# of isolated & socially vulnerable counties")
              )
            ),

            # Cluster & Outlier analysis panel
            tags$div(
              class = "info-box-panel",
              tags$div(class = "info-box-title", "Social Vulnerability Index Cluster & Outlier Analysis"),
              tags$div(class = "info-box-subtitle", "This enables us to identify potential regional trends and differentiate between isolated vs. regional occurrences of social vulnerability."),
              tags$p(tags$strong("SVI Clusters & Outliers:"), " The Southeast, Gulf Coast, and Texas are areas of concern given the presence of high-high clusters, meaning these are counties with high SVI surrounded by other counties with high SVI. Some data centers appear in high-low outlier counties: these are high SVI counties isolated by low SVI counties."),
              tags$p(tags$strong("Electricity Prices:"), " All states showed positive values in price percent change, meaning every state’s average electricity price got more expensive from 2020-25. Most states had a moderate (20-38%) change in price. Washington D.C. had the largest price surge (73.7%), and Iowa saw the smallest price increase (10.1%)."),
              tags$p(tags$strong("Price Volatility:"), " Hawaii, California, and Maine had the highest price volatility (4.8-5.5). Most midwest and some southern states had low volatility (0.5-1.5)")
            )
          )
        )
      ),  # end tab_svi


      # ===========================================================
      # TAB 4: INDIANA CASE STUDY
      # ===========================================================
      tabItem(
        tabName = "tab_indiana",
        tags$div(
          class = "map-page-wrapper",
          tags$h2(class = "map-title", "Zooming In: Proposed Data Centers in Indiana"),
          tags$p(class = "map-subtitle",
            "Indiana county-level Social Vulnerability Index (overall percentile) with proposed data center locations as of 2025."
          ),
          box(
            width = 12,
            solidHeader = TRUE,
            leafletOutput("map_indiana", height = "560px")
          ),
          tags$div(
            class = "writeup-section",

            tags$p("I connected with Ben Inskeep, Program Director with Citizens Coalition – the state’s oldest and largest consumer and environmental advocacy organization – to discuss the state of the data center boom in Indiana."),
            tags$p("According to Inskeep, “the avalanche of data center proposals, their unprecedented size, and their extraordinary impacts caught me off guard.” Just two years ago, Inskeep wouldn’t have even considered data centers to be a relevant concern in Indiana."),
            tags$p("He says that changed in June 2024:"),

            tags$div(class = "pull-quote",
              tags$em("“[I]n a span of a few days, we attended separate stakeholder meetings with Indiana Michigan Power and NIPSCO (two utilities serving northern Indiana) where each utility shared that as a result of prospective data center customers coming to their service territories, "),
              tags$strong(tags$em("the total amount of power they needed would more than double within the next 5-10 years.")),
              tags$em("”")
            ),

            # Indiana attractiveness factors
            tags$div(
              class = "info-box-panel",
              tags$div(class = "info-box-title", "What makes Indiana attractive for AI data center construction?"),
              tags$div(class = "info-box-subtitle", "While a number of states are seeing a boom in data center proposals, there are some specific factors unique to Indiana driving the influx of data center proposals in the state."),
              tags$div(
                class = "factor-grid",
                tags$div(class = "factor-card",
                  tags$div(class = "factor-title", "Lucrative state subsidies"),
                  tags$div(class = "factor-body", "Particularly a 50-year sales tax exemption on data center equipment and electricity purchases which can result in billions of dollars in subsidies for each large data center.")
                ),
                tags$div(class = "factor-card",
                  tags$div(class = "factor-title", "Regulatory and legislative capture"),
                  tags$div(class = "factor-body", "Powerful industries have a track record of getting favorable laws and regulatory treatment, such as special rates on electricity.")
                ),
                tags$div(class = "factor-card",
                  tags$div(class = "factor-title", "Existing infrastructure and connectivity"),
                  tags$div(class = "factor-body", "This includes high-voltage transmission lines, power access on two major regional grids (PJM and MISO), fiber optic cable, highways, and railways.")
                ),
                tags$div(class = "factor-card",
                  tags$div(class = "factor-title", "Water availability"),
                  tags$div(class = "factor-body", "Relative to the western United States, Indiana has better access to water (which is used in data center cooling systems).")
                ),
                tags$div(class = "factor-card",
                  tags$div(class = "factor-title", "Low risk of major natural disasters"),
                  tags$div(class = "factor-body", "Hurricanes and wildfires are not common in this region.")
                ),
                tags$div(class = "factor-card",
                  tags$div(class = "factor-title", "Proximity to population centers"),
                  tags$div(class = "factor-body", "Where it is situated, Indiana is relatively close to other midwest population centers such as Chicago.")
                ),
                tags$div(class = "factor-card",
                  tags$div(class = "factor-title", "Relatively low cost land and labor"),
                  tags$div(class = "factor-body", "This makes buying land and building data centers on it lower cost than in many other places.")
                )
              )
            ),

            tags$p("From that point on, Inskeep has pivoted and reprioritized his work to address the impact of data centers on affordability and environmental sustainability. Part of this work, says Inskeep, involves educating, organizing, and mobilizing citizens."),

            tags$div(class = "pull-quote",
              tags$em("“To date, 10 of [Indiana’s] 92 counties have passed data center moratoriums, and one county has permanently banned data centers.”")
            ),

            tags$p("It’s clear the anti-AI data center movement is building momentum across the state. Inskeep says one of the challenges will continue to be working with elected officials who have ignored the wishes of their constituents in favor of appeasing Big Tech.")
          )
        )
      ),  # end tab_indiana


      # ===========================================================
      # TAB: REFERENCES

      # ===========================================================
      # TAB: REFERENCES / ABOUT
      # ===========================================================
      tabItem(
        tabName = "references",
        tags$div(
          class = "references-container",

          tags$h1(
            style = paste0("font-family:'Space Grotesk',sans-serif; font-size:26px; font-weight:700; color:", COLORS$text_heading, "; margin-bottom:4px;"),
            "About"
          ),
          tags$p(style = paste0("color:", COLORS$text_muted, "; font-size:13px; margin-bottom:24px; line-height:1.8;"),
            "All data, code, and methodology documentation are publicly available in the GitHub repository linked below."
          ),

          # GitHub link box
          tags$div(
            class = "github-link-box",
            tags$span(style = paste0("font-size:20px; color:", COLORS$accent_violet, ";"), "\u2325"),
            tags$div(
              tags$p(style = "margin:0; font-size:11px; color:#a09ab8; letter-spacing:0.08em; text-transform:uppercase;",
                "Code & Data Repository"),
              tags$a(
                href   = "https://github.com/daniellestemp/data-center-dashboard",
                target = "_blank",
                "github.com/daniellestemp/data-center-dashboard"
              )
            )
          ),

          # References
          tags$p(class = "ref-section-title", "References"),

          tags$p(class = "ref-item",
            "Campbell, D., & Beckler, H. (2025, September 29). Are you living near a data center? Our interactive map shows where construction is skyrocketing. ",
            tags$em("Business Insider."), " ",
            tags$a(href = "https://www.businessinsider.com/data-center-locations-us-map-ai-boom-2025-9",
              target = "_blank", "https://www.businessinsider.com/data-center-locations-us-map-ai-boom-2025-9")
          ),

          tags$p(class = "ref-item",
            "CDC/ATSDR. (2024, July 22). Social Vulnerability Index. ",
            tags$em("Centers for Disease Control and Prevention."), " ",
            tags$a(href = "https://www.atsdr.cdc.gov/place-health/php/svi/index.html",
              target = "_blank", "https://www.atsdr.cdc.gov/place-health/php/svi/index.html")
          ),

          tags$p(class = "ref-item",
            "Coble, P. (2026, April 8). Texas losing a billion dollars a year on Data Center Tax Break. ",
            tags$em("The Texas Tribune."), " ",
            tags$a(href = "https://www.texastribune.org/2026/04/08/texas-data-centers-sales-tax-break-billion-dollars/",
              target = "_blank", "https://www.texastribune.org/2026/04/08/texas-data-centers-sales-tax-break-billion-dollars/")
          ),

          tags$p(class = "ref-item",
            "Data Center Coalition. (2025, July 23). Statement from Data Center Coalition on Winning the AI Race: America\u2019s AI Action Plan. ",
            tags$em("DCC."), " ",
            tags$a(href = "https://www.datacentercoalition.org/cpages/ai-action-plan",
              target = "_blank", "https://www.datacentercoalition.org/cpages/ai-action-plan")
          ),

          tags$p(class = "ref-item",
            "Katzenberger, T. (2026, April 8). Wisconsin City Passes Nation\u2019s First Anti-Data Center Referendum. ",
            tags$em("Politico."), " ",
            tags$a(href = "https://www.politico.com/news/2026/04/08/wisconsin-city-passes-nations-first-anti-data-center-referendum-00863432",
              target = "_blank", "https://www.politico.com/news/2026/04/08/wisconsin-city-passes-nations-first-anti-data-center-referendum-00863432")
          ),

          tags$p(class = "ref-item",
            "Levins, C. (2026, March 26). \u201cHellish\u201d 24-hour data center construction in Port Washington to end. ",
            tags$em("Milwaukee Journal Sentinel."), " ",
            tags$a(href = "https://www.jsonline.com/story/communities/north/2026/03/26/hellish-24-hour-data-center-construction-in-port-washington-to-end/89289886007/",
              target = "_blank", "https://www.jsonline.com/story/communities/north/2026/03/26/hellish-24-hour-data-center-construction-in-port-washington-to-end/89289886007/")
          ),

          tags$p(class = "ref-item",
            "Mapping AI Working Group. (2026). Mapping AI. ",
            tags$a(href = "https://www.mapping-ai.org/", target = "_blank", "https://www.mapping-ai.org/")
          ),

          tags$p(class = "ref-item",
            "U.S. Energy Information Administration. (2025, October 7). Electric Sales, Revenue, and Average Price. ",
            tags$em("EIA Independent Statistics and Analysis."), " ",
            tags$a(href = "https://www.eia.gov/electricity/sales_revenue_price/",
              target = "_blank", "https://www.eia.gov/electricity/sales_revenue_price/")
          ),

          tags$p(class = "ref-item",
            "Ya\u00f1ez-Barnuevo, M. (2026, February 24). Data center power demands are contributing to higher energy bills. ",
            tags$em("EESI \u2014 Environmental and Energy Study Institute."), " ",
            tags$a(href = "https://www.eesi.org/articles/view/data-center-power-demands-are-contributing-to-higher-energy-bills",
              target = "_blank", "https://www.eesi.org/articles/view/data-center-power-demands-are-contributing-to-higher-energy-bills")
          ),

          tags$p(class = "ref-item",
            "Indiana Citizens Action Coalition. (2026). AI Data Center Build Out Creates Unprecedented Risk to Hoosiers. ",
            tags$a(href = "https://www.citact.org/ai-data-centers",
              target = "_blank", "https://www.citact.org/ai-data-centers")
          ),

          tags$p(class = "ref-item",
            "Hegde, G. (2026, January 23). Myths vs. reality: Data centers and water usage. ",
            tags$em("Florida Water and Pollution Control Operators Association."), " ",
            tags$a(href = "https://www.fwpcoa.org/content.aspx?page_id=5&club_id=859275&item_id=130961",
              target = "_blank", "https://www.fwpcoa.org/content.aspx?page_id=5&club_id=859275&item_id=130961")
          ),

          # Tools & Libraries
          tags$p(class = "ref-section-title", "Tools & Libraries"),
          tags$p(style = paste0("font-size:13px; color:", COLORS$text_primary, "; line-height:1.8;"),
            "This dashboard was built entirely in R using the following packages: ",
            tags$code("shiny"), " and ", tags$code("shinydashboard"),
            " (application framework and layout); ",
            tags$code("leaflet"), " and ", tags$code("leaflet.extras"),
            " (interactive mapping); ",
            tags$code("sf"), " (spatial data reading and transformation); ",
            tags$code("tigris"), " (U.S. Census county boundary shapefiles); ",
            tags$code("dplyr"), ", ", tags$code("readr"), ", and ", tags$code("stringr"),
            " (data wrangling); ",
            tags$code("ggplot2"), " and ", tags$code("plotly"),
            " (charting); ",
            tags$code("scales"), ", ", tags$code("RColorBrewer"), ", ", tags$code("htmltools"),
            ", and ", tags$code("bslib"), " (formatting, color palettes, and theming). ",
            "Spatial preprocessing was performed in ArcGIS Online and QGIS."
          ),

          # Author's Note
          tags$p(class = "ref-section-title", "Author\u2019s Note"),
          tags$p(style = paste0("font-size:13px; color:", COLORS$text_primary, "; line-height:1.8;"),
            "Special thank you to Ben Inskeep, Hannah Beckler, and Isabelle Reksopuro, who took the time to connect with me and answer the many questions I had about their own work and experience around the topic of data centers."
          ),
          tags$p(style = paste0("font-size:13px; color:", COLORS$text_primary, "; line-height:1.8;"),
            "This project was developed for INFO-696 Advanced Projects in Visualization, a course in the MS-Data Analysis & Visualization program at Pratt Institute in New York City."
          ),

          tags$div(class = "dashboard-footer", "Copyright Danielle Stemper 2026  \u00b7  Pratt MSDAV")
        )
      )  # end references tabItem


    )  # end tabItems
  )  # end dashboardBody
)  # end dashboardPage


# =============================================================================
# SECTION 7: SERVER LOGIC
# =============================================================================

server <- function(input, output, session) {

  # ---------------------------------------------------------------------------
  # MAP 1: Data Center Power Usage (Tab 1)
  # ---------------------------------------------------------------------------
  output$map_power <- renderLeaflet({
    map <- make_base_map()

    if (is.null(dc_points) || is.null(dc_df)) {
      # Graceful fallback: show empty map with a message
      return(
        map |>
          addControl(
            html = tags$div(
              style = "background:rgba(42,37,64,0.9); color:#f1f0f5; padding:12px 18px; border-radius:6px; font-family:'IBM Plex Mono',monospace; font-size:12px;",
              "⚠ Data file not found: us_data_centers_geocoded.gpkg"
            ),
            position = "topright"
          )
      )
    }


    # NAME is the confirmed display column in us_data_centers_geocoded.gpkg
    dc_labels <- ifelse(is.na(dc_df$NAME) | dc_df$NAME == "", "Data Center",
                        as.character(dc_df$NAME))

    # Build popup as a plain character vector (no ~ formula — avoids metaData error)
    popup_html <- paste0(
      "<div style='font-family:IBM Plex Mono,monospace;font-size:12px;line-height:1.6;'>",
      "<strong style='color:#7c3aed;'>", dc_labels, "</strong><br>",
      "Est. Power Use (High): <strong>",
      ifelse(is.na(dc_df$pwr_clean) | dc_df$pwr_clean == 0, "N/A",
             paste0(round(dc_df$pwr_clean, 1), " MW")),
      "</strong></div>"
    )

    map |>
      addCircleMarkers(
        lng          = dc_df$lon,
        lat          = dc_df$lat,
        radius       = dc_df$pwr_radius,
        color        = COLORS$accent_amber,
        fillColor    = COLORS$accent_amber,
        fillOpacity  = 0.70,
        weight       = 1,
        opacity      = 0.9,
        popup        = popup_html,
        label        = dc_labels,         # plain vector, not ~ formula
        labelOptions = labelOptions(
          style     = list("font-family" = "IBM Plex Mono, monospace", "font-size" = "11px"),
          direction = "top", offset = c(0, -8)
        )
      ) |>
      addLegend(
        position  = "bottomright",
        title     = "<span style='font-family:IBM Plex Mono,monospace;font-size:11px;'>Est. Power Use (MW)</span>",
        colors    = c(COLORS$accent_amber, COLORS$accent_amber, COLORS$accent_amber),
        labels    = c("Low", "Medium", "High"),
        opacity   = 0.85
      )
  })



  # ---------------------------------------------------------------------------
  # MAP 2: Electricity Prices (Tab 2)
  # All layers added at render time with named groups; Leaflet's native
  # addLayersControl() provides the toggle UI — matching SVI and Indiana tabs.
  # ---------------------------------------------------------------------------
  output$map_energy <- renderLeaflet({
    map <- make_base_map()

    if (!is.null(electricity)) {
      elec_df <- electricity

      # ---- % Price Change palette & layer ----
      pct_pal <- colorNumeric(
        palette  = c("#fef08a", "#fb923c", "#b91c1c"),
        domain   = elec_df$pct_change,
        na.color = "#555555"
      )
      pct_popup <- paste0(
        "<div style='font-family:IBM Plex Mono,monospace;font-size:12px;line-height:1.7;'>",
        "<strong style='font-size:13px;'>", elec_df$NAME, " (", elec_df$STUSPS, ")</strong><br>",
        "<strong style='color:#ef4444;'>% Price Change 2020–25:</strong> +",
        elec_df$pct_change, "%<br>",
        "2020 avg rate: ", elec_df$avg_rate_2020, "¢/kWh<br>",
        "2025 avg rate: ", elec_df$avg_rate_2025, "¢/kWh",
        "</div>"
      )
      map <- map |>
        addPolygons(
          data        = elec_df,
          fillColor   = pct_pal(elec_df$pct_change),
          fillOpacity = 0.72,
          color       = "#0d0b14",
          weight      = 0.6,
          popup       = pct_popup,
          highlight   = highlightOptions(
            weight = 2, color = "#f59e0b",
            fillOpacity = 0.85, bringToFront = TRUE
          ),
          group = "% Price Change"
        ) |>
        addLegend(
          pal      = pct_pal,
          values   = elec_df$pct_change,
          position = "bottomright",
          title    = "<span style='font-family:IBM Plex Mono,monospace;font-size:11px;'>% Price Change<br>2020–25</span>",
          labFormat = labelFormat(suffix = "%"),
          opacity  = 0.88,
          layerId  = "legend_pct"
        )

      # ---- Price Volatility palette & layer ----
      vol_pal <- colorNumeric(
        palette  = c("#fef08a", "#fb923c", "#b91c1c"),
        domain   = elec_df$volatility,
        na.color = "#555555"
      )
      vol_popup <- paste0(
        "<div style='font-family:IBM Plex Mono,monospace;font-size:12px;line-height:1.7;'>",
        "<strong style='font-size:13px;'>", elec_df$NAME, " (", elec_df$STUSPS, ")</strong><br>",
        "<strong style='color:#4f46e5;'>Price Volatility (Std Dev):</strong> ",
        elec_df$volatility, "¢/kWh",
        "</div>"
      )
      map <- map |>
        addPolygons(
          data        = elec_df,
          fillColor   = vol_pal(elec_df$volatility),
          fillOpacity = 0.68,
          color       = "#0d0b14",
          weight      = 0.6,
          popup       = vol_popup,
          highlight   = highlightOptions(
            weight = 2, color = "#14b8a6",
            fillOpacity = 0.85, bringToFront = TRUE
          ),
          group = "Price Volatility"
        ) |>
        addLegend(
          pal      = vol_pal,
          values   = elec_df$volatility,
          position = "bottomleft",
          title    = "<span style='font-family:IBM Plex Mono,monospace;font-size:11px;'>Price Volatility<br>(Std. Dev. ¢/kWh)</span>",
          opacity  = 0.88,
          layerId  = "legend_vol"
        )
    }

    # ---- Data Center point layer ----
    if (!is.null(dc_df)) {
      map <- map |>
        addCircleMarkers(
          lng         = dc_df$lon,
          lat         = dc_df$lat,
          radius      = 4,
          color       = COLORS$accent_amber,
          fillColor   = COLORS$accent_amber,
          fillOpacity = 0.8,
          weight      = 1,
          popup       = ifelse(is.na(dc_df$NAME) | dc_df$NAME == "", "Data Center",
                               as.character(dc_df$NAME)),
          group       = "Data Centers"
        )
    }

    # ---- Native Leaflet layer toggle — matches SVI and Indiana tabs ----
    map |>
      addLayersControl(
        overlayGroups = c("% Price Change", "Price Volatility", "Data Centers"),
        options       = layersControlOptions(collapsed = FALSE)
      ) |>
      # Hide Volatility by default (matches original UX intent)
      hideGroup("Price Volatility")
  })


  # ---------------------------------------------------------------------------
  # MAP 3: Social Vulnerability Index (Tab 3)
  # County choropleth from tigris polygons + SVI join, overlaid with DC points
  # ---------------------------------------------------------------------------
  output$map_svi <- renderLeaflet({
    map <- make_base_map()

    # Graceful fallback if data failed to load
    if (is.null(svi_sf)) {
      return(
        map |>
          addControl(
            html = tags$div(
              style = "background:rgba(42,37,64,0.9); color:#f1f0f5; padding:12px 18px; border-radius:6px; font-family:'IBM Plex Mono',monospace; font-size:12px;",
              "⚠ SVI county polygons could not be loaded. Check tigris / internet connection."
            ),
            position = "topright"
          )
      )
    }

    # SVI palette: dark purple (low vulnerability) → amber → red (high vulnerability)
    svi_pal <- colorNumeric(
      palette  = c("#2d1b69", "#7c3aed", "#f59e0b", "#ef4444"),
      domain   = c(0, 1),
      na.color = "#333333"
    )

    # Build popup for county polygons
    svi_popup <- paste0(
      "<div style='font-family:IBM Plex Mono,monospace;font-size:12px;line-height:1.7;'>",
      "<strong style='font-size:13px;'>", svi_sf$county, ", ", svi_sf$st_abbr, "</strong><br>",
      "<strong style='color:#f59e0b;'>Overall SVI Percentile:</strong> ",
      round(svi_sf$svi_rank * 100, 1), "<br>",
      "Socioeconomic: ", round(svi_sf$svi_rank1 * 100, 1), "<br>",
      "Household Char.: ", round(svi_sf$svi_rank2 * 100, 1), "<br>",
      "Minority Status: ", round(svi_sf$svi_rank3 * 100, 1), "<br>",
      "Housing/Transport: ", round(svi_sf$svi_rank4 * 100, 1),
      "</div>"
    )

    # Layer 1: SVI county choropleth
    map <- map |>
      addPolygons(
        data        = svi_sf,
        fillColor   = svi_pal(svi_sf$svi_rank),
        fillOpacity = 0.72,
        color       = "#0d0b14",
        weight      = 0.4,
        popup       = svi_popup,
        highlight   = highlightOptions(
          weight      = 2,
          color       = "#f59e0b",
          fillOpacity = 0.88,
          bringToFront = TRUE
        ),
        group = "SVI by County"
      )

    # Layer 2: Data center points joined to SVI by county_fips
    if (!is.null(dc_df)) {
      dc_svi <- dc_df |>
        left_join(
          svi |> select(FIPS, svi_rank, county, high_svi),
          by = c("county_fips" = "FIPS")
        )

      dc_svi_popup <- paste0(
        "<div style='font-family:IBM Plex Mono,monospace;font-size:12px;line-height:1.7;'>",
        "<strong>", ifelse(is.na(dc_svi$NAME) | dc_svi$NAME == "", "Data Center", dc_svi$NAME), "</strong><br>",
        "County: ", ifelse(is.na(dc_svi$county), "N/A", dc_svi$county), "<br>",
        "SVI Percentile: ", ifelse(is.na(dc_svi$svi_rank), "N/A", round(dc_svi$svi_rank * 100, 1)),
        ifelse(isTRUE(dc_svi$high_svi), "<br><strong style='color:#ef4444;'>⚠ High Vulnerability County</strong>", ""),
        "</div>"
      )

      map <- map |>
        addCircleMarkers(
          lng         = dc_svi$lon,
          lat         = dc_svi$lat,
          radius      = 5,
          color       = "#ffffff",
          fillColor   = COLORS$accent_amber,
          fillOpacity = 0.90,
          weight      = 1,
          opacity     = 0.9,
          popup       = dc_svi_popup,
          group       = "Data Centers"
        )
    }

    map |>
      addLegend(
        position  = "bottomright",
        pal       = svi_pal,
        values    = svi_sf$svi_rank,
        title     = "<span style='font-family:IBM Plex Mono,monospace;font-size:11px;'>Overall SVI<br>Percentile Rank</span>",
        labFormat = labelFormat(transform = function(x) round(x * 100)),
        opacity   = 0.88
      ) |>
      addLayersControl(
        overlayGroups = c("SVI by County", "Data Centers"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })


  # ---------------------------------------------------------------------------

  # ---------------------------------------------------------------------------
  # MAP 4: Indiana Case Study (Tab 4)
  # Indiana county SVI choropleth (tigris polygons) + proposed DC point markers
  # Known DC columns: name, owner, city, county, latitude, longitude,
  #                   project_status, electric_utility, iso,
  #                   anticipated_pwr_demand_mw, acres
  # ---------------------------------------------------------------------------
  output$map_indiana <- renderLeaflet({
    map <- leaflet() |>
      addTiles(urlTemplate = DARK_TILE_URL, attribution = DARK_TILE_ATTR) |>
      setView(lng = -86.13, lat = 40.27, zoom = 7) |>
      addFullscreenControl() |>
      addScaleBar(position = "bottomleft")

    if (is.null(indiana_dc)) {
      return(
        map |>
          addControl(
            html = tags$div(
              style = "background:rgba(42,37,64,0.9); color:#f1f0f5; padding:12px 18px; border-radius:6px; font-family:'IBM Plex Mono',monospace; font-size:12px;",
              "\u26a0 Indiana data center CSV not found."
            ),
            position = "topright"
          )
      )
    }

    # ---- Indiana SVI county choropleth ----------------------------------------
    if (!is.null(svi_indiana_sf)) {
      svi_pal_in <- colorNumeric(
        palette  = c("#2d1b69", "#7c3aed", "#f59e0b", "#ef4444"),
        domain   = c(0, 1),
        na.color = "#333333"
      )

      svi_in_popup <- paste0(
        "<div style='font-family:IBM Plex Mono,monospace;font-size:12px;line-height:1.7;'>",
        "<strong style='font-size:13px;'>", svi_indiana_sf$county, " County</strong><br>",
        "<strong style='color:#f59e0b;'>Overall SVI Percentile:</strong> ",
        round(svi_indiana_sf$svi_rank * 100, 1), "<br>",
        "Socioeconomic: ",     round(svi_indiana_sf$svi_rank1 * 100, 1), "<br>",
        "Household Char.: ",   round(svi_indiana_sf$svi_rank2 * 100, 1), "<br>",
        "Minority Status: ",   round(svi_indiana_sf$svi_rank3 * 100, 1), "<br>",
        "Housing/Transport: ", round(svi_indiana_sf$svi_rank4 * 100, 1),
        "</div>"
      )

      map <- map |>
        addPolygons(
          data        = svi_indiana_sf,
          fillColor   = svi_pal_in(svi_indiana_sf$svi_rank),
          fillOpacity = 0.75,
          color       = "#0d0b14",
          weight      = 0.8,
          popup       = svi_in_popup,
          highlight   = highlightOptions(
            weight = 2, color = "#f59e0b",
            fillOpacity = 0.90, bringToFront = TRUE
          ),
          group = "SVI by County"
        ) |>
        addLegend(
          position  = "bottomright",
          pal       = svi_pal_in,
          values    = svi_indiana_sf$svi_rank,
          title     = "<span style='font-family:IBM Plex Mono,monospace;font-size:11px;'>Overall SVI<br>Percentile Rank</span>",
          labFormat = labelFormat(transform = function(x) round(x * 100)),
          opacity   = 0.88
        )
    }

    # ---- Proposed data center markers ----------------------------------------
    # Point color encodes project_status:
    #   Unknown / NA          → grey   (#9ca3af)
    #   Withdrawn /
    #   Temporarily Withdrawn → blue   (#3b82f6)
    #   Rumored               → yellow (#fde047)
    #   Proposed              → orange (#f97316)
    #   Under Construction    → red    (#ef4444)
    STATUS_COLORS <- c(
      "Unknown"              = "#9ca3af",
      "Withdrawn"            = "#3b82f6",
      "Temporarily Withdrawn"= "#3b82f6",
      "Rumored"              = "#fde047",
      "Proposed"             = "#f97316",
      "Under Construction"   = "#ef4444"
    )

    # Map each row's project_status to its color; fall back to grey if unrecognised.
    # unname() strips R's named-vector names so Leaflet receives a plain character
    # vector — without it, Leaflet ignores the values and renders black fills.
    dc_fill_colors <- unname(STATUS_COLORS[indiana_dc$project_status])
    dc_fill_colors[is.na(dc_fill_colors)] <- "#9ca3af"

    in_popup <- paste0(
      "<div style='font-family:IBM Plex Mono,monospace;font-size:12px;line-height:1.8;'>",
      "<strong style='font-size:13px;color:#f59e0b;'>", indiana_dc$name, "</strong><br>",
      "<strong>Owner:</strong> ", indiana_dc$owner, "<br>",
      "<strong>City:</strong> ", indiana_dc$city,
      " | <strong>County:</strong> ", indiana_dc$county, "<br>",
      "<strong>Status:</strong> ", indiana_dc$project_status, "<br>",
      "<strong>Expected Demand:</strong> ",
      ifelse(is.na(indiana_dc$anticipated_pwr_demand_mw), "N/A",
             paste0(indiana_dc$anticipated_pwr_demand_mw, " MW")), "<br>",
      "<strong>Utility:</strong> ", indiana_dc$electric_utility,
      "</div>"
    )

    # Build a manual HTML legend for project status colors
    status_legend_html <- tags$div(
      style = paste0(
        "background:rgba(26,22,37,0.92);",
        "border:1px solid #3d3560;",
        "border-radius:6px;",
        "padding:10px 14px;",
        "font-family:'IBM Plex Mono',monospace;",
        "font-size:11px;",
        "color:#f1f0f5;",
        "line-height:2;"
      ),
      tags$strong(style = "font-size:11px; letter-spacing:0.05em;",
                  "PROJECT STATUS"),
      tags$br(),
      tags$span(style = "display:inline-block;width:12px;height:12px;border-radius:50%;background:#ef4444;margin-right:6px;vertical-align:middle;"), "Under Construction", tags$br(),
      tags$span(style = "display:inline-block;width:12px;height:12px;border-radius:50%;background:#f97316;margin-right:6px;vertical-align:middle;"), "Proposed", tags$br(),
      tags$span(style = "display:inline-block;width:12px;height:12px;border-radius:50%;background:#fde047;margin-right:6px;vertical-align:middle;"), "Rumored", tags$br(),
      tags$span(style = "display:inline-block;width:12px;height:12px;border-radius:50%;background:#3b82f6;margin-right:6px;vertical-align:middle;"), "Withdrawn", tags$br(),
      tags$span(style = "display:inline-block;width:12px;height:12px;border-radius:50%;background:#9ca3af;margin-right:6px;vertical-align:middle;"), "Unknown"
    )

    map |>
      addCircleMarkers(
        lng          = indiana_dc$longitude,
        lat          = indiana_dc$latitude,
        radius       = 8,
        color        = "#ffffff",
        fillColor    = dc_fill_colors,
        fillOpacity  = 0.92,
        weight       = 1.5,
        opacity      = 1,
        popup        = in_popup,
        label        = indiana_dc$name,
        labelOptions = labelOptions(
          style     = list("font-family" = "IBM Plex Mono, monospace", "font-size" = "11px"),
          direction = "top", offset = c(0, -10)
        ),
        group = "Proposed Data Centers"
      ) |>
      addControl(
        html     = status_legend_html,
        position = "bottomleft"
      ) |>
      addLayersControl(
        overlayGroups = c("SVI by County", "Proposed Data Centers"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })


  # ---------------------------------------------------------------------------
  # CHART: Google water consumption bar chart (Home tab)
  # Top 10 locations by water consumption in millions of gallons
  # ---------------------------------------------------------------------------
  output$chart_water <- renderPlotly({
    water_data <- data.frame(
      location = c("Council Bluffs, IA", "Mayes County, OK", "Berkeley County, SC",
                   "Papillion, NE", "Douglas County, GA", "The Dalles, OR",
                   "New Albany, OH", "Lenoir, NC", "Montgomery County, TN", "Henderson, NV"),
      consumption = c(1010.2, 833.2, 776.5, 416.9, 366.9, 361.4, 352.7, 327.8, 321.8, 207.4),
      stringsAsFactors = FALSE
    )
    # Sort ascending so largest bar appears at top in horizontal chart
    water_data <- water_data[order(water_data$consumption), ]
    water_data$location <- factor(water_data$location, levels = water_data$location)

    plot_ly(
      data        = water_data,
      x           = ~consumption,
      y           = ~location,
      type        = "bar",
      orientation = "h",
      marker      = list(
        color = "#e34a33",
        line  = list(color = "#e34a33", width = 0)
      ),
      hovertemplate = "<b>%{y}</b><br>%{x:.1f}M gallons<extra></extra>"
    ) |>
      layout(
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin        = list(l = 10, r = 20, t = 10, b = 40),
        xaxis = list(
          title      = list(text = "Water Consumption (millions of gallons)",
                            font = list(family = "IBM Plex Mono", size = 11, color = "#a09ab8")),
          tickfont   = list(family = "IBM Plex Mono", size = 10, color = "#a09ab8"),
          gridcolor  = "#3d3560",
          zerolinecolor = "#3d3560"
        ),
        yaxis = list(
          title    = "",
          tickfont = list(family = "IBM Plex Mono", size = 11, color = "#f1f0f5"),
          gridcolor = "rgba(0,0,0,0)"
        ),
        hoverlabel = list(
          bgcolor   = "#2a2540",
          bordercolor = "#3d3560",
          font      = list(family = "IBM Plex Mono", size = 12, color = "#f1f0f5")
        )
      ) |>
      config(displayModeBar = FALSE)
  })

  # ---------------------------------------------------------------------------
  # CHART: Virginia poll 100% stacked bar (Home tab)
  # Data: Washington Post-Schar School poll, March 2026
  # Order: Comfortable | No Opinion | Uncomfortable — all positive, sums to 100%
  # ---------------------------------------------------------------------------
  output$chart_virginia_poll <- renderPlotly({

    years         <- c("2023", "2026")
    comfortable   <- c(69, 35)
    no_opinion    <- c(8, 6)
    uncomfortable <- c(24, 59)

    fig <- plot_ly() |>

      # Segment 1: Comfortable (leftmost)
      add_trace(
        x            = comfortable,
        y            = years,
        type         = "bar",
        orientation  = "h",
        name         = "Comfortable",
        marker       = list(color = "#b39ddb"),
        text         = paste0(comfortable, "%"),
        textposition = "inside",
        insidetextanchor = "middle",
        textfont     = list(family = "IBM Plex Mono", size = 12, color = "#ffffff"),
        hovertemplate = "<b>%{y}</b><br>Comfortable: %{x}%<extra></extra>"
      ) |>

      # Segment 2: No Opinion (middle)
      add_trace(
        x            = no_opinion,
        y            = years,
        type         = "bar",
        orientation  = "h",
        name         = "No opinion",
        marker       = list(color = "#888888"),
        text         = paste0(no_opinion, "%"),
        textposition = "inside",
        insidetextanchor = "middle",
        textfont     = list(family = "IBM Plex Mono", size = 11, color = "#ffffff"),
        hovertemplate = "<b>%{y}</b><br>No opinion: %{x}%<extra></extra>"
      ) |>

      # Segment 3: Uncomfortable (rightmost)
      add_trace(
        x            = uncomfortable,
        y            = years,
        type         = "bar",
        orientation  = "h",
        name         = "Uncomfortable",
        marker       = list(color = "#e34a33"),
        text         = paste0(uncomfortable, "%"),
        textposition = "inside",
        insidetextanchor = "middle",
        textfont     = list(family = "IBM Plex Mono", size = 12, color = "#ffffff"),
        hovertemplate = "<b>%{y}</b><br>Uncomfortable: %{x}%<extra></extra>"
      ) |>

      layout(
        barmode       = "stack",
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor  = "rgba(0,0,0,0)",
        margin        = list(l = 10, r = 10, t = 10, b = 10),
        legend = list(
          orientation = "h",
          x = 0, y = 1.25,
          font = list(family = "IBM Plex Mono", size = 11, color = "#a09ab8"),
          bgcolor = "rgba(0,0,0,0)",
          traceorder = "normal"
        ),
        xaxis = list(
          showticklabels = FALSE,
          showgrid       = FALSE,
          zeroline       = FALSE,
          range          = c(0, 101)
        ),
        yaxis = list(
          title    = "",
          tickfont = list(family = "IBM Plex Mono", size = 12, color = "#f1f0f5"),
          showgrid = FALSE
        ),
        hoverlabel = list(
          bgcolor     = "#2a2540",
          bordercolor = "#3d3560",
          font        = list(family = "IBM Plex Mono", size = 12, color = "#f1f0f5")
        )
      ) |>
      config(displayModeBar = FALSE)

    fig
  })

}  # end server


# =============================================================================
# SECTION 8: LAUNCH APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)
