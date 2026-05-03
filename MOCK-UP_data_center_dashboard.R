# =============================================================================
# Reckoning with Data Centers in the U.S.
# Interactive Shiny Dashboard
# Master's Research Project — Data Analysis & Visualization
# =============================================================================
#
# PACKAGE MANIFEST
# ─────────────────────────────────────────────────────────────────────────────
# shiny         :: Core reactive web application framework for R
# shinydashboard:: Provides dashboard layout primitives (sidebar, boxes, etc.)
# bslib         :: Bootstrap theming engine; used here for custom dark theme
#                  variables and fluid typography
# htmltools     :: Low-level HTML tag construction (tags$, HTML(), css())
# leaflet       :: R bindings for the Leaflet.js interactive mapping library;
#                  used for the placeholder US map panels
# dplyr         :: Grammar of data manipulation (filter, mutate, summarise)
# ggplot2       :: Grammar of graphics; produces all inline narrative charts
# plotly        :: Converts ggplot2 objects to interactive HTML widgets via
#                  ggplotly(); also used for standalone Plotly traces
# scales        :: Formatting helpers for ggplot2 axes (comma, dollar, percent)
# glue          :: String interpolation for readable dynamic text in UI
# =============================================================================

library(shiny)
library(bslib)
library(htmltools)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(glue)

# =============================================================================
# 0. THEME DEFINITION
# =============================================================================
# All color decisions live here. Touch this block to restyle the entire app.

COLORS <- list(
  bg_deep    = "#1a0f24",   # Page background — deep plum
  bg_card    = "#261535",   # Card / panel surfaces 
  bg_sidebar = "#1a0f24",   # Sidebar / navigation rail
  accent1    = "#f47b1a",   # Primary accent — ember orange
  accent2    = "#c8190f",   # Alert / highlight accent — crimson
  accent3    = "#f9e07e",   # Data accent — pale gold
  text_pri   = "#ffffff",   # Primary text — soft lavender-white
  text_sec   = "#8a6fa8",   # Secondary / muted text — muted plum
  border     = "#341e48",   # Subtle dividers
  chart_bg   = "#261535"    # ggplot / plotly panel background
)

# bslib custom theme — injected into every Bootstrap component
app_theme <- bs_theme(
  version       = 5,
  bg            = COLORS$bg_deep,
  fg            = COLORS$text_pri,
  primary       = COLORS$accent1,
  secondary     = COLORS$text_sec,
  base_font     = font_google("IBM Plex Sans"),
  heading_font  = font_google("Syne"),
  code_font     = font_google("IBM Plex Mono"),
  font_scale    = 1.0
)

# =============================================================================
# 1. MOCK DATA  (replace with real data imports in later iterations)
# =============================================================================

# Growth of US data centers over time
dc_growth <- tibble(
  year     = 2015:2024,
  count    = c(2800, 3100, 3500, 4000, 4700, 5600, 6200, 7100, 8300, 10000),
  power_gw = c(15,   18,   22,   27,   34,   43,   51,   61,   76,   100)
)

# Water consumption (billions of gallons per year)
water_data <- tibble(
  year   = 2018:2024,
  water_bg = c(626, 650, 671, 700, 740, 790, 860)
)

# Top states by data center count
top_states <- tibble(
  state = c("Virginia", "Texas", "California", "Illinois", "Georgia",
            "Arizona", "Ohio", "New York", "Washington", "Florida"),
  count = c(425, 312, 280, 190, 180, 165, 158, 145, 130, 122)
) |> arrange(desc(count))

# =============================================================================
# 2. SHARED GGPLOT2 THEME
# =============================================================================
# Applied to every chart for visual consistency across the narrative section.

theme_dc <- function() {
  theme_minimal(base_family = "IBM Plex Sans") +
    theme(
      plot.background    = element_rect(fill = COLORS$chart_bg,  color = NA),
      panel.background   = element_rect(fill = COLORS$chart_bg,  color = NA),
      panel.grid.major   = element_line(color = COLORS$border,   linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      text               = element_text(color = COLORS$text_pri),
      axis.text          = element_text(color = COLORS$text_sec, size = 10),
      axis.title         = element_text(color = COLORS$text_sec, size = 11),
      plot.title         = element_text(
                             color  = COLORS$text_pri,
                             family = "Syne",
                             size   = 14,
                             face   = "bold",
                             margin = margin(b = 6)
                           ),
      plot.subtitle      = element_text(color = COLORS$text_sec, size = 11,
                                        margin = margin(b = 12)),
      plot.caption       = element_text(color = COLORS$text_sec, size = 9,
                                        hjust = 0),
      legend.background  = element_rect(fill = COLORS$chart_bg,  color = NA),
      legend.text        = element_text(color = COLORS$text_sec),
      plot.margin        = margin(16, 16, 16, 16)
    )
}

# =============================================================================
# 3. HELPER: INLINE CHART BUILDERS
# =============================================================================

make_growth_chart <- function() {
  p <- ggplot(dc_growth, aes(x = year, y = count)) +
    geom_area(fill = COLORS$accent1, alpha = 0.18) +
    geom_line(color = COLORS$accent1, linewidth = 1.4) +
    geom_point(color = COLORS$accent1, size = 3) +
    scale_x_continuous(breaks = 2015:2024) +
    scale_y_continuous(labels = comma) +
    labs(
      title    = "U.S. Data Center Count, 2015–2024",
      subtitle = "Estimated number of hyperscale and colocation facilities",
      caption  = "Source: Statista / Synergy Research Group (illustrative)"
    ) +
    theme_dc()

  ggplotly(p, tooltip = c("x", "y")) |>
    layout(
      paper_bgcolor = COLORS$chart_bg,
      plot_bgcolor  = COLORS$chart_bg,
      font          = list(color = COLORS$text_pri)
    ) |>
    config(displayModeBar = FALSE)
}

make_power_chart <- function() {
  p <- ggplot(dc_growth, aes(x = year, y = power_gw)) +
    geom_col(fill = COLORS$accent2, alpha = 0.85, width = 0.6) +
    scale_x_continuous(breaks = 2015:2024) +
    scale_y_continuous(labels = function(x) paste0(x, " GW")) +
    labs(
      title    = "Estimated U.S. Data Center Power Demand",
      subtitle = "Annual aggregate electricity consumption in gigawatts",
      caption  = "Source: IEA / Lawrence Berkeley National Laboratory (illustrative)"
    ) +
    theme_dc()

  ggplotly(p, tooltip = c("x", "y")) |>
    layout(
      paper_bgcolor = COLORS$chart_bg,
      plot_bgcolor  = COLORS$chart_bg,
      font          = list(color = COLORS$text_pri)
    ) |>
    config(displayModeBar = FALSE)
}

make_states_chart <- function() {
  p <- ggplot(top_states, aes(x = reorder(state, count), y = count)) +
    geom_col(fill = COLORS$accent3, alpha = 0.85, width = 0.65) +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    labs(
      title    = "Top 10 States by Data Center Count",
      subtitle = "Virginia's dominance reflects the Northern Virginia data center corridor",
      caption  = "Source: Data Center Map / CBRE Research (illustrative)"
    ) +
    theme_dc() +
    theme(axis.title = element_blank())

  ggplotly(p, tooltip = c("y", "x")) |>
    layout(
      paper_bgcolor = COLORS$chart_bg,
      plot_bgcolor  = COLORS$chart_bg,
      font          = list(color = COLORS$text_pri)
    ) |>
    config(displayModeBar = FALSE)
}

# =============================================================================
# 4. CSS — CUSTOM STYLES
# =============================================================================
# Injected into <head> via tags$style(); keeps all design tokens in one place.

custom_css <- tags$style(HTML(glue('

  /* ── Root & body ───────────────────────────────────────────────────────── */
  :root {{
    --bg-deep:    {COLORS$bg_deep};
    --bg-card:    {COLORS$bg_card};
    --bg-sidebar: {COLORS$bg_sidebar};
    --accent1:    {COLORS$accent1};
    --accent2:    {COLORS$accent2};
    --accent3:    {COLORS$accent3};
    --text-pri:   {COLORS$text_pri};
    --text-sec:   {COLORS$text_sec};
    --border:     {COLORS$border};
  }}

  body, .tab-content {{
    background-color: var(--bg-deep) !important;
    color: var(--text-pri) !important;
  }}

  /* ── Navigation bar ────────────────────────────────────────────────────── */
  .navbar {{
    background-color: var(--bg-sidebar) !important;
    border-bottom: 1px solid var(--border) !important;
    padding: 0.6rem 1.5rem;
  }}
  .navbar-brand {{
    font-family: "Syne", sans-serif;
    font-weight: 800;
    font-size: 1.05rem;
    color: var(--text-pri) !important;
    letter-spacing: 0.01em;
  }}
  .nav-link {{
    color: var(--text-sec) !important;
    font-size: 0.87rem;
    letter-spacing: 0.04em;
    text-transform: uppercase;
    padding: 0.55rem 1rem !important;
    transition: color 0.2s;
  }}
  .nav-link:hover, .nav-link.active {{
    color: var(--accent1) !important;
  }}
  .nav-link.active {{
    border-bottom: 2px solid var(--accent1) !important;
    background: transparent !important;
  }}

  /* ── Page wrapper ───────────────────────────────────────────────────────── */
  .page-content {{
    max-width: 860px;
    margin: 0 auto;
    padding: 2.5rem 1.5rem 4rem;
  }}
  .page-content-wide {{
    max-width: 1100px;
    margin: 0 auto;
    padding: 2rem 1.5rem 4rem;
  }}

  /* ── Article / narrative typography ────────────────────────────────────── */
  .article-kicker {{
    font-family: "IBM Plex Mono", monospace;
    font-size: 0.75rem;
    letter-spacing: 0.15em;
    text-transform: uppercase;
    color: var(--accent1);
    margin-bottom: 0.6rem;
  }}
  .article-headline {{
    font-family: "Syne", sans-serif;
    font-size: clamp(1.9rem, 4vw, 2.8rem);
    font-weight: 800;
    line-height: 1.15;
    color: var(--text-pri);
    margin-bottom: 1rem;
  }}
  .article-dek {{
    font-size: 1.15rem;
    line-height: 1.6;
    color: var(--text-sec);
    border-left: 3px solid var(--accent1);
    padding-left: 1rem;
    margin-bottom: 2rem;
  }}
  .article-byline {{
    font-family: "IBM Plex Mono", monospace;
    font-size: 0.78rem;
    color: var(--text-sec);
    letter-spacing: 0.06em;
    margin-bottom: 2.5rem;
    padding-bottom: 1.5rem;
    border-bottom: 1px solid var(--border);
  }}
  .article-body h2 {{
    font-family: "Syne", sans-serif;
    font-size: 1.4rem;
    font-weight: 700;
    color: var(--text-pri);
    margin: 2.5rem 0 0.8rem;
  }}
  .article-body h3 {{
    font-family: "Syne", sans-serif;
    font-size: 1.1rem;
    font-weight: 600;
    color: var(--accent1);
    margin: 2rem 0 0.6rem;
  }}
  .article-body p {{
    font-size: 1.02rem;
    line-height: 1.8;
    color: var(--text-pri);
    margin-bottom: 1.2rem;
  }}

  /* ── Pull quote ────────────────────────────────────────────────────────── */
  .pull-quote {{
    font-family: "Syne", sans-serif;
    font-size: 1.35rem;
    font-weight: 700;
    line-height: 1.45;
    color: var(--accent1);
    border-left: 4px solid var(--accent2);
    padding: 1rem 1.5rem;
    margin: 2rem 0;
    background: rgba(155, 114, 207, 0.07);
    border-radius: 0 6px 6px 0;
  }}

  /* ── Stat callout cards ─────────────────────────────────────────────────── */
  .stat-grid {{
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(170px, 1fr));
    gap: 1rem;
    margin: 2rem 0;
  }}
  .stat-card {{
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 1.2rem 1rem;
    text-align: center;
  }}
  .stat-card .stat-number {{
    font-family: "Syne", sans-serif;
    font-size: 2rem;
    font-weight: 800;
    color: var(--accent1);
    display: block;
    line-height: 1;
  }}
  .stat-card .stat-label {{
    font-size: 0.78rem;
    color: var(--text-sec);
    text-transform: uppercase;
    letter-spacing: 0.08em;
    margin-top: 0.5rem;
    display: block;
  }}
  .stat-card.red   .stat-number {{ color: var(--accent2); }}
  .stat-card.teal  .stat-number {{ color: var(--accent3); }}

  /* ── Chart container ────────────────────────────────────────────────────── */
  .chart-container {{
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 1.2rem;
    margin: 2rem 0;
  }}
  .chart-label {{
    font-family: "IBM Plex Mono", monospace;
    font-size: 0.72rem;
    letter-spacing: 0.1em;
    text-transform: uppercase;
    color: var(--text-sec);
    margin-bottom: 0.4rem;
  }}

  /* ── Placeholder panels (Tab 1–4) ──────────────────────────────────────── */
  .placeholder-header {{
    padding: 2rem 1.5rem 1.2rem;
    border-bottom: 1px solid var(--border);
    margin-bottom: 1.5rem;
  }}
  .placeholder-header h1 {{
    font-family: "Syne", sans-serif;
    font-size: 1.7rem;
    font-weight: 800;
    color: var(--text-pri);
    margin-bottom: 0.3rem;
  }}
  .placeholder-header p {{
    color: var(--text-sec);
    font-size: 0.92rem;
    margin: 0;
  }}
  .placeholder-badge {{
    display: inline-block;
    font-family: "IBM Plex Mono", monospace;
    font-size: 0.68rem;
    letter-spacing: 0.12em;
    text-transform: uppercase;
    color: var(--accent2);
    border: 1px solid var(--accent2);
    border-radius: 3px;
    padding: 2px 7px;
    margin-bottom: 0.6rem;
  }}

  /* ── Leaflet map ─────────────────────────────────────────────────────────── */
  .leaflet-container {{
    background: var(--bg-deep) !important;
    border-radius: 6px;
  }}

  /* ── Methodology & References ────────────────────────────────────────────── */
  .method-section {{
    background: var(--bg-card);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 1.2rem;
  }}
  .method-section h3 {{
    font-family: "Syne", sans-serif;
    font-size: 1rem;
    font-weight: 700;
    color: var(--accent1);
    margin-bottom: 0.6rem;
  }}
  .method-section p {{
    color: var(--text-sec);
    font-size: 0.92rem;
    line-height: 1.7;
    margin: 0;
  }}
  .ref-link {{
    color: var(--accent1) !important;
    text-decoration: underline;
    text-underline-offset: 3px;
  }}
  .ref-link:hover {{ color: var(--accent3) !important; }}

  /* ── Footer rule ─────────────────────────────────────────────────────────── */
  .site-footer {{
    text-align: center;
    padding: 2rem;
    color: var(--text-sec);
    font-size: 0.78rem;
    font-family: "IBM Plex Mono", monospace;
    letter-spacing: 0.05em;
    border-top: 1px solid var(--border);
    margin-top: 3rem;
  }}
')))

# =============================================================================
# 5. UI — PAGE NAVBAR STRUCTURE
# =============================================================================

ui <- page_navbar(
  title  = "Reckoning with the U.S. Data Center Boom",
  theme  = app_theme,
  header = custom_css,
  footer = tags$div(class = "site-footer",
    "Danielle Stemper · Pratt Institute · 2026"
  ),

  # ── HOMEPAGE ───────────────────────────────────────────────────────────────
  nav_panel(
    title = "Home",

    div(class = "page-content",

      # Kicker + Headline + Dek
      div(class = "article-kicker", "Investigating the Data Center Boom"),
      div(class = "article-headline",
        "Reckoning with Data Centers in the U.S."
      ),
      div(class = "article-dek",
        "As Big Tech pushes AI adoption, companies like Meta, Microsoft, and 
        Amazon are racing to build the physical infrastructure to support the 
        unprecedented computing power needed to support it. This is a geographic 
        analysis of the socioeconomic factors – and costs – of the U.S. data 
        center boom, and exploration of how the public is pushing back."
      ),
      div(class = "article-byline",
        "By Danielle Stemper · Advanced Projects in Visualization · 2026"
      ),

      # ── Article body ──────────────────────────────────────────────────────
      div(class = "article-body",

        tags$h2("The Physical Infrastructure of Everything Digital"),
        tags$p(
          "Every search query, every streamed video, every AI-generated image
           passes through a data center. These facilities —
           massive warehouses filled with racks of servers humming at near-constant capacity —
           have become the unseen substrate of modern digital life. In 2024,
           the United States was estimated to host more than XX data center
           facilities, more than any other country in the world."
        ),
        tags$p(
          "But the data center boom has accelerated to a pace that is drawing
           scrutiny from utility regulators, environmental advocates, and
           local governments alike. Driven by the explosive demand for
           cloud computing, artificial intelligence, and streaming media,
           the sector's electricity consumption is now a measurable and
           growing share of the national grid."
        ),

        # Stat callout cards
        div(class = "stat-grid",
          div(class = "stat-card",
            tags$span(class = "stat-number", "1,500+"),
            tags$span(class = "stat-label", "U.S. Data Centers (2024)")
          ),
          div(class = "stat-card red",
            tags$span(class = "stat-number", "~3%"),
            tags$span(class = "stat-label", "Average U.S. Electricity Price")
          ),
          div(class = "stat-card teal",
            tags$span(class = "stat-number", "860B"),
            tags$span(class = "stat-label", "Gallons of Water / Year")
          ),
          div(class = "stat-card",
            tags$span(class = "stat-number", "$200B+"),
            tags$span(class = "stat-label", "Annual Global Investment")
          )
        ),

        tags$h2("Years of Accelerating Growth"),
        tags$p(
          "The chart below illustrates the trajectory of U.S. data center
           expansion since 2015. What had been a steady climb became a
           near-vertical ascent between 2022 and 2024, corresponding with
           the mass adoption of generative AI tools and the scaling of
           hyperscale cloud platforms operated by Amazon, Microsoft, and Google."
        ),

        # Chart 1 — Growth over time
        div(class = "chart-container",
          div(class = "chart-label", "Figure 1 · Facility Count"),
          plotlyOutput("chart_growth", height = "320px")
        ),

        tags$h2("The Energy Equation"),
        tags$p(
          "Data centers are among the most energy-intensive building types
           ever constructed. A single hyperscale campus can consume as much
           electricity as a small city. As AI workloads — which require
           dense arrays of specialized graphics processors running at
           maximum utilization for extended periods — grow as a proportion
           of total compute demand, the energy intensity per unit of
           useful output has actually increased, reversing gains made
           by efficiency improvements in previous years."
        ),

        div(class = "pull-quote",
          "\u201cThe coming wave of AI infrastructure is already stressing
           grids that were not designed to absorb it.\u201d"
        ),

        # Chart 2 — Power demand
        div(class = "chart-container",
          div(class = "chart-label", "Figure 2 · Aggregate Power Demand"),
          plotlyOutput("chart_power", height = "300px")
        ),

        tags$h2("Geography of Concentration"),
        tags$p(
          "The United States' data center footprint is not evenly distributed.
           Northern Virginia — particularly the county of Loudoun, nicknamed
           'Data Center Alley' — is widely regarded as the world's largest
           data center market. The concentration of fiber infrastructure,
           favorable tax incentives, and proximity to Washington D.C.
           have made the region a magnet for facility development.
           But as land and power become constrained, development is spreading
           to secondary and tertiary markets across the Sun Belt and Midwest."
        ),

        # Chart 3 — State rankings
        div(class = "chart-container",
          div(class = "chart-label", "Figure 3 · State Rankings"),
          plotlyOutput("chart_states", height = "360px")
        ),

        tags$h2("Water: The Essential Resource"),
        tags$p(
          "Beyond electricity, data centers are heavy consumers of water.
           Cooling towers and evaporative systems — the dominant
           thermal management approach in large facilities — can consume
           millions of gallons per day. In drought-prone regions of the
           American West, including Arizona, Nevada, and parts of Texas,
           this demand is drawing increasingly pointed criticism
           from local water authorities and environmental groups."
        ),
        tags$p(
          "This project seeks to map, quantify, and contextualize
           the full geographic and infrastructural footprint of
           U.S. data centers — examining not just where they are,
           but what they cost the communities and ecosystems
           that surround them."
        ),

        tags$h2("Navigating This Project"),
        tags$p(
          "The interactive maps in the tabs above (currently in development)
           will allow readers to explore data center locations, local energy
           and water impacts, and community-level demographic context.
           Each map is accompanied by explanatory text and linked to
           the full methodology."
        )
      ) # /article-body
    )   # /page-content
  ),    # /nav_panel Home

  # ── TAB 1 ─────────────────────────────────────────────────────────────────
  nav_panel(
    title = "Explore",
    div(class = "page-content-wide",
      div(class = "placeholder-header",
        div(class = "placeholder-badge", "Coming Soon"),
        tags$h1("Data Center Locations & Density"),
        tags$p(
          "This map will display the geographic distribution and
           concentration of data center facilities across the United States."
        )
      ),
      leafletOutput("map1", height = "520px")
    )
  ),

  # ── CASE STUDY ─────────────────────────────────────────────────────────────────
  nav_panel(
    title = "Case Study: Indiana",
    div(class = "page-content-wide",
      div(class = "placeholder-header",
        div(class = "placeholder-badge", "Coming Soon"),
        tags$h1("Proposed Data Center Locations & Population Vulnerability"),
        tags$p(
          "This map will visualize proposed data center locations in Indiana 
          against the Center for Disease Control's Population Vulnerability Index."
        )
      ),
      leafletOutput("map2", height = "520px")
    )
  ),

  # ── TAB 3 ─────────────────────────────────────────────────────────────────
  nav_panel(
    title = "Energy Price Volatility",
    div(class = "page-content-wide",
      div(class = "placeholder-header",
        div(class = "placeholder-badge", "Coming Soon"),
        tags$h1("Are Citizens Footing the Bill for Data Centers?"),
        tags$p(
          "This map will chart residential energy price volatility 2020-2024. 
          Will briefly discuss method behind the map (standard dev = 
          price volatility) using data from the U.S. Energy Information Adminstration"
        )
      ),
      leafletOutput("map3", height = "520px")
    )
  ),

  # ── TAB 4 ─────────────────────────────────────────────────────────────────
  nav_panel(
    title = "Map 4",
    div(class = "page-content-wide",
      div(class = "placeholder-header",
        div(class = "placeholder-badge", "Coming Soon"),
        tags$h1("Community & Demographic Context"),
        tags$p(
          "This map will examine proximity of data center facilities
           to residential communities, with demographic and environmental
           justice indicators."
        )
      ),
      leafletOutput("map4", height = "520px")
    )
  ),

  # ── METHODOLOGY ───────────────────────────────────────────────────────────
  nav_panel(
    title = "Methodology",
    div(class = "page-content",
      div(class = "article-kicker", "Research Process"),
      div(class = "article-headline", style = "font-size: 2rem;",
        "Methodology"
      ),
      tags$p(
        style = "color: var(--text-sec); margin-bottom: 2rem;",
        "This section will document the full analytical methodology
         underpinning each map and visualization in this project.
         Placeholder sections are included below."
      ),

      div(class = "method-section",
        tags$h3("Data Sources"),
        tags$p(
          "[Placeholder] This section will describe all primary and secondary
           data sources used, including government datasets, commercial
           databases, and journalistic records. Sources will be cited
           in full with access dates and any known limitations."
        )
      ),
      div(class = "method-section",
        tags$h3("Data Cleaning & Processing"),
        tags$p(
          "[Placeholder] This section will document the data wrangling
           pipeline — including how raw records were deduplicated,
           geocoded, and joined across sources — with reproducible
           R code available in the linked GitHub repository."
        )
      ),
      div(class = "method-section",
        tags$h3("Geographic Analysis"),
        tags$p(
          "[Placeholder] This section will explain the spatial analysis
           methods used, including kernel density estimation for the
           facility concentration maps and buffer-zone analysis for
           community proximity calculations."
        )
      ),
      div(class = "method-section",
        tags$h3("Map Design Decisions"),
        tags$p(
          "[Placeholder] This section will address design choices in
           the interactive maps — including classification schemes,
           color scales, and projection selections — along with
           rationale for each."
        )
      ),
      div(class = "method-section",
        tags$h3("Limitations & Caveats"),
        tags$p(
          "[Placeholder] This section will clearly state the limitations
           of the data and analysis, including gaps in public records,
           definitional inconsistencies across sources, and areas
           of analytical uncertainty."
        )
      )
    )
  ),

  # ── REFERENCES ────────────────────────────────────────────────────────────
  nav_panel(
    title = "References",
    div(class = "page-content",
      div(class = "article-kicker", "Sources & Code"),
      div(class = "article-headline", style = "font-size: 2rem;",
        "References"
      ),
      tags$p(
        style = "color: var(--text-sec); margin-bottom: 2rem;",
        "All data, code, and documentation for this project are
         openly available. Full citations will be added as the
         project develops."
      ),

      div(class = "method-section",
        tags$h3("GitHub Repository"),
        tags$p(
          "All data files, cleaning scripts, and Shiny application code
           are hosted publicly on GitHub. ",
          tags$a(
            href   = "https://github.com/[your-username]/[repo-name]",
            target = "_blank",
            class  = "ref-link",
            "github.com"
          )
        )
      ),
      div(class = "method-section",
        tags$h3("Sources (Placeholder)"),
        tags$p(
          "[Placeholder] U.S. Energy Information Administration (EIA),
           Environmental Protection Agency (EPA), U.S. Census Bureau,
           Internet Archive / Data Center Map, and other primary
           government and industry sources will be listed here
           with full citation details."
        )
      ),
      div(class = "method-section",
        tags$h3("R Packages"),
        tags$p(
          "This dashboard was built using R and the following packages:
           shiny, bslib, htmltools, leaflet, dplyr, ggplot2, plotly,
           scales, and glue. Full version information is available
           in the GitHub repository's renv.lock file."
        )
      )
    )
  )
)

# =============================================================================
# 6. SERVER
# =============================================================================

server <- function(input, output, session) {

  # ── Narrative charts ───────────────────────────────────────────────────────
  output$chart_growth <- renderPlotly({ make_growth_chart() })
  output$chart_power  <- renderPlotly({ make_power_chart()  })
  output$chart_states <- renderPlotly({ make_states_chart() })

  # ── Shared map factory — renders a minimal styled US base map ─────────────
  make_us_map <- function() {
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      setView(lng = -96, lat = 38, zoom = 4) |>
      addProviderTiles(
        "CartoDB.DarkMatter",                 # Dark basemap matching theme
        options = providerTileOptions(opacity = 0.9)
      )
  }

  output$map1 <- renderLeaflet({ make_us_map() })
  output$map2 <- renderLeaflet({ make_us_map() })
  output$map3 <- renderLeaflet({ make_us_map() })
  output$map4 <- renderLeaflet({ make_us_map() })
}

# =============================================================================
# 7. RUN
# =============================================================================

shinyApp(ui = ui, server = server)
