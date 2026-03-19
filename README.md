# GDDP — Gender Data Discovery Platform

> **Live App →** [https://didier-ngamije.shinyapps.io/GDRD/](https://didier-ngamije.shinyapps.io/GDRD/)

---

```
╔══════════════════════════════════════════════════════════════════════╗
║   GIZ Gender Data Resources Discovery Hackathon 2026                ║
║   19–20 March 2026  ·  GIZ Digital Technical Center, Kigali         ║
║   Track: Civil Society & Advocacy Tools                              ║
╚══════════════════════════════════════════════════════════════════════╝
```

---

## What Is GDDP?

**GDDP (Gender Data Discovery Platform)** is a web application built during
the GIZ Gender Data Resources Discovery Hackathon 2026. It is designed for
**civil society organisations, policy advocates, journalists, and researchers**
working under Rwanda's **FAST program** (Feminism in Action for Structural
Transformation).

The platform solves one specific, high-urgency problem:

> *Rwanda produces gender-disaggregated data every year through 73+ national
> surveys — but that data remains invisible to the people who need it most.*

GDDP brings all of it into one place, makes it searchable in seconds, and
surfaces quality signals and relevance scores so advocates can trust what they
find.

---

## The Problem We Are Solving

Through GIZ workshops and surveys with CSOs and policy actors, three
recurring pain points were consistently reported:

| # | Pain Point | Impact |
|---|-----------|--------|
| 1 | No unified discovery interface for NISR gender data | Hours lost searching scattered repositories |
| 2 | PDF-heavy, narrative-only outputs | Data hard to extract and re-use |
| 3 | No quality signal or relevance rating | Users cannot tell good data from bad |
| 4 | No disaggregated, district-level view | National aggregates miss local realities |
| 5 | Advocacy cycles move faster than current workflows | Evidence arrives too late to influence decisions |

GDDP was selected as the hackathon challenge because it has the **highest
immediate value** for evidence-based advocacy — and the highest feasibility
for a 48-hour sprint.

---

## Our Solution

GDDP is a **pure R Shiny application** — no dashboarding framework, fully
custom-designed — combining three tools in one:

### 1. Google-Style Search Engine
A full-text search engine over 73 NISR national surveys. Type any keyword
(topic, series name, year, organisation) and the engine matches against
titles, abstracts, and scope notes. Results appear in a familiar
Google-style layout:

- **Green URL breadcrumb** — `microdata.statistics.gov.rw › catalog › Series`
- **Clickable study title** — opens a rich detail modal
- **Colour-coded chips** — year, survey series, gender relevance score, access type, quality status
- **Highlighted snippet** — abstract excerpt with matching keywords bolded

### 2. Advanced Filters
A collapsible filter layer appears after search:
- Sort by year, gender relevance, views, or title
- Year range slider (1978–2024)
- Gender relevance score slider (0–10)
- Survey series checkboxes (DHS, EICV, Census, LFS, Agriculture, FinScope …)
- Data quality and access type filters

### 3. Gender Data Dashboard
A comprehensive analytics layer visualising Rwanda's gender landscape:

| Section | Charts |
|---------|--------|
| Survey Landscape | Studies by year, access type donut, series distribution, top-10 most accessed |
| Gender Coverage | Avg. relevance score by series, topic radar (DHS · EICV · RLFS · Census), quality stack |
| Rwanda Gender Indicators | Maternal mortality, skilled birth attendance, CPR, under-5 mortality, stunting, HIV, education parity, labour force participation, provincial household structure |
| Quality Intelligence | Missing metadata fields, resource completeness |

---

## Data Architecture

```
Data/
├── studies.csv           # 73 NISR national surveys (metadata)
├── study_resources.csv   # 387 downloadable files linked to studies
└── quality_report.csv    # Metadata completeness audit per study
```

All three files are merged at startup on `study_id`. The platform enriches
each study with:

| Derived Field | Logic |
|---------------|-------|
| `collection` | Rule-based classifier → DHS, EICV, Census, LFS, Agriculture, FinScope … |
| `gender_score` | 0–10 keyword relevance score across title, abstract, scope notes |
| `quality_status` | Complete / Minor Issues / Incomplete (based on missing field count) |
| `access_clean` | Public / Licensed / Other (normalised from `data_access_type`) |
| `abstract_card` | Truncated 280-char snippet for display |

---

## Tech Stack

| Layer | Technology |
|-------|-----------|
| Framework | R Shiny (pure — no shinydashboard) |
| Data wrangling | dplyr, tidyr, readr, stringr |
| Visualisations | Plotly (fully interactive, custom themed) |
| Styling | Custom CSS design system (`www/styles.css`) with CSS variables |
| Fonts | Playfair Display · Lora · Dancing Script · DM Sans (Google Fonts) |
| Icons | Font Awesome 5 |
| Deployment | shinyapps.io |

---

## Repository Structure

```
GDRD/
├── app.R                  # Entry point — sources all modules, runs shinyApp()
├── requirements.R         # Package installer
├── README.md
│
├── R/
│   ├── constants.R        # CLR colour palette (mirrors CSS variables for Plotly)
│   ├── reference_data.R   # Curated Rwanda gender indicator data frames
│   ├── data_load.R        # CSV ingestion, merging, feature engineering
│   ├── helpers.R          # build_search_result(), gender_bar_html(), gddp_theme() …
│   ├── ui.R               # Full UI definition (zero inline CSS)
│   └── server.R           # All reactive logic, outputs, modal observers
│
├── Data/
│   ├── studies.csv
│   ├── study_resources.csv
│   └── quality_report.csv
│
└── www/
    ├── styles.css         # Full design system (CSS variables, components, layout)
    ├── logo.png           # GIZ / GDDP brand logo
    ├── Femin.png          # Feminism in Action mark
    ├── footer.jpg         # Footer background image
    ├── problem.png        # Home tab PMV illustration
    ├── mission.png
    └── vision.png
```

---

## Design System

The UI is governed by a single CSS file (`www/styles.css`) built around
CSS custom properties:

```css
:root {
  --color-primary-500 : #d62027;   /* vivid red — brand */
  --color-secondary   : #d86367;   /* coral-red */
  --font-display      : 'Playfair Display', serif;
  --font-script       : 'Dancing Script', cursive;
  --font-body         : 'Lora', serif;
  --font-ui           : 'DM Sans', sans-serif;
  /* … 60+ more tokens */
}
```

Key design principles:
- **White canvas** — `#ffffff` background throughout
- **Red brand accents** — primary `#d62027`, coral `#d86367`, blush `#f8d6c6`
- **Corner blobs** — decorative organic shapes fixed in viewport corners
- **Brushstroke highlights** — SVG-based highlight effect on all section headings
- **Glass footer** — `footer.jpg` with `backdrop-filter: blur(18px)` frosted glass panel

---

## Running Locally

```r
# 1. Install dependencies
source("requirements.R")

# 2. Launch the app
shiny::runApp()
```

Or open `app.R` in RStudio and click **Run App**.

**R version:** 4.3+ recommended  
**Required packages:** shiny, dplyr, tidyr, readr, stringr, plotly

---

## The Team

| Role | Name |
|------|------|
| Data Science · Team Lead | **Ngamije Didier** |
| UI / UX Designer | **Dan Munyaneza** |
| Front-End Developer | **Gatete Bugingo Jimmy** |
| Back-End Developer | **Ishimwe Sibomana Christian** |

---

## Hackathon Context

**Event:** GIZ Gender Data Resources Discovery Hackathon 2026  
**Date:** 19–20 March 2026  
**Venue:** GIZ Digital Technical Center, Kigali, Rwanda  
**Program:** FAST — Feminism in Action for Structural Transformation  
**Challenge:** Build a digital tool that makes Rwanda's gender data more
visible, accessible, and actionable for civil society actors.

---

> *"A Rwanda where every gender advocate finds the data they need in under
> a minute, understands its quality at a glance, and uses it immediately
> to drive structural transformation."*
>
> — GDDP Vision Statement
