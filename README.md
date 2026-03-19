# Gender Data Discovery Platform (GDDP)

Streamlit prototype for helping CSOs, advocates, and policy actors quickly discover, interpret, and use gender-related data for evidence-based action.

## Project Context

Many users in gender advocacy and policy work face recurring barriers:

- Difficulty finding up-to-date gender-related data quickly
- Uncertainty about the correct institution and latest source
- Heavy dependence on PDF or narrative-only resources
- Limited access to disaggregated, district-useful data
- Slow evidence workflows that do not match advocacy and policy timelines

This project responds to a high-urgency challenge identified through workshop and survey evidence.

## Mission

Build a **Streamlit prototype** that reduces friction in discovering and using gender-related resources.

The solution can evolve as:

- A dashboard
- A searchable catalog
- A hybrid dashboard + discovery tool

## What Good Looks Like

Our prototype should help users:

1. Find relevant resources quickly
2. Understand relevance through metadata
3. Access source links clearly
4. See quality caveats and limitations
5. Use outputs in at least one advocacy scenario

## Scope Boundary

- Use the baseline CSV inventory provided for a fast start
- Validate key resources directly from NISR links
- Do **not** bypass restricted systems or protected endpoints

## Team

- **Dan Munyaneza** - UI/UX Designer
- **Ngamije Didier** - Data Scientist (Team Leader)
- **Gatete Bugingo Jimmy** - Front-End Developer
- **Ishimwe Sibomana Christian** - Back-End Developer

## Proposed MVP Features

- Search and filter gender-related resources
- Metadata cards (source, date, geography, disaggregation level, topic)
- Quick-access source links
- Quality notes and caveats section per resource
- Simple advocacy output view (download/copy key summary points)

## Suggested Repository Structure

```text
GDRD/
  app.py
  data/
    baseline_inventory.csv
  src/
    data_loader.py
    filters.py
    scoring.py
    ui_components.py
  docs/
    advocacy_scenarios.md
  README.md
```

## Quick Start

1. Clone the repository
2. Create and activate a virtual environment
3. Install dependencies
4. Run the Streamlit app

Example:

```bash
pip install -r requirements.txt
streamlit run app.py
```

## First Milestones

- [ ] Load and inspect baseline CSV inventory
- [ ] Define metadata schema used in the app
- [ ] Build search and filter interface in Streamlit
- [ ] Add source validation references (NISR)
- [ ] Design one complete advocacy-use scenario
- [ ] Prepare demo flow for hackathon judging

## Hackathon Goal

Deliver a practical, user-friendly prototype that makes gender data easier to discover, trust, and use in real decision-making moments.
