# What Predicts AI Usage Across Countries?

Patterns in the Anthropic Economic Index

> **This is a vibe-coded research note.** It was mostly Claude-generated and put together over a weekend. I don't know what I'm going to do with it — it might become a paper, stay a blog post, or go nowhere. Treat it accordingly.

## Overview

This project analyzes Anthropic's Economic Index data alongside World Bank development indicators, survey data, and other sources to understand patterns in AI adoption at the country level. The analysis is exploratory—pattern-finding rather than hypothesis testing.

**Key patterns observed:**

- **GDP per capita** and **internet access** explain most of the variance in adoption
- **Fertility rate** has the strongest marginal effect after controlling for economics—countries with lower fertility use AI more
- **Automation vs. augmentation** patterns vary by development level: lower-income countries use Claude more for task replacement (r = -0.83 with GDP)
- **Survey data** on AI enthusiasm correlates *negatively* with actual usage—skeptical rich countries outadopt enthusiastic developing ones

**Limitations:** Single cross-section (n≈127), one product from one company, cross-country regressions are noisy. See blog post for full discussion.

## Repository Structure

```
viz-playground/
├── generate_html.R          # Main analysis script (generates blog_post.html)
├── blog_post.html           # Output: the analysis write-up
├── data/
│   ├── anthropic/           # Anthropic Economic Index data
│   │   └── aei_geo.csv      # Main AEI dataset (26MB)
│   ├── worldbank/           # World Bank WDI indicators
│   │   └── API_*.csv        # Development indicators (see Data Sources)
│   ├── surveys/             # Survey data on AI attitudes
│   │   ├── ai_usage_survey.csv    # BCG ChatGPT adoption survey
│   │   ├── ai_perception.csv      # Ipsos AI attitudes survey
│   │   └── ai_readiness.csv       # Government AI readiness
│   └── rti_country_specific_survey_predicted.csv  # RTI from Lewandowski et al.
├── figures/                 # Figures used in blog post
│   ├── gdp_residuals.png           # Residuals from GDP model
│   ├── gdp_vs_automation.png       # Automation rate by GDP
│   ├── map_automation.png          # World map: automation vs augmentation
│   ├── map_residuals.png           # World map: over/under-performers vs GDP
│   ├── map_usage.png               # World map: raw usage per capita
│   ├── lasso_variable_importance.png
│   ├── shapley_decomposition.png
│   ├── perception_quadrant.png
│   └── survey_vs_actual_usage.png
├── scripts/                 # Exploratory scripts (not required for reproduction)
│   ├── create_world_map.R          # Generates world maps
│   ├── explore_ict_exports.R       # ICT services exporter analysis
│   ├── explore_rti_growth.R        # RTI and GDP growth exploration
│   ├── check_automation_means.R    # Regional automation means
│   └── [other exploratory scripts]
├── output/                  # Archived/intermediate outputs
├── clauded.md               # Tone guidance notes
└── README.md
```

## Reproduction

### Requirements

R 4.0+ with packages:
```r
install.packages(c(
  "tidyverse",    # Data manipulation and visualization
  "glmnet",       # LASSO regression
  "broom",        # Tidy model outputs
  "sf",           # Spatial data (for maps)
  "rnaturalearth", # World map geometries (for maps)
  "rnaturalearthdata",
  "showtext"      # Custom fonts (for maps)
))
```

### Running the main analysis

```bash
Rscript generate_html.R
```

This will:
1. Load Anthropic Economic Index data
2. Merge with World Bank development indicators
3. Run OLS regressions (incremental model building)
4. Run LASSO regression with 10-fold CV
5. Compute Shapley R² decomposition
6. Generate figures
7. Output `blog_post.html` with all tables and analysis

### Generating world maps (optional)

```bash
Rscript scripts/create_world_map.R
```

Outputs `figures/map_automation.png`, `map_residuals.png`, `map_usage.png`.

## Data Sources

### Primary Data

| Source | Description | File | Access |
|--------|-------------|------|--------|
| [Anthropic Economic Index](https://www.anthropic.com/research/anthropic-economic-index-september-2025-report) | Claude usage by country, occupation, task type | `data/anthropic/aei_geo.csv` | Public release |
| [World Bank WDI](https://databank.worldbank.org/source/world-development-indicators) | Development indicators | `data/worldbank/API_*.csv` | Public |

### World Bank Indicators Used

| Indicator Code | Variable | Description |
|----------------|----------|-------------|
| NY.GDP.PCAP.CD | GDP per capita | Current USD |
| IT.NET.USER.ZS | Internet access | % of population |
| RQ.EST | Regulatory quality | WGI estimate |
| SP.DYN.TFRT.IN | Fertility rate | Births per woman |
| SP.POP.65UP.TO.ZS | Population 65+ | % of total |
| GB.XPD.RSDV.GD.ZS | R&D spending | % of GDP |
| IT.CEL.SETS.P2 | Mobile subscriptions | Per 100 people |
| SP.DYN.LE00.IN | Life expectancy | Years at birth |
| BX.GSR.CCIS.ZS | ICT service exports | % of service exports |
| CC.EST | Control of corruption | WGI estimate |
| SE.TER.ENRR | Tertiary enrollment | % gross |
| NE.TRD.GNFS.ZS | Trade openness | % of GDP |
| TX.VAL.TECH.MF.ZS | High-tech exports | % of manufactured |
| SL.UEM.TOTL.ZS | Unemployment | % of labor force |
| SP.URB.TOTL.IN.ZS | Urban population | % of total |
| SI.POV.GINI | Gini coefficient | 0-100 scale |

### Additional Sources

| Source | Description | File |
|--------|-------------|------|
| [BCG AI Survey 2023](https://www.bcg.com) | Self-reported ChatGPT adoption by country | `data/surveys/ai_usage_survey.csv` |
| [Ipsos AI Monitor 2024](https://www.ipsos.com/en/ipsos-ai-monitor-2024) | AI attitudes survey | `data/surveys/ai_perception.csv` |
| [EF EPI 2024](https://www.ef.com/epi/) | English proficiency index | Hardcoded in `generate_html.R` |
| [Lewandowski et al. 2022](https://doi.org/10.1093/wber/lhac005) | Routine Task Intensity (102 countries) | `data/rti_country_specific_survey_predicted.csv` |
| [UNDP HDI 2024](https://hdr.undp.org/) | Human Development Index | Hardcoded in `generate_html.R` |

## Methods

### Regression approach

1. **OLS regression**: Incremental model building (6 specifications)
   - Model 1: GDP only
   - Model 2: + Internet access
   - Model 3: + Regulatory quality
   - Model 4: + Fertility rate
   - Model 5: + Population 65+
   - Model 6: + R&D spending

2. **LASSO regression**: L1 regularization with 10-fold CV for variable selection
   - Lambda selected via `cv.glmnet` (lambda.1se)
   - 20 candidate predictors, ~8 survive regularization

3. **Shapley decomposition**: R² attribution across predictors

### Sample sizes

- Core regression: n = 127 countries
- With English proficiency: n = 90 countries
- RTI analysis: n = 98 countries with overlap

### Caveats

- Cross-sectional analysis; cannot establish causation
- Claude is ~3-4% of AI market; patterns may not generalize
- Standard errors don't account for spatial correlation
- Ecological fallacy: country patterns ≠ individual patterns

## Scripts Directory

The `scripts/` folder contains exploratory analyses. These are **not required** for reproducing the main results—they document the analytical process.

| Script | Purpose | Status |
|--------|---------|--------|
| `create_world_map.R` | Generate choropleth maps | Used for figures |
| `explore_ict_exports.R` | ICT services analysis | Exploratory |
| `explore_rti_growth.R` | RTI and GDP growth | Exploratory |
| `check_automation_means.R` | Regional automation stats | Verification |
| `ai_gdp_correlation.R` | Initial correlation analysis | Exploratory |
| `controlled_analysis.R` | Early regression specs | Superseded |
| Other scripts | Various explorations | Archived |

## Citation

```bibtex
@misc{aguirre2025aiadoption,
  author = {Aguirre, Tomas},
  title = {What Predicts AI Usage Across Countries? Patterns in the Anthropic Economic Index},
  year = {2025},
  note = {Research note}
}
```

## Contact

- Email: t6aguirre@gmail.com
- Twitter: [@t6aguirre](https://twitter.com/t6aguirre)

## License

Analysis code is provided as-is for educational and research purposes. Data sources retain their original licenses (see individual sources for terms).
