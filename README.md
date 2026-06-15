# Hierarchical Nowcasting of Euro Area Recession Risk

This repository implements a **Hierarchical Bayesian Logit** model to nowcast recession risk in the Euro Area (EA) and its four largest member states: Germany (DE), France (FR), Italy (IT), and Spain (ES). 

The methodology is a hierarchical extension of the framework proposed by **Francesco Furno and Domenico Giannone (2024)** in their paper *"Nowcasting recession risk"*.

## Project Objective

The primary goal of this project is to assess and nowcast recession probabilities in real-time. While official business cycle dating committees (such as the CEPR for the Euro Area) announce recessions with significant lags, this framework leverages high-frequency financial and macroeconomic data to estimate the probability that each economy is currently in a recession.

By using a hierarchical structure, we pool statistical strength across the Euro Area and the individual countries. This allows the model to learn common relationships between indicators and recessions while adapting parameters to country-specific dynamics.

## Data Sources

The model uses monthly and quarterly indicators retrieved directly from official APIs:
1. **Macroeconomic Activity**: **Economic Sentiment Indicator (ESI)** from Eurostat (dataset `ei_bssi_m_r2`).
2. **Financial Conditions**: **Composite Indicator of Systemic Stress (CISS)** from the European Central Bank (ECB) Data Portal.
3. **Target Variable**: **Real GDP Growth** (Quarter-on-Quarter percentage changes) from Eurostat (dataset `namq_10_gdp`). A technical recession is defined as two consecutive quarters of negative GDP growth.

## Methodology Summary

The probability of a recession $p_{i,t}$ for region $i$ at quarter $t$ is modeled using a hierarchical logit specification:
$$ \text{logit}(p_{i,t}) = \alpha_i + \beta_{1,i} \text{CISS}_{i,t} + \beta_{2,i} \text{ESI}_{i,t} $$

Where the coefficients are modeled hierarchically around a global mean:
- Intercepts: $\alpha_i = \alpha_0 + u_{0,i}$
- CISS impact: $\beta_{1,i} = \beta_1 + u_{1,i}$
- ESI impact: $\beta_{2,i} = \beta_2 + u_{2,i}$
- Random effects: $(u_{0,i}, u_{1,i}, u_{2,i}) \sim \mathcal{N}(0, \Sigma)$

Priors are selected to be weakly informative: $\alpha_0 \sim \mathcal{N}(0, 5)$ and $\beta_k \sim \mathcal{N}(0, 2)$.

## Getting Started

### Prerequisites
The project uses R and Quarto. You should have R (>= 4.0) and Quarto installed.

The project dependencies are managed via `renv`. To install them, launch R in the project root and run:
```R
renv::restore()
```

This will install packages including `brms` (for Bayesian modeling), `eurostat`, `ecb`, `dplyr`, `ggplot2`, and `patchwork`.

### Running the Analysis
To run the R script that downloads data, estimates the model, and generates results locally:
```bash
Rscript recession-index.R
```

### Rendering the Website
The results and methodology are compiled into a Quarto website. To build the website:
```bash
quarto render
```
The rendered files will be placed in the `_site/` directory.

To preview the website locally with live-reloading:
```bash
quarto preview
```

## Repository Structure
- `index.qmd`: Home page of the website displaying the compiled charts and results.
- `about.qmd`: Details about the project background and data sources.
- `methodology.qmd`: Technical report explaining the equations, priors, and references.
- `_quarto.yml`: Quarto website configuration.
- `recession-index.R`: Standalone R script for processing and estimation.
- `references.bib`: Bibliographical database containing the citation entries.
- `reference/`: Folder containing the reference paper PDF.
