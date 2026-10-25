# Recession-risk project targets.
#
#   make test         run the offline test suite (unit + offline pipeline)
#   make update-data  refresh dashboard artifacts (needs RJSDMX/brms host;
#                     run on production or pass PREDICTOR=pmi there)
#   make dashboard    re-render the dashboard page from committed artifacts
#   make site         render the whole Quarto site (needs LaTeX for PDFs)
#
# The typical refresh cycle on the data host is:  make update-data && git commit
# and locally:                                    make dashboard

PREDICTOR ?= esi

.PHONY: test update-data dashboard site

test:
	Rscript --vanilla new_code/tests/run_all_tests.R

update-data:
	DASHBOARD_PREDICTOR=$(PREDICTOR) Rscript --vanilla new_code/update_dashboard_data.R

dashboard:
	quarto render index.qmd

site:
	quarto render
