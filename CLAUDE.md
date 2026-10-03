# CLAUDE.md

`floodchlorinationsurvey` is an openwashdata R data package with the USAID Flood Response rehabilitation and chlorination survey from Mulanje, Malawi (2019-2020), collected with mWater.

## Package facts

- Raw data: `data-raw/flood response waterpoint rehabilitation and chrolination survey.csv` (the file name has this spelling).
- Processing script: `data-raw/data_processing.R`. It reads the raw data and writes `data/floodchlorinationsurvey.rda` and the CSV and XLSX exports in `inst/extdata/`.
- Data dictionary: `data-raw/dictionary.csv`.
- Branches: work and review PRs go to `dev`; `master` holds released versions. This repo has no `main` branch, so where the pkgreview skills name `main`, use `master`.

## Reviews and releases

Reviews and releases follow the installed pkgreview skills. `/review-package` starts a review, `/review-issue` works through one review issue, `/create-release` makes a release and `/add-doi` adds the Zenodo DOI. The skills hold the steps and the current standards, so this file does not repeat them.
