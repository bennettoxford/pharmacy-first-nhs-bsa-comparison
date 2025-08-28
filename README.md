# Pharmacy First - NHS BSA comparison

To contextualise the Pharmacy First consultation data in OpenSAFELY-TPP, we compared it with publicly available NHS Business Services Authority (NHS BSA) data.

The OpenSAFELY analysis and more background can be found at https://github.com/opensafely/pharmacy-first.

## Steps to reproduce the results

1. **Get OpenSAFELY results**: Go to the [pharmacy-first-report](https://jobs.opensafely.org/analysis-of-the-pharmacy-first-element-in-the-plan-to-restore-access-to-primary-care-following-the-impact-of-covid-19/pharmacy-first-report/) workspace on [OpenSAFELY Jobs](https://jobs.opensafely.org/), download the released files, and add them to the subdirectory `released_output/`
2. **Get NHS BSA comparison data**: Run the following R script to get the latest NHS BSA data added to the `lib/nhsbsa/lib/validation/data/pf_consultation_validation_data.csv`
3. **Side by side comparison**: To compare the OpenSAFELY results with the NHS BSA data run the following file `analysis/nhs_bsa_pharmacy_first_comparison.Rmd` and download the figure from the subdirectory `output/`

## Data files for comparison

- `/pf_consultation_validation_data.csv`: Validation data taken from NHS BSA containing Pharmacy First consultation counts by condition.
- `/pf_consultation_validation_data_by_region.csv`: Validation data taken from NHS BSA containing Pharmacy First consultation counts by condition and by region (not currently being used in code).
