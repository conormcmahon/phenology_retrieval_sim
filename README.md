# Phenology Retrieval - Sensitivity Simulations

Tests the sensitivity of local polynomial regression phenology retrieval method to cloud cover and sensor / algorithmic noise. 

phenology_fit_tester.R

- Core code for performing simulations
- Relies on a parameter file (phenology_fit_test_parameters.csv) which includes the range of parameter values to simulate over
- For more information on parameters, see associated manuscript.

inter_sensor_comparison.R

- compares Landsat, Sentinel-2, and MODIS phenology estimates from Earth Engine

data_prep_for_manuscript.R

- generates small tiles of line plots for R^2 values in each simulated sensor

initial_figure_prep.R

- all other figures for manuscript
