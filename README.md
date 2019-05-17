# Survey Weighting with Differentially Private Releases of Population Data

This repository contains the work done for the final project for Harvard's CS208 (Applied Privacy for Data Science) in the spring of 2019.

Team members:
- Anthony Rentsch
- Bhaven Patel

## data
- 2016 CCES data
- State-level PUMS files from 2012 ACS 5-year estimates
- ACS joint distribution cell counts
- Maximum ACS person weights by state

## deliverables
- Draft and final version of paper
- Slides delivered in class 

## plots
- Difference between estimates when weighted to noisy vs. true ACS cell counts, by race and education
- RMSE between estimates weighted to noisy vs. true ACS cell counts, by race and education
- Person weights histogram

## scripts
- Pull and save PUMS files
- Create join distribution cell counts from state-level files
- Perform post-stratification analysis
- Perform bias analysis
- Create histogram of ACS person weights