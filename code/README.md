Analysis and curation code related to the analysis conducted for this project.
This study used  linked electronic health care datasets from England and Wales to identify infants with 9 sentinel Congenital Heart Disease (CHD) born and undergoing intervention in 2018-2022.  
For inquiries: qi_huang@ucl.ac.uk

Step1
Process the primary dataset, i.e., National Congenital Heart Disease Audit (NCHDA)

NCHDA data processing (R code)

Step 2
Create linked data, i.e., link the NCHDA dataset to HES and create care spells and derive the hospital resource utilization before age 1-year (study outcome)

Data linkage and processing (Stata code)

Further process the NCHDA data using the characteristics derived from HES (deprivation), GDPPR (gender and ethnicity) and death registration (vital status), and assign comorbidity 

NCHDA further data processing (Stata code)

Step 3
Assign the 9 CHDs diagnoses, subgroups, complexity characteristics, treatment pathways  (including study outcome of treatment pathway age) to individual patients using NCHDA data. 

The detailed rules, code list and R code have been shared in CHAMPION project: Congenital Heart Audit: Measuring Progress In Outcomes Nationally
https://github.com/UCL-CORU/CHAMPION-work-stream-2.

Step 4
Create a final study cohort and define the exposure, social factors, and other risk factors

Create the final study cohort with 9 CHDs (R code)

Step 5
Analyse the study outcomes: descriptive analysis (including figures) and statistical modelling

Outcome 1: Age at treatment pathway procedure

Study outcome: pathway age (all analysis and modelling) (R code)

Outcome 2: Infant mortality

Study outcome: infant mortality (all analysis and modelling) (R code)

Outcome 3: Hospital resource utilization

Study outcome: hospital resource utilization (all analysis and modelling) (R code)


Step 6
Create forest plot for modelling results related to pandemic era

Create forest figure (R code)
