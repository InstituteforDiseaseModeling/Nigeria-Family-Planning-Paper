Paper: https://arxiv.org/abs/1812.00022

The files are numbered in the order in which they are run.  Files with the same number can be run in any order, provided you have already run all of the smaller numbers.  A brief description of each file is below.

0_AnalyzeSurveys.R - 
This file calls the files from the folder SurveyCode to analyze the microdata from all of the included surveys.  The result is a csv with direct estimates and standard errors by age, state, and parity for each survey.

1_CombineSurveys.R - 
This file combines all of the csvs created in the previous file into one master dataset used for the rest of the analysis.  Some surveys do not have all of the variables, so variables need to be added before datasets can be merged.

2_ModelSelection.R -
This file runs 12 different models for each outcome (4) for all women and the four age-parity subgroups.  In total this is 12 models for 20 outcome-group combinations.

3_IdentifySelectedModels.R -
This file selects the top performing model based on WAIC scores for each outcome-group combination.

4_FittingSelectedModels.R - 
This file fits the selected models from 3 and saves the model output.

5_Fig1.R -
This file generates the figures required for Figure 1 in the paper.

5_Fig2.R - 
This file generates the figures required for Figure 2 in the paper.

5_Fig3.R -  
This file generates the figures required for Figure 3 in the paper.

6_RandomEffectsSummaries.R -
This file....

7_LGA_AnalyzeCombineSurveys.R - 

8_DHS_SAE_LGA.R -

8_DHS_SAE_State.R -

Run in Docker
-------------
docker-compose up

or

docker build -t nigeria-fpp
docker run --rm -p 8787:8787 -v <path_to_code_folder>:/home/rstudio -e "DISABLE_AUTH=true" nigeria-fpp