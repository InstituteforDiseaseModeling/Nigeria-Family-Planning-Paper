#!/usr/bin/env bash

# Go to the right folder
cd /home/rstudio

# Remove all previous results and Processed data
rm -rf Results
rm -rf Data/Processed

# Create the Processed folder
mkdir -p Data/Processed/Nigeria

# Create the Results folders and set correct permissions
mkdir -p Results/{DecompVariance,Figures_Supplement,Model_Fits,ModelSelection,PaperFigs/Figure{1...4}}
cd Results/Figures_Supplement
mkdir -p {ds,mcpr,trad,unmet}_{Age15-24,Age15-24,Age25+,AgeALL}_{Partity0,Parity1+,ParityALL}
mkdir LGA_SAE_mcpr
cd ../..

# Give permissions to all on Results and Data
chmod 777 Results -R
chmod 777 Data/Processed -R

# Start
/init