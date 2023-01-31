# SmokeProject

There's a lot of code in this repository, but only some of it is necessary to reproduce the analyses for this project. Much of the code is vestigial. 

<br>

## Here are the steps to fully reproduce the analyses and figures in the manuscript.

1. Download the visitation data .csv file titled 'VisitationData.csv'.

2. Download the smoke data .csv titled 'final_means2.csv'.

3. Merge and clean these data by running the 'DataMergeClean.R' script.

4. Run all of the models by running the 'FinalFullAnalysisCode.R' script. Note that you will need to have STAN compiled on your computer and have the 'AROnly.stan' and 'AR_Set_BP.stan' files downloaded.

5. Make some of the figures using the code in the 'PaperPlotsFinal.R' script

6. Make the rest of the figures using the 'PosteriorPlotsWith0BP.R' script. 

<br>

If you have any issues running this code, contact Matt Clark at matthewclark989@boisestate.edu for help!
