# IJC437-project---Can-Acoustic-Features-be-used-to-Predict-Song-Success?-
This anlaysis evaluates whether the sonic profile of a song can be used to predict its success and popularity. 

Instructions for Running Code 

1.)
Download Data Files: 

acoustic_features.csv
song_chart.csv
songs.csv
(Do not rename) 

2.)
Save Files in a single Directory 

Create a new folder on your computer (e.g. Music_Analytics_Project).
Place all three CSV files into this folder.
Save the R script containing the provided code into the same folder.
Keeping all files in one directory ensures the code can locate the datasets using relative file paths.

3.)
Open RStudio

The analysis was originally developed and run using RStudio, which is the recommended IDE.
Open RStudio.
Open the R script (File → Open File…).

4.) 
Set the Working Directory

In RStudio, set the working directory to the folder containing the data files:
Option A:
Session → Set Working Directory → To Source File Location
Option B (manual):
setwd("path/to/Music_Analytics_Project")

5.) 
Confirm the files are visible by running:
list.files()


6.)
Install Required Packages (First Run Only)

If any libraries are not installed, run:
install.packages(c("tidyverse", "ggplot2", "hexbin", "readr", "viridis", "reshape2"))


7.) 
Review Outputs

Plots will appear in the Plots pane.
Tables and summaries will appear in the Console or Viewer.
Model performance is reported using RMSE in the console.


Warning (Do not change file names unless you also update them in the code)

The entire workflow assumes all datasets are stored in the same directory for reproducibility and ease of access.

