# Explore OpenActive data with R code
Example R code to harvest and display open data about opportunities for sport and physical activity. 

## Setting up
This code assumes you're using a recent version of [RStudio](https://posit.co/downloads/) and [R](https://www.r-project.org/).  
I'm using RStudio 2023.09.1+494 "Desert Sunflower" Release for macOS, with R version 4.2.1 (2022-06-23) "Funny-Looking Kid"

I use the following libraries for harvesting, manipulating and displaying the data:
```
library(httr)  
library(jsonlite)  
library(lubridate)  
library(stringr)  
library(dplyr)  
library(rvest)   
library(readr)  
library(purrr)  
library(tidyr)  
library(leaflet)  
library(htmltools)
```
