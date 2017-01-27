#######################################
#Script for analyzing IMDB-ratings
#Created by @oskarflygare for the "Improving Your Statistical Inferences" course, assignment 7.1
#Date created: 2017-01-19
#R version 3.3.1
#######################################

library(tidyverse) #in case data cleaning or rearranging is needed
library(equivalence) #to perform the equivalence testing

#Data cleaning
imdb_data <- read.csv2("original_data_oskarflygare.csv", dec=",") #Loading the .csv-file, specify decimals to ,
imdb_data <- select(imdb_data, rating, actor)
hf_data <- filter(imdb_data, actor=="hf")
jn_data <- filter(imdb_data, actor=="jn")

#Descriptive statistics
imdb_summary <- group_by(imdb_data, actor) %>%
  summarize(mean(rating), sd(rating), length(rating)) #mean(rating), sd(rating) and n for both actors
hf_summary <- filter(imdb_summary, actor=="hf") #only for Harrison Ford
jn_summary <- filter(imdb_summary, actor=="jn") #only for Jack Nicholson

#Analyses
pooled_sd <- sqrt((((hf_summary$`length(rating)`-1)*hf_summary$`sd(rating)`^2)+((jn_summary$`length(rating)`-1)*jn_summary$`sd(rating)`^2))/(hf_summary$`length(rating)`+jn_summary$`length(rating)`-2)) #pooled sd calculation
d_rating <- (hf_summary$`mean(rating)` - jn_summary$`mean(rating)`) / pooled_sd #effect size calculations

tost_test_imdb <- tost(x = hf_data$rating, y = jn_data$rating, var.equal=TRUE) #Perform TOST-test for two samples, assume variance to be equal
tost_test_imdb #Display results
tost_test_imdb$tost.interval #Display confidence intervals
tost_test_imdb$tost.p.value #Display p-value