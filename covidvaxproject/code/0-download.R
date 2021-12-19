# load libraries
library(tidyverse)

#read election & county demographic data
election = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/raw/countypres_2000-2020.csv")

#read covid vaccination data
vax = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/raw/COVID-19_Vaccinations_in_the_United_States_County.csv")

#read health data 
health = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/raw/health access and misc data.csv")

#read demographic data 
demo = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/raw/demographic misc data.csv")

#read poverty data
poverty = read_csv("Desktop/stat471/stat-471-fall-2021/finalproject/data/raw/PovertyEstimates.csv")

