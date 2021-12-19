# load libraries
library(lubridate)
library(tidyverse)


#clean Vaccination data, 'vax'

unique(vax[c("MMWR_week")])

vax <- vax %>% mutate (Series_Complete_18Plus= as.double(Series_Complete_18Plus)) #change data type to double
vax <- vax %>% mutate (SVI_CTGY= as.factor(SVI_CTGY))
vax <- vax %>% filter(MMWR_week == 49) #last full MMWR week at time of data download = week 49

sum(vax[c("FIPS")] == "UNK") #count unknown counties, 413 total 
vax <- vax %>% filter(FIPS != "UNK") #eliminate rows of unknown counties

vax1 <- vax %>% filter(Date == "12/11/2021") #filter to most recent vax updates

unique(vax1[c("FIPS")]) #confirm each row is a unique FIPS code, this is true since 3224 unique FIPs and 3224 rows

#clean election data

election1 <- election %>% filter(year == 2020) %>% filter(party == "REPUBLICAN") %>% mutate(per_trump = candidatevotes/totalvotes)

election2 <- election1[c("county_fips", "per_trump")]

election2 <- election2 %>% group_by(county_fips) %>% summarise(per_gop = sum(per_trump))

unique(election2[c("county_fips")]) #confirm each row is a unique FIPS code, this is true since 3154 unique FIPs and 3154 rows

#Join Vax with election data, 'election'

newdf <- left_join(vax1, election2, by = c("FIPS" = "county_fips"))
colnames(newdf)

#In new df, select relevant columns and omit NAs
newdf1 <- newdf[c("FIPS", "Recip_State", "Recip_County", "Series_Complete_18PlusPop_Pct", "SVI_CTGY", "per_gop")] 

newdf2 <- na.omit(newdf1)

#clean health indicators data, 'health'

health1 <- health[c("FIPS", "% Uninsured", "Primary Care Physicians Ratio", 
                    "% Vaccinated", "% Some College", 
                    "% Unemployed")] #select relevant columns to health access / education / employment

health2 <- na.omit(health1) #omit nas 

health2$'Primary Care Physicians Ratio' <- substr(health2$`Primary Care Physicians Ratio`, 
                                                  1, nchar(health2$`Primary Care Physicians Ratio`)-2) #take out :1 from ratio

health2$`% Uninsured` <- health2$`% Uninsured`/100 #make into percentage
health2$`% Vaccinated` <- health2$`% Vaccinated`/100 #make into percentage
health2$`% Some College` <- health2$`% Some College`/100 #make into percentage
health2$`% Unemployed` <- health2$`% Unemployed`/100 #make into percentage

colnames(health2)[which(names(health2) == "% Vaccinated")] <- "%_Vaccinated_FluShot" #change column name to avoid confusion 

#clean other demographic and health indicators, 'demo'

demo1 <- demo[c("FIPS", "Life Expectancy", "% Food Insecure", "Median Household Income", 
       "% Rural", "% 65 and Over", "% Less Than 18 Years of Age", 
       "% Broadband Access", "% Black", "% American Indian & Alaska Native", 
       "% Native Hawaiian/Other Pacific Islander")] #select columns relevant to age and race demographics, socioeconomic status, gender, info access w/o lots of missing values to preserve sample size
                                                    # also not including women bc basically the same across all counties

demo1$`% Food Insecure` <- demo1$`% Food Insecure`/100 #make into percentage
demo1$`% Rural` <- demo1$`% Rural`/100
demo1$`% 65 and Over` <- demo1$`% 65 and Over`/100
demo1$`% Less Than 18 Years of Age` <- demo1$`% Less Than 18 Years of Age`/100
demo1$`% Broadband Access` <- demo1$`% Broadband Access`/100
demo1$`% Black` <- demo1$`% Black`/100
demo1$`% American Indian & Alaska Native` <- demo1$`% American Indian & Alaska Native`/100
demo1$`% Native Hawaiian/Other Pacific Islander`<- demo1$`% Native Hawaiian/Other Pacific Islander`/100

#rename columns without spaces and % as leading letter to mutate
colnames(demo1)[which(names(demo1) == "% Black")] <- "perBlack"
colnames(demo1)[which(names(demo1) == "% American Indian & Alaska Native")] <- "perAmerIndian"
colnames(demo1)[which(names(demo1) == "% Native Hawaiian/Other Pacific Islander")] <- "perNative"

demo1 <- demo1 %>% mutate(per_BIPOC = perBlack + perAmerIndian + perNative) #create one variable, % BIPOC

demo2 <- subset(demo1, select = -c(perBlack, perAmerIndian, perNative))
  
demo3 <- na.omit(demo2) #omit nas

combinedhealthdemo <- left_join(demo2, health2, "FIPS") #combine health and demo data

#clean poverty data
poverty2 <- subset(poverty, select = c("FIPStxt", "PCTPOVALL_2019")) #% of all people in poverty in 2019

fulldata <- left_join(newdf2, combinedhealthdemo, "FIPS") #combine feature data with election data 

fulldata1 <- left_join(fulldata, poverty2, by = c("FIPS" = "FIPStxt"))

fulldata2 <- na.omit(fulldata1) #omit nas

fulldata2$Series_Complete_18PlusPop_Pct <- fulldata2$Series_Complete_18PlusPop_Pct/100 #put outcome, vaccination rate into percentage form 


fulldata2$PCTPOVALL_2019 <- fulldata2$PCTPOVALL_2019/100

data = fulldata2

#Change column names to avoid error in random forest
colnames(data)[which(names(data) == "Life Expectancy")] <- "life_expectancy"
colnames(data)[which(names(data) == "% Food Insecure")] <- "per_foodinsecurity"
colnames(data)[which(names(data) == "% Uninsured")] <- "per_uninsured"
colnames(data)[which(names(data) == "Median Household Income")] <- "med_household_income"
colnames(data)[which(names(data) == "% Rural")] <- "per_rural"
colnames(data)[which(names(data) == "% 65 and Over")] <- "per_65andover"
colnames(data)[which(names(data) == "% Less Than 18 Years of Age")] <- "per_18andyounger"
colnames(data)[which(names(data) == "% Broadband Access")] <- "per_broadband"
colnames(data)[which(names(data) == "%_Vaccinated_FluShot")] <- "per_flushotvaccine"
colnames(data)[which(names(data) == "% Some College")] <- "per_somecollege"
colnames(data)[which(names(data) == "% Unemployed")] <- "per_unemployed"
colnames(data)[which(names(data) == "PCTPOVALL_2019")] <- "per_poverty"
colnames(data)[which(names(data) == "Primary Care Physicians Ratio")] <- "primarycarephysicians_ratio"

# write cleaned data to file
write_csv(data, file = "Desktop/stat471/stat-471-fall-2021/finalproject/data/clean/vaxdata.csv")
            
sapply(data, class)

