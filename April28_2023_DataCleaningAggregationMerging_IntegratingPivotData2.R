
#### DATA ENRICHMENT #####

# 1. Data Cleaning
# --> eliminating discontinued metrics 
# ---> Changing character to numeric data types
# 2. Aggregating data 
# ---> creating aggregated metrics of the retained metrics
# ---> retained excluded metrics after commenting out for posterity
# ----> Overall index as an average of the people, planet and governance indices.
# 3. Merging with automated financial data




##Libraries and Packages ##
library(tidyverse)
library(haven)
library(readr)
library(labelled)
library(dplyr)

#ex=read.csv("https://www.dropbox.com/s/er3yvmkxg9j88x5/Open%20For%20Good%20Platform%20World%20Economic%20Forum%20Metrics%20Project_April%2027%2C%202023_08.29.csv?dl=0", header = TRUE, sep = ",")
#ex
#colnames(ex)
##DIRS
#Intake the latest Qualtrics data that has been pulled
##environmental competencies - 0.5 or 1 what were the competencies
#setwd("C:/UCLAAnderson/10KfilingsTables/Data")
setwd("C:/UCLAAnderson/10KfilingsTables/Data/Open+For+Good+Platform+World+Economic+Forum+Metrics+Project_August+25,+2023_11.04")
data<-read.csv("Open For Good Platform World Economic Forum Metrics Project_August 25, 2023_11.04.csv",header=T)
#data <- read.csv("Open For Good Platform World Economic Forum Metrics Project_April 27, 2023_08.29.csv", header=T)<<-previously used for Launch Data
#data[c(1:2),c("CM11.Index", "CM12.Index", "CM13.Index", "CM14.Index","CM15.Index")]
colnames(data)
nrow(data)
#######Changing sectors from numeric to text ############


sector_maps<-as.data.frame(cbind(GICS.Sector = c(4:11,13:15), 
                                 txt_GICS.Sector = c( "Communication Services", "Consumer Discretionary","Consumer Staples",
                                                      "Energy","Financials","Health Care","Industrials","Information Technology",	
                                                      "Materials","Real Estate","Utilities")))
data$GICS.Sector<-as.character(data$GICS.Sector)
data_alt<-left_join(data,sector_maps)
data_alt$txt_GICS.Sector

data$GICS.Sector<-data_alt$txt_GICS.Sector
data$GICS.Sector

# data_alt2=data_alt
# data_alt2$GICS.Sector<-data_alt2$txt_GICS.Sector
# data_alt3<-data_alt2[,-(ncol(data_alt2))]
# head(data_alt3$GICS.Sector)
# colnames(data_alt3)
# 

colnames(data)
head(data[,c(138:141)])
summary(data[,140])

#environmental competencies for Kelly:
Environ_comp<-subset(data,CM2a..Environ.Comps>0 )
subset(data,CM2a..Environ.Comps>0 )$Company.Name
data$CM2a..Environ.Comps
nrow(subset(as.data.frame(data[,140]),data[,1]>0))
colnames(Environ_comp)
EnvCompetency<-Environ_comp[,c("Company.Name" ,"Ticker.Symbol" ,"CUSIP" ,"SIC.Code" ,"GICS.Sector","GICS.Industry","CM2a..Environ.Comps")]

#setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly")
#write.csv(EnvCompetency,"CompaniesWithEnvCompetency.csv")
nrow(EnvCompetency)

nrow(Environ_comp)
unique(EnvCompetency$CM2a..Environ.Comps)
length(unique(EnvCompetency$Company.Name))
#Open For Good Platform World Economic Forum Metrics Project_March 21, 2023_10.14
# Note previous analysis done on:Open For Good Platform World Economic Forum Metrics Project_February 21, 2023_10.56
#Open For Good Platform World Economic Forum Metrics Project_February 2, 2023_12.11
##Open For Good Platform World Economic Forum Metrics Project_January 21, 2023_11.23 done previously on

## cross-checking the incoming data ###

colnames(data)### 362 columns

##Removing Extra Header Rows from XM Format

dat <- data[-c(1,2),]## Removing "StartDate" and  "EndDate" columns
head(data)

## Removing the columns that contain metrics that are no longer in use ###########
colnames(dat[209])## i.e. metrics beyonf this column 209: "CM16a.Training.Provi"
colnames(dat[,c(1:208)])## i.e. uptil column 208: "Q432"

ofg_dat<-dat[,c(1:208)] #### Updating the data to be used to eliminate columns beyond 208

colnames(ofg_dat)## sanity check


## Changing the DATA TYPE for some of the metrics to numeric instead of character to be able to aggregate later

ofg_data <- ofg_dat %>% mutate(CM2a.Econ.Comps = as.numeric(CM2a.Econ.Comps),
                                CM2a..Social.Comps = as.numeric(CM2a..Social.Comps),
                                CM2a..Environ.Comps = as.numeric(CM2a..Environ.Comps),
                                CM2b.Governance = as.numeric(CM2b.Governance),
                                CM2c.Governance = as.numeric(CM2c.Governance),
                                CM2d.Governance = as.numeric(CM2d.Governance),
                                CM2e.Governance = as.numeric(CM2e.Governance),
                                CM2f.Governance = as.numeric(CM2f.Governance),
                                CM2g.Governance = as.numeric(CM2g.Governance),
                                CM2h.Governance = as.numeric(CM2h.Governance))

summary(ofg_data)## cross-checking data types of each column
colnames(ofg_data)
head(ofg_data)## sanity check



####################### Calculating average using rowSums to account for any NAs#########


new_DF <- ofg_data[,c("CM2a..Social.Comps","CM2a..Environ.Comps", 
                    "CM2b.Governance" , "CM2c.Governance" ,
                    "CM2d.Governance", "CM2e.Governance",
                    "CM2f.Governance", "CM2g.Governance",
                    "CM2h.Governance")][rowSums(is.na( ofg_data[,c("CM2a..Social.Comps","CM2a..Environ.Comps", 
                                                                   "CM2b.Governance" , "CM2c.Governance" ,
                                                                   "CM2d.Governance", "CM2e.Governance",
                                                                   "CM2f.Governance", "CM2g.Governance",
                                                                   "CM2h.Governance")])) > 0,]


library(dplyr)

ofg_data$CM2.Index=rowMeans(ofg_data[,c("CM2a..Social.Comps","CM2a..Environ.Comps", 
                                       "CM2b.Governance" , "CM2c.Governance" ,
                                       "CM2d.Governance", "CM2e.Governance",
                                       "CM2f.Governance", "CM2g.Governance",
                                       "CM2h.Governance")],na.rm=T)





###### Replace with Mean instead of denominator<<<<
# ofg_data <- ofg_data %>% mutate(CM2.Index = (CM2a..Social.Comps + CM2a..Environ.Comps + 
#                                                CM2b.Governance + CM2c.Governance +
#                                                CM2d.Governance +CM2e.Governance +
#                                                CM2f.Governance +CM2g.Governance +
#                                                CM2h.Governance)/9)

ofg_data <- ofg_data %>% mutate(CM3.Material.is = as.numeric(CM3.Material.is))
ofg_data <- ofg_data %>% mutate(CM3.Index = ((CM3.Material.is)/1))



ofg_data <- ofg_data %>% mutate(CM4a.Anti.corruption = as.numeric(CM4a.Anti.corruption),
                               # CM4b.Anti.corruption = as.numeric(CM4b.Anti.corruption),
                               # CM4c.Anti.corruption = as.numeric(CM4c.Anti.corruption),
                                CM4d.Anti.corruption = as.numeric(CM4d.Anti.corruption))

## "CM4b.Anti.corruption" has been made redundant

ofg_data[,c("CM4a.Anti.corruption",#"CM4b.Anti.corruption"#, 
            #"CM4c.Anti.corruption",
            "CM4d.Anti.corruption"
            )
            ]

ofg_data$CM4.Index=rowMeans(ofg_data[,c("CM4a.Anti.corruption",#"CM4b.Anti.corruption", 
                                          #"CM4c.Anti.corruption",
                                        "CM4d.Anti.corruption")], na.rm=T)

#ofg_data[,c("CM6a.Integrating.ris","CM6b.Integrating.ris","CM6c.Integrating.ris")]
colnames(ofg_data)

########### Commenting out previously captured but now redundant metrics to retain the decision trails ###########

# ofg_data <- ofg_data %>% mutate(CM4.Index = (CM4a.Anti.corruption +  CM4b.Anti.corruption + 
#                                    CM4c.Anti.corruption + CM4d.Anti.corruption)/4)

#ofg_data <- ofg_data %>% mutate(CM5a.Protected.Ethic = as.numeric(CM5a.Protected.Ethic),
#                                  CM5b.Protected.Ethic = as.numeric(CM5b.Protected.Ethic))


#ofg_data <- ofg_data %>% mutate(CM5.Index = (CM5a.Protected.Ethic + CM5b.Protected.Ethic)/2)

# ofg_data <- ofg_data %>% mutate(CM6a.Integrating.ris = as.numeric(CM6a.Integrating.ris),
#                                 CM6b.Integrating.ris = as.numeric(CM6b.Integrating.ris),
#                                 CM6c.Integrating.ris = as.numeric(CM6c.Integrating.ris)
#                                 )
# 
# 
# 
# ofg_data <- ofg_data %>% mutate(CM6.Index = (CM6a.Integrating.ris + CM6b.Integrating.ris + 
#                                   CM6c.Integrating.ris)/3)

ofg_data <- ofg_data %>% mutate(CM7a.GHG.Emissions. = as.numeric(CM7a.GHG.Emissions.),
                                CM7b.GHG.Emissions. = as.numeric(CM7b.GHG.Emissions.),
                                CM7c.GHG.Emissions. = as.numeric(CM7c.GHG.Emissions.))


ofg_data$CM7.Index <- rowMeans(ofg_data[,c("CM7a.GHG.Emissions.","CM7b.GHG.Emissions.","CM7c.GHG.Emissions.")],na.rm = TRUE)
                                        #ofg_data %>% mutate(CM7.Index = (CM7a.GHG.Emissions. + CM7b.GHG.Emissions. + 
                                             #  CM7c.GHG.Emissions.)/3)

###Commenting the now removed metrics and sub-metrics###

####### TCFD Metrics edited from multiple to single ########


# 
# ofg_dat1<-subset(ofg_data,CM8.TFCD.New=="")
# nrow(ofg_dat1)
# sum(ofg_dat1$CM8.TFCD.New != "")## No. of entered companies in new TCFD
# tail(ofg_dat1$CM8.TFCD.New)
# 
# ofg_data <- ofg_data %>% mutate(CM8a.TCFD.Implementa = as.numeric(CM8a.TCFD.Implementa),
#                                 CM8b.TCFD.Implementa = as.numeric(CM8b.TCFD.Implementa),
#                                 CM8c.TCFD.Implementa = as.numeric(CM8c.TCFD.Implementa),
#                                 CM8d.TCFD.Implementa = as.numeric(CM8d.TCFD.Implementa),
#                                 CM8e.TCFD.Implementa = as.numeric(CM8e.TCFD.Implementa),
#                                 CM8f.TCFD.Implementa = as.numeric(CM8f.TCFD.Implementa),
#                                 CM8g.TCFD.Implementa = as.numeric(CM8g.TCFD.Implementa),
#                                 CM8h.TCFD.Implementa = as.numeric(CM8h.TCFD.Implementa),
#                                 CM8i.TCFD.Implementa = as.numeric(CM8i.TCFD.Implementa),
#                                 #CM8j.TCFD.Implementa = as.numeric(CM8j.TCFD.Implementa),
#                                 CM8k.TCFD.Implementa = as.numeric(CM8k.TCFD.Implementa))
# 
# 
# 
# ### Removing last 9 rows 
# ofg_data$CM8.Index<-rowMeans(ofg_data[,c("CM8a.TCFD.Implementa","CM8b.TCFD.Implementa", "CM8c.TCFD.Implementa","CM8d.TCFD.Implementa","CM8e.TCFD.Implementa",
# "CM8f.TCFD.Implementa","CM8g.TCFD.Implementa","CM8h.TCFD.Implementa","CM8i.TCFD.Implementa",#" CM8j.TCFD.Implementa",
# "CM8k.TCFD.Implementa")], na.rm = T)
# 

# ofg_data <- ofg_data %>% mutate(CM8.Index = (CM8a.TCFD.Implementa + CM8b.TCFD.Implementa + 
#                                              CM8c.TCFD.Implementa + CM8d.TCFD.Implementa +
#                                              CM8e.TCFD.Implementa + CM8f.TCFD.Implementa +
#                                              CM8g.TCFD.Implementa + CM8h.TCFD.Implementa +
#                                              CM8i.TCFD.Implementa + CM8j.TCFD.Implementa + 
#                                              CM8j.TCFD.Implementa + CM8k.TCFD.Implementa)/11)

ofg_data <- ofg_data %>% mutate(CM9.Land.use.and.eco = as.numeric(CM9.Land.use.and.eco))
ofg_data <- ofg_data %>% mutate(CM9.Index = (CM9.Land.use.and.eco)/1)

ofg_data <- ofg_data %>% mutate(CM10a.Water.Use..meg = as.numeric(CM10a.Water.Use..meg),
                                CM10b.Water.Use....o = as.numeric(CM10b.Water.Use....o),
                                CM10c.Water.Use..Meg = as.numeric(CM10c.Water.Use..Meg),
                                CM10d.Water.Use....o = as.numeric(CM10d.Water.Use....o))
ofg_data <- ofg_data %>% mutate(CM10.Index = (CM10a.Water.Use..meg + CM10b.Water.Use....o +
                                              CM10c.Water.Use..Meg + CM10d.Water.Use....o)/4)

ofg_data <- ofg_data %>% mutate(CM11a.Diversity.and = as.numeric(CM11a.Diversity.and),
                                CM11b.Diversity.and = as.numeric(CM11b.Diversity.and),
                                CM11c.Diversity.and = as.numeric(CM11c.Diversity.and))
ofg_data <- ofg_data %>% mutate(CM11.Index = (CM11a.Diversity.and + CM11b.Diversity.and +
                                CM11c.Diversity.and)/3)


ofg_data <- ofg_data %>% mutate(CM12a.Pay.Equality. = as.numeric(CM12a.Pay.Equality.),
                                #CM12b.Pay.Equality. = as.numeric(CM12b.Pay.Equality.)
                                )

ofg_data <- ofg_data %>% mutate(CM12.Index = (CM12a.Pay.Equality.# + CM12b.Pay.Equality.)/2
                                              ))##<<<<- since cm12b is now removed

ofg_data <- ofg_data %>% mutate(#CM13a.Wage.Level..Ra = as.numeric(CM13a.Wage.Level..Ra),
                                CM13b.Wage.Level..Ra = as.numeric(CM13b.Wage.Level..Ra))
ofg_data <- ofg_data %>% mutate(CM13.Index = (#CM13a.Wage.Level..Ra + 
  CM13b.Wage.Level..Ra)#/2
  )

ofg_data <- ofg_data %>% mutate(CM14a.Risks.for.inci = as.numeric(CM14a.Risks.for.inci),
                                CM14b.Risks.for.inci = as.numeric(CM14b.Risks.for.inci))
ofg_data <- ofg_data %>% mutate(CM14.Index = (CM14a.Risks.for.inci + CM14b.Risks.for.inci)/2)

ofg_data <- ofg_data %>% mutate(#CM15a.Health.and.Saf = as.numeric(CM15a.Health.and.Saf),
                                #CM15b.Health.and.Saf = as.numeric(CM15b.Health.and.Saf),
                                # CM15c.Health.and.Saf = as.numeric(CM15c.Health.and.Saf),
                                # CM15d.Health.and.Saf = as.numeric(CM15d.Health.and.Saf),
                                # CM15e.Health.and.Saf = as.numeric(CM15e.Health.and.Saf),
                                CM15f.Health.and.Saf = as.numeric(CM15f.Health.and.Saf))
ofg_data <- ofg_data %>% mutate(CM15.Index = (#CM15a.Health.and.Saf + CM15b.Health.and.Saf +
                                              #CM15c.Health.and.Saf + CM15d.Health.and.Saf +
                                              #CM15e.Health.and.Saf + 
  CM15f.Health.and.Saf))#/6)


#ofg_data[,"CM20.Response..in.do"]

# ofg_data <- ofg_data %>% mutate(CM16a.Training.Provi = as.numeric(CM16a.Training.Provi),
#                                 CM16b.Training.Provi = as.numeric(CM16b.Training.Provi),
#                                 CM16c.Training.Provi = as.numeric(CM16c.Training.Provi))
# ofg_data <- ofg_data %>% mutate(CM16.Index = (CM16a.Training.Provi + CM16b.Training.Provi +
#                                               CM16c.Training.Provi)/3)

# ofg_data <- ofg_data %>% mutate(CM17a.Absolute.numbe = as.numeric(CM17a.Absolute.numbe),
#                                 CM17b.Absolute.numbe = as.numeric(CM17b.Absolute.numbe),
#                                 CM17c.Absolute.numbe = as.numeric(CM17c.Absolute.numbe),
#                                 CM17d.Absolute.numbe = as.numeric(CM17d.Absolute.numbe),
#                                 CM17e.Absolute.numbe = as.numeric(CM17e.Absolute.numbe),
#                                 CM17f.Absolute.numbe = as.numeric(CM17f.Absolute.numbe))
# ofg_data <- ofg_data %>% mutate(CM17.Index = (CM17a.Absolute.numbe + CM17b.Absolute.numbe +
#                                               CM17c.Absolute.numbe + CM17d.Absolute.numbe +
#                                               CM17e.Absolute.numbe + CM17f.Absolute.numbe)/6)

#ofg_data[,"CM18g.Economic.Contr"]


# ofg_data <- ofg_data %>% mutate(CM18e.Economic.Contr = as.numeric(CM18e.Economic.Contr),
#                                 CM18f.Economic.Contr = as.numeric(CM18f.Economic.Contr),
#                                 CM18g.Economic.Contr = as.numeric(CM18g.Economic.Contr))
# ofg_data <- ofg_data %>% mutate(CM18.Index = (4 + CM18e.Economic.Contr + CM18f.Economic.Contr + 
#                                                 CM18g.Economic.Contr)/7)
##4 is added to Core Metric 18's aggregation due to 4 of the sub-metrics being reported by all firms on their 10-k's

ofg_data <- ofg_data %>% mutate(CM19.Index = 1)
## Core Metric 19 is reported by all firms on their 10-k's


ofg_data[,"CM15.Index"]
class(ofg_data[,"CM15.Index"])

# ofg_data <- ofg_data %>% mutate(CM20.Response..in.do = as.numeric(CM20.Response..in.do))
# ofg_data <- ofg_data %>% mutate(CM20.Index = if_else(is.na(CM20.Response..in.do),0,1))
## Core Metric 20 is R&D expense reporting. This is not reported by all firms, 
##so if any number is reported then a 1 is given for the index

# ofg_data <- ofg_data %>% mutate(CM21a.Response = as.numeric(CM21a.Response),
#                                 CM21b.Response = as.numeric(CM21b.Response))
# ofg_data <- ofg_data %>% mutate(CM21.Index = (if_else(is.na(CM21a.Response),0,1) + 
#                                                 if_else(is.na(CM21b.Response),0,1))/2)
## Core Metric 21 is Tax expense reporting. This is not reported by all firms, 
##so if any number is reported then a 1 is given for each half (US taxes, Worldwide taxes)

##Core Metric to Pillar Aggregation 
##Removed --- CM1.Index +

ofg_data[,c("CM8.TFCD.New","CM11.Index","CM12.Index","CM13.Index","CM14.Index","CM15.Index")]
tail(ofg_data$CM8.TFCD.New)
colnames(ofg_data)
ofg_data[ofg_data==""]<-NA
ofg_data$CM8.TFCD.New<-as.numeric(as.character(ofg_data$CM8.TFCD.New))### for the time when these are not NA

ofg_data <- ofg_data %>% mutate(Gov.Index = (( CM2.Index + CM3.Index + 
                                             CM4.Index) #+ CM5.Index + 
                                               #CM6.Index)
                                               /3#6
                                             ),
                                Planet.Index = ((CM7.Index + CM9.Index +CM8.TFCD.New+ #CM8.Index +# replacing TCFD CM8.Index with CM8.TCFD.New
                                                CM10.Index)/4),
                                People.Index = ((CM11.Index + CM12.Index + CM13.Index +
                                                 CM14.Index + CM15.Index) #+ CM16.Index)
                                                 /5#6
                                                ))#,
                                # Pros.Index = ((CM17.Index + CM18.Index + CM19.Index +
                                #                CM20.Index + CM21.Index)/5))<<<<<<<<<<<
tail(ofg_data$CM8.TFCD.New)

##Pillar to Overall Aggregation
ofg_data <- ofg_data %>% mutate(Overall.Index = ((Gov.Index + Planet.Index + 
                                                  People.Index) 
                                                  #+ Pros.Index)
                                                   /3)) ##<<<< changed to remove prosperity

ofg_data$CM2a..Social.Comps
summary
nrow(ofg_data)
##Saving Output Data Frame
# output_file_name <- paste("ofg_data_c+a_",Sys.Date(),".csv",sep = "")
# output_file_name_dir <- paste(working_dir,output_file_dir,output_file_name,sep = "\\")

### S&P500 or not?######
setwd("C:/UCLAAnderson/10KfilingsTables/Data/NikithaData/RE S&P 500 companies")
SP500<-read.csv("Name master.csv")

head(SP500)
colnames(ofg_data)
Joined_SP500_data<-left_join(ofg_data,SP500, by="Company.Name")

head(Joined_SP500_data)
colnames(Joined_SP500_data)
###### Actual company name in the official filings -- Kelly list ----

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly/fromKelly")
OfficialCompanies<-read.csv("2022.04.21_SECCompanyFilingNames.csv")
head(OfficialCompanies)
subset(OfficialCompanies, Ticker.Symbol=="ALLE")

AllCompNames<-left_join(Joined_SP500_data,OfficialCompanies[,c("Ticker.Symbol","SEC.Filing.Name")], by="Ticker.Symbol")
head(AllCompNames)
nrow(AllCompNames)
subset(AllCompNames,Ticker.Symbol=="NDAQ")
##### RECENTMOST YEAR - Only selecting the row that has the most recent year's data collected for each company:############

Joined_ofg_data<-as.data.frame(AllCompNames %>% group_by(Ticker.Symbol) %>% top_n(1, Year)) ### We wanted to have the recentmost year only for the launch on May 3rd, 2023

#### We want to retain all years of data collection for the dashboard eventually so removing the above step. 

#Joined_ofg_data<-AllCompNames###without filtering years

unique(Joined_ofg_data$Year)
unique(Joined_SP500_data$Year)

nrow(Joined_SP500_data)
nrow(Joined_ofg_data)

#Joined_ofg_data<-AllCompNames


Joined_ofg_data[548,]
unique(Joined_ofg_data$Company.Name)

###Removing empty row######
nrow(Joined_ofg_data)
SP500GrandDataset=subset(Joined_ofg_data,Company.Name!="")
nrow(SP500GrandDataset)
subset(SP500GrandDataset, Ticker.Symbol=="NDAQ")

# CM7b. Scope 2 response (CM7b.Uncategorized) – has some text and the entire column had to be converted into float (as there were commas)

unique(SP500GrandDataset$CM7b.Uncategorized.)
subset(SP500GrandDataset,CM7b.Uncategorized.=="73,495 combined scope 1&2")$Company.Name

## removing characters 
SP500GrandDataset$CM7b.Uncategorized. <- gsub(" combined scope 1&2","",SP500GrandDataset$CM7b.Uncategorized.)
unique(SP500GrandDataset$CM7b.Uncategorized.)


###removing commas so the numbers are not treated as character
SP500GrandDataset$CM7b.Uncategorized. <- as.numeric(gsub(",","",SP500GrandDataset$CM7b.Uncategorized.))
unique(SP500GrandDataset$CM7b.Uncategorized.)

##NAs introduced by corecion so the replacement of NAs withe empty cells can only occur after this step

# All “NA” to be changed to blanks in the master file from R

summary(SP500GrandDataset)# does not look like there are any NAs
SP500GrandDataset[!complete.cases(SP500GrandDataset), ]

SP500GrandDataset[is.na(SP500GrandDataset)] <- ""## Replace NAs if any, with blank string



subset(SP500GrandDataset, Ticker.Symbol =="CVS")$Year
## autonation
## Raytheon Technologies



unique(SP500GrandDataset$Year)

subset(SP500GrandDataset, Year == "2019")$Company.Name
###   Number of companies per sector ###########

library(plyr)

count(SP500GrandDataset, "GICS.Sector")
colnames(SP500GrandDataset)
unique(subset(SP500GrandDataset, GICS.Sector =="Communication Services")$Ticker.Symbol)


unique(SP500GrandDataset$Year)

subset(SP500GrandDataset, Year == "2018")$Ticker.Symbol



## Some companies were not S&P 500 and were also selected for multiple years so we are trying to deduplicate so there is one entry only for each company ####

## remove the rows for duplicate companies that are not the above years.

# nrow(SP500GrandDataset)
# 
# ## First remove all companies with duplicated entries
# 
# RemovedDuplicateComps<-subset(SP500GrandDataset, Ticker.Symbol!="NWSA"&
#                                 Ticker.Symbol != "ODFL"&Ticker.Symbol != "PBF"&
#                                 Ticker.Symbol != "SEE"&Ticker.Symbol != "LW"&Ticker.Symbol != "SNA"&
#                                 Ticker.Symbol != "TSCO"&Ticker.Symbol != "GPS"&Ticker.Symbol != "SFM"&Ticker.Symbol != "STT")
# 
# 
# nrow(RemovedDuplicateComps)
# 
# ### Carefully add only rows of the companies with duplicates to include years of data collection you want to include -- periodically cross-check the duplicates and check with Tyson
# 
# DuplicatedTIKRS<-subset(SP500GrandDataset,Ticker.Symbol=="NWSA" & Year =="2021"|Ticker.Symbol == "ODFL"& Year =="2020"|Ticker.Symbol == "PBF" & Year =="2021"|
#                           Ticker.Symbol == "SEE"& Year =="2020"|Ticker.Symbol == "LW"& Year =="2020"
#                         |Ticker.Symbol == "SNA"& Year =="2020"|Ticker.Symbol == "TSCO"& Year =="2020"|Ticker.Symbol == "GPS" & Year =="2019"|Ticker.Symbol == "STT" & Year =="2020")# Removing Sprouts because it is not SP500 #|Ticker.Symbol == "SFM")
# 



# DuplicatedTIKRS[,c('Company.Name', 'Ticker.Symbol', 'Year')]
# nrow(DuplicatedTIKRS)
# Dedupe_SP500GrandDataset<-as.data.frame(rbind(RemovedDuplicateComps, DuplicatedTIKRS))
# nrow(Dedupe_SP500GrandDataset)
# subset(Dedupe_SP500GrandDataset, Ticker.Symbol=="STT")$Year
# 
# colnames(Dedupe_SP500GrandDataset)

nrow(SP500GrandDataset)
dedupe_SP500Grand<-SP500GrandDataset[!duplicated(SP500GrandDataset), ]
duplicate_SP500Grand<-SP500GrandDataset[duplicated(SP500GrandDataset), ]
colnames(duplicate_SP500Grand)
nrow(dedupe_SP500Grand)
nrow(duplicate_SP500Grand)

duplicate_SP500Grand[,c("Company.Name","Ticker.Symbol","SEC.Filing.Name","Year")]

setdiff(unique(SP500GrandDataset$Ticker.Symbol),unique(dedupe_SP500Grand$Ticker.Symbol))

SP500GrandDataset[duplicated(SP500GrandDataset), ][,c("Company.Name","Ticker.Symbol","SEC.Filing.Name","Year")]
length(unique(SP500GrandDataset$Ticker.Symbol))

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly")
write.csv(dedupe_SP500Grand[,c("Company.Name","Ticker.Symbol","Year")],"August25_CompanyTIKRnYear.csv")

subset(dedupe_SP500Grand,Company.Name=="NEWS CORP")

##Remove YETI and News Corp one of the duplicates
Dedupe_SP500GrandDataset<-subset(dedupe_SP500Grand,Company.Name!="NEWS CORP" & Ticker.Symbol!="YETI"& Ticker.Symbol!="ALLE3")

nrow(dedupe_SP500Grand)
nrow(Dedupe_SP500GrandDataset)
unique(Dedupe_SP500GrandDataset$Year)
##### Replacing NAs in audited.Report with 0s###########
unique(Dedupe_SP500GrandDataset$Audited.Report)
Dedupe_SP500GrandDataset$Audited.Report <- sub("^$", "0", Dedupe_SP500GrandDataset$Audited.Report)
nrow(Dedupe_SP500GrandDataset)
colnames(Dedupe_SP500GrandDataset)

Dedupe_SP500GrandDataset$Company.Name<-Dedupe_SP500GrandDataset$SEC.Filing.Name
Dedupe_SP500GrandDataset[,c("Company.Name","Cleaned.Company.Name","SEC.Filing.Name")]
subset(Dedupe_SP500GrandDataset, Ticker.Symbol=="ALLE3")

subset(dedupe_SP500Grand, Ticker.Symbol=="ALLE3")$SEC.Filing.Name
str(dedupe_SP500Grand$SEC.Filing.Name)

unique(Dedupe_SP500GrandDataset$Ticker.Symbol)
colnames(Dedupe_SP500GrandDataset)

###Removing non S&P500 companies
OnlySP500<-subset(Dedupe_SP500GrandDataset,S.P500.or.Not!="No")
length(unique(OnlySP500$Ticker.Symbol))

#########Some companies' Ticker Symbols have changed ###########


OnlySP5002<-OnlySP500 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'Berkshire Hathaway Inc.', "BRK.B", Ticker.Symbol))
OnlySP5003<-OnlySP5002 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'Ball Corporation', "BALL", Ticker.Symbol))
OnlySP5004<-OnlySP5003 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'Discovery, Inc.', "WBD", Ticker.Symbol))
OnlySP5005<-OnlySP5004 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'Cabot Oil & Gas Corporation', "CBT", Ticker.Symbol))
OnlySP5006<-OnlySP5005 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'NortonLifeLock Inc.', "GEN", Ticker.Symbol))
OnlySP5007<-OnlySP5006 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'Anthem, Inc.', "ELV", Ticker.Symbol))
OnlySP5008<-OnlySP5007 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'Facebook, Inc.', "META", Ticker.Symbol))
OnlySP5009<-OnlySP5008 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'ViacomCBS Inc.', "PARA", Ticker.Symbol))
OnlySP50010<-OnlySP5009 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'SVB Financial Group', "SIVBQ", Ticker.Symbol))
OnlySP50011<-OnlySP50010 %>% mutate(Ticker.Symbol = ifelse(SEC.Filing.Name == 'Fortune Brands Home & Security, Inc.', "FBIN", Ticker.Symbol))


length(OnlySP500$Ticker.Symbol)
subset(OnlySP500, Ticker.Symbol=="NDAQ")

############# First Dataset OutPut #################
setwd("C:/UCLAAnderson/10KfilingsTables/Data")
write.csv(OnlySP50011, "August25_TCFDCompCorrected_DataOutput1RecenctYearOnlyTruncatedAggregation.csv")
write.csv(colnames(OnlySP50011),"August25_submetrics.csv")
colnames(SP500GrandDataset)


setwd("C:/UCLAAnderson/10KfilingsTables/Data")
data<-read.csv('May2_DataOutputYearOnlyTruncatedAggregation.csv')

# setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly")
# wanted<-read.csv("OFG Code Book_KHC.csv")
# wantMets<-subset(wanted, Count=="1")
# 
# setdiff(unique(wantMets$Sub.Metric.Name), colnames(SP500GrandDataset)[-c(1:24)])

# unlist(lapply(SP500GrandDataset, is.numeric), use.names = FALSE) 
#####################################################################################################
#                         Second Melted Dataset for Exec Ed 
#####################################################################################################

setwd("C:/UCLAAnderson/10KfilingsTables/Data")
data1<-read.csv("August25_TCFDCompCorrected_DataOutput1RecenctYearOnlyTruncatedAggregation.csv")
subset(data1,"Company.Name"=="International" )

nrow(subset(data1, Year=="2022"))
colnames(data1)
length(unique(data1$Ticker.Symbol))
###Adding the revenue column which can be a proxy until the finalized list with value 1:

data1$CM18a.Response<-rep(1, nrow(data1))
data1$CM18a.Response
##Instead of having to import a csv that may be edited or located in different systems or folders,
# incorporating the metrics of interest withiin the R script
colnames(data1)
dat<-data1[,c("Company.Name","SEC.Filing.Name","Ticker.Symbol","GICS.Sector","GICS.Industry","Year","Audited.Report",
"Number.of.Board","CM2a.Econ.Comps","CM2a....economic","CM2a..Environ.Comps","CM2a..environ","CM2a..Social.Comps",
"CM2a....social","CM2b.Governance","CM2b.Response","CM2c.Governance","CM2c.Response",
"CM2d.Governance","CM2d.Response","CM2e.Governance","CM2f.Governance","CM2f.Response.b",
"CM2g.Governance","CM2g.Number","CM2h.Governance","CM3.Material.is","CM4a.Anti.corruption","CM4a.Percent.of.empl",
"CM4a.Percent.of.busi","CM4d.Anti.corruption","CM7a.GHG.Emissions.","CM7a.Response..in.me","CM7b.GHG.Emissions.",
"CM7b.Uncategorized.","CM7c.GHG.Emissions.","CM7c.Response..metri","CM8.TFCD.New","CM9.Land.use.and.eco","CM9.Area.in.hectares",
"CM10a.Water.Use..meg","CM10a.Response","CM10b.Water.Use....o","CM10b.Response","CM10c.Water.Use..Meg","CM10c.Response",
"CM10d.Water.Use....o","CM10d.Response","CM11a.Diversity.and","CM11a...less.than.30","CM11a...between.30.5",
"CM11a...over.50","CM11b.Diversity.and","CM11b.Response","CM11c.Diversity.and","CM11c...Black","CM11c...Hispanic",
"CM11c...White","CM11c...Asian","CM11c..Pacific","CM11c..Native","CM11c..Twoormore","CM12a.Pay.Equality.",
"CM12a.Pay.Equality.W.1","CM13b.Wage.Level..Ra","CM13b.Response","CM14a.Risks.for.inci","CM14b.Risks.for.inci",
"CM15f.Health.and.Saf","CM2.Index","CM3.Index","CM4.Index","CM7.Index","CM9.Index","CM10.Index",
"CM11.Index","CM12.Index","CM13.Index","CM14.Index","CM15.Index","CM19.Index",
"Gov.Index","Planet.Index","People.Index","Overall.Index","CM18a.Response",
"Cleaned.Company.Name","S.P500.or.Not")]

###Removed "CM8a.TCFD.Implementa","CM8b.TCFD.Implementa",
# "CM8c.TCFD.Implementa","CM8d.TCFD.Implementa","CM8e.TCFD.Implementa","CM8f.TCFD.Implementa","CM8g.TCFD.Implementa",
# "CM8h.TCFD.Implementa","CM8i.TCFD.Implementa","CM8k.TCFD.Implementa","CM8.Index",
#
#### Adding alternative descriptve names to metric names ##########
setwd("C:/UCLAAnderson/10KfilingsTables/Data/NikithaData/RE S&P 500 companies")
alt_col_names<-read.csv("Exec_Education_col_names.csv", header=TRUE)

head(alt_col_names)
dat_mid=dat


head(dat)
library(dplyr)
library(tibble)

length(colnames(dat))
nrow(alt_col_names)

## Replacing colnames of the data which match the new exec. ed names
dat<-dat_mid %>% 
  rename_with(~deframe(alt_col_names)[.x], .cols = alt_col_names$Master.dataset.names)# %>% 
  
head(dat)
colnames(dat)
#select(Name, Reference, any_of(alt_col_names$Exec.Education.names))
#### Adding new columns calculated from others ########

dat['CM7a.Responseinmetrictons/Rev'] = dat['CM7a.Response..in.me']/dat['CM18a.Response']
dat['CM7b.TotalScope2uncategorizedmetrictons/Rev'] = dat['CM7b.Uncategorized.']/dat['CM18a.Response']

#dat['CM7b.TotalScope2uncategorizedmetrictons/Rev'] = dat['CM7b.TotalScope2uncategorizedmetrictons']/dat['CM18a.Response']
dat['CM7c.Responsemetrictons/Rev'] = dat['CM7c.Response..metri']/dat['CM18a.Response']
dat['CM9.Area.in.hectares/Rev'] = dat['CM9.Area.in.hectares']/dat['CM18a.Response']

colnames(dat)


needed<-read.csv("Base_data_updated v14.csv")
unique(needed[,c("Cleaned_names","Dimension")])
# Performance_metrics<-unique(subset(needed, Type=="Performance metrics")$Variable_name)
# Reporting_metrics<-unique(subset(needed, Type=="Reporting metrics")$Variable_name)
# QuantitativeMetrics<-unique(subset(needed, Quant.or.Qual=="Quantitative")$Variable_name)
# QualitativeMetrics<-unique(subset(needed, Quant.or.Qual=="Qualitative")$Variable_name)
# 
# vec=QualitativeMetrics
# # converting vector
# fvec <- shQuote(vec, type = "cmd")
# # combining elements using ,
# comma_vec <- paste(fvec, collapse = ", ")
# cat(comma_vec)

#########   Pivoting Longer ##############
# 
# pivot_longer(
#   data,
#   cols,
#   ...,
#   cols_vary = "fastest",
#   names_to = "name",
#   names_prefix = NULL,
#   names_sep = NULL,
#   names_pattern = NULL,
#   names_ptypes = NULL,
#   names_transform = NULL,
#   names_repair = "check_unique",
#   values_to = "value",
#   values_drop_na = FALSE,
#   values_ptypes = NULL,
#   values_transform = NULL
# )

colnames(dat)
dat_dupe<-dat
dat_dupe<-dat_dupe %>% mutate(across(where(is.double), as.character))
dat_dupe<-dat_dupe %>% mutate(across(where(is.integer), as.character))
dat_dupe$CM2d.Response<-as.numeric(as.character(dat_dupe$CM2d.Response))
library(tidyr)
colnames(dat_dupe)
dat_long<-dat_dupe %>%
  pivot_longer(!c("SEC.Filing.Name","Cleaned.Company.Name","Ticker.Symbol","GICS.Sector","GICS.Industry","Year","S.P500.or.Not"), names_to = "Variable_name", values_to = "Value",values_transform = as.numeric)
nrow(dat_long)

head(dat_long)

###Adding cleaned variable names #####

cleanVars<-read.csv("Cleaned_Variable_Names.csv")

dat_withVarNames<-left_join(dat_long, cleanVars, by=c("Variable_name"="Variable_name"))

head(dat_withVarNames)

###### Adding Revenue column #######

dat_withVarNames$Revenue<-rep(1, nrow(dat_withVarNames))###<<<-to be edited

######## Adding Dimension column ######
colnames(dat)
Governance<-colnames(dat[,c(7:31)])##chaanged colnames because the arrangement changed after TCFD merger
Planet<-colnames(dat[,c(32:48)])
People<-colnames(dat[,c(49:69)])
# Overall<-colnames(dat[,c(49:78)])
Overall<-colnames(dat[,c(70:85)])
Others<-colnames(dat[,c(86,89,90,91,92)])


gov_sub<-as.data.frame(cbind(Variable_name=Governance, Dimension=rep("Governance",length(Governance))))
ppl_sub<-as.data.frame(cbind(Variable_name=People, Dimension=rep("People",length(People))))
planet_sub<-as.data.frame(cbind(Variable_name=Planet, Dimension=rep("Planet",length(Planet))))
ovr_sub<-as.data.frame(cbind(Variable_name=Overall, Dimension=rep("Overall",length(Overall))))
others_sub<-as.data.frame(cbind(Variable_name=Others, Dimension=rep("Others",length(Others))))

Dimensions = as.data.frame(rbind(gov_sub, ppl_sub, planet_sub, ovr_sub, others_sub))


dat_dims<-left_join(dat_withVarNames, Dimensions)

############ Variable_type column ######

unique(needed[,c("Variable_type","Dimension")])
OverallLevelVariableTypeMetrics<-colnames(dat[,c(70:85)])#[,c(78:94)])
DimensionLevelVariableTypeMetrics<-colnames(dat[,c(7:69,86,89,90,91,92)])



VarType<-as.data.frame(rbind(as.data.frame(cbind(Variable_name=DimensionLevelVariableTypeMetrics, Variable_type=rep("Dimension level", length(DimensionLevelVariableTypeMetrics)))),
                            as.data.frame(cbind(Variable_name=OverallLevelVariableTypeMetrics, Variable_type=rep("Overall level", length(OverallLevelVariableTypeMetrics))))))

Type_PerformanceMetrics<-c("Number.of.Board","CM2a....economic",                       
                           "CM2a..environ","CM2a....social",
                           "CM2b.Response","CM2c.Response", 
                           "CM2d.Response","CM2f.Response.b",                      
                           "CM2g.Number","CM4a.Percent.of.empl",                 
                           "CM4a.Percent.of.busi","CM7a.Responseinmetrictons",
                           "CM7b.TotalScope2uncategorizedmetrictons","CM7c.Responsemetrictons",
                           "CM9.Area.in.hectares" ,"CM10a.Response",
                           "CM10b.Response", "CM10c.Response",
                           "CM10d.Response", "CM11a.lessthan30",                       
                           "CM11a.between3050", "CM11a.over50",  
                           "CM11b.Response", "CM11c.Black",   
                           "CM11c.Hispanic", "CM11c.White",   
                           "CM11c.Asian",    "CM11c.Pacific", 
                           "CM11c.Native",   "CM11c.Two or more",                      
                           "CM12a.Pay.Equality.W.1" ,"CM13b.Response",
                           "CM18a.Response")

Type_ReportingMetrics<-c("Audited1", "CM2a.Econ.Comps", "CM2a..Environ.Comps", "CM2a..Social.Comps", 
                         "CM2b.Governance", "CM2c.Governance", "CM2d.Governance", "CM2e.Governance", 
                         "CM2f.Governance", "CM2g.Governance", "CM2h.Governance", "CM3.Material.is", 
                         "CM4a.Anti.corruption", "CM4d.Anti.corruption", "CM7a.GHGEmissionsScope1", 
                         "CM7b.GHGEmissionsScope2", "CM7c.GHGEmissionsScope3", "CM8a.TCFD.Implementa", 
                         "CM8b.TCFD.Implementa", "CM8c.TCFD.Implementa", "CM8d.TCFD.Implementa",
                         "CM8e.TCFD.Implementa", "CM8f.TCFD.Implementa", "CM8g.TCFD.Implementa", 
                         "CM8h.TCFD.Implementa", "CM8i.TCFD.Implementa", "CM8k.TCFD.Implementa", 
                         "CM9.Land.use.and.eco", "CM10a.Water.Use..meg", "CM10b.Water.Use....o", 
                         "CM10c.Water.Use..Meg", "CM10d.Water.Use....o", "CM11a.Diversity.and", 
                         "CM11b.Diversity.and", "CM11c.Diversity.and", "CM12a.Pay.Equality.", 
                         "CM13b.Wage.Level..Ra", "CM14a.Risks.for.inci", "CM14b.Risks.for.inci",
                         "CM15f.Health.and.Saf", "GovernceBodyComposition", "MaterialIssuesImpactingStakeholders", 
                         "Anticorruption", "GHGEmissions", "TCFD", "LandUseandEcologicalSensitivity", 
                         "FreshWaterConsumptioninWaterStressedAreas", "DiversityandInclusion", "GenderPayEquality", 
                         "WageLevel", "RiskforIncidentsofChildorCompulsoryLabor", "HealthandSafety", "NetInvestmnet", 
                         "GovernancePillar", "PlanetPillar", "PeoplePillar", "Overall")
types<-as.data.frame(rbind(as.data.frame(cbind(Variable_name=Type_PerformanceMetrics, Type=rep("Performative metrics", length(Type_PerformanceMetrics)))),
                                   as.data.frame(cbind(Variable_name=Type_ReportingMetrics, Type=rep("Reporting metrics", length(Type_ReportingMetrics))))))


Quantitative<-c("CM2a.Econ.Comps", "CM2a....economic", "CM2a..Environ.Comps", "CM2a..environ", "CM2a..Social.Comps",
                "CM2a....social", "CM2d.Governance", "CM2d.Response", "CM2f.Governance", "CM2f.Response.b", 
                "CM2g.Governance", "CM2g.Number", "CM7a.GHGEmissionsScope1", "CM7a.Responseinmetrictons", 
                "CM7b.GHGEmissionsScope2", "CM7b.TotalScope2uncategorizedmetrictons", "CM7c.GHGEmissionsScope3", 
                "CM7c.Responsemetrictons", "CM9.Land.use.and.eco", "CM9.Area.in.hectares", "CM10c.Water.Use..Meg", 
                "CM10c.Response", "CM11a.Diversity.and", "CM11a.lessthan30", "CM11a.between3050", "CM11a.over50", 
                "CM11b.Diversity.and", "CM11b.Response", "CM11c.Diversity.and", "CM11c.Black", "CM11c.Hispanic",
                "CM11c.White", "CM11c.Asian", "CM11c.Pacific", "CM11c.Native", "CM11c.Two or more", "CM12a.Pay.Equality.",
                "CM12a.Pay.Equality.W.1", "CM13b.Wage.Level..Ra", "CM13b.Response")

Qualitative<-c("Audited1", "CM2b.Governance", "CM2b.Response", "CM2c.Governance", "CM2c.Response", "CM2e.Governance", 
               "CM2h.Governance", "CM3.Material.is", "CM4a.Anti.corruption", "CM4a.Percent.of.empl", "CM4a.Percent.of.busi", 
               "CM4d.Anti.corruption", "CM8a.TCFD.Implementa", "CM8b.TCFD.Implementa", "CM8c.TCFD.Implementa", "CM8d.TCFD.Implementa",
               "CM8e.TCFD.Implementa", "CM8f.TCFD.Implementa", "CM8g.TCFD.Implementa", "CM8h.TCFD.Implementa", "CM8i.TCFD.Implementa", 
               "CM8k.TCFD.Implementa", "CM10a.Water.Use..meg", "CM10a.Response", "CM10b.Water.Use....o", "CM10b.Response", 
               "CM10d.Water.Use....o", "CM10d.Response", "CM14a.Risks.for.inci", "CM14b.Risks.for.inci", "CM15f.Health.and.Saf")


QuanQual<-as.data.frame(rbind(as.data.frame(cbind(Variable_name=Quantitative, 'Quant or Qual'=rep("Quantitative", length(Quantitative)))),
                              as.data.frame(cbind(Variable_name=Qualitative, 'Quant or Qual'=rep("Qualitative", length(Qualitative))))))



AllCols_dataoutput2<-left_join(dat_dims,left_join(left_join(VarType,types),QuanQual))

colnames(AllCols_dataoutput2)

head(AllCols_dataoutput2)

Base_data_TableauPivot<-AllCols_dataoutput2[,c("SEC.Filing.Name","Cleaned.Company.Name","Ticker.Symbol","GICS.Sector","GICS.Industry","Year","Variable_name","Cleaned_names","Value","Revenue","Dimension","Variable_type","Type","Quant or Qual")]

head(Base_data_TableauPivot)

colnames(Base_data_TableauPivot)
colnames(Base_data_TableauPivot)[1]<-"CompanyName"
unique(Base_data_TableauPivot$`Ticker Symbol`)
subset(Base_data_TableauPivot, `Ticker Symbol`=="NDAQ")

length(unique(Base_data_TableauPivot$`Ticker Symbol`))
setwd("C:/UCLAAnderson/10KfilingsTables/Data/NikithaData/RE S&P 500 companies")

####Remove unnecessary NA rows:
subset(Base_data_TableauPivot,`Cleaned_names`!="NA" )
library(tidyverse)
library(tidyr)
Base_data_TableauPivot%>%drop_na(`Cleaned_names`)
Base_data_TableauPivot[!is.na(Base_data_TableauPivot$`Cleaned_names`),]

setwd("C:/UCLAAnderson/10KfilingsTables/Data/")
write.csv(subset(Base_data_TableauPivot,`Cleaned_names`!="NA" ),"August25_nonNACompCorrected_PivotedDataOutput2.csv")
length(unique(Base_data_TableauPivot$`Ticker Symbol`))

View(Base_data_TableauPivot[!complete.cases(Base_data_TableauPivot$Value),])

#####################################################################################################
#                         Ouput of Second Melted Dataset for Exec Ed - end
#####################################################################################################



### rank top 10 companies' overall index ######

ranked<-ofg_data[order(ofg_data$Overall.Index, decreasing = TRUE), ]   
colnames(ranked)
ranked_subset<-ranked[,c("Company.Name","Ticker.Symbol","GICS.Sector","GICS.Industry","Overall.Index",
          "Audited.Report","Auditor","Auditor_15_TEXT")]

AuditedRanked<-subset(ranked_subset,Audited.Report==1)
head(AuditedRanked,10)

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly")
write.csv(head(ranked_subset,10),"August252023_Top10OverallIndex_AuditStatus.csv")
write.csv(head(AuditedRanked,10),"August252023_Top10AuditedComps.csv" )
######## Have an additional output of csv pivoted for tableau ######### <<<<audited or not?? #####
### top 10 fully audited companies ######
### 0,0.5, 1#####

#########################  Merging above non-financial data with automated financial data ###########################

setwd("C:/UCLAAnderson/10KfilingsTables/Data/AutomatedFinancialData")
NetLiability<-read.csv("March29_428CompsNetliabilityOnly.csv")
colnames(NetLiability) <- paste("NetLiability", colnames(NetLiability), sep = "_")


NetRev<-read.csv("March28_311CompsNetRevenueOnly.csv")
colnames(NetRev) <- paste("NetRev", colnames(NetRev), sep = "_")

NetSale<-read.csv("March28_277CompsNetsaleOnly.csv")
colnames(NetSale) <- paste("NetSale", colnames(NetSale), sep = "_")


NetEarning<-read.csv("March28_86CompsNetEarningOnly.csv")
colnames(NetEarning) <- paste("NetEarning", colnames(NetEarning), sep = "_")


NetAssets<-read.csv("March28_83CompsNetAssetOnly.csv")
colnames(NetAssets) <- paste("NetAssets", colnames(NetAssets), sep = "_")


SGA<-read.csv("March28_293CompsNetSGAOnly.csv")
colnames(SGA) <- paste("SGA", colnames(SGA), sep = "_")

EffTax<-read.csv("March28_331CompsNetEffTaxOnly.csv")
colnames(EffTax) <- paste("EffTax", colnames(EffTax), sep = "_")

RnD<-read.csv("March28_139CompsNetR&DOnly.csv")
colnames(RnD) <- paste("RnD", colnames(RnD), sep = "_")


### Non-Financial aggregated data #######

setwd("C:/UCLAAnderson/10KfilingsTables/Data")
nonFinOFGdata<-read.csv("May2_DataOutput1TruncatedAggregation.csv")

#### Attaching CIK to non-financial data ####
setwd("C:/UCLAAnderson/10KfilingsTables/Data")
TikrCIK=read.csv("March27_2023_CIKwithTIKRComps.csv")### Create the CIK to Ticker mapping in Python by accessing the EDGAR dictionary
head(TikrCIK)
library(dplyr)
NonFin542_Cik=left_join(nonFinOFGdata,TikrCIK, by = c("Ticker.Symbol"="ticker"))
head(NonFin542_Cik)
nrow(NonFin542_Cik)
length(unique(TikrCIK$cik_str))
length(unique(NonFin542_Cik$Ticker.Symbol))

colnames(nonFinOFGdata)
colnames(NetRev)

Rev_NF<-left_join(NonFin542_Cik,NetRev, by = c("cik_str"="NetRev_cik_str"))
colnames(Rev_NF)

Sale_Rev_NF<-left_join(Rev_NF,NetSale, by = c("cik_str"="NetSale_cik_str"))
colnames(Sale_Rev_NF)
#### For Maggie's MBA class data prep for dashboards #############


setwd("C:/UCLAAnderson/10KfilingsTables/Data")
# data<-read.csv("TruncatedAggregation.csv")
data<-read.csv("February272023_NewData_NitikaJMS.csv")###does not contain NA sectors
library(dplyr)
#data$CM18a.Response<-as.numeric(as.character(data$CM18a.Response))
str(data)
SP500Means<-as.data.frame(t(data %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)))


colnames(data)
str(data)
head(data)
colnames(data)
library(tidyverse)

#### Calculating sector-wise Means across numerical metrics as well as means across all S&P 500 copanies ##########

SectorMeans=as.data.frame(t(data %>%
  group_by(GICSSector) %>%
  summarise_at(vars(-StartDate,-EndDate,-Status,-IPAddress), funs(mean(., na.rm=TRUE)))))

colnames(SectorMeans)<-SectorMeans[1,]
#colnames(SectorMeans)[1]<-"Metrics"
SectorMeansClean = SectorMeans[-1,]
SectorMeansClean$Metrics<-rownames(SectorMeansClean)
rownames(SectorMeansClean)<-NULL
#SectorMeansClean[!is.na(SectorMeansClean),]

class(SectorMeans)
SP500Means$Metrics<-rownames(SP500Means)
rownames(SP500Means)<-NULL
colnames(SP500Means)[1]<-"S&P500"

CombinedSectorSP500Means<-left_join(SectorMeansClean,SP500Means,by="Metrics")
nonNA_CombinedSectorSP500Means<-CombinedSectorSP500Means[complete.cases(CombinedSectorSP500Means),]

setwd("C:/UCLAAnderson/10KfilingsTables/Data/MaggiesClass")
write.csv(SectorMeansClean[rowSums(is.na(SectorMeansClean)) == 0, ],"Feb27_NonNASectorMeansForMBA.csv")
write.csv(SP500Means,"Feb27_S&P500ColMeansForMBA.csv")
write.csv(nonNA_CombinedSectorSP500Means,"February27_CombinedSectorSP500Means.csv")

# Sector maps for later reference from Qualtrics output
# 4 Communication Services
# 5 Consumer Discretionary
# 6 Consumer Staples
# 7 Energy
# 8 Financials
# 9 Health Care
# 10 Industrials
# 11 Information Technology
# 13 Materials
# 14 Real Estate
# 15 Utilities



agg_SectorWise<-t(aggregate(combinedData[,], list(combinedData$GICSSectorNames), mean))
agg_SectorWise[complete.cases(agg_SectorWise),]
##removing rows with entire NAs:
m=agg_SectorWise
m[rowSums(is.na(m)) != ncol(m), ]

df1=agg_SectorWise

library(dplyr)
df1 %>%
  summarise_if(is.numeric, mean)

colMeans(agg_SectorWise)
str(agg_SectorWise)
library(dplyr)
df1 %>%
  #group_by(id1, id2) %>% 
  summarise_each(funs(mean))

#### Outputs created for the MBA assignment:
#1. A pdf with the comparison of their chosen firm for selected variables  with the sector average and S&P 500
#2. A pdf or excel of the disclosure rates for all disclosures and performance quantitative variables for each sector 
# and S&P 500 (one row for each variable, and one column for each sector and one column for S&P).
# This pdf would be the same for everyone. 

### For Kelly's net zero companies:

competencies<-ofg_data[,c(18:24,32:37)]
colnames(competencies)##Looking at company boards' economic, environmental and social competencies.
# ### For Kelly, Net Zero companies:
# setwd("C:\\UCLAAnderson\\10KfilingsTables\\Data")
# NZ<-read.csv("NZ company List.csv")
# head(NZ)
# 
# NZCompetencies<-competencies[competencies$Ticker.Symbol %in% NZ$Ticker.Symbol,]
# 
# nrow(competencies)
# nrow(NZCompetencies)
# 
# write.csv(NZCompetencies,"NZCompetencies.csv")



############ For Kelly ##################
setwd("C:/UCLAAnderson/10KfilingsTables/Data")
data<-read.csv("May1_DataOutput1TruncatedAggregation.csv")
colnames(data)
head(data[,c(33:55)])
# setwd("C:/UCLAAnderson/10KfilingsTables/Data")
# data<-read.csv("April6_TruncatedAggregation.csv")
colnames(data)
data$GICS.Sector

dat<-data[,c("SEC.Filing.Name", "S.P500.or.Not","Ticker.Symbol","GICS.Sector","GICS.Industry","Year","Audited.Report",
             "Gov.Index" ,"Planet.Index" ,"People.Index","Overall.Index")]

dat$GICS.Sector

head(dat)
summary(dat)

SP500<-subset(dat,S.P500.or.Not!="No")
All500OverallIndex<-SP500[order(SP500$Overall.Index, decreasing=TRUE),]
SP500[is.na(SP500$Audited.Report),]<-0

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly/ForLaunchReport")
write.csv(All500OverallIndex,"May1_OnlySP500OverallIndex.csv",row.names=FALSE)


#### Top 10 overall index ###########


Top10Overall<-head(SP500[order(SP500$Overall.Index, decreasing=TRUE),],10)
Top10Environment<-head(SP500[order(SP500$Planet.Index, decreasing=TRUE),],10)
Top10Social<-head(SP500[order(SP500$People.Index, decreasing=TRUE),],10)
Top10Governance<-head(SP500[order(SP500$Gov.Index, decreasing=TRUE),],10)
colnames(Top10Environment)

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly/ForLaunchReport/ToBeSent")
write.csv(Top10Overall,"May1_Top10Overall.csv", row.names=FALSE)
write.csv(Top10Environment,"May1_Top10Environment.csv", row.names=FALSE)
write.csv(Top10Social,"May1_Top10Social.csv", row.names=FALSE)
write.csv(Top10Governance,"May1_Top10Governance.csv", row.names=FALSE)

write.csv(Top10Overall[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"May1_Top10OverallWithAuditStatus.csv", row.names=FALSE)
write.csv(Top10Environment[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index","Planet.Index")],"May1_Top10EnvironmentWithAuditStatus.csv", row.names=FALSE)
write.csv(Top10Social[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index","People.Index")],"May1_Top10SocialWithAuditStatus.csv", row.names=FALSE)
write.csv(Top10Governance[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index","Gov.Index")],"May1_Top10GovernanceWithAuditStatus.csv", row.names=FALSE)



### Total audited or unaudited #######
##convert NAs in audited report to no auditing=0
summary(SP500)##NA's   :34 
SP500$Audited.Report[is.na(SP500$Audited.Report)] <- 0##turned NAs in auditing to 0


AuditedCompanies<-subset(SP500, Audited.Report==1)[,c("SEC.Filing.Name","Ticker.Symbol","GICS.Sector","GICS.Industry","Year","Audited.Report","Overall.Index")]
UnAuditedCompanies<-subset(SP500, Audited.Report==0)[,c("SEC.Filing.Name","Ticker.Symbol","GICS.Sector","GICS.Industry","Year","Audited.Report","Overall.Index")]
PartiallyAudited<-subset(SP500,Audited.Report==0.5)[,c("SEC.Filing.Name","Ticker.Symbol","GICS.Sector","GICS.Industry","Year","Audited.Report","Overall.Index")]


setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly/ForLaunchReport")
write.csv(as.data.frame(unique(AuditedCompanies$SEC.Filing.Name)), "May1_AuditedCompanies.csv",row.names=FALSE)
write.csv(as.data.frame(unique(UnAuditedCompanies$SEC.Filing.Name)), "May1_UnauditedCompanies.csv",row.names=FALSE)
write.csv(as.data.frame(unique(PartiallyAudited$SEC.Filing.Name)), "May1_PartiallyAuditedCompanies.csv",row.names=FALSE)

####Percentage of companies partially or fully audited######

(((length(unique(AuditedCompanies$Ticker.Symbol)))+(length(unique(PartiallyAudited$Ticker.Symbol))))/((length(unique(AuditedCompanies$Ticker.Symbol)))+(length(unique(PartiallyAudited$Ticker.Symbol)))+(length(unique(UnAuditedCompanies$Ticker.Symbol)))))*100

##186/500
186+314
186/500
length(unique(SP500$Ticker.Symbol))

####### Percentage of overall disclosures #######

mean(SP500$Overall.Index,na.rm=TRUE)
mean(SP500$Planet.Index,na.rm=TRUE)
mean(SP500$People.Index,na.rm=TRUE)
mean(SP500$Gov.Index,na.rm=TRUE)


nrow(SP500)

UnAuditedCompanies[duplicated(UnAuditedCompanies$Ticker.Symbol),]

nrow(AuditedCompanies)#40
nrow(UnAuditedCompanies)#358
nrow(PartiallyAudited)#162

length(unique(AuditedCompanies$Ticker.Symbol))#40
length(unique(UnAuditedCompanies$Company.Name))#358
length(unique(PartiallyAudited$Ticker.Symbol))#161

subset(data, Ticker.Symbol=="STT")$Company.Name

DuplicatedTIKRS<-subset(UnAuditedCompanies,Ticker.Symbol=="NWSA"|Ticker.Symbol == "ODFL"|Ticker.Symbol == "PBF"|Ticker.Symbol == "SEE"|Ticker.Symbol == "LW"
       |Ticker.Symbol == "SNA"|Ticker.Symbol == "TSCO"|Ticker.Symbol == "GPS"|Ticker.Symbol == "SFM")

Duplicates<-DuplicatedTIKRS[order(DuplicatedTIKRS$Ticker.Symbol, decreasing=TRUE),]
subset(Duplicates, Year=="2020")

nrow(DuplicatedTIKRS)
unique(DuplicatedTIKRS$Year)
nrow(PartiallyAudited)

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly/ForLaunchReport")
write.csv(AuditedCompanies,"May1_AuditedData39Rows.csv")
write.csv(UnAuditedCompanies,"UnAuditedData352Rows.csv")
write.csv(PartiallyAudited,"PartiallyAuditedData160Rows.csv")

write.csv(unique(AuditedCompanies$Company.Name),"Audited39Companies.csv")
write.csv(unique(UnAuditedCompanies$Company.Name),"UnAudited354Companies.csv")
write.csv(unique(AuditedCompanies$Company.Name),"PartiallyAudited161Companies.csv")


####### Audit status based top tens #######

PartialFullAudited<-subset(dat, Audited.Report>0)
FullAudited<-subset(dat, Audited.Report>0.5)


PartialFullAudited_Top10Overall<-head(PartialFullAudited[order(PartialFullAudited$Overall.Index, decreasing=TRUE),],10)
PartialFullAudited_Top10Environment<-head(PartialFullAudited[order(PartialFullAudited$Planet.Index, decreasing=TRUE),],10)
PartialFullAudited_Top10Social<-head(PartialFullAudited[order(PartialFullAudited$People.Index, decreasing=TRUE),],10)
PartialFullAudited_Top10Governance<-head(PartialFullAudited[order(PartialFullAudited$Gov.Index, decreasing=TRUE),],10)

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly/ForLaunchReport")
write.csv(PartialFullAudited_Top10Overall,"April27_PartialFullAudited_Top10Overall.csv", row.names=FALSE)
write.csv(PartialFullAudited_Top10Environment,"April27_PartialFullAudited_Top10Environment.csv", row.names=FALSE)
write.csv(PartialFullAudited_Top10Social,"April27_PartialFullAudited_Top10Social.csv", row.names=FALSE)
write.csv(PartialFullAudited_Top10Governance,"April27_PartialFullAudited_Top10Governance.csv", row.names=FALSE)

write.csv(PartialFullAudited_Top10Overall[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_PartialFullAudited_Top10OverallWithAuditStatus.csv", row.names=FALSE)
write.csv(PartialFullAudited_Top10Environment[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_PartialFullAudited_Top10EnvironmentWithAuditStatus.csv", row.names=FALSE)
write.csv(PartialFullAudited_Top10Social[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_PartialFullAudited_Top10SocialWithAuditStatus.csv", row.names=FALSE)
write.csv(PartialFullAudited_Top10Governance[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_PartialFullAudited_Top10GovernanceWithAuditStatus.csv", row.names=FALSE)



FullAudited_Top10Overall<-head(FullAudited[order(FullAudited$Overall.Index, decreasing=TRUE),],10)
FullAudited_Top10Environment<-head(FullAudited[order(FullAudited$Planet.Index, decreasing=TRUE),],10)
FullAudited_Top10Social<-head(FullAudited[order(FullAudited$People.Index, decreasing=TRUE),],10)
FullAudited_Top10Governance<-head(FullAudited[order(FullAudited$Gov.Index, decreasing=TRUE),],10)

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly/ForLaunchReport")
write.csv(FullAudited_Top10Overall,"April27_FullAudited_Top10Overall.csv", row.names=FALSE)
write.csv(FullAudited_Top10Environment,"April27_FullAudited_Top10Environment.csv", row.names=FALSE)
write.csv(FullAudited_Top10Social,"April27_FullAudited_Top10Social.csv", row.names=FALSE)
write.csv(FullAudited_Top10Governance,"April27_FullAudited_Top10Governance.csv", row.names=FALSE)

write.csv(FullAudited_Top10Overall[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_FullAudited_Top10OverallWithAuditStatus.csv", row.names=FALSE)
write.csv(FullAudited_Top10Environment[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_FullAudited_Top10EnvironmentWithAuditStatus.csv", row.names=FALSE)
write.csv(FullAudited_Top10Social[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_FullAudited_Top10SocialWithAuditStatus.csv", row.names=FALSE)
write.csv(FullAudited_Top10Governance[,c("SEC.Filing.Name","Ticker.Symbol","Year","Audited.Report","Overall.Index")],"April27_FullAudited_Top10GovernanceWithAuditStatus.csv", row.names=FALSE)

colnames(data)

#### Exactly S&P500 companies that were selected for launch #######

setwd("C:/UCLAAnderson/10KfilingsTables/Data")
launchdata<-read.csv("May2_DataOutput1TruncatedAggregation.csv")

length(unique(launchdata$Company.Name))

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly")
write.csv(unique(launchdata$Company.Name), "S&P500Companies_singleYears_ForLaunch.csv")
############# For Maggie ##############

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly")
CompYr<-read.csv("April21_CompanyTIKRnYear.csv")


CompYr$Value=rep(1, nrow(CompYr))
library(tidyr)
Pivoted_CompYr<-CompYr %>%
  pivot_wider(names_from = Year, values_from = Value)

Pivoted_CompYr[is.na(Pivoted_CompYr)]<-0

setwd("C:/UCLAAnderson/10KfilingsTables/Data/ForKelly")
write.csv(Pivoted_CompYr, "April21_Pivoted_CompTIKRYr.csv")


