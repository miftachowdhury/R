#' ---
#' title: "R coding for public policy"
#' subtitle: "Final Part II"
#' author: "Mifta Chowdhury|mc6913"
#' output: word_document
#' ---
#' 
## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' Instruction:
#' 
#'   Design an R function for policy analysis.
#'   
#' 
#' #Import and clean your dataset of choice
## ------------------------------------------------------------------------
#Step 1: create data frame 1 using opioid related deaths dataset
#dataset 1 description - New York State Vital Statistics: Opioid-Related Deaths by County: Beginning 2003
#dataset 1 selected metadata (from download page):
  #modified: 2019-09-30
  #release date: 2017-11-16

#import opioid data (dataset 1 - opioid deaths 2003-present)
opd <- read.csv("https://health.data.ny.gov/api/views/sn5m-dv52/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)

#check opioid data
  str(opd)
  #dataset contains 3 variables; 930 records/62 NYS counties (personal knowledge) = 15 years; 2003-2017
  #deaths column name kind of long, but column names are clear; no real need to rename columns
  
  summary(opd)
  #year min=2003;  year max=2017
  
  table(opd$Year)
  #all 15 years include 62 counties each
  
  range(table(opd$County))
  mean(table(opd$County))
  #don't want to print 62 counties; county range and mean confirm all 62 counties include 15 years each

  range(is.na(opd))
  mean(is.na(opd))
  #range and mean of NA confirm no missing data
  
  opd[opd$County=="St Lawrence", "County"] <- "St. Lawrence"
  #need to match with second dataset
  table(opd$County)
  

#############################################################################  
  
#Step 2: create data frame 2 using population data
#dataset 2 description - Annual Population Estimates for New York State and Counties: Beginning 1970
#dataset 2 selected metadata:
  #data last updated: April 19, 2019
  #date created: August 14, 2014
  
#import population data (dataset 2 - resident population of New York State and counties beginning 1970)
pop <- read.csv("https://data.ny.gov/api/views/krt9-ym2k/rows.csv?accessType=DOWNLOAD", stringsAsFactors = FALSE)
  
#check population data
  str(pop)
  summary(pop)
    #dataset contains 3402 obs. of 5 variables
    #year min=1970; year max=2018
    
            #LATER REALIZED I NEED "PROGRAM TYPE" VARIABLE SO I DROPPED THE BELOW CODE
            #pop <- pop[c(2,3,5)]
            #str(pop)
            #just keep 3 variables: Geography, Year, Population
    
  pop <- subset(pop, Year %in% 2003:2017)
  summary(pop)
  str(pop)
    #just keep observations from 2003 to 2017 inclusive (to match opioid deaths dataset)
    #1,008  observations of 3 variables
  
  length(unique(pop$Geography))
    #63 counties
  
  length(unique(pop$Year))
    #15 years
  
  #questions:
    #Q1) why 63 geographies instead of 62?
    #Q2) why 1,008 observations (=63x16) when # unique years = 15?
  
  table(pop$Geography)
    #A1) because the 63rd geography is New York State; keep this for later
  
  table(pop$Geography, pop$Year)
    #A2) each county has two observations for 2010; one is the official decennial census count
    #but should drop the census count 2010 data for consistency
  
  table(pop$Program.Type)
    #want to drop Program Type="Census Base Population"
  
  pop<-subset(pop, Program.Type != "Census Base Population")
  str(pop)
    #dropped observations where Program Type= "Census Base Population"
    #now we have 930 records (62 counties x 15 years) that should correspond to records in opioid dataset and an additional 15 records for New York State totals per year - total 945 records.
  
  i<-1
  for (i in 1:dim(pop)){
      if (pop[i,2]!="New York State"){
      pop[i,2] <- substr(pop[i,2], 1, nchar(pop[i,2])-7)
      i<-i+1
    }
  }
  #drop the " County" substring from the data because it is extraneous and to prep for merge
  table(pop$Geography)
    

  names(pop)[names(pop)=="Geography"] <- "County"
    #rename the "Geography" column to "County" to prep for merge
  
  str(pop)
  
  range(is.na(pop))
  mean(is.na(pop))
  #range and mean of NA confirm no missing data
  
#############################################################################

#Step 3: merge data frame 1(opd) and data frame 2(pop) on County, to create a new merged data frame

#merge the data frames
  opd2 <- merge(opd, pop, all=TRUE, sort=TRUE)
  str(opd2)
  #15 extra records; "St. Lawrence" is spelled differently; go back and make spellings match

#############################################################################
  
#Step 4: create new variables in the new merged data frame
  
   y<-2003
  for (y in 2003:2017){
    opd2$Opioid.Poisoning.Deaths[which(opd2$County=="New York State" & opd2$Year==y)] <- sum(opd2$Opioid.Poisoning.Deaths[which(opd2$County!="New York State" & opd2$Year==y)])
  }
  #calculated total opioid-related deaths in NYS per year (which wasn't in the original opd dataset)
  
  opd2$PopMillion <- opd2$Population/1000000
  opd2$Death.Rate <- opd2$Opioid.Poisoning.Deaths/opd2$PopMillion
  #calculated opioid-related death rate per million people

y<-2003
i<-1
for (y in 2003:2017){
  for (i in 1:dim(opd2)){
    ifelse(opd2[i,1]==y, opd2$Total.Deaths[i] <- opd2$Opioid.Poisoning.Deaths[which(opd2$County=="New York State" & opd2$Year==y)], opd2$Total.Deaths<-opd2$Total.Deaths)
    ifelse(opd2[i,1]==y, opd2$Total.Pop[i] <- opd2$Population[which(opd2$County=="New York State" & opd2$Year==y)], opd2$Total.Pop<-opd2$Total.Pop)
  }
  y<-y+1
}

opd2$Pct.Deaths<-opd2$Opioid.Poisoning.Deaths*100/opd2$Total.Deaths
opd2$Pct.Pop <- opd2$Population*100/opd2$Total.Pop



#' 
#' 
#' #what is the goal of the function
#' 
#' The goal of my function is to allow a user to input the name of a New York State county, as well as two different years between 2003 and 2017 and for both county and state return: 
#' 1) Rates of opioid-related deaths per million people for each of the two years
#' 2) The percent change between the two years, including whether the rate increased or decreased
#' 
#' Additionally, I would also like to return for each of the two years:
#' 3) Percentage of total statewide deaths accounted for by the county
#' 4) Percentage of total statewide population accounted for by the county
#' 
#' 
#' #function script
#'   Step 1. name the function
#'   Step 2. calling function() with "{ }"
#'   Step 3. Start programming the expressions within the "{ }"
#' 	  Make sure to have a returned value
#'   Step 4. fill out the formals
#'   Step 5. run the function script
#'   Step 6. test the function
## ------------------------------------------------------------------------
#############################################################################

#Step 5: create the function

op_deaths <- function(data, county, year1, year2){
attach(data)
#year 1
  opd1C <- subset(data, County==county & Year==year1)
  opd1S <- subset(data, County=="New York State" & Year==year1)

#year 2
  opd2C <- subset(data, County==county & Year==year2)
  opd2S <- subset(data, County=="New York State" & Year==year2)

#percent change
  pct.chgC <- round((opd2C$Death.Rate - opd1C$Death.Rate)*100/opd1C$Death.Rate, 1)
  pct.chgS <- round((opd2S$Death.Rate - opd1S$Death.Rate)*100/opd1S$Death.Rate, 1)
  chg.typeC <- ifelse(pct.chgC>=0, "increased by ", "decreased by ")
  chg.typeS <- ifelse(pct.chgS>=0, "increased by ", "decreased by ")
  
#year 1 rates per million people
  y1rateC <- round(opd1C$Death.Rate,1)
  y1rateS <- round(opd1S$Death.Rate,1)
  
#year 1 percentages 
  y1pct.deaths <- round(opd1C$Pct.Deaths,1)
  y1pct.pop <- round(opd1C$Pct.Pop,1)
  
#year 2 rates per million people
  y2rateC <- round(opd2C$Death.Rate,1)
  y2rateS <- round(opd2S$Death.Rate,1)

#year 2 percentages
  y2pct.deaths <- round(opd2C$Pct.Deaths,1)
  y2pct.pop <- round(opd2C$Pct.Pop,1)


detach(data)
  
  cat("In",year1,":\n", 
      "There were",y1rateC,"opioid-related deaths per million people in",county,"County.\n", 
      "There were",y1rateS,"opioid-related deaths per million people in New York State.", "\n\n", 
      "In",year2,":\n",
      "There were",y2rateC,"opioid-related deaths per million people in",county,"County.\n", 
      "There were",y2rateS,"opioid-related deaths per million people in New York State.","\n\n",
      "In",year1,",",county,"County accounted for",y1pct.deaths,"percent of opioid-related deaths in New York State and",y1pct.pop,"percent of the state's total population.\n", 
      "In",year2,",",county,"County accounted for",y2pct.deaths,"percent of opioid-related deaths in New York State and",y2pct.pop,"percent of the state's total population.\n\n", 
      "From",year1,"to",year2,"the opioid-related death rate",chg.typeC,pct.chgC,"percent in",county, "County.\n",
      "From",year1,"to",year2,"the opioid-related death rate",chg.typeS, pct.chgS,"percent in New York State.")
}


#' 
#' 
#' 
#' #function test
## ------------------------------------------------------------------------

op_deaths(opd2, "Kings", 2003, 2008)


#' 
#' 
#' #did your function meet the goal of your design?
#' 
#' Yes, my function met the goals of my design.
