setwd("C:/Users/snlpt/Desktop/MSDS 692/datas")

library(readxl)
library(xlsx)
library(tidyverse)
library(DataExplorer)
library(lubridate)
library(tidyr)
library(stringr)
library(reshape2)
library(plm)
library(rd)

counter <- 1

#Import data
County_Health_Ranked <- read_excel("C:/Users/snlpt/Desktop/MSDS 692/Datas/2020 County Health Rankings Data - v2.xlsx", sheet = "Ranked Measure Data", range = "A1:P3195")

Restaurant_Association <- read_excel("C:/Users/snlpt/Desktop/MSDS 692/Datas/Restaurant Association.xlsx")


County_FIPS_Years_of_life <- read_excel("C:/Users/snlpt/Desktop/MSDS 692/Datas/2020 County Health Rankings Data - v2.xlsx", sheet = "Ranked Measure Data", col_names = TRUE)
General_County_Data <- read_csv("C:/Users/snlpt/Desktop/MSDS 692/Datas/General County Data.csv")

urban_area_to_county <- read_delim("C:/Users/snlpt/Desktop/MSDS 692/Datas/urban area to county.txt", "\t", escape_double = FALSE, trim_ws = TRUE)

us_counties_covidcounts <- read_csv("C:/Users/snlpt/Desktop/MSDS 692/Datas/us-counties-covidcounts.txt")

Pubtrans_Master <- read_excel("C:/Users/snlpt/Desktop/MSDS 692/Datas/Pubtrans - May 2020 Adjusted Database_1.xlsx", sheet = "MASTER")

Pubtrans_UPT <- Pubtrans_May_2020_Adjusted_Database_1 <- read_excel("C:/Users/snlpt/Desktop/MSDS 692/Datas/Pubtrans - May 2020 Adjusted Database_1.xlsx", sheet = "UPT")

state_policy_updates <- read_csv("C:/Users/snlpt/Desktop/MSDS 692/Datas/state_policy_updates.csv")

####functions
############################remove_cumulative

#the case counts in the NYTIMES data is cumulative, remove cumulative so we can get monthly total new cases
remove_cumulative <- function(x){
  #x <- us_counties_covidcounts %>% group_by(fips)
  new_cases <- tibble(date = Date(), cases = numeric(), deaths=numeric())
  i <- 1

  if ((counter %% 100) == 0) print(counter)
  counter <- counter+1
  
  while(i<=nrow(x)){ #1000 for testing
    if (i==1){
      new_cases[1,] <- x[1,c("date", "cases", "deaths")]
      if ((counter %% 100) == 0) print(counter)
      counter <- counter+1
      
    }else{
      c_df <- x$cases[i]-x$cases[(i-1)]
      d_df <- x$deaths[i]-x$deaths[(i-1)]
      dt_df <- x$date[i]
      if(c_df == -1) {c_df = 0}
      #print("add row")
      new_cases <- add_row(new_cases, date = dt_df, cases=c_df, deaths=d_df)
    }
    #print("new loop")
    i<-i+1
    if(i%%1000 == 0) print(i)
  }
  return(new_cases)
}


##################

fill_out_fips <- function(x){
  
  #x<-as.character(urban_area_to_county$FIPS)
  j <- 1
  y <- vector(mode = "character", length = 0)
  
  
  while (j <= length(x)){
    fips_hold <- x[j]
    while (nchar(fips_hold) < 5){
      fips_hold <- paste0("0", x[j])
      
    }
    y[j] <- fips_hold
    j <- j+1
    
  }
  
  y <- as.character(y)
  return(y)
  
}

################# difference between start and stop dates


difference_days <- function(x){
  x <- unique(x[,c("FIPS", "ss_date", "start_stop")])
  x <- unique(policy_by_county[,c("FIPS", "ss_date", "start_stop")])
  
  i = 1
  j = 2
  found = TRUE
  #take passed through data fips, date, start/stop
  source_data <- x
  #new data.frame with fips, days_at_home
  holder_FIPS <- matrix(nrow = nrow(x))
  holder_days_at_home <- matrix(nrow = nrow(x))

  
  time_at_home <- data.frame(FIPS = as.character(), days_at_home = as.numeric())
  #take next fips add to new fips in data frame, loop until find next fips
  while(i <= nrow(source_data)){
    #if next fips can't be found, set found_end_date to false
    print(i)
    while(((source_data$FIPS[j] != source_data$FIPS[i]) && (j <= nrow(source_data)))){
      j = j+1
    }
    #if found_end_date is true take date positions, if false take todays date
    #set dates to right format, take ABS time difference
    if(j==nrow(source_data)) {time_between <- as.double(difftime(Sys.Date(), source_data$ss_date[i],  units="days"))

      }else {time_between <- as.double( difftime(source_data$ss_date[j], source_data$ss_date[i], units = "days")) #https://stackoverflow.com/questions/14454476/get-the-difference-between-dates-in-terms-of-weeks-months-quarters-and-years/29785779
      
      source_data[-j,] #delete found row
      }
    
      #add date difference to days at home
      
      holder_days_at_home[i] <- time_between
      holder_FIPS[i] <- source_data$FIPS[i]
    i=i+1
    j=i+1
  }
  
  time_at_home <- data.frame(days_at_home = holder_days_at_home, FIPS = holder_FIPS) %>% na.omit()
  
  
  
  #look for duplicates, if duplicate add differences together
  
#  k=1
 # l=2
  #while(k <= nrow(time_at_home)){
   # while(l <= nrow(time_at_home)){
      #overwrite the first date difference, delete the duplicate row
    #  if(time_at_home$FIPS[k]==time_at_home$FIPS[l]){
     #   time_at_home$days_at_home[k] <- time_at_home$days_at_home[k] + time_at_home$days_at_home[l]
      #  time_at_home <- time_at_home[-l,]
      #}
  #  l+1
   # }
    #k+1
    #l=k+1
  #}
  ###
 # rm(x, source_data, time_at_home, i, j, k, l, holder_FIPS, holder_days_at_home)
  
return(time_at_home)
}

#create identifiers that are linked to one another

County_FIPS_Years_of_life$State <- as.character(sapply(County_FIPS_Years_of_life$State, function (x) setNames(state.abb, state.name)[x]))

County_FIPS_Years_of_life <- County_FIPS_Years_of_life %>% left_join(Restaurant_Association, by = "State")

County_FIPS_Years_of_life$ctystid <-  with(County_FIPS_Years_of_life, paste(County, State, sep = ","))

full_data_set <- County_FIPS_Years_of_life[,c("FIPS", "ctystid", "Restaurants","State")]

final_key_set <- County_FIPS_Years_of_life[,c("FIPS", "ctystid","County","State")]

final_key_set <- final_key_set[complete.cases(final_key_set[ , 1:4]),]

urban_area_to_county <- rename(urban_area_to_county, FIPS = GEOID)

#fill out GEO ID's to match FIPS length with zeroes

urban_area_to_county$FIPS <- as.character(urban_area_to_county$FIPS)

urban_area_to_county$FIPS <- fill_out_fips(urban_area_to_county$FIPS)

urban_area_to_county$UANAME <- sapply(urban_area_to_county$UANAME, function(x) trimws(x))

urban_area_to_county$STATE <- sapply(urban_area_to_county$STATE, function(x) trimws(x))

urban_area_to_county$UANAME <- with(urban_area_to_county, paste(UANAME, STATE, sep = ","))

full_data_set <- right_join(full_data_set, urban_area_to_county[,c("FIPS", "UANAME", "CAREALAND")], by = "FIPS")

set_of_keys <- full_data_set


#add health data years lost

full_data_set <- full_data_set %>% left_join(County_Health_Ranked[,c("FIPS", "Years of Potential Life Lost Rate")])


#Link urban area and general county by county/state. No consistent id's between these two datasets

General_County_Data$STNAME <- as.character(General_County_Data$STNAME)

STNAME <- as.character(sapply(General_County_Data$STNAME, function (x) setNames(state.abb, state.name)[x]))

General_County_Data <- rename(General_County_Data, CNAME = CTYNAME)

CNAME <- as.character(General_County_Data$CNAME)

CNAME <- CNAME %>% sapply(function(x) str_remove(x, " County")) %>% as.character()

GCD_Hold <- tibble(CNAME = CNAME, STNAME = STNAME, POPESTIMATE2018 = General_County_Data$POPESTIMATE2018)

GCD_Hold$ctystid <- with(GCD_Hold, paste(CNAME, STNAME, sep = ","))

full_data_set <- left_join(full_data_set, GCD_Hold[,c("POPESTIMATE2018","ctystid")], by = "ctystid")

full_data_set <- full_data_set[complete.cases(full_data_set), ]


#### PUblic transit data processing


PUBTRANS_HOLD <- inner_join(Pubtrans_Master, Pubtrans_May_2020_Adjusted_Database_1[,c("UZA", "JAN20", "FEB20", "MAR20",  "APR20")], by = "UZA")

PUBTRANS_HOLD <- PUBTRANS_HOLD[,c(1,11:15,19,20,26:29)]

names(PUBTRANS_HOLD) <- c("fivedigit", "UANAME", "UZA_SQ_MILES", "UZA POP", "SRVC_AREA_SQML", "SRVC_AREA_POP", "Pass_Miles", "U_P_Trips", "V1.rides", "V2.rides", "V3.rides", "V4.rides")

PUBTRANS_HOLD$fivedigit <- as.character(PUBTRANS_HOLD$fivedigit)

PUBTRANS_HOLDmerge <- aggregate(cbind(V1.rides, V2.rides, V3.rides, V4.rides, Pass_Miles, U_P_Trips)~ UANAME, data=PUBTRANS_HOLD,  FUN=sum)

PUBTRANS_HOLDmerge$Avg_PMiles_Trip <- PUBTRANS_HOLDmerge$Pass_Miles/PUBTRANS_HOLDmerge$U_P_Trips

PUBTRANS_HOLD <- distinct(PUBTRANS_HOLD[,c(1:6)])



#bring together hold and merge

#set_of_keys1 <- set_of_keys

PUBTRANS_HOLD <- full_join(PUBTRANS_HOLDmerge, PUBTRANS_HOLD, by = "UANAME")

PUBTRANS_HOLD$UANAME <- sapply(PUBTRANS_HOLD$UANAME, function(x) str_replace(x,"\\s", "")) #remove space in UANAME

full_data_set <- full_data_set %>% left_join(PUBTRANS_HOLD[,c("UANAME","V1.rides", "V2.rides", "V3.rides", "V4.rides", "Pass_Miles", "U_P_Trips", "Avg_PMiles_Trip", "SRVC_AREA_SQML", "SRVC_AREA_POP")], by="UANAME")

#Do your thing

final_data_sums <- full_data_set %>% aggregate(cbind(SRVC_AREA_SQML,SRVC_AREA_POP, Pass_Miles, U_P_Trips)~FIPS, data=., FUN=sum) #. or something else? 

final_data_avgs <- full_data_set %>% aggregate(cbind(CAREALAND,`Years of Potential Life Lost Rate`, POPESTIMATE2018, Restaurants) ~FIPS, data=., FUN=mean) #. or something else? 

final_data_avgs2 <- full_data_set %>% aggregate(cbind(V1.rides, V2.rides, V3.rides, V4.rides, Avg_PMiles_Trip) ~FIPS, data=., FUN=mean)

#full_data_set <- full_data_set[complete.cases(full_data_set), ]

final_data_assemble <- final_key_set %>% inner_join(final_data_avgs, by="FIPS") %>% full_join(final_data_avgs2, by="FIPS")%>% full_join(final_data_sums, by="FIPS") 

timeseries_bucket <- final_data_assemble[,c("FIPS", "V1.rides", "V2.rides" , "V3.rides", "V4.rides")]

#get covid cases and deaths by months

us_counties_covidcounts <- us_counties_covidcounts %>% group_by(fips)

#uscntscvdcnts <- us_counties_covidcounts %>% group_modify(~ remove_cumulative(.)) #stops here - add an export

#######adapt

#write.csv(us_counties_covidcounts, file = "uscntscvdcnts.csv") #section above takes several minutes to run export to save time

uscntscvdcnts <- read_csv("C:/Users/snlpt/Desktop/MSDS 692/Datas/uscntscvdcnts.csv")

######adapt

uscntscvdcnts <- uscntscvdcnts %>% group_by(fips) %>% group_modify(~ aggregate(cbind(cases)~month(date),data=.,FUN=sum))# something else? 

uscntscvdcnts$fips <-  lapply(as.character(uscntscvdcnts$fips),fill_out_fips)

uscntscvdcnts_cases <- uscntscvdcnts[,c("fips", "month(date)","cases")]

names(uscntscvdcnts_cases) <- c("FIPS", "month(date)", "cases") #keeping to month(date) because I don't want to mess with my already lengthy and confusing processing

#uscntscvdcnts$`month(date)` <- as.character(uscntscvdcnts$`month(date)`)

uscntscvdcnts_cases <- uscntscvdcnts %>% spread('month(date)', cases)

names(uscntscvdcnts_cases) <- c("FIPS","V1.cases", "V2.cases", "V3.cases", "V4.cases","V5.cases","V6.cases","V7.cases")

uscntscvdcnts_cases <- uscntscvdcnts_cases %>% ungroup() %>% mutate(V1.cases = coalesce(V1.cases, 0), V2.cases= coalesce(V2.cases, 0), V3.cases = coalesce(V3.cases, 0), V4.cases = coalesce(V4.cases, 0), V5.cases = coalesce(V5.cases, 0), V6.cases = coalesce(V6.cases, 0), V7.cases = coalesce(V7.cases, 0), FIPS = FIPS)

final_data_assemble <- uscntscvdcnts_cases %>% right_join(final_data_assemble, by = "FIPS")

test <- uscntscvdcnts_cases %>% melt()

test <- test %>% ungroup() %>% mutate(cases = coalesce(value, 0))

#test <- test %>% rename(FIPS = fips)

#panel_final <- test %>% right_join(final_data_assemble, by = "FIPS")

namethis <- names(final_data_assemble)

namethis <- namethis[c(1:8,10:15,20:24)]

panel_final <- melt(final_data_assemble[,namethis], id.vars = c("FIPS", "County", "State", "CAREALAND", "Years of Potential Life Lost Rate", "POPESTIMATE2018", "Restaurants", "Avg_PMiles_Trip"))

panel_final <- rename(panel_final, months = variable, cases = value)

timeseries_bucket <- uscntscvdcnts_cases %>% left_join(timeseries_bucket, by = "FIPS")

#############################################################################

#State and County Policies

state_policy <- state_policy_updates[,c("state_id", "policy_level","date","start_stop","policy_type")] %>% rename(State=state_id) %>% filter(policy_level=="state") %>% filter(policy_type=="Shelter in Place") 

county_policy <- state_policy_updates[,c("state_id","fips_code","policy_level","date","start_stop", "policy_type")] %>% rename(State=state_id, FIPS = fips_code) %>% filter(policy_level=="county") %>% filter(policy_type=="Shelter in Place")

county_policy$FIPS <- as.character(county_policy$FIPS) %>% fill_out_fips()

policy_by_county <- set_of_keys %>% 
  left_join(county_policy, by = "FIPS") %>% mutate(State = coalesce(State.x, State.y)) %>% select(-State.x, -State.y) 

policy_by_county <- policy_by_county %>% left_join(state_policy, by = "State") %>% mutate(date = coalesce(date.x, date.y)) %>% mutate(start_stop = coalesce(start_stop.x, start_stop.y)) %>% select(-date.x, -date.y, -start_stop.x, -start_stop.y, -policy_level.x, -policy_level.y, -policy_type.x, -policy_type.y) 

policy_by_county$date <- as.Date(policy_by_county$date, "%m/%d/%y")

policy_by_county <- policy_by_county %>% rename(ss_date = date)

policy_by_county <- unique(policy_by_county[order(policy_by_county$ss_date),])

policy_by_countyt <- policy_by_county %>% difference_days()

panel_final <- panel_final %>% left_join(policy_by_countyt, by = "FIPS")

panel_final$Avg_PMiles_Trip <- replace_na(panel_final$Avg_PMiles_Trip, 0)

#rm(policy_by_county)

#############################################################################

write.xlsx(final_data_assemble, file = "final_data_assemble08012020.xlsx")

write.csv(panel_final, file = "final_data_assemble_ts08102020.csv")
