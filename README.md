# Regis-692-August-2020---COVID-19
Video Presentation: https://youtu.be/zXCSipizXFw

The Exploration and Analysis files contain the markup code. 

Mapping of Cluster.R maps the clusters created in the most recent Exploration and Analysis files.



This is my project in detecting the effect public transportation systems have on the spread of COVID-19. 

The files progress from building the dataset using .xlsx and .csv files, to exploration, to analysis. 

The data are gathered from publicly available sources. Case numbers are from New York Times data found on Kaggle. Public transportation information from the US Department of Transportation. Other county and urban area data are gathered from the US Census Bureau.

The method chosen for this analysis is linear regression, linear regression using panel data and random forest techniques. These techniques are able to show if some feature has an effect, how strong that effect is and how certain that measurement is. Cluster analysis is used to look into how the data may be clustered and if ideas for future research could be found. 

Public transportation shares many features with locations where super spreading events occurred. Among these are an enclosed space, a large number of people and potentially a long period of time spent in that environment. There are mitigating factors in the spread of COVID-19 in public transportation. Frequent stops, opening and closing of doors, people getting and off the vehicle and so on. 

The data exploration phase showed that it was possible to detect the effect a public transportation system has on COVID-19 spread. Low multicolinearity across the data suggested linear regression could be a good method. Early experiments with linear regression did find statistically significant results associated with public transportation systems. The feature selected to be the feature of interest is average passenger miles per trip. This captures the number of rides as well as the length of the rides. Typically low and positive coefficients. If this early result remained throughout the analysis it would suggest there is a low possibility of infection. The coefficients were so low, however, that thousands of rides would be required to put at any specific individual at risk.

The more thorough analysis failed to deliver a reliable result. While the early results were shown to be significant even when the data was cleansed of outliers and other unreliable observations; the results were highly sensitive to specificiation. Adding or removing features had an important effect on the sign, size and significance of the public transportation coefficient. Leading to the conclusion that no effect could be found through linear regression methods. The random forest (RF) method showed that average length of passenger trips was an important component of the RF model. The weakness of RF is that it does not provide a measure of whether or not the effect is positive, directly linked to the feature or if this feature is simply useful for determining the number of cases that an area is expected to have. 

In the cluster analysis phase it was found that one particular cluster had a high average of passenger miles per trip. These counties were unusual in that they are not what was expected. High, but not the highest population density, good health, low per-person infection rate, long stay at home orders, and a high number of restaurants (a proxy for entertainment venues). This cluster appeared in four US states. California, Texas, Florida and New York.

Based on this analysis there is no indication that public transportation is an important source of COVID-19 infection spread. Linear analysis was unable to find a statistically significant result, nor did it return a consistently large coefficient across different analyses. Random forest did find that public transportation was an important component of predicting case numbers, however public transportation use can be an indicator of other behaviors that are important to the spread of COVID-19.

Feature List

FIPS: County FIPS Code

Variable: Cases in a month

Cases: number of COVID Cases detected

VX.cases: Number of cases in the numbered month V(month).cases

ctystid: County and State

County: County name

State: State Abbreviation

CAREALAND: County area by square miles

YPLLR or Years of Potential Life Lost Rate: Total number of years of life lost to early death

POPESTIMATE2018: Estimate of the county population in 2018

Restaurants: Number of restaurants in the state

VX.rides: Number of public transportation passenger rides 

Avg_PMiles_Trip: Average passenger miles per trip

SRVC_AREA_SQML: Transportation district square mileage

SRVC_AREA_POP: Population of the transportation district

Pass_Miles: Annual Passenger Miles

U_P_Trips: Unlinked passenger trips per year

Days_at_home: Total government mandated days at home

Res_per_Person: Number of restaurants in state divided by county population

Cases_per_Person: Number of cases per person by county

Pop_Dense: Population density by county

X_Cases_per_Person: Number of cases per person in June by county

