################################################################################
########################### Pre-processing #####################################
################################################################################

# ------------- Task characteristics -----------------------------------
groupSize <- 4
ntrials <- 10
pi <- 1.4 # multiplication factor in game
ntokens <- 20
vals <- seq(0,ntokens,1) #possible values to contribute - from 0 to 20 tokens

# ------------- load, define, and prepare data - including covariates ---
rawDat <- read.csv("216377/Module5/data/HerrmannThoeniGaechterDATA.csv", skip = 3) # Public goods game

#- create covariates in raw data matrix
# nation index
rawDat$nation <- c()
rawDat$nation[rawDat$city=="Melbourne"]=1
rawDat$nation[rawDat$city=="Minsk"]=2
rawDat$nation[rawDat$city=="Chengdu"]=3
rawDat$nation[rawDat$city=="Copenhagen"]=4
rawDat$nation[rawDat$city=="Bonn"]=5
rawDat$nation[rawDat$city=="Athens"]=6
rawDat$nation[rawDat$city=="Seoul"]=7
rawDat$nation[rawDat$city=="Samara"]=8
rawDat$nation[rawDat$city=="Zurich"]=9
rawDat$nation[rawDat$city=="St. Gallen"]=9
rawDat$nation[rawDat$city=="Istanbul"]=10
rawDat$nation[rawDat$city=="Nottingham"]=11
rawDat$nation[rawDat$city=="Dnipropetrovs'k"]=12
rawDat$nation[rawDat$city=="Boston"]=13

# Variable for GINI. Data from 
# http://hdr.undp.org/sites/default/files/reports/269/hdr_2009_en_complete.pdf,
rawDat$gini <- c()
rawDat$gini[rawDat$city=="Melbourne"]=34.3
rawDat$gini[rawDat$city=="Minsk"]=25.3
rawDat$gini[rawDat$city=="Chengdu"]=38.5
rawDat$gini[rawDat$city=="Copenhagen"]=28.7
rawDat$gini[rawDat$city=="Bonn"]=31.9
rawDat$gini[rawDat$city=="Athens"]=34.4
rawDat$gini[rawDat$city=="Seoul"]=31.6
rawDat$gini[rawDat$city=="Samara"]=37.5
rawDat$gini[rawDat$city=="Zurich"]=32.7
rawDat$gini[rawDat$city=="St. Gallen"]=32.7
rawDat$gini[rawDat$city=="Istanbul"]=41.9
rawDat$gini[rawDat$city=="Nottingham"]=34.8
rawDat$gini[rawDat$city=="Dnipropetrovs'k"]=26.1
rawDat$gini[rawDat$city=="Boston"]=41.1

# Variable for individualism. Data from
# https://www.hofstede-insights.com/product/compare-countries/
# Reference: Hofstede, Hofstede, & Minkov, 2010
# Used instead of data from paper - includes all nations, and closer in time
rawDat$ind <- c()
rawDat$ind[rawDat$city=="Melbourne"]=90
rawDat$ind[rawDat$city=="Minsk"]=25 # not in paper - added to 2010 edition?
rawDat$ind[rawDat$city=="Chengdu"]=20
rawDat$ind[rawDat$city=="Copenhagen"]=74
rawDat$ind[rawDat$city=="Bonn"]=67
rawDat$ind[rawDat$city=="Athens"]=35
rawDat$ind[rawDat$city=="Seoul"]=18
rawDat$ind[rawDat$city=="Samara"]=25 #updated in 2010 edition - paper says 39
rawDat$ind[rawDat$city=="Zurich"]=68
rawDat$ind[rawDat$city=="St. Gallen"]=68
rawDat$ind[rawDat$city=="Istanbul"]=37
rawDat$ind[rawDat$city=="Nottingham"]=89
rawDat$ind[rawDat$city=="Dnipropetrovs'k"]=25 # not in paper - added to 2010 edition?
rawDat$ind[rawDat$city=="Boston"]=91

# Variable for per capita GDP. Included data from paper. In thousands
# 2000 to 2009 data from IMF estimates included in comments)
# https://en.wikipedia.org/wiki/List_of_countries_by_past_and_projected_GDP_(PPP)#IMF_estimates_from_2000_to_2009
rawDat$gdp <- c()
rawDat$gdp[rawDat$city=="Melbourne"]=32.9 #40822
rawDat$gdp[rawDat$city=="Minsk"]=8.9 #14051
rawDat$gdp[rawDat$city=="Chengdu"]=7.6 #8215
rawDat$gdp[rawDat$city=="Copenhagen"]=36.5 #42096
rawDat$gdp[rawDat$city=="Bonn"]=31.1 #37949
rawDat$gdp[rawDat$city=="Athens"]=26.0 #29312
rawDat$gdp[rawDat$city=="Seoul"]=23.9 #28820
rawDat$gdp[rawDat$city=="Samara"]=12.1 #20123
rawDat$gdp[rawDat$city=="Zurich"]=37.4 #54812
rawDat$gdp[rawDat$city=="St. Gallen"]=37.4 #54812
rawDat$gdp[rawDat$city=="Istanbul"]=9.1 #16093
rawDat$gdp[rawDat$city=="Nottingham"]=35.1 #35372
rawDat$gdp[rawDat$city=="Dnipropetrovs'k"]=7.6 #7845
rawDat$gdp[rawDat$city=="Boston"]=43.4 #47008 

# Variable for trust/social cohesion. Data from paper
# Also available from world value and european values surveys (commented out)
# https://ourworldindata.org/trust?utm_source=Our+World+in+Data+%E2%80%93+Newsletter&utm_campaign=ab0d6124e2-Newsletter_OurWorldInData_No4_Trust&utm_medium=email&utm_term=0_2e166c1fc1-ab0d6124e2-284219045
# values from 2009 - also csv saved in data folder. Source is wave five survey 
# Inglehart, R., C. Haerpfer, A. Moreno, C. Welzel, K. Kizilova, J. Diez-Medrano, M. Lagos, P. Norris, E. Ponarin & B. Puranen et al. (eds.). 2014. 
# World Values Survey: Round Five - Country-Pooled Datafile Version:
# Values for Denmark and Greece from Halman, L. 2001. The European Values Study: A Third Wave. Tilburg: EVS, WORC, Tilburg
# University. Quoted here: https://www.oecd.org/innovation/research/2381883.pdf
# Data from the EVS and WVS are stable and comparable for countries that are included on both
# https://www.researchgate.net/publication/287200653_Trust_in_institutions_and_human_values_in_the_European_context_A_comparison_between_the_World_Value_Survey_and_the_European_Social_Survey
rawDat$trust <- c()
rawDat$trust[rawDat$city=="Melbourne"]=.40 #.48 #2009
rawDat$trust[rawDat$city=="Minsk"]= .42 #.41 #2008-2010 EVS - european values survey (only 2014 value of .33 available for WVS)
rawDat$trust[rawDat$city=="Chengdu"]= .55 #.49 #2009
rawDat$trust[rawDat$city=="Copenhagen"]= .67 #.75 #2008-2010 EVS - european values survey
rawDat$trust[rawDat$city=="Bonn"]=.38 #.32 #2009
rawDat$trust[rawDat$city=="Athens"]=.24 #.19 #2008-2010 #EVS - european values survey
rawDat$trust[rawDat$city=="Seoul"]=.27 #.30 #2009
rawDat$trust[rawDat$city=="Samara"]=.24 #.25 #2009
rawDat$trust[rawDat$city=="Zurich"]= .37 #.49 #2009
rawDat$trust[rawDat$city=="St. Gallen"]= .37 #.49 #2009
rawDat$trust[rawDat$city=="Istanbul"]=.16 #.05 #2009
rawDat$trust[rawDat$city=="Nottingham"]= .29 #.30 #2009
rawDat$trust[rawDat$city=="Dnipropetrovs'k"]=.27 #.27 #2009
rawDat$trust[rawDat$city=="Boston"]=.36 #.40 #2009
