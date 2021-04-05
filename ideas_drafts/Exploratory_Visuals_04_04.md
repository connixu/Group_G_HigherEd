---
title: "Exploratory Visuals"
author: Arielle Herman
date: 2021-04-04
always_allow_html: yes
output: 
  html_document:
    keep_md: true
    toc: true
---


```r
rm(list=ls())
knitr::opts_chunk$set(echo = TRUE)

# Load packages.
packages <- c("devtools","knitr","tidyverse","ggthemes", "ggmap", "educationdata",
              "spData")

packages <- lapply(packages, FUN = function(x) {
  if(!require(x, character.only = TRUE)) {
    install.packages(x)
  library(x, character.only = TRUE)
  }
}
)
library(educationdata)
library(spData)
library(urbnmapr)
library(readxl)
library(httr)

# https://github.com/UrbanInstitute/education-data-package-r
```


```r
# educationdata ipeds outcome measures
filepath <- "../data/educationdata/ipedsOutcomes2017.csv"
## if file exists, don't make it again cuz it takes a real long time
if(file.exists(filepath)) {
  ipeds17 <- read_csv(filepath)
} else {
  ipeds17 <- get_education_data(level = 'college-university', source = 'ipeds', 
                                topic = 'outcome-measures', filters = list(year = 2017),
                                add_labels = TRUE)
  write.csv(ipeds17, filepath, row.names = FALSE)
}
ipeds17
```

```
## # A tibble: 92,368 x 38
##     year unitid ftpt      fips    cohort_rev_6yr exclusions_6yr cohort_adj_6yr
##    <dbl>  <dbl> <chr>     <chr>   <lgl>          <lgl>          <lgl>         
##  1  2017 100654 Part-time Alabama NA             NA             NA            
##  2  2017 100654 Full-time Alabama NA             NA             NA            
##  3  2017 100654 Full-time Alabama NA             NA             NA            
##  4  2017 100654 Part-time Alabama NA             NA             NA            
##  5  2017 100654 Full-time Alabama NA             NA             NA            
##  6  2017 100654 Total     Alabama NA             NA             NA            
##  7  2017 100654 Part-time Alabama NA             NA             NA            
##  8  2017 100654 Total     Alabama NA             NA             NA            
##  9  2017 100654 Part-time Alabama NA             NA             NA            
## 10  2017 100654 Full-time Alabama NA             NA             NA            
## # … with 92,358 more rows, and 31 more variables: completers_6yr <dbl>,
## #   completion_rate_6yr <dbl>, exclusions_add_8yr <lgl>, cohort_adj_8yr <lgl>,
## #   completers_8yr <dbl>, still_enroll_8yr <dbl>, transfer_8yr <dbl>,
## #   unknown_8yr <dbl>, no_award_8yr <dbl>, completion_rate_8yr <dbl>,
## #   still_enroll_transfer_rate_8yr <dbl>, still_enroll_rate_8yr <dbl>,
## #   transfer_rate_8yr <dbl>, class_level <chr>, cohort_year <dbl>,
## #   fed_aid_type <chr>, cohort_rev <dbl>, exclusions <dbl>, cohort_adj <dbl>,
## #   award_cert_4yr <dbl>, award_assoc_4yr <dbl>, award_bach_4yr <dbl>,
## #   completers_4yr <dbl>, completion_rate_4yr <dbl>, award_cert_6yr <dbl>,
## #   award_assoc_6yr <dbl>, award_bach_6yr <dbl>, award_cert_8yr <dbl>,
## #   award_assoc_8yr <dbl>, award_bach_8yr <dbl>, unknown_rate_8yr <dbl>
```

```r
# us_states from spData
## updated as of 2015 or something
data("us_states")
data(us_states_df) # has poverty measures and things
head(us_states)
```

```
##   GEOID        NAME   REGION      AREA total_pop_10 total_pop_15
## 1    01     Alabama    South 133709.27      4712651      4830620
## 2    04     Arizona     West 295281.25      6246816      6641928
## 3    08    Colorado     West 269573.06      4887061      5278906
## 4    09 Connecticut Norteast  12976.59      3545837      3593222
## 5    12     Florida    South 151052.01     18511620     19645772
## 6    13     Georgia    South 152725.21      9468815     10006693
##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 geometry
## 1                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          -88.20006, -88.20296, -87.42861, -86.86215, -85.60516, -85.47047, -85.30449, -85.18440, -85.12219, -85.10534, -85.00710, -84.96343, -85.00187, -84.89184, -85.05875, -85.05382, -85.14183, -85.12553, -85.05817, -85.04499, -85.09249, -85.10752, -85.03562, -85.00250, -85.89363, -86.52000, -87.59894, -87.63494, -87.53262, -87.40697, -87.44659, -87.42958, -87.51832, -87.65689, -87.75552, -87.90634, -87.90171, -87.93672, -88.00840, -88.10087, -88.10727, -88.20449, -88.33228, -88.39502, -88.43898, -88.47323, -88.40379, -88.33093, -88.21074, -88.09789, -88.20006, 34.99563, 35.00803, 35.00279, 34.99196, 34.98468, 34.32824, 33.48288, 32.86132, 32.77335, 32.64484, 32.52387, 32.42254, 32.32202, 32.26340, 32.13602, 32.01350, 31.83926, 31.69496, 31.62023, 31.51823, 31.36288, 31.18645, 31.10819, 31.00068, 30.99346, 30.99322, 30.99742, 30.86586, 30.74347, 30.67515, 30.52706, 30.40649, 30.28044, 30.24971, 30.29122, 30.40938, 30.55088, 30.65743, 30.68496, 30.50975, 30.37725, 30.36210, 30.38844, 30.36942, 31.24690, 31.89386, 32.44977, 33.07312, 34.02920, 34.89220, 34.99563
## 2                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             -114.71963, -114.53909, -114.46897, -114.50613, -114.67080, -114.70790, -114.67703, -114.72287, -114.62964, -114.55890, -114.49649, -114.53368, -114.46026, -114.41591, -114.25414, -114.13828, -114.34261, -114.47162, -114.63068, -114.63349, -114.57275, -114.59593, -114.67764, -114.65341, -114.68941, -114.71211, -114.66719, -114.73116, -114.73616, -114.57203, -114.37211, -114.23880, -114.15413, -114.04684, -114.05060, -112.96647, -112.35769, -111.41278, -110.50069, -110.47019, -109.62567, -109.04522, -109.04618, -109.04602, -109.04618, -109.04666, -109.04748, -109.04829, -109.05004, -109.82969, -111.07483, -111.56019, -112.39942, -113.12596, -113.78168, -114.81361, -114.80939, -114.71963, 32.71876, 32.75695, 32.84515, 33.01701, 33.03798, 33.09743, 33.27017, 33.39878, 33.42814, 33.53182, 33.69690, 33.92607, 33.99665, 34.10764, 34.17383, 34.30323, 34.45144, 34.71297, 34.86635, 35.00186, 35.13873, 35.32523, 35.48974, 35.61079, 35.65141, 35.80618, 35.87479, 35.94392, 36.10437, 36.15161, 36.14311, 36.01456, 36.02386, 36.19407, 37.00040, 37.00022, 37.00102, 37.00148, 37.00426, 36.99800, 36.99831, 36.99908, 36.18175, 35.17551, 34.52239, 33.62506, 33.06842, 32.08911, 31.33250, 31.33407, 31.33224, 31.48814, 31.75176, 31.97278, 32.17903, 32.49428, 32.61712, 32.71876
## 3                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 -109.05008, -108.25065, -107.62562, -106.21757, -105.73042, -104.85527, -104.05325, -102.86578, -102.05161, -102.05174, -102.04845, -102.04539, -102.04464, -102.04224, -103.00220, -104.33883, -105.00055, -106.20147, -106.86980, -107.42092, -108.24936, -109.04522, -109.04187, -109.04176, -109.06006, -109.05151, -109.05061, -109.05008, 41.00066, 41.00011, 41.00212, 40.99773, 40.99689, 40.99805, 41.00141, 41.00199, 41.00238, 40.00308, 39.30314, 38.81339, 38.04553, 36.99308, 37.00010, 36.99354, 36.99326, 36.99412, 36.99243, 37.00001, 36.99901, 36.99908, 37.53073, 38.16469, 38.27549, 39.12609, 39.87497, 41.00066
## 4                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                -73.48731, -72.99955, -71.80065, -71.79924, -71.78699, -71.79768, -71.86051, -71.94565, -72.38663, -72.45193, -72.96205, -73.13025, -73.17777, -73.33066, -73.38723, -73.49333, -73.65734, -73.72777, -73.48271, -73.55096, -73.48731, 42.04964, 42.03865, 42.02357, 42.00807, 41.65599, 41.41671, 41.32025, 41.33780, 41.26180, 41.27889, 41.25160, 41.14680, 41.16670, 41.11000, 41.05825, 41.04817, 40.98517, 41.10070, 41.21276, 41.29542, 42.04964
## 5 -81.81169, -81.74565, -81.44351, -81.30505, -81.25771, -81.40189, -81.51740, -81.68524, -81.81169, 24.56874, 24.65988, 24.81336, 24.75519, 24.66431, 24.62354, 24.62124, 24.55868, 24.56874, -85.00250, -84.93696, -84.91445, -84.86469, -84.05732, -83.13143, -82.21487, -82.21032, -82.17008, -82.04794, -82.01213, -82.03966, -81.94981, -81.69479, -81.44412, -81.38550, -81.25671, -81.16358, -80.96618, -80.70973, -80.57487, -80.52509, -80.58781, -80.60421, -80.56643, -80.38370, -80.25366, -80.09391, -80.03136, -80.03886, -80.10957, -80.12799, -80.24453, -80.33942, -80.32578, -80.35818, -80.46883, -80.46455, -80.66403, -80.74775, -80.81213, -80.90058, -81.07986, -81.17204, -81.11727, -81.29033, -81.35056, -81.38381, -81.46849, -81.62348, -81.68954, -81.80166, -81.83314, -81.91171, -82.01368, -82.10567, -82.13787, -82.18157, -82.06658, -82.07635, -82.17524, -82.14707, -82.24989, -82.44572, -82.56925, -82.70782, -82.58463, -82.39338, -82.41392, -82.62959, -82.58652, -82.63362, -82.73802, -82.85113, -82.82816, -82.86081, -82.76264, -82.67479, -82.65414, -82.66872, -82.73024, -82.68886, -82.76055, -82.75970, -82.82707, -82.99614, -83.16958, -83.21807, -83.40025, -83.41277, -83.53764, -83.63798, -84.02427, -84.15728, -84.26936, -84.36611, -84.33375, -84.53587, -84.82420, -84.91511, -84.99326, -85.12147, -85.04507, -85.22161, -85.31921, -85.30133, -85.40505, -85.48776, -85.58824, -85.69681, -85.87814, -86.08996, -86.29870, -86.63295, -86.90968, -87.15539, -87.29542, -87.51832, -87.42958, -87.44659, -87.40697, -87.53262, -87.63494, -87.59894, -86.52000, -85.89363, -85.00250, 31.00068, 30.88468, 30.75358, 30.71154, 30.67470, 30.62357, 30.56858, 30.42458, 30.35891, 30.36325, 30.59377, 30.74773, 30.82750, 30.74842, 30.70971, 30.27384, 29.78469, 29.55529, 29.14796, 28.75669, 28.58517, 28.45945, 28.41086, 28.25773, 28.09563, 27.74004, 27.37979, 27.01859, 26.79634, 26.56935, 26.08716, 25.77225, 25.71709, 25.49943, 25.39801, 25.15323, 25.09203, 25.20907, 25.18726, 25.14744, 25.18604, 25.13967, 25.11880, 25.22228, 25.35495, 25.68751, 25.68983, 25.77675, 25.80332, 25.89716, 25.85271, 26.08823, 26.29452, 26.42716, 26.49083, 26.48393, 26.63744, 26.68171, 26.74266, 26.95826, 26.91687, 26.78980, 26.76295, 27.06063, 27.29859, 27.48762, 27.59602, 27.83752, 27.90140, 27.99847, 27.81670, 27.71061, 27.70681, 27.88630, 28.02013, 28.21708, 28.21901, 28.44196, 28.59084, 28.69566, 28.85016, 28.90561, 28.99309, 29.05419, 29.15843, 29.17807, 29.29036, 29.42049, 29.51724, 29.66849, 29.72306, 29.88607, 30.10327, 30.07271, 30.09766, 30.00866, 29.92372, 29.91009, 29.75829, 29.78330, 29.71496, 29.71585, 29.58699, 29.67776, 29.68149, 29.79712, 29.93849, 29.96123, 30.05554, 30.09689, 30.21562, 30.30357, 30.36305, 30.39630, 30.37242, 30.32775, 30.32350, 30.28044, 30.40649, 30.52706, 30.67515, 30.74347, 30.86586, 30.99742, 30.99322, 30.99346, 31.00068
## 6                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   -85.60516, -84.32187, -83.61998, -83.10861, -83.12111, -83.23908, -83.32387, -83.33869, -83.16828, -83.00292, -82.90266, -82.74666, -82.71751, -82.59615, -82.55684, -82.34693, -82.25527, -82.19658, -82.04633, -81.91353, -81.94474, -81.84730, -81.82794, -81.70463, -81.61778, -81.49142, -81.50272, -81.42161, -81.41191, -81.28132, -81.11963, -81.15600, -81.11723, -81.00674, -80.88552, -80.84055, -81.13063, -81.13349, -81.17725, -81.27880, -81.27469, -81.42047, -81.40515, -81.44693, -81.44412, -81.69479, -81.94981, -82.03966, -82.01213, -82.04794, -82.17008, -82.21032, -82.21487, -83.13143, -84.05732, -84.86469, -84.91445, -84.93696, -85.00250, -85.03562, -85.10752, -85.09249, -85.04499, -85.05817, -85.12553, -85.14183, -85.05382, -85.05875, -84.89184, -85.00187, -84.96343, -85.00710, -85.10534, -85.12219, -85.18440, -85.30449, -85.47047, -85.60516, 34.98468, 34.98841, 34.98659, 35.00066, 34.93913, 34.87566, 34.78971, 34.68200, 34.59100, 34.47213, 34.48590, 34.26641, 34.15050, 34.03052, 33.94535, 33.83430, 33.75969, 33.63058, 33.56383, 33.44127, 33.36404, 33.30678, 33.22875, 33.11645, 33.09528, 33.00808, 32.93869, 32.83518, 32.61841, 32.55646, 32.28760, 32.24148, 32.11760, 32.10115, 32.03460, 32.01131, 31.72269, 31.62335, 31.51707, 31.36721, 31.28945, 31.01670, 30.90820, 30.81039, 30.70971, 30.74842, 30.82750, 30.74773, 30.59377, 30.36325, 30.35891, 30.42458, 30.56858, 30.62357, 30.67470, 30.71154, 30.75358, 30.88468, 31.00068, 31.10819, 31.18645, 31.36288, 31.51823, 31.62023, 31.69496, 31.83926, 32.01350, 32.13602, 32.26340, 32.32202, 32.42254, 32.52387, 32.64484, 32.77335, 32.86132, 33.48288, 34.32824, 34.98468
```

```r
head(us_states_df)
```

```
## # A tibble: 6 x 5
##   state      median_income_10 median_income_15 poverty_level_10 poverty_level_15
##   <chr>                 <dbl>            <dbl>            <dbl>            <dbl>
## 1 Alabama               21746            22890           786544           887260
## 2 Alaska                29509            31455            64245            72957
## 3 Arizona               26412            26156           933113          1180690
## 4 Arkansas              20881            22205           502684           553644
## 5 California            27207            27035          4919945          6135142
## 6 Colorado              29365            30752           584184           653969
```

```r
# us_counties from urbnmapr
us_counties <- get_urbn_map("counties", sf = TRUE)

# scorecard data
sc <- read_csv('../data/2019_College_Scorecard_Valid_Admissions_Data.csv')
sc
```

```
## # A tibble: 1,933 x 2,392
##       X1 UNITID  OPEID OPEID6 INSTNM   CITY   STABBR ZIP   ACCREDAGENCY  INSTURL
##    <dbl>  <dbl>  <dbl>  <dbl> <chr>    <chr>  <chr>  <chr> <chr>         <chr>  
##  1     0 100654 100200   1002 Alabama… Normal AL     35762 Southern Ass… www.aa…
##  2     1 100663 105200   1052 Univers… Birmi… AL     3529… Southern Ass… https:…
##  3     3 100706 105500   1055 Univers… Hunts… AL     35899 Southern Ass… www.ua…
##  4     4 100724 100500   1005 Alabama… Montg… AL     3610… Southern Ass… www.al…
##  5     5 100751 105100   1051 The Uni… Tusca… AL     3548… Southern Ass… www.ua…
##  6     7 100830 831000   8310 Auburn … Montg… AL     3611… Southern Ass… www.au…
##  7     8 100858 100900   1009 Auburn … Auburn AL     36849 Southern Ass… www.au…
##  8     9 100937 101200   1012 Birming… Birmi… AL     35254 Southern Ass… www.bs…
##  9    10 101189 100300   1003 Faulkne… Montg… AL     3610… Southern Ass… www.fa…
## 10    13 101365 962107   9621 Herzing… Birmi… AL     35209 Higher Learn… https:…
## # … with 1,923 more rows, and 2,382 more variables: NPCURL <chr>,
## #   SCH_DEG <lgl>, HCM2 <dbl>, MAIN <dbl>, NUMBRANCH <dbl>, PREDDEG <dbl>,
## #   HIGHDEG <dbl>, CONTROL <dbl>, ST_FIPS <dbl>, REGION <dbl>, LOCALE <dbl>,
## #   LOCALE2 <lgl>, LATITUDE <dbl>, LONGITUDE <dbl>, CCBASIC <dbl>,
## #   CCUGPROF <dbl>, CCSIZSET <dbl>, HBCU <dbl>, PBI <dbl>, ANNHI <dbl>,
## #   TRIBAL <dbl>, AANAPII <dbl>, HSI <dbl>, NANTI <dbl>, MENONLY <dbl>,
## #   WOMENONLY <dbl>, RELAFFIL <dbl>, ADM_RATE <dbl>, ADM_RATE_ALL <dbl>,
## #   SATVR25 <dbl>, SATVR75 <dbl>, SATMT25 <dbl>, SATMT75 <dbl>, SATWR25 <lgl>,
## #   SATWR75 <lgl>, SATVRMID <dbl>, SATMTMID <dbl>, SATWRMID <lgl>,
## #   ACTCM25 <dbl>, ACTCM75 <dbl>, ACTEN25 <dbl>, ACTEN75 <dbl>, ACTMT25 <dbl>,
## #   ACTMT75 <dbl>, ACTWR25 <lgl>, ACTWR75 <lgl>, ACTCMMID <dbl>,
## #   ACTENMID <dbl>, ACTMTMID <dbl>, ACTWRMID <lgl>, SAT_AVG <dbl>,
## #   SAT_AVG_ALL <dbl>, PCIP01 <dbl>, PCIP03 <dbl>, PCIP04 <dbl>, PCIP05 <dbl>,
## #   PCIP09 <dbl>, PCIP10 <dbl>, PCIP11 <dbl>, PCIP12 <dbl>, PCIP13 <dbl>,
## #   PCIP14 <dbl>, PCIP15 <dbl>, PCIP16 <dbl>, PCIP19 <dbl>, PCIP22 <dbl>,
## #   PCIP23 <dbl>, PCIP24 <dbl>, PCIP25 <dbl>, PCIP26 <dbl>, PCIP27 <dbl>,
## #   PCIP29 <dbl>, PCIP30 <dbl>, PCIP31 <dbl>, PCIP38 <dbl>, PCIP39 <dbl>,
## #   PCIP40 <dbl>, PCIP41 <dbl>, PCIP42 <dbl>, PCIP43 <dbl>, PCIP44 <dbl>,
## #   PCIP45 <dbl>, PCIP46 <dbl>, PCIP47 <dbl>, PCIP48 <dbl>, PCIP49 <dbl>,
## #   PCIP50 <dbl>, PCIP51 <dbl>, PCIP52 <dbl>, PCIP54 <dbl>, CIP01CERT1 <dbl>,
## #   CIP01CERT2 <dbl>, CIP01ASSOC <dbl>, CIP01CERT4 <dbl>, CIP01BACHL <dbl>,
## #   CIP03CERT1 <dbl>, CIP03CERT2 <dbl>, CIP03ASSOC <dbl>, CIP03CERT4 <dbl>,
## #   CIP03BACHL <dbl>, …
```

```r
# county level poverty data
#usda_pov_19 <- read_xls("../data/usda/PovertyEstimates.xls", skip = 4)
#usda_pov_19

#usda
## https://www.ers.usda.gov/data-products/county-level-data-sets/download-data/
### poverty by county level
url <- "https://www.ers.usda.gov/webdocs/DataFiles/48747/PovertyEstimates.xls?v=9293.8"
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
```

```
## Response [https://www.ers.usda.gov/webdocs/DataFiles/48747/PovertyEstimates.xls?v=9293.8]
##   Date: 2021-04-05 00:08
##   Status: 200
##   Content-Type: application/vnd.ms-excel
##   Size: 1 MB
## <ON DISK>  /tmp/RtmpbesqPH/file41f22d504c0f.xls
```

```r
usda_pv <- read_xls(tf, sheet = 1, skip = 4)
usda_pv
```

```
## # A tibble: 3,193 x 34
##    FIPStxt Stabr Area_name  `Rural-urban_Con… Urban_Influence… `Rural-urban_Con…
##    <chr>   <chr> <chr>                  <dbl>            <dbl>             <dbl>
##  1 00000   US    United St…                NA               NA                NA
##  2 01000   AL    Alabama                   NA               NA                NA
##  3 01001   AL    Autauga C…                 2                2                 2
##  4 01003   AL    Baldwin C…                 4                5                 3
##  5 01005   AL    Barbour C…                 6                6                 6
##  6 01007   AL    Bibb Coun…                 1                1                 1
##  7 01009   AL    Blount Co…                 1                1                 1
##  8 01011   AL    Bullock C…                 6                6                 6
##  9 01013   AL    Butler Co…                 6                6                 6
## 10 01015   AL    Calhoun C…                 3                2                 3
## # … with 3,183 more rows, and 28 more variables:
## #   Urban_Influence_Code_2013 <dbl>, POVALL_2019 <dbl>, CI90LBALL_2019 <dbl>,
## #   CI90UBALL_2019 <dbl>, PCTPOVALL_2019 <dbl>, CI90LBALLP_2019 <dbl>,
## #   CI90UBALLP_2019 <dbl>, POV017_2019 <dbl>, CI90LB017_2019 <dbl>,
## #   CI90UB017_2019 <dbl>, PCTPOV017_2019 <dbl>, CI90LB017P_2019 <dbl>,
## #   CI90UB017P_2019 <dbl>, POV517_2019 <dbl>, CI90LB517_2019 <dbl>,
## #   CI90UB517_2019 <dbl>, PCTPOV517_2019 <dbl>, CI90LB517P_2019 <dbl>,
## #   CI90UB517P_2019 <dbl>, MEDHHINC_2019 <dbl>, CI90LBINC_2019 <dbl>,
## #   CI90UBINC_2019 <dbl>, POV04_2019 <dbl>, CI90LB04_2019 <dbl>,
## #   CI90UB04_2019 <dbl>, PCTPOV04_2019 <dbl>, CI90LB04P_2019 <dbl>,
## #   CI90UB04P_2019 <dbl>
```

# state maps


```r
## states by population
### question: should this be scaled by area?
us_states %>% ggplot() + geom_sf(aes(fill = total_pop_15)) +
  scale_fill_continuous(type = "viridis", direction = -1)
```

![](Exploratory_Visuals_04_04_files/figure-html/states-1.png)<!-- -->

```r
## states by median income in 2015
us_states %>% left_join(us_states_df, by = c("NAME" = "state")) %>%
  ggplot() + geom_sf(aes(fill = median_income_15)) +
  scale_fill_continuous(type = "viridis", direction = -1)
```

![](Exploratory_Visuals_04_04_files/figure-html/states-2.png)<!-- -->

```r
## states by median poverty level in 2015
### does this poverty level look quite right?
us_states %>% left_join(us_states_df, by = c("NAME" = "state")) %>%
  ggplot() + geom_sf(aes(fill = poverty_level_15))
```

![](Exploratory_Visuals_04_04_files/figure-html/states-3.png)<!-- -->

# county maps

```r
# prep data for plotting
## check shared names
mean(unique(usda_pv$Area_name) %in% unique(us_counties$county_name))
```

```
## [1] 0.9683446
```

```r
## change projection of sc data
sc_sf <- sc %>%
  dplyr::mutate(uni_rank = case_when(
    ADM_RATE < 0.05 ~ 'elite',
    ADM_RATE < 0.2 ~ 'highly selective',
    ADM_RATE < 0.3 ~ 'selective',
    ADM_RATE < 0.7 ~ 'less selective',
    TRUE ~ 'not selective')) %>% filter(uni_rank %in% c('elite', 'highly selective')) %>%
  select(uni_rank, LONGITUDE, LATITUDE) %>% na.omit() %>% sf::st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326)

# base plot
p <- us_counties %>% left_join(usda_pv, by = c("county_name" = "Area_name")) %>%
  ggplot() + scale_fill_continuous(type = "viridis", direction = -1) + ggthemes::theme_tufte()
## rural-urban continuum_code
p + geom_sf(aes(fill = `Rural-urban_Continuum_Code_2013`), lwd = 0) +
  geom_sf(data = us_states, color = "white", fill = NA, lwd = 0.4) +
  ggtitle("Rural-urban Continuum Code, 2013")
```

![](Exploratory_Visuals_04_04_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
## poverty estimate of people of all ages living in poverty: PCTPOVALL_2019
### question: how change the legend to %?
### note: change colors so that they are easily visible
### note: look at other patterns?
p + geom_sf(aes(fill = PCTPOVALL_2019), lwd = 0) +
  geom_sf(data = us_states, color = "white", fill = NA, lwd = 0.4) +
  ggtitle("Estimated percent of people of all ages in poverty (2019)") +
  labs(fill = "Percent") + geom_sf(data = sc_sf, aes(color = uni_rank), size = 1.5)
```

![](Exploratory_Visuals_04_04_files/figure-html/unnamed-chunk-1-2.png)<!-- -->
