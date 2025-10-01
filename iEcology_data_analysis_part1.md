Data processing and statistical analysis of iEcology-IAS-miner data in
R - Part 1
================
Simon Reynaert
25/08/2025

- [1. Loading the required packages](#1-loading-the-required-packages)
- [2. Load in all data sources and
  preprocess](#2-load-in-all-data-sources-and-preprocess)
  - [2.1. Load in the EASIN lists, filter out species x country
    invasions \>=2016 and save as new
    csv](#21-load-in-the-easin-lists-filter-out-species-x-country-invasions-2016-and-save-as-new-csv)
  - [2.2. Load and pre-process the wikipedia language to country mapping
    pageview
    data](#22-load-and-pre-process-the-wikipedia-language-to-country-mapping-pageview-data)
  - [2.3. Load, join and pre-process the wikipedia geolocated pageview
    datasets](#23-load-join-and-pre-process-the-wikipedia-geolocated-pageview-datasets)
  - [2.4. Load and pre-process geolocated Google health activity
    data](#24-load-and-pre-process-geolocated-google-health-activity-data)
  - [2.5. Load and pre-process geolocated Flickr
    data](#25-load-and-pre-process-geolocated-flickr-data)
  - [2.6. Load and pre-process geolocated iNaturalist casual
    observations](#26-load-and-pre-process-geolocated-inaturalist-casual-observations)
  - [2.7. Load and pre-process youtube geolocated
    videos](#27-load-and-pre-process-youtube-geolocated-videos)
  - [2.8. Load and pre-process the Facebook
    data](#28-load-and-pre-process-the-facebook-data)
  - [2.9. Load and pre-process the GBIF
    data](#29-load-and-pre-process-the-gbif-data)
- [3. Combine all datasets into long format df including daily activity
  patterns](#3-combine-all-datasets-into-long-format-df-including-daily-activity-patterns)
- [4. Bin ‘views’ (= activity) per species x country x platform per
  month to capture signals of increased frequency and save as new
  file](#4-bin-views--activity-per-species-x-country-x-platform-per-month-to-capture-signals-of-increased-frequency-and-save-as-new-file)
- [5. Join monthly long format data with the intro_year EASIN data and
  save as combined
  file](#5-join-monthly-long-format-data-with-the-intro_year-easin-data-and-save-as-combined-file)

This R markdown document goes over the processing and statistical
analysis that was performed in R of the mined iEcology data for the
*iEcology-IAS-miner* Github repository. This .rmd file covers part 1 of
the analysis which includes data processing.

# 1. Loading the required packages

``` r
# install.packages(c("tidyverse","lubridate","tidyr","timetk","anomalize","tibbletime","ggplot2","stringr","viridis","countrycode","tbl2xts","reshape2","Hmisc","broom","lme4","nlme","gamlss","data.table","changepoint.np"))  # if needed
library(tidyverse)
library(lubridate)
library(dplyr)
library(timetk)
library(anomalize)
library(tidyr)
library(tibbletime)
library(ggplot2)
library(stringr)
library(viridis)
library(countrycode)
library(tbl2xts)
library(reshape2)
library(Hmisc)
library(broom)
library(lme4)
library(nlme)
library(gamlss)
library(readr)
library(cowplot)
library(changepoint.np)
library(data.table)
library(strucchange)
```

``` r
#check working directory
getwd()
```

    ## [1] "C:/Users/simon/Documents"

# 2. Load in all data sources and preprocess

``` r
#load in union full list + traits
union_traits<-data.table::fread("unionlist_traits_combined.csv")

union_traits=union_traits%>%
  rename(SCIENTIFIC_NAME = 'Scientific Name')%>%
  mutate(
    SCIENTIFIC_NAME = case_when(
      # Safe synonym collapses:
      SCIENTIFIC_NAME == "Cortaderia selloana subsp. jubata" ~ "Cortaderia jubata",
      SCIENTIFIC_NAME == "Eichhornia crassipes" ~ "Pontederia crassipes",
      SCIENTIFIC_NAME == "Pennisetum setaceum" ~ "Cenchrus setaceus",
      SCIENTIFIC_NAME == "Orconectes limosus" ~ "Faxonius limosus",
      SCIENTIFIC_NAME == "Orconectes virilis" ~ "Faxonius virilis",
      SCIENTIFIC_NAME == "Vespa velutina nigrithorax" ~ "Vespa velutina",
      SCIENTIFIC_NAME == "Prosopis juliflora" ~ "Neltuma juliflora",       
      SCIENTIFIC_NAME == "Urva auropunctata" ~ "Herpestes javanicus",
      SCIENTIFIC_NAME == "Trachemys scripta elegans" ~ "Trachemys scripta",
      SCIENTIFIC_NAME == "Pueraria montana (Lour.) Merr. var. lobata" ~ "Pueraria montana",
      TRUE ~ SCIENTIFIC_NAME # Keep other names as they are
    )
  )

unique(union_traits$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Acridotheres tristis"       
    ##  [3] "Ailanthus altissima"         "Alopochen aegyptiaca"       
    ##  [5] "Alternanthera philoxeroides" "Ameiurus melas"             
    ##  [7] "Andropogon virginicus"       "Arthurdendyus triangulatus" 
    ##  [9] "Asclepias syriaca"           "Axis axis"                  
    ## [11] "Baccharis halimifolia"       "Cabomba caroliniana"        
    ## [13] "Callosciurus erythraeus"     "Callosciurus finlaysonii"   
    ## [15] "Cardiospermum grandiflorum"  "Celastrus orbiculatus"      
    ## [17] "Channa argus"                "Cortaderia jubata"          
    ## [19] "Corvus splendens"            "Ehrharta calycina"          
    ## [21] "Pontederia crassipes"        "Elodea nuttallii"           
    ## [23] "Eriocheir sinensis"          "Faxonius rusticus"          
    ## [25] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [27] "Gambusia holbrooki"          "Gunnera tinctoria"          
    ## [29] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [31] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [33] "Heracleum sosnowskyi"        "Herpestes javanicus"        
    ## [35] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [37] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [39] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [41] "Lepomis gibbosus"            "Lespedeza cuneata"          
    ## [43] "Limnoperna fortunei"         "Lithobates catesbeianus"    
    ## [45] "Ludwigia grandiflora"        "Ludwigia peploides"         
    ## [47] "Lygodium japonicum"          "Lysichiton americanus"      
    ## [49] "Microstegium vimineum"       "Morone americana"           
    ## [51] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [53] "Myriophyllum aquaticum"      "Myriophyllum heterophyllum" 
    ## [55] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [57] "Ondatra zibethicus"          "Faxonius limosus"           
    ## [59] "Faxonius virilis"            "Oxyura jamaicensis"         
    ## [61] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [63] "Cenchrus setaceus"           "Perccottus glenii"          
    ## [65] "Persicaria perfoliata"       "Pistia stratiotes"          
    ## [67] "Plotosus lineatus"           "Procambarus clarkii"        
    ## [69] "Procambarus virginalis"      "Procyon lotor"              
    ## [71] "Neltuma juliflora"           "Pseudorasbora parva"        
    ## [73] "Pueraria montana"            "Pycnonotus cafer"           
    ## [75] "Rugulopteryx okamurae"       "Salvinia molesta"           
    ## [77] "Sciurus carolinensis"        "Sciurus niger"              
    ## [79] "Solenopsis geminata"         "Solenopsis invicta"         
    ## [81] "Solenopsis richteri"         "Tamias sibiricus"           
    ## [83] "Threskiornis aethiopicus"    "Trachemys scripta"          
    ## [85] "Triadica sebifera"           "Vespa velutina"             
    ## [87] "Wasmannia auropunctata"      "Xenopus laevis"

## 2.1. Load in the EASIN lists, filter out species x country invasions \>=2016 and save as new csv

``` r
#––  Load data
uni <- data.table::fread("UnionList_Species_Traits_85_present.csv")
uni=uni%>%
mutate(COUNTRY = ifelse(COUNTRY == "UK", "GB",ifelse(COUNTRY == "EL", "GR", COUNTRY)))
  
glimpse((uni))
```

    ## Rows: 937
    ## Columns: 7
    ## $ EASIN.ID        <chr> "R00053", "R00053", "R00053", "R00053", "R00053", "R00…
    ## $ SCIENTIFIC_NAME <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna", …
    ## $ COUNTRY         <chr> "CY", "CZ", "DK", "GR", "ES", "FR", "HR", "IT", "MT", …
    ## $ YEAR            <int> 1881, 2012, 2023, 1842, 1800, 1838, 1926, 1842, 1864, …
    ## $ Group           <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae",…
    ## $ Habitat         <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terrestr…
    ## $ `WIKI NAME`     <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna", …

``` r
unique(uni$COUNTRY)
```

    ##  [1] "CY" "CZ" "DK" "GR" "ES" "FR" "HR" "IT" "MT" "PT" "SE" "GB" "AT" "DE" "NL"
    ## [16] "BE" "BG" "EE" "HU" "IE" "LU" "LV" "PL" "RO" "SI" "SK" "LT" "FI"

``` r
length(unique(uni$SCIENTIFIC_NAME))
```

    ## [1] 85

``` r
unique(uni$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"                            
    ##  [2] "Acridotheres tristis"                      
    ##  [3] "Ailanthus altissima"                       
    ##  [4] "Alopochen aegyptiaca"                      
    ##  [5] "Alternanthera philoxeroides"               
    ##  [6] "Ameiurus melas"                            
    ##  [7] "Andropogon virginicus"                     
    ##  [8] "Arthurdendyus triangulatus"                
    ##  [9] "Asclepias syriaca"                         
    ## [10] "Axis axis"                                 
    ## [11] "Baccharis halimifolia"                     
    ## [12] "Cabomba caroliniana"                       
    ## [13] "Callosciurus erythraeus"                   
    ## [14] "Callosciurus finlaysonii"                  
    ## [15] "Cardiospermum grandiflorum"                
    ## [16] "Celastrus orbiculatus"                     
    ## [17] "Cenchrus setaceus"                         
    ## [18] "Channa argus"                              
    ## [19] "Cortaderia selloana subsp. jubata"         
    ## [20] "Corvus splendens"                          
    ## [21] "Ehrharta calycina"                         
    ## [22] "Elodea nuttallii"                          
    ## [23] "Eriocheir sinensis"                        
    ## [24] "Faxonius limosus"                          
    ## [25] "Faxonius rusticus"                         
    ## [26] "Faxonius virilis"                          
    ## [27] "Fundulus heteroclitus"                     
    ## [28] "Gambusia affinis"                          
    ## [29] "Gambusia holbrooki"                        
    ## [30] "Gunnera tinctoria"                         
    ## [31] "Gymnocoronis spilanthoides"                
    ## [32] "Hakea sericea"                             
    ## [33] "Heracleum mantegazzianum"                  
    ## [34] "Heracleum persicum"                        
    ## [35] "Heracleum sosnowskyi"                      
    ## [36] "Humulus scandens"                          
    ## [37] "Hydrocotyle ranunculoides"                 
    ## [38] "Impatiens glandulifera"                    
    ## [39] "Koenigia polystachya"                      
    ## [40] "Lagarosiphon major"                        
    ## [41] "Lampropeltis getula"                       
    ## [42] "Lepomis gibbosus"                          
    ## [43] "Lespedeza cuneata"                         
    ## [44] "Lithobates catesbeianus"                   
    ## [45] "Ludwigia grandiflora"                      
    ## [46] "Ludwigia peploides"                        
    ## [47] "Lygodium japonicum"                        
    ## [48] "Lysichiton americanus"                     
    ## [49] "Microstegium vimineum"                     
    ## [50] "Muntiacus reevesi"                         
    ## [51] "Myocastor coypus"                          
    ## [52] "Myriophyllum aquaticum"                    
    ## [53] "Myriophyllum heterophyllum"                
    ## [54] "Nasua nasua"                               
    ## [55] "Neltuma juliflora"                         
    ## [56] "Nyctereutes procyonoides"                  
    ## [57] "Ondatra zibethicus"                        
    ## [58] "Oxyura jamaicensis"                        
    ## [59] "Pacifastacus leniusculus"                  
    ## [60] "Parthenium hysterophorus"                  
    ## [61] "Perccottus glenii"                         
    ## [62] "Persicaria perfoliata"                     
    ## [63] "Pistia stratiotes"                         
    ## [64] "Plotosus lineatus"                         
    ## [65] "Pontederia crassipes"                      
    ## [66] "Procambarus clarkii"                       
    ## [67] "Procambarus virginalis"                    
    ## [68] "Procyon lotor"                             
    ## [69] "Pseudorasbora parva"                       
    ## [70] "Pueraria montana (Lour.) Merr. var. lobata"
    ## [71] "Pycnonotus cafer"                          
    ## [72] "Rugulopteryx okamurae"                     
    ## [73] "Salvinia molesta"                          
    ## [74] "Sciurus carolinensis"                      
    ## [75] "Sciurus niger"                             
    ## [76] "Solenopsis geminata"                       
    ## [77] "Solenopsis invicta"                        
    ## [78] "Tamias sibiricus"                          
    ## [79] "Threskiornis aethiopicus"                  
    ## [80] "Trachemys scripta"                         
    ## [81] "Triadica sebifera"                         
    ## [82] "Urva auropunctata"                         
    ## [83] "Vespa velutina nigrithorax"                
    ## [84] "Wasmannia auropunctata"                    
    ## [85] "Xenopus laevis"

``` r
# We will apply this mapping to the standardized_df to create a new, cleaned dataframe.
uni <- uni %>%
  mutate(
    SCIENTIFIC_NAME = case_when(
      # Safe synonym collapses:
      SCIENTIFIC_NAME == "Cortaderia selloana subsp. jubata" ~ "Cortaderia jubata",
      SCIENTIFIC_NAME == "Eichhornia crassipes" ~ "Pontederia crassipes",
      SCIENTIFIC_NAME == "Pennisetum setaceum" ~ "Cenchrus setaceus",
      SCIENTIFIC_NAME == "Orconectes limosus" ~ "Faxonius limosus",
      SCIENTIFIC_NAME == "Orconectes virilis" ~ "Faxonius virilis",
      SCIENTIFIC_NAME == "Vespa velutina nigrithorax" ~ "Vespa velutina",
      SCIENTIFIC_NAME == "Prosopis juliflora" ~ "Neltuma juliflora",      
      SCIENTIFIC_NAME == "Urva auropunctata" ~ "Herpestes javanicus",
      SCIENTIFIC_NAME == "Trachemys scripta elegans" ~ "Trachemys scripta",
      SCIENTIFIC_NAME == "Pueraria montana (Lour.) Merr. var. lobata" ~ "Pueraria montana",
      TRUE ~ SCIENTIFIC_NAME # Keep other names as they are
    )
  )

length(unique(uni$SCIENTIFIC_NAME))
```

    ## [1] 85

``` r
unique(uni$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Acridotheres tristis"       
    ##  [3] "Ailanthus altissima"         "Alopochen aegyptiaca"       
    ##  [5] "Alternanthera philoxeroides" "Ameiurus melas"             
    ##  [7] "Andropogon virginicus"       "Arthurdendyus triangulatus" 
    ##  [9] "Asclepias syriaca"           "Axis axis"                  
    ## [11] "Baccharis halimifolia"       "Cabomba caroliniana"        
    ## [13] "Callosciurus erythraeus"     "Callosciurus finlaysonii"   
    ## [15] "Cardiospermum grandiflorum"  "Celastrus orbiculatus"      
    ## [17] "Cenchrus setaceus"           "Channa argus"               
    ## [19] "Cortaderia jubata"           "Corvus splendens"           
    ## [21] "Ehrharta calycina"           "Elodea nuttallii"           
    ## [23] "Eriocheir sinensis"          "Faxonius limosus"           
    ## [25] "Faxonius rusticus"           "Faxonius virilis"           
    ## [27] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [29] "Gambusia holbrooki"          "Gunnera tinctoria"          
    ## [31] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [33] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [35] "Heracleum sosnowskyi"        "Humulus scandens"           
    ## [37] "Hydrocotyle ranunculoides"   "Impatiens glandulifera"     
    ## [39] "Koenigia polystachya"        "Lagarosiphon major"         
    ## [41] "Lampropeltis getula"         "Lepomis gibbosus"           
    ## [43] "Lespedeza cuneata"           "Lithobates catesbeianus"    
    ## [45] "Ludwigia grandiflora"        "Ludwigia peploides"         
    ## [47] "Lygodium japonicum"          "Lysichiton americanus"      
    ## [49] "Microstegium vimineum"       "Muntiacus reevesi"          
    ## [51] "Myocastor coypus"            "Myriophyllum aquaticum"     
    ## [53] "Myriophyllum heterophyllum"  "Nasua nasua"                
    ## [55] "Neltuma juliflora"           "Nyctereutes procyonoides"   
    ## [57] "Ondatra zibethicus"          "Oxyura jamaicensis"         
    ## [59] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [61] "Perccottus glenii"           "Persicaria perfoliata"      
    ## [63] "Pistia stratiotes"           "Plotosus lineatus"          
    ## [65] "Pontederia crassipes"        "Procambarus clarkii"        
    ## [67] "Procambarus virginalis"      "Procyon lotor"              
    ## [69] "Pseudorasbora parva"         "Pueraria montana"           
    ## [71] "Pycnonotus cafer"            "Rugulopteryx okamurae"      
    ## [73] "Salvinia molesta"            "Sciurus carolinensis"       
    ## [75] "Sciurus niger"               "Solenopsis geminata"        
    ## [77] "Solenopsis invicta"          "Tamias sibiricus"           
    ## [79] "Threskiornis aethiopicus"    "Trachemys scripta"          
    ## [81] "Triadica sebifera"           "Herpestes javanicus"        
    ## [83] "Vespa velutina"              "Wasmannia auropunctata"     
    ## [85] "Xenopus laevis"

``` r
#––  uni#––  Filter introductions to YEAR >= 2016
intro_year <- uni %>%
  filter(YEAR >= 2016) %>%
  select(SCIENTIFIC_NAME, COUNTRY, YEAR, 'WIKI NAME', Group, Habitat,EASIN.ID) %>%
  distinct()
if (nrow(intro_year) == 0) stop("No introductions found after 2016.")

glimpse((intro_year))
```

    ## Rows: 112
    ## Columns: 7
    ## $ SCIENTIFIC_NAME <chr> "Acacia saligna", "Acridotheres tristis", "Acridothere…
    ## $ COUNTRY         <chr> "DK", "CY", "GR", "LT", "AT", "BG", "EE", "ES", "FI", …
    ## $ YEAR            <int> 2023, 2022, 2017, 2016, 2017, 2016, 2019, 2018, 2018, …
    ## $ `WIKI NAME`     <chr> "Acacia saligna", "Acridotheres tristis", "Acridothere…
    ## $ Group           <chr> "Plantae", "Aves", "Aves", "Aves", "Pisces", "Pisces",…
    ## $ Habitat         <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Both", "…
    ## $ EASIN.ID        <chr> "R00053", "R00212", "R00212", "R00644", "R00826", "R00…

``` r
unique(intro_year$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"             "Acridotheres tristis"      
    ##  [3] "Alopochen aegyptiaca"       "Ameiurus melas"            
    ##  [5] "Asclepias syriaca"          "Cabomba caroliniana"       
    ##  [7] "Celastrus orbiculatus"      "Cenchrus setaceus"         
    ##  [9] "Cortaderia jubata"          "Elodea nuttallii"          
    ## [11] "Eriocheir sinensis"         "Faxonius limosus"          
    ## [13] "Faxonius rusticus"          "Gunnera tinctoria"         
    ## [15] "Gymnocoronis spilanthoides" "Hakea sericea"             
    ## [17] "Heracleum mantegazzianum"   "Heracleum sosnowskyi"      
    ## [19] "Humulus scandens"           "Hydrocotyle ranunculoides" 
    ## [21] "Impatiens glandulifera"     "Koenigia polystachya"      
    ## [23] "Lampropeltis getula"        "Lespedeza cuneata"         
    ## [25] "Ludwigia grandiflora"       "Ludwigia peploides"        
    ## [27] "Lygodium japonicum"         "Lysichiton americanus"     
    ## [29] "Microstegium vimineum"      "Muntiacus reevesi"         
    ## [31] "Myocastor coypus"           "Myriophyllum heterophyllum"
    ## [33] "Nasua nasua"                "Neltuma juliflora"         
    ## [35] "Nyctereutes procyonoides"   "Pacifastacus leniusculus"  
    ## [37] "Parthenium hysterophorus"   "Perccottus glenii"         
    ## [39] "Persicaria perfoliata"      "Pistia stratiotes"         
    ## [41] "Plotosus lineatus"          "Pontederia crassipes"      
    ## [43] "Procambarus clarkii"        "Procambarus virginalis"    
    ## [45] "Rugulopteryx okamurae"      "Salvinia molesta"          
    ## [47] "Sciurus carolinensis"       "Solenopsis invicta"        
    ## [49] "Threskiornis aethiopicus"   "Trachemys scripta"         
    ## [51] "Triadica sebifera"          "Vespa velutina"            
    ## [53] "Wasmannia auropunctata"     "Xenopus laevis"

``` r
length(unique(intro_year$SCIENTIFIC_NAME))
```

    ## [1] 54

``` r
write.csv(intro_year, "intros_after_2016_EASIN.csv", row.names = FALSE)
```

## 2.2. Load and pre-process the wikipedia language to country mapping pageview data

``` r
#load wiki language pageview data

pv  <- data.table::fread("species_pageviews_analysis_2016_present.csv",header=TRUE)

head(pv[,1:5])
```

    ##    Scientific Name Language    Wikipedia Title 20160101 20160102
    ##             <char>   <char>             <char>    <int>    <int>
    ## 1:  Acacia saligna       de Weidenblatt-Akazie        5       10
    ## 2:  Acacia saligna       en     Acacia_saligna       15       42
    ## 3:  Acacia saligna       es     Acacia_saligna        7        9
    ## 4:  Acacia saligna       fi        Siniakaasia       NA       NA
    ## 5:  Acacia saligna       fr     Acacia_saligna        3        5
    ## 6:  Acacia saligna       pt     Acacia_saligna       NA        0

``` r
unique(pv$`Scientific Name`)
```

    ##  [1] "Acacia saligna"              "Acridotheres tristis"       
    ##  [3] "Ailanthus altissima"         "Alopochen aegyptiaca"       
    ##  [5] "Alternanthera philoxeroides" "Ameiurus melas"             
    ##  [7] "Andropogon virginicus"       "Arthurdendyus triangulatus" 
    ##  [9] "Asclepias syriaca"           "Axis axis"                  
    ## [11] "Baccharis halimifolia"       "Cabomba caroliniana"        
    ## [13] "Callosciurus erythraeus"     "Callosciurus finlaysonii"   
    ## [15] "Cardiospermum grandiflorum"  "Celastrus orbiculatus"      
    ## [17] "Channa argus"                "Cortaderia jubata"          
    ## [19] "Corvus splendens"            "Ehrharta calycina"          
    ## [21] "Eichhornia crassipes"        "Elodea nuttallii"           
    ## [23] "Eriocheir sinensis"          "Faxonius rusticus"          
    ## [25] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [27] "Gambusia holbrooki"          "Gunnera tinctoria"          
    ## [29] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [31] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [33] "Heracleum sosnowskyi"        "Herpestes javanicus"        
    ## [35] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [37] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [39] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [41] "Lepomis gibbosus"            "Lespedeza cuneata"          
    ## [43] "Limnoperna fortunei"         "Lithobates catesbeianus"    
    ## [45] "Ludwigia grandiflora"        "Ludwigia peploides"         
    ## [47] "Lygodium japonicum"          "Lysichiton americanus"      
    ## [49] "Microstegium vimineum"       "Morone americana"           
    ## [51] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [53] "Myriophyllum aquaticum"      "Myriophyllum heterophyllum" 
    ## [55] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [57] "Ondatra zibethicus"          "Orconectes limosus"         
    ## [59] "Orconectes virilis"          "Oxyura jamaicensis"         
    ## [61] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [63] "Pennisetum setaceum"         "Perccottus glenii"          
    ## [65] "Persicaria perfoliata"       "Pistia stratiotes"          
    ## [67] "Plotosus lineatus"           "Procambarus clarkii"        
    ## [69] "Procambarus virginalis"      "Procyon lotor"              
    ## [71] "Prosopis juliflora"          "Pseudorasbora parva"        
    ## [73] "Pueraria montana"            "Pycnonotus cafer"           
    ## [75] "Rugulopteryx okamurae"       "Salvinia molesta"           
    ## [77] "Sciurus carolinensis"        "Sciurus niger"              
    ## [79] "Solenopsis geminata"         "Solenopsis invicta"         
    ## [81] "Solenopsis richteri"         "Tamias sibiricus"           
    ## [83] "Threskiornis aethiopicus"    "Trachemys scripta elegans"  
    ## [85] "Triadica sebifera"           "Vespa velutina nigrithorax" 
    ## [87] "Wasmannia auropunctata"      "Xenopus laevis"

``` r
#––Define language → country mapping (based on the language that most people speak in the respective countries)
lang2country <- list(
  en = c("GB","IE"), es = "ES", el = c("GR","CY"), fr = c("FR","LU"), de = c("DE","AT","CH","LI"),
  it = c("IT"), pt = "PT", nl = c("NL","BE"), sv = "SE", da = "DK", fi = "FI", cs = "CZ",
  hr = "HR", hu = "HU", pl = "PL", ro = "RO", sk = "SK", sl = "SI",
  bg = "BG", et = "EE", lv = "LV", lt = "LT", mt = "MT",
  no = "NO", is = "IS", mk = "MK", bs = c("BA"), sq = c("AL","XK"), sr = c("RS","ME")
)

# Sanity check: ensure all intro countries are covered
mapped_countries <- unique(unlist(lang2country))
missing_countries <- setdiff(unique(intro_year$COUNTRY), mapped_countries)
if (length(missing_countries) > 0) {
  warning(paste(
    "Countries with no language mapping:", paste(missing_countries, collapse = ", ")
  ))
}

# language to country mapping
pv2 <- pv %>%
  rename(SCIENTIFIC_NAME = `Scientific Name`) %>%
  mutate(COUNTRY = purrr::map(Language, ~lang2country[[.x]])) %>%
  unnest_longer(COUNTRY)

setdiff(unique(pv$`Scientific Name`), unique(pv2$SCIENTIFIC_NAME))
```

    ## character(0)

``` r
unique(pv2$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Acridotheres tristis"       
    ##  [3] "Ailanthus altissima"         "Alopochen aegyptiaca"       
    ##  [5] "Alternanthera philoxeroides" "Ameiurus melas"             
    ##  [7] "Andropogon virginicus"       "Arthurdendyus triangulatus" 
    ##  [9] "Asclepias syriaca"           "Axis axis"                  
    ## [11] "Baccharis halimifolia"       "Cabomba caroliniana"        
    ## [13] "Callosciurus erythraeus"     "Callosciurus finlaysonii"   
    ## [15] "Cardiospermum grandiflorum"  "Celastrus orbiculatus"      
    ## [17] "Channa argus"                "Cortaderia jubata"          
    ## [19] "Corvus splendens"            "Ehrharta calycina"          
    ## [21] "Eichhornia crassipes"        "Elodea nuttallii"           
    ## [23] "Eriocheir sinensis"          "Faxonius rusticus"          
    ## [25] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [27] "Gambusia holbrooki"          "Gunnera tinctoria"          
    ## [29] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [31] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [33] "Heracleum sosnowskyi"        "Herpestes javanicus"        
    ## [35] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [37] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [39] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [41] "Lepomis gibbosus"            "Lespedeza cuneata"          
    ## [43] "Limnoperna fortunei"         "Lithobates catesbeianus"    
    ## [45] "Ludwigia grandiflora"        "Ludwigia peploides"         
    ## [47] "Lygodium japonicum"          "Lysichiton americanus"      
    ## [49] "Microstegium vimineum"       "Morone americana"           
    ## [51] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [53] "Myriophyllum aquaticum"      "Myriophyllum heterophyllum" 
    ## [55] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [57] "Ondatra zibethicus"          "Orconectes limosus"         
    ## [59] "Orconectes virilis"          "Oxyura jamaicensis"         
    ## [61] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [63] "Pennisetum setaceum"         "Perccottus glenii"          
    ## [65] "Persicaria perfoliata"       "Pistia stratiotes"          
    ## [67] "Plotosus lineatus"           "Procambarus clarkii"        
    ## [69] "Procambarus virginalis"      "Procyon lotor"              
    ## [71] "Prosopis juliflora"          "Pseudorasbora parva"        
    ## [73] "Pueraria montana"            "Pycnonotus cafer"           
    ## [75] "Rugulopteryx okamurae"       "Salvinia molesta"           
    ## [77] "Sciurus carolinensis"        "Sciurus niger"              
    ## [79] "Solenopsis geminata"         "Solenopsis invicta"         
    ## [81] "Solenopsis richteri"         "Tamias sibiricus"           
    ## [83] "Threskiornis aethiopicus"    "Trachemys scripta elegans"  
    ## [85] "Triadica sebifera"           "Vespa velutina nigrithorax" 
    ## [87] "Wasmannia auropunctata"      "Xenopus laevis"

``` r
# Assuming pv2 is your starting data frame
pv_long_wiki <- pv2 %>%
  pivot_longer(
    cols = matches("^[0-9]{8}$"),
    names_to = "date",
    values_to = "views"
  ) %>%
  mutate(
    date = ymd(date),
    views = as.numeric(views)
  ) %>%
  # Fill NA values in 'views' with 0
  replace_na(list(views = 0)) %>%
  select(SCIENTIFIC_NAME, COUNTRY, date, views)

glimpse((pv_long_wiki))
```

    ## Rows: 5,186,236
    ## Columns: 4
    ## $ SCIENTIFIC_NAME <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna", …
    ## $ COUNTRY         <chr> "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", …
    ## $ date            <date> 2016-01-01, 2016-01-02, 2016-01-03, 2016-01-04, 2016-…
    ## $ views           <dbl> 5, 10, 4, 3, 4, 5, 7, 41, 6, 5, 4, 7, 4, 4, 7, 3, 6, 3…

## 2.3. Load, join and pre-process the wikipedia geolocated pageview datasets

``` r
#load in the data
wikidat_geo <- data.table::fread("species_pageviews_wiki_geolocated_2023-02-06_now.csv",header=TRUE)
head(wikidat_geo[,1:5])
```

    ##         Scientific Name   Country Wikidata Q-number 2023-02-06 2023-02-07
    ##                  <char>    <char>            <char>      <num>      <num>
    ## 1:       Acacia saligna    France           Q402385         NA         NA
    ## 2:       Acacia saligna   Germany           Q402385         NA         NA
    ## 3:       Acacia saligna    Israel           Q402385         NA         NA
    ## 4: Acridotheres tristis   Algeria           Q116667         NA         NA
    ## 5: Acridotheres tristis Australia           Q116667         NA         96
    ## 6: Acridotheres tristis  Bulgaria           Q116667         NA         NA

``` r
wikidat_geo_old <- data.table::fread("species_pageviews_wiki_geolocated_2017-02-09_2023-02-05.csv", header=TRUE)
head(wikidat_geo_old[, 1:5])
```

    ##         Scientific Name     Country Wikidata Q-number 2017-02-09 2017-02-10
    ##                  <char>      <char>            <char>      <num>      <num>
    ## 1: Acridotheres tristis     Germany           Q116667         NA         NA
    ## 2: Acridotheres tristis       India           Q116667         NA         NA
    ## 3: Acridotheres tristis      Israel           Q116667         NA         NA
    ## 4: Acridotheres tristis       Japan           Q116667         NA         NA
    ## 5: Acridotheres tristis Netherlands           Q116667         NA         NA
    ## 6: Acridotheres tristis      Taiwan           Q116667         NA         NA

``` r
# Join the two dataframes on shared ID columns
combined_geo <- full_join(
  wikidat_geo_old,
  wikidat_geo,
  by = c("Scientific Name", "Country", "Wikidata Q-number")
)

head(combined_geo[,1:5])
```

    ##         Scientific Name     Country Wikidata Q-number 2017-02-09 2017-02-10
    ##                  <char>      <char>            <char>      <num>      <num>
    ## 1: Acridotheres tristis     Germany           Q116667         NA         NA
    ## 2: Acridotheres tristis       India           Q116667         NA         NA
    ## 3: Acridotheres tristis      Israel           Q116667         NA         NA
    ## 4: Acridotheres tristis       Japan           Q116667         NA         NA
    ## 5: Acridotheres tristis Netherlands           Q116667         NA         NA
    ## 6: Acridotheres tristis      Taiwan           Q116667         NA         NA

``` r
write_csv(combined_geo, "combined_wiki_pageviews_geo_2017-now.csv")
```

    ## Registered S3 method overwritten by 'bit':
    ##   method   from  
    ##   print.ri gamlss

``` r
combined_geo_mapped <- combined_geo %>%
  mutate(
    COUNTRY = countrycode(Country,
                               origin = "country.name",
                               destination = "iso2c")
  )
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `COUNTRY = countrycode(Country, origin = "country.name",
    ##   destination = "iso2c")`.
    ## Caused by warning:
    ## ! Some values were not matched unambiguously: unknown

``` r
head(combined_geo_mapped[,1:5])
```

    ##         Scientific Name     Country Wikidata Q-number 2017-02-09 2017-02-10
    ##                  <char>      <char>            <char>      <num>      <num>
    ## 1: Acridotheres tristis     Germany           Q116667         NA         NA
    ## 2: Acridotheres tristis       India           Q116667         NA         NA
    ## 3: Acridotheres tristis      Israel           Q116667         NA         NA
    ## 4: Acridotheres tristis       Japan           Q116667         NA         NA
    ## 5: Acridotheres tristis Netherlands           Q116667         NA         NA
    ## 6: Acridotheres tristis      Taiwan           Q116667         NA         NA

``` r
# Find the unique country names that were not matched
unmatched_countries <- combined_geo_mapped %>%
  filter(is.na(COUNTRY)) %>%
  distinct(Country)

# Print the list of unmatched names
print(unmatched_countries)
```

    ##    Country
    ##     <char>
    ## 1: unknown

``` r
# narrow down to EU countries on list
desired_iso2_countries <- c( 
  "GB","IE", "ES", "GR", "FR","LU", "DE","AT","CH", "IT", "PT", "NL","BE",
  "SE", "DK", "FI", "CZ", "HR", "HU", "PL", "RO", "SK", "SI", "BG", "EE",
  "LV", "LT", "MT", "CY", "NO","IS","MK","BA","RS","AL","XK", "LI", "ME"
)

filtered_data <- combined_geo_mapped %>%
  rename(SCIENTIFIC_NAME = 'Scientific Name')%>%
  filter(COUNTRY %in% desired_iso2_countries)

unique(filtered_data$COUNTRY)
```

    ##  [1] "DE" "NL" "AT" "BE" "HR" "CZ" "DK" "FR" "GR" "IT" "SI" "ES" "SE" "GB" "PL"
    ## [16] "HU" "CH" "BG" "FI" "IE" "NO" "SK" "LT" "EE" "MK" "RO" "RS" "LU" "IS" "PT"
    ## [31] "BA"

``` r
# Convert date columns to long format
long_data_geo <- filtered_data %>%
  pivot_longer(
    cols = matches("^\\d{4}-\\d{2}-\\d{2}$"),
    names_to = "Date",
    values_to = "Pageviews"
  )

glimpse((long_data_geo))
```

    ## Rows: 1,230,890
    ## Columns: 6
    ## $ SCIENTIFIC_NAME     <chr> "Acridotheres tristis", "Acridotheres tristis", "A…
    ## $ Country             <chr> "Germany", "Germany", "Germany", "Germany", "Germa…
    ## $ `Wikidata Q-number` <chr> "Q116667", "Q116667", "Q116667", "Q116667", "Q1166…
    ## $ COUNTRY             <chr> "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "D…
    ## $ Date                <chr> "2017-02-09", "2017-02-10", "2017-02-11", "2017-02…
    ## $ Pageviews           <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
summary(long_data_geo)
```

    ##  SCIENTIFIC_NAME      Country          Wikidata Q-number    COUNTRY         
    ##  Length:1230890     Length:1230890     Length:1230890     Length:1230890    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##      Date             Pageviews      
    ##  Length:1230890     Min.   :   90.0  
    ##  Class :character   1st Qu.:  195.0  
    ##  Mode  :character   Median :  451.0  
    ##                     Mean   :  519.1  
    ##                     3rd Qu.:  682.0  
    ##                     Max.   :20686.0  
    ##                     NA's   :1198047

## 2.4. Load and pre-process geolocated Google health activity data

``` r
# Set the path to the folder containing the CSV files
folder_path <- "C:/Users/simon/Documents/Google health_OneStop"

# List all CSV files in the folder
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Function to read a CSV and add species name
read_and_tag <- function(file_path) {
  # Extract the species name from the file name (remove directory and extension)
  species_name <- tools::file_path_sans_ext(basename(file_path))
  
  # Read the CSV file
  df <- read.csv(file_path)
  
  # Add species name column
  df$Species <- species_name
  
  return(df)
}

# Apply the function to all files and combine into one DataFrame
combined_df <- do.call(rbind, lapply(csv_files, read_and_tag))

# Reorder columns to put Species first (optional)
combined_df <- combined_df %>% select(Species, everything())

# View result
glimpse((combined_df))
```

    ## Rows: 40,506
    ## Columns: 30
    ## $ Species <chr> "Acacia_saligna", "Acacia_saligna", "Acacia_saligna", "Acacia_…
    ## $ date    <chr> "2015-12-27", "2016-01-03", "2016-01-10", "2016-01-17", "2016-…
    ## $ AT      <dbl> 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000…
    ## $ BE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ BG      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ CY      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ CZ      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ DE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ DK      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ EE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ ES      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ FI      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ FR      <dbl> 0.000000, 0.000000, 8.675739, 0.000000, 0.000000, 0.000000, 0.…
    ## $ GB      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ GR      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ HR      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ HU      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ IE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ IT      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ LT      <dbl> 0.0000, 0.0000, 0.0000, 281.4073, 0.0000, 0.0000, 0.0000, 0.00…
    ## $ LU      <dbl> 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000, 0.000,…
    ## $ LV      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ MT      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ NL      <dbl> 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.00000,…
    ## $ PL      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ PT      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ RO      <dbl> 0.00000, 0.00000, 71.18939, 0.00000, 0.00000, 0.00000, 0.00000…
    ## $ SE      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ SI      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ SK      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

``` r
#Reshape combined_df from wide to long format
long_df <- combined_df %>%
  pivot_longer(
    cols = -c(Species, date),
    names_to = "COUNTRY",
    values_to = "value"
  )

# Clean species names to match intro_year_googlehealth format
long_df <- long_df %>%
  mutate(SCIENTIFIC_NAME = str_replace_all(Species, "_", " "))

# Filter to only relevant columns
final_df_googlehealth <- long_df %>%
  select(date, SCIENTIFIC_NAME, COUNTRY, value)

# View the result
glimpse((final_df_googlehealth))
```

    ## Rows: 1,134,168
    ## Columns: 4
    ## $ date            <chr> "2015-12-27", "2015-12-27", "2015-12-27", "2015-12-27"…
    ## $ SCIENTIFIC_NAME <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna", …
    ## $ COUNTRY         <chr> "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", …
    ## $ value           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

``` r
unique(final_df_googlehealth$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Acridotheres tristis"       
    ##  [3] "Ailanthus altissima"         "Alopochen aegyptiaca"       
    ##  [5] "Alternanthera philoxeroides" "Ameiurus melas"             
    ##  [7] "Andropogon virginicus"       "Arthurdendyus triangulatus" 
    ##  [9] "Asclepias syriaca"           "Baccharis halimifolia"      
    ## [11] "Cabomba caroliniana"         "Callosciurus erythraeus"    
    ## [13] "Callosciurus finlaysonii"    "Cardiospermum grandiflorum" 
    ## [15] "Celastrus orbiculatus"       "Channa argus"               
    ## [17] "Cortaderia jubata"           "Corvus splendens"           
    ## [19] "Ehrharta calycina"           "Elodea nuttallii"           
    ## [21] "Eriocheir sinensis"          "Faxonius limosus"           
    ## [23] "Faxonius rusticus"           "Faxonius virilis"           
    ## [25] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [27] "Gambusia holbrooki"          "Gunnera tinctoria"          
    ## [29] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [31] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [33] "Heracleum sosnowskyi"        "Herpestes auropunctatus"    
    ## [35] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [37] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [39] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [41] "Lepomis gibbosus"            "Lespedeza cuneata"          
    ## [43] "Limnoperna fortunei"         "Lithobates catesbeianus"    
    ## [45] "Ludwigia grandiflora"        "Ludwigia peploides"         
    ## [47] "Lygodium japonicum"          "Lysichiton americanus"      
    ## [49] "Microstegium vimineum"       "Morone americana"           
    ## [51] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [53] "Myriophyllum aquaticum"      "Myriophyllum heterophyllum" 
    ## [55] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [57] "Ondatra zibethicus"          "Oxyura jamaicensis"         
    ## [59] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [61] "Pennisetum setaceum"         "Perccottus glenii"          
    ## [63] "Persicaria perfoliata"       "Pistia stratiotes"          
    ## [65] "Plotosus lineatus"           "Pontederia crassipes"       
    ## [67] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [69] "Procyon lotor"               "Prosopis juliflora"         
    ## [71] "Pseudorasbora parva"         "Pueraria montana"           
    ## [73] "Pycnonotus cafer"            "Rugulopteryx okamurae"      
    ## [75] "Salvinia molesta"            "Sciurus carolinensis"       
    ## [77] "Sciurus niger"               "Solenopsis geminata"        
    ## [79] "Solenopsis invicta"          "Solenopsis richteri"        
    ## [81] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [83] "Trachemys scripta"           "Triadica sebifera"          
    ## [85] "Wasmannia auropunctata"      "Xenopus laevis"

## 2.5. Load and pre-process geolocated Flickr data

``` r
# Read the CSV file (update path if needed)
data_flickr <- data.table::fread("deduplicated_geocoded_flickr_2004-now.csv",header=TRUE)

daily_counts_flickr <- data_flickr %>%
  mutate(
    date = as.Date(date_taken), # extract date only, ignore time
    # Convert detected_country (full name) to ISO2 code (e.g., "US", "GB")
    detected_country_iso2 = countrycode(detected_country,
                                        origin = "country.name",
                                        destination = "iso2c",
                                        nomatch = NA_character_),
  ) %>%
  # Filter to include only the desired EU countries
  filter(detected_country_iso2 %in% desired_iso2_countries ) %>%
  rename(COUNTRY = detected_country_iso2)%>%
  # Group by species, the NEW ISO2 country column, and date to count observations
  group_by(scientific_name, COUNTRY, date) %>%
  summarise(observations = n(), .groups = "drop")

glimpse((daily_counts_flickr))
```

    ## Rows: 2,702
    ## Columns: 4
    ## $ scientific_name <chr> "Acacia saligna", "Acacia saligna", "Ailanthus altissi…
    ## $ COUNTRY         <chr> "ES", "GR", "AT", "DE", "DE", "DE", "DE", "DE", "DE", …
    ## $ date            <date> 2008-01-04, 2016-05-02, 2022-11-01, 2008-08-03, 2009-…
    ## $ observations    <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …

``` r
# Create full date range (This part seems independent of country filtering logic)
full_dates <- seq.Date(from = as.Date("2016-01-01"), to = Sys.Date(), by = "day")

# Expand the data to all combinations, fill missing observations with 0
expanded_df_flickr <- daily_counts_flickr %>%
  # Select the scientific_name, the NEW ISO2 country column, date, and observations
  select(scientific_name, COUNTRY, date, observations) %>%
  distinct() %>% # Ensure unique combinations before expanding
  complete(
    scientific_name,
    # Use the ISO2 country column for expansion
    COUNTRY,
    date = full_dates,
    fill = list(observations = 0)
  )

glimpse((expanded_df_flickr))
```

    ## Rows: 4,261,238
    ## Columns: 4
    ## $ scientific_name <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna", …
    ## $ COUNTRY         <chr> "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", …
    ## $ date            <date> 2016-01-01, 2016-01-02, 2016-01-03, 2016-01-04, 2016-…
    ## $ observations    <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …

## 2.6. Load and pre-process geolocated iNaturalist casual observations

``` r
# Read the CSV file (update path if needed)
data_inat <- data.table::fread("species_country_observations_inat_2016_present.csv",header=TRUE)

head(data_inat[,1:5])
```

    ##    Scientific Name Country 2016-01-01 2016-01-02 2016-01-03
    ##             <char>  <char>      <num>      <num>      <int>
    ## 1:  Acacia saligna      AL          0          0          0
    ## 2:  Acacia saligna      AT          0          0          0
    ## 3:  Acacia saligna      ES          0          0          0
    ## 4:  Acacia saligna      FR          0          0          0
    ## 5:  Acacia saligna      GR          0          0          0
    ## 6:  Acacia saligna      IT          0          0          0

``` r
# Convert data_inat to long format
long_inat_df <- data_inat %>%
  # Rename 'Scientific.Name' to 'SCIENTIFIC_NAME' for consistency
  rename(SCIENTIFIC_NAME = `Scientific Name`) %>%
  # Pivot all columns EXCEPT 'SCIENTIFIC_NAME' and 'Country'
  pivot_longer(
    cols = -c(SCIENTIFIC_NAME, Country), # Selects all columns EXCEPT these ID columns
    names_to = "date",                   # New column to store the date (old column names)
    values_to = "observations"           # New column to store the observation values
  ) %>%
  # Ensure the date column is in date format
  mutate(date = as.Date(date)) %>%
  # Add the 'Internet_platform' column with the tag 'inat'
  mutate(Internet_platform = "iNaturalist (casual)",
         views = observations,
         COUNTRY = Country
         ) %>%
  select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform) #reorder as intended

#display first rows
(glimpse(long_inat_df))
```

    ## Rows: 3,397,875
    ## Columns: 5
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"…
    ## $ date              <date> 2016-01-01, 2016-01-02, 2016-01-03, 2016-01-04, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ Internet_platform <chr> "iNaturalist (casual)", "iNaturalist (casual)", "iNa…

    ## # A tibble: 3,397,875 × 5
    ##    SCIENTIFIC_NAME COUNTRY date       views Internet_platform   
    ##    <chr>           <chr>   <date>     <dbl> <chr>               
    ##  1 Acacia saligna  AL      2016-01-01     0 iNaturalist (casual)
    ##  2 Acacia saligna  AL      2016-01-02     0 iNaturalist (casual)
    ##  3 Acacia saligna  AL      2016-01-03     0 iNaturalist (casual)
    ##  4 Acacia saligna  AL      2016-01-04     0 iNaturalist (casual)
    ##  5 Acacia saligna  AL      2016-01-05     0 iNaturalist (casual)
    ##  6 Acacia saligna  AL      2016-01-06     0 iNaturalist (casual)
    ##  7 Acacia saligna  AL      2016-01-07     0 iNaturalist (casual)
    ##  8 Acacia saligna  AL      2016-01-08     0 iNaturalist (casual)
    ##  9 Acacia saligna  AL      2016-01-09     0 iNaturalist (casual)
    ## 10 Acacia saligna  AL      2016-01-10     0 iNaturalist (casual)
    ## # ℹ 3,397,865 more rows

## 2.7. Load and pre-process youtube geolocated videos

``` r
# Define the folder path where the YouTube CSVs are located
# IMPORTANT: Adjust this path to match your actual directory structure.
youtube_folder_path <- "youtube_results_2016-now_fuzzymatch"

# Get a list of all CSV files in the folder
csv_files <- list.files(
  path = youtube_folder_path,
  pattern = "\\.csv$",
  full.names = TRUE
)

# Initialize an empty list to store processed data frames
youtube_data_list <- list()

# Loop through each CSV file
for (file_path in csv_files) {
  # Read the CSV file
  df <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Process the data:
  # 1. Extract date from 'published_at'
  # 2. Rename 'species' column to 'SCIENTIFIC_NAME' for consistency
  # 3. Count observations per day for each species x country combination
  processed_df <- df %>%
    mutate(
      date = as.Date(substr(published_at, 1, 10)), # Extract YYYY-MM-DD
      SCIENTIFIC_NAME = species # Rename for consistency
    ) %>%
    group_by(SCIENTIFIC_NAME, country, date) %>%
    summarise(observations = n(), .groups = "drop")
  
  youtube_data_list[[length(youtube_data_list) + 1]] <- processed_df
}

# Combine all processed data frames into a single one
if (length(youtube_data_list) > 0) {
  raw_youtube_counts <- bind_rows(youtube_data_list)
} else {
  raw_youtube_counts <- data.frame(
    SCIENTIFIC_NAME = character(),
    country = character(),
    date = as.Date(character()),
    observations = numeric()
  )
}

# Define the start date
start_date <- as.Date("2016-01-01")

# Determine the end date from the data, or current date if no data is present
# Using an if-else block to ensure end_date is always a Date object
if (nrow(raw_youtube_counts) > 0) {
  end_date <- max(raw_youtube_counts$date)
} else {
  end_date <- Sys.Date()
}

# Create a complete sequence of dates from 2016-01-01 to the latest date found
all_dates <- seq(from = start_date, to = end_date, by = "day")

# Use complete() instead of expand_grid() for more efficient date filling
processed_youtube_df <- raw_youtube_counts %>%
  complete(SCIENTIFIC_NAME, country, date = all_dates, fill = list(observations = 0)) %>%
  mutate(Internet_platform = "Youtube",
         COUNTRY = countrycode(country, # This is the original 'COUNTRY' column with full names
                                    origin = "country.name",
                                    destination = "iso2c",
                                    nomatch = NA_character_)) %>%
        
  filter(COUNTRY %in% desired_iso2_countries) %>%
  rename(views = observations) %>%
  select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform)

# Display the first few rows of the final processed YouTube dataframe
print(glimpse((processed_youtube_df)))
```

    ## Rows: 8,779,680
    ## Columns: 5
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT", "AT"…
    ## $ date              <date> 2016-01-01, 2016-01-02, 2016-01-03, 2016-01-04, 201…
    ## $ views             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ Internet_platform <chr> "Youtube", "Youtube", "Youtube", "Youtube", "Youtube…
    ## # A tibble: 8,779,680 × 5
    ##    SCIENTIFIC_NAME COUNTRY date       views Internet_platform
    ##    <chr>           <chr>   <date>     <int> <chr>            
    ##  1 Acacia saligna  AT      2016-01-01     0 Youtube          
    ##  2 Acacia saligna  AT      2016-01-02     0 Youtube          
    ##  3 Acacia saligna  AT      2016-01-03     0 Youtube          
    ##  4 Acacia saligna  AT      2016-01-04     0 Youtube          
    ##  5 Acacia saligna  AT      2016-01-05     0 Youtube          
    ##  6 Acacia saligna  AT      2016-01-06     0 Youtube          
    ##  7 Acacia saligna  AT      2016-01-07     0 Youtube          
    ##  8 Acacia saligna  AT      2016-01-08     0 Youtube          
    ##  9 Acacia saligna  AT      2016-01-09     0 Youtube          
    ## 10 Acacia saligna  AT      2016-01-10     0 Youtube          
    ## # ℹ 8,779,670 more rows

## 2.8. Load and pre-process the Facebook data

``` r
facebook_df <-  data.table::fread("facebook_data_normalized.csv",header=TRUE) #read in data, ALREADY NORMALIZED DUE TO VDE REQUIREMENTS!
glimpse(facebook_df)
```

    ## Rows: 183,080
    ## Columns: 4
    ## $ species          <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna",…
    ## $ country          <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL",…
    ## $ month            <chr> "2016-01", "2016-02", "2016-03", "2016-04", "2016-05"…
    ## $ normalized_count <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…

``` r
facebook_df$date_string <- paste0(facebook_df$month, "-01")
facebook_df$date= as.POSIXct(facebook_df$date_string, format = "%Y-%m-%d") #make date object

glimpse(facebook_df$date)
```

    ##  POSIXct[1:183080], format: "2016-01-01" "2016-02-01" "2016-03-01" "2016-04-01" "2016-05-01" ...

``` r
facebook_df_clean <- facebook_df %>%
  rename(
    views = normalized_count,
    COUNTRY = country,
    SCIENTIFIC_NAME = species
  ) %>%
  mutate(
    # Convert the POSIXct date to a simple Date object
    date = as.Date(date),
    Internet_platform = "Facebook"
  ) %>%
  select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform)

glimpse((facebook_df_clean))
```

    ## Rows: 183,080
    ## Columns: 5
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"…
    ## $ date              <date> 2015-12-31, 2016-01-31, 2016-02-29, 2016-03-31, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ Internet_platform <chr> "Facebook", "Facebook", "Facebook", "Facebook", "Fac…

``` r
unique(facebook_df_clean$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Acridotheres tristis"       
    ##  [3] "Ailanthus altissima"         "Alopochen aegyptiaca"       
    ##  [5] "Alternanthera philoxeroides" "Ameiurus melas"             
    ##  [7] "Asclepias syriaca"           "Axis axis"                  
    ##  [9] "Baccharis halimifolia"       "Cabomba caroliniana"        
    ## [11] "Callosciurus erythraeus"     "Callosciurus finlaysonii"   
    ## [13] "Cardiospermum grandiflorum"  "Celastrus orbiculatus"      
    ## [15] "Channa argus"                "Cortaderia jubata"          
    ## [17] "Corvus splendens"            "Ehrharta calycina"          
    ## [19] "Eichhornia crassipes"        "Elodea nuttallii"           
    ## [21] "Eriocheir sinensis"          "Faxonius rusticus"          
    ## [23] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [25] "Gambusia holbrooki"          "Gunnera tinctoria"          
    ## [27] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [29] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [31] "Heracleum sosnowskyi"        "Herpestes javanicus"        
    ## [33] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [35] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [37] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [39] "Lepomis gibbosus"            "Lespedeza cuneata"          
    ## [41] "Limnoperna fortunei"         "Lithobates catesbeianus"    
    ## [43] "Ludwigia grandiflora"        "Ludwigia peploides"         
    ## [45] "Lygodium japonicum"          "Lysichiton americanus"      
    ## [47] "Microstegium vimineum"       "Morone americana"           
    ## [49] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [51] "Myriophyllum aquaticum"      "Myriophyllum heterophyllum" 
    ## [53] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [55] "Ondatra zibethicus"          "Orconectes limosus"         
    ## [57] "Orconectes virilis"          "Oxyura jamaicensis"         
    ## [59] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [61] "Perccottus glenii"           "Persicaria perfoliata"      
    ## [63] "Pistia stratiotes"           "Plotosus lineatus"          
    ## [65] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [67] "Procyon lotor"               "Prosopis juliflora"         
    ## [69] "Pseudorasbora parva"         "Pueraria montana"           
    ## [71] "Pycnonotus cafer"            "Rugulopteryx okamurae"      
    ## [73] "Salvinia molesta"            "Sciurus carolinensis"       
    ## [75] "Sciurus niger"               "Solenopsis geminata"        
    ## [77] "Solenopsis invicta"          "Solenopsis richteri"        
    ## [79] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [81] "Trachemys scripta"           "Triadica sebifera"          
    ## [83] "Vespa velutina"              "Wasmannia auropunctata"     
    ## [85] "Xenopus laevis"

``` r
unique(facebook_df_clean$COUNTRY)
```

    ##  [1] "AL" "BG" "CH" "CY" "CZ" "DE" "DK" "ES" "FR" "GB" "GR" "IE" "IT" "LT" "MT"
    ## [16] "NL" "PL" "PT" "RO" "AT" "BE" "FI" "HU" "SE" "SK" "BA" "HR" "ME" "MK" "NO"
    ## [31] "RS" "SI" "XK" "IS" "LU" "LV" "EE"

## 2.9. Load and pre-process the GBIF data

``` r
gbif_df <-  data.table::fread("GBIF_Observations_2016-present_final.csv",header=TRUE)
head((gbif_df[,1:5]))
```

    ##    Scientific Name Country 2016-01-01 2016-01-02 2016-01-03
    ##             <char>  <char>      <num>      <num>      <num>
    ## 1:  Acacia saligna      AL          0          0          0
    ## 2:  Acacia saligna      BE          0          0          0
    ## 3:  Acacia saligna      CY          0          0          0
    ## 4:  Acacia saligna      DK          0          0          0
    ## 5:  Acacia saligna      ES          0          0          0
    ## 6:  Acacia saligna      FR          0          0          0

``` r
gbif_long <- gbif_df %>%
  pivot_longer( #pivot table
    cols = 3:ncol(gbif_df),
    names_to = "Date",
    values_to = "Observations"
  ) %>%
  mutate(
    Date = as.Date(Date, format = "%Y-%m-%d"), #make date object
    COUNTRY = Country,
    date = Date,
    views = Observations,
    SCIENTIFIC_NAME = `Scientific Name`,
    Internet_platform = "GBIF"
  ) %>%
  select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform) #select properly named and reordered columns

# View the result
glimpse((gbif_long))
```

    ## Rows: 3,158,174
    ## Columns: 5
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"…
    ## $ date              <date> 2016-01-01, 2016-01-02, 2016-01-03, 2016-01-04, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ Internet_platform <chr> "GBIF", "GBIF", "GBIF", "GBIF", "GBIF", "GBIF", "GBI…

``` r
unique(gbif_long$COUNTRY)
```

    ##  [1] "AL" "BE" "CY" "DK" "ES" "FR" "GR" "IT" "MT" "PT" "DE" "GB" "NL" "RS" "SE"
    ## [16] "AT" "BA" "BG" "CH" "CZ" "HR" "HU" "LI" "LU" "ME" "MK" "NO" "PL" "RO" "SI"
    ## [31] "SK" "XK" "IE" "LT" "EE" "LV" "FI" "IS"

``` r
unique(gbif_long$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Acridotheres tristis"       
    ##  [3] "Ailanthus altissima"         "Alopochen aegyptiaca"       
    ##  [5] "Alternanthera philoxeroides" "Ameiurus melas"             
    ##  [7] "Andropogon virginicus"       "Arthurdendyus triangulatus" 
    ##  [9] "Asclepias syriaca"           "Axis axis"                  
    ## [11] "Baccharis halimifolia"       "Cabomba caroliniana"        
    ## [13] "Callosciurus erythraeus"     "Callosciurus finlaysonii"   
    ## [15] "Cardiospermum grandiflorum"  "Celastrus orbiculatus"      
    ## [17] "Cortaderia jubata"           "Corvus splendens"           
    ## [19] "Ehrharta calycina"           "Eichhornia crassipes"       
    ## [21] "Elodea nuttallii"            "Eriocheir sinensis"         
    ## [23] "Faxonius rusticus"           "Fundulus heteroclitus"      
    ## [25] "Gambusia affinis"            "Gambusia holbrooki"         
    ## [27] "Gunnera tinctoria"           "Gymnocoronis spilanthoides" 
    ## [29] "Hakea sericea"               "Heracleum mantegazzianum"   
    ## [31] "Heracleum persicum"          "Heracleum sosnowskyi"       
    ## [33] "Herpestes javanicus"         "Humulus scandens"           
    ## [35] "Hydrocotyle ranunculoides"   "Impatiens glandulifera"     
    ## [37] "Koenigia polystachya"        "Lagarosiphon major"         
    ## [39] "Lampropeltis getula"         "Lepomis gibbosus"           
    ## [41] "Lespedeza cuneata"           "Lithobates catesbeianus"    
    ## [43] "Ludwigia grandiflora"        "Ludwigia peploides"         
    ## [45] "Lygodium japonicum"          "Lysichiton americanus"      
    ## [47] "Microstegium vimineum"       "Muntiacus reevesi"          
    ## [49] "Myocastor coypus"            "Myriophyllum aquaticum"     
    ## [51] "Myriophyllum heterophyllum"  "Nasua nasua"                
    ## [53] "Nyctereutes procyonoides"    "Ondatra zibethicus"         
    ## [55] "Orconectes limosus"          "Orconectes virilis"         
    ## [57] "Oxyura jamaicensis"          "Pacifastacus leniusculus"   
    ## [59] "Parthenium hysterophorus"    "Pennisetum setaceum"        
    ## [61] "Perccottus glenii"           "Persicaria perfoliata"      
    ## [63] "Pistia stratiotes"           "Plotosus lineatus"          
    ## [65] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [67] "Procyon lotor"               "Prosopis juliflora"         
    ## [69] "Pseudorasbora parva"         "Pueraria montana"           
    ## [71] "Pycnonotus cafer"            "Rugulopteryx okamurae"      
    ## [73] "Salvinia molesta"            "Sciurus carolinensis"       
    ## [75] "Sciurus niger"               "Solenopsis invicta"         
    ## [77] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [79] "Trachemys scripta elegans"   "Vespa velutina nigrithorax" 
    ## [81] "Wasmannia auropunctata"      "Xenopus laevis"

# 3. Combine all datasets into long format df including daily activity patterns

``` r
# Prepare long_data_geo (Wikipedia geotagged)
wiki_geo_df <- long_data_geo %>%
  rename(date = Date,
         views = Pageviews) %>%
  mutate(date = as.Date(date),
         views = ifelse(is.na(views), 0, views), #replace NA views with 0
         Internet_platform = "Wikipedia (geo)") %>%
  dplyr::select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform)

# Prepare pv_long (Wikipedia language views)
wiki_lang_df <- pv_long_wiki %>%
  mutate(views = ifelse(is.na(views), 0, views)) %>%  # Replace NA views with 0
  mutate(Internet_platform = "Wikipedia (lan)") %>%
  dplyr::select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform)

# Prepare long_df (Google Health Trends)
google_health_df <- long_df %>%
  mutate(
    views = ifelse(is.na(value), 0, value),  # fill NA with zero and create 'views'
    date = as.Date(date),
    Internet_platform = "Google health"
  ) %>%
  dplyr::select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform)


# Prepare data_flickr (Flickr images)
flickr_df <- expanded_df_flickr %>%
  rename(SCIENTIFIC_NAME = scientific_name) %>%
  mutate(
    views = ifelse(is.na(observations), 0, observations), #replace NA views with 0
    Internet_platform = "Flickr"
  ) %>% 
  dplyr::select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform)

# Perform zero padding (making sure all potential combinations have zero when no activity was mined)

# Create a master list of all unique species x country combinations
master_combos <- bind_rows(
  wiki_geo_df %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY),
  wiki_lang_df %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY),
  google_health_df %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY),
  flickr_df %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY),
  gbif_long %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY),
  long_inat_df %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY),
  processed_youtube_df %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY),
  facebook_df_clean %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY)
) %>%
  distinct()

# Create a full date sequence from min to max date
start_date <- as.Date("2016-01-01")
end_date <- as.Date("2025-07-15")
full_date_range <- data.frame(date = seq.Date(from = start_date, to = end_date, by = "month")) #monthly basis

# Create a master grid of all combinations for every date
master_grid <- tidyr::crossing(master_combos, full_date_range)

# Zero-pad each dataframe using the master grid
pad_and_fill <- function(df, master_grid, platform_name) {
  df %>%
    full_join(master_grid, by = c("SCIENTIFIC_NAME", "COUNTRY", "date")) %>%
    mutate(
      views = if_else(is.na(views), 0, views),
      Internet_platform = if_else(is.na(Internet_platform), platform_name, Internet_platform)
    ) %>%
    # Ensure all columns are in the same order
    dplyr::select(SCIENTIFIC_NAME, COUNTRY, date, views, Internet_platform)
}

# Apply padding to all dataframes
wiki_geo_df_padded <- pad_and_fill(wiki_geo_df, master_grid, "Wikipedia (geo)")
wiki_lang_df_padded <- pad_and_fill(wiki_lang_df, master_grid, "Wikipedia (lan)")
google_health_df_padded <- pad_and_fill(google_health_df, master_grid, "Google health")
flickr_df_padded <- pad_and_fill(flickr_df, master_grid, "Flickr")
gbif_long_padded <- pad_and_fill(gbif_long, master_grid, "GBIF")
long_inat_df_padded <- pad_and_fill(long_inat_df, master_grid, "iNaturalist (casual)")
processed_youtube_df_padded <- pad_and_fill(processed_youtube_df, master_grid, "Youtube")
facebook_df_clean_padded <- pad_and_fill(facebook_df_clean, master_grid, "Facebook")

# Put all your padded data frames into a list
list_of_padded_dfs <- list(
  wiki_geo_df_padded,
  wiki_lang_df_padded,
  google_health_df_padded,
  flickr_df_padded,
  gbif_long_padded,
  long_inat_df_padded,
  processed_youtube_df_padded,
  facebook_df_clean_padded
)

# Use rbindlist() to combine them all at once
combined_data_padded <- data.table::rbindlist(list_of_padded_dfs, fill = TRUE)

unique(combined_data_padded$SCIENTIFIC_NAME)
```

    ##  [1] "Acridotheres tristis"        "Ailanthus altissima"        
    ##  [3] "Alopochen aegyptiaca"        "Ameiurus melas"             
    ##  [5] "Asclepias syriaca"           "Axis axis"                  
    ##  [7] "Celastrus orbiculatus"       "Channa argus"               
    ##  [9] "Corvus splendens"            "Eichhornia crassipes"       
    ## [11] "Eriocheir sinensis"          "Gambusia affinis"           
    ## [13] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [15] "Heracleum sosnowskyi"        "Impatiens glandulifera"     
    ## [17] "Koenigia polystachya"        "Lampropeltis getula"        
    ## [19] "Lepomis gibbosus"            "Lithobates catesbeianus"    
    ## [21] "Lysichiton americanus"       "Muntiacus reevesi"          
    ## [23] "Myocastor coypus"            "Nasua nasua"                
    ## [25] "Nyctereutes procyonoides"    "Ondatra zibethicus"         
    ## [27] "Orconectes limosus"          "Pacifastacus leniusculus"   
    ## [29] "Parthenium hysterophorus"    "Perccottus glenii"          
    ## [31] "Pistia stratiotes"           "Plotosus lineatus"          
    ## [33] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [35] "Procyon lotor"               "Pseudorasbora parva"        
    ## [37] "Pueraria montana"            "Pycnonotus cafer"           
    ## [39] "Sciurus carolinensis"        "Sciurus niger"              
    ## [41] "Solenopsis invicta"          "Tamias sibiricus"           
    ## [43] "Threskiornis aethiopicus"    "Trachemys scripta elegans"  
    ## [45] "Vespa velutina nigrithorax"  "Wasmannia auropunctata"     
    ## [47] "Xenopus laevis"              "Acacia saligna"             
    ## [49] "Arthurdendyus triangulatus"  "Baccharis halimifolia"      
    ## [51] "Cabomba caroliniana"         "Callosciurus erythraeus"    
    ## [53] "Callosciurus finlaysonii"    "Cortaderia jubata"          
    ## [55] "Elodea nuttallii"            "Fundulus heteroclitus"      
    ## [57] "Gunnera tinctoria"           "Hakea sericea"              
    ## [59] "Herpestes javanicus"         "Ludwigia grandiflora"       
    ## [61] "Ludwigia peploides"          "Myriophyllum aquaticum"     
    ## [63] "Orconectes virilis"          "Oxyura jamaicensis"         
    ## [65] "Rugulopteryx okamurae"       "Triadica sebifera"          
    ## [67] "Alternanthera philoxeroides" "Andropogon virginicus"      
    ## [69] "Cardiospermum grandiflorum"  "Ehrharta calycina"          
    ## [71] "Faxonius limosus"            "Faxonius rusticus"          
    ## [73] "Faxonius virilis"            "Gambusia holbrooki"         
    ## [75] "Gymnocoronis spilanthoides"  "Herpestes auropunctatus"    
    ## [77] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [79] "Lagarosiphon major"          "Lespedeza cuneata"          
    ## [81] "Limnoperna fortunei"         "Lygodium japonicum"         
    ## [83] "Microstegium vimineum"       "Morone americana"           
    ## [85] "Myriophyllum heterophyllum"  "Pennisetum setaceum"        
    ## [87] "Persicaria perfoliata"       "Pontederia crassipes"       
    ## [89] "Prosopis juliflora"          "Salvinia molesta"           
    ## [91] "Solenopsis geminata"         "Solenopsis richteri"        
    ## [93] "Trachemys scripta"           "Vespa velutina"

``` r
glimpse(combined_data_padded)
```

    ## Rows: 29,401,873
    ## Columns: 5
    ## $ SCIENTIFIC_NAME   <chr> "Acridotheres tristis", "Acridotheres tristis", "Acr…
    ## $ COUNTRY           <chr> "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE"…
    ## $ date              <date> 2017-02-09, 2017-02-10, 2017-02-11, 2017-02-12, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ Internet_platform <chr> "Wikipedia (geo)", "Wikipedia (geo)", "Wikipedia (ge…

``` r
# Apply the renaming logic with corrections for synonyms and misspellings
standardized_df <-combined_data_padded %>%
  mutate(
    SCIENTIFIC_NAME = case_when(
      SCIENTIFIC_NAME == "Cortaderia selloana subsp. jubata" ~ "Cortaderia jubata",
      SCIENTIFIC_NAME == "Eichhornia crassipes" ~ "Pontederia crassipes",
      SCIENTIFIC_NAME == "Pennisetum setaceum" ~ "Cenchrus setaceus",
      SCIENTIFIC_NAME == "Orconectes limosus" ~ "Faxonius limosus",
      SCIENTIFIC_NAME == "Orconectes virilis" ~ "Faxonius virilis",
      SCIENTIFIC_NAME == "Vespa velutina nigrithorax" ~ "Vespa velutina",
      SCIENTIFIC_NAME == "Urva auropunctata" ~ "Herpestes javanicus",
      SCIENTIFIC_NAME == "Herpestes auropunctatus" ~ "Herpestes javanicus",
      SCIENTIFIC_NAME == "Prosopis juliflora" ~ "Neltuma juliflora",
      SCIENTIFIC_NAME == "Trachemys scripta elegans" ~ "Trachemys scripta",
      SCIENTIFIC_NAME == "Pueraria montana (Lour.) Merr. var. lobata" ~ "Pueraria montana",
      TRUE ~ SCIENTIFIC_NAME
    )
  )

# Now check the unique names again after the expanded fix
unique(standardized_df$SCIENTIFIC_NAME)
```

    ##  [1] "Acridotheres tristis"        "Ailanthus altissima"        
    ##  [3] "Alopochen aegyptiaca"        "Ameiurus melas"             
    ##  [5] "Asclepias syriaca"           "Axis axis"                  
    ##  [7] "Celastrus orbiculatus"       "Channa argus"               
    ##  [9] "Corvus splendens"            "Pontederia crassipes"       
    ## [11] "Eriocheir sinensis"          "Gambusia affinis"           
    ## [13] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [15] "Heracleum sosnowskyi"        "Impatiens glandulifera"     
    ## [17] "Koenigia polystachya"        "Lampropeltis getula"        
    ## [19] "Lepomis gibbosus"            "Lithobates catesbeianus"    
    ## [21] "Lysichiton americanus"       "Muntiacus reevesi"          
    ## [23] "Myocastor coypus"            "Nasua nasua"                
    ## [25] "Nyctereutes procyonoides"    "Ondatra zibethicus"         
    ## [27] "Faxonius limosus"            "Pacifastacus leniusculus"   
    ## [29] "Parthenium hysterophorus"    "Perccottus glenii"          
    ## [31] "Pistia stratiotes"           "Plotosus lineatus"          
    ## [33] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [35] "Procyon lotor"               "Pseudorasbora parva"        
    ## [37] "Pueraria montana"            "Pycnonotus cafer"           
    ## [39] "Sciurus carolinensis"        "Sciurus niger"              
    ## [41] "Solenopsis invicta"          "Tamias sibiricus"           
    ## [43] "Threskiornis aethiopicus"    "Trachemys scripta"          
    ## [45] "Vespa velutina"              "Wasmannia auropunctata"     
    ## [47] "Xenopus laevis"              "Acacia saligna"             
    ## [49] "Arthurdendyus triangulatus"  "Baccharis halimifolia"      
    ## [51] "Cabomba caroliniana"         "Callosciurus erythraeus"    
    ## [53] "Callosciurus finlaysonii"    "Cortaderia jubata"          
    ## [55] "Elodea nuttallii"            "Fundulus heteroclitus"      
    ## [57] "Gunnera tinctoria"           "Hakea sericea"              
    ## [59] "Herpestes javanicus"         "Ludwigia grandiflora"       
    ## [61] "Ludwigia peploides"          "Myriophyllum aquaticum"     
    ## [63] "Faxonius virilis"            "Oxyura jamaicensis"         
    ## [65] "Rugulopteryx okamurae"       "Triadica sebifera"          
    ## [67] "Alternanthera philoxeroides" "Andropogon virginicus"      
    ## [69] "Cardiospermum grandiflorum"  "Ehrharta calycina"          
    ## [71] "Faxonius rusticus"           "Gambusia holbrooki"         
    ## [73] "Gymnocoronis spilanthoides"  "Humulus scandens"           
    ## [75] "Hydrocotyle ranunculoides"   "Lagarosiphon major"         
    ## [77] "Lespedeza cuneata"           "Limnoperna fortunei"        
    ## [79] "Lygodium japonicum"          "Microstegium vimineum"      
    ## [81] "Morone americana"            "Myriophyllum heterophyllum" 
    ## [83] "Cenchrus setaceus"           "Persicaria perfoliata"      
    ## [85] "Neltuma juliflora"           "Salvinia molesta"           
    ## [87] "Solenopsis geminata"         "Solenopsis richteri"

``` r
unique(standardized_df$Internet_platform)
```

    ## [1] "Wikipedia (geo)"      "Wikipedia (lan)"      "Google health"       
    ## [4] "Flickr"               "GBIF"                 "iNaturalist (casual)"
    ## [7] "Youtube"              "Facebook"

``` r
# Display the unique names after standardization to verify
cat("Original unique names number:")
```

    ## Original unique names number:

``` r
length((unique(combined_data_padded$SCIENTIFIC_NAME)))
```

    ## [1] 94

``` r
cat("Standardized unique names number:")
```

    ## Standardized unique names number:

``` r
length(unique(standardized_df$SCIENTIFIC_NAME))
```

    ## [1] 88

``` r
# View the first few rows of the updated dataframe
length(standardized_df$SCIENTIFIC_NAME)
```

    ## [1] 29401873

``` r
glimpse(standardized_df)
```

    ## Rows: 29,401,873
    ## Columns: 5
    ## $ SCIENTIFIC_NAME   <chr> "Acridotheres tristis", "Acridotheres tristis", "Acr…
    ## $ COUNTRY           <chr> "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE"…
    ## $ date              <date> 2017-02-09, 2017-02-10, 2017-02-11, 2017-02-12, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ Internet_platform <chr> "Wikipedia (geo)", "Wikipedia (geo)", "Wikipedia (ge…

``` r
data.table::fwrite(standardized_df, "allplatforms_daily_activity_combined.csv", row.names = FALSE)
```

# 4. Bin ‘views’ (= activity) per species x country x platform per month to capture signals of increased frequency and save as new file

``` r
# Make sure your data frame is a data.table first
setDT(standardized_df)

# Group by columns and calculate the sum in one step
grouped_monthly_views_dt <- standardized_df[,
  .(total_views = sum(views, na.rm = TRUE)), # The '.(...)'' is a list, and na.rm = TRUE handles NAs
  by = .(
    Internet_platform,
    SCIENTIFIC_NAME,
    COUNTRY,
    month_year = format(date, "%Y-%m")
  )
]

# Display a preview of the grouped data
glimpse(grouped_monthly_views_dt)
```

    ## Rows: 2,808,832
    ## Columns: 5
    ## $ Internet_platform <chr> "Wikipedia (geo)", "Wikipedia (geo)", "Wikipedia (ge…
    ## $ SCIENTIFIC_NAME   <chr> "Acridotheres tristis", "Acridotheres tristis", "Acr…
    ## $ COUNTRY           <chr> "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE"…
    ## $ month_year        <chr> "2017-02", "2017-03", "2017-04", "2017-05", "2017-06…
    ## $ total_views       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

``` r
unique(grouped_monthly_views_dt$SCIENTIFIC_NAME)
```

    ##  [1] "Acridotheres tristis"        "Ailanthus altissima"        
    ##  [3] "Alopochen aegyptiaca"        "Ameiurus melas"             
    ##  [5] "Asclepias syriaca"           "Axis axis"                  
    ##  [7] "Celastrus orbiculatus"       "Channa argus"               
    ##  [9] "Corvus splendens"            "Pontederia crassipes"       
    ## [11] "Eriocheir sinensis"          "Gambusia affinis"           
    ## [13] "Heracleum mantegazzianum"    "Heracleum persicum"         
    ## [15] "Heracleum sosnowskyi"        "Impatiens glandulifera"     
    ## [17] "Koenigia polystachya"        "Lampropeltis getula"        
    ## [19] "Lepomis gibbosus"            "Lithobates catesbeianus"    
    ## [21] "Lysichiton americanus"       "Muntiacus reevesi"          
    ## [23] "Myocastor coypus"            "Nasua nasua"                
    ## [25] "Nyctereutes procyonoides"    "Ondatra zibethicus"         
    ## [27] "Faxonius limosus"            "Pacifastacus leniusculus"   
    ## [29] "Parthenium hysterophorus"    "Perccottus glenii"          
    ## [31] "Pistia stratiotes"           "Plotosus lineatus"          
    ## [33] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [35] "Procyon lotor"               "Pseudorasbora parva"        
    ## [37] "Pueraria montana"            "Pycnonotus cafer"           
    ## [39] "Sciurus carolinensis"        "Sciurus niger"              
    ## [41] "Solenopsis invicta"          "Tamias sibiricus"           
    ## [43] "Threskiornis aethiopicus"    "Trachemys scripta"          
    ## [45] "Vespa velutina"              "Wasmannia auropunctata"     
    ## [47] "Xenopus laevis"              "Acacia saligna"             
    ## [49] "Arthurdendyus triangulatus"  "Baccharis halimifolia"      
    ## [51] "Cabomba caroliniana"         "Callosciurus erythraeus"    
    ## [53] "Callosciurus finlaysonii"    "Cortaderia jubata"          
    ## [55] "Elodea nuttallii"            "Fundulus heteroclitus"      
    ## [57] "Gunnera tinctoria"           "Hakea sericea"              
    ## [59] "Herpestes javanicus"         "Ludwigia grandiflora"       
    ## [61] "Ludwigia peploides"          "Myriophyllum aquaticum"     
    ## [63] "Faxonius virilis"            "Oxyura jamaicensis"         
    ## [65] "Rugulopteryx okamurae"       "Triadica sebifera"          
    ## [67] "Alternanthera philoxeroides" "Andropogon virginicus"      
    ## [69] "Cardiospermum grandiflorum"  "Ehrharta calycina"          
    ## [71] "Faxonius rusticus"           "Gambusia holbrooki"         
    ## [73] "Gymnocoronis spilanthoides"  "Humulus scandens"           
    ## [75] "Hydrocotyle ranunculoides"   "Lagarosiphon major"         
    ## [77] "Lespedeza cuneata"           "Limnoperna fortunei"        
    ## [79] "Lygodium japonicum"          "Microstegium vimineum"      
    ## [81] "Morone americana"            "Myriophyllum heterophyllum" 
    ## [83] "Cenchrus setaceus"           "Persicaria perfoliata"      
    ## [85] "Neltuma juliflora"           "Salvinia molesta"           
    ## [87] "Solenopsis geminata"         "Solenopsis richteri"

``` r
# Display unique platforms
unique(grouped_monthly_views_dt$Internet_platform)
```

    ## [1] "Wikipedia (geo)"      "Wikipedia (lan)"      "Google health"       
    ## [4] "Flickr"               "GBIF"                 "iNaturalist (casual)"
    ## [7] "Youtube"              "Facebook"

``` r
# Save as new file
data.table::fwrite(grouped_monthly_views_dt, "allplatforms_monthly_activity_combined.csv", row.names = FALSE)
```

# 5. Join monthly long format data with the intro_year EASIN data and save as combined file

``` r
# Change inner_join to left_join to keep all rows from grouped_monthly_views
# even if they don't have a matching SCIENTIFIC_NAME/COUNTRY in intro_year.
# The 'YEAR' column will be NA for non-matching rows.
filtered_and_joined_views <- grouped_monthly_views_dt %>%
  inner_join( 
    intro_year %>% dplyr::select(SCIENTIFIC_NAME, COUNTRY, YEAR, Group, Habitat),
    by = c("SCIENTIFIC_NAME", "COUNTRY")
  )

# Show the data
glimpse((filtered_and_joined_views))
```

    ## Rows: 103,430
    ## Columns: 8
    ## $ Internet_platform <chr> "Wikipedia (geo)", "Wikipedia (geo)", "Wikipedia (ge…
    ## $ SCIENTIFIC_NAME   <chr> "Heracleum mantegazzianum", "Heracleum mantegazzianu…
    ## $ COUNTRY           <chr> "BG", "BG", "BG", "BG", "BG", "BG", "BG", "BG", "BG"…
    ## $ month_year        <chr> "2017-02", "2017-03", "2017-04", "2017-05", "2017-06…
    ## $ total_views       <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ YEAR              <int> 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017…
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…

``` r
# Now all platforms should appear, but their YEAR column might be NA
print("Unique platforms after left_join:")
```

    ## [1] "Unique platforms after left_join:"

``` r
print(unique(filtered_and_joined_views$Internet_platform))
```

    ## [1] "Wikipedia (geo)"      "Wikipedia (lan)"      "Google health"       
    ## [4] "Flickr"               "GBIF"                 "iNaturalist (casual)"
    ## [7] "Youtube"              "Facebook"

``` r
data.table::fwrite(filtered_and_joined_views, "allplatforms_views_introyears.csv", row.names = FALSE)
```
