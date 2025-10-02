Data processing and statistical analysis of iEcology-IAS-miner data in
R - Part 2
================
Simon Reynaert
25/08/2025

- [Loading the required packages](#loading-the-required-packages)
- [6. Perform the anomalous activity detection surrounding the EASIN
  reported invasion year for all country x species x platform
  combinations and make
  plots](#6-perform-the-anomalous-activity-detection-surrounding-the-easin-reported-invasion-year-for-all-country-x-species-x-platform-combinations-and-make-plots)
  - [6.1. Prepare the data to feed to anomaly detection
    loop](#61-prepare-the-data-to-feed-to-anomaly-detection-loop)
  - [6.2. Detect the anomalies + changepoints and combine with raw
    data](#62-detect-the-anomalies--changepoints-and-combine-with-raw-data)
  - [6.3. Generate the anomaly plots for visual
    inspection](#63-generate-the-anomaly-plots-for-visual-inspection)
  - [6.4. Calculate lead or lag between earliest possible invasion date
    (i.e., january 1st of the invasion year) and anomalous increase in
    platform activity (if
    any)](#64-calculate-lead-or-lag-between-earliest-possible-invasion-date-ie-january-1st-of-the-invasion-year-and-anomalous-increase-in-platform-activity-if-any)
  - [6.5. Generate mean comparison plots between countries, species,
    platforms](#65-generate-mean-comparison-plots-between-countries-species-platforms)
- [7. Lumping normalized data together to check if it improves
  predictability](#7-lumping-normalized-data-together-to-check-if-it-improves-predictability)
  - [7.1. Normalize all activities by dividing each observation by its
    maximum](#71-normalize-all-activities-by-dividing-each-observation-by-its-maximum)
  - [7.2.Generate the lump plots with gam fit for all observations
    except
    GBIF](#72generate-the-lump-plots-with-gam-fit-for-all-observations-except-gbif)
- [8. Verify correlations between GBIF and
  platforms](#8-verify-correlations-between-gbif-and-platforms)
  - [8.1. Clean up monthly data to allow 1-by-1
    correlations](#81-clean-up-monthly-data-to-allow-1-by-1-correlations)
  - [8.2. Calculate Spearman correlations with GBIF
    data](#82-calculate-spearman-correlations-with-gbif-data)
  - [8.3. Join with species characteristics data and calculate
    statistics](#83-join-with-species-characteristics-data-and-calculate-statistics)
  - [8.4. Make the Spearman rank correlation
    plots](#84-make-the-spearman-rank-correlation-plots)
- [9. Calculate the popularity per species x country x platform
  combinations and
  compare](#9-calculate-the-popularity-per-species-x-country-x-platform-combinations-and-compare)
- [10. Calculate changepoints on lump data for false positives /
  negatives
  detection](#10-calculate-changepoints-on-lump-data-for-false-positives--negatives-detection)
  - [10.1 Final selected calculation
    method](#101-final-selected-calculation-method)
  - [10.2 Changepoint classification and sensitivity
    analysis](#102-changepoint-classification-and-sensitivity-analysis)
  - [10.3. Check for the best thresholds minimizing false positive
    rate](#103-check-for-the-best-thresholds-minimizing-false-positive-rate)
  - [10.4 Example plot Sciurus
    carolinensis](#104-example-plot-sciurus-carolinensis)
  - [10.5 Alternative detection calculation methods (NOT
    SHOWN)](#105-alternative-detection-calculation-methods-not-shown)

This R markdown document goes over the processing and statistical
analysis that was performed in R of the mined iEcology data for the
*iEcology-IAS-miner* Github repository. This .rmd file covers part 2 of
the analysis which includes statistical analysis and generation of
output figures.

# Loading the required packages

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
library(mgcv)
library(zoo)
```

``` r
#check working directory
getwd()
```

    ## [1] "C:/Users/simon/Documents"

# 6. Perform the anomalous activity detection surrounding the EASIN reported invasion year for all country x species x platform combinations and make plots

## 6.1. Prepare the data to feed to anomaly detection loop

``` r
# Read the CSV file into a data.table object
filtered_and_joined_views <- data.table::fread("allplatforms_views_introyears.csv")
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
# Count the number of NA values in the 'views' column
na_count_views <- sum(is.na(filtered_and_joined_views$views))

# Print the result
print(paste("The number of NA values in the 'views' column is:", na_count_views))
```

    ## [1] "The number of NA values in the 'views' column is: 0"

``` r
# `pv_long` will be the primary input for the anomaly detection loop.
# The `YEAR` column in `pv_long` now *is* the introduction year for that row.
pv_long <- filtered_and_joined_views %>%
  mutate(date = ymd(paste0(month_year, "-01")), # Convert 'month_year' to 'date'
         views = as.numeric(total_views)) %>%    # Rename total_views to views for consistency
  # Explicitly select all columns needed, including the YEAR (intro year) from the join
  dplyr::select(Internet_platform, SCIENTIFIC_NAME, COUNTRY, date, views, YEAR, Group, Habitat) %>%
  arrange(Internet_platform, SCIENTIFIC_NAME, COUNTRY, date)


# Print pv_long to confirm its structure
print("Head of pv_long (main input for anomaly detection):")
```

    ## [1] "Head of pv_long (main input for anomaly detection):"

``` r
print(glimpse(pv_long))
```

    ## Rows: 103,430
    ## Columns: 8
    ## $ Internet_platform <chr> "Facebook", "Facebook", "Facebook", "Facebook", "Fac…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK"…
    ## $ date              <date> 2015-12-01, 2016-01-01, 2016-02-01, 2016-03-01, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ YEAR              <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023…
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…
    ##            Internet_platform SCIENTIFIC_NAME COUNTRY       date views  YEAR
    ##                       <char>          <char>  <char>     <Date> <num> <int>
    ##      1:             Facebook  Acacia saligna      DK 2015-12-01     0  2023
    ##      2:             Facebook  Acacia saligna      DK 2016-01-01     0  2023
    ##      3:             Facebook  Acacia saligna      DK 2016-02-01     0  2023
    ##      4:             Facebook  Acacia saligna      DK 2016-03-01     0  2023
    ##      5:             Facebook  Acacia saligna      DK 2016-04-01     0  2023
    ##     ---                                                                    
    ## 103426: iNaturalist (casual)  Xenopus laevis      NL 2025-03-01     0  2022
    ## 103427: iNaturalist (casual)  Xenopus laevis      NL 2025-04-01     0  2022
    ## 103428: iNaturalist (casual)  Xenopus laevis      NL 2025-05-01     0  2022
    ## 103429: iNaturalist (casual)  Xenopus laevis      NL 2025-06-01     0  2022
    ## 103430: iNaturalist (casual)  Xenopus laevis      NL 2025-07-01     0  2022
    ##            Group     Habitat
    ##           <char>      <char>
    ##      1:  Plantae Terrestrial
    ##      2:  Plantae Terrestrial
    ##      3:  Plantae Terrestrial
    ##      4:  Plantae Terrestrial
    ##      5:  Plantae Terrestrial
    ##     ---                     
    ## 103426: Amphibia     Aquatic
    ## 103427: Amphibia     Aquatic
    ## 103428: Amphibia     Aquatic
    ## 103429: Amphibia     Aquatic
    ## 103430: Amphibia     Aquatic

``` r
# This step also incorporates the checks for minimum data points and unique dates
# as these are prerequisites for time series decomposition.
pv_long <- pv_long %>%
  group_by(Internet_platform, SCIENTIFIC_NAME, COUNTRY) %>%
  filter({
    # Get non-NA views and unique dates for the current group
    non_na_views <- views[!is.na(views)]
    unique_dates <- unique(date)
    
    # Condition to KEEP the group:
    # 1. Must have at least 3 non-NA data points (common minimum for time series)
    has_enough_data_points <- length(non_na_views) >= 3
    
    # 2. Must have variability (not all NA and not all constant values)
    has_variability <- !(length(non_na_views) == 0 || min(non_na_views) == max(non_na_views))
    
    # 3. Must have at least 3 unique dates (critical for time series decomposition)
    has_enough_unique_dates <- length(unique_dates) >= 3
    
    # Combine all conditions to KEEP the group for analysis
    has_enough_data_points && has_variability && has_enough_unique_dates
  }) %>%
  ungroup() # Ungroup after filtering to avoid unexpected behavior in subsequent steps

glimpse((pv_long))
```

    ## Rows: 52,326
    ## Columns: 8
    ## $ Internet_platform <chr> "Facebook", "Facebook", "Facebook", "Facebook", "Fac…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK"…
    ## $ date              <date> 2015-12-01, 2016-01-01, 2016-02-01, 2016-03-01, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ YEAR              <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023…
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…

``` r
unique(pv_long$Internet_platform)
```

    ## [1] "Facebook"             "Flickr"               "GBIF"                
    ## [4] "Google health"        "Wikipedia (geo)"      "Wikipedia (lan)"     
    ## [7] "Youtube"              "iNaturalist (casual)"

``` r
# Count the number of NA values in the 'views' column
na_count_views <- sum(is.na(pv_long$views))

# Print the result
print(paste("The number of NA values in the 'views' column is:", na_count_views))
```

    ## [1] "The number of NA values in the 'views' column is: 0"

## 6.2. Detect the anomalies + changepoints and combine with raw data

``` r
#ACTUAL CHANGEPOINT + ANOMALY DETECTION SCRIPT

# Initialize final data frames to store results
anomaly_flags_full <- tibble()
changepoint_flags_full <- tibble()

# Get unique platform-species-country combos
combos <- pv_long %>% distinct(Internet_platform, SCIENTIFIC_NAME, COUNTRY)
total_combos <- nrow(combos)

print(paste0("Starting analysis for ", total_combos, " unique combinations."))
```

    ## [1] "Starting analysis for 453 unique combinations."

``` r
problematic_combos <- list()
problematic_cpt_combos <- list()

for (i in seq_len(total_combos)) {
  platform <- combos$Internet_platform[i]
  sci_name <- combos$SCIENTIFIC_NAME[i]
  country  <- combos$COUNTRY[i]
  
  cat(paste0("\nProcessing combination ", i, " of ", total_combos, ": Platform='", platform, "', Species='", sci_name, "', Country='", country, "'\n"))
  
  sub_df <- pv_long %>%
    filter(Internet_platform == platform,
           SCIENTIFIC_NAME == sci_name,
           COUNTRY == country) %>%
    arrange(date)

  # The validation checks remain important
  if (nrow(sub_df) < 5 || length(unique(sub_df$date)) < 5) {
    cat(paste0("  Skipping: Not enough data points (", nrow(sub_df), ") or unique dates for analysis.\n"))
    next
  }
  
  #date_diffs <- as.numeric(diff(sub_df$date)) / 30.44
  #if (any(date_diffs > 3)) {
  #  cat("  Skipping: The time series contains a gap larger than 3 months, which may cause errors.\n")
  #  next
  #}
  
  non_zero_views <- sub_df$views[sub_df$views > 0]
  if (length(non_zero_views) == 0) {
    cat("  Skipping: 'views' column is all zero.\n")
    next
  }
  
  # Anomaly Detection with anomalize
  tryCatch({
    results_anomalies <- sub_df %>%
      as_tbl_time(index = date) %>%
      group_by(Internet_platform, SCIENTIFIC_NAME, COUNTRY) %>%
      time_decompose(views, method = "twitter", frequency = "auto", trend = "auto") %>%
      anomalize(remainder, method = "gesd", alpha = 0.25) %>%
      time_recompose() %>%
      ungroup()
    
    flagged <- results_anomalies %>%
      filter(anomaly == "Yes", observed > recomposed_l1) %>%
      select(Internet_platform, SCIENTIFIC_NAME, COUNTRY, date) %>%
      distinct() %>%
      mutate(is_anomaly = "Yes")
    
    # Append results to the final data frame inside the loop
    if (nrow(flagged) > 0) {
      anomaly_flags_full <- bind_rows(anomaly_flags_full, flagged)
      cat(paste0("  Anomaly detected for this combination (", nrow(flagged), " anomalies flagged).\n"))
      
      clean_platform <- gsub("[[:punct:]]", "_", platform)
      clean_sci_name <- gsub("[[:punct:]]", "_", sci_name)
      clean_country <- gsub("[[:punct:]]", "_", country)
      filename_anomaly <- paste0("anomaly_plot_", clean_platform, "_", clean_sci_name, "_", clean_country, ".png")
      
      if (!dir.exists("confidence_anomaly_plots")) {
        dir.create("confidence_anomaly_plots")
      }
      
      p_anomaly <- results_anomalies %>% plot_anomalies(time_recomposed = TRUE, ncol = 3) +
        labs(
          title = paste("Anomalies for", sci_name, "on", platform, "in", country),
          subtitle = "Anomalies are flagged in red"
        )
      
      ggsave(file.path("confidence_anomaly_plots", filename_anomaly), plot = p_anomaly, width = 12, height = 8, units = "in")
      cat(paste0("  Saved anomaly plot to '", file.path("confidence_anomaly_plots", filename_anomaly), "'.\n"))
    } else {
      cat("  No anomalies detected for this combination.\n")
    }
  }, error = function(e) {
    problem_info <- list(
      platform = platform,
      sci_name = sci_name,
      country = country,
      error_message = e$message
    )
    problematic_combos[[length(problematic_combos) + 1]] <<- problem_info
    cat(paste0("  ERROR: Anomaly detection crashed. Error message: ", e$message, "\n"))
  })
  
  # Changepoint Detection with changepoint.np
  tryCatch({
    views_vector <- sub_df$views
    
    cpt_result <- cpt.np(
      views_vector,
      penalty = "MBIC",
      method = "PELT",
      test.stat = "empirical_distribution",
      minseglen = 12
    )
    
    cpts <- cpts(cpt_result)
    
    if (length(cpts) > 0) {
      changepoint_dates <- sub_df$date[cpts]
      cpt_df <- tibble(
        Internet_platform = platform,
        SCIENTIFIC_NAME = sci_name,
        COUNTRY = country,
        changepoint_date = changepoint_dates,
        is_changepoint = "Yes"
      ) %>%
        distinct()
      
      # Append changepoint results to the final data frame inside the loop
      changepoint_flags_full <- bind_rows(changepoint_flags_full, cpt_df)
      cat(paste0("  Changepoint(s) detected at date(s): ", paste(changepoint_dates, collapse = ", "), "\n"))
      
      clean_platform <- gsub("[[:punct:]]", "_", platform)
      clean_sci_name <- gsub("[[:punct:]]", "_", sci_name)
      clean_country <- gsub("[[:punct:]]", "_", country)
      filename_cpt <- paste0("changepoint_plot_", clean_platform, "_", clean_sci_name, "_", clean_country, ".png")
      
      if (!dir.exists("changepoint_plots")) {
        dir.create("changepoint_plots")
      }
      
      png(file.path("changepoint_plots", filename_cpt), width = 12, height = 8, units = "in", res = 300)
      plot(cpt_result, main = paste("Changepoints for", sci_name, "on", platform, "in", country), xlab = "Time Index", ylab = "Views", cpt.col = "red", lty = 1, type = "l")
      dev.off()
      
      cat(paste0("  Saved changepoint plot to '", file.path("changepoint_plots", filename_cpt), "'.\n"))
    } else {
      cat("  No changepoints detected for this combination.\n")
    }
  }, error = function(e) {
    problem_info <- list(
      platform = platform,
      sci_name = sci_name,
      country = country,
      error_message = e$message
    )
    problematic_cpt_combos[[length(problematic_cpt_combos) + 1]] <<- problem_info
    cat(paste0("  ERROR: Changepoint detection crashed. Error message: ", e$message, "\n"))
  })
}
```

    ## 
    ## Processing combination 1 of 453: Platform='Facebook', Species='Acacia saligna', Country='DK'

    ## Registered S3 method overwritten by 'quantmod':
    ##   method            from
    ##   as.zoo.data.frame zoo

    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Acacia saligna_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 2 of 453: Platform='Facebook', Species='Acridotheres tristis', Country='CY'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Acridotheres tristis_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 3 of 453: Platform='Facebook', Species='Acridotheres tristis', Country='GR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Acridotheres tristis_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 4 of 453: Platform='Facebook', Species='Alopochen aegyptiaca', Country='LT'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Alopochen aegyptiaca_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 5 of 453: Platform='Facebook', Species='Ameiurus melas', Country='BG'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Ameiurus melas_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 6 of 453: Platform='Facebook', Species='Asclepias syriaca', Country='EE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Asclepias syriaca_EE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 7 of 453: Platform='Facebook', Species='Asclepias syriaca', Country='ES'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Asclepias syriaca_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 8 of 453: Platform='Facebook', Species='Asclepias syriaca', Country='FI'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Asclepias syriaca_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 9 of 453: Platform='Facebook', Species='Asclepias syriaca', Country='LV'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Asclepias syriaca_LV.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 10 of 453: Platform='Facebook', Species='Celastrus orbiculatus', Country='SK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Celastrus orbiculatus_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 11 of 453: Platform='Facebook', Species='Cortaderia jubata', Country='ES'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Cortaderia jubata_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 12 of 453: Platform='Facebook', Species='Elodea nuttallii', Country='ES'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Elodea nuttallii_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 13 of 453: Platform='Facebook', Species='Faxonius rusticus', Country='FR'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Faxonius rusticus_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 14 of 453: Platform='Facebook', Species='Gymnocoronis spilanthoides', Country='FR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Gymnocoronis spilanthoides_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 15 of 453: Platform='Facebook', Species='Gymnocoronis spilanthoides', Country='NL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Gymnocoronis spilanthoides_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 16 of 453: Platform='Facebook', Species='Heracleum mantegazzianum', Country='BG'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum mantegazzianum_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 17 of 453: Platform='Facebook', Species='Heracleum mantegazzianum', Country='GR'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum mantegazzianum_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 18 of 453: Platform='Facebook', Species='Heracleum mantegazzianum', Country='LT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum mantegazzianum_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 19 of 453: Platform='Facebook', Species='Heracleum mantegazzianum', Country='RO'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum mantegazzianum_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 20 of 453: Platform='Facebook', Species='Heracleum sosnowskyi', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum sosnowskyi_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 21 of 453: Platform='Facebook', Species='Heracleum sosnowskyi', Country='BG'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum sosnowskyi_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 22 of 453: Platform='Facebook', Species='Heracleum sosnowskyi', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum sosnowskyi_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 23 of 453: Platform='Facebook', Species='Heracleum sosnowskyi', Country='NL'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum sosnowskyi_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 24 of 453: Platform='Facebook', Species='Heracleum sosnowskyi', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Heracleum sosnowskyi_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 25 of 453: Platform='Facebook', Species='Hydrocotyle ranunculoides', Country='PT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Hydrocotyle ranunculoides_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 26 of 453: Platform='Facebook', Species='Impatiens glandulifera', Country='GR'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Impatiens glandulifera_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 27 of 453: Platform='Facebook', Species='Lampropeltis getula', Country='IT'
    ##   Anomaly detected for this combination (17 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Lampropeltis getula_IT.png'.
    ##   Changepoint(s) detected at date(s): 2019-02-01, 2022-09-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Lampropeltis getula_IT.png'.
    ## 
    ## Processing combination 28 of 453: Platform='Facebook', Species='Ludwigia grandiflora', Country='FI'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Ludwigia grandiflora_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 29 of 453: Platform='Facebook', Species='Ludwigia grandiflora', Country='PT'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Ludwigia grandiflora_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 30 of 453: Platform='Facebook', Species='Ludwigia peploides', Country='HR'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Ludwigia peploides_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 31 of 453: Platform='Facebook', Species='Ludwigia peploides', Country='RO'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Ludwigia peploides_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 32 of 453: Platform='Facebook', Species='Lysichiton americanus', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Lysichiton americanus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 33 of 453: Platform='Facebook', Species='Lysichiton americanus', Country='PL'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Lysichiton americanus_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 34 of 453: Platform='Facebook', Species='Myocastor coypus', Country='HU'
    ##   Anomaly detected for this combination (15 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Myocastor coypus_HU.png'.
    ##   Changepoint(s) detected at date(s): 2020-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Myocastor coypus_HU.png'.
    ## 
    ## Processing combination 35 of 453: Platform='Facebook', Species='Myocastor coypus', Country='LT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Myocastor coypus_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 36 of 453: Platform='Facebook', Species='Myocastor coypus', Country='LV'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Myocastor coypus_LV.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 37 of 453: Platform='Facebook', Species='Myriophyllum heterophyllum', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Myriophyllum heterophyllum_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 38 of 453: Platform='Facebook', Species='Nasua nasua', Country='BE'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Nasua nasua_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 39 of 453: Platform='Facebook', Species='Nasua nasua', Country='DE'
    ##   Anomaly detected for this combination (18 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Nasua nasua_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 40 of 453: Platform='Facebook', Species='Nasua nasua', Country='DK'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Nasua nasua_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 41 of 453: Platform='Facebook', Species='Nasua nasua', Country='HU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Nasua nasua_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 42 of 453: Platform='Facebook', Species='Neltuma juliflora', Country='BE'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Neltuma juliflora_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 43 of 453: Platform='Facebook', Species='Neltuma juliflora', Country='HU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Neltuma juliflora_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 44 of 453: Platform='Facebook', Species='Nyctereutes procyonoides', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Nyctereutes procyonoides_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 45 of 453: Platform='Facebook', Species='Pacifastacus leniusculus', Country='MT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Pacifastacus leniusculus_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 46 of 453: Platform='Facebook', Species='Parthenium hysterophorus', Country='FR'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Parthenium hysterophorus_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 47 of 453: Platform='Facebook', Species='Parthenium hysterophorus', Country='RO'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Parthenium hysterophorus_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 48 of 453: Platform='Facebook', Species='Perccottus glenii', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Perccottus glenii_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 49 of 453: Platform='Facebook', Species='Persicaria perfoliata', Country='NL'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Persicaria perfoliata_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 50 of 453: Platform='Facebook', Species='Pistia stratiotes', Country='FI'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Pistia stratiotes_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 51 of 453: Platform='Facebook', Species='Pistia stratiotes', Country='HR'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Pistia stratiotes_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 52 of 453: Platform='Facebook', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (15 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Pistia stratiotes_PL.png'.
    ##   Changepoint(s) detected at date(s): 2020-06-01, 2021-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Pistia stratiotes_PL.png'.
    ## 
    ## Processing combination 53 of 453: Platform='Facebook', Species='Plotosus lineatus', Country='CY'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Plotosus lineatus_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 54 of 453: Platform='Facebook', Species='Pontederia crassipes', Country='PL'
    ##   Anomaly detected for this combination (12 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Pontederia crassipes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 55 of 453: Platform='Facebook', Species='Procambarus clarkii', Country='GR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus clarkii_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 56 of 453: Platform='Facebook', Species='Procambarus clarkii', Country='MT'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus clarkii_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 57 of 453: Platform='Facebook', Species='Procambarus clarkii', Country='PL'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus clarkii_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 58 of 453: Platform='Facebook', Species='Procambarus virginalis', Country='AT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus virginalis_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 59 of 453: Platform='Facebook', Species='Procambarus virginalis', Country='BE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus virginalis_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 60 of 453: Platform='Facebook', Species='Procambarus virginalis', Country='ES'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus virginalis_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 61 of 453: Platform='Facebook', Species='Procambarus virginalis', Country='FR'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus virginalis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 62 of 453: Platform='Facebook', Species='Procambarus virginalis', Country='MT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus virginalis_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 63 of 453: Platform='Facebook', Species='Procambarus virginalis', Country='RO'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Procambarus virginalis_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 64 of 453: Platform='Facebook', Species='Rugulopteryx okamurae', Country='IT'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Rugulopteryx okamurae_IT.png'.
    ##   Changepoint(s) detected at date(s): 2020-09-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Rugulopteryx okamurae_IT.png'.
    ## 
    ## Processing combination 65 of 453: Platform='Facebook', Species='Salvinia molesta', Country='GB'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Salvinia molesta_GB.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 66 of 453: Platform='Facebook', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Sciurus carolinensis_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 67 of 453: Platform='Facebook', Species='Sciurus carolinensis', Country='FR'
    ##   Anomaly detected for this combination (17 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Sciurus carolinensis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 68 of 453: Platform='Facebook', Species='Solenopsis invicta', Country='IT'
    ##   Anomaly detected for this combination (16 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Solenopsis invicta_IT.png'.
    ##   Changepoint(s) detected at date(s): 2023-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Solenopsis invicta_IT.png'.
    ## 
    ## Processing combination 69 of 453: Platform='Facebook', Species='Threskiornis aethiopicus', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Threskiornis aethiopicus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 70 of 453: Platform='Facebook', Species='Threskiornis aethiopicus', Country='DE'
    ##   Anomaly detected for this combination (15 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Threskiornis aethiopicus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 71 of 453: Platform='Facebook', Species='Trachemys scripta', Country='CY'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Trachemys scripta_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 72 of 453: Platform='Facebook', Species='Trachemys scripta', Country='SK'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Trachemys scripta_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 73 of 453: Platform='Facebook', Species='Vespa velutina', Country='AT'
    ##   Anomaly detected for this combination (21 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Vespa velutina_AT.png'.
    ##   Changepoint(s) detected at date(s): 2022-12-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Vespa velutina_AT.png'.
    ## 
    ## Processing combination 74 of 453: Platform='Facebook', Species='Vespa velutina', Country='CZ'
    ##   Anomaly detected for this combination (18 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Vespa velutina_CZ.png'.
    ##   Changepoint(s) detected at date(s): 2023-07-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Vespa velutina_CZ.png'.
    ## 
    ## Processing combination 75 of 453: Platform='Facebook', Species='Vespa velutina', Country='GB'
    ##   Anomaly detected for this combination (16 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Vespa velutina_GB.png'.
    ##   Changepoint(s) detected at date(s): 2023-01-01, 2024-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Vespa velutina_GB.png'.
    ## 
    ## Processing combination 76 of 453: Platform='Facebook', Species='Vespa velutina', Country='HU'
    ##   Anomaly detected for this combination (15 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Vespa velutina_HU.png'.
    ##   Changepoint(s) detected at date(s): 2023-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Vespa velutina_HU.png'.
    ## 
    ## Processing combination 77 of 453: Platform='Facebook', Species='Vespa velutina', Country='IE'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Vespa velutina_IE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 78 of 453: Platform='Facebook', Species='Vespa velutina', Country='LU'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Vespa velutina_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 79 of 453: Platform='Facebook', Species='Vespa velutina', Country='NL'
    ##   Anomaly detected for this combination (16 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Vespa velutina_NL.png'.
    ##   Changepoint(s) detected at date(s): 2023-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Facebook_Vespa velutina_NL.png'.
    ## 
    ## Processing combination 80 of 453: Platform='Facebook', Species='Wasmannia auropunctata', Country='FR'
    ##   Anomaly detected for this combination (13 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Wasmannia auropunctata_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 81 of 453: Platform='Facebook', Species='Xenopus laevis', Country='NL'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Facebook_Xenopus laevis_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 82 of 453: Platform='Flickr', Species='Gunnera tinctoria', Country='DK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Gunnera tinctoria_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 83 of 453: Platform='Flickr', Species='Nasua nasua', Country='BE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Nasua nasua_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 84 of 453: Platform='Flickr', Species='Nasua nasua', Country='DE'
    ##   Anomaly detected for this combination (13 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Nasua nasua_DE.png'.
    ##   Changepoint(s) detected at date(s): 2015-12-01, 2017-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Flickr_Nasua nasua_DE.png'.
    ## 
    ## Processing combination 85 of 453: Platform='Flickr', Species='Nasua nasua', Country='DK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Nasua nasua_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 86 of 453: Platform='Flickr', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Pistia stratiotes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 87 of 453: Platform='Flickr', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Sciurus carolinensis_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 88 of 453: Platform='Flickr', Species='Sciurus carolinensis', Country='FR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Sciurus carolinensis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 89 of 453: Platform='Flickr', Species='Threskiornis aethiopicus', Country='CZ'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Threskiornis aethiopicus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 90 of 453: Platform='Flickr', Species='Threskiornis aethiopicus', Country='DE'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Flickr_Threskiornis aethiopicus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 91 of 453: Platform='GBIF', Species='Acacia saligna', Country='DK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Acacia saligna_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 92 of 453: Platform='GBIF', Species='Acridotheres tristis', Country='CY'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Acridotheres tristis_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 93 of 453: Platform='GBIF', Species='Acridotheres tristis', Country='GR'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Acridotheres tristis_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 94 of 453: Platform='GBIF', Species='Alopochen aegyptiaca', Country='LT'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Alopochen aegyptiaca_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 95 of 453: Platform='GBIF', Species='Ameiurus melas', Country='AT'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Ameiurus melas_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 96 of 453: Platform='GBIF', Species='Ameiurus melas', Country='BG'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Ameiurus melas_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 97 of 453: Platform='GBIF', Species='Asclepias syriaca', Country='EE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Asclepias syriaca_EE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 98 of 453: Platform='GBIF', Species='Asclepias syriaca', Country='LV'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Asclepias syriaca_LV.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 99 of 453: Platform='GBIF', Species='Celastrus orbiculatus', Country='SK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Celastrus orbiculatus_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 100 of 453: Platform='GBIF', Species='Cenchrus setaceus', Country='BE'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Cenchrus setaceus_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 101 of 453: Platform='GBIF', Species='Cenchrus setaceus', Country='CY'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Cenchrus setaceus_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 102 of 453: Platform='GBIF', Species='Cenchrus setaceus', Country='DE'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Cenchrus setaceus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 103 of 453: Platform='GBIF', Species='Cenchrus setaceus', Country='LU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Cenchrus setaceus_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 104 of 453: Platform='GBIF', Species='Cortaderia jubata', Country='ES'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Cortaderia jubata_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 105 of 453: Platform='GBIF', Species='Elodea nuttallii', Country='ES'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Elodea nuttallii_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 106 of 453: Platform='GBIF', Species='Elodea nuttallii', Country='LT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Elodea nuttallii_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 107 of 453: Platform='GBIF', Species='Faxonius rusticus', Country='FR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Faxonius rusticus_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 108 of 453: Platform='GBIF', Species='Gunnera tinctoria', Country='DK'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Gunnera tinctoria_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 109 of 453: Platform='GBIF', Species='Gunnera tinctoria', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Gunnera tinctoria_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 110 of 453: Platform='GBIF', Species='Gymnocoronis spilanthoides', Country='NL'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Gymnocoronis spilanthoides_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 111 of 453: Platform='GBIF', Species='Hakea sericea', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Hakea sericea_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 112 of 453: Platform='GBIF', Species='Heracleum mantegazzianum', Country='GR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Heracleum mantegazzianum_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 113 of 453: Platform='GBIF', Species='Heracleum mantegazzianum', Country='RO'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Heracleum mantegazzianum_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 114 of 453: Platform='GBIF', Species='Heracleum sosnowskyi', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Heracleum sosnowskyi_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 115 of 453: Platform='GBIF', Species='Heracleum sosnowskyi', Country='SE'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Heracleum sosnowskyi_SE.png'.
    ##   Changepoint(s) detected at date(s): 2023-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Heracleum sosnowskyi_SE.png'.
    ## 
    ## Processing combination 116 of 453: Platform='GBIF', Species='Heracleum sosnowskyi', Country='SK'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Heracleum sosnowskyi_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 117 of 453: Platform='GBIF', Species='Hydrocotyle ranunculoides', Country='DK'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Hydrocotyle ranunculoides_DK.png'.
    ##   Changepoint(s) detected at date(s): 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Hydrocotyle ranunculoides_DK.png'.
    ## 
    ## Processing combination 118 of 453: Platform='GBIF', Species='Hydrocotyle ranunculoides', Country='PT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Hydrocotyle ranunculoides_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 119 of 453: Platform='GBIF', Species='Impatiens glandulifera', Country='GR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Impatiens glandulifera_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 120 of 453: Platform='GBIF', Species='Koenigia polystachya', Country='SI'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Koenigia polystachya_SI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 121 of 453: Platform='GBIF', Species='Lespedeza cuneata', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Lespedeza cuneata_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 122 of 453: Platform='GBIF', Species='Ludwigia grandiflora', Country='AT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Ludwigia grandiflora_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 123 of 453: Platform='GBIF', Species='Ludwigia grandiflora', Country='PT'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Ludwigia grandiflora_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 124 of 453: Platform='GBIF', Species='Ludwigia peploides', Country='HR'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Ludwigia peploides_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 125 of 453: Platform='GBIF', Species='Ludwigia peploides', Country='HU'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Ludwigia peploides_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 126 of 453: Platform='GBIF', Species='Ludwigia peploides', Country='RO'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Ludwigia peploides_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 127 of 453: Platform='GBIF', Species='Lygodium japonicum', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Lygodium japonicum_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 128 of 453: Platform='GBIF', Species='Lysichiton americanus', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Lysichiton americanus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 129 of 453: Platform='GBIF', Species='Lysichiton americanus', Country='LU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Lysichiton americanus_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 130 of 453: Platform='GBIF', Species='Lysichiton americanus', Country='PL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Lysichiton americanus_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 131 of 453: Platform='GBIF', Species='Microstegium vimineum', Country='BE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Microstegium vimineum_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 132 of 453: Platform='GBIF', Species='Muntiacus reevesi', Country='AT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Muntiacus reevesi_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 133 of 453: Platform='GBIF', Species='Myocastor coypus', Country='HU'
    ##   Anomaly detected for this combination (15 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Myocastor coypus_HU.png'.
    ##   Changepoint(s) detected at date(s): 2023-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Myocastor coypus_HU.png'.
    ## 
    ## Processing combination 134 of 453: Platform='GBIF', Species='Myocastor coypus', Country='LT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Myocastor coypus_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 135 of 453: Platform='GBIF', Species='Myriophyllum heterophyllum', Country='SE'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Myriophyllum heterophyllum_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 136 of 453: Platform='GBIF', Species='Nasua nasua', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Nasua nasua_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 137 of 453: Platform='GBIF', Species='Nasua nasua', Country='DK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Nasua nasua_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 138 of 453: Platform='GBIF', Species='Nasua nasua', Country='HU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Nasua nasua_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 139 of 453: Platform='GBIF', Species='Neltuma juliflora', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Neltuma juliflora_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 140 of 453: Platform='GBIF', Species='Nyctereutes procyonoides', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Nyctereutes procyonoides_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 141 of 453: Platform='GBIF', Species='Parthenium hysterophorus', Country='RO'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Parthenium hysterophorus_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 142 of 453: Platform='GBIF', Species='Perccottus glenii', Country='CZ'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Perccottus glenii_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 143 of 453: Platform='GBIF', Species='Persicaria perfoliata', Country='NL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Persicaria perfoliata_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 144 of 453: Platform='GBIF', Species='Pistia stratiotes', Country='FI'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Pistia stratiotes_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 145 of 453: Platform='GBIF', Species='Pistia stratiotes', Country='HR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Pistia stratiotes_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 146 of 453: Platform='GBIF', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Pistia stratiotes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 147 of 453: Platform='GBIF', Species='Pontederia crassipes', Country='PL'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Pontederia crassipes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 148 of 453: Platform='GBIF', Species='Procambarus clarkii', Country='MT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Procambarus clarkii_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 149 of 453: Platform='GBIF', Species='Procambarus clarkii', Country='PL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Procambarus clarkii_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 150 of 453: Platform='GBIF', Species='Procambarus virginalis', Country='AT'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Procambarus virginalis_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 151 of 453: Platform='GBIF', Species='Procambarus virginalis', Country='BE'
    ##   Anomaly detected for this combination (21 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Procambarus virginalis_BE.png'.
    ##   Changepoint(s) detected at date(s): 2020-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Procambarus virginalis_BE.png'.
    ## 
    ## Processing combination 152 of 453: Platform='GBIF', Species='Procambarus virginalis', Country='ES'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Procambarus virginalis_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 153 of 453: Platform='GBIF', Species='Procambarus virginalis', Country='FR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Procambarus virginalis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 154 of 453: Platform='GBIF', Species='Rugulopteryx okamurae', Country='IT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Rugulopteryx okamurae_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 155 of 453: Platform='GBIF', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Sciurus carolinensis_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 156 of 453: Platform='GBIF', Species='Sciurus carolinensis', Country='FR'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Sciurus carolinensis_FR.png'.
    ##   Changepoint(s) detected at date(s): 2020-09-01, 2021-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Sciurus carolinensis_FR.png'.
    ## 
    ## Processing combination 157 of 453: Platform='GBIF', Species='Solenopsis invicta', Country='IT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Solenopsis invicta_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 158 of 453: Platform='GBIF', Species='Threskiornis aethiopicus', Country='CZ'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Threskiornis aethiopicus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 159 of 453: Platform='GBIF', Species='Threskiornis aethiopicus', Country='DE'
    ##   Anomaly detected for this combination (15 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Threskiornis aethiopicus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 160 of 453: Platform='GBIF', Species='Trachemys scripta', Country='CY'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Trachemys scripta_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 161 of 453: Platform='GBIF', Species='Trachemys scripta', Country='SK'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Trachemys scripta_SK.png'.
    ##   Changepoint(s) detected at date(s): 2023-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Trachemys scripta_SK.png'.
    ## 
    ## Processing combination 162 of 453: Platform='GBIF', Species='Vespa velutina', Country='AT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Vespa velutina_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 163 of 453: Platform='GBIF', Species='Vespa velutina', Country='GB'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Vespa velutina_GB.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 164 of 453: Platform='GBIF', Species='Vespa velutina', Country='LU'
    ##   Anomaly detected for this combination (22 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Vespa velutina_LU.png'.
    ##   Changepoint(s) detected at date(s): 2023-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Vespa velutina_LU.png'.
    ## 
    ## Processing combination 165 of 453: Platform='GBIF', Species='Vespa velutina', Country='NL'
    ##   Anomaly detected for this combination (23 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Vespa velutina_NL.png'.
    ##   Changepoint(s) detected at date(s): 2020-05-01, 2023-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Vespa velutina_NL.png'.
    ## 
    ## Processing combination 166 of 453: Platform='GBIF', Species='Wasmannia auropunctata', Country='CY'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Wasmannia auropunctata_CY.png'.
    ##   Changepoint(s) detected at date(s): 2022-02-01, 2023-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_GBIF_Wasmannia auropunctata_CY.png'.
    ## 
    ## Processing combination 167 of 453: Platform='GBIF', Species='Wasmannia auropunctata', Country='FR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Wasmannia auropunctata_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 168 of 453: Platform='GBIF', Species='Xenopus laevis', Country='NL'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_GBIF_Xenopus laevis_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 169 of 453: Platform='Google health', Species='Acacia saligna', Country='DK'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Acacia saligna_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 170 of 453: Platform='Google health', Species='Acridotheres tristis', Country='CY'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Acridotheres tristis_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 171 of 453: Platform='Google health', Species='Acridotheres tristis', Country='GR'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Acridotheres tristis_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 172 of 453: Platform='Google health', Species='Ameiurus melas', Country='AT'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ameiurus melas_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 173 of 453: Platform='Google health', Species='Ameiurus melas', Country='BG'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ameiurus melas_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 174 of 453: Platform='Google health', Species='Asclepias syriaca', Country='EE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Asclepias syriaca_EE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 175 of 453: Platform='Google health', Species='Asclepias syriaca', Country='ES'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Asclepias syriaca_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 176 of 453: Platform='Google health', Species='Asclepias syriaca', Country='FI'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Asclepias syriaca_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 177 of 453: Platform='Google health', Species='Asclepias syriaca', Country='LV'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Asclepias syriaca_LV.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 178 of 453: Platform='Google health', Species='Cabomba caroliniana', Country='AT'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Cabomba caroliniana_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 179 of 453: Platform='Google health', Species='Celastrus orbiculatus', Country='SK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Celastrus orbiculatus_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 180 of 453: Platform='Google health', Species='Cenchrus setaceus', Country='BE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Cenchrus setaceus_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 181 of 453: Platform='Google health', Species='Cenchrus setaceus', Country='DE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Cenchrus setaceus_DE.png'.
    ##   Changepoint(s) detected at date(s): 2020-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Cenchrus setaceus_DE.png'.
    ## 
    ## Processing combination 182 of 453: Platform='Google health', Species='Cenchrus setaceus', Country='LU'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Cenchrus setaceus_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 183 of 453: Platform='Google health', Species='Cortaderia jubata', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Cortaderia jubata_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 184 of 453: Platform='Google health', Species='Cortaderia jubata', Country='ES'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Cortaderia jubata_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 185 of 453: Platform='Google health', Species='Elodea nuttallii', Country='ES'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Elodea nuttallii_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 186 of 453: Platform='Google health', Species='Elodea nuttallii', Country='LT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Elodea nuttallii_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 187 of 453: Platform='Google health', Species='Eriocheir sinensis', Country='SK'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Eriocheir sinensis_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 188 of 453: Platform='Google health', Species='Faxonius limosus', Country='EE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Faxonius limosus_EE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 189 of 453: Platform='Google health', Species='Faxonius rusticus', Country='FR'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Faxonius rusticus_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 190 of 453: Platform='Google health', Species='Gunnera tinctoria', Country='DK'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Gunnera tinctoria_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 191 of 453: Platform='Google health', Species='Gunnera tinctoria', Country='LU'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Gunnera tinctoria_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 192 of 453: Platform='Google health', Species='Gymnocoronis spilanthoides', Country='FR'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Gymnocoronis spilanthoides_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 193 of 453: Platform='Google health', Species='Gymnocoronis spilanthoides', Country='NL'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Gymnocoronis spilanthoides_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 194 of 453: Platform='Google health', Species='Hakea sericea', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Hakea sericea_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 195 of 453: Platform='Google health', Species='Heracleum mantegazzianum', Country='BG'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum mantegazzianum_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 196 of 453: Platform='Google health', Species='Heracleum mantegazzianum', Country='GR'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum mantegazzianum_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 197 of 453: Platform='Google health', Species='Heracleum mantegazzianum', Country='LT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum mantegazzianum_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 198 of 453: Platform='Google health', Species='Heracleum sosnowskyi', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum sosnowskyi_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 199 of 453: Platform='Google health', Species='Heracleum sosnowskyi', Country='BG'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum sosnowskyi_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 200 of 453: Platform='Google health', Species='Heracleum sosnowskyi', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum sosnowskyi_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 201 of 453: Platform='Google health', Species='Heracleum sosnowskyi', Country='NL'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum sosnowskyi_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 202 of 453: Platform='Google health', Species='Heracleum sosnowskyi', Country='PT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum sosnowskyi_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 203 of 453: Platform='Google health', Species='Heracleum sosnowskyi', Country='SE'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Heracleum sosnowskyi_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 204 of 453: Platform='Google health', Species='Humulus scandens', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Humulus scandens_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 205 of 453: Platform='Google health', Species='Humulus scandens', Country='HR'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Humulus scandens_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 206 of 453: Platform='Google health', Species='Hydrocotyle ranunculoides', Country='DK'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Hydrocotyle ranunculoides_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 207 of 453: Platform='Google health', Species='Hydrocotyle ranunculoides', Country='PT'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Hydrocotyle ranunculoides_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 208 of 453: Platform='Google health', Species='Impatiens glandulifera', Country='GR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Impatiens glandulifera_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 209 of 453: Platform='Google health', Species='Koenigia polystachya', Country='SI'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Koenigia polystachya_SI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 210 of 453: Platform='Google health', Species='Lampropeltis getula', Country='IT'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Lampropeltis getula_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 211 of 453: Platform='Google health', Species='Lespedeza cuneata', Country='BE'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Lespedeza cuneata_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 212 of 453: Platform='Google health', Species='Ludwigia grandiflora', Country='AT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ludwigia grandiflora_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 213 of 453: Platform='Google health', Species='Ludwigia grandiflora', Country='FI'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ludwigia grandiflora_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 214 of 453: Platform='Google health', Species='Ludwigia grandiflora', Country='PT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ludwigia grandiflora_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 215 of 453: Platform='Google health', Species='Ludwigia peploides', Country='HR'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ludwigia peploides_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 216 of 453: Platform='Google health', Species='Ludwigia peploides', Country='HU'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ludwigia peploides_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 217 of 453: Platform='Google health', Species='Ludwigia peploides', Country='RO'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Ludwigia peploides_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 218 of 453: Platform='Google health', Species='Lygodium japonicum', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Lygodium japonicum_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 219 of 453: Platform='Google health', Species='Lysichiton americanus', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Lysichiton americanus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 220 of 453: Platform='Google health', Species='Lysichiton americanus', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Lysichiton americanus_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 221 of 453: Platform='Google health', Species='Lysichiton americanus', Country='PL'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Lysichiton americanus_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 222 of 453: Platform='Google health', Species='Microstegium vimineum', Country='BE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Microstegium vimineum_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 223 of 453: Platform='Google health', Species='Muntiacus reevesi', Country='AT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Muntiacus reevesi_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 224 of 453: Platform='Google health', Species='Myocastor coypus', Country='HU'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Myocastor coypus_HU.png'.
    ##   Changepoint(s) detected at date(s): 2018-06-01, 2021-12-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Myocastor coypus_HU.png'.
    ## 
    ## Processing combination 225 of 453: Platform='Google health', Species='Myocastor coypus', Country='LT'
    ##   Anomaly detected for this combination (23 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Myocastor coypus_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 226 of 453: Platform='Google health', Species='Myocastor coypus', Country='LV'
    ##   Anomaly detected for this combination (12 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Myocastor coypus_LV.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 227 of 453: Platform='Google health', Species='Myriophyllum heterophyllum', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Myriophyllum heterophyllum_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 228 of 453: Platform='Google health', Species='Nasua nasua', Country='BE'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Nasua nasua_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 229 of 453: Platform='Google health', Species='Nasua nasua', Country='DE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Nasua nasua_DE.png'.
    ##   Changepoint(s) detected at date(s): 2021-11-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Nasua nasua_DE.png'.
    ## 
    ## Processing combination 230 of 453: Platform='Google health', Species='Nasua nasua', Country='DK'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Nasua nasua_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 231 of 453: Platform='Google health', Species='Nasua nasua', Country='HU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Nasua nasua_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 232 of 453: Platform='Google health', Species='Neltuma juliflora', Country='BE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Neltuma juliflora_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 233 of 453: Platform='Google health', Species='Neltuma juliflora', Country='HU'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Neltuma juliflora_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 234 of 453: Platform='Google health', Species='Nyctereutes procyonoides', Country='LU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Nyctereutes procyonoides_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 235 of 453: Platform='Google health', Species='Pacifastacus leniusculus', Country='MT'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Pacifastacus leniusculus_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 236 of 453: Platform='Google health', Species='Parthenium hysterophorus', Country='FR'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Parthenium hysterophorus_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 237 of 453: Platform='Google health', Species='Parthenium hysterophorus', Country='RO'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Parthenium hysterophorus_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 238 of 453: Platform='Google health', Species='Perccottus glenii', Country='CZ'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Perccottus glenii_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 239 of 453: Platform='Google health', Species='Persicaria perfoliata', Country='NL'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Persicaria perfoliata_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 240 of 453: Platform='Google health', Species='Pistia stratiotes', Country='FI'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Pistia stratiotes_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 241 of 453: Platform='Google health', Species='Pistia stratiotes', Country='HR'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Pistia stratiotes_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 242 of 453: Platform='Google health', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (23 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Pistia stratiotes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 243 of 453: Platform='Google health', Species='Plotosus lineatus', Country='CY'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Plotosus lineatus_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 244 of 453: Platform='Google health', Species='Pontederia crassipes', Country='MT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Pontederia crassipes_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 245 of 453: Platform='Google health', Species='Pontederia crassipes', Country='PL'
    ##   Anomaly detected for this combination (23 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Pontederia crassipes_PL.png'.
    ##   Changepoint(s) detected at date(s): 2020-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Pontederia crassipes_PL.png'.
    ## 
    ## Processing combination 246 of 453: Platform='Google health', Species='Procambarus clarkii', Country='GR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus clarkii_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 247 of 453: Platform='Google health', Species='Procambarus clarkii', Country='MT'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus clarkii_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 248 of 453: Platform='Google health', Species='Procambarus clarkii', Country='PL'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus clarkii_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 249 of 453: Platform='Google health', Species='Procambarus virginalis', Country='AT'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus virginalis_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 250 of 453: Platform='Google health', Species='Procambarus virginalis', Country='BE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus virginalis_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 251 of 453: Platform='Google health', Species='Procambarus virginalis', Country='EE'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus virginalis_EE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 252 of 453: Platform='Google health', Species='Procambarus virginalis', Country='ES'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus virginalis_ES.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 253 of 453: Platform='Google health', Species='Procambarus virginalis', Country='FR'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus virginalis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 254 of 453: Platform='Google health', Species='Procambarus virginalis', Country='MT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Procambarus virginalis_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 255 of 453: Platform='Google health', Species='Rugulopteryx okamurae', Country='IT'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Rugulopteryx okamurae_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 256 of 453: Platform='Google health', Species='Salvinia molesta', Country='GB'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Salvinia molesta_GB.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 257 of 453: Platform='Google health', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Sciurus carolinensis_DE.png'.
    ##   Changepoint(s) detected at date(s): 2018-10-01, 2024-06-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Sciurus carolinensis_DE.png'.
    ## 
    ## Processing combination 258 of 453: Platform='Google health', Species='Sciurus carolinensis', Country='FR'
    ##   No anomalies detected for this combination.
    ##   Changepoint(s) detected at date(s): 2019-01-01, 2020-09-01, 2021-12-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Sciurus carolinensis_FR.png'.
    ## 
    ## Processing combination 259 of 453: Platform='Google health', Species='Solenopsis invicta', Country='IT'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Solenopsis invicta_IT.png'.
    ##   Changepoint(s) detected at date(s): 2023-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Solenopsis invicta_IT.png'.
    ## 
    ## Processing combination 260 of 453: Platform='Google health', Species='Threskiornis aethiopicus', Country='DE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Threskiornis aethiopicus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 261 of 453: Platform='Google health', Species='Trachemys scripta', Country='CY'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Trachemys scripta_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 262 of 453: Platform='Google health', Species='Trachemys scripta', Country='SK'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Trachemys scripta_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 263 of 453: Platform='Google health', Species='Triadica sebifera', Country='DE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Triadica sebifera_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 264 of 453: Platform='Google health', Species='Triadica sebifera', Country='PT'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Triadica sebifera_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 265 of 453: Platform='Google health', Species='Wasmannia auropunctata', Country='CY'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Wasmannia auropunctata_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 266 of 453: Platform='Google health', Species='Wasmannia auropunctata', Country='FR'
    ##   Anomaly detected for this combination (14 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Wasmannia auropunctata_FR.png'.
    ##   Changepoint(s) detected at date(s): 2022-09-01, 2023-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Google health_Wasmannia auropunctata_FR.png'.
    ## 
    ## Processing combination 267 of 453: Platform='Google health', Species='Xenopus laevis', Country='NL'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Google health_Xenopus laevis_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 268 of 453: Platform='Wikipedia (geo)', Species='Heracleum mantegazzianum', Country='BG'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Heracleum mantegazzianum_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 269 of 453: Platform='Wikipedia (geo)', Species='Heracleum sosnowskyi', Country='BE'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Heracleum sosnowskyi_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 270 of 453: Platform='Wikipedia (geo)', Species='Heracleum sosnowskyi', Country='CZ'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Heracleum sosnowskyi_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 271 of 453: Platform='Wikipedia (geo)', Species='Heracleum sosnowskyi', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Heracleum sosnowskyi_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 272 of 453: Platform='Wikipedia (geo)', Species='Heracleum sosnowskyi', Country='SK'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Heracleum sosnowskyi_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 273 of 453: Platform='Wikipedia (geo)', Species='Myocastor coypus', Country='HU'
    ##   Anomaly detected for this combination (18 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Myocastor coypus_HU.png'.
    ##   Changepoint(s) detected at date(s): 2023-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _geo__Myocastor coypus_HU.png'.
    ## 
    ## Processing combination 274 of 453: Platform='Wikipedia (geo)', Species='Myocastor coypus', Country='LT'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Myocastor coypus_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 275 of 453: Platform='Wikipedia (geo)', Species='Nyctereutes procyonoides', Country='LU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Nyctereutes procyonoides_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 276 of 453: Platform='Wikipedia (geo)', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Pistia stratiotes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 277 of 453: Platform='Wikipedia (geo)', Species='Pontederia crassipes', Country='PL'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Pontederia crassipes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 278 of 453: Platform='Wikipedia (geo)', Species='Procambarus clarkii', Country='PL'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Procambarus clarkii_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 279 of 453: Platform='Wikipedia (geo)', Species='Procambarus virginalis', Country='AT'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Procambarus virginalis_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 280 of 453: Platform='Wikipedia (geo)', Species='Procambarus virginalis', Country='BE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Procambarus virginalis_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 281 of 453: Platform='Wikipedia (geo)', Species='Procambarus virginalis', Country='FR'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Procambarus virginalis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 282 of 453: Platform='Wikipedia (geo)', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (20 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Sciurus carolinensis_DE.png'.
    ##   Changepoint(s) detected at date(s): 2023-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _geo__Sciurus carolinensis_DE.png'.
    ## 
    ## Processing combination 283 of 453: Platform='Wikipedia (geo)', Species='Sciurus carolinensis', Country='FR'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Sciurus carolinensis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 284 of 453: Platform='Wikipedia (geo)', Species='Solenopsis invicta', Country='IT'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Solenopsis invicta_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 285 of 453: Platform='Wikipedia (geo)', Species='Triadica sebifera', Country='DE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Triadica sebifera_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 286 of 453: Platform='Wikipedia (geo)', Species='Vespa velutina', Country='AT'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Vespa velutina_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 287 of 453: Platform='Wikipedia (geo)', Species='Vespa velutina', Country='CZ'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Vespa velutina_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 288 of 453: Platform='Wikipedia (geo)', Species='Vespa velutina', Country='GB'
    ##   Anomaly detected for this combination (23 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Vespa velutina_GB.png'.
    ##   Changepoint(s) detected at date(s): 2023-03-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _geo__Vespa velutina_GB.png'.
    ## 
    ## Processing combination 289 of 453: Platform='Wikipedia (geo)', Species='Vespa velutina', Country='HU'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Vespa velutina_HU.png'.
    ##   Changepoint(s) detected at date(s): 2023-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _geo__Vespa velutina_HU.png'.
    ## 
    ## Processing combination 290 of 453: Platform='Wikipedia (geo)', Species='Vespa velutina', Country='IE'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Vespa velutina_IE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 291 of 453: Platform='Wikipedia (geo)', Species='Vespa velutina', Country='NL'
    ##   Anomaly detected for this combination (20 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Vespa velutina_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 292 of 453: Platform='Wikipedia (geo)', Species='Wasmannia auropunctata', Country='FR'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _geo__Wasmannia auropunctata_FR.png'.
    ##   Changepoint(s) detected at date(s): 2022-09-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _geo__Wasmannia auropunctata_FR.png'.
    ## 
    ## Processing combination 293 of 453: Platform='Wikipedia (lan)', Species='Alopochen aegyptiaca', Country='LT'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Alopochen aegyptiaca_LT.png'.
    ##   Changepoint(s) detected at date(s): 2024-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Alopochen aegyptiaca_LT.png'.
    ## 
    ## Processing combination 294 of 453: Platform='Wikipedia (lan)', Species='Ameiurus melas', Country='AT'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Ameiurus melas_AT.png'.
    ##   Changepoint(s) detected at date(s): 2021-08-01, 2024-06-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Ameiurus melas_AT.png'.
    ## 
    ## Processing combination 295 of 453: Platform='Wikipedia (lan)', Species='Ameiurus melas', Country='BG'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Ameiurus melas_BG.png'.
    ##   Changepoint(s) detected at date(s): 2023-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Ameiurus melas_BG.png'.
    ## 
    ## Processing combination 296 of 453: Platform='Wikipedia (lan)', Species='Asclepias syriaca', Country='ES'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Asclepias syriaca_ES.png'.
    ##   Changepoint(s) detected at date(s): 2017-11-01, 2019-02-01, 2022-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Asclepias syriaca_ES.png'.
    ## 
    ## Processing combination 297 of 453: Platform='Wikipedia (lan)', Species='Asclepias syriaca', Country='FI'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Asclepias syriaca_FI.png'.
    ##   Changepoint(s) detected at date(s): 2020-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Asclepias syriaca_FI.png'.
    ## 
    ## Processing combination 298 of 453: Platform='Wikipedia (lan)', Species='Cabomba caroliniana', Country='AT'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Cabomba caroliniana_AT.png'.
    ##   Changepoint(s) detected at date(s): 2017-01-01, 2019-01-01, 2021-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Cabomba caroliniana_AT.png'.
    ## 
    ## Processing combination 299 of 453: Platform='Wikipedia (lan)', Species='Cenchrus setaceus', Country='DE'
    ##   Anomaly detected for this combination (17 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Cenchrus setaceus_DE.png'.
    ##   Changepoint(s) detected at date(s): 2020-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Cenchrus setaceus_DE.png'.
    ## 
    ## Processing combination 300 of 453: Platform='Wikipedia (lan)', Species='Cortaderia jubata', Country='ES'
    ##   Anomaly detected for this combination (18 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Cortaderia jubata_ES.png'.
    ##   Changepoint(s) detected at date(s): 2019-09-01, 2020-09-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Cortaderia jubata_ES.png'.
    ## 
    ## Processing combination 301 of 453: Platform='Wikipedia (lan)', Species='Elodea nuttallii', Country='ES'
    ##   Anomaly detected for this combination (14 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Elodea nuttallii_ES.png'.
    ##   Changepoint(s) detected at date(s): 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Elodea nuttallii_ES.png'.
    ## 
    ## Processing combination 302 of 453: Platform='Wikipedia (lan)', Species='Faxonius limosus', Country='EE'
    ##   Anomaly detected for this combination (22 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Faxonius limosus_EE.png'.
    ##   Changepoint(s) detected at date(s): 2018-09-01, 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Faxonius limosus_EE.png'.
    ## 
    ## Processing combination 303 of 453: Platform='Wikipedia (lan)', Species='Gunnera tinctoria', Country='LU'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Gunnera tinctoria_LU.png'.
    ##   Changepoint(s) detected at date(s): 2019-04-01, 2024-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Gunnera tinctoria_LU.png'.
    ## 
    ## Processing combination 304 of 453: Platform='Wikipedia (lan)', Species='Gymnocoronis spilanthoides', Country='FR'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Gymnocoronis spilanthoides_FR.png'.
    ##   Changepoint(s) detected at date(s): 2022-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Gymnocoronis spilanthoides_FR.png'.
    ## 
    ## Processing combination 305 of 453: Platform='Wikipedia (lan)', Species='Gymnocoronis spilanthoides', Country='NL'
    ##   Anomaly detected for this combination (12 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Gymnocoronis spilanthoides_NL.png'.
    ##   Changepoint(s) detected at date(s): 2023-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Gymnocoronis spilanthoides_NL.png'.
    ## 
    ## Processing combination 306 of 453: Platform='Wikipedia (lan)', Species='Heracleum mantegazzianum', Country='LT'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Heracleum mantegazzianum_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 307 of 453: Platform='Wikipedia (lan)', Species='Heracleum sosnowskyi', Country='BE'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Heracleum sosnowskyi_BE.png'.
    ##   Changepoint(s) detected at date(s): 2018-06-01, 2021-04-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Heracleum sosnowskyi_BE.png'.
    ## 
    ## Processing combination 308 of 453: Platform='Wikipedia (lan)', Species='Heracleum sosnowskyi', Country='CZ'
    ##   Anomaly detected for this combination (16 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Heracleum sosnowskyi_CZ.png'.
    ##   Changepoint(s) detected at date(s): 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Heracleum sosnowskyi_CZ.png'.
    ## 
    ## Processing combination 309 of 453: Platform='Wikipedia (lan)', Species='Heracleum sosnowskyi', Country='NL'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Heracleum sosnowskyi_NL.png'.
    ##   Changepoint(s) detected at date(s): 2018-06-01, 2021-04-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Heracleum sosnowskyi_NL.png'.
    ## 
    ## Processing combination 310 of 453: Platform='Wikipedia (lan)', Species='Heracleum sosnowskyi', Country='SE'
    ##   Anomaly detected for this combination (12 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Heracleum sosnowskyi_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 311 of 453: Platform='Wikipedia (lan)', Species='Humulus scandens', Country='HR'
    ##   Anomaly detected for this combination (14 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Humulus scandens_HR.png'.
    ##   Changepoint(s) detected at date(s): 2018-12-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Humulus scandens_HR.png'.
    ## 
    ## Processing combination 312 of 453: Platform='Wikipedia (lan)', Species='Ludwigia grandiflora', Country='AT'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Ludwigia grandiflora_AT.png'.
    ##   Changepoint(s) detected at date(s): 2019-04-01, 2020-12-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Ludwigia grandiflora_AT.png'.
    ## 
    ## Processing combination 313 of 453: Platform='Wikipedia (lan)', Species='Lygodium japonicum', Country='SE'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Lygodium japonicum_SE.png'.
    ##   Changepoint(s) detected at date(s): 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Lygodium japonicum_SE.png'.
    ## 
    ## Processing combination 314 of 453: Platform='Wikipedia (lan)', Species='Lysichiton americanus', Country='LU'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Lysichiton americanus_LU.png'.
    ##   Changepoint(s) detected at date(s): 2024-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Lysichiton americanus_LU.png'.
    ## 
    ## Processing combination 315 of 453: Platform='Wikipedia (lan)', Species='Lysichiton americanus', Country='PL'
    ##   Anomaly detected for this combination (18 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Lysichiton americanus_PL.png'.
    ##   Changepoint(s) detected at date(s): 2019-12-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Lysichiton americanus_PL.png'.
    ## 
    ## Processing combination 316 of 453: Platform='Wikipedia (lan)', Species='Muntiacus reevesi', Country='AT'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Muntiacus reevesi_AT.png'.
    ##   Changepoint(s) detected at date(s): 2017-01-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Muntiacus reevesi_AT.png'.
    ## 
    ## Processing combination 317 of 453: Platform='Wikipedia (lan)', Species='Myocastor coypus', Country='HU'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Myocastor coypus_HU.png'.
    ##   Changepoint(s) detected at date(s): 2017-12-01, 2019-12-01, 2023-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Myocastor coypus_HU.png'.
    ## 
    ## Processing combination 318 of 453: Platform='Wikipedia (lan)', Species='Myocastor coypus', Country='LT'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Myocastor coypus_LT.png'.
    ##   Changepoint(s) detected at date(s): 2019-12-01, 2021-06-01, 2023-01-01, 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Myocastor coypus_LT.png'.
    ## 
    ## Processing combination 319 of 453: Platform='Wikipedia (lan)', Species='Myriophyllum heterophyllum', Country='SE'
    ##   Anomaly detected for this combination (13 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Myriophyllum heterophyllum_SE.png'.
    ##   Changepoint(s) detected at date(s): 2016-12-01, 2017-12-01, 2023-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Myriophyllum heterophyllum_SE.png'.
    ## 
    ## Processing combination 320 of 453: Platform='Wikipedia (lan)', Species='Nasua nasua', Country='BE'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Nasua nasua_BE.png'.
    ##   Changepoint(s) detected at date(s): 2020-07-01, 2021-12-01, 2024-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Nasua nasua_BE.png'.
    ## 
    ## Processing combination 321 of 453: Platform='Wikipedia (lan)', Species='Nasua nasua', Country='DE'
    ##   Anomaly detected for this combination (14 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Nasua nasua_DE.png'.
    ##   Changepoint(s) detected at date(s): 2017-02-01, 2020-06-01, 2022-06-01, 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Nasua nasua_DE.png'.
    ## 
    ## Processing combination 322 of 453: Platform='Wikipedia (lan)', Species='Nasua nasua', Country='DK'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Nasua nasua_DK.png'.
    ##   Changepoint(s) detected at date(s): 2020-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Nasua nasua_DK.png'.
    ## 
    ## Processing combination 323 of 453: Platform='Wikipedia (lan)', Species='Nasua nasua', Country='HU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Nasua nasua_HU.png'.
    ##   Changepoint(s) detected at date(s): 2017-06-01, 2024-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Nasua nasua_HU.png'.
    ## 
    ## Processing combination 324 of 453: Platform='Wikipedia (lan)', Species='Nyctereutes procyonoides', Country='LU'
    ##   Anomaly detected for this combination (14 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Nyctereutes procyonoides_LU.png'.
    ##   Changepoint(s) detected at date(s): 2016-12-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Nyctereutes procyonoides_LU.png'.
    ## 
    ## Processing combination 325 of 453: Platform='Wikipedia (lan)', Species='Parthenium hysterophorus', Country='FR'
    ##   Anomaly detected for this combination (21 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Parthenium hysterophorus_FR.png'.
    ##   Changepoint(s) detected at date(s): 2017-05-01, 2018-07-01, 2021-05-01, 2022-11-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Parthenium hysterophorus_FR.png'.
    ## 
    ## Processing combination 326 of 453: Platform='Wikipedia (lan)', Species='Persicaria perfoliata', Country='NL'
    ##   Anomaly detected for this combination (21 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Persicaria perfoliata_NL.png'.
    ##   Changepoint(s) detected at date(s): 2019-03-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Persicaria perfoliata_NL.png'.
    ## 
    ## Processing combination 327 of 453: Platform='Wikipedia (lan)', Species='Pistia stratiotes', Country='FI'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Pistia stratiotes_FI.png'.
    ##   Changepoint(s) detected at date(s): 2020-05-01, 2022-06-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Pistia stratiotes_FI.png'.
    ## 
    ## Processing combination 328 of 453: Platform='Wikipedia (lan)', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Pistia stratiotes_PL.png'.
    ##   Changepoint(s) detected at date(s): 2021-08-01, 2023-12-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Pistia stratiotes_PL.png'.
    ## 
    ## Processing combination 329 of 453: Platform='Wikipedia (lan)', Species='Pontederia crassipes', Country='PL'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Pontederia crassipes_PL.png'.
    ##   Changepoint(s) detected at date(s): 2017-07-01, 2024-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Pontederia crassipes_PL.png'.
    ## 
    ## Processing combination 330 of 453: Platform='Wikipedia (lan)', Species='Procambarus virginalis', Country='AT'
    ##   Anomaly detected for this combination (20 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Procambarus virginalis_AT.png'.
    ##   Changepoint(s) detected at date(s): 2022-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Procambarus virginalis_AT.png'.
    ## 
    ## Processing combination 331 of 453: Platform='Wikipedia (lan)', Species='Procambarus virginalis', Country='BE'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Procambarus virginalis_BE.png'.
    ##   Changepoint(s) detected at date(s): 2019-10-01, 2024-01-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Procambarus virginalis_BE.png'.
    ## 
    ## Processing combination 332 of 453: Platform='Wikipedia (lan)', Species='Procambarus virginalis', Country='EE'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Procambarus virginalis_EE.png'.
    ##   Changepoint(s) detected at date(s): 2018-01-01, 2021-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Procambarus virginalis_EE.png'.
    ## 
    ## Processing combination 333 of 453: Platform='Wikipedia (lan)', Species='Procambarus virginalis', Country='FR'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Procambarus virginalis_FR.png'.
    ##   Changepoint(s) detected at date(s): 2021-12-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Procambarus virginalis_FR.png'.
    ## 
    ## Processing combination 334 of 453: Platform='Wikipedia (lan)', Species='Salvinia molesta', Country='GB'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Salvinia molesta_GB.png'.
    ##   Changepoint(s) detected at date(s): 2018-09-01, 2019-11-01, 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Salvinia molesta_GB.png'.
    ## 
    ## Processing combination 335 of 453: Platform='Wikipedia (lan)', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Sciurus carolinensis_DE.png'.
    ##   Changepoint(s) detected at date(s): 2022-01-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Sciurus carolinensis_DE.png'.
    ## 
    ## Processing combination 336 of 453: Platform='Wikipedia (lan)', Species='Sciurus carolinensis', Country='FR'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Sciurus carolinensis_FR.png'.
    ##   Changepoint(s) detected at date(s): 2020-03-01, 2022-11-01, 2024-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Sciurus carolinensis_FR.png'.
    ## 
    ## Processing combination 337 of 453: Platform='Wikipedia (lan)', Species='Solenopsis invicta', Country='IT'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Solenopsis invicta_IT.png'.
    ##   Changepoint(s) detected at date(s): 2021-02-01, 2023-06-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Solenopsis invicta_IT.png'.
    ## 
    ## Processing combination 338 of 453: Platform='Wikipedia (lan)', Species='Threskiornis aethiopicus', Country='CZ'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Threskiornis aethiopicus_CZ.png'.
    ##   Changepoint(s) detected at date(s): 2024-06-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Threskiornis aethiopicus_CZ.png'.
    ## 
    ## Processing combination 339 of 453: Platform='Wikipedia (lan)', Species='Threskiornis aethiopicus', Country='DE'
    ##   Anomaly detected for this combination (23 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Threskiornis aethiopicus_DE.png'.
    ##   Changepoint(s) detected at date(s): 2023-06-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Threskiornis aethiopicus_DE.png'.
    ## 
    ## Processing combination 340 of 453: Platform='Wikipedia (lan)', Species='Trachemys scripta', Country='CY'
    ##   Anomaly detected for this combination (16 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Trachemys scripta_CY.png'.
    ##   Changepoint(s) detected at date(s): 2019-06-01, 2022-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Trachemys scripta_CY.png'.
    ## 
    ## Processing combination 341 of 453: Platform='Wikipedia (lan)', Species='Trachemys scripta', Country='SK'
    ##   No anomalies detected for this combination.
    ##   Changepoint(s) detected at date(s): 2018-11-01, 2023-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Trachemys scripta_SK.png'.
    ## 
    ## Processing combination 342 of 453: Platform='Wikipedia (lan)', Species='Triadica sebifera', Country='DE'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Triadica sebifera_DE.png'.
    ##   Changepoint(s) detected at date(s): 2019-05-01, 2024-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Triadica sebifera_DE.png'.
    ## 
    ## Processing combination 343 of 453: Platform='Wikipedia (lan)', Species='Triadica sebifera', Country='PT'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Triadica sebifera_PT.png'.
    ##   Changepoint(s) detected at date(s): 2018-02-01, 2020-11-01, 2024-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Triadica sebifera_PT.png'.
    ## 
    ## Processing combination 344 of 453: Platform='Wikipedia (lan)', Species='Vespa velutina', Country='AT'
    ##   Anomaly detected for this combination (18 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Vespa velutina_AT.png'.
    ##   Changepoint(s) detected at date(s): 2016-12-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Vespa velutina_AT.png'.
    ## 
    ## Processing combination 345 of 453: Platform='Wikipedia (lan)', Species='Vespa velutina', Country='CZ'
    ##   Anomaly detected for this combination (21 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Vespa velutina_CZ.png'.
    ##   Changepoint(s) detected at date(s): 2018-03-01, 2023-01-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Vespa velutina_CZ.png'.
    ## 
    ## Processing combination 346 of 453: Platform='Wikipedia (lan)', Species='Vespa velutina', Country='GB'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Vespa velutina_GB.png'.
    ##   Changepoint(s) detected at date(s): 2017-12-01, 2023-04-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Vespa velutina_GB.png'.
    ## 
    ## Processing combination 347 of 453: Platform='Wikipedia (lan)', Species='Vespa velutina', Country='HU'
    ##   Anomaly detected for this combination (13 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Vespa velutina_HU.png'.
    ##   Changepoint(s) detected at date(s): 2023-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Vespa velutina_HU.png'.
    ## 
    ## Processing combination 348 of 453: Platform='Wikipedia (lan)', Species='Vespa velutina', Country='IE'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Vespa velutina_IE.png'.
    ##   Changepoint(s) detected at date(s): 2017-12-01, 2023-04-01, 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Vespa velutina_IE.png'.
    ## 
    ## Processing combination 349 of 453: Platform='Wikipedia (lan)', Species='Vespa velutina', Country='LU'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Vespa velutina_LU.png'.
    ##   Changepoint(s) detected at date(s): 2018-03-01, 2020-11-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Vespa velutina_LU.png'.
    ## 
    ## Processing combination 350 of 453: Platform='Wikipedia (lan)', Species='Vespa velutina', Country='NL'
    ##   Anomaly detected for this combination (17 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Vespa velutina_NL.png'.
    ##   Changepoint(s) detected at date(s): 2018-03-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Vespa velutina_NL.png'.
    ## 
    ## Processing combination 351 of 453: Platform='Wikipedia (lan)', Species='Wasmannia auropunctata', Country='FR'
    ##   Anomaly detected for this combination (19 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Wasmannia auropunctata_FR.png'.
    ##   Changepoint(s) detected at date(s): 2020-08-01, 2022-09-01, 2023-10-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Wasmannia auropunctata_FR.png'.
    ## 
    ## Processing combination 352 of 453: Platform='Wikipedia (lan)', Species='Xenopus laevis', Country='NL'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Wikipedia _lan__Xenopus laevis_NL.png'.
    ##   Changepoint(s) detected at date(s): 2021-04-01, 2024-05-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Wikipedia _lan__Xenopus laevis_NL.png'.
    ## 
    ## Processing combination 353 of 453: Platform='Youtube', Species='Acridotheres tristis', Country='GR'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Acridotheres tristis_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 354 of 453: Platform='Youtube', Species='Alopochen aegyptiaca', Country='LT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Alopochen aegyptiaca_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 355 of 453: Platform='Youtube', Species='Cabomba caroliniana', Country='AT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Cabomba caroliniana_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 356 of 453: Platform='Youtube', Species='Cenchrus setaceus', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Cenchrus setaceus_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 357 of 453: Platform='Youtube', Species='Cenchrus setaceus', Country='CY'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Cenchrus setaceus_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 358 of 453: Platform='Youtube', Species='Cenchrus setaceus', Country='DE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Cenchrus setaceus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 359 of 453: Platform='Youtube', Species='Cortaderia jubata', Country='BE'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Cortaderia jubata_BE.png'.
    ##   Changepoint(s) detected at date(s): 2024-07-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Youtube_Cortaderia jubata_BE.png'.
    ## 
    ## Processing combination 360 of 453: Platform='Youtube', Species='Gunnera tinctoria', Country='DK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Gunnera tinctoria_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 361 of 453: Platform='Youtube', Species='Heracleum mantegazzianum', Country='LT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Heracleum mantegazzianum_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 362 of 453: Platform='Youtube', Species='Heracleum sosnowskyi', Country='BE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Heracleum sosnowskyi_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 363 of 453: Platform='Youtube', Species='Heracleum sosnowskyi', Country='CZ'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Heracleum sosnowskyi_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 364 of 453: Platform='Youtube', Species='Heracleum sosnowskyi', Country='NL'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Heracleum sosnowskyi_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 365 of 453: Platform='Youtube', Species='Heracleum sosnowskyi', Country='SE'
    ##   Anomaly detected for this combination (9 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Heracleum sosnowskyi_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 366 of 453: Platform='Youtube', Species='Heracleum sosnowskyi', Country='SK'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Heracleum sosnowskyi_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 367 of 453: Platform='Youtube', Species='Humulus scandens', Country='CZ'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Humulus scandens_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 368 of 453: Platform='Youtube', Species='Hydrocotyle ranunculoides', Country='PT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Hydrocotyle ranunculoides_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 369 of 453: Platform='Youtube', Species='Lampropeltis getula', Country='IT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Lampropeltis getula_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 370 of 453: Platform='Youtube', Species='Ludwigia peploides', Country='HR'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Ludwigia peploides_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 371 of 453: Platform='Youtube', Species='Lysichiton americanus', Country='CZ'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Lysichiton americanus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 372 of 453: Platform='Youtube', Species='Lysichiton americanus', Country='PL'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Lysichiton americanus_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 373 of 453: Platform='Youtube', Species='Muntiacus reevesi', Country='AT'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Muntiacus reevesi_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 374 of 453: Platform='Youtube', Species='Myocastor coypus', Country='HU'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Myocastor coypus_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 375 of 453: Platform='Youtube', Species='Myocastor coypus', Country='LT'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Myocastor coypus_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 376 of 453: Platform='Youtube', Species='Myocastor coypus', Country='LV'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Myocastor coypus_LV.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 377 of 453: Platform='Youtube', Species='Myriophyllum heterophyllum', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Myriophyllum heterophyllum_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 378 of 453: Platform='Youtube', Species='Nasua nasua', Country='BE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Nasua nasua_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 379 of 453: Platform='Youtube', Species='Nasua nasua', Country='DE'
    ##   Anomaly detected for this combination (11 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Nasua nasua_DE.png'.
    ##   Changepoint(s) detected at date(s): 2021-02-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Youtube_Nasua nasua_DE.png'.
    ## 
    ## Processing combination 380 of 453: Platform='Youtube', Species='Nasua nasua', Country='DK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Nasua nasua_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 381 of 453: Platform='Youtube', Species='Nasua nasua', Country='HU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Nasua nasua_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 382 of 453: Platform='Youtube', Species='Nyctereutes procyonoides', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Nyctereutes procyonoides_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 383 of 453: Platform='Youtube', Species='Pistia stratiotes', Country='HR'
    ##   Anomaly detected for this combination (6 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Pistia stratiotes_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 384 of 453: Platform='Youtube', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Pistia stratiotes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 385 of 453: Platform='Youtube', Species='Pontederia crassipes', Country='PL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Pontederia crassipes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 386 of 453: Platform='Youtube', Species='Procambarus clarkii', Country='GR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Procambarus clarkii_GR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 387 of 453: Platform='Youtube', Species='Procambarus clarkii', Country='MT'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Procambarus clarkii_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 388 of 453: Platform='Youtube', Species='Procambarus clarkii', Country='PL'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Procambarus clarkii_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 389 of 453: Platform='Youtube', Species='Procambarus virginalis', Country='AT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Procambarus virginalis_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 390 of 453: Platform='Youtube', Species='Procambarus virginalis', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Procambarus virginalis_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 391 of 453: Platform='Youtube', Species='Procambarus virginalis', Country='EE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Procambarus virginalis_EE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 392 of 453: Platform='Youtube', Species='Procambarus virginalis', Country='RO'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Procambarus virginalis_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 393 of 453: Platform='Youtube', Species='Rugulopteryx okamurae', Country='IT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Rugulopteryx okamurae_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 394 of 453: Platform='Youtube', Species='Salvinia molesta', Country='GB'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Salvinia molesta_GB.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 395 of 453: Platform='Youtube', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (17 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Sciurus carolinensis_DE.png'.
    ##   Changepoint(s) detected at date(s): 2023-04-01, 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_Youtube_Sciurus carolinensis_DE.png'.
    ## 
    ## Processing combination 396 of 453: Platform='Youtube', Species='Sciurus carolinensis', Country='FR'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Sciurus carolinensis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 397 of 453: Platform='Youtube', Species='Solenopsis invicta', Country='IT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Solenopsis invicta_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 398 of 453: Platform='Youtube', Species='Threskiornis aethiopicus', Country='CZ'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Threskiornis aethiopicus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 399 of 453: Platform='Youtube', Species='Threskiornis aethiopicus', Country='DE'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Threskiornis aethiopicus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 400 of 453: Platform='Youtube', Species='Trachemys scripta', Country='CY'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Trachemys scripta_CY.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 401 of 453: Platform='Youtube', Species='Trachemys scripta', Country='SK'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Trachemys scripta_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 402 of 453: Platform='Youtube', Species='Vespa velutina', Country='AT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Vespa velutina_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 403 of 453: Platform='Youtube', Species='Vespa velutina', Country='CZ'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Vespa velutina_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 404 of 453: Platform='Youtube', Species='Vespa velutina', Country='GB'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Vespa velutina_GB.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 405 of 453: Platform='Youtube', Species='Vespa velutina', Country='HU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Vespa velutina_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 406 of 453: Platform='Youtube', Species='Vespa velutina', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Vespa velutina_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 407 of 453: Platform='Youtube', Species='Vespa velutina', Country='NL'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Vespa velutina_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 408 of 453: Platform='Youtube', Species='Wasmannia auropunctata', Country='FR'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Wasmannia auropunctata_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 409 of 453: Platform='Youtube', Species='Xenopus laevis', Country='NL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_Youtube_Xenopus laevis_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 410 of 453: Platform='iNaturalist (casual)', Species='Ameiurus melas', Country='AT'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Ameiurus melas_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 411 of 453: Platform='iNaturalist (casual)', Species='Ameiurus melas', Country='BG'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Ameiurus melas_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 412 of 453: Platform='iNaturalist (casual)', Species='Asclepias syriaca', Country='FI'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Asclepias syriaca_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 413 of 453: Platform='iNaturalist (casual)', Species='Cenchrus setaceus', Country='BE'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Cenchrus setaceus_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 414 of 453: Platform='iNaturalist (casual)', Species='Cenchrus setaceus', Country='DE'
    ##   Anomaly detected for this combination (7 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Cenchrus setaceus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 415 of 453: Platform='iNaturalist (casual)', Species='Cenchrus setaceus', Country='LU'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Cenchrus setaceus_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 416 of 453: Platform='iNaturalist (casual)', Species='Gunnera tinctoria', Country='DK'
    ##   Anomaly detected for this combination (17 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Gunnera tinctoria_DK.png'.
    ##   Changepoint(s) detected at date(s): 2020-04-01, 2021-08-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_iNaturalist _casual__Gunnera tinctoria_DK.png'.
    ## 
    ## Processing combination 417 of 453: Platform='iNaturalist (casual)', Species='Heracleum mantegazzianum', Country='BG'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Heracleum mantegazzianum_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 418 of 453: Platform='iNaturalist (casual)', Species='Heracleum mantegazzianum', Country='LT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Heracleum mantegazzianum_LT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 419 of 453: Platform='iNaturalist (casual)', Species='Heracleum mantegazzianum', Country='RO'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Heracleum mantegazzianum_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 420 of 453: Platform='iNaturalist (casual)', Species='Heracleum sosnowskyi', Country='BG'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Heracleum sosnowskyi_BG.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 421 of 453: Platform='iNaturalist (casual)', Species='Heracleum sosnowskyi', Country='CZ'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Heracleum sosnowskyi_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 422 of 453: Platform='iNaturalist (casual)', Species='Heracleum sosnowskyi', Country='NL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Heracleum sosnowskyi_NL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 423 of 453: Platform='iNaturalist (casual)', Species='Heracleum sosnowskyi', Country='SE'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Heracleum sosnowskyi_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 424 of 453: Platform='iNaturalist (casual)', Species='Hydrocotyle ranunculoides', Country='DK'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Hydrocotyle ranunculoides_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 425 of 453: Platform='iNaturalist (casual)', Species='Ludwigia grandiflora', Country='PT'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Ludwigia grandiflora_PT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 426 of 453: Platform='iNaturalist (casual)', Species='Ludwigia peploides', Country='HR'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Ludwigia peploides_HR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 427 of 453: Platform='iNaturalist (casual)', Species='Ludwigia peploides', Country='HU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Ludwigia peploides_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 428 of 453: Platform='iNaturalist (casual)', Species='Lysichiton americanus', Country='CZ'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Lysichiton americanus_CZ.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 429 of 453: Platform='iNaturalist (casual)', Species='Lysichiton americanus', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Lysichiton americanus_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 430 of 453: Platform='iNaturalist (casual)', Species='Lysichiton americanus', Country='PL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Lysichiton americanus_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 431 of 453: Platform='iNaturalist (casual)', Species='Muntiacus reevesi', Country='AT'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Muntiacus reevesi_AT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 432 of 453: Platform='iNaturalist (casual)', Species='Myocastor coypus', Country='HU'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Myocastor coypus_HU.png'.
    ##   Changepoint(s) detected at date(s): 2024-04-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_iNaturalist _casual__Myocastor coypus_HU.png'.
    ## 
    ## Processing combination 433 of 453: Platform='iNaturalist (casual)', Species='Myriophyllum heterophyllum', Country='SE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Myriophyllum heterophyllum_SE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 434 of 453: Platform='iNaturalist (casual)', Species='Nasua nasua', Country='BE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Nasua nasua_BE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 435 of 453: Platform='iNaturalist (casual)', Species='Nasua nasua', Country='DE'
    ##   Anomaly detected for this combination (10 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Nasua nasua_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 436 of 453: Platform='iNaturalist (casual)', Species='Nasua nasua', Country='DK'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Nasua nasua_DK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 437 of 453: Platform='iNaturalist (casual)', Species='Nasua nasua', Country='HU'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Nasua nasua_HU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 438 of 453: Platform='iNaturalist (casual)', Species='Nyctereutes procyonoides', Country='LU'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Nyctereutes procyonoides_LU.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 439 of 453: Platform='iNaturalist (casual)', Species='Pistia stratiotes', Country='FI'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Pistia stratiotes_FI.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 440 of 453: Platform='iNaturalist (casual)', Species='Pistia stratiotes', Country='PL'
    ##   Anomaly detected for this combination (8 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Pistia stratiotes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 441 of 453: Platform='iNaturalist (casual)', Species='Pontederia crassipes', Country='PL'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Pontederia crassipes_PL.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 442 of 453: Platform='iNaturalist (casual)', Species='Procambarus clarkii', Country='MT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Procambarus clarkii_MT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 443 of 453: Platform='iNaturalist (casual)', Species='Procambarus virginalis', Country='AT'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Procambarus virginalis_AT.png'.
    ##   Changepoint(s) detected at date(s): 2024-06-01

    ##   Saved changepoint plot to 'changepoint_plots/changepoint_plot_iNaturalist _casual__Procambarus virginalis_AT.png'.
    ## 
    ## Processing combination 444 of 453: Platform='iNaturalist (casual)', Species='Procambarus virginalis', Country='RO'
    ##   Anomaly detected for this combination (5 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Procambarus virginalis_RO.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 445 of 453: Platform='iNaturalist (casual)', Species='Salvinia molesta', Country='GB'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Salvinia molesta_GB.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 446 of 453: Platform='iNaturalist (casual)', Species='Sciurus carolinensis', Country='DE'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Sciurus carolinensis_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 447 of 453: Platform='iNaturalist (casual)', Species='Sciurus carolinensis', Country='FR'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Sciurus carolinensis_FR.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 448 of 453: Platform='iNaturalist (casual)', Species='Solenopsis invicta', Country='IT'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Solenopsis invicta_IT.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 449 of 453: Platform='iNaturalist (casual)', Species='Threskiornis aethiopicus', Country='DE'
    ##   Anomaly detected for this combination (3 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Threskiornis aethiopicus_DE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 450 of 453: Platform='iNaturalist (casual)', Species='Trachemys scripta', Country='SK'
    ##   Anomaly detected for this combination (2 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Trachemys scripta_SK.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 451 of 453: Platform='iNaturalist (casual)', Species='Vespa velutina', Country='GB'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Vespa velutina_GB.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 452 of 453: Platform='iNaturalist (casual)', Species='Vespa velutina', Country='IE'
    ##   Anomaly detected for this combination (1 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Vespa velutina_IE.png'.
    ##   No changepoints detected for this combination.
    ## 
    ## Processing combination 453 of 453: Platform='iNaturalist (casual)', Species='Vespa velutina', Country='LU'
    ##   Anomaly detected for this combination (4 anomalies flagged).

    ##   Saved anomaly plot to 'confidence_anomaly_plots/anomaly_plot_iNaturalist _casual__Vespa velutina_LU.png'.
    ##   No changepoints detected for this combination.

``` r
# Add a print statement to indicate the end of the loop
print("\nAnalysis loop completed.")
```

    ## [1] "\nAnalysis loop completed."

``` r
# Final join outside the loop
pv_long_with_flags <- pv_long %>%
  left_join(anomaly_flags_full, by = c("Internet_platform", "SCIENTIFIC_NAME", "COUNTRY", "date")) %>%
  mutate(is_anomaly = replace_na(is_anomaly, "No")) %>%
  left_join(changepoint_flags_full, by = c("Internet_platform", "SCIENTIFIC_NAME", "COUNTRY", c("date" = "changepoint_date"))) %>%
  mutate(is_changepoint = replace_na(is_changepoint, "No"))

# View or save result
print("Original Data with Anomaly and Changepoint Flags (pv_long_with_flags):")
```

    ## [1] "Original Data with Anomaly and Changepoint Flags (pv_long_with_flags):"

``` r
print(glimpse((pv_long_with_flags)))
```

    ## Rows: 52,326
    ## Columns: 10
    ## $ Internet_platform <chr> "Facebook", "Facebook", "Facebook", "Facebook", "Fac…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK"…
    ## $ date              <date> 2015-12-01, 2016-01-01, 2016-02-01, 2016-03-01, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ YEAR              <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023…
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…
    ## $ is_anomaly        <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No"…
    ## $ is_changepoint    <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No"…
    ## # A tibble: 52,326 × 10
    ##    Internet_platform SCIENTIFIC_NAME COUNTRY date       views  YEAR Group  
    ##    <chr>             <chr>           <chr>   <date>     <dbl> <int> <chr>  
    ##  1 Facebook          Acacia saligna  DK      2015-12-01     0  2023 Plantae
    ##  2 Facebook          Acacia saligna  DK      2016-01-01     0  2023 Plantae
    ##  3 Facebook          Acacia saligna  DK      2016-02-01     0  2023 Plantae
    ##  4 Facebook          Acacia saligna  DK      2016-03-01     0  2023 Plantae
    ##  5 Facebook          Acacia saligna  DK      2016-04-01     0  2023 Plantae
    ##  6 Facebook          Acacia saligna  DK      2016-05-01     0  2023 Plantae
    ##  7 Facebook          Acacia saligna  DK      2016-06-01     0  2023 Plantae
    ##  8 Facebook          Acacia saligna  DK      2016-07-01     0  2023 Plantae
    ##  9 Facebook          Acacia saligna  DK      2016-08-01     0  2023 Plantae
    ## 10 Facebook          Acacia saligna  DK      2016-09-01     0  2023 Plantae
    ## # ℹ 52,316 more rows
    ## # ℹ 3 more variables: Habitat <chr>, is_anomaly <chr>, is_changepoint <chr>

``` r
if (length(problematic_cpt_combos) > 0) {
  print("\n--- Summary of Problematic Changepoint Combinations ---")
  for (pc in problematic_cpt_combos) {
    cat(paste0("Platform: ", pc$platform, ", Species: ", pc$sci_name, ", Country: ", pc$country, "\n"))
    cat(paste0("  Error: ", pc$error_message, "\n"))
    cat("---\n")
  }
}
```

## 6.3. Generate the anomaly plots for visual inspection

``` r
intro_year=fread("intros_after_2016_EASIN.csv")

# Ensure output directory exists
out_dir <- "anomaly_plots_views_per_platform" # Renamed directory
dir.create(out_dir, showWarnings = FALSE)

glimpse((pv_long_with_flags))
```

    ## Rows: 52,326
    ## Columns: 10
    ## $ Internet_platform <chr> "Facebook", "Facebook", "Facebook", "Facebook", "Fac…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ COUNTRY           <chr> "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK", "DK"…
    ## $ date              <date> 2015-12-01, 2016-01-01, 2016-02-01, 2016-03-01, 201…
    ## $ views             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ YEAR              <int> 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023, 2023…
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…
    ## $ is_anomaly        <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No"…
    ## $ is_changepoint    <chr> "No", "No", "No", "No", "No", "No", "No", "No", "No"…

``` r
pv_long_with_flags$is_anomaly = factor(pv_long_with_flags$is_anomaly, levels = c("Yes", "No"), ordered = T) #make ordered factor

#Loop per platform and species, facet by country
platform_species_list <- unique(pv_long_with_flags %>% distinct(Internet_platform, SCIENTIFIC_NAME))

for (ps_row in seq_len(nrow(platform_species_list))) {
  current_platform <- platform_species_list$Internet_platform[ps_row]
  current_species <- platform_species_list$SCIENTIFIC_NAME[ps_row]
  
  df_sp <- pv_long_with_flags %>%
    filter(Internet_platform == current_platform,
           SCIENTIFIC_NAME == current_species)
  
  # Skip if no data for this platform-species combination at all
  if(nrow(df_sp) == 0) {
    cat(paste0("  Skipping plot: No data found for '", current_platform, "', '", current_species, "'.\n"))
    next
  }
  
  # --- NEW: Filter out countries with all NA or constant 'views' for plotting ---
  df_sp_filtered_for_plot <- df_sp %>%
    group_by(COUNTRY) %>% # Group by country to check each facet's data
    filter({
      non_na_views <- views[!is.na(views)]
      # Keep the group if it's NOT (all NAs OR has constant non-NA values)
      !(length(non_na_views) == 0 || (length(non_na_views) > 0 && min(non_na_views) == max(non_na_views)))
    }) %>%
    ungroup() # Ungroup after filtering
  
  # If after filtering, no data remains for any country for this platform-species, skip the plot
  if(nrow(df_sp_filtered_for_plot) == 0) {
    cat(paste0("  Skipping plot: All countries for '", current_platform, "', '", current_species, "' have constant or no views (will not plot).\n"))
    next # Skip saving the plot entirely
  }
  
  # Ensure intro years match countries that remain in df_sp_filtered_for_plot
  intro_sp <- intro_year %>%
    filter(SCIENTIFIC_NAME == current_species) %>%
    semi_join(df_sp_filtered_for_plot, by = "COUNTRY")%>% # Use the filtered df_sp for semi_join 
    mutate(
        start_date = ymd(paste0(YEAR - 1, "-01-01")),  # Box starts Jan 1 of previous year
        end_date   = ymd(paste0(YEAR, "-12-31"))       # Box ends Dec 31 of current year
    )
  
  
  p <- ggplot(df_sp_filtered_for_plot, aes(date, views)) + # Use the filtered dataframe for plotting
    geom_point(aes(color = is_anomaly)) +
    geom_rect(
    data = intro_sp,
    aes(
        xmin = start_date,
        xmax = end_date,
        ymin = -Inf,
        ymax = Inf,
        group = YEAR
    ),
    fill = "red",
    alpha = 0.1,
    inherit.aes = FALSE
    )+
    facet_wrap(~ COUNTRY, scales = "free_y", ncol = 2) +
    labs(title = bquote(.(current_platform) ~ "activity by country for " * italic(.(current_species)) * " with invasion window"),
         x = "Date", y = "Observations / pageviews / searches / posts (#)") +
    theme_minimal() +
    scale_color_manual(name = "Anomaly", values = c("red", "steelblue")) +
    theme(panel.background = element_rect(fill = "white", color = NA),
          plot.background = element_rect(fill = "white", color = NA),
          strip.text = element_text(size = 10))
  
  # Compute the actual intro-dates for this species/platform for x-axis expansion
  # Ensure intro_sp is not empty before attempting to get intro_dates
  if (nrow(intro_sp) > 0) {
    intro_dates <- ymd(paste0(intro_sp$YEAR, "-01-01"))
    p <- p + expand_limits(x = intro_dates)
  }
  
  # Save plot with platform and species in filename
  file_name <- paste0(
    str_replace_all(current_platform, "[^a-zA-Z0-9_]", "_"), "_",
    str_replace_all(current_species, "\\s+", "_"), ".png"
  )
  ggsave(file.path(out_dir, file_name),
         plot = p, width = 12, height = 7, bg = "white")
  
  cat(paste0("  Plot saved for '", current_platform, "', '", current_species, "'.\n"))
}
```

    ##   Plot saved for 'Facebook', 'Acacia saligna'.

    ##   Plot saved for 'Facebook', 'Acridotheres tristis'.

    ##   Plot saved for 'Facebook', 'Alopochen aegyptiaca'.

    ##   Plot saved for 'Facebook', 'Ameiurus melas'.

    ##   Plot saved for 'Facebook', 'Asclepias syriaca'.

    ##   Plot saved for 'Facebook', 'Celastrus orbiculatus'.

    ##   Plot saved for 'Facebook', 'Cortaderia jubata'.

    ##   Plot saved for 'Facebook', 'Elodea nuttallii'.

    ##   Plot saved for 'Facebook', 'Faxonius rusticus'.

    ##   Plot saved for 'Facebook', 'Gymnocoronis spilanthoides'.

    ##   Plot saved for 'Facebook', 'Heracleum mantegazzianum'.

    ##   Plot saved for 'Facebook', 'Heracleum sosnowskyi'.

    ##   Plot saved for 'Facebook', 'Hydrocotyle ranunculoides'.

    ##   Plot saved for 'Facebook', 'Impatiens glandulifera'.

    ##   Plot saved for 'Facebook', 'Lampropeltis getula'.

    ##   Plot saved for 'Facebook', 'Ludwigia grandiflora'.

    ##   Plot saved for 'Facebook', 'Ludwigia peploides'.

    ##   Plot saved for 'Facebook', 'Lysichiton americanus'.

    ##   Plot saved for 'Facebook', 'Myocastor coypus'.

    ##   Plot saved for 'Facebook', 'Myriophyllum heterophyllum'.

    ##   Plot saved for 'Facebook', 'Nasua nasua'.

    ##   Plot saved for 'Facebook', 'Neltuma juliflora'.

    ##   Plot saved for 'Facebook', 'Nyctereutes procyonoides'.

    ##   Plot saved for 'Facebook', 'Pacifastacus leniusculus'.

    ##   Plot saved for 'Facebook', 'Parthenium hysterophorus'.

    ##   Plot saved for 'Facebook', 'Perccottus glenii'.

    ##   Plot saved for 'Facebook', 'Persicaria perfoliata'.

    ##   Plot saved for 'Facebook', 'Pistia stratiotes'.

    ##   Plot saved for 'Facebook', 'Plotosus lineatus'.

    ##   Plot saved for 'Facebook', 'Pontederia crassipes'.

    ##   Plot saved for 'Facebook', 'Procambarus clarkii'.

    ##   Plot saved for 'Facebook', 'Procambarus virginalis'.

    ##   Plot saved for 'Facebook', 'Rugulopteryx okamurae'.

    ##   Plot saved for 'Facebook', 'Salvinia molesta'.

    ##   Plot saved for 'Facebook', 'Sciurus carolinensis'.

    ##   Plot saved for 'Facebook', 'Solenopsis invicta'.

    ##   Plot saved for 'Facebook', 'Threskiornis aethiopicus'.

    ##   Plot saved for 'Facebook', 'Trachemys scripta'.

    ##   Plot saved for 'Facebook', 'Vespa velutina'.

    ##   Plot saved for 'Facebook', 'Wasmannia auropunctata'.

    ##   Plot saved for 'Facebook', 'Xenopus laevis'.

    ##   Plot saved for 'Flickr', 'Gunnera tinctoria'.

    ##   Plot saved for 'Flickr', 'Nasua nasua'.

    ##   Plot saved for 'Flickr', 'Pistia stratiotes'.

    ##   Plot saved for 'Flickr', 'Sciurus carolinensis'.

    ##   Plot saved for 'Flickr', 'Threskiornis aethiopicus'.

    ##   Plot saved for 'GBIF', 'Acacia saligna'.

    ##   Plot saved for 'GBIF', 'Acridotheres tristis'.

    ##   Plot saved for 'GBIF', 'Alopochen aegyptiaca'.

    ##   Plot saved for 'GBIF', 'Ameiurus melas'.

    ##   Plot saved for 'GBIF', 'Asclepias syriaca'.

    ##   Plot saved for 'GBIF', 'Celastrus orbiculatus'.

    ##   Plot saved for 'GBIF', 'Cenchrus setaceus'.

    ##   Plot saved for 'GBIF', 'Cortaderia jubata'.

    ##   Plot saved for 'GBIF', 'Elodea nuttallii'.

    ##   Plot saved for 'GBIF', 'Faxonius rusticus'.

    ##   Plot saved for 'GBIF', 'Gunnera tinctoria'.

    ##   Plot saved for 'GBIF', 'Gymnocoronis spilanthoides'.

    ##   Plot saved for 'GBIF', 'Hakea sericea'.

    ##   Plot saved for 'GBIF', 'Heracleum mantegazzianum'.

    ##   Plot saved for 'GBIF', 'Heracleum sosnowskyi'.

    ##   Plot saved for 'GBIF', 'Hydrocotyle ranunculoides'.

    ##   Plot saved for 'GBIF', 'Impatiens glandulifera'.

    ##   Plot saved for 'GBIF', 'Koenigia polystachya'.

    ##   Plot saved for 'GBIF', 'Lespedeza cuneata'.

    ##   Plot saved for 'GBIF', 'Ludwigia grandiflora'.

    ##   Plot saved for 'GBIF', 'Ludwigia peploides'.

    ##   Plot saved for 'GBIF', 'Lygodium japonicum'.

    ##   Plot saved for 'GBIF', 'Lysichiton americanus'.

    ##   Plot saved for 'GBIF', 'Microstegium vimineum'.

    ##   Plot saved for 'GBIF', 'Muntiacus reevesi'.

    ##   Plot saved for 'GBIF', 'Myocastor coypus'.

    ##   Plot saved for 'GBIF', 'Myriophyllum heterophyllum'.

    ##   Plot saved for 'GBIF', 'Nasua nasua'.

    ##   Plot saved for 'GBIF', 'Neltuma juliflora'.

    ##   Plot saved for 'GBIF', 'Nyctereutes procyonoides'.

    ##   Plot saved for 'GBIF', 'Parthenium hysterophorus'.

    ##   Plot saved for 'GBIF', 'Perccottus glenii'.

    ##   Plot saved for 'GBIF', 'Persicaria perfoliata'.

    ##   Plot saved for 'GBIF', 'Pistia stratiotes'.

    ##   Plot saved for 'GBIF', 'Pontederia crassipes'.

    ##   Plot saved for 'GBIF', 'Procambarus clarkii'.

    ##   Plot saved for 'GBIF', 'Procambarus virginalis'.

    ##   Plot saved for 'GBIF', 'Rugulopteryx okamurae'.

    ##   Plot saved for 'GBIF', 'Sciurus carolinensis'.

    ##   Plot saved for 'GBIF', 'Solenopsis invicta'.

    ##   Plot saved for 'GBIF', 'Threskiornis aethiopicus'.

    ##   Plot saved for 'GBIF', 'Trachemys scripta'.

    ##   Plot saved for 'GBIF', 'Vespa velutina'.

    ##   Plot saved for 'GBIF', 'Wasmannia auropunctata'.

    ##   Plot saved for 'GBIF', 'Xenopus laevis'.

    ##   Plot saved for 'Google health', 'Acacia saligna'.

    ##   Plot saved for 'Google health', 'Acridotheres tristis'.

    ##   Plot saved for 'Google health', 'Ameiurus melas'.

    ##   Plot saved for 'Google health', 'Asclepias syriaca'.

    ##   Plot saved for 'Google health', 'Cabomba caroliniana'.

    ##   Plot saved for 'Google health', 'Celastrus orbiculatus'.

    ##   Plot saved for 'Google health', 'Cenchrus setaceus'.

    ##   Plot saved for 'Google health', 'Cortaderia jubata'.

    ##   Plot saved for 'Google health', 'Elodea nuttallii'.

    ##   Plot saved for 'Google health', 'Eriocheir sinensis'.

    ##   Plot saved for 'Google health', 'Faxonius limosus'.

    ##   Plot saved for 'Google health', 'Faxonius rusticus'.

    ##   Plot saved for 'Google health', 'Gunnera tinctoria'.

    ##   Plot saved for 'Google health', 'Gymnocoronis spilanthoides'.

    ##   Plot saved for 'Google health', 'Hakea sericea'.

    ##   Plot saved for 'Google health', 'Heracleum mantegazzianum'.

    ##   Plot saved for 'Google health', 'Heracleum sosnowskyi'.

    ##   Plot saved for 'Google health', 'Humulus scandens'.

    ##   Plot saved for 'Google health', 'Hydrocotyle ranunculoides'.

    ##   Plot saved for 'Google health', 'Impatiens glandulifera'.

    ##   Plot saved for 'Google health', 'Koenigia polystachya'.

    ##   Plot saved for 'Google health', 'Lampropeltis getula'.

    ##   Plot saved for 'Google health', 'Lespedeza cuneata'.

    ##   Plot saved for 'Google health', 'Ludwigia grandiflora'.

    ##   Plot saved for 'Google health', 'Ludwigia peploides'.

    ##   Plot saved for 'Google health', 'Lygodium japonicum'.

    ##   Plot saved for 'Google health', 'Lysichiton americanus'.

    ##   Plot saved for 'Google health', 'Microstegium vimineum'.

    ##   Plot saved for 'Google health', 'Muntiacus reevesi'.

    ##   Plot saved for 'Google health', 'Myocastor coypus'.

    ##   Plot saved for 'Google health', 'Myriophyllum heterophyllum'.

    ##   Plot saved for 'Google health', 'Nasua nasua'.

    ##   Plot saved for 'Google health', 'Neltuma juliflora'.

    ##   Plot saved for 'Google health', 'Nyctereutes procyonoides'.

    ##   Plot saved for 'Google health', 'Pacifastacus leniusculus'.

    ##   Plot saved for 'Google health', 'Parthenium hysterophorus'.

    ##   Plot saved for 'Google health', 'Perccottus glenii'.

    ##   Plot saved for 'Google health', 'Persicaria perfoliata'.

    ##   Plot saved for 'Google health', 'Pistia stratiotes'.

    ##   Plot saved for 'Google health', 'Plotosus lineatus'.

    ##   Plot saved for 'Google health', 'Pontederia crassipes'.

    ##   Plot saved for 'Google health', 'Procambarus clarkii'.

    ##   Plot saved for 'Google health', 'Procambarus virginalis'.

    ##   Plot saved for 'Google health', 'Rugulopteryx okamurae'.

    ##   Plot saved for 'Google health', 'Salvinia molesta'.

    ##   Plot saved for 'Google health', 'Sciurus carolinensis'.

    ##   Plot saved for 'Google health', 'Solenopsis invicta'.

    ##   Plot saved for 'Google health', 'Threskiornis aethiopicus'.

    ##   Plot saved for 'Google health', 'Trachemys scripta'.

    ##   Plot saved for 'Google health', 'Triadica sebifera'.

    ##   Plot saved for 'Google health', 'Wasmannia auropunctata'.

    ##   Plot saved for 'Google health', 'Xenopus laevis'.

    ##   Plot saved for 'Wikipedia (geo)', 'Heracleum mantegazzianum'.

    ##   Plot saved for 'Wikipedia (geo)', 'Heracleum sosnowskyi'.

    ##   Plot saved for 'Wikipedia (geo)', 'Myocastor coypus'.

    ##   Plot saved for 'Wikipedia (geo)', 'Nyctereutes procyonoides'.

    ##   Plot saved for 'Wikipedia (geo)', 'Pistia stratiotes'.

    ##   Plot saved for 'Wikipedia (geo)', 'Pontederia crassipes'.

    ##   Plot saved for 'Wikipedia (geo)', 'Procambarus clarkii'.

    ##   Plot saved for 'Wikipedia (geo)', 'Procambarus virginalis'.

    ##   Plot saved for 'Wikipedia (geo)', 'Sciurus carolinensis'.

    ##   Plot saved for 'Wikipedia (geo)', 'Solenopsis invicta'.

    ##   Plot saved for 'Wikipedia (geo)', 'Triadica sebifera'.

    ##   Plot saved for 'Wikipedia (geo)', 'Vespa velutina'.

    ##   Plot saved for 'Wikipedia (geo)', 'Wasmannia auropunctata'.

    ##   Plot saved for 'Wikipedia (lan)', 'Alopochen aegyptiaca'.

    ##   Plot saved for 'Wikipedia (lan)', 'Ameiurus melas'.

    ##   Plot saved for 'Wikipedia (lan)', 'Asclepias syriaca'.

    ##   Plot saved for 'Wikipedia (lan)', 'Cabomba caroliniana'.

    ##   Plot saved for 'Wikipedia (lan)', 'Cenchrus setaceus'.

    ##   Plot saved for 'Wikipedia (lan)', 'Cortaderia jubata'.

    ##   Plot saved for 'Wikipedia (lan)', 'Elodea nuttallii'.

    ##   Plot saved for 'Wikipedia (lan)', 'Faxonius limosus'.

    ##   Plot saved for 'Wikipedia (lan)', 'Gunnera tinctoria'.

    ##   Plot saved for 'Wikipedia (lan)', 'Gymnocoronis spilanthoides'.

    ##   Plot saved for 'Wikipedia (lan)', 'Heracleum mantegazzianum'.

    ##   Plot saved for 'Wikipedia (lan)', 'Heracleum sosnowskyi'.

    ##   Plot saved for 'Wikipedia (lan)', 'Humulus scandens'.

    ##   Plot saved for 'Wikipedia (lan)', 'Ludwigia grandiflora'.

    ##   Plot saved for 'Wikipedia (lan)', 'Lygodium japonicum'.

    ##   Plot saved for 'Wikipedia (lan)', 'Lysichiton americanus'.

    ##   Plot saved for 'Wikipedia (lan)', 'Muntiacus reevesi'.

    ##   Plot saved for 'Wikipedia (lan)', 'Myocastor coypus'.

    ##   Plot saved for 'Wikipedia (lan)', 'Myriophyllum heterophyllum'.

    ##   Plot saved for 'Wikipedia (lan)', 'Nasua nasua'.

    ##   Plot saved for 'Wikipedia (lan)', 'Nyctereutes procyonoides'.

    ##   Plot saved for 'Wikipedia (lan)', 'Parthenium hysterophorus'.

    ##   Plot saved for 'Wikipedia (lan)', 'Persicaria perfoliata'.

    ##   Plot saved for 'Wikipedia (lan)', 'Pistia stratiotes'.

    ##   Plot saved for 'Wikipedia (lan)', 'Pontederia crassipes'.

    ##   Plot saved for 'Wikipedia (lan)', 'Procambarus virginalis'.

    ##   Plot saved for 'Wikipedia (lan)', 'Salvinia molesta'.

    ##   Plot saved for 'Wikipedia (lan)', 'Sciurus carolinensis'.

    ##   Plot saved for 'Wikipedia (lan)', 'Solenopsis invicta'.

    ##   Plot saved for 'Wikipedia (lan)', 'Threskiornis aethiopicus'.

    ##   Plot saved for 'Wikipedia (lan)', 'Trachemys scripta'.

    ##   Plot saved for 'Wikipedia (lan)', 'Triadica sebifera'.

    ##   Plot saved for 'Wikipedia (lan)', 'Vespa velutina'.

    ##   Plot saved for 'Wikipedia (lan)', 'Wasmannia auropunctata'.

    ##   Plot saved for 'Wikipedia (lan)', 'Xenopus laevis'.

    ##   Plot saved for 'Youtube', 'Acridotheres tristis'.

    ##   Plot saved for 'Youtube', 'Alopochen aegyptiaca'.

    ##   Plot saved for 'Youtube', 'Cabomba caroliniana'.

    ##   Plot saved for 'Youtube', 'Cenchrus setaceus'.

    ##   Plot saved for 'Youtube', 'Cortaderia jubata'.

    ##   Plot saved for 'Youtube', 'Gunnera tinctoria'.

    ##   Plot saved for 'Youtube', 'Heracleum mantegazzianum'.

    ##   Plot saved for 'Youtube', 'Heracleum sosnowskyi'.

    ##   Plot saved for 'Youtube', 'Humulus scandens'.

    ##   Plot saved for 'Youtube', 'Hydrocotyle ranunculoides'.

    ##   Plot saved for 'Youtube', 'Lampropeltis getula'.

    ##   Plot saved for 'Youtube', 'Ludwigia peploides'.

    ##   Plot saved for 'Youtube', 'Lysichiton americanus'.

    ##   Plot saved for 'Youtube', 'Muntiacus reevesi'.

    ##   Plot saved for 'Youtube', 'Myocastor coypus'.

    ##   Plot saved for 'Youtube', 'Myriophyllum heterophyllum'.

    ##   Plot saved for 'Youtube', 'Nasua nasua'.

    ##   Plot saved for 'Youtube', 'Nyctereutes procyonoides'.

    ##   Plot saved for 'Youtube', 'Pistia stratiotes'.

    ##   Plot saved for 'Youtube', 'Pontederia crassipes'.

    ##   Plot saved for 'Youtube', 'Procambarus clarkii'.

    ##   Plot saved for 'Youtube', 'Procambarus virginalis'.

    ##   Plot saved for 'Youtube', 'Rugulopteryx okamurae'.

    ##   Plot saved for 'Youtube', 'Salvinia molesta'.

    ##   Plot saved for 'Youtube', 'Sciurus carolinensis'.

    ##   Plot saved for 'Youtube', 'Solenopsis invicta'.

    ##   Plot saved for 'Youtube', 'Threskiornis aethiopicus'.

    ##   Plot saved for 'Youtube', 'Trachemys scripta'.

    ##   Plot saved for 'Youtube', 'Vespa velutina'.

    ##   Plot saved for 'Youtube', 'Wasmannia auropunctata'.

    ##   Plot saved for 'Youtube', 'Xenopus laevis'.

    ##   Plot saved for 'iNaturalist (casual)', 'Ameiurus melas'.

    ##   Plot saved for 'iNaturalist (casual)', 'Asclepias syriaca'.

    ##   Plot saved for 'iNaturalist (casual)', 'Cenchrus setaceus'.

    ##   Plot saved for 'iNaturalist (casual)', 'Gunnera tinctoria'.

    ##   Plot saved for 'iNaturalist (casual)', 'Heracleum mantegazzianum'.

    ##   Plot saved for 'iNaturalist (casual)', 'Heracleum sosnowskyi'.

    ##   Plot saved for 'iNaturalist (casual)', 'Hydrocotyle ranunculoides'.

    ##   Plot saved for 'iNaturalist (casual)', 'Ludwigia grandiflora'.

    ##   Plot saved for 'iNaturalist (casual)', 'Ludwigia peploides'.

    ##   Plot saved for 'iNaturalist (casual)', 'Lysichiton americanus'.

    ##   Plot saved for 'iNaturalist (casual)', 'Muntiacus reevesi'.

    ##   Plot saved for 'iNaturalist (casual)', 'Myocastor coypus'.

    ##   Plot saved for 'iNaturalist (casual)', 'Myriophyllum heterophyllum'.

    ##   Plot saved for 'iNaturalist (casual)', 'Nasua nasua'.

    ##   Plot saved for 'iNaturalist (casual)', 'Nyctereutes procyonoides'.

    ##   Plot saved for 'iNaturalist (casual)', 'Pistia stratiotes'.

    ##   Plot saved for 'iNaturalist (casual)', 'Pontederia crassipes'.

    ##   Plot saved for 'iNaturalist (casual)', 'Procambarus clarkii'.

    ##   Plot saved for 'iNaturalist (casual)', 'Procambarus virginalis'.

    ##   Plot saved for 'iNaturalist (casual)', 'Salvinia molesta'.

    ##   Plot saved for 'iNaturalist (casual)', 'Sciurus carolinensis'.

    ##   Plot saved for 'iNaturalist (casual)', 'Solenopsis invicta'.

    ##   Plot saved for 'iNaturalist (casual)', 'Threskiornis aethiopicus'.

    ##   Plot saved for 'iNaturalist (casual)', 'Trachemys scripta'.

    ##   Plot saved for 'iNaturalist (casual)', 'Vespa velutina'.

``` r
message(sprintf("Plots saved to '%s'/", out_dir))
```

    ## Plots saved to 'anomaly_plots_views_per_platform'/

## 6.4. Calculate lead or lag between earliest possible invasion date (i.e., january 1st of the invasion year) and anomalous increase in platform activity (if any)

``` r
# Assume your input dataframe is called pv_long_with_flags

# Ensure 'date' is of Date type (already done)
# Add 'internet_platform' column to pv_long_with_flags for consistency in lag_df
pv_long_with_anomaly_for_lag <- pv_long_with_flags %>%
  mutate(internet_platform = Internet_platform) # Use the existing Internet_platform column

# Main calculation
lag_df <- pv_long_with_anomaly_for_lag %>%
  group_by(Internet_platform, SCIENTIFIC_NAME, COUNTRY) %>% # Group by platform
  # Ensure intro_date is correctly derived from the YEAR associated with the data point
  # This relies on pv_long_with_flags having the correct YEAR for each combo.
  mutate(intro_date = as.Date(paste0(YEAR, "-01-01"))) %>%
  # Filter anomalies within a year before and a year after introduction
  filter(date >= (intro_date - days(366)) & date <= (intro_date + days(366))) %>%
  filter(is_anomaly == "Yes") %>%
  # Select the closest anomaly
  slice_min(abs(as.numeric(date - intro_date)), with_ties = FALSE) %>%
  mutate(lag_days = as.numeric(date - intro_date)) %>%
  select(Internet_platform, SCIENTIFIC_NAME, COUNTRY, lag_days) %>% # Select platform
  ungroup()

# Fill in combinations with no anomaly in window as NA
all_combinations <- pv_long_with_anomaly_for_lag %>%
  distinct(Internet_platform, SCIENTIFIC_NAME, COUNTRY) # Include platform

final_lags_views_per_platform <- all_combinations %>%
  left_join(lag_df, by = c("Internet_platform", "SCIENTIFIC_NAME", "COUNTRY"))

# View result
print("Final Lags (final_lags_views_per_platform):")
```

    ## [1] "Final Lags (final_lags_views_per_platform):"

``` r
glimpse(final_lags_views_per_platform)
```

    ## Rows: 453
    ## Columns: 4
    ## $ Internet_platform <chr> "Facebook", "Facebook", "Facebook", "Facebook", "Fac…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acridotheres tristis", "Acridothe…
    ## $ COUNTRY           <chr> "DK", "CY", "GR", "LT", "BG", "EE", "ES", "FI", "LV"…
    ## $ lag_days          <dbl> -245, 0, 243, NA, 244, NA, NA, NA, NA, NA, 151, NA, …

``` r
# Check non-NA values for reporting of percentages
non_na_count <- final_lags_views_per_platform %>%
  filter(Internet_platform != "GBIF") %>%
  summarise(non_na_count = sum(!is.na(lag_days)),
            total_rows = n())

print(non_na_count)
```

    ## # A tibble: 1 × 2
    ##   non_na_count total_rows
    ##          <int>      <int>
    ## 1          175        375

``` r
#Calculate mean lag day_days + sd for all platforms except GBIF

lag_days_stats <- final_lags_views_per_platform %>%
  filter(Internet_platform != "GBIF") %>%
  summarise(
    mean_lag_days = mean(lag_days, na.rm = TRUE),
    sd_lag_days = sd(lag_days, na.rm = TRUE)
  )

lag_days_stats
```

    ## # A tibble: 1 × 2
    ##   mean_lag_days sd_lag_days
    ##           <dbl>       <dbl>
    ## 1          44.9        178.

## 6.5. Generate mean comparison plots between countries, species, platforms

``` r
# Use the 'final_lags_views_per_platform' as the base for plotting.
# Rename it to df_plot to match your template.
df_plot <- final_lags_views_per_platform %>%
  rename(internet_platform = Internet_platform) # Rename for consistency with plotting section
df_plot
```

    ## # A tibble: 453 × 4
    ##    internet_platform SCIENTIFIC_NAME       COUNTRY lag_days
    ##    <chr>             <chr>                 <chr>      <dbl>
    ##  1 Facebook          Acacia saligna        DK          -245
    ##  2 Facebook          Acridotheres tristis  CY             0
    ##  3 Facebook          Acridotheres tristis  GR           243
    ##  4 Facebook          Alopochen aegyptiaca  LT            NA
    ##  5 Facebook          Ameiurus melas        BG           244
    ##  6 Facebook          Asclepias syriaca     EE            NA
    ##  7 Facebook          Asclepias syriaca     ES            NA
    ##  8 Facebook          Asclepias syriaca     FI            NA
    ##  9 Facebook          Asclepias syriaca     LV            NA
    ## 10 Facebook          Celastrus orbiculatus SK            NA
    ## # ℹ 443 more rows

``` r
# Join df_plot with intro_year on SCIENTIFIC_NAME and COUNTRY
df_plot_joined <- df_plot %>%
  left_join(intro_year, by = c("SCIENTIFIC_NAME", "COUNTRY"))

# Ensure the join was successful (optional check, but good for debugging)
if (nrow(df_plot_joined) == 0) {
  stop("df_plot_joined is empty after left_join. Check join keys and data.")
}
print("df_plot_joined after initial join:")
```

    ## [1] "df_plot_joined after initial join:"

``` r
glimpse(df_plot_joined)
```

    ## Rows: 453
    ## Columns: 9
    ## $ internet_platform <chr> "Facebook", "Facebook", "Facebook", "Facebook", "Fac…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acridotheres tristis", "Acridothe…
    ## $ COUNTRY           <chr> "DK", "CY", "GR", "LT", "BG", "EE", "ES", "FI", "LV"…
    ## $ lag_days          <dbl> -245, 0, 243, NA, 244, NA, NA, NA, NA, NA, 151, NA, …
    ## $ YEAR              <int> 2023, 2022, 2017, 2016, 2016, 2019, 2018, 2018, 2016…
    ## $ `WIKI NAME`       <chr> "Acacia saligna", "Acridotheres tristis", "Acridothe…
    ## $ Group             <chr> "Plantae", "Aves", "Aves", "Aves", "Pisces", "Planta…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Both",…
    ## $ EASIN.ID          <chr> "R00053", "R00212", "R00212", "R00644", "R00826", "R…

``` r
df_plot_filtered_by_intro_year <- df_plot_joined %>%
  filter(!is.na(YEAR)) # Keep only rows where the YEAR from intro_year is present

# Check how many rows remain after this filter
cat("Number of rows after filtering for valid intro_year matches:", nrow(df_plot_filtered_by_intro_year), "\n")
```

    ## Number of rows after filtering for valid intro_year matches: 453

``` r
df_plot_joined$internet_platform <- factor(df_plot_joined$internet_platform, levels = c ("GBIF", "Youtube", "Wikipedia (lan)", "Wikipedia (geo)", "iNaturalist (casual)", "Google health", "Flickr","Facebook"), ordered = T)

glimpse(df_plot_joined)
```

    ## Rows: 453
    ## Columns: 9
    ## $ internet_platform <ord> Facebook, Facebook, Facebook, Facebook, Facebook, Fa…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acridotheres tristis", "Acridothe…
    ## $ COUNTRY           <chr> "DK", "CY", "GR", "LT", "BG", "EE", "ES", "FI", "LV"…
    ## $ lag_days          <dbl> -245, 0, 243, NA, 244, NA, NA, NA, NA, NA, 151, NA, …
    ## $ YEAR              <int> 2023, 2022, 2017, 2016, 2016, 2019, 2018, 2018, 2016…
    ## $ `WIKI NAME`       <chr> "Acacia saligna", "Acridotheres tristis", "Acridothe…
    ## $ Group             <chr> "Plantae", "Aves", "Aves", "Aves", "Pisces", "Planta…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Both",…
    ## $ EASIN.ID          <chr> "R00053", "R00212", "R00212", "R00644", "R00826", "R…

``` r
# Calculate all means and standard deviations in a single pipeline
# These calculations will be applied to EACH ROW after grouping
df_plot_final <- df_plot_joined %>%
  filter(internet_platform != "GBIF")%>%
  # Group and mutate for internet_platform specific stats
  group_by(internet_platform) %>%
  mutate(
    platform_m = mean(lag_days, na.rm = T), # Renamed to avoid confusion with countrysp_m
    platform_sd = sd(lag_days, na.rm = T)   # Renamed
  ) %>%
  # Group and mutate for COUNTRY specific stats
  group_by(COUNTRY) %>%
  mutate(
    country_m = mean(lag_days, na.rm = T),
    country_sd = sd(lag_days, na.rm = T)
  ) %>%
  # Group and mutate for SCIENTIFIC_NAME specific stats
  group_by(SCIENTIFIC_NAME) %>%
  mutate(
    species_m = mean(lag_days, na.rm = T),
    species_sd = sd(lag_days, na.rm = T)
  ) %>%
  # Group and mutate for Group specific stats (from intro_year)
  group_by(Group) %>%
  mutate(
    group_m = mean(lag_days, na.rm = T),
    group_sd = sd(lag_days, na.rm = T)
  ) %>%
  # Group and mutate for Habitat specific stats (from intro_year)
  group_by(Habitat) %>%
  mutate(
    habitat_m = mean(lag_days, na.rm = T),
    habitat_sd = sd(lag_days, na.rm = T)
  ) %>%
  ungroup() # Ungroup after all mutations to avoid unintended grouping effects later

df_plot_platform <- df_plot_joined %>%
  # Group and mutate for internet_platform specific stats
  group_by(internet_platform) %>%
  mutate(
    platform_m = mean(lag_days, na.rm = T), # Renamed to avoid confusion with countrysp_m
    platform_sd = sd(lag_days, na.rm = T)   # Renamed
  ) 

print("df_plot_final after all calculations:")
```

    ## [1] "df_plot_final after all calculations:"

``` r
glimpse(df_plot_final)
```

    ## Rows: 375
    ## Columns: 19
    ## $ internet_platform <ord> Facebook, Facebook, Facebook, Facebook, Facebook, Fa…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acridotheres tristis", "Acridothe…
    ## $ COUNTRY           <chr> "DK", "CY", "GR", "LT", "BG", "EE", "ES", "FI", "LV"…
    ## $ lag_days          <dbl> -245, 0, 243, NA, 244, NA, NA, NA, NA, NA, 151, NA, …
    ## $ YEAR              <int> 2023, 2022, 2017, 2016, 2016, 2019, 2018, 2018, 2016…
    ## $ `WIKI NAME`       <chr> "Acacia saligna", "Acridotheres tristis", "Acridothe…
    ## $ Group             <chr> "Plantae", "Aves", "Aves", "Aves", "Pisces", "Planta…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Both",…
    ## $ EASIN.ID          <chr> "R00053", "R00212", "R00212", "R00644", "R00826", "R…
    ## $ platform_m        <dbl> 37.44444, 37.44444, 37.44444, 37.44444, 37.44444, 37…
    ## $ platform_sd       <dbl> 179.907, 179.907, 179.907, 179.907, 179.907, 179.907…
    ## $ country_m         <dbl> -26.85714, 172.16667, -71.16667, -25.83333, 0.00000,…
    ## $ country_sd        <dbl> 193.17300, 161.88319, 190.90879, 217.12339, 256.4644…
    ## $ species_m         <dbl> -153.00000, 60.66667, 60.66667, NaN, 152.33333, 45.5…
    ## $ species_sd        <dbl> 130.10765, 160.82392, 160.82392, NA, 185.36001, 280.…
    ## $ group_m           <dbl> 17.41026, 78.80000, 78.80000, 78.80000, 121.57143, 1…
    ## $ group_sd          <dbl> 180.9805, 138.4439, 138.4439, 138.4439, 140.5144, 18…
    ## $ habitat_m         <dbl> 43.62000, 43.62000, 43.62000, -17.92857, 61.47541, 4…
    ## $ habitat_sd        <dbl> 171.6589, 171.6589, 171.6589, 168.2573, 188.4620, 17…

``` r
# Ensure a dedicated output directory for these new plots
comparison_plots_dir <- "comparison_lags_plots"
dir.create(comparison_plots_dir, showWarnings = FALSE)

#IMPORTANT!!: possibly still need to exclude GBIF from mean comparisons to properly reflect traditional internet platform trends!

# plot p1
p1 <- ggplot(df_plot_platform) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red", size = 1) +
  geom_point(aes(x = platform_m, y = internet_platform), size = 4, shape = "square") +
  geom_errorbarh(aes(xmin = platform_m - platform_sd, xmax = platform_m + platform_sd, y = internet_platform), size = 1, height = 0.5) +
  geom_jitter(aes(x = lag_days, y = internet_platform, color = Group, fill = Group), shape = 21, color = "black", size = 4, width = 0) +
  geom_hline(aes(yintercept=1.5),color="black")+
  ylab("Internet platform") +
  xlab("Time between invasion and increase in platform activity (# days)") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 25, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Species group", fill = "Species group") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text (size = 12, color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
print(p1)
```

    ## Warning: Removed 224 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-1.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanlags_groups.png"), plot = p1, width = 10, height = 10, dpi = 300, bg = "white") # Saved to new directory
```

    ## Warning: Removed 224 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
# plot p2 
p2 <- ggplot(df_plot_final) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red", size = 1) +
  geom_point(aes(x = platform_m, y = internet_platform), size = 4, shape = "square") +
  geom_errorbarh(aes(xmin = platform_m - platform_sd, xmax = platform_m + platform_sd, y = internet_platform), size = 1, height = 0.5) +
  geom_jitter(aes(x = lag_days, y = internet_platform, color = COUNTRY, fill = COUNTRY), color = "black", shape = 21, size = 4, width = 0) +
  ylab("Internet platform") +
  xlab("Time between invasion and increase in platform activity (# days)") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 25, 23)) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(color = "Country", fill = "Country") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text (size = 12, color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"))
print(p2)
```

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-2.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanlags_countries.png"), plot = p2, width = 10, height = 10, dpi = 300, bg = "white") # Saved to new directory
```

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#plot p3

p3 <- ggplot(df_plot_final) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red", size = 1) +
  geom_point(aes(x = country_m, y = COUNTRY), size = 4, shape = "square") +
  geom_errorbarh(aes(xmin = country_m - country_sd, xmax = country_m + country_sd, y = COUNTRY), size = 1, height = 0.5) +
  geom_jitter(aes(x = lag_days, y = COUNTRY, color = internet_platform, fill = internet_platform), size = 4, width = 0, color = "black", shape = 21) +
  ylab("Country") +
  xlab("Time between invasion and increase in platform activity (# days)") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 25, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text (size = 12, color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"))
print(p3)
```

    ## Warning: Removed 5 rows containing missing values or values outside the scale range
    ## (`geom_errorbarh()`).

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-3.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanlags_countries_y.png"), plot = p3, width = 10, height = 15, dpi = 300, bg = "white") # Saved to new directory
```

    ## Warning: Removed 5 rows containing missing values or values outside the scale range
    ## (`geom_errorbarh()`).
    ## Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#plot p4

p4 <- ggplot(df_plot_final) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red", size = 1) +
  geom_point(aes(x = species_m, y = SCIENTIFIC_NAME), size = 4, shape = "square") +
  geom_errorbarh(aes(xmin = species_m - species_sd, xmax = species_m + species_sd, y = SCIENTIFIC_NAME), size = 1, height = 0.5) +
  geom_jitter(aes(x = lag_days, y = SCIENTIFIC_NAME, color = internet_platform, fill = internet_platform), size = 4, width = 0, color = "black", shape = 21) +
  ylab("Species") +
  xlab("Time between invasion and increase in platform activity (# days)") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 25, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text (size = 12, face = "italic", color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"))
print(p4)
```

    ## Warning: Removed 15 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 52 rows containing missing values or values outside the scale range
    ## (`geom_errorbarh()`).

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-4.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanlags_species_y.png"), plot = p4, width = 10, height = 15, dpi = 300, bg = "white") # Saved to new directory
```

    ## Warning: Removed 15 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Warning: Removed 52 rows containing missing values or values outside the scale range
    ## (`geom_errorbarh()`).

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#plot and save p5

p5 <- ggplot(df_plot_final) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red", size = 1) +
  geom_point(aes(x = group_m, y = Group), size = 4, shape = "square") +
  geom_errorbarh(aes(xmin = group_m - group_sd, xmax = group_m + group_sd, y = Group), size = 1, height = 0.5) +
  geom_jitter(aes(x = lag_days, y = Group, color = internet_platform, fill = internet_platform), color = "black", shape = 21, size = 4, width = 0) +
  ylab("Species group") +
  xlab("Time between invasion and increase in platform activity (# days)") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 25, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text (size = 12, color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"))
print(p5)
```

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-5.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanlags_group_y.png"), plot = p5, width = 10, height = 10, dpi = 300, bg = "white") # Saved to new directory
```

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#plot and save p6

p6 <- ggplot(df_plot_final) +
  geom_vline(aes(xintercept = 0), linetype = "dashed", color = "red", size = 1) +
  geom_point(aes(x = habitat_m, y = Habitat), size = 4, shape = "square") +
  geom_errorbarh(aes(xmin = habitat_m - habitat_sd, xmax = habitat_m + habitat_sd, y = Habitat), size = 1, height = 0.5) +
  geom_jitter(aes(x = lag_days, y = Habitat, color = internet_platform, fill = internet_platform), color = "black", shape = 21, size = 4, width = 0) +
  ylab("Habitat") +
  xlab("Time between invasion and increase in platform activity (# days)") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 25, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text (size = 12, color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"))
print(p6)
```

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-6.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanlags_habitat_y.png"), plot = p6, width = 10, height = 6, dpi = 300, bg = "white") # Saved to new directory
```

    ## Warning: Removed 200 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
#Plot 7, proportion of actual detections over total possible combinations with some platform data

df_plot_final_counts=df_plot_platform%>%
  group_by(internet_platform)%>%
  summarise(
  n_perplatform = n(),
totalprop = sum(!is.na(lag_days)) / n()) # 

print("Proportion of non-NA detections in total:")
```

    ## [1] "Proportion of non-NA detections in total:"

``` r
print(sum(!is.na(df_plot_platform$lag_days))/length(df_plot_platform$lag_days))
```

    ## [1] 0.5055188

``` r
print("Total number of potential detections:")
```

    ## [1] "Total number of potential detections:"

``` r
length(df_plot_platform$lag_days)
```

    ## [1] 453

``` r
print(df_plot_final_counts)
```

    ## # A tibble: 8 × 3
    ##   internet_platform    n_perplatform totalprop
    ##   <ord>                        <int>     <dbl>
    ## 1 GBIF                            78     0.692
    ## 2 Youtube                         57     0.333
    ## 3 Wikipedia (lan)                 60     0.517
    ## 4 Wikipedia (geo)                 25     0.6  
    ## 5 iNaturalist (casual)            44     0.227
    ## 6 Google health                   99     0.525
    ## 7 Flickr                           9     0.333
    ## 8 Facebook                        81     0.556

``` r
p7 <- ggplot(df_plot_final_counts) +
  geom_bar(aes(x=internet_platform, y=totalprop*100, color = internet_platform, fill = internet_platform),stat="identity",color="black")+
  geom_text(aes(x = internet_platform, y = totalprop*100,
                label = paste0("N = ", n_perplatform)), # Create the label text
            vjust = -0.5, # Adjust vertical position (slightly above the bar)
            color = "black", # Color of the text label
            size = 4) + # Size of the text label
  geom_vline(aes(xintercept=1.5),color="black")+
  ylab("Percentage of activity increases detected surrounding invasion (%)") +
  xlab("Internet platform") +
  scale_shape_manual(values = c(0, 1, 2, 3, 4, 25, 23)) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text (size = 12, color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"))
print(p7)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-7.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "proportion_activity_changes_per_platform.png"), plot = p7, width = 14, height = 6, dpi = 300, bg = "white") # Saved to new directory

#p8 combining p1 and p7 side-by-side for comparison

p8 <- plot_grid(
  p7, p1,
  labels = c("(a)", "(b)"), # Labels for subplots
  label_size = 16,          # Label size
  nrow = 2,                 # Place plots in one column
  align = "vh"               # Horizontal alignment
)
```

    ## Warning: Removed 224 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
print(p8)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Calculate%20mean%20lag%20and%20lead%20statistics%20ifv%20closest%20anomaly%20and%20plot%20the%20plots-8.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "combined_lags_and_props_final.png"), plot = p8, width = 15, height = 15, dpi = 300, bg = "white")
```

# 7. Lumping normalized data together to check if it improves predictability

## 7.1. Normalize all activities by dividing each observation by its maximum

``` r
normalized_monthly_views_EASIN = filtered_and_joined_views %>%
  group_by(Internet_platform, SCIENTIFIC_NAME, COUNTRY) %>%
  mutate(
    max_views = max(total_views, na.rm = TRUE),
    # If Internet_platform is 'Facebook', use total_views (as it's already normalized).
    # Otherwise, perform the min-max normalization.
    normalized_views = if_else(
      Internet_platform == "Facebook",
      total_views, # Use the original value
      if_else(max_views > 0, total_views/max_views, 0) # Normalization for other platforms
    )
  ) %>%
  dplyr::select(-max_views)
summary(normalized_monthly_views_EASIN)
```

    ##  Internet_platform  SCIENTIFIC_NAME      COUNTRY           month_year       
    ##  Length:103430      Length:103430      Length:103430      Length:103430     
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##   total_views            YEAR         Group             Habitat         
    ##  Min.   :    0.00   Min.   :2016   Length:103430      Length:103430     
    ##  1st Qu.:    0.00   1st Qu.:2017   Class :character   Class :character  
    ##  Median :    0.00   Median :2019   Mode  :character   Mode  :character  
    ##  Mean   :   72.51   Mean   :2019                                        
    ##  3rd Qu.:    0.00   3rd Qu.:2020                                        
    ##  Max.   :94193.00   Max.   :2024                                        
    ##  normalized_views 
    ##  Min.   :0.00000  
    ##  1st Qu.:0.00000  
    ##  Median :0.00000  
    ##  Mean   :0.02598  
    ##  3rd Qu.:0.00000  
    ##  Max.   :1.00000

``` r
# Ensure the month_year column is POSIXct element for clean plotting
normalized_monthly_views_EASIN$month_year <- as.POSIXct(paste0(normalized_monthly_views_EASIN$month_year, "-01"))

#Make internet platform an ordered factor to ensure color scheme

normalized_monthly_views_EASIN$Internet_platform <- factor(normalized_monthly_views_EASIN$Internet_platform, levels = c ("GBIF", "Youtube", "Wikipedia (lan)", "Wikipedia (geo)", "iNaturalist (casual)", "Google health", "Flickr","Facebook"), ordered = T)

str(normalized_monthly_views_EASIN$Internet_platform)
```

    ##  Ord.factor w/ 8 levels "GBIF"<"Youtube"<..: 4 4 4 4 4 4 4 4 4 4 ...

``` r
# Get a list of all unique combinations of country and species
combinations <- normalized_monthly_views_EASIN %>%
  ungroup()%>%
  distinct(COUNTRY, SCIENTIFIC_NAME)

glimpse(combinations)
```

    ## Rows: 112
    ## Columns: 2
    ## $ COUNTRY         <chr> "BG", "CZ", "HU", "LT", "LU", "AT", "BE", "FR", "DE", …
    ## $ SCIENTIFIC_NAME <chr> "Heracleum mantegazzianum", "Heracleum sosnowskyi", "M…

``` r
#manually assign colors

platform_colors <- c(
  "GBIF" = "#A6CEE3",
  "Youtube" = "#1F78B4",
  "Wikipedia (lan)" = "#B2DF8A",
  "Wikipedia (geo)" = "#33A02C",
  "iNaturalist (casual)" = "#FB9A99",
  "Google health" = "#E31A1C",
  "Flickr" = "#FDBF6F",
  "Facebook" = "#FF7F00")

normalized_monthly_views_EASIN <- normalized_monthly_views_EASIN %>%
  group_by(month_year, COUNTRY, SCIENTIFIC_NAME) %>%
  mutate(sum_normalized_views = sum(ifelse(Internet_platform != "GBIF", normalized_views, 0), na.rm = TRUE)) %>%
  filter(month_year >= as.Date("2015-01-01")) %>%
  ungroup()

glimpse(normalized_monthly_views_EASIN)
```

    ## Rows: 103,412
    ## Columns: 10
    ## $ Internet_platform    <ord> Wikipedia (geo), Wikipedia (geo), Wikipedia (geo)…
    ## $ SCIENTIFIC_NAME      <chr> "Heracleum mantegazzianum", "Heracleum mantegazzi…
    ## $ COUNTRY              <chr> "BG", "BG", "BG", "BG", "BG", "BG", "BG", "BG", "…
    ## $ month_year           <dttm> 2017-02-01, 2017-03-01, 2017-04-01, 2017-05-01, …
    ## $ total_views          <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ YEAR                 <int> 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2017, 2…
    ## $ Group                <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plan…
    ## $ Habitat              <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Ter…
    ## $ normalized_views     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ sum_normalized_views <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…

``` r
min(normalized_monthly_views_EASIN$month_year)
```

    ## [1] "2015-02-01 CET"

## 7.2.Generate the lump plots with gam fit for all observations except GBIF

``` r
# Create a directory to save the plots
if (!dir.exists("lump_plots_by_species_country")) {
  dir.create("lump_plots_by_species_country")
}

# Loop through each unique combination
for (i in 1:nrow(combinations)) {
  current_country <- combinations$COUNTRY[i]
  current_species <- combinations$SCIENTIFIC_NAME[i]
  
  message(paste("Processing combination", i, "of", nrow(combinations), ":", current_species, "in", current_country))
  
  plot_data <- normalized_monthly_views_EASIN %>%
    filter(COUNTRY == current_country, SCIENTIFIC_NAME == current_species) %>%
    filter(!is.na(normalized_views))
  
  if (nrow(plot_data) == 0) {
    message(paste("Skipping plot for", current_species, "in", current_country, "- no valid data points after filtering."))
    next
  }
  
  invasion_year <- unique(plot_data$YEAR)[1]
  
  shading_layer <- NULL
  
  if (!is.na(invasion_year) && is.numeric(invasion_year)) {
    invasion_start <- as.POSIXct(paste0(invasion_year - 1, "-01-01"))
    invasion_end <- as.POSIXct(paste0(invasion_year, "-12-31"))
    
    shading_df <- data.frame(
      xmin = invasion_start,
      xmax = invasion_end,
      ymin = -Inf,
      ymax = Inf
    )
    
    shading_layer <- geom_rect(
      data = shading_df,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      fill = "red", alpha = 0.15, inherit.aes = FALSE
    )
  }
  
  p <- ggplot(plot_data, aes(x = month_year, y = normalized_views)) +
    shading_layer +
    geom_smooth(data = plot_data %>% filter(Internet_platform != "GBIF"),
                aes(group = 1),
                method = "gam", formula = y ~ s(x, bs = "cs", fx = TRUE), se = FALSE, color = "darkgrey") +
    geom_point(aes(group = Internet_platform, fill = Internet_platform), color = "black", shape = 21, size = 4) +
    xlim(as.POSIXct(paste0(2015, "-01-01")),as.POSIXct(paste0(2025, "-07-01")))+
    ylim(0,1)+
    #scale_color_brewer(palette = "Paired") +
    #scale_fill_brewer(palette = "Paired") +
    scale_fill_manual(values = platform_colors) +
    labs(
      title = bquote("Normalized activity for " * italic(.(current_species)) * " in " * .(current_country)),
      subtitle = "Trends across different internet platforms and GBIF",
      x = "Date",
      y = "Normalized activity (0-1)",
      fill = "Internet platform",
      color = "Internet platform"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black", face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
  
  filename <- paste0("lump_plots_by_species_country/",
                     gsub(" ", "_", current_species),
                     "_", current_country, ".png")
  
  ggsave(filename, plot = p, width = 10, height = 6, units = "in", dpi = 300, bg = "white")
  
}
```

    ## Processing combination 1 of 112 : Heracleum mantegazzianum in BG

    ## Warning: Removed 20 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 2 of 112 : Heracleum sosnowskyi in CZ

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 3 of 112 : Myocastor coypus in HU

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 4 of 112 : Myocastor coypus in LT

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 5 of 112 : Nyctereutes procyonoides in LU

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 6 of 112 : Procambarus virginalis in AT

    ## Processing combination 7 of 112 : Procambarus virginalis in BE

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 8 of 112 : Procambarus virginalis in FR

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 9 of 112 : Sciurus carolinensis in DE

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 10 of 112 : Solenopsis invicta in IT

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 11 of 112 : Vespa velutina in AT

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 12 of 112 : Vespa velutina in CZ

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 13 of 112 : Vespa velutina in NL

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 14 of 112 : Vespa velutina in GB

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 15 of 112 : Wasmannia auropunctata in FR

    ## Warning: Removed 6 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 16 of 112 : Pontederia crassipes in PL

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 17 of 112 : Heracleum sosnowskyi in BE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 5 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 18 of 112 : Heracleum sosnowskyi in SK

    ## Warning: Removed 13 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 19 of 112 : Heracleum sosnowskyi in SE

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 20 of 112 : Pistia stratiotes in PL

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 21 of 112 : Procambarus clarkii in PL

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 22 of 112 : Sciurus carolinensis in FR

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 23 of 112 : Triadica sebifera in DE

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 24 of 112 : Vespa velutina in HU

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 6 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 25 of 112 : Vespa velutina in IE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 2 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 26 of 112 : Acacia saligna in DK

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 35 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 27 of 112 : Acridotheres tristis in CY

    ## Warning: Removed 26 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 28 of 112 : Acridotheres tristis in GR

    ## Warning: Removed 14 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 29 of 112 : Alopochen aegyptiaca in LT

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 30 of 112 : Ameiurus melas in AT

    ## Processing combination 31 of 112 : Ameiurus melas in BG

    ## Processing combination 32 of 112 : Asclepias syriaca in EE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 30 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 33 of 112 : Asclepias syriaca in ES

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 34 of 112 : Asclepias syriaca in FI

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 35 of 112 : Asclepias syriaca in LV

    ## Warning: Removed 30 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 36 of 112 : Cabomba caroliniana in AT

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 37 of 112 : Celastrus orbiculatus in SK

    ## Warning: Removed 31 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 38 of 112 : Cortaderia jubata in BE

    ## Warning: Removed 31 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 39 of 112 : Cortaderia jubata in ES

    ## Warning: Removed 13 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 40 of 112 : Pontederia crassipes in MT

    ## Warning: Removed 34 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 41 of 112 : Elodea nuttallii in ES

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 29 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 42 of 112 : Elodea nuttallii in LT

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 35 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 43 of 112 : Eriocheir sinensis in SK

    ## Warning: Removed 36 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 44 of 112 : Faxonius limosus in EE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 45 of 112 : Faxonius rusticus in FR

    ## Warning: Removed 15 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 46 of 112 : Gunnera tinctoria in DK

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 47 of 112 : Gunnera tinctoria in LU

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 48 of 112 : Gymnocoronis spilanthoides in FR

    ## Warning: Removed 6 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 49 of 112 : Gymnocoronis spilanthoides in NL

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 50 of 112 : Hakea sericea in BE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 34 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 51 of 112 : Heracleum mantegazzianum in GR

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 24 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 52 of 112 : Heracleum mantegazzianum in LT

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 53 of 112 : Heracleum mantegazzianum in RO

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 54 of 112 : Heracleum sosnowskyi in BG

    ## Warning: Removed 29 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 55 of 112 : Heracleum sosnowskyi in NL

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 56 of 112 : Heracleum sosnowskyi in PT

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 37 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 57 of 112 : Humulus scandens in CZ

    ## Warning: Removed 27 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 58 of 112 : Humulus scandens in HR

    ## Warning: Removed 12 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 59 of 112 : Hydrocotyle ranunculoides in DK

    ## Warning: Removed 33 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 60 of 112 : Hydrocotyle ranunculoides in PT

    ## Warning: Removed 14 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 61 of 112 : Impatiens glandulifera in GR

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 27 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 62 of 112 : Koenigia polystachya in SI

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 25 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 63 of 112 : Lampropeltis getula in IT

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 64 of 112 : Lespedeza cuneata in BE

    ## Warning: Removed 35 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 65 of 112 : Ludwigia grandiflora in AT

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 66 of 112 : Ludwigia grandiflora in FI

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 25 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 67 of 112 : Ludwigia grandiflora in PT

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 14 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 68 of 112 : Ludwigia peploides in HR

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 17 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 69 of 112 : Ludwigia peploides in HU

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 27 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 70 of 112 : Ludwigia peploides in RO

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 32 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 71 of 112 : Lygodium japonicum in SE

    ## Processing combination 72 of 112 : Lysichiton americanus in CZ

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 21 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 73 of 112 : Lysichiton americanus in LU

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 74 of 112 : Lysichiton americanus in PL

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 75 of 112 : Microstegium vimineum in BE

    ## Warning: Removed 31 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 76 of 112 : Muntiacus reevesi in AT

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 77 of 112 : Myocastor coypus in LV

    ## Warning: Removed 12 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 78 of 112 : Myriophyllum heterophyllum in SE

    ## Processing combination 79 of 112 : Nasua nasua in BE

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 80 of 112 : Nasua nasua in DE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 81 of 112 : Nasua nasua in DK

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 82 of 112 : Nasua nasua in HU

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 83 of 112 : Pacifastacus leniusculus in MT

    ## Warning: Removed 31 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 84 of 112 : Parthenium hysterophorus in FR

    ## Processing combination 85 of 112 : Parthenium hysterophorus in RO

    ## Warning: Removed 15 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 86 of 112 : Cenchrus setaceus in BE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 7 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 87 of 112 : Cenchrus setaceus in CY

    ## Warning: Removed 33 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 88 of 112 : Cenchrus setaceus in DE

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 5 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 89 of 112 : Cenchrus setaceus in LU

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 31 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 90 of 112 : Perccottus glenii in CZ

    ## Warning: Removed 25 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 91 of 112 : Persicaria perfoliata in NL

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 92 of 112 : Pistia stratiotes in FI

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 93 of 112 : Pistia stratiotes in HR

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 12 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 94 of 112 : Plotosus lineatus in CY

    ## Warning: Removed 12 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 95 of 112 : Procambarus clarkii in GR

    ## Warning: Removed 3 rows containing non-finite outside the scale range
    ## (`stat_smooth()`).

    ## Warning: Removed 10 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Warning: Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 96 of 112 : Procambarus clarkii in MT

    ## Warning: Removed 8 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 97 of 112 : Procambarus virginalis in EE

    ## Warning: Removed 1 row containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 98 of 112 : Procambarus virginalis in ES

    ## Warning: Removed 15 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 99 of 112 : Procambarus virginalis in MT

    ## Warning: Removed 24 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 100 of 112 : Procambarus virginalis in RO

    ## Warning: Removed 26 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 101 of 112 : Neltuma juliflora in BE

    ## Warning: Removed 22 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 102 of 112 : Neltuma juliflora in HU

    ## Warning: Removed 31 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 103 of 112 : Rugulopteryx okamurae in IT

    ## Warning: Removed 11 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 104 of 112 : Salvinia molesta in GB

    ## Processing combination 105 of 112 : Threskiornis aethiopicus in CZ

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 106 of 112 : Threskiornis aethiopicus in DE

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 107 of 112 : Trachemys scripta in CY

    ## Processing combination 108 of 112 : Trachemys scripta in SK

    ## Processing combination 109 of 112 : Triadica sebifera in PT

    ## Processing combination 110 of 112 : Vespa velutina in LU

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

    ## Processing combination 111 of 112 : Wasmannia auropunctata in CY

    ## Warning: Removed 24 rows containing missing values or values outside the scale range
    ## (`geom_smooth()`).

    ## Processing combination 112 of 112 : Xenopus laevis in NL

    ## Warning: Removed 3 rows containing non-finite outside the scale range (`stat_smooth()`).
    ## Removed 3 rows containing missing values or values outside the scale range
    ## (`geom_point()`).

``` r
message(paste0("Loop finished, verify that number of plots corresponds to loops in directory." ))
```

    ## Loop finished, verify that number of plots corresponds to loops in directory.

# 8. Verify correlations between GBIF and platforms

## 8.1. Clean up monthly data to allow 1-by-1 correlations

``` r
# Check dataframe structures

grouped_monthly_views_dt<-data.table::fread("allplatforms_monthly_activity_combined.csv")
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
# Print the new unique list to confirm the changes have been made
# You should no longer see the old names in the output.
message("Original unique scientific names in standardized_df:")
```

    ## Original unique scientific names in standardized_df:

``` r
print(unique(grouped_monthly_views_dt$SCIENTIFIC_NAME))
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
#pivot table to have columns with observations ready for correlations
wide_raw_views <- grouped_monthly_views_dt %>%
  pivot_wider(
    names_from = Internet_platform,
    values_from = total_views,
    values_fill = 0
  )

glimpse(wide_raw_views)
```

    ## Rows: 357,037
    ## Columns: 11
    ## $ SCIENTIFIC_NAME        <chr> "Acridotheres tristis", "Acridotheres tristis",…
    ## $ COUNTRY                <chr> "DE", "DE", "DE", "DE", "DE", "DE", "DE", "DE",…
    ## $ month_year             <chr> "2017-02", "2017-03", "2017-04", "2017-05", "20…
    ## $ `Wikipedia (geo)`      <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ `Wikipedia (lan)`      <dbl> 1255, 1353, 1594, 1212, 780, 898, 902, 889, 102…
    ## $ `Google health`        <dbl> 0.00000, 0.00000, 0.00000, 0.00000, 0.00000, 0.…
    ## $ Flickr                 <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ GBIF                   <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ `iNaturalist (casual)` <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Youtube                <dbl> 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,…
    ## $ Facebook               <dbl> 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.…

``` r
# Get a list of all non-GBIF platforms for later use
non_gbif_platforms <- wide_raw_views %>%
  select(where(is.numeric)) %>%
  select(-GBIF) %>%
  colnames()
```

## 8.2. Calculate Spearman correlations with GBIF data

``` r
# Calculate all Spearman rank correlations with GBIF data and store results
# Use `nest` to group the data by species and country
# Then use `map` to run the model for each group and platform
unique(wide_raw_views$SCIENTIFIC_NAME)
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
correlation_results_df <- wide_raw_views %>%
  group_by(SCIENTIFIC_NAME, COUNTRY) %>%
  nest() %>%
  mutate(
    correlations = map(data, ~ {
      platform_correlations <- tibble()
      
      for (platform in non_gbif_platforms) {
        # Check if the platform column exists and has sufficient data
        # Note: cor.test() requires at least 4 observations for Spearman.
        if (platform %in% colnames(.x) &&
            is.numeric(.x[[platform]]) &&
            is.numeric(.x$GBIF) &&
            !is.na(var(.x[[platform]], na.rm = TRUE)) &&
            !is.na(var(.x$GBIF, na.rm = TRUE)) &&
            var(.x[[platform]], na.rm = TRUE) > 0 &&
            var(.x$GBIF, na.rm = TRUE) > 0 &&
            nrow(.x) > 3) { # Changed to > 3 for Spearman
          
          # Use cor.test for Spearman correlation
          cor_test_result <- cor.test(.x$GBIF, .x[[platform]], method = "spearman")
          
          # Extract the rho coefficient and p-value
          rho <- cor_test_result$estimate
          p_value <- cor_test_result$p.value
          
          # Combine results into a row
          result_row <- tibble(
            platform_name = platform,
            rho = rho,
            p_value = p_value
          )
          
          platform_correlations <- bind_rows(platform_correlations, result_row)
        }
      }
      return(platform_correlations)
    })
  ) %>%
  select(-data) %>%
  unnest(correlations)
```

    ## Warning: There were 3876 warnings in `mutate()`.
    ## The first warning was:
    ## ℹ In argument: `correlations = map(...)`.
    ## ℹ In group 1: `SCIENTIFIC_NAME = "Acacia saligna"` and `COUNTRY = "AL"`.
    ## Caused by warning in `cor.test.default()`:
    ## ! Cannot compute exact p-value with ties
    ## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3875 remaining warnings.

``` r
unique(correlation_results_df$SCIENTIFIC_NAME)
```

    ##  [1] "Acridotheres tristis"        "Ailanthus altissima"        
    ##  [3] "Alopochen aegyptiaca"        "Ameiurus melas"             
    ##  [5] "Asclepias syriaca"           "Celastrus orbiculatus"      
    ##  [7] "Corvus splendens"            "Pontederia crassipes"       
    ##  [9] "Eriocheir sinensis"          "Heracleum mantegazzianum"   
    ## [11] "Heracleum persicum"          "Heracleum sosnowskyi"       
    ## [13] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [15] "Lampropeltis getula"         "Lepomis gibbosus"           
    ## [17] "Lithobates catesbeianus"     "Lysichiton americanus"      
    ## [19] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [21] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [23] "Ondatra zibethicus"          "Faxonius limosus"           
    ## [25] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [27] "Perccottus glenii"           "Pistia stratiotes"          
    ## [29] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [31] "Procyon lotor"               "Pseudorasbora parva"        
    ## [33] "Pueraria montana"            "Pycnonotus cafer"           
    ## [35] "Sciurus carolinensis"        "Solenopsis invicta"         
    ## [37] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [39] "Trachemys scripta"           "Vespa velutina"             
    ## [41] "Wasmannia auropunctata"      "Xenopus laevis"             
    ## [43] "Acacia saligna"              "Arthurdendyus triangulatus" 
    ## [45] "Axis axis"                   "Baccharis halimifolia"      
    ## [47] "Cabomba caroliniana"         "Callosciurus finlaysonii"   
    ## [49] "Elodea nuttallii"            "Gunnera tinctoria"          
    ## [51] "Hakea sericea"               "Ludwigia grandiflora"       
    ## [53] "Ludwigia peploides"          "Myriophyllum aquaticum"     
    ## [55] "Faxonius virilis"            "Oxyura jamaicensis"         
    ## [57] "Rugulopteryx okamurae"       "Alternanthera philoxeroides"
    ## [59] "Andropogon virginicus"       "Callosciurus erythraeus"    
    ## [61] "Cardiospermum grandiflorum"  "Cortaderia jubata"          
    ## [63] "Ehrharta calycina"           "Faxonius rusticus"          
    ## [65] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [67] "Gambusia holbrooki"          "Gymnocoronis spilanthoides" 
    ## [69] "Herpestes javanicus"         "Humulus scandens"           
    ## [71] "Hydrocotyle ranunculoides"   "Lagarosiphon major"         
    ## [73] "Lespedeza cuneata"           "Lygodium japonicum"         
    ## [75] "Microstegium vimineum"       "Myriophyllum heterophyllum" 
    ## [77] "Cenchrus setaceus"           "Persicaria perfoliata"      
    ## [79] "Plotosus lineatus"           "Neltuma juliflora"          
    ## [81] "Salvinia molesta"            "Sciurus niger"

``` r
unique(correlation_results_df$platform_name)
```

    ## [1] "Wikipedia (geo)"      "Wikipedia (lan)"      "Google health"       
    ## [4] "Youtube"              "Facebook"             "iNaturalist (casual)"
    ## [7] "Flickr"

## 8.3. Join with species characteristics data and calculate statistics

``` r
#read in traits of all union species
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

``` r
# Now, perform a left_join with the correlation results.
# The join key is a combination of species and country, which ensures a precise merge.
final_results_df <- correlation_results_df %>%
  left_join(union_traits, by = c("SCIENTIFIC_NAME"))

unique(final_results_df$SCIENTIFIC_NAME)
```

    ##  [1] "Acridotheres tristis"        "Ailanthus altissima"        
    ##  [3] "Alopochen aegyptiaca"        "Ameiurus melas"             
    ##  [5] "Asclepias syriaca"           "Celastrus orbiculatus"      
    ##  [7] "Corvus splendens"            "Pontederia crassipes"       
    ##  [9] "Eriocheir sinensis"          "Heracleum mantegazzianum"   
    ## [11] "Heracleum persicum"          "Heracleum sosnowskyi"       
    ## [13] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [15] "Lampropeltis getula"         "Lepomis gibbosus"           
    ## [17] "Lithobates catesbeianus"     "Lysichiton americanus"      
    ## [19] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [21] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [23] "Ondatra zibethicus"          "Faxonius limosus"           
    ## [25] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [27] "Perccottus glenii"           "Pistia stratiotes"          
    ## [29] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [31] "Procyon lotor"               "Pseudorasbora parva"        
    ## [33] "Pueraria montana"            "Pycnonotus cafer"           
    ## [35] "Sciurus carolinensis"        "Solenopsis invicta"         
    ## [37] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [39] "Trachemys scripta"           "Vespa velutina"             
    ## [41] "Wasmannia auropunctata"      "Xenopus laevis"             
    ## [43] "Acacia saligna"              "Arthurdendyus triangulatus" 
    ## [45] "Axis axis"                   "Baccharis halimifolia"      
    ## [47] "Cabomba caroliniana"         "Callosciurus finlaysonii"   
    ## [49] "Elodea nuttallii"            "Gunnera tinctoria"          
    ## [51] "Hakea sericea"               "Ludwigia grandiflora"       
    ## [53] "Ludwigia peploides"          "Myriophyllum aquaticum"     
    ## [55] "Faxonius virilis"            "Oxyura jamaicensis"         
    ## [57] "Rugulopteryx okamurae"       "Alternanthera philoxeroides"
    ## [59] "Andropogon virginicus"       "Callosciurus erythraeus"    
    ## [61] "Cardiospermum grandiflorum"  "Cortaderia jubata"          
    ## [63] "Ehrharta calycina"           "Faxonius rusticus"          
    ## [65] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [67] "Gambusia holbrooki"          "Gymnocoronis spilanthoides" 
    ## [69] "Herpestes javanicus"         "Humulus scandens"           
    ## [71] "Hydrocotyle ranunculoides"   "Lagarosiphon major"         
    ## [73] "Lespedeza cuneata"           "Lygodium japonicum"         
    ## [75] "Microstegium vimineum"       "Myriophyllum heterophyllum" 
    ## [77] "Cenchrus setaceus"           "Persicaria perfoliata"      
    ## [79] "Plotosus lineatus"           "Neltuma juliflora"          
    ## [81] "Salvinia molesta"            "Sciurus niger"

``` r
final_results_df <- final_results_df %>%
  mutate(
    Group = case_when(SCIENTIFIC_NAME == "Herpestes javanicus" ~ "Mammalia", TRUE ~ Group),
    Habitat = case_when(SCIENTIFIC_NAME == "Herpestes javanicus" ~ "Terrestrial", TRUE ~ Habitat)
  )

# View the final data frame
glimpse(final_results_df)
```

    ## Rows: 3,876
    ## Columns: 7
    ## $ SCIENTIFIC_NAME <chr> "Acridotheres tristis", "Acridotheres tristis", "Acrid…
    ## $ COUNTRY         <chr> "DE", "DE", "DE", "DE", "DE", "NL", "NL", "NL", "NL", …
    ## $ platform_name   <chr> "Wikipedia (geo)", "Wikipedia (lan)", "Google health",…
    ## $ rho             <dbl> -0.008695652, 0.076584502, -0.027015587, -0.015194013,…
    ## $ p_value         <dbl> 9.261876e-01, 4.138664e-01, 7.734476e-01, 8.713991e-01…
    ## $ Group           <chr> "Aves", "Aves", "Aves", "Aves", "Aves", "Aves", "Aves"…
    ## $ Habitat         <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terrestr…

``` r
unique(final_results_df$SCIENTIFIC_NAME)
```

    ##  [1] "Acridotheres tristis"        "Ailanthus altissima"        
    ##  [3] "Alopochen aegyptiaca"        "Ameiurus melas"             
    ##  [5] "Asclepias syriaca"           "Celastrus orbiculatus"      
    ##  [7] "Corvus splendens"            "Pontederia crassipes"       
    ##  [9] "Eriocheir sinensis"          "Heracleum mantegazzianum"   
    ## [11] "Heracleum persicum"          "Heracleum sosnowskyi"       
    ## [13] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [15] "Lampropeltis getula"         "Lepomis gibbosus"           
    ## [17] "Lithobates catesbeianus"     "Lysichiton americanus"      
    ## [19] "Muntiacus reevesi"           "Myocastor coypus"           
    ## [21] "Nasua nasua"                 "Nyctereutes procyonoides"   
    ## [23] "Ondatra zibethicus"          "Faxonius limosus"           
    ## [25] "Pacifastacus leniusculus"    "Parthenium hysterophorus"   
    ## [27] "Perccottus glenii"           "Pistia stratiotes"          
    ## [29] "Procambarus clarkii"         "Procambarus virginalis"     
    ## [31] "Procyon lotor"               "Pseudorasbora parva"        
    ## [33] "Pueraria montana"            "Pycnonotus cafer"           
    ## [35] "Sciurus carolinensis"        "Solenopsis invicta"         
    ## [37] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [39] "Trachemys scripta"           "Vespa velutina"             
    ## [41] "Wasmannia auropunctata"      "Xenopus laevis"             
    ## [43] "Acacia saligna"              "Arthurdendyus triangulatus" 
    ## [45] "Axis axis"                   "Baccharis halimifolia"      
    ## [47] "Cabomba caroliniana"         "Callosciurus finlaysonii"   
    ## [49] "Elodea nuttallii"            "Gunnera tinctoria"          
    ## [51] "Hakea sericea"               "Ludwigia grandiflora"       
    ## [53] "Ludwigia peploides"          "Myriophyllum aquaticum"     
    ## [55] "Faxonius virilis"            "Oxyura jamaicensis"         
    ## [57] "Rugulopteryx okamurae"       "Alternanthera philoxeroides"
    ## [59] "Andropogon virginicus"       "Callosciurus erythraeus"    
    ## [61] "Cardiospermum grandiflorum"  "Cortaderia jubata"          
    ## [63] "Ehrharta calycina"           "Faxonius rusticus"          
    ## [65] "Fundulus heteroclitus"       "Gambusia affinis"           
    ## [67] "Gambusia holbrooki"          "Gymnocoronis spilanthoides" 
    ## [69] "Herpestes javanicus"         "Humulus scandens"           
    ## [71] "Hydrocotyle ranunculoides"   "Lagarosiphon major"         
    ## [73] "Lespedeza cuneata"           "Lygodium japonicum"         
    ## [75] "Microstegium vimineum"       "Myriophyllum heterophyllum" 
    ## [77] "Cenchrus setaceus"           "Persicaria perfoliata"      
    ## [79] "Plotosus lineatus"           "Neltuma juliflora"          
    ## [81] "Salvinia molesta"            "Sciurus niger"

``` r
# Rename final_results_df to df_plot_joined for plotting
df_plot_joined <- final_results_df

# Calculate all means and standard deviations in a single pipeline
# These calculations will be applied to EACH ROW after grouping
df_plot_final <- df_plot_joined %>%
  # Group and mutate for internet_platform specific stats
  group_by(platform_name) %>%
  mutate(
    platform_m = mean(rho, na.rm = TRUE), # Using r_squared as the metric
    platform_sd = sd(rho, na.rm = TRUE)
  ) %>%
  # Ungroup to reset for the next grouping
  ungroup() %>%
  # Group and mutate for COUNTRY specific stats
  group_by(COUNTRY) %>%
  mutate(
    country_m = mean(rho, na.rm = TRUE),
    country_sd = sd(rho, na.rm = TRUE)
  ) %>%
  # Ungroup to reset for the next grouping
  ungroup() %>%
  # Group and mutate for SCIENTIFIC_NAME specific stats
  group_by(SCIENTIFIC_NAME) %>%
  mutate(
    species_m = mean(rho, na.rm = TRUE),
    species_sd = sd(rho, na.rm = TRUE)
  ) %>%
  # Ungroup to reset for the next grouping
  ungroup() %>%
  # Group and mutate for Group specific stats (from intro_year)
  group_by(Group) %>%
  mutate(
    group_m = mean(rho, na.rm = TRUE),
    group_sd = sd(rho, na.rm = TRUE)
  ) %>%
  # Ungroup to reset for the next grouping
  ungroup() %>%
  # Group and mutate for Habitat specific stats (from intro_year)
  group_by(Habitat) %>%
  mutate(
    habitat_m = mean(rho, na.rm = TRUE),
    habitat_sd = sd(rho, na.rm = TRUE)
  ) %>%
  ungroup() # Ungroup after all mutations to avoid unintended grouping effects later

# Ensure platform_name is a factor with specified levels for consistent plotting order
# The user's example had "GBIF", "Youtube", "Wikipedia (lan)", "Wikipedia (geo)", "iNaturalist (casual)", "Google health", "Flickr"
# We'll use the unique platforms from the data to define levels.
platform_levels_ordered <- c("GBIF", "Youtube", "Wikipedia (lan)", "Wikipedia (geo)", "iNaturalist (casual)", "Google health", "Flickr")
# Ensure all platforms in df_plot_final are covered, and add any missing ones
actual_platforms_in_data <- unique(df_plot_final$platform_name)
platform_levels <- unique(c(platform_levels_ordered, actual_platforms_in_data))
df_plot_final$platform_name <- factor(df_plot_final$platform_name, levels = platform_levels, ordered = TRUE)


message("df_plot_final after all calculations (first few rows):")
```

    ## df_plot_final after all calculations (first few rows):

``` r
print(glimpse(df_plot_final))
```

    ## Rows: 3,876
    ## Columns: 17
    ## $ SCIENTIFIC_NAME <chr> "Acridotheres tristis", "Acridotheres tristis", "Acrid…
    ## $ COUNTRY         <chr> "DE", "DE", "DE", "DE", "DE", "NL", "NL", "NL", "NL", …
    ## $ platform_name   <ord> Wikipedia (geo), Wikipedia (lan), Google health, Youtu…
    ## $ rho             <dbl> -0.008695652, 0.076584502, -0.027015587, -0.015194013,…
    ## $ p_value         <dbl> 9.261876e-01, 4.138664e-01, 7.734476e-01, 8.713991e-01…
    ## $ Group           <chr> "Aves", "Aves", "Aves", "Aves", "Aves", "Aves", "Aves"…
    ## $ Habitat         <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terrestr…
    ## $ platform_m      <dbl> 0.12096648, 0.24792064, 0.09920058, 0.05205744, 0.0733…
    ## $ platform_sd     <dbl> 0.1922771, 0.2716230, 0.2050226, 0.1157161, 0.1450108,…
    ## $ country_m       <dbl> 0.17472843, 0.17472843, 0.17472843, 0.17472843, 0.1747…
    ## $ country_sd      <dbl> 0.2379125, 0.2379125, 0.2379125, 0.2379125, 0.2379125,…
    ## $ species_m       <dbl> 0.01562881, 0.01562881, 0.01562881, 0.01562881, 0.0156…
    ## $ species_sd      <dbl> 0.1150955, 0.1150955, 0.1150955, 0.1150955, 0.1150955,…
    ## $ group_m         <dbl> 0.08517968, 0.08517968, 0.08517968, 0.08517968, 0.0851…
    ## $ group_sd        <dbl> 0.1901185, 0.1901185, 0.1901185, 0.1901185, 0.1901185,…
    ## $ habitat_m       <dbl> 0.1349932, 0.1349932, 0.1349932, 0.1349932, 0.1349932,…
    ## $ habitat_sd      <dbl> 0.222124, 0.222124, 0.222124, 0.222124, 0.222124, 0.22…
    ## # A tibble: 3,876 × 17
    ##    SCIENTIFIC_NAME      COUNTRY platform_name          rho p_value Group Habitat
    ##    <chr>                <chr>   <ord>                <dbl>   <dbl> <chr> <chr>  
    ##  1 Acridotheres tristis DE      Wikipedia (geo)   -0.00870 0.926   Aves  Terres…
    ##  2 Acridotheres tristis DE      Wikipedia (lan)    0.0766  0.414   Aves  Terres…
    ##  3 Acridotheres tristis DE      Google health     -0.0270  0.773   Aves  Terres…
    ##  4 Acridotheres tristis DE      Youtube           -0.0152  0.871   Aves  Terres…
    ##  5 Acridotheres tristis DE      Facebook          -0.0487  0.603   Aves  Terres…
    ##  6 Acridotheres tristis NL      Wikipedia (geo)    0.270   0.00340 Aves  Terres…
    ##  7 Acridotheres tristis NL      Wikipedia (lan)    0.216   0.0196  Aves  Terres…
    ##  8 Acridotheres tristis NL      Google health      0.0320  0.733   Aves  Terres…
    ##  9 Acridotheres tristis NL      iNaturalist (cas… -0.0578  0.538   Aves  Terres…
    ## 10 Acridotheres tristis NL      Youtube            0.196   0.0350  Aves  Terres…
    ## # ℹ 3,866 more rows
    ## # ℹ 10 more variables: platform_m <dbl>, platform_sd <dbl>, country_m <dbl>,
    ## #   country_sd <dbl>, species_m <dbl>, species_sd <dbl>, group_m <dbl>,
    ## #   group_sd <dbl>, habitat_m <dbl>, habitat_sd <dbl>

## 8.4. Make the Spearman rank correlation plots

``` r
# Ensure a dedicated output directory for these new plots
comparison_plots_dir <- "mean_r2_plots" # Changed name to reflect R2
dir.create(comparison_plots_dir, showWarnings = FALSE)
  
### Plot 1: Mean R² by Internet Platform
  
#This plot displays the mean spearman's rho value for each internet platform, along with error bars representing the standard deviation. Individual rho values are shown as jittered points, colored by their `Group`. Other plots have similar logic.

p_platform <- ggplot(df_plot_final) +
  geom_jitter(aes(x = reorder(platform_name, rho, FUN = median), y = rho, color = Group, fill = Group),shape = 21, color = "black", size = 4, height = 0,width=0.2) +
  geom_boxplot(aes(x = reorder(platform_name, rho, FUN = median),y=rho),size=1,color="black",fill=NA,outliers=F)+
  #geom_point(aes(x = platform_name, y = platform_m), size = 4, shape = "square", color = "black") +
  #geom_errorbar(aes(x = platform_name, ymin = platform_m - platform_sd, ymax = platform_m + platform_sd), size = 1.5, width = 0.5, color = "black") +
  ylab("Spearman's ρ with monthly GBIF observations") +
  xlab("Internet platform") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Species group", fill = "Species group") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black",angle = 65,hjust=1),
        axis.text.y = element_text (size = 12, color = "black"),
        legend.text = element_text (size = 12, color = "black"),
        legend.title = element_text (size = 14, face = "bold", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_platform)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20spearman%20correlations%20with%20GBIF%20occurence%20data-1.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanR2_platforms.png"), plot = p_platform, width = 10, height = 10, dpi = 300, bg = "white")

#Plot 2: mean Rho per species

p_species <- ggplot(df_plot_final) +
  geom_jitter(aes(x = reorder(SCIENTIFIC_NAME, rho, FUN = median), y = rho, color = Group, fill = Group), shape = 21, color = "black", size = 4, height = 0, width = 0.2) +
  geom_boxplot(aes(x = reorder(SCIENTIFIC_NAME, rho, FUN = median), y = rho), size = 1, color = "black", fill = NA, outliers = FALSE) +
  ylab("Spearman's ρ with monthly GBIF observations") +
  xlab("Species") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1,face='italic'),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_species)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20spearman%20correlations%20with%20GBIF%20occurence%20data-2.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanR2_species.png"), plot = p_species, width = 15, height = 10, dpi = 300, bg = "white")

# Plot 3: mean rho per country

p_country <- ggplot(df_plot_final) +
  geom_jitter(aes(x = reorder(COUNTRY, rho, FUN = median), y = rho, color = platform_name, fill = platform_name), shape = 21, color = "black", size = 4, height = 0, width = 0.2) +
  geom_boxplot(aes(x = reorder(COUNTRY, rho, FUN = median), y = rho), size = 1, color = "black", fill = NA, outliers = FALSE) +
  ylab("Spearman's ρ with monthly GBIF observations") +
  xlab("Country") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_country)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20spearman%20correlations%20with%20GBIF%20occurence%20data-3.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanR2_countries.png"), plot = p_country, width = 10, height = 10, dpi = 300, bg = "white")

# Plot 4: mean rho per species group

p_group <- ggplot(df_plot_final) +
  geom_jitter(aes(x = reorder(Group, rho, FUN = median), y = rho, color = platform_name, fill = platform_name), shape = 21, color = "black", size = 4, height = 0, width = 0.2) +
  geom_boxplot(aes(x = reorder(Group, rho, FUN = median), y = rho), size = 1, color = "black", fill = NA, outliers = FALSE) +
  ylab("Spearman's ρ with monthly GBIF observations") +
  xlab("Species Group") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_group)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20spearman%20correlations%20with%20GBIF%20occurence%20data-4.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanR2_groups.png"), plot = p_group, width = 10, height = 10, dpi = 300, bg = "white")

# Plot 5: mean rho per habitat

p_habitat <- ggplot(df_plot_final) +
  geom_jitter(aes(x = reorder(Habitat, rho, FUN = median), y = rho, color = platform_name, fill = platform_name), shape = 21, color = "black", size = 4, height = 0, width = 0.2) +
  geom_boxplot(aes(x = reorder(Habitat, rho, FUN = median), y = rho), size = 1, color = "black", fill = NA, outliers = FALSE) +
  ylab("Spearman's ρ with monthly GBIF observations") +
  xlab("Habitat") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_habitat)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20spearman%20correlations%20with%20GBIF%20occurence%20data-5.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_meanR2_habitats.png"), plot = p_habitat, width = 10, height = 10, dpi = 300, bg = "white")

#combined plot for publication
combined_plot <- plot_grid(p_platform, p_group,
                           labels = c("(a)", "(b)"),
                           label_size = 14,
                           ncol = 2,
                           align = "h")

print(combined_plot)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20spearman%20correlations%20with%20GBIF%20occurence%20data-6.png)<!-- -->

``` r
ggsave(file.path(comparison_plots_dir, "comparison_plot_combined.png"), combined_plot,
          bg = "white",
          width = 14,  # You can adjust these dimensions as needed
          height = 7,
          dpi = 300)
```

# 9. Calculate the popularity per species x country x platform combinations and compare

``` r
daily_activity_df<-data.table::fread("allplatforms_daily_activity_combined.csv")

# Calculate popularity for each country x species x platform combination
popularity_df <- daily_activity_df %>%
  group_by(COUNTRY, SCIENTIFIC_NAME, Internet_platform) %>%
  summarise(
    popularity = sum(views > 0 & !is.na(views)), #calculates no of unique days with activity
    .groups = 'drop' 
  )

# Display the result
glimpse(popularity_df)
```

    ## Rows: 24,352
    ## Columns: 4
    ## $ COUNTRY           <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ Internet_platform <chr> "Facebook", "Flickr", "GBIF", "Google health", "Wiki…
    ## $ popularity        <int> 1, 0, 11, 0, 0, 0, 0, 3, 2, 0, 16, 0, 0, 0, 0, 15, 1…

``` r
unique(popularity_df$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Ailanthus altissima"        
    ##  [3] "Asclepias syriaca"           "Axis axis"                  
    ##  [5] "Cenchrus setaceus"           "Gambusia affinis"           
    ##  [7] "Gambusia holbrooki"          "Heracleum mantegazzianum"   
    ##  [9] "Herpestes javanicus"         "Lepomis gibbosus"           
    ## [11] "Lysichiton americanus"       "Myocastor coypus"           
    ## [13] "Myriophyllum aquaticum"      "Pistia stratiotes"          
    ## [15] "Pontederia crassipes"        "Procambarus clarkii"        
    ## [17] "Procambarus virginalis"      "Solenopsis invicta"         
    ## [19] "Trachemys scripta"           "Vespa velutina"             
    ## [21] "Xenopus laevis"              "Acridotheres tristis"       
    ## [23] "Alopochen aegyptiaca"        "Alternanthera philoxeroides"
    ## [25] "Ameiurus melas"              "Andropogon virginicus"      
    ## [27] "Arthurdendyus triangulatus"  "Baccharis halimifolia"      
    ## [29] "Cabomba caroliniana"         "Callosciurus erythraeus"    
    ## [31] "Callosciurus finlaysonii"    "Cardiospermum grandiflorum" 
    ## [33] "Celastrus orbiculatus"       "Channa argus"               
    ## [35] "Cortaderia jubata"           "Corvus splendens"           
    ## [37] "Ehrharta calycina"           "Elodea nuttallii"           
    ## [39] "Eriocheir sinensis"          "Faxonius limosus"           
    ## [41] "Faxonius rusticus"           "Faxonius virilis"           
    ## [43] "Fundulus heteroclitus"       "Gunnera tinctoria"          
    ## [45] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [47] "Heracleum persicum"          "Heracleum sosnowskyi"       
    ## [49] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [51] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [53] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [55] "Lespedeza cuneata"           "Limnoperna fortunei"        
    ## [57] "Lithobates catesbeianus"     "Ludwigia grandiflora"       
    ## [59] "Ludwigia peploides"          "Lygodium japonicum"         
    ## [61] "Microstegium vimineum"       "Morone americana"           
    ## [63] "Muntiacus reevesi"           "Myriophyllum heterophyllum" 
    ## [65] "Nasua nasua"                 "Neltuma juliflora"          
    ## [67] "Nyctereutes procyonoides"    "Ondatra zibethicus"         
    ## [69] "Oxyura jamaicensis"          "Pacifastacus leniusculus"   
    ## [71] "Parthenium hysterophorus"    "Perccottus glenii"          
    ## [73] "Persicaria perfoliata"       "Plotosus lineatus"          
    ## [75] "Procyon lotor"               "Pseudorasbora parva"        
    ## [77] "Pueraria montana"            "Pycnonotus cafer"           
    ## [79] "Rugulopteryx okamurae"       "Salvinia molesta"           
    ## [81] "Sciurus carolinensis"        "Sciurus niger"              
    ## [83] "Solenopsis geminata"         "Solenopsis richteri"        
    ## [85] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [87] "Triadica sebifera"           "Wasmannia auropunctata"

``` r
popularity_df=popularity_df%>%
  mutate(
    SCIENTIFIC_NAME = case_when(
      # Safe synonym collapses:
      SCIENTIFIC_NAME == "Cortaderia selloana subsp. jubata" ~ "Cortaderia jubata",
      SCIENTIFIC_NAME == "Eichhornia crassipes" ~ "Pontederia crassipes",
      SCIENTIFIC_NAME == "Trachemys scripta elegans" ~ "Trachemys scripta",
      SCIENTIFIC_NAME == "Pennisetum setaceum" ~ "Cenchrus setaceus",
      SCIENTIFIC_NAME == "Orconectes limosus" ~ "Faxonius limosus",
      SCIENTIFIC_NAME == "Orconectes virilis" ~ "Faxonius virilis",
      SCIENTIFIC_NAME == "Vespa velutina nigrithorax" ~ "Vespa velutina",
      SCIENTIFIC_NAME == "Herpestes auropunctatus" ~ "Herpestes javanicus",
      SCIENTIFIC_NAME == "Prosopis juliflora" ~ "Neltuma juliflora",       #  NEW: synonym handled
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

``` r
unique(popularity_df$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Ailanthus altissima"        
    ##  [3] "Asclepias syriaca"           "Axis axis"                  
    ##  [5] "Cenchrus setaceus"           "Gambusia affinis"           
    ##  [7] "Gambusia holbrooki"          "Heracleum mantegazzianum"   
    ##  [9] "Herpestes javanicus"         "Lepomis gibbosus"           
    ## [11] "Lysichiton americanus"       "Myocastor coypus"           
    ## [13] "Myriophyllum aquaticum"      "Pistia stratiotes"          
    ## [15] "Pontederia crassipes"        "Procambarus clarkii"        
    ## [17] "Procambarus virginalis"      "Solenopsis invicta"         
    ## [19] "Trachemys scripta"           "Vespa velutina"             
    ## [21] "Xenopus laevis"              "Acridotheres tristis"       
    ## [23] "Alopochen aegyptiaca"        "Alternanthera philoxeroides"
    ## [25] "Ameiurus melas"              "Andropogon virginicus"      
    ## [27] "Arthurdendyus triangulatus"  "Baccharis halimifolia"      
    ## [29] "Cabomba caroliniana"         "Callosciurus erythraeus"    
    ## [31] "Callosciurus finlaysonii"    "Cardiospermum grandiflorum" 
    ## [33] "Celastrus orbiculatus"       "Channa argus"               
    ## [35] "Cortaderia jubata"           "Corvus splendens"           
    ## [37] "Ehrharta calycina"           "Elodea nuttallii"           
    ## [39] "Eriocheir sinensis"          "Faxonius limosus"           
    ## [41] "Faxonius rusticus"           "Faxonius virilis"           
    ## [43] "Fundulus heteroclitus"       "Gunnera tinctoria"          
    ## [45] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [47] "Heracleum persicum"          "Heracleum sosnowskyi"       
    ## [49] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [51] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [53] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [55] "Lespedeza cuneata"           "Limnoperna fortunei"        
    ## [57] "Lithobates catesbeianus"     "Ludwigia grandiflora"       
    ## [59] "Ludwigia peploides"          "Lygodium japonicum"         
    ## [61] "Microstegium vimineum"       "Morone americana"           
    ## [63] "Muntiacus reevesi"           "Myriophyllum heterophyllum" 
    ## [65] "Nasua nasua"                 "Neltuma juliflora"          
    ## [67] "Nyctereutes procyonoides"    "Ondatra zibethicus"         
    ## [69] "Oxyura jamaicensis"          "Pacifastacus leniusculus"   
    ## [71] "Parthenium hysterophorus"    "Perccottus glenii"          
    ## [73] "Persicaria perfoliata"       "Plotosus lineatus"          
    ## [75] "Procyon lotor"               "Pseudorasbora parva"        
    ## [77] "Pueraria montana"            "Pycnonotus cafer"           
    ## [79] "Rugulopteryx okamurae"       "Salvinia molesta"           
    ## [81] "Sciurus carolinensis"        "Sciurus niger"              
    ## [83] "Solenopsis geminata"         "Solenopsis richteri"        
    ## [85] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [87] "Triadica sebifera"           "Wasmannia auropunctata"

``` r
# Perform the left join
joined_df <- left_join(popularity_df, union_traits, by = "SCIENTIFIC_NAME")

glimpse(joined_df)
```

    ## Rows: 24,352
    ## Columns: 6
    ## $ COUNTRY           <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ Internet_platform <chr> "Facebook", "Flickr", "GBIF", "Google health", "Wiki…
    ## $ popularity        <int> 1, 0, 11, 0, 0, 0, 0, 3, 2, 0, 16, 0, 0, 0, 0, 15, 1…
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…

``` r
unique(joined_df$SCIENTIFIC_NAME)
```

    ##  [1] "Acacia saligna"              "Ailanthus altissima"        
    ##  [3] "Asclepias syriaca"           "Axis axis"                  
    ##  [5] "Cenchrus setaceus"           "Gambusia affinis"           
    ##  [7] "Gambusia holbrooki"          "Heracleum mantegazzianum"   
    ##  [9] "Herpestes javanicus"         "Lepomis gibbosus"           
    ## [11] "Lysichiton americanus"       "Myocastor coypus"           
    ## [13] "Myriophyllum aquaticum"      "Pistia stratiotes"          
    ## [15] "Pontederia crassipes"        "Procambarus clarkii"        
    ## [17] "Procambarus virginalis"      "Solenopsis invicta"         
    ## [19] "Trachemys scripta"           "Vespa velutina"             
    ## [21] "Xenopus laevis"              "Acridotheres tristis"       
    ## [23] "Alopochen aegyptiaca"        "Alternanthera philoxeroides"
    ## [25] "Ameiurus melas"              "Andropogon virginicus"      
    ## [27] "Arthurdendyus triangulatus"  "Baccharis halimifolia"      
    ## [29] "Cabomba caroliniana"         "Callosciurus erythraeus"    
    ## [31] "Callosciurus finlaysonii"    "Cardiospermum grandiflorum" 
    ## [33] "Celastrus orbiculatus"       "Channa argus"               
    ## [35] "Cortaderia jubata"           "Corvus splendens"           
    ## [37] "Ehrharta calycina"           "Elodea nuttallii"           
    ## [39] "Eriocheir sinensis"          "Faxonius limosus"           
    ## [41] "Faxonius rusticus"           "Faxonius virilis"           
    ## [43] "Fundulus heteroclitus"       "Gunnera tinctoria"          
    ## [45] "Gymnocoronis spilanthoides"  "Hakea sericea"              
    ## [47] "Heracleum persicum"          "Heracleum sosnowskyi"       
    ## [49] "Humulus scandens"            "Hydrocotyle ranunculoides"  
    ## [51] "Impatiens glandulifera"      "Koenigia polystachya"       
    ## [53] "Lagarosiphon major"          "Lampropeltis getula"        
    ## [55] "Lespedeza cuneata"           "Limnoperna fortunei"        
    ## [57] "Lithobates catesbeianus"     "Ludwigia grandiflora"       
    ## [59] "Ludwigia peploides"          "Lygodium japonicum"         
    ## [61] "Microstegium vimineum"       "Morone americana"           
    ## [63] "Muntiacus reevesi"           "Myriophyllum heterophyllum" 
    ## [65] "Nasua nasua"                 "Neltuma juliflora"          
    ## [67] "Nyctereutes procyonoides"    "Ondatra zibethicus"         
    ## [69] "Oxyura jamaicensis"          "Pacifastacus leniusculus"   
    ## [71] "Parthenium hysterophorus"    "Perccottus glenii"          
    ## [73] "Persicaria perfoliata"       "Plotosus lineatus"          
    ## [75] "Procyon lotor"               "Pseudorasbora parva"        
    ## [77] "Pueraria montana"            "Pycnonotus cafer"           
    ## [79] "Rugulopteryx okamurae"       "Salvinia molesta"           
    ## [81] "Sciurus carolinensis"        "Sciurus niger"              
    ## [83] "Solenopsis geminata"         "Solenopsis richteri"        
    ## [85] "Tamias sibiricus"            "Threskiornis aethiopicus"   
    ## [87] "Triadica sebifera"           "Wasmannia auropunctata"

``` r
#calculate the total number of non-zero activities across all social media excluding GBIF
popularities_by_platform <- joined_df %>%
  filter(Internet_platform != "GBIF") %>%
  filter(popularity > 0) %>%
  #group_by(Internet_platform)%>%
  summarise(total_popularity = sum(popularity))

print(popularities_by_platform)
```

    ## # A tibble: 1 × 1
    ##   total_popularity
    ##              <int>
    ## 1          3830528

``` r
# Filter the data frame to find rows with NA in 'Group' or 'Habitat'
species_with_na_classification <- joined_df %>%
  filter(is.na(Group) | is.na(Habitat)) %>%
  dplyr::select(SCIENTIFIC_NAME, Group, Habitat)

# Display the result
species_with_na_classification #needs to be zero
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: SCIENTIFIC_NAME <chr>, Group <chr>, Habitat <chr>

``` r
# Calculate mean popularities, correcting for number of species in group / habitat

df_all_stats <- joined_df %>%
  filter(Internet_platform != "GBIF")%>%
  # First, calculate the mean popularity for each species
  group_by(SCIENTIFIC_NAME) %>%
  mutate(
    species_m = mean(popularity, na.rm = TRUE),
    species_sd = sd(popularity, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Now, use the species-level mean to calculate the group-level means
  # This corrects for the number of species within each group
  group_by(Group) %>%
  mutate(
    group_m = mean(species_m, na.rm = TRUE),
    group_sd = sd(species_m, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Now calculate the means for Habitat, using the species-level mean
  group_by(Habitat) %>%
  mutate(
    habitat_m = mean(species_m, na.rm = TRUE),
    habitat_sd = sd(species_m, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Finally, calculate the means for Country and Internet_platform as these
  # are already at a detailed level and don't require the same correction
  group_by(COUNTRY) %>%
  mutate(
    country_m = mean(popularity, na.rm = TRUE),
    country_sd = sd(popularity, na.rm = TRUE)
  ) %>%
  ungroup()

unique(df_all_stats$Internet_platform)
```

    ## [1] "Facebook"             "Flickr"               "Google health"       
    ## [4] "Wikipedia (geo)"      "Wikipedia (lan)"      "Youtube"             
    ## [7] "iNaturalist (casual)"

``` r
platform_comp=joined_df %>% #includes GBIF for comparison with reference data
  group_by(Internet_platform) %>%
  mutate(
    platform_m = mean(popularity, na.rm = TRUE),
    platform_sd = sd(popularity, na.rm = TRUE)
  ) %>%
  ungroup()

# View the structure of the final data frame
print(glimpse(platform_comp))
```

    ## Rows: 24,352
    ## Columns: 8
    ## $ COUNTRY           <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ Internet_platform <chr> "Facebook", "Flickr", "GBIF", "Google health", "Wiki…
    ## $ popularity        <int> 1, 0, 11, 0, 0, 0, 0, 3, 2, 0, 16, 0, 0, 0, 0, 15, 1…
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…
    ## $ platform_m        <dbl> 4.8229304, 0.8876478, 78.4438239, 17.7713535, 10.789…
    ## $ platform_sd       <dbl> 11.68087, 13.15313, 340.64340, 72.31691, 117.20423, …
    ## # A tibble: 24,352 × 8
    ##    COUNTRY SCIENTIFIC_NAME Internet_platform popularity Group Habitat platform_m
    ##    <chr>   <chr>           <chr>                  <int> <chr> <chr>        <dbl>
    ##  1 AL      Acacia saligna  Facebook                   1 Plan… Terres…      4.82 
    ##  2 AL      Acacia saligna  Flickr                     0 Plan… Terres…      0.888
    ##  3 AL      Acacia saligna  GBIF                      11 Plan… Terres…     78.4  
    ##  4 AL      Acacia saligna  Google health              0 Plan… Terres…     17.8  
    ##  5 AL      Acacia saligna  Wikipedia (geo)            0 Plan… Terres…     10.8  
    ##  6 AL      Acacia saligna  Wikipedia (lan)            0 Plan… Terres…   1215.   
    ##  7 AL      Acacia saligna  Youtube                    0 Plan… Terres…      3.02 
    ##  8 AL      Acacia saligna  iNaturalist (cas…          3 Plan… Terres…      6.16 
    ##  9 AL      Ailanthus alti… Facebook                   2 Plan… Terres…      4.82 
    ## 10 AL      Ailanthus alti… Flickr                     0 Plan… Terres…      0.888
    ## # ℹ 24,342 more rows
    ## # ℹ 1 more variable: platform_sd <dbl>

``` r
print(glimpse(df_all_stats))
```

    ## Rows: 21,308
    ## Columns: 14
    ## $ COUNTRY           <chr> "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL", "AL"…
    ## $ SCIENTIFIC_NAME   <chr> "Acacia saligna", "Acacia saligna", "Acacia saligna"…
    ## $ Internet_platform <chr> "Facebook", "Flickr", "Google health", "Wikipedia (g…
    ## $ popularity        <int> 1, 0, 0, 0, 0, 0, 3, 2, 0, 0, 0, 0, 0, 15, 1, 0, 0, …
    ## $ Group             <chr> "Plantae", "Plantae", "Plantae", "Plantae", "Plantae…
    ## $ Habitat           <chr> "Terrestrial", "Terrestrial", "Terrestrial", "Terres…
    ## $ species_m         <dbl> 138.9592, 138.9592, 138.9592, 138.9592, 138.9592, 13…
    ## $ species_sd        <dbl> 639.1267, 639.1267, 639.1267, 639.1267, 639.1267, 63…
    ## $ group_m           <dbl> 130.8200, 130.8200, 130.8200, 130.8200, 130.8200, 13…
    ## $ group_sd          <dbl> 76.51216, 76.51216, 76.51216, 76.51216, 76.51216, 76…
    ## $ habitat_m         <dbl> 168.2495, 168.2495, 168.2495, 168.2495, 168.2495, 16…
    ## $ habitat_sd        <dbl> 104.8402, 104.8402, 104.8402, 104.8402, 104.8402, 10…
    ## $ country_m         <dbl> 5.47619, 5.47619, 5.47619, 5.47619, 5.47619, 5.47619…
    ## $ country_sd        <dbl> 53.29491, 53.29491, 53.29491, 53.29491, 53.29491, 53…
    ## # A tibble: 21,308 × 14
    ##    COUNTRY SCIENTIFIC_NAME  Internet_platform popularity Group Habitat species_m
    ##    <chr>   <chr>            <chr>                  <int> <chr> <chr>       <dbl>
    ##  1 AL      Acacia saligna   Facebook                   1 Plan… Terres…      139.
    ##  2 AL      Acacia saligna   Flickr                     0 Plan… Terres…      139.
    ##  3 AL      Acacia saligna   Google health              0 Plan… Terres…      139.
    ##  4 AL      Acacia saligna   Wikipedia (geo)            0 Plan… Terres…      139.
    ##  5 AL      Acacia saligna   Wikipedia (lan)            0 Plan… Terres…      139.
    ##  6 AL      Acacia saligna   Youtube                    0 Plan… Terres…      139.
    ##  7 AL      Acacia saligna   iNaturalist (cas…          3 Plan… Terres…      139.
    ##  8 AL      Ailanthus altis… Facebook                   2 Plan… Terres…      312.
    ##  9 AL      Ailanthus altis… Flickr                     0 Plan… Terres…      312.
    ## 10 AL      Ailanthus altis… Google health              0 Plan… Terres…      312.
    ## # ℹ 21,298 more rows
    ## # ℹ 7 more variables: species_sd <dbl>, group_m <dbl>, group_sd <dbl>,
    ## #   habitat_m <dbl>, habitat_sd <dbl>, country_m <dbl>, country_sd <dbl>

``` r
#make the actual plots

# Ensure a dedicated output directory for these new plots
popularity_plots_dir <- "popularity_plots"
dir.create(popularity_plots_dir, showWarnings = FALSE)

### Plot 1: Mean Popularity by Internet Platform

p_platform <- ggplot(platform_comp) +
  geom_errorbar(aes(x = reorder(Internet_platform, platform_m, FUN = median), ymin = platform_m, ymax = platform_m + platform_sd),color="black", width = 0.5) +
  stat_summary(aes(x=Internet_platform, y=platform_m,fill=Internet_platform),fun=mean, geom="bar", color="black") +
  ylab("Popularity (# of days with activity)") +
  xlab("Internet platform") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Internet platform", fill = "Internet platform") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_platform)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20popularity%20plots-1.png)<!-- -->

``` r
ggsave(file.path(popularity_plots_dir, "popularity_platforms.png"), plot = p_platform, width = 10, height = 10, dpi = 300, bg = "white")

### Plot 2: Mean Popularity by Species

p_species <- ggplot(df_all_stats) +
  geom_errorbar(aes(x = reorder(SCIENTIFIC_NAME, species_m, FUN = median), ymin = species_m, ymax = species_m + species_sd), width = 0.5, color = "black") +
  stat_summary(aes(x = reorder(SCIENTIFIC_NAME, species_m, FUN = median), y=species_m,fill=Group),fun=mean, geom="bar", color="black") +
  ylab("Popularity (# of days with activity)") +
  xlab("Species") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(color = "Species group", fill = "Species group") +
  theme_bw() +
  theme(axis.title = element_text(size = 14, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1, face = 'italic'),
        axis.text.y = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.title = element_text(size = 14, face = "bold", color = "black"),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black"),
        plot.title = element_text(face = "bold", hjust = 0.5)
  )

print(p_species)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20popularity%20plots-2.png)<!-- -->

``` r
ggsave(file.path(popularity_plots_dir, "popularity_species.png"), plot = p_species, width = 15, height = 10, dpi = 300, bg = "white")

### Plot 3: Mean Popularity by Country
p_country <- ggplot(df_all_stats) +
  geom_errorbar(
    aes(x = reorder(COUNTRY, country_m, FUN = median),
        ymin = country_m,
        ymax = country_m + country_sd),
    width = 0.5, color = "black"
  ) +
  stat_summary(
    aes(x = reorder(COUNTRY, country_m, FUN = median),
        y = country_m),
    fill = "black", fun = mean, geom = "bar", color = "black"
  ) +
  ylab("Popularity (# of days with activity)") +
  xlab("Country") +
  theme_bw() +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme(
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
print(p_country)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20popularity%20plots-3.png)<!-- -->

``` r
ggsave(file.path(popularity_plots_dir, "popularity_countries.png"), plot = p_country, width = 10, height = 10, dpi = 300, bg = "white")


### Plot 4: Mean Popularity by Species Group
p_group <- ggplot(df_all_stats) +
  geom_errorbar(
    aes(x = reorder(Group, group_m, FUN = median),
        ymin = group_m,
        ymax = group_m + group_sd),
    width = 0.5, color = "black"
  ) +
  stat_summary(
    aes(x = reorder(Group, group_m, FUN = median),
        y = group_m,fill=Group), fun = mean, geom = "bar", color = "black"
  ) +
  ylab("Popularity (# of days with activity)") +
  xlab("Species Group") +
  labs(fill="Species group")+
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(fill = "Species group") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
print(p_group)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20popularity%20plots-4.png)<!-- -->

``` r
ggsave(file.path(popularity_plots_dir, "popularity_groups.png"), plot = p_group, width = 10, height = 10, dpi = 300, bg = "white")


### Plot 5: Mean Popularity by Habitat
p_habitat <- ggplot(df_all_stats) +
  geom_errorbar(
    aes(x = reorder(Habitat, habitat_m, FUN = median),
        ymin = habitat_m,
        ymax = habitat_m + habitat_sd),
    width = 0.5, color = "black"
  ) +
  stat_summary(
    aes(x = reorder(Habitat, habitat_m, FUN = median),
        y = habitat_m,fill=Habitat), fun = mean, geom = "bar", color = "black"
  ) +
  ylab("Popularity (# of daiys with activity)") +
  xlab("Habitat") +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  labs(fill = "Habitat") +
  theme_bw() +
  theme(
    axis.title = element_text(size = 14, face = "bold", color = "black"),
    axis.text.x = element_text(size = 12, color = "black", angle = 65, hjust = 1),
    axis.text.y = element_text(size = 12, color = "black"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
print(p_habitat)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20popularity%20plots-5.png)<!-- -->

``` r
ggsave(file.path(popularity_plots_dir, "popularity_habitats.png"), plot = p_habitat, width = 10, height = 10, dpi = 300, bg = "white")

#combined plot for publication
combined_plot <- plot_grid(p_platform, p_group,
                           labels = c("(a)", "(b)"),
                           label_size = 14,
                           ncol = 2,
                           align = "h")

print(combined_plot)
```

![](iEcology_data_analysis_part2_files/figure-gfm/Plot%20the%20popularity%20plots-6.png)<!-- -->

``` r
ggsave(file.path(popularity_plots_dir, "popularity_plot_combined.png"), combined_plot,
          bg = "white",
          width = 14,  # You can adjust these dimensions as needed
          height = 6,
          dpi = 300)
```

# 10. Calculate changepoints on lump data for false positives / negatives detection

## 10.1 Final selected calculation method

``` r
#strongest GAM + slope + variance detection (best in the end)

# Initialize lists
results_list <- list()
problematic_cpt_combos <- list()

# Create output directory
plot_directory <- "changepoint_slope_positive_varcheck_plots"
if (!dir.exists(plot_directory)) dir.create(plot_directory)

# Parameters
min_gap_months <- 1 # Minimum gap between detected points
base_threshold <- 0.001 # Fraction of range for first derivative
second_deriv_threshold <- 0.001 # Fraction of range for second derivative

for (i in 1:nrow(combinations)) {
  current_species <- combinations$SCIENTIFIC_NAME[i]
  current_country <- combinations$COUNTRY[i]
  
  message(paste("Processing", i, "of", nrow(combinations), ":", current_species, "in", current_country))
  
  # Filter data
  plot_data <- normalized_monthly_views_EASIN %>%
    filter(COUNTRY == current_country, SCIENTIFIC_NAME == current_species) %>%
    filter(!is.na(sum_normalized_views))
  
  analysis_data <- plot_data %>%
    filter(Internet_platform != "GBIF")
  
  if (nrow(plot_data) == 0) next
  invasion_year <- unique(plot_data$YEAR)[1]
  if (is.na(invasion_year)) next
  
  start_window_year <- invasion_year - 1
  end_window_year <- invasion_year
  
  # Initialize results
  case_result <- tibble(
    SCIENTIFIC_NAME = current_species,
    COUNTRY = current_country,
    invasion_year = invasion_year,
    detected_changepoints = "None",
    changepoint_in_window = FALSE,
    num_changepoints = 0,
    classification = "False Negative"
  )
  
  plot_pred <- NULL
  blocks_df <- tibble(xmin = as.POSIXct(character()), xmax = as.POSIXct(character()))
  
  # 1. GAM 1st and 2nd derivate changepoint detection
  
  tryCatch({
    if (nrow(analysis_data) >= 24) {
      # Prepare data
      analysis_data <- analysis_data %>%
        arrange(month_year) %>%
        mutate(month_idx = 1:n())
      
      month_count <- nrow(analysis_data)
      k_use <- min(9, month_count - 1) #maximum of k = 9 for the nine years of data covered (2016 - 2025)
      
      # Fit GAM
      gam_fit <- gam(sum_normalized_views ~ s(month_idx, k = k_use),
                     data = analysis_data, method = "REML")
      
      # Prediction
      pred_grid <- data.frame(month_idx = 1:nrow(analysis_data))
      pred_grid$month_year <- analysis_data$month_year
      pred_grid$fit <- predict(gam_fit, newdata = pred_grid)
      
      # First derivative (slope)
      pred_grid$derivative <- c(NA, diff(pred_grid$fit))
      pred_grid$derivative_smooth <- stats::filter(pred_grid$derivative, rep(1/3, 3), sides = 2)
      
      # Second derivative (acceleration)
      pred_grid$second_derivative <- c(NA, diff(pred_grid$derivative_smooth))
      pred_grid$second_derivative_smooth <- stats::filter(pred_grid$second_derivative, rep(1/3, 3), sides = 2)
      
      # Thresholds
      slope_threshold <- base_threshold * (max(pred_grid$fit, na.rm = TRUE) - min(pred_grid$fit, na.rm = TRUE))
      accel_threshold <- second_deriv_threshold * (max(pred_grid$fit, na.rm = TRUE) - min(pred_grid$fit, na.rm = TRUE))
      
      # Candidate changepoints: positive slope OR acceleration
      changepoint_flags <- (pred_grid$derivative_smooth > slope_threshold) | #or increasing first derivate (slope)
        (pred_grid$second_derivative_smooth > accel_threshold) #or increasing second derivatve (acceleration)
      changepoints_idx <- which(changepoint_flags)
      
      if(length(changepoints_idx) > 0){
        # Group contiguous detections into blocks
        diffs <- c(Inf, diff(changepoints_idx))
        group_id <- cumsum(diffs > min_gap_months)
        
        changepoint_blocks <- lapply(unique(group_id), function(g){
          idxs <- changepoints_idx[group_id == g]
          peak_deriv <- max(pred_grid$derivative_smooth[idxs], na.rm = TRUE)
          list(
            idxs = idxs,
            xmin = min(pred_grid$month_year[idxs]),
            xmax = max(pred_grid$month_year[idxs]),
            strength = peak_deriv
          )
        })
        
        # Keep only the strongest block
        first_block <- changepoint_blocks[[1]]
        strongest_block <- first_block # Continue using 'strongest_block' variable name        
        # 2.Check for increasing variance in the raw data
        
        # Create a new dataframe for variance analysis
        variance_data <- analysis_data
        
        # Calculate moving standard deviation of the raw normalized sums
        window_size <- 2 # 2-month window
        variance_data$local_sd <- rollapply(variance_data$sum_normalized_views, 
                                            width = window_size, 
                                            FUN = sd, 
                                            align = "center", 
                                            fill = NA)
        
        # Check if variance is increasing within the strongest block
        strongest_block_data <- variance_data %>% 
          filter(month_year >= strongest_block$xmin & month_year <= strongest_block$xmax)
        
        is_variance_increasing <- FALSE
        if(nrow(strongest_block_data) > 2 && !all(is.na(strongest_block_data$local_sd))) {
          # Use linear regression to check for a positive slope in the local variance
          var_lm <- lm(local_sd ~ month_idx, data = strongest_block_data)
          # Check for a positive coefficient
          if(summary(var_lm)$coefficients["month_idx", "Estimate"] > 0) {
            is_variance_increasing <- TRUE
          }
        }
        
        # Conditional logic to decide if we highlight the block
        if (is_variance_increasing) {
          # Update indices & block data ONLY IF variance is increasing
          changepoints_idx <- strongest_block$idxs
          blocks_df <- tibble(
            xmin = strongest_block$xmin,
            xmax = strongest_block$xmax
          )
          
          # Update results to reflect the detected point
          changepoint_dates <- pred_grid$month_year[changepoints_idx]
          changepoint_years <- year(changepoint_dates)
          in_window <- changepoint_years >= start_window_year & changepoint_years <= end_window_year
          
          case_result$detected_changepoints <- paste(changepoint_dates, collapse = ", ")
          case_result$num_changepoints <- 1
          case_result$classification <- ifelse(any(in_window), "True Positive", "False Positive")
          case_result$changepoint_in_window <- any(in_window)
        } else {
          # If variance is not increasing, treat it as a false negative and do not highlight
          case_result$classification <- "False Negative"
        }
      }
      
      plot_pred <- pred_grid
    }
  }, error = function(e) {
    problematic_cpt_combos[[length(problematic_cpt_combos) + 1]] <<- list(
      sci_name = current_species,
      country = current_country,
      error_message = e$message
    )
    message(paste0("  ERROR: ", e$message))
  })
  
  # Plot preparation
  shading_df <- data.frame(
    xmin = as.POSIXct(paste0(start_window_year, "-01-01")),
    xmax = as.POSIXct(paste0(end_window_year, "-12-31")),
    ymin = -Inf,
    ymax = Inf
  )
  
  p <- ggplot(plot_data, aes(x = month_year, y = normalized_views)) +
    # Highlight invasion window
    geom_rect(data = shading_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.15, inherit.aes = FALSE)
  
  # Plot the changepoint block only if it exists (i.e., if blocks_df is not empty)
  if(nrow(blocks_df) > 0) {
    p <- p + geom_rect(data = blocks_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                       fill = "blue", alpha = 0.15, inherit.aes = FALSE)
  }
  
  p <- p + geom_line(data = plot_pred, aes(x = month_year, y = fit),
                     color = "darkgrey", linewidth = 1) +
    geom_point(aes(group = Internet_platform, fill = Internet_platform),
               color = "black", shape = 21, size = 3) +
    scale_fill_manual(values = platform_colors) +
    labs(
      title = paste("Classification:", case_result$classification,
                    "-", current_species, "in", current_country),
      subtitle = paste("Invasion Year:", invasion_year),
      x = "Date",
      y = "Normalized activity"
    ) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black", face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold")
    )
  
  filename <- paste0(plot_directory, "/", gsub(" ", "_", current_species),
                     "_", current_country, ".png")
  tryCatch({
    ggsave(filename, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
  }, error = function(e){
    message("Failed to save plot for ", current_species, " in ", current_country, ": ", e$message)
  })
  
  results_list[[length(results_list) + 1]] <- case_result
}
```

    ## Processing 1 of 112 : Heracleum mantegazzianum in BG

    ## Processing 2 of 112 : Heracleum sosnowskyi in CZ

    ## Processing 3 of 112 : Myocastor coypus in HU

    ## Processing 4 of 112 : Myocastor coypus in LT

    ## Processing 5 of 112 : Nyctereutes procyonoides in LU

    ## Processing 6 of 112 : Procambarus virginalis in AT

    ## Processing 7 of 112 : Procambarus virginalis in BE

    ## Processing 8 of 112 : Procambarus virginalis in FR

    ## Processing 9 of 112 : Sciurus carolinensis in DE

    ## Processing 10 of 112 : Solenopsis invicta in IT

    ## Processing 11 of 112 : Vespa velutina in AT

    ## Processing 12 of 112 : Vespa velutina in CZ

    ## Processing 13 of 112 : Vespa velutina in NL

    ## Processing 14 of 112 : Vespa velutina in GB

    ## Processing 15 of 112 : Wasmannia auropunctata in FR

    ## Processing 16 of 112 : Pontederia crassipes in PL

    ## Processing 17 of 112 : Heracleum sosnowskyi in BE

    ## Processing 18 of 112 : Heracleum sosnowskyi in SK

    ## Processing 19 of 112 : Heracleum sosnowskyi in SE

    ## Processing 20 of 112 : Pistia stratiotes in PL

    ## Processing 21 of 112 : Procambarus clarkii in PL

    ## Processing 22 of 112 : Sciurus carolinensis in FR

    ## Processing 23 of 112 : Triadica sebifera in DE

    ## Processing 24 of 112 : Vespa velutina in HU

    ## Processing 25 of 112 : Vespa velutina in IE

    ## Processing 26 of 112 : Acacia saligna in DK

    ## Processing 27 of 112 : Acridotheres tristis in CY

    ## Processing 28 of 112 : Acridotheres tristis in GR

    ## Processing 29 of 112 : Alopochen aegyptiaca in LT

    ## Processing 30 of 112 : Ameiurus melas in AT

    ## Processing 31 of 112 : Ameiurus melas in BG

    ## Processing 32 of 112 : Asclepias syriaca in EE

    ## Processing 33 of 112 : Asclepias syriaca in ES

    ## Processing 34 of 112 : Asclepias syriaca in FI

    ## Processing 35 of 112 : Asclepias syriaca in LV

    ## Processing 36 of 112 : Cabomba caroliniana in AT

    ## Processing 37 of 112 : Celastrus orbiculatus in SK

    ## Processing 38 of 112 : Cortaderia jubata in BE

    ## Processing 39 of 112 : Cortaderia jubata in ES

    ## Processing 40 of 112 : Pontederia crassipes in MT

    ## Processing 41 of 112 : Elodea nuttallii in ES

    ## Processing 42 of 112 : Elodea nuttallii in LT

    ## Processing 43 of 112 : Eriocheir sinensis in SK

    ## Processing 44 of 112 : Faxonius limosus in EE

    ## Processing 45 of 112 : Faxonius rusticus in FR

    ## Processing 46 of 112 : Gunnera tinctoria in DK

    ## Processing 47 of 112 : Gunnera tinctoria in LU

    ## Processing 48 of 112 : Gymnocoronis spilanthoides in FR

    ## Processing 49 of 112 : Gymnocoronis spilanthoides in NL

    ## Processing 50 of 112 : Hakea sericea in BE

    ## Processing 51 of 112 : Heracleum mantegazzianum in GR

    ## Processing 52 of 112 : Heracleum mantegazzianum in LT

    ## Processing 53 of 112 : Heracleum mantegazzianum in RO

    ## Processing 54 of 112 : Heracleum sosnowskyi in BG

    ## Processing 55 of 112 : Heracleum sosnowskyi in NL

    ## Processing 56 of 112 : Heracleum sosnowskyi in PT

    ## Processing 57 of 112 : Humulus scandens in CZ

    ## Processing 58 of 112 : Humulus scandens in HR

    ## Processing 59 of 112 : Hydrocotyle ranunculoides in DK

    ## Processing 60 of 112 : Hydrocotyle ranunculoides in PT

    ## Processing 61 of 112 : Impatiens glandulifera in GR

    ## Processing 62 of 112 : Koenigia polystachya in SI

    ## Processing 63 of 112 : Lampropeltis getula in IT

    ## Processing 64 of 112 : Lespedeza cuneata in BE

    ## Processing 65 of 112 : Ludwigia grandiflora in AT

    ## Processing 66 of 112 : Ludwigia grandiflora in FI

    ## Processing 67 of 112 : Ludwigia grandiflora in PT

    ## Processing 68 of 112 : Ludwigia peploides in HR

    ## Processing 69 of 112 : Ludwigia peploides in HU

    ## Processing 70 of 112 : Ludwigia peploides in RO

    ## Processing 71 of 112 : Lygodium japonicum in SE

    ## Processing 72 of 112 : Lysichiton americanus in CZ

    ## Processing 73 of 112 : Lysichiton americanus in LU

    ## Processing 74 of 112 : Lysichiton americanus in PL

    ## Processing 75 of 112 : Microstegium vimineum in BE

    ## Processing 76 of 112 : Muntiacus reevesi in AT

    ## Processing 77 of 112 : Myocastor coypus in LV

    ## Processing 78 of 112 : Myriophyllum heterophyllum in SE

    ## Processing 79 of 112 : Nasua nasua in BE

    ## Processing 80 of 112 : Nasua nasua in DE

    ## Processing 81 of 112 : Nasua nasua in DK

    ## Processing 82 of 112 : Nasua nasua in HU

    ## Processing 83 of 112 : Pacifastacus leniusculus in MT

    ## Processing 84 of 112 : Parthenium hysterophorus in FR

    ## Processing 85 of 112 : Parthenium hysterophorus in RO

    ## Processing 86 of 112 : Cenchrus setaceus in BE

    ## Processing 87 of 112 : Cenchrus setaceus in CY

    ## Processing 88 of 112 : Cenchrus setaceus in DE

    ## Processing 89 of 112 : Cenchrus setaceus in LU

    ## Processing 90 of 112 : Perccottus glenii in CZ

    ## Processing 91 of 112 : Persicaria perfoliata in NL

    ## Processing 92 of 112 : Pistia stratiotes in FI

    ## Processing 93 of 112 : Pistia stratiotes in HR

    ## Processing 94 of 112 : Plotosus lineatus in CY

    ## Processing 95 of 112 : Procambarus clarkii in GR

    ## Processing 96 of 112 : Procambarus clarkii in MT

    ## Processing 97 of 112 : Procambarus virginalis in EE

    ## Processing 98 of 112 : Procambarus virginalis in ES

    ## Processing 99 of 112 : Procambarus virginalis in MT

    ## Processing 100 of 112 : Procambarus virginalis in RO

    ## Processing 101 of 112 : Neltuma juliflora in BE

    ## Processing 102 of 112 : Neltuma juliflora in HU

    ## Processing 103 of 112 : Rugulopteryx okamurae in IT

    ## Processing 104 of 112 : Salvinia molesta in GB

    ## Processing 105 of 112 : Threskiornis aethiopicus in CZ

    ## Processing 106 of 112 : Threskiornis aethiopicus in DE

    ## Processing 107 of 112 : Trachemys scripta in CY

    ## Processing 108 of 112 : Trachemys scripta in SK

    ## Processing 109 of 112 : Triadica sebifera in PT

    ## Processing 110 of 112 : Vespa velutina in LU

    ## Processing 111 of 112 : Wasmannia auropunctata in CY

    ## Processing 112 of 112 : Xenopus laevis in NL

``` r
# Combine results
final_results_df <- bind_rows(results_list)
write.csv(final_results_df, "gam_positive_changepoints_varcheck.csv", row.names = FALSE)
cat("\nFinal results saved to 'gam_positive_changepoints_varcheck.csv'.\n")
```

    ## 
    ## Final results saved to 'gam_positive_changepoints_varcheck.csv'.

``` r
# Print problematic combos
if(length(problematic_cpt_combos) > 0){
  cat("\nThe following combinations caused errors:\n")
  for(problem in problematic_cpt_combos){
    cat(paste0(" - ", problem$sci_name, " in ", problem$country, ": ", problem$error_message, "\n"))
  }
}
```

## 10.2 Changepoint classification and sensitivity analysis

``` r
#read dataframe
df_classification <- data.table::fread("gam_positive_changepoints_varcheck.csv")
glimpse(df_classification)
```

    ## Rows: 112
    ## Columns: 7
    ## $ SCIENTIFIC_NAME       <chr> "Heracleum mantegazzianum", "Heracleum sosnowsky…
    ## $ COUNTRY               <chr> "BG", "CZ", "HU", "LT", "LU", "AT", "BE", "FR", …
    ## $ invasion_year         <int> 2017, 2016, 2017, 2020, 2021, 2018, 2020, 2019, …
    ## $ detected_changepoints <chr> "2017-07-01, 2017-07-01, 2017-07-01, 2017-07-01,…
    ## $ changepoint_in_window <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRU…
    ## $ num_changepoints      <int> 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ classification        <chr> "True Positive", "True Positive", "False Negativ…

``` r
#calculate percentages
df_classification_total <- df_classification %>%
  group_by(classification) %>%
  summarise(prop_classification = n() / nrow(.))

df_classification_total
```

    ## # A tibble: 3 × 2
    ##   classification prop_classification
    ##   <chr>                        <dbl>
    ## 1 False Negative               0.241
    ## 2 False Positive               0.312
    ## 3 True Positive                0.446

``` r
#make the plot
p_sensitivity =ggplot(df_classification_total) +
  geom_bar(aes(x=classification, y= prop_classification*100,fill=classification),stat="identity") +
  scale_fill_manual(values = c("False Positive"="#ff6961", "False Negative"="#fdfd96", "True Positive"="#77dd77")) +
  labs(title = "Summary of detections (N = 112)", x = "Classification", y = "Percentage of detections (%)",fill="Classification") +
   theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(color = "black"),
      axis.title = element_text(color = "black", face = "bold"),
      plot.title = element_text(face = "bold"),
      legend.title = element_text(face = "bold"))
print(p_sensitivity)
```

![](iEcology_data_analysis_part2_files/figure-gfm/changepoint%20sensitivity%20(i.e.%20true/false%20pos/neg%20plot)-1.png)<!-- -->

``` r
#save the plot
ggsave(file.path(popularity_plots_dir, "sensitivity_plot.png"), p_sensitivity,
          bg = "white",
          width = 6,  # You can adjust these dimensions as needed
          height = 4,
          dpi = 300)

#join with species traits for further analysis

df_classified_with_traits <- df_classification %>%
  left_join(union_traits, by = "SCIENTIFIC_NAME")

glimpse(df_classified_with_traits)
```

    ## Rows: 112
    ## Columns: 9
    ## $ SCIENTIFIC_NAME       <chr> "Heracleum mantegazzianum", "Heracleum sosnowsky…
    ## $ COUNTRY               <chr> "BG", "CZ", "HU", "LT", "LU", "AT", "BE", "FR", …
    ## $ invasion_year         <int> 2017, 2016, 2017, 2020, 2021, 2018, 2020, 2019, …
    ## $ detected_changepoints <chr> "2017-07-01, 2017-07-01, 2017-07-01, 2017-07-01,…
    ## $ changepoint_in_window <lgl> TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRU…
    ## $ num_changepoints      <int> 1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, …
    ## $ classification        <chr> "True Positive", "True Positive", "False Negativ…
    ## $ Group                 <chr> "Plantae", "Plantae", "Mammalia", "Mammalia", "M…
    ## $ Habitat               <chr> "Terrestrial", "Terrestrial", "Both", "Both", "T…

``` r
N_total <- nrow(df_classified_with_traits)

# --- Calculate Classification Rates by Group ---
# Define the total number of rows for plot title (N = 112 from your glimpse)
N_total <- nrow(df_classified_with_traits)

# --- 1. Calculate Classification Rates by Group ---
df_group_rates <- df_classified_with_traits %>%
  # Group by the trait 'Group' and the outcome 'classification'
  group_by(Group, classification) %>%
  summarise(n = n(), .groups = 'drop') %>%
  # Group only by 'Group' now to calculate percentage within each group
  group_by(Group) %>%
  mutate(
    prop_classification = n / sum(n),
    # Calculate the number of observations in this group for labels
    Group_N = sum(n)
  ) %>%
  ungroup() %>%
  mutate(
    Group_Label = paste0(Group, "\n(N=", Group_N, ")")
  )

# View the calculated rates (optional)
glimpse(df_group_rates)
```

    ## Rows: 17
    ## Columns: 6
    ## $ Group               <chr> "Amphibia", "Aves", "Aves", "Crustacea", "Crustace…
    ## $ classification      <chr> "False Positive", "False Positive", "True Positive…
    ## $ n                   <int> 1, 2, 3, 1, 3, 10, 9, 1, 2, 6, 3, 4, 23, 14, 27, 1…
    ## $ prop_classification <dbl> 1.00000000, 0.40000000, 0.60000000, 0.07142857, 0.…
    ## $ Group_N             <int> 1, 5, 5, 14, 14, 14, 10, 10, 11, 11, 11, 4, 64, 64…
    ## $ Group_Label         <chr> "Amphibia\n(N=1)", "Aves\n(N=5)", "Aves\n(N=5)", "…

``` r
# --- 2. Plot Classification by Group ---
p_group_sensitivity <- ggplot(df_group_rates, aes(x = classification, y = prop_classification * 100, fill = classification)) +
  geom_bar(stat = "identity") +
  
  # UPDATED: Use the new label column in facet_wrap
  facet_wrap(~ Group_Label, scales = "free_x") +
  
  # Use the specified color palette
  scale_fill_manual(values = c("False Positive" = "#ff6961", "False Negative" = "#fdfd96", "True Positive" = "#77dd77")) +
  
  labs(
    title = paste("Classification rates by taxonomic group (Total N =", N_total, ")"), 
    x = "Classification", 
    y = "Percentage of detections (%)",
    fill = "Classification"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"), # Only X-axis line needed for minimal theme
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    # Customize facet strip appearance
    strip.background = element_rect(fill = "#e0e0e0", color = "black"),
    strip.text = element_text(face = "bold", color = "black")
  )

print(p_group_sensitivity)
```

![](iEcology_data_analysis_part2_files/figure-gfm/changepoint%20sensitivity%20(i.e.%20true/false%20pos/neg%20plot)-2.png)<!-- -->

``` r
# Save the plot (Update the directory path as needed)
ggsave(file.path(popularity_plots_dir, "sensitivity_by_group_plot.png"), p_group_sensitivity,
        bg = "white",
        width = 12,
        height = 7,
        dpi = 300)

# --- 3. Calculate Classification Rates by Habitat ---
df_habitat_rates <- df_classified_with_traits %>%
  # Group by the trait 'Habitat' and the outcome 'classification'
  group_by(Habitat, classification) %>%
  summarise(n = n(), .groups = 'drop') %>%
  # Group only by 'Habitat' now to calculate percentage within each habitat
  group_by(Habitat) %>%
  mutate(
    prop_classification = n / sum(n),
    Habitat_N = sum(n)
  ) %>%
  ungroup() %>%
  # Create a new label combining Habitat name and its count (N)
  mutate(
    Habitat_Label = paste0(Habitat, "\n(N=", Habitat_N, ")")
  )

# View the calculated rates (optional)
glimpse(df_habitat_rates)
```

    ## Rows: 9
    ## Columns: 6
    ## $ Habitat             <chr> "Aquatic", "Aquatic", "Aquatic", "Both", "Both", "…
    ## $ classification      <chr> "False Negative", "False Positive", "True Positive…
    ## $ n                   <int> 8, 7, 23, 2, 2, 4, 17, 26, 23
    ## $ prop_classification <dbl> 0.2105263, 0.1842105, 0.6052632, 0.2500000, 0.2500…
    ## $ Habitat_N           <int> 38, 38, 38, 8, 8, 8, 66, 66, 66
    ## $ Habitat_Label       <chr> "Aquatic\n(N=38)", "Aquatic\n(N=38)", "Aquatic\n(N…

``` r
# --- 4. Plot Classification by Habitat ---
p_habitat_sensitivity <- ggplot(df_habitat_rates, aes(x = classification, y = prop_classification * 100, fill = classification)) +
  geom_bar(stat = "identity") +
  
  # Use facet_wrap to create separate plots for each Habitat, using the new label
  facet_wrap(~ Habitat_Label, scales = "free_x") +
  
  # Use the specified color palette
  scale_fill_manual(values = c("False Positive" = "#ff6961", "False Negative" = "#fdfd96", "True Positive" = "#77dd77")) +
  
  labs(
    title = paste("Classification rates by habitat type (Total N =", N_total, ")"), 
    x = "Classification", 
    y = "Percentage of detections (%)",
    fill = "Classification"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    # Customize facet strip appearance
    strip.background = element_rect(fill = "#e0e0e0", color = "black"),
    strip.text = element_text(face = "bold", color = "black")
  )

print(p_habitat_sensitivity)
```

![](iEcology_data_analysis_part2_files/figure-gfm/changepoint%20sensitivity%20(i.e.%20true/false%20pos/neg%20plot)-3.png)<!-- -->

``` r
# Save the plot (Update the directory path as needed)
ggsave(file.path(popularity_plots_dir, "sensitivity_by_habitat_plot.png"), p_habitat_sensitivity,
        bg = "white",
        width = 10,
        height = 5,
        dpi = 300)
```

## 10.3. Check for the best thresholds minimizing false positive rate

``` r
# 1. The run_detection function to store as function object

run_detection <- function(analysis_data, invasion_year, slope_thresh, second_deriv_thresh) {
  result <- "False Negative" # Default classification
  
  if (nrow(analysis_data) >= 24) {
    analysis_data <- analysis_data %>%
      dplyr::arrange(month_year) %>%
      dplyr::mutate(month_idx = 1:n())
    
    month_count <- nrow(analysis_data)
    k_use <- min(9, month_count - 1)
    
    # Fit GAM
    gam_fit <- mgcv::gam(sum_normalized_views ~ s(month_idx, k = k_use),
                         data = analysis_data, method = "REML")
    
    # Predict and calculate derivatives
    pred_grid <- data.frame(month_idx = 1:nrow(analysis_data))
    pred_grid$month_year <- analysis_data$month_year
    pred_grid$fit <- predict(gam_fit, newdata = pred_grid)
    pred_grid$derivative <- c(NA, diff(pred_grid$fit))
    pred_grid$derivative_smooth <- stats::filter(pred_grid$derivative, rep(1/3, 3), sides = 2)
    pred_grid$second_derivative <- c(NA, diff(pred_grid$derivative_smooth))
    pred_grid$second_derivative_smooth <- stats::filter(pred_grid$second_derivative, rep(1/3, 3), sides = 2)
    
    # Apply thresholds based on range
    slope_threshold_value <- slope_thresh * (max(pred_grid$fit, na.rm = TRUE) - min(pred_grid$fit, na.rm = TRUE))
    accel_threshold_value <- second_deriv_thresh * (max(pred_grid$fit, na.rm = TRUE) - min(pred_grid$fit, na.rm = TRUE))
    
    changepoint_flags <- (pred_grid$derivative_smooth > slope_threshold_value) | (pred_grid$second_derivative_smooth > accel_threshold_value)
    changepoints_idx <- which(changepoint_flags)
    
    if (length(changepoints_idx) > 0) {
      # Group contiguous detections and find the strongest block
      diffs <- c(Inf, diff(changepoints_idx))
      group_id <- cumsum(diffs > 1)
      changepoint_blocks <- lapply(unique(group_id), function(g){
        idxs <- changepoints_idx[group_id == g]
        peak_deriv <- max(pred_grid$derivative_smooth[idxs], na.rm = TRUE)
        list(idxs = idxs, xmin = min(pred_grid$month_year[idxs]), xmax = max(pred_grid$month_year[idxs]), strength = peak_deriv)
      })
      strongest_block <- changepoint_blocks[[which.max(sapply(changepoint_blocks, `[[`, "strength"))]]
      
      # Perform the variance check
      variance_data <- analysis_data
      window_size <- 2
      variance_data$local_sd <- zoo::rollapply(variance_data$sum_normalized_views, width = window_size, FUN = sd, align = "center", fill = NA)
      strongest_block_data <- variance_data %>% dplyr::filter(month_year >= strongest_block$xmin & month_year <= strongest_block$xmax)
      
      is_variance_increasing <- FALSE
      if (nrow(strongest_block_data) > 2 && !all(is.na(strongest_block_data$local_sd))) {
        var_lm <- lm(local_sd ~ month_idx, data = strongest_block_data)
        if (summary(var_lm)$coefficients["month_idx", "Estimate"] > 0) {
          is_variance_increasing <- TRUE
        }
      }
      
      # Classify based on detection and variance check
      if (is_variance_increasing) {
        changepoint_dates <- pred_grid$month_year[strongest_block$idxs]
        changepoint_years <- lubridate::year(changepoint_dates)
        start_window_year <- invasion_year - 1
        end_window_year <- invasion_year
        in_window <- changepoint_years >= start_window_year & changepoint_years <= end_window_year
        
        result <- ifelse(any(in_window), "True Positive", "False Positive")
      }
    }
  }
  return(result)
}


normalized_monthly_views_EASIN$invasion_year = normalized_monthly_views_EASIN$YEAR
```

``` r
# 1. The run_detection function to store as function object
run_detection <- function(analysis_data, invasion_year, slope_thresh, second_deriv_thresh) {
  result <- "False Negative" # Default classification
  
  if (nrow(analysis_data) >= 24) {
    analysis_data <- analysis_data %>%
      dplyr::arrange(month_year) %>%
      dplyr::mutate(month_idx = 1:n())
    
    month_count <- nrow(analysis_data)
    k_use <- min(9, month_count - 1)
    
    # Fit GAM
    gam_fit <- mgcv::gam(sum_normalized_views ~ s(month_idx, k = k_use),
                         data = analysis_data, method = "REML")
    
    # Predict and calculate derivatives
    pred_grid <- data.frame(month_idx = 1:nrow(analysis_data))
    pred_grid$month_year <- analysis_data$month_year
    pred_grid$fit <- predict(gam_fit, newdata = pred_grid)
    pred_grid$derivative <- c(NA, diff(pred_grid$fit))
    pred_grid$derivative_smooth <- stats::filter(pred_grid$derivative, rep(1/3, 3), sides = 2)
    pred_grid$second_derivative <- c(NA, diff(pred_grid$derivative_smooth))
    pred_grid$second_derivative_smooth <- stats::filter(pred_grid$second_derivative, rep(1/3, 3), sides = 2)
    
    # Apply thresholds based on range
    slope_threshold_value <- slope_thresh * (max(pred_grid$fit, na.rm = TRUE) - min(pred_grid$fit, na.rm = TRUE))
    accel_threshold_value <- second_deriv_thresh * (max(pred_grid$fit, na.rm = TRUE) - min(pred_grid$fit, na.rm = TRUE))
    
    changepoint_flags <- (pred_grid$derivative_smooth > slope_threshold_value) | (pred_grid$second_derivative_smooth > accel_threshold_value)
    changepoints_idx <- which(changepoint_flags)
    
    if (length(changepoints_idx) > 0) {
      # Group contiguous detections
      diffs <- c(Inf, diff(changepoints_idx))
      group_id <- cumsum(diffs > 1)
      changepoint_blocks <- lapply(unique(group_id), function(g){
        idxs <- changepoints_idx[group_id == g]
        peak_deriv <- max(pred_grid$derivative_smooth[idxs], na.rm = TRUE)
        list(idxs = idxs, xmin = min(pred_grid$month_year[idxs]), xmax = max(pred_grid$month_year[idxs]), strength = peak_deriv)
      })
      # >>> MODIFICATION: Select the FIRST block instead of the strongest <<<
      first_block <- changepoint_blocks[[1]]
      block_to_check <- first_block
      
      # Perform the variance check
      variance_data <- analysis_data
      window_size <- 2
      variance_data$local_sd <- zoo::rollapply(variance_data$sum_normalized_views, width = window_size, FUN = sd, align = "center", fill = NA)
      block_data <- variance_data %>% dplyr::filter(month_year >= block_to_check$xmin & month_year <= block_to_check$xmax)
      
      is_variance_increasing <- FALSE
      if (nrow(block_data) > 2 && !all(is.na(block_data$local_sd))) {
        var_lm <- lm(local_sd ~ month_idx, data = block_data)
        if (summary(var_lm)$coefficients["month_idx", "Estimate"] > 0) {
          is_variance_increasing <- TRUE
        }
      }
      
      # Classify based on detection and variance check
      if (is_variance_increasing) {
        changepoint_dates <- pred_grid$month_year[block_to_check$idxs]
        changepoint_years <- lubridate::year(changepoint_dates)
        start_window_year <- invasion_year - 1
        end_window_year <- invasion_year
        in_window <- changepoint_years >= start_window_year & changepoint_years <= end_window_year
        
        result <- ifelse(any(in_window), "True Positive", "False Positive")
      }
    }
  }
  return(result)
}

# The line below ensures 'invasion_year' is available for the split function
normalized_monthly_views_EASIN$invasion_year = normalized_monthly_views_EASIN$YEAR
```

This part of the script orchestrates the entire process. It sets up the
data, defines the grid of thresholds, loops through them, and calculates
the final performance metrics. It then identifies the best-performing
threshold combination based on your specified criteria (highest true
positive rate with a low false positive rate).

``` r
# Load necessary libraries
library(mgcv)
library(dplyr)
library(lubridate)
library(zoo)

# Assuming normalized_monthly_views_EASIN is your time series data
# This script assumes it has columns: SCIENTIFIC_NAME, COUNTRY, month_year, sum_normalized_views, invasion_year

# Step 1: Prepare the 'all_cases' list
all_cases <- split(normalized_monthly_views_EASIN,
                   list(normalized_monthly_views_EASIN$SCIENTIFIC_NAME, normalized_monthly_views_EASIN$COUNTRY),
                   drop = TRUE)

# Step 2: Define the grid of thresholds to test
slope_thresholds <- c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008)
second_deriv_thresholds <- c(0.001, 0.002, 0.003, 0.004, 0.005, 0.006, 0.007, 0.008)

# Initialize a list to store results
optimization_results <- list()

# Step 3: Run the grid search loop
for (slope_thresh in slope_thresholds) {
  for (second_deriv_thresh in second_deriv_thresholds) {
    message(paste("Testing thresholds: slope =", slope_thresh, ", second deriv =", second_deriv_thresh))
    
    tp_count <- 0
    fp_count <- 0
    fn_count <- 0
    
    # Iterate through each case
    for (case_name in names(all_cases)) {
      case_data <- all_cases[[case_name]]
      
      if (nrow(case_data) == 0 || all(is.na(case_data$invasion_year))) next
      
      invasion_year <- unique(case_data$invasion_year)[1]
      
      tryCatch({
        detection_result <- run_detection(case_data, invasion_year, slope_thresh, second_deriv_thresh)
        
        # Update confusion matrix counts
        if (detection_result == "True Positive") {
          tp_count <- tp_count + 1
        } else if (detection_result == "False Positive") {
          fp_count <- fp_count + 1
        } else {
          fn_count <- fn_count + 1
        }
      }, error = function(e) {
        message(paste("  Error processing", case_name, ":", e$message))
        # Count as False Negative on error, as it's a missed detection
        fn_count <- fn_count + 1
      })
    }
    
    # Step 4: Calculate performance metrics
    total_detections <- tp_count + fp_count
    total_actual_positives <- tp_count + fn_count
    
    true_positive_rate <- if (total_actual_positives > 0) tp_count / total_actual_positives else 0
    false_positive_rate <- if (total_detections > 0) fp_count / total_detections else 0
    
    # Store the results
    optimization_results[[length(optimization_results) + 1]] <- data.frame(
      slope_threshold = slope_thresh,
      second_deriv_threshold = second_deriv_thresh,
      TP_Rate = true_positive_rate,
      FP_Rate = false_positive_rate,
      TP_Count = tp_count,
      FP_Count = fp_count,
      FN_Count = fn_count,
      Total_Actual_Positives = total_actual_positives
    )
  }
}
```

    ## Testing thresholds: slope = 0.001 , second deriv = 0.001

    ## Testing thresholds: slope = 0.001 , second deriv = 0.002

    ## Testing thresholds: slope = 0.001 , second deriv = 0.003

    ## Testing thresholds: slope = 0.001 , second deriv = 0.004

    ## Testing thresholds: slope = 0.001 , second deriv = 0.005

    ## Testing thresholds: slope = 0.001 , second deriv = 0.006

    ## Testing thresholds: slope = 0.001 , second deriv = 0.007

    ## Testing thresholds: slope = 0.001 , second deriv = 0.008

    ## Testing thresholds: slope = 0.002 , second deriv = 0.001

    ## Testing thresholds: slope = 0.002 , second deriv = 0.002

    ## Testing thresholds: slope = 0.002 , second deriv = 0.003

    ## Testing thresholds: slope = 0.002 , second deriv = 0.004

    ## Testing thresholds: slope = 0.002 , second deriv = 0.005

    ## Testing thresholds: slope = 0.002 , second deriv = 0.006

    ## Testing thresholds: slope = 0.002 , second deriv = 0.007

    ## Testing thresholds: slope = 0.002 , second deriv = 0.008

    ## Testing thresholds: slope = 0.003 , second deriv = 0.001

    ## Testing thresholds: slope = 0.003 , second deriv = 0.002

    ## Testing thresholds: slope = 0.003 , second deriv = 0.003

    ## Testing thresholds: slope = 0.003 , second deriv = 0.004

    ## Testing thresholds: slope = 0.003 , second deriv = 0.005

    ## Testing thresholds: slope = 0.003 , second deriv = 0.006

    ## Testing thresholds: slope = 0.003 , second deriv = 0.007

    ## Testing thresholds: slope = 0.003 , second deriv = 0.008

    ## Testing thresholds: slope = 0.004 , second deriv = 0.001

    ## Testing thresholds: slope = 0.004 , second deriv = 0.002

    ## Testing thresholds: slope = 0.004 , second deriv = 0.003

    ## Testing thresholds: slope = 0.004 , second deriv = 0.004

    ## Testing thresholds: slope = 0.004 , second deriv = 0.005

    ## Testing thresholds: slope = 0.004 , second deriv = 0.006

    ## Testing thresholds: slope = 0.004 , second deriv = 0.007

    ## Testing thresholds: slope = 0.004 , second deriv = 0.008

    ## Testing thresholds: slope = 0.005 , second deriv = 0.001

    ## Testing thresholds: slope = 0.005 , second deriv = 0.002

    ## Testing thresholds: slope = 0.005 , second deriv = 0.003

    ## Testing thresholds: slope = 0.005 , second deriv = 0.004

    ## Testing thresholds: slope = 0.005 , second deriv = 0.005

    ## Testing thresholds: slope = 0.005 , second deriv = 0.006

    ## Testing thresholds: slope = 0.005 , second deriv = 0.007

    ## Testing thresholds: slope = 0.005 , second deriv = 0.008

    ## Testing thresholds: slope = 0.006 , second deriv = 0.001

    ## Testing thresholds: slope = 0.006 , second deriv = 0.002

    ## Testing thresholds: slope = 0.006 , second deriv = 0.003

    ## Testing thresholds: slope = 0.006 , second deriv = 0.004

    ## Testing thresholds: slope = 0.006 , second deriv = 0.005

    ## Testing thresholds: slope = 0.006 , second deriv = 0.006

    ## Testing thresholds: slope = 0.006 , second deriv = 0.007

    ## Testing thresholds: slope = 0.006 , second deriv = 0.008

    ## Testing thresholds: slope = 0.007 , second deriv = 0.001

    ## Testing thresholds: slope = 0.007 , second deriv = 0.002

    ## Testing thresholds: slope = 0.007 , second deriv = 0.003

    ## Testing thresholds: slope = 0.007 , second deriv = 0.004

    ## Testing thresholds: slope = 0.007 , second deriv = 0.005

    ## Testing thresholds: slope = 0.007 , second deriv = 0.006

    ## Testing thresholds: slope = 0.007 , second deriv = 0.007

    ## Testing thresholds: slope = 0.007 , second deriv = 0.008

    ## Testing thresholds: slope = 0.008 , second deriv = 0.001

    ## Testing thresholds: slope = 0.008 , second deriv = 0.002

    ## Testing thresholds: slope = 0.008 , second deriv = 0.003

    ## Testing thresholds: slope = 0.008 , second deriv = 0.004

    ## Testing thresholds: slope = 0.008 , second deriv = 0.005

    ## Testing thresholds: slope = 0.008 , second deriv = 0.006

    ## Testing thresholds: slope = 0.008 , second deriv = 0.007

    ## Testing thresholds: slope = 0.008 , second deriv = 0.008

``` r
# Step 5: Find the optimal thresholds
results_df <- do.call(rbind, optimization_results)

# A good balance is often a high TP rate with a low FP rate.
best_thresholds <- results_df %>%
  filter(TP_Rate > 0) %>% # Exclude combinations with no true positives
  arrange(desc(TP_Rate), FP_Rate) %>% # Prioritize high TP rate, then low FP rate
  head(1)

# Display results
print("Optimization Results:")
```

    ## [1] "Optimization Results:"

``` r
print(results_df)
```

    ##    slope_threshold second_deriv_threshold    TP_Rate   FP_Rate TP_Count
    ## 1            0.001                  0.001 0.64473684 0.4235294       49
    ## 2            0.001                  0.002 0.64473684 0.4235294       49
    ## 3            0.001                  0.003 0.64473684 0.4235294       49
    ## 4            0.001                  0.004 0.64473684 0.4235294       49
    ## 5            0.001                  0.005 0.64473684 0.4235294       49
    ## 6            0.001                  0.006 0.64473684 0.4235294       49
    ## 7            0.001                  0.007 0.64473684 0.4235294       49
    ## 8            0.001                  0.008 0.64473684 0.4235294       49
    ## 9            0.002                  0.001 0.48571429 0.5526316       34
    ## 10           0.002                  0.002 0.48571429 0.5526316       34
    ## 11           0.002                  0.003 0.48571429 0.5526316       34
    ## 12           0.002                  0.004 0.48571429 0.5526316       34
    ## 13           0.002                  0.005 0.48571429 0.5526316       34
    ## 14           0.002                  0.006 0.48571429 0.5526316       34
    ## 15           0.002                  0.007 0.48571429 0.5526316       34
    ## 16           0.002                  0.008 0.48571429 0.5526316       34
    ## 17           0.003                  0.001 0.40259740 0.5303030       31
    ## 18           0.003                  0.002 0.40259740 0.5303030       31
    ## 19           0.003                  0.003 0.40259740 0.5303030       31
    ## 20           0.003                  0.004 0.40259740 0.5303030       31
    ## 21           0.003                  0.005 0.40259740 0.5303030       31
    ## 22           0.003                  0.006 0.40259740 0.5303030       31
    ## 23           0.003                  0.007 0.40259740 0.5303030       31
    ## 24           0.003                  0.008 0.40259740 0.5303030       31
    ## 25           0.004                  0.001 0.22891566 0.6041667       19
    ## 26           0.004                  0.002 0.22891566 0.6041667       19
    ## 27           0.004                  0.003 0.22891566 0.6041667       19
    ## 28           0.004                  0.004 0.22891566 0.6041667       19
    ## 29           0.004                  0.005 0.22891566 0.6041667       19
    ## 30           0.004                  0.006 0.22891566 0.6041667       19
    ## 31           0.004                  0.007 0.22891566 0.6041667       19
    ## 32           0.004                  0.008 0.22891566 0.6041667       19
    ## 33           0.005                  0.001 0.18085106 0.5142857       17
    ## 34           0.005                  0.002 0.18085106 0.5142857       17
    ## 35           0.005                  0.003 0.18085106 0.5142857       17
    ## 36           0.005                  0.004 0.18085106 0.5142857       17
    ## 37           0.005                  0.005 0.18085106 0.5142857       17
    ## 38           0.005                  0.006 0.18085106 0.5142857       17
    ## 39           0.005                  0.007 0.18085106 0.5142857       17
    ## 40           0.005                  0.008 0.18085106 0.5142857       17
    ## 41           0.006                  0.001 0.10526316 0.6296296       10
    ## 42           0.006                  0.002 0.10526316 0.6296296       10
    ## 43           0.006                  0.003 0.10526316 0.6296296       10
    ## 44           0.006                  0.004 0.10526316 0.6296296       10
    ## 45           0.006                  0.005 0.10526316 0.6296296       10
    ## 46           0.006                  0.006 0.10526316 0.6296296       10
    ## 47           0.006                  0.007 0.10526316 0.6296296       10
    ## 48           0.006                  0.008 0.10526316 0.6296296       10
    ## 49           0.007                  0.001 0.06930693 0.6111111        7
    ## 50           0.007                  0.002 0.06930693 0.6111111        7
    ## 51           0.007                  0.003 0.06930693 0.6111111        7
    ## 52           0.007                  0.004 0.06930693 0.6111111        7
    ## 53           0.007                  0.005 0.06930693 0.6111111        7
    ## 54           0.007                  0.006 0.06930693 0.6111111        7
    ## 55           0.007                  0.007 0.06930693 0.6111111        7
    ## 56           0.007                  0.008 0.06930693 0.6111111        7
    ## 57           0.008                  0.001 0.03773585 0.6000000        4
    ## 58           0.008                  0.002 0.03773585 0.6000000        4
    ## 59           0.008                  0.003 0.03773585 0.6000000        4
    ## 60           0.008                  0.004 0.03773585 0.6000000        4
    ## 61           0.008                  0.005 0.03773585 0.6000000        4
    ## 62           0.008                  0.006 0.03773585 0.6000000        4
    ## 63           0.008                  0.007 0.03773585 0.6000000        4
    ## 64           0.008                  0.008 0.03773585 0.6000000        4
    ##    FP_Count FN_Count Total_Actual_Positives
    ## 1        36       27                     76
    ## 2        36       27                     76
    ## 3        36       27                     76
    ## 4        36       27                     76
    ## 5        36       27                     76
    ## 6        36       27                     76
    ## 7        36       27                     76
    ## 8        36       27                     76
    ## 9        42       36                     70
    ## 10       42       36                     70
    ## 11       42       36                     70
    ## 12       42       36                     70
    ## 13       42       36                     70
    ## 14       42       36                     70
    ## 15       42       36                     70
    ## 16       42       36                     70
    ## 17       35       46                     77
    ## 18       35       46                     77
    ## 19       35       46                     77
    ## 20       35       46                     77
    ## 21       35       46                     77
    ## 22       35       46                     77
    ## 23       35       46                     77
    ## 24       35       46                     77
    ## 25       29       64                     83
    ## 26       29       64                     83
    ## 27       29       64                     83
    ## 28       29       64                     83
    ## 29       29       64                     83
    ## 30       29       64                     83
    ## 31       29       64                     83
    ## 32       29       64                     83
    ## 33       18       77                     94
    ## 34       18       77                     94
    ## 35       18       77                     94
    ## 36       18       77                     94
    ## 37       18       77                     94
    ## 38       18       77                     94
    ## 39       18       77                     94
    ## 40       18       77                     94
    ## 41       17       85                     95
    ## 42       17       85                     95
    ## 43       17       85                     95
    ## 44       17       85                     95
    ## 45       17       85                     95
    ## 46       17       85                     95
    ## 47       17       85                     95
    ## 48       17       85                     95
    ## 49       11       94                    101
    ## 50       11       94                    101
    ## 51       11       94                    101
    ## 52       11       94                    101
    ## 53       11       94                    101
    ## 54       11       94                    101
    ## 55       11       94                    101
    ## 56       11       94                    101
    ## 57        6      102                    106
    ## 58        6      102                    106
    ## 59        6      102                    106
    ## 60        6      102                    106
    ## 61        6      102                    106
    ## 62        6      102                    106
    ## 63        6      102                    106
    ## 64        6      102                    106

``` r
print("\nBest Thresholds Found:")
```

    ## [1] "\nBest Thresholds Found:"

``` r
print(best_thresholds)
```

    ##   slope_threshold second_deriv_threshold   TP_Rate   FP_Rate TP_Count FP_Count
    ## 1           0.001                  0.001 0.6447368 0.4235294       49       36
    ##   FN_Count Total_Actual_Positives
    ## 1       27                     76

``` r
# Save the results
write.csv(results_df, "gam_threshold_optimization_results.csv", row.names = FALSE)
```

## 10.4 Example plot Sciurus carolinensis

    ## Processing: Sciurus carolinensis in FR

``` r
# --- Plot Generation ---

shading_df <- data.frame(
  xmin = as.POSIXct(paste0(start_window_year, "-01-01")),
  xmax = as.POSIXct(paste0(end_window_year, "-12-31")),
  ymin = -Inf,
  ymax = Inf
)

# Initialize the ggplot object
p_sciurus_fr <- ggplot(plot_data, aes(x = month_year, y = sum_normalized_views)) + # Changed y-axis to sum_normalized_views
  # Highlight invasion window
  geom_rect(data = shading_df, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "red", alpha = 0.15, inherit.aes = FALSE)

# Plot the changepoint block only if it exists
if(nrow(blocks_df) > 0) {
  p_sciurus_fr <- p_sciurus_fr + geom_rect(data = blocks_df, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
                     fill = "blue", alpha = 0.15, inherit.aes = FALSE)
}

p_sciurus_fr <- p_sciurus_fr + geom_line(data = plot_pred, aes(x = month_year, y = fit),
                   color = "darkgrey", linewidth = 1) +
  # Note: The original code uses 'normalized_views' for geom_point and 'sum_normalized_views' in the main plot_data..
  geom_point(data = plot_data, aes(x = month_year, y = normalized_views,
                                   group = Internet_platform, fill = Internet_platform),
             color = "black", shape = 21, size = 3, inherit.aes = FALSE) +
  # This requires 'platform_colors' to be defined
  scale_fill_manual(values = platform_colors,name="Internet platform") +
  labs(
    #title = paste("Classification:", case_result$classification,
    #              "-", current_species, "in", current_country),
    #subtitle = paste("Invasion Year:", invasion_year),
    x = "Date",
    y = "Normalized activity"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black", face = "bold"),
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.margin = unit(c(1, 2, 0.2, 0.2), "cm")
  )

# --- Return and Print the ggplot Object ---
print(p_sciurus_fr)
```

![](iEcology_data_analysis_part2_files/figure-gfm/example%20plot%20generation,%20can%20be%20shown-1.png)<!-- -->

``` r
#join with cowplot and print in 300 dpi

print(p_sensitivity)
```

![](iEcology_data_analysis_part2_files/figure-gfm/example%20plot%20generation,%20can%20be%20shown-2.png)<!-- -->

``` r
p_sensitivity=p_sensitivity+
  labs(x="Classification (N = 112)")+
  theme(plot.title = element_blank(),
        plot.margin = unit(c(1,0.2, 0.2, 0.2), "cm"))

sciurus_sens_p <- plot_grid(
  p_sciurus_fr,  # Your Sciurus carolinensis in France plot
  p_sensitivity, # Your sensitivity plot
  labels = c("(a)", "(b)"), # Add labels to the sub-plots
  label_size = 14,
  rel_widths = c(1.2,1)
)  

ggsave(file.path(popularity_plots_dir, "sciurus_sens_plot.png"), sciurus_sens_p,
          bg = "white",
          width = 12,  # You can adjust these dimensions as needed
          height = 4,
          dpi = 300)

print(sciurus_sens_p)
```

![](iEcology_data_analysis_part2_files/figure-gfm/example%20plot%20generation,%20can%20be%20shown-3.png)<!-- -->

## 10.5 Alternative detection calculation methods (NOT SHOWN)
