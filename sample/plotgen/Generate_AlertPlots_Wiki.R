# List of required packages
packages <- c("mgcv", "ggplot2", "dplyr", "readr", "tidyr", "cowplot", "grid")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
lapply(packages, library, character.only = TRUE)

# Load your data
df <- read.csv("species_pageviews_analysis_2022_present.csv")

# Convert to long format
df_long <- df %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "date", 
               values_to = "pageviews") %>%
  mutate(date = as.Date(sub("X", "", date), format="%Y%m%d"),
         doy = as.numeric(format(date, "%j")),  # Day of Year (DOY)
         month = factor(format(date, "%B"), #extract month
                        levels = c("January", "February", "March", "April", "May", "June", 
                                   "July", "August", "September", "October", "November", "December")),
         year = format(date, "%Y"),             # Extract year
         year_factor = factor(ifelse(year %in% c("2022", "2023", "2024"), "historical", "2025"), 
                              levels = c("historical", "2025")))  # Historical vs. 2025

# Check structure
str(df_long)

# Create a new folder for saving plots (if not exists)
if (!dir.exists("species_plots_wiki")) {
  dir.create("species_plots_wiki")
}

# Initialize an empty dataframe to store alert info
alert_info <- data.frame(species = character(), 
                         language = character(),
                         alert = logical(),
                         stringsAsFactors = FALSE)

# Set the starting index if you want to resume from a specific combination
start_index <- 1  # Change this value if you want to resume from a different row

# Loop through all species-language combinations
species_languages <- unique(df_long %>% select(Scientific.Name, Language))
head(species_languages)

for (i in start_index:nrow(species_languages)) {
  species_name <- species_languages$Scientific.Name[i]
  language <- species_languages$Language[i]
  
  # Filter data for the current species-language combination
  df_subset <- df_long %>%
    filter(Scientific.Name == species_name & Language == language)
  
  # Wrap model fitting and plotting in tryCatch to skip on error
  tryCatch({
    
    #check for ALL zero observations
    if (all(df_subset$observations == 0 | is.na(df_subset$observations))) {
      message(paste(Sys.time(), ": Skipping model for species", species_name, "in language", language, "because all observations are zero or NA."))
      next # Skip to the next iteration of the loop
    }
    
    # Count the number of non-zero and non-NA observations
    non_zero_non_na_count <- df_subset %>%
      filter(!is.na(observations) & observations != 0) %>%
      nrow()
    
    # Check if the count is less than 10
    if (non_zero_non_na_count < 10) {
      message(paste(Sys.time(), ": Skipping model for species", species_name, "in language", language, "because there are less than 10 non-zero and non-NA observations."))
      next # Skip to the next iteration of the loop
    }
    
    # Fit the specified GAM model
    gam_combined <- gam(pageviews ~ s(doy, k=52, m=2, bs="tp") +
                          s(doy, by=year_factor, k=52, m=1, bs="tp") +
                          s(year_factor, bs="re", k=2),
                        data=na.omit(df_subset),
                        family=poisson,
                        method="REML")
    
    # Predict on the link (log) scale and obtain standard errors
    pred <- predict(gam_combined, newdata = df_subset, type = "link", se.fit = TRUE)
    
    # Convert mean predictions to the response scale
    df_subset$fit    <- exp(pred$fit)
    
    # Calculate confidence intervals on the response scale using the delta method
    df_subset$lwr <- exp(pred$fit - 1.96 * pred$se.fit)
    df_subset$upr <- exp(pred$fit + 1.96 * pred$se.fit)
    
    # Historical data (e.g., 2022-2024)
    df_hist <- df_subset %>%
      filter(year_factor == "historical") %>%
      arrange(doy)
    
    # 2025 data
    df_2025 <- df_subset %>%
      filter(year_factor == "2025") %>%
      arrange(doy)
    
    # Aggregate historical data by doy to get the upper confidence interval per day
    hist_by_doy <- df_hist %>%
      group_by(doy) %>%
      summarize(hist_upr = max(upr, na.rm = TRUE))
    
    # Join the aggregated historical data to the 2025 data by doy *AND* create the color column
    df_2025 <- df_2025 %>%
      left_join(hist_by_doy, by = "doy") %>%
      mutate(color = if_else(lwr > hist_upr, "red", "darkgreen")) # Create color *here*
    
    
    # -------------------------
    # Build Segments for 2025 Fit and CI Lines
    # -------------------------
    # Now 'color' is available!
    
    df_segments_fit <- df_2025 %>%
      arrange(doy) %>%
      mutate(xend = lead(doy),
             yend = lead(fit),
             seg_color = color) %>%  # color is available
      filter(!is.na(xend))
    
    df_segments_lwr <- df_2025 %>%
      arrange(doy) %>%
      mutate(xend = lead(doy),
             yend = lead(lwr),
             seg_color = color) %>%  # color is available
      filter(!is.na(xend))
    
    df_segments_upr <- df_2025 %>%
      arrange(doy) %>%
      mutate(xend = lead(doy),
             yend = lead(upr),
             seg_color = color) %>%  # color is available
      filter(!is.na(xend))
    
    # Create the base plot (without legend annotations)
    base_plot <- ggplot() +
      # Historical observations (points) with different shapes for years
      geom_point(data = df_hist, 
                 aes(x = doy, y = pageviews, shape = as.factor(year)), 
                 color = "black", alpha = 0.4,size=0.8) +
      # Raw 2025 observations (points), colored as assigned by the join/comparison
      geom_point(data = df_2025, 
                 aes(x = doy, y = pageviews, color = color), 
                 size = 1, alpha = 0.6, show.legend = FALSE) +
      # Shaded confidence interval for historical data
      geom_ribbon(data = df_hist, 
                  aes(x = doy, ymin = lwr, ymax = upr), 
                  fill = "black", alpha = 0.2) +
      # Model fit for historical data (black line)
      geom_line(data = df_hist, 
                aes(x = doy, y = fit), 
                color = "black", size = 0.9,alpha=0.8) +
      # Dashed segments for CI intervals for 2025 data (lower and upper)
      geom_segment(data = df_segments_lwr, 
                   aes(x = doy, y = lwr, xend = xend, yend = yend, color = seg_color), 
                   linetype = "solid", alpha = 0.4, size = 0.9, show.legend = FALSE) +
      geom_segment(data = df_segments_upr, 
                   aes(x = doy, y = upr, xend = xend, yend = yend, color = seg_color), 
                   linetype = "solid", alpha = 0.4, size = 0.9, show.legend = FALSE) +
      # Segments for the 2025 fit line
      geom_segment(data = df_segments_fit, 
                   aes(x = doy, y = fit, xend = xend, yend = yend, color = seg_color), 
                   size = 1.2, show.legend = FALSE) +
      labs(title = paste("GAM Model for", species_name, "in", language),
           subtitle = "Black = Historical (2022–2024), Blue/Red = 2025 (available data)",
           x = "Day of Year (DOY)", y = "Pageviews") +
      # Manual color legend for 2025 (Normal = Blue, Alert = Red)
      scale_color_manual(values = c("darkgreen" = "darkgreen", "red" = "red"), 
                         labels = c("2025 (Normal)", "2025 (Alert)")) +
      # Manual shape legend for historical years
      scale_shape_manual(values = c(1, 2, 3), 
                         labels = c("2022", "2023", "2024")) +
      # Remove ggplot's automatic legend
      guides(color = guide_legend(title = "2025 Model Line")) +
      theme_bw() +
      theme(legend.position = "none",  # Remove auto legend
            plot.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    
    # Build the custom legend grob with white background
    legend_grob <- grid::grobTree(
      # White background rectangle (with no border)
      grid::rectGrob(x = 0, y = 0, width = 1, height = 1,
                     gp = grid::gpar(fill = "white", col = "white", lwd = 1), just = c("left", "bottom")),
      
      # 2022 legend item
      grid::pointsGrob(x = unit(0.05, "npc"), y = unit(0.75, "npc"), pch = 1, 
                       gp = grid::gpar(col = "black",cex=0.3)),
      grid::textGrob("2022", x = unit(0.15, "npc"), y = unit(0.75, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      
      # 2023 legend item
      grid::pointsGrob(x = unit(0.05, "npc"), y = unit(0.65, "npc"), pch = 2, 
                       gp = grid::gpar(col = "black",cex=0.3)),
      grid::textGrob("2023", x = unit(0.15, "npc"), y = unit(0.65, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      
      # 2024 legend item
      grid::pointsGrob(x = unit(0.05, "npc"), y = unit(0.55, "npc"), pch = 3, 
                       gp = grid::gpar(col = "black",cex=0.3)),
      grid::textGrob("2024", x = unit(0.15, "npc"), y = unit(0.55, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      
      # Mean Historical line (custom geom_line entry)
      grid::segmentsGrob(x0 = unit(0.05, "npc"), x1 = unit(0.10, "npc"), 
                         y0 = unit(0.45, "npc"), y1 = unit(0.45, "npc"), 
                         gp = grid::gpar(col = "black", lwd = 2)),
      grid::textGrob("Mean 2022–2024", x = unit(0.15, "npc"), 
                     y = unit(0.45, "npc"), just = "left", gp = grid::gpar(fontsize = 10)),
      
      # 2025 (Normal) legend item
      grid::pointsGrob(x = unit(0.075, "npc"), y = unit(0.35, "npc"), pch = 16, 
                       gp = grid::gpar(col = "darkgreen",cex=0.3)),
      
      # 2025 (Normal) legend item
      grid::segmentsGrob(x0 = unit(0.05, "npc"), x1 = unit(0.10, "npc"), 
                         y0 = unit(0.35, "npc"), y1 = unit(0.35, "npc"), 
                         gp = grid::gpar(col = "darkgreen", lwd = 2)),
      grid::textGrob("2025 (Normal)", x = unit(0.15, "npc"), y = unit(0.35, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      
      # 2025 (Normal) legend item
      grid::pointsGrob(x = unit(0.075, "npc"), y = unit(0.25, "npc"), pch = 16, 
                       gp = grid::gpar(col = "red",cex=0.3)),
      
      # 2025 (Alert) legend item
      grid::segmentsGrob(x0 = unit(0.05, "npc"), x1 = unit(0.10, "npc"), 
                         y0 = unit(0.25, "npc"), y1 = unit(0.25, "npc"), 
                         gp = grid::gpar(col = "red", lwd = 2)),
      grid::textGrob("2025 (Alert)", x = unit(0.15, "npc"), y = unit(0.25, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10))
      
  
    )
    
    # Create a separate legend plot from the legend grob
    legend_plot <- ggdraw() + draw_grob(legend_grob, x = 0, y = 0, width = 1, height = 1)
    
    # Combine the base plot and the legend plot side-by-side.
    final_plot <- plot_grid(base_plot, legend_plot, ncol = 2, rel_widths = c(1, 0.3))
    
    # Save the combined plot
    ggsave(paste0("species_plots_wiki/", species_name, "_", language, ".png"), final_plot, width = 8, height = 6)
    
    print(paste(Sys.time(), ": Plot generated for species", species_name, "in language", language))
    
    alert_info <- rbind(alert_info, data.frame(species = species_name, 
                                               language = language, 
                                               alert = tail(df_2025$color, n = 1) == "red"))
    
  }, error = function(e) {
    message(paste(Sys.time(), ": Skipping model for species", species_name, "in language", language, "due to error:", e$message))
  })
}

write.csv(alert_info, "species_alert_info_wiki.csv", row.names = FALSE)
