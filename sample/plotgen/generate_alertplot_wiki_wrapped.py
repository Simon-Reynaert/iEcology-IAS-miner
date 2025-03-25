import os

# Set R_HOME to the directory where R is installed (adjust the path accordingly)
os.environ['R_HOME'] = r"C:\Program Files\R\R-4.4.2"  # Windows example
# For Linux/Mac, something like: os.environ['R_HOME'] = "/usr/lib/R"

import rpy2.robjects as ro

# Get the directory of the current Python script
current_dir = os.path.dirname(os.path.abspath(__file__))

# Build the absolute path to the CSV file located one directory above
df_path = os.path.join(current_dir, "..", "species_pageviews_analysis_2022_present.csv")
df_path = os.path.abspath(df_path)

# Check if the file exists; if not, raise an error
if not os.path.exists(df_path):
    raise FileNotFoundError(f"CSV file not found at: {df_path}")

# Pass the CSV file path to R's global environment so that your R code can use it
ro.globalenv["df_path"] = df_path

print("CSV path passed to R:", df_path)

# Define the full R code as a multi-line string
r_code = r'''
# Install and load necessary R packages
packages <- c("mgcv", "ggplot2", "dplyr", "readr", "tidyr", "cowplot", "grid")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# Load your data using the path provided by Python
df <- read.csv(df_path)

# Convert to long format
df_long <- df %>%
  tidyr::pivot_longer(cols = starts_with("X"), 
               names_to = "date", 
               values_to = "pageviews") %>%
  dplyr::mutate(date = as.Date(sub("X", "", date), format="%Y%m%d"),
         doy = as.numeric(format(date, "%j")),  # Day of Year (DOY)
         month = factor(format(date, "%B"), 
                        levels = c("January", "February", "March", "April", "May", "June", 
                                   "July", "August", "September", "October", "November", "December")),
         year = format(date, "%Y"),             
         year_factor = factor(ifelse(year %in% c("2022", "2023", "2024"), "historical", "2025"), 
                              levels = c("historical", "2025")))
print(str(df_long))

# Create a new folder for saving plots (if it doesn't exist)
if (!dir.exists("species_plots_wiki")) {
  dir.create("species_plots_wiki")
}

# Initialize an empty dataframe to store alert info based on statistical inference
alert_info <- data.frame(species = character(), 
                         language = character(),
                         alert = logical(),
                         stringsAsFactors = FALSE)

# Set the starting index if you want to resume from a specific combination
start_index <- 1

# Loop through all species-language combinations
species_languages <- unique(df_long %>% dplyr::select(Scientific.Name, Language))
print(head(species_languages))

for (i in start_index:nrow(species_languages)) {
  species_name <- species_languages$Scientific.Name[i]
  language <- species_languages$Language[i]
  
  # Filter data for the current species-language combination
  df_subset <- df_long %>%
    dplyr::filter(Scientific.Name == species_name & Language == language)
  
  # Wrap model fitting and plotting in tryCatch to skip on error
  tryCatch({
    # Check for ALL zero observations in 'pageviews'
    if (all(df_subset$pageviews == 0 | is.na(df_subset$pageviews))) {
      message(paste(Sys.time(), ": Skipping model for species", species_name, "in language", language, "because all observations are zero or NA."))
      next
    }
    
    # Count the number of non-zero and non-NA observations
    non_zero_non_na_count <- df_subset %>%
      dplyr::filter(!is.na(pageviews) & pageviews != 0) %>%
      nrow()
    
    # Check if the count is less than 10
    if (non_zero_non_na_count < 10) {
      message(paste(Sys.time(), ": Skipping model for species", species_name, "in language", language, "because there are less than 10 non-zero and non-NA observations."))
      next
    }
    
    # Fit the specified GAM model
    gam_combined <- mgcv::gam(pageviews ~ s(doy, k=52, m=2, bs="tp") +
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
    
    # Historical data (2022-2024)
    df_hist <- df_subset %>%
      dplyr::filter(year_factor == "historical") %>%
      dplyr::arrange(doy)
    
    # 2025 data
    df_2025 <- df_subset %>%
      dplyr::filter(year_factor == "2025") %>%
      dplyr::arrange(doy)
    
    # Aggregate historical data by doy to get the upper confidence interval per day
    hist_by_doy <- df_hist %>%
      dplyr::group_by(doy) %>%
      dplyr::summarize(hist_upr = max(upr, na.rm = TRUE))
    
    # Join aggregated historical data to the 2025 data by doy and create the color column
    df_2025 <- df_2025 %>%
      dplyr::left_join(hist_by_doy, by = "doy") %>%
      dplyr::mutate(color = if_else(lwr > hist_upr, "red", "darkgreen"))
    
    # Build Segments for 2025 Fit and CI Lines
    df_segments_fit <- df_2025 %>%
      dplyr::arrange(doy) %>%
      dplyr::mutate(xend = lead(doy),
             yend = lead(fit),
             seg_color = color) %>%
      dplyr::filter(!is.na(xend))
    
    df_segments_lwr <- df_2025 %>%
      dplyr::arrange(doy) %>%
      dplyr::mutate(xend = lead(doy),
             yend = lead(lwr),
             seg_color = color) %>%
      dplyr::filter(!is.na(xend))
    
    df_segments_upr <- df_2025 %>%
      dplyr::arrange(doy) %>%
      dplyr::mutate(xend = lead(doy),
             yend = lead(upr),
             seg_color = color) %>%
      dplyr::filter(!is.na(xend))
    
    # Create the base plot with historical and 2025 data, including custom legends
    base_plot <- ggplot2::ggplot() +
      ggplot2::geom_point(data = df_hist, 
                 aes(x = doy, y = pageviews, shape = as.factor(year)), 
                 color = "black", alpha = 0.4, size = 0.8) +
      ggplot2::geom_point(data = df_2025, 
                 aes(x = doy, y = pageviews, color = color), 
                 size = 1, alpha = 0.6, show.legend = FALSE) +
      ggplot2::geom_ribbon(data = df_hist, 
                  aes(x = doy, ymin = lwr, ymax = upr), 
                  fill = "black", alpha = 0.2) +
      ggplot2::geom_line(data = df_hist, 
                aes(x = doy, y = fit), 
                color = "black", size = 0.9, alpha = 0.8) +
      ggplot2::geom_segment(data = df_segments_lwr, 
                   aes(x = doy, y = lwr, xend = xend, yend = yend, color = seg_color), 
                   linetype = "solid", alpha = 0.4, size = 0.9, show.legend = FALSE) +
      ggplot2::geom_segment(data = df_segments_upr, 
                   aes(x = doy, y = upr, xend = xend, yend = yend, color = seg_color), 
                   linetype = "solid", alpha = 0.4, size = 0.9, show.legend = FALSE) +
      geom_segment(data = df_segments_fit, 
                   aes(x = doy, y = fit, xend = xend, yend = yend, color = seg_color), 
                   size = 1.2, show.legend = FALSE) +
      labs(title = paste("GAM Model for", species_name, "in", language),
           subtitle = "Black = Historical (2022–2024), Blue/Red = 2025 (available data)",
           x = "Day of Year (DOY)", y = "Pageviews") +
      scale_color_manual(values = c("darkgreen" = "darkgreen", "red" = "red"), 
                         labels = c("2025 (Normal)", "2025 (Alert)")) +
      scale_shape_manual(values = c(1, 2, 3), 
                         labels = c("2022", "2023", "2024")) +
      guides(color = guide_legend(title = "2025 Model Line")) +
      theme_bw() +
      theme(legend.position = "none",
            plot.title = element_text(face = "bold"),
            axis.title = element_text(face = "bold"),
            axis.text = element_text(face = "bold"))
    
    # Build custom legend grob with white background
    legend_grob <- grid::grobTree(
      grid::rectGrob(x = 0, y = 0, width = 1, height = 1,
                     gp = grid::gpar(fill = "white", col = "white", lwd = 1), just = c("left", "bottom")),
      grid::pointsGrob(x = unit(0.05, "npc"), y = unit(0.75, "npc"), pch = 1, 
                       gp = grid::gpar(col = "black", cex = 0.3)),
      grid::textGrob("2022", x = unit(0.15, "npc"), y = unit(0.75, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      grid::pointsGrob(x = unit(0.05, "npc"), y = unit(0.65, "npc"), pch = 2, 
                       gp = grid::gpar(col = "black", cex = 0.3)),
      grid::textGrob("2023", x = unit(0.15, "npc"), y = unit(0.65, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      grid::pointsGrob(x = unit(0.05, "npc"), y = unit(0.55, "npc"), pch = 3, 
                       gp = grid::gpar(col = "black", cex = 0.3)),
      grid::textGrob("2024", x = unit(0.15, "npc"), y = unit(0.55, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      grid::segmentsGrob(x0 = unit(0.05, "npc"), x1 = unit(0.10, "npc"), 
                         y0 = unit(0.45, "npc"), y1 = unit(0.45, "npc"), 
                         gp = grid::gpar(col = "black", lwd = 2)),
      grid::textGrob("Mean 2022–2024", x = unit(0.15, "npc"), 
                     y = unit(0.45, "npc"), just = "left", gp = grid::gpar(fontsize = 10)),
      grid::pointsGrob(x = unit(0.075, "npc"), y = unit(0.35, "npc"), pch = 16, 
                       gp = grid::gpar(col = "darkgreen", cex = 0.3)),
      grid::segmentsGrob(x0 = unit(0.05, "npc"), x1 = unit(0.10, "npc"), 
                         y0 = unit(0.35, "npc"), y1 = unit(0.35, "npc"), 
                         gp = grid::gpar(col = "darkgreen", lwd = 2)),
      grid::textGrob("2025 (Normal)", x = unit(0.15, "npc"), y = unit(0.35, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10)),
      grid::pointsGrob(x = unit(0.075, "npc"), y = unit(0.25, "npc"), pch = 16, 
                       gp = grid::gpar(col = "red", cex = 0.3)),
      grid::segmentsGrob(x0 = unit(0.05, "npc"), x1 = unit(0.10, "npc"), 
                         y0 = unit(0.25, "npc"), y1 = unit(0.25, "npc"), 
                         gp = grid::gpar(col = "red", lwd = 2)),
      grid::textGrob("2025 (Alert)", x = unit(0.15, "npc"), y = unit(0.25, "npc"), 
                     just = "left", gp = grid::gpar(fontsize = 10))
    )
    
    # Create a separate legend plot from the legend grob
    legend_plot <- ggdraw() + draw_grob(legend_grob, x = 0, y = 0, width = 1, height = 1)
    
    # Combine the base plot and the legend plot side-by-side
    final_plot <- plot_grid(base_plot, legend_plot, ncol = 2, rel_widths = c(1, 0.3))
    
    # Save the combined plot to the species_plots_wiki folder
    ggsave(paste0("species_plots_wiki/", species_name, "_", language, ".png"), final_plot, width = 8, height = 6)
    
    print(paste(Sys.time(), ": Plot generated for species", species_name, "in language", language))
    
    # Save alert info based on the statistical inference from the 2025 model:
    # (Here we use the color of the last observation to decide the alert status.)
    alert_info <- rbind(alert_info, data.frame(species = species_name, 
                                               language = language, 
                                               alert = tail(df_2025$color, n = 1) == "red"))
    
  }, error = function(e) {
    message(paste(Sys.time(), ": Skipping model for species", species_name, "in language", language, "due to error:", e$message))
  })
}

# Save the alert info dataframe to a CSV file in the working directory
write.csv(alert_info, "species_alert_info_wiki.csv", row.names = FALSE)
'''

# Execute the R code using rpy2
ro.r(r_code)
