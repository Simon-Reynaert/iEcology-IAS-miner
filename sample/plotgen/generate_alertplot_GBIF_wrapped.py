import os

# Set R_HOME to the directory where R is installed (adjust the path accordingly)
os.environ['R_HOME'] = r"C:\Program Files\R\R-4.4.2"  # Windows example
# For Linux/Mac, you might use: os.environ['R_HOME'] = "/usr/lib/R"

import rpy2.robjects as ro

# Get the directory of the current Python script
current_dir = os.path.dirname(os.path.abspath(__file__))

# Build the absolute path to the CSV file located one directory above
df_path = os.path.join(current_dir, "..", "GBIF_Observations_2022-present_final.csv")
df_path = os.path.abspath(df_path)

# Check if the file exists; if not, raise an error
if not os.path.exists(df_path):
    raise FileNotFoundError(f"CSV file not found at: {df_path}")

# Pass the CSV file path to R's global environment so that the R code can use it
ro.globalenv["df_path"] = df_path
print("CSV path passed to R:", df_path)

# Define the full R code as a multi-line string.
# Note that in the R code, replace the static CSV filename with the variable 'df_path'
r_code = r'''
# List of required packages
packages <- c("mgcv", "ggplot2", "dplyr", "readr", "tidyr", "cowplot", "grid")

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
lapply(packages, library, character.only = TRUE)

# --- Data Loading and Preprocessing (GBIF) ---
# Use the CSV path provided by Python
df <- read.csv(df_path)
head(df)

df_long <- df %>%
  pivot_longer(cols = starts_with("X"),
               names_to = "date",
               values_to = "observations") %>%
  mutate(date = as.Date(sub("X", "", date), format="%Y.%m.%d"),
         doy = as.numeric(format(date, "%j")),
         month = factor(format(date, "%B"),
                        levels = c("January", "February", "March", "April", "May", "June",
                                   "July", "August", "September", "October", "November", "December")),
         year = format(date, "%Y"),
         year_factor = factor(ifelse(year %in% c("2022", "2023", "2024"), "historical", "2025"),
                              levels = c("historical", "2025")))
str(df_long)

# --- Directory Setup (GBIF) ---
if (!dir.exists("species_plots_GBIF")) {
  dir.create("species_plots_GBIF")
}

# --- Alert Dataframe (GBIF) ---
alert_info <- data.frame(species = character(),
                         country = character(),
                         alert = logical(),
                         stringsAsFactors = FALSE)

# --- Looping and Modeling ---
start_index <- 1  # Reset start_index if needed

species_countries <- unique(df_long %>% select(Scientific.Name, Country))
print(species_countries)

for (i in start_index:nrow(species_countries)) {
  species_name <- species_countries$Scientific.Name[i]
  country <- species_countries$Country[i]
  
  df_subset <- df_long %>%
    filter(Scientific.Name == species_name & Country == country)
  
  tryCatch({
    if (all(df_subset$observations == 0 | is.na(df_subset$observations))) {
      message(paste(Sys.time(), ": Skipping model for species", species_name, "in country", country, "because all observations are zero or NA."))
      next # Skip to the next iteration of the loop
    }
    
    # Count the number of non-zero and non-NA observations
    non_zero_non_na_count <- df_subset %>%
      filter(!is.na(observations) & observations != 0) %>%
      nrow()
    
    # Check if the count is less than 10
    if (non_zero_non_na_count < 10) {
      message(paste(Sys.time(), ": Skipping model for species", species_name, "in country", country, "because there are less than 10 non-zero and non-NA observations."))
      next # Skip to the next iteration of the loop
    }
    
    # --- Model Fitting (Poisson Family) ---
    gam_combined <- gam(observations ~ s(doy, k=52, m=2, bs="tp") +
                          s(doy, by=year_factor, k=52, m=1, bs="tp") +
                          s(year_factor, bs="re", k=2),
                        data=na.omit(df_subset),
                        family=poisson,
                        method="REML")
    
    # Predict on the link (log) scale and obtain standard errors
    pred <- predict(gam_combined, newdata = df_subset, type = "link", se.fit = TRUE)
    
    # Convert mean predictions to the response scale
    df_subset$fit   <- exp(pred$fit)
    
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
      mutate(color = if_else(lwr > hist_upr, "red", "darkgreen"))
    
    # -------------------------
    # Build Segments for 2025 Fit and CI Lines
    # -------------------------
    df_segments_fit <- df_2025 %>%
      arrange(doy) %>%
      mutate(xend = lead(doy),
             yend = lead(fit),
             seg_color = color) %>%
      filter(!is.na(xend))
    
    df_segments_lwr <- df_2025 %>%
      arrange(doy) %>%
      mutate(xend = lead(doy),
             yend = lead(lwr),
             seg_color = color) %>%
      filter(!is.na(xend))
    
    df_segments_upr <- df_2025 %>%
      arrange(doy) %>%
      mutate(xend = lead(doy),
             yend = lead(upr),
             seg_color = color) %>%
      filter(!is.na(xend))
    
    # Create the base plot (without legend annotations)
    base_plot <- ggplot() +
      geom_point(data = df_hist,
                 aes(x = doy, y = observations, shape = as.factor(year)),
                 color = "black", alpha = 0.4, size = 0.8) +
      geom_point(data = df_2025,
                 aes(x = doy, y = observations, color = color),
                 size = 1, alpha = 0.6, show.legend = FALSE) +
      geom_ribbon(data = df_hist,
                  aes(x = doy, ymin = lwr, ymax = upr),
                  fill = "black", alpha = 0.2) +
      geom_line(data = df_hist,
                aes(x = doy, y = fit),
                color = "black", size = 0.9, alpha = 0.8) +
      geom_segment(data = df_segments_lwr,
                   aes(x = doy, y = lwr, xend = xend, yend = yend, color = seg_color),
                   linetype = "solid", alpha = 0.4, size = 0.9, show.legend = FALSE) +
      geom_segment(data = df_segments_upr,
                   aes(x = doy, y = upr, xend = xend, yend = yend, color = seg_color),
                   linetype = "solid", alpha = 0.4, size = 0.9, show.legend = FALSE) +
      geom_segment(data = df_segments_fit,
                   aes(x = doy, y = fit, xend = xend, yend = yend, color = seg_color),
                   size = 1.2, show.legend = FALSE) +
      labs(title = paste("GAM Model for", species_name, "in", country),
           subtitle = "Black = Historical (2022–2024), Blue/Red = 2025 (available data)",
           x = "Day of Year (DOY)", y = "Observations (#)") +
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
    
    # Build the custom legend grob with white background
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
    
    # Combine the base plot and the legend plot side-by-side.
    final_plot <- plot_grid(base_plot, legend_plot, ncol = 2, rel_widths = c(1, 0.3))
    
    # Save the combined plot
    ggsave(paste0("species_plots_GBIF/", species_name, "_", country, ".png"), final_plot, width = 8, height = 6)
    
    print(paste(Sys.time(), ": Plot generated for species", species_name, "in country", country))
    
    alert_info <- rbind(alert_info, data.frame(species = species_name,
                                               country = country,
                                               alert = tail(df_2025$color, n = 1) == "red"))
    
  }, error = function(e) {
    message(paste(Sys.time(), ": Skipping model for species", species_name, "in country", country, "due to error:", e$message))
  })
}

# --- Saving Alert Information ---
write.csv(alert_info, "species_alert_info_GBIF.csv", row.names = FALSE)
'''

# Execute the R code using rpy2
ro.r(r_code)
