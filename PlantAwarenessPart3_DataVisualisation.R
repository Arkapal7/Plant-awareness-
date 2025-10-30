#----#Loading required packages and libraries-----
install.packages("tidyverse")
install.packages("ggdist")
install.packages("ggrepel")
install.packages("gt")
devtools::install_github("rstudio/gt")
library(gt)
library(ggdist)
library(plotly)
library(tidyverse)
library(ggrepel)
required_packages <- c("readxl", "dplyr", "sf", "ggplot2", "rnaturalearth")
installed <- required_packages %in% rownames(installed.packages())
if (any(!installed)) install.packages(required_packages[!installed])
lapply(required_packages, library, character.only = TRUE)
install.packages("rnaturalearthdata")
library(rnaturalearth)

#-----#Part1: Visualising threat vs views-----

# Reading required dataset
th_views_allsp_df <- read.csv("WCVP_views_threatstatus.csv")

# Reshaping data into long format
th_views_allsp_long_df <- th_views_allsp_df %>%
  pivot_longer(
    cols = starts_with("totalviews_"),
    names_to = "Language",
    names_prefix = "totalviews_",
    values_to = "PageViews"
  ) %>%
  mutate(Language = factor(Language, levels = c("ar", "de", "es", "en", "fr", "it", 
                                                "ja", "pt", "ru", "zh")))

# Defining language mapping
language_names <- c(ar = "Arabic", de = "German", es = "Spanish", en = "English",
                    fr = "French", it = "Italian", ja = "Japanese", pt = "Portuguese",
                    ru = "Russian", zh = "Chinese")

# Changing language name to full
th_views_allsp_long_df <- th_views_allsp_long_df %>%
  mutate(LanguageFull = language_names[Language])

filtered_data <- th_views_allsp_long_df %>%
  filter(LanguageFull %in% "Spanish")

annots <- th_views_allsp_long_df %>%
  filter(!is.na(PageViews)) %>%
  group_by(LanguageFull, ThreatStatus) %>%
  summarise(n=n(),
            x = quantile(PageViews, na.rm=TRUE)[4]) %>%
  mutate(label = paste0("n = ", n),
         y = case_when(
           ThreatStatus=="Threatened"~3.2,
           ThreatStatus=="Non-threatened"~2.2,
           ThreatStatus=="Data Deficient"~1.2
         ))

# Calculate the max (right end) of the half-eye plot for each ThreatStatus and LanguageFull
endpoints <- th_views_allsp_long_df %>%
  group_by(ThreatStatus, LanguageFull) %>%
  summarize(end_x = max(PageViews, na.rm = TRUE)) %>%
  ungroup()

# Adjust the end_x value to keep labels within plot margins
adjustment_factor <- 0.1  # Adjust this value as needed
endpoints <- endpoints %>%
  mutate(adjusted_x = end_x / (1 + adjustment_factor))

# Merge endpoints with annotations (if necessary)
# Assuming `annots` has columns 'ThreatStatus', 'LanguageFull', and 'label'
annots <- annots %>%
  left_join(endpoints, by = c("ThreatStatus", "LanguageFull"))

# Custom colour codes
colour_code <- c("Threatened" = "#EE7733", 
                 "Non-threatened" = "#009988",
                 "Data Deficient" = "grey")

# Re-arranging legend order
legend_order <- c("Threatened", 
                  "Non-threatened",
                  "Data Deficient")

# Update your ggplot code
threatviews_plot <- 
  ggplot(th_views_allsp_long_df, aes(y = ThreatStatus, x = PageViews, group = LanguageFull, fill = ThreatStatus)) +
  ggdist::stat_halfeye(point_size = 4,
                       linewidth  = 7.5,
                       slab_linewidth = 0.25,
                       slab_color = "black") +
  scale_x_log10() +
  geom_text(data = annots, aes(label = label, x = adjusted_x, y = ThreatStatus),
            inherit.aes = FALSE, hjust = 1, vjust = 2, size = 4) +
  facet_wrap(~LanguageFull, scales = "free", ncol = 2, nrow = 5) +  # Faceting by language
  labs(x = "Log page views", y = "Extinction risk", 
       title = "Extinction risk against log page views") +
  scale_fill_manual(values = colour_code, name = "ThreatStatus", limits = legend_order) + 
  theme_ggdist(base_size = 25) +
  theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),          
    axis.title.x = element_text(size = 16),                       
    axis.title.y = element_text(size = 16),                      
    axis.text.x = element_text(size = 9), 
    axis.text.y = element_text(size = 9),   
    strip.text = element_text(face="bold", size = 15),
    strip.background.x = element_rect(colour="grey70", fill="grey85"),
    legend.title = element_text(size = 14, face = "bold"),                       
    legend.text = element_text(size = 12),
    legend.key.size = unit(0.4, "lines"),  
    legend.background = element_rect(color = "black", size = 0.2)
  ) +
  guides(fill = guide_legend(
    title = "Extinction risk",
    title.position = "top",
    title.hjust = 0.5, # Center the title
    label.position = "right", # Position labels to the right of the keys
    keyheight = unit(0.5, "lines"), # Increase the height of the legend keys
    keywidth = unit(1, "lines"), # Increase the width of the legend keys
    ncol = 1, # Arrange legend items in a single column
    byrow = TRUE # Arrange legend items by row
  ))

ggsave("Threatvsviews_allLang.png", threatviews_plot, bg = "white", width = 12, height = 18, dpi = 300)


#-----#Part2: Visualising threat and use vs views----

#Reading required df
th_nth_use_df <- read.csv("Threat_vs_use.csv")

# Converting wide format to long format for threat + use df
th_nth_use_long <- th_nth_use_df %>%
  pivot_longer(
    cols = starts_with("totalviews_"),  # Selects columns that start with 'total_views_'
    names_to = "Language",               # New column for language
    names_prefix = "totalviews_",       # Removes this prefix from the column names in 'Language'
    values_to = "TotalViews"             # New column for values
  ) 

# Defining language mapping
language_names <- c(ar = "Arabic", de = "German", es = "Spanish", en = "English",
                    fr = "French", it = "Italian", ja = "Japanese", pt = "Portuguese",
                    ru = "Russian", zh = "Chinese")

# Changing language name to full
th_nth_use_long <- th_nth_use_long %>%
  mutate(LanguageFull = language_names[Language])

# Creating the interaction variable
th_nth_use_long <- th_nth_use_long %>%
  mutate(Interaction = interaction(factored_use, ThreatStatus))

# Checking for duplicates in scientific names
th_nth_use_long %>%
  group_by(scientificName, Language) %>% 
  summarise(n = n()) %>% 
  filter(n>1) %>% 
  view()

filtered_data <- th_nth_use_long %>%
  filter(LanguageFull %in% c("Spanish"))

# Getting total number of species for the interaction
species_threat_use <- filtered_data %>%
  filter(!is.na(TotalViews)) %>% 
  group_by(Interaction, LanguageFull) %>%
  summarise(
    total = n(),
    x = quantile(TotalViews, na.rm = TRUE)[4],
    .groups = 'drop'  # Ensure summarize doesn't carry grouping
  ) %>%
  mutate(
    label = paste0("n = ", total),
    y = case_when(
      Interaction == "1.Threatened" ~ 6.2,
      Interaction == "0.Threatened" ~ 5.2,
      Interaction == "1.Non-threatened" ~ 4.2,
      Interaction == "0.Non-threatened" ~ 3.2,
      Interaction == "1.Data Deficient" ~ 2.2,
      Interaction == "0.Data Deficient" ~ 1.2
    )
  ) 

# Saving the df
# write.csv(species_threat_use, "Total_species_by_threat_and_use_each_lang.csv")

endpoints <- filtered_data %>%
  filter(!is.na(TotalViews)) %>%
  group_by(Interaction, LanguageFull) %>%
  summarize(end_x = max(TotalViews, na.rm = TRUE), .groups = 'drop')

# Adjust the end_x value to keep labels within plot margins
adjustment_factor <- 0.1  # Adjust this value as needed
endpoints <- endpoints %>%
  mutate(adjusted_x = end_x / (1 + adjustment_factor))

# Merge endpoints with annotations
annots <- species_threat_use %>%
  left_join(endpoints, by = c("Interaction", "LanguageFull"))

# Custom colour codes
colour_code <- c("1.Threatened" = "#EE7733", "0.Threatened" = "white",
                 "1.Non-threatened" = "#009988", "0.Non-threatened" = "white",
                 "1.Data Deficient" = "grey", "0.Data Deficient" = "white")

# Re-arranging legend order
legend_order <- c("1.Threatened", "0.Threatened", "1.Non-threatened", "0.Non-threatened",
                  "1.Data Deficient", "0.Data Deficient")

th_nth_use_long <- filtered_data %>%
  mutate(ThreatStatus = factor(
    ThreatStatus,
    levels = c("Threatened", "Non-threatened", "Data Deficient")
  ))

th_nth_use_long <- filtered_data %>%
  mutate(
    Usefulness = factor(
      factored_use,  # or whatever your column name is
      levels = c(0, 1),
      labels = c("No documented use", "Documented use")
    )
  )

# Plotting the data
Th_use_views_plot <- 
  ggplot(filtered_data, aes(y = Interaction, x = TotalViews, fill = Interaction, alpha = Usefulness)) +
  ggdist::stat_halfeye(data = filtered_data %>% 
                         filter(Interaction =="0.Data Deficient"),
                       point_size = 4,
                       linewidth  = 7.5,
                       slab_linewidth = 1.5,
                       slab_color = "grey60") +
  ggdist::stat_halfeye(data = th_nth_use_long %>% 
                         filter(Interaction == "1.Data Deficient"),
                       point_size = 4,
                       linewidth  = 7.5,
                       slab_linewidth = 0.5,
                       slab_color = "black") +
  ggdist::stat_halfeye(data = th_nth_use_long %>% 
                         filter(Interaction == "0.Non-threatened"),
                       point_size = 4,
                       linewidth  = 7.5,
                       slab_linewidth = 1.5,
                       slab_color = "#009988") +
  ggdist::stat_halfeye(data = th_nth_use_long %>% 
                         filter(Interaction == "1.Non-threatened"),
                       point_size = 4,
                       linewidth  = 7.5,
                       slab_linewidth = 0.5,
                       slab_color = "black") +
  ggdist::stat_halfeye(data = th_nth_use_long %>% 
                         filter(Interaction == "0.Threatened"),
                       point_size = 4,
                       linewidth  = 7.5,
                       slab_linewidth = 1.5,
                       slab_color = "#EE7733") +
  ggdist::stat_halfeye(data = th_nth_use_long %>% 
                         filter(Interaction == "1.Threatened"),
                       point_size = 4,
                       linewidth  = 7.5,
                       slab_linewidth = 0.5,
                       slab_color = "black") +
  facet_wrap(~ LanguageFull, scales = "free_x", ncol = 2, nrow = 5) +  # Facets by Language to compare across languages
  labs(title = "Extinction risk and use against log page views",
       y = "Extinction risk and use",
       x = "Log page views") +
  geom_text(data = annots, aes(label = label, x = adjusted_x, y = y),
            inherit.aes = FALSE, hjust = 1, vjust = 3, size = 3) +
  scale_x_log10() +
  scale_fill_manual(values = colour_code, name = "Interaction", limits = legend_order) + 
  scale_alpha_manual(
    values = c("No documented use" = 0.4, "Documented use" = 1),
    name = "Usefulness",
    labels = c("No documented use" = "0 - No documented use", 
               "Documented use" = "1 - Documented use")) +
  ggdist::theme_ggdist(base_size = 10) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),          
    axis.title.x = element_text(size = 16),                       
    axis.title.y = element_text(size = 16),                      
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10),   
    strip.text = element_text(face="bold", size = 16),
    strip.background.x = element_rect(colour="grey70", fill="grey85"),
    legend.title = element_text(size = 10, face = "bold", hjust = 0.5),                       
    legend.text = element_text(size = 7.5),
    legend.key.size = unit(0.1, "lines"),  
    legend.background = element_rect(color = "black", size = 0.05)
  ) +
  guides(fill = guide_legend(
    title = "Interaction",
    title.position = "top",
    title.hjust = 0.5, # Center the title
    label.position = "right", # Position labels to the right of the keys
    keyheight = unit(1, "lines"), # Increase the height of the legend keys
    keywidth = unit(1, "lines"), # Increase the width of the legend keys
    ncol = 1, # Arrange legend items in a single column
    byrow = TRUE # Arrange legend items by row
  ))

Th_use_views_plot

ggsave("ThreatUsevsviews_es.png", Th_use_views_plot, bg = "white", width = 10, height = 6, dpi = 300)


#-----#Part3: Visualising threat, use and area vs views-----

# Reading df
th_use_area_df <- read.csv("Threat&use+area_codes.csv")

# Pivot long the df to make scatterplot
thuse_area_long <- th_use_area_df %>%
  pivot_longer(
    cols = starts_with("totalviews_"),  # Selects columns that start with 'total_views_'
    names_to = "Language",               # New column for language
    names_prefix = "totalviews_",       # Removes this prefix from the column names in 'Language'
    values_to = "TotalViews"             # New column for values
  ) 

# Defining language mapping
language_names <- c(ar = "Arabic", de = "German", es = "Spanish", en = "English",
                    fr = "French", it = "Italian", ja = "Japanese", pt = "Portuguese",
                    ru = "Russian", zh = "Chinese")

# Changing language name to full
thuse_area_long <- thuse_area_long %>%
  mutate(LanguageFull = language_names[Language])

# Creating the interaction variable
thuse_area_long <- thuse_area_long %>%
  mutate(Interaction = interaction(factored_use, ThreatStatus))

# Checking for duplicates in scientific names
thuse_area_long %>%
  group_by(scientificName, Language) %>% 
  summarise(n = n()) %>% 
  filter(n>1) %>% 
  view()

filtered_data <- thuse_area_long %>%
  filter(LanguageFull %in% c("Spanish"))

# # Calculating the 99.9th percentile for page views for each language
# filtered_data <- filtered_data %>%
#   group_by(LanguageFull) %>%
#   mutate(threshold = quantile(TotalViews, 0.99, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(is_outlier = TotalViews > threshold)
# 
# # Identifying outliers based on the calculated threshold for each language
# threshold <- 150  # Setting threshold value
# filtered_data<- filtered_data %>%
#   mutate(is_outlier = Areacode > threshold)

# Custom colour codes
colour_code <- c("1.Threatened" = "#EE7733", "0.Threatened" = "orangered4",
                 "1.Non-threatened" = "#009988", "0.Non-threatened" = "#666600",
                 "1.Data Deficient" = "grey40", "0.Data Deficient" = "grey60")

# Re-arranging legend order
legend_order <- c("1.Threatened", "0.Threatened", "1.Non-threatened", "0.Non-threatened",
                  "1.Data Deficient", "0.Data Deficient")

# Ensure that Interaction is a factor with the expected levels
filtered_data$Interaction <- factor(filtered_data$Interaction, levels = legend_order)

# Define shapes and fills for each combination of ThreatStatus and Interaction
shape_values <- c("1.Threatened" = 15, "0.Threatened" = 0,
                  "1.Non-threatened" = 16, "0.Non-threatened" = 1,
                  "1.Data Deficient" = 17, "0.Data Deficient" = 2)

fill_values <- c("1.Threatened" = "#EE7733", "0.Threatened" = "white",
                 "1.Non-threatened" = "#009988", "0.Non-threatened" = "white",
                 "1.Data Deficient" = "grey40", "0.Data Deficient" = "white")

filtered_data <- filtered_data %>%
  mutate(ThreatStatus = factor(
    ThreatStatus,
    levels = c("Threatened", "Non-threatened", "Data Deficient")
  ))

filtered_data <- filtered_data %>%
  mutate(
    Usefulness = factor(
      factored_use,  # or whatever your column name is
      levels = c(0, 1),
      labels = c("No documented use", "Documented use")
    )
  )

# Creating scatterplot
th_use_area_plot <-
  ggplot(filtered_data, aes(x = Areacode, y = TotalViews, shape = Interaction,
                            color = Interaction, fill = paste(ThreatStatus, Interaction), 
                            text = scientificName, stroke = 2, alpha = Usefulness)) +
  geom_point(size = 3) +
  labs(x = "Log range size", y = "Log page views", color = "Interaction",
       title = "Log page views against log range size by extinction risk and use") + 
  facet_grid(ThreatStatus~LanguageFull) +
  ggdist::theme_ggdist(base_size = 10) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(values = colour_code, name = "Interaction") +
  scale_shape_manual(values = shape_values) +
  scale_fill_manual(values = fill_values, name = "Threat Status") +
  scale_alpha_manual(
    values = c("No documented use" = 0.4, "Documented use" = 1),
    name = "Usefulness",
    labels = c("No documented use" = "0 - No documented use", 
               "Documented use" = "1 - Documented use")) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),          
    axis.title.x = element_text(size = 16),                       
    axis.title.y = element_text(size = 16),                      
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10),   
    strip.text.x = element_text(face="bold", size = 14),
    strip.text.y = element_text(face="bold", size = 14),
    strip.background.x = element_rect(colour="grey70", fill="grey85"),
    strip.background.y = element_rect(colour="grey70", fill="grey85"),
    legend.title = element_text(size = 12, face = "bold", hjust = 0.5),                       
    legend.text = element_text(size = 9),
    legend.key.size = unit(0.1, "lines"),  
    legend.background = element_rect(color = "black", size = 0.2),
    legend.key.spacing.y = unit(0.5, 'cm')
  ) +
  guides(
    fill = guide_legend(
      override.aes = list(stroke = 2)
    ),
    color = guide_legend( # Settings for the 'Interaction' legend (mapped to color)
      title = "Interaction",
      title.position = "top",
      title.hjust = 0.5,
      label.position = "right",
      keyheight = unit(2, "lines"),
      keywidth = unit(1, "lines"),
      ncol = 1,
      byrow = TRUE
    ),
    shape = guide_legend(
      override.aes = list(stroke = 2)
    ),
    alpha = guide_legend(order = 2) # To make Usefulness legend appear third
  )
# geom_text_repel(data = filter(filtered_data, is_outlier),  # Ensure correct data filtering
#                 aes(label = scientificName), color = "black", vjust = -1,
#                 size = 3, max.overlaps = 20)  # Adds labels for outliers

th_use_area_plot

ggsave("ThreatUseRangevsviews_es.png", th_use_area_plot, bg = "white", width = 10, height = 6, dpi = 300)

# Convert to plotly
(interactive_plot <- ggplotly(p, tooltip = c("text", "x", "y")))

# Save as HTML
saveWidget(interactive_plot, "interactive_plot_it.html")

# Calculating total species and pageviews by language
aggregated_df <- thuse_area_long %>%
  filter(!is.na(TotalViews)) %>%
  group_by(LanguageFull) %>%
  summarise(
    total_species = n_distinct(scientificName, na.rm = TRUE),
    total_pageviews = sum(TotalViews, na.rm = TRUE)
  ) %>%
  arrange(desc(total_species))

# Adding totals row
totals_row <- aggregated_df %>%
  summarise(
    LanguageFull = "Total",
    total_species = sum(total_species, na.rm = TRUE),
    total_pageviews = sum(total_pageviews, na.rm = TRUE)
  )

# Combine the original dataframe with the totals row
aggregated_df <- bind_rows(aggregated_df, totals_row)

# Making gt table
(aggregated_df_gt <- aggregated_df %>% 
    gt() %>% 
    tab_header(
      title = "Total species and pageviews by language"
    ) %>% 
    fmt_scientific(columns = c("LanguageFull", "total_pageviews")) %>%   
    cols_label(
      LanguageFull = "Language",
      total_species = "Total species",
      total_pageviews = "Total pageviews"
    )) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "black",
      weight = px(2)
    ),
    locations = cells_column_labels(
      columns = c(LanguageFull, total_species, total_pageviews)
    )) %>% 
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "transparent",
      weight = px(1)
    ),
    locations = cells_body(
      columns = c(LanguageFull, total_species, total_pageviews)
    ))


test = thuse_area_long %>% 
  group_by(TotalViews) %>% 
  count()


#-----#Part4: Visualising distribution maps-----

# User-defined variables (update file paths and column names!) ---
excel_file_path <- "Lang+spatial_df.csv"   
shapefile_path   <- "shapefile/level3.shp"  
excel_col        <- "area_code_l3"
shapefile_col    <- "LEVEL3_COD"
language_col     <- "Chinese"  # Change this to map a different language

# Load and prepare language data ---
lang_data <- read.csv(excel_file_path)

if (!language_col %in% names(lang_data)) {
  stop("Language column '", language_col, "' not found. Available columns:\n", 
       paste(names(lang_data), collapse = ", "))
}

language_data <- lang_data %>%
  select(region_id = all_of(excel_col), Language_Presence = all_of(language_col)) %>%
  mutate(
    region_id = as.character(region_id),
    Language_Presence = factor(Language_Presence, levels = c(0, 1), labels = c("Absent", "Present"))
  ) %>%
  distinct(region_id, .keep_all = TRUE)

# Load shapefile and ensure compatible join column ---
regions_sf <- st_read(shapefile_path, quiet = TRUE) %>%
  mutate(!!shapefile_col := as.character(.data[[shapefile_col]]))

if (!shapefile_col %in% names(regions_sf)) {
  stop("Join column '", shapefile_col, "' not found in shapefile. Columns:\n", 
       paste(names(regions_sf), collapse = ", "))
}

# Join shapefile with language data ---
map_data <- regions_sf %>%
  left_join(language_data, by = setNames("region_id", shapefile_col))

# Saving map data
write.csv(map_data, "LangDistrib_ar.csv")

# Optional: Load basemap ---
world <- ne_countries(scale = "medium", returnclass = "sf")
plot(world)

# Plot map ---
wplot <- ggplot() +
  geom_sf(data = world, fill = "gray90", color = "white", size = 0.2) +
  geom_sf(data = map_data, aes(fill = Language_Presence), color = "black", size = 0.1) +
  scale_fill_manual(
    values = c("Present" = "lightskyblue", "Absent" = "grey80"),
    name = paste(language_col, "spoken")
  ) +
  labs(
    title = paste("Geographical distribution of", language_col),
    subtitle = "By WGSRPD level 3 regions"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
wplot
# Combine map and Full model results by range with cowplot ---

final_plot_cowplot <- plot_grid(
  p1, wplot,
  nrow = 2,
  labels = c("A", "B"),   # <-- adds subplot labels
  label_size = 22         # optional: adjust label size
)

final_plot_cowplot

# To save:
ggsave("FullModel_RangeMap_zh.png", final_plot_cowplot, bg = "white", width = 10, height = 14, dpi = 300)


