#----#Loading necessary packages and libraries-----
install.packages("tidyverse")
install.packages("ggdist")
install.packages("rWCVP")
library(rWCVP)
remotes::install_github('matildabrown/rWCVPdata')
library(ggdist)
library(plotly)
library(tidyverse)
library(htmlwidgets)
install.packages("ggrepel")
library(ggrepel)
install.packages("MuMIn")
library(MuMIn)

#----#Part1: Curating WCUP dataset-----

# Reading useful sp dataset
use_df <- read.csv("WCUP2020_updated_230228.csv")

# Curating the df
# Filtering unnecessary columns
use_df_clean <- use_df[, c("taxon_name", "Total")]

# Changing column name
use_df_final <- use_df_clean %>%
  rename(scientificName = taxon_name, total_use = Total)

# Saving the dataset
write.csv(use_df_final, "WCUP_cleaned.csv", row.names = F)

#----#Part2: Preparing df for analysis-----

# Reading required dfs
use_df_final <- read.csv("WCUP_cleaned.csv")
th_views_allsp_df <- read.csv("WCVP_views_threatstatus.csv")

# Replacing '_' with ' ' in scintificName col
use_df_final$scientificName <- gsub("_", " ", use_df_final$scientificName)

# Joining with threat dataset
th_nth_use_df <- left_join(th_views_allsp_df, use_df_final, by = "scientificName")
##On full joining, 26481 species not getting joined from use_df. 

# Filtering df
th_nth_use_df <- th_nth_use_df %>%
  filter(!is.na(ThreatStatus)) %>%  # filtering no threat status
  filter(!is.na(scientificName)) %>%  # filtering no scientific name
  filter(!is.na(QID)) %>%  # filtering no QID
  distinct(QID, .keep_all = T) # removing duplications

# Looking into the column total use
unique(th_nth_use_df$total_use)
class(th_nth_use_df$total_use)

# Converting total uses into factors
th_nth_use_df <- th_nth_use_df %>%
  mutate(factored_use = if_else(total_use %in% c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1, 0))

# Factorising the columns
th_nth_use_df$ThreatStatus <- as.factor(th_nth_use_df$ThreatStatus)
th_nth_use_df$factored_use <- as.factor(th_nth_use_df$factored_use)
th_nth_use_df$ThreatStatus <- gsub("Data Defient", "Data Deficient", th_nth_use_df$ThreatStatus)

# Saving the df
write.csv(th_nth_use_df, "Threat_vs_use.csv", row.names = F)

#----#Part3: Visualising threat vs use----

# Reading required df
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

# Filtering data for particular language
filtered_data <- th_nth_use_long %>%
  filter(LanguageFull %in% c("English"))

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
write.csv(species_threat_use, "Total_species_by_threat_and_use_each_lang.csv")

# Preparing location of annotation
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
colour_code <- c("1.Threatened" = "orangered2", "0.Threatened" = "white",
                 "1.Non-threatened" = "palegreen3", "0.Non-threatened" = "white",
                 "1.Data Deficient" = "grey", "0.Data Deficient" = "white")

# Re-arranging legend order
legend_order <- c("1.Threatened", "0.Threatened", "1.Non-threatened", "0.Non-threatened",
                  "1.Data Deficient", "0.Data Deficient")

# Plotting the data
ggplot(filtered_data, aes(y = Interaction, x = TotalViews, fill = Interaction)) +
  ggdist::stat_halfeye(data = filtered_data %>% 
                         filter(Interaction =="0.Data Deficient"),
                       point_size = 6,
                       linewidth  = 10,
                       slab_linewidth = 1.5,
                       slab_color = "grey60") +
  ggdist::stat_halfeye(data = filtered_data %>% 
                         filter(Interaction == "1.Data Deficient"),
                       point_size = 6,
                       linewidth  = 10,
                       slab_linewidth = 0.5,
                       slab_color = "black") +
  ggdist::stat_halfeye(data = filtered_data %>% 
                         filter(Interaction == "0.Non-threatened"),
                       point_size = 6,
                       linewidth  = 10,
                       slab_linewidth = 1.5,
                       slab_color = "palegreen3") +
  ggdist::stat_halfeye(data = filtered_data %>% 
                         filter(Interaction == "1.Non-threatened"),
                       point_size = 6,
                       linewidth  = 10,
                       slab_linewidth = 0.5,
                       slab_color = "black") +
  ggdist::stat_halfeye(data = filtered_data %>% 
                         filter(Interaction == "0.Threatened"),
                       point_size = 6,
                       linewidth  = 10,
                       slab_linewidth = 1.5,
                       slab_color = "orangered2") +
  ggdist::stat_halfeye(data = filtered_data %>% 
                         filter(Interaction == "1.Threatened"),
                       point_size = 6,
                       linewidth  = 10,
                       slab_linewidth = 0.5,
                       slab_color = "black") +
  # facet_wrap(~ LanguageFull, scales = "free_x") +  # Facets by Language to compare across languages
  labs(title = "Use and threat status against log pageviews",
       y = "Use and threat status",
       x = "log pageviews") +
  geom_text(data = annots, aes(label = label, x = adjusted_x, y = y),
            inherit.aes = FALSE, hjust = 1, vjust = 3, size = 6) +
  scale_x_log10() +
  scale_fill_manual(values = colour_code, name = "Interaction", limits = legend_order) + 
  theme_bw() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),          
    axis.title.x = element_text(size = 18, face = "bold"),                       
    axis.title.y = element_text(size = 18, face = "bold"),                      
    axis.text.x = element_text(size = 14), 
    axis.text.y = element_text(size = 14),                        
    legend.title = element_text(size = 20, face = "bold"),                       
    legend.text = element_text(size = 16),
    legend.key.size = unit(0.5, "lines"),  
    legend.background = element_rect(color = "black", size = 0.5)
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

