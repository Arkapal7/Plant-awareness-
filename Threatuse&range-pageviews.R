#----# Loading necessary packages and libraries-----
install.packages("tidyverse")
install.packages("ggdist")
install.packages("rWCVP")
library(rWCVP)
remotes::install_github('matildabrown/rWCVPdata')
remove.packages("rWCVP")
install.packages("gt")
devtools::install_github("rstudio/gt")
library(gt)
library(ggdist)
library(ggplot2)
library(plotly)
library(tidyverse)
library(tidyr)
library(dplyr)
library(forcats)
library(htmlwidgets)
install.packages("ggrepel")
library(ggrepel)
install.packages("MuMIn")
library(MuMIn)
install.packages("cowplot")
library(cowplot)
install.packages("devtools")
install.packages("DHARMa")
library(DHARMa)
install.packages("MASS")
library(MASS)
install.packages("lmtest")
library(lmtest)

#-----## Part1: Checking range distribution------

# Loading dfs 
th_nth_use_df <- read.csv("Threat_vs_use.csv")

# downloading the area codes 
area_codes <- wcvp_checklist(synonyms = F, hybrids = F, infraspecies = F) %>% 
  filter(plant_name_id %in% th_views_allsp_df$wcvp12_id) %>% 
  group_by(plant_name_id) %>% 
  summarise(n=n()) %>%
  rename(wcvp12_id = plant_name_id, 
         Areacode = n)

# Saving df
write.csv(area_codes, "WCVP_area_codes.csv", row.names = F)

# Joining two dfs
th_use_area_df <- left_join(th_nth_use_df, area_codes, by = "wcvp12_id") %>%
  mutate(Interaction = interaction(factored_use, ThreatStatus))

# th_use_q <- th_use_area_df %>%
#   mutate(threshold = quantile(totalviews_en, 0.999, na.rm = TRUE)) %>%
#   mutate(is_outlier = totalviews_en > threshold)

# Saving df
write.csv(th_use_area_df, "Threat&use+area_codes.csv", row.names = F)
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
                    fr = "French", it = "Italian", ja = "Japanese", pt = "Portugese",
                    ru = "Russian", zh = "Chinese")

# Changing language name to full
thuse_area_long <- thuse_area_long %>%
  mutate(LanguageFull = language_names[Language])

# Creating the interaction variable
thuse_area_long <- thuse_area_long %>%
  mutate(Interaction = interaction(factored_use, ThreatStatus))

filtered_data <- thuse_area_long %>%
  filter(LanguageFull %in% c("Italian"))

# # Calculating the 99.9th percentile for page views for each language
# thuse_area_long <- thuse_area_long %>%
#   group_by(LanguageFull) %>%
#   mutate(threshold = quantile(TotalViews, 0.999, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(is_outlier = TotalViews > threshold)

# # Identifying outliers based on the calculated threshold for each language
# threshold <- 200  # Setting threshold value
# thuse_area_long <- thuse_area_long %>%
#   mutate(is_outlier = Areacode > threshold)


# Creating scatterplot
p <- ggplot(filtered_data, aes(x = Areacode, y = TotalViews, color = Interaction, text = scientificName)) +
  geom_point(size = 2) +
  # geom_smooth(method = "lm") +
  facet_wrap(~ LanguageFull, scales = "free_y", ncol = 4) +  # Facet by Language
  labs(x = "Log range size", y = "Log total views", color = "Threat Status",
       title = "Scatter plot of range size by threat status and use") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_brewer(palette = "Set1", name = "Interaction")
# geom_text_repel(data = filter(thuse_area_long, is_outlier),
# aes(label = scientificName), colour = "black", vjust = -1,
# size = 3, max.overlaps = 20) # Adds labels for outliers

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
  )
gt(aggregated_df)

#-----## Part2: glm using threat, use and range distribution------

# Loading required df
th_use_area_df <- read.csv("Threat&use+area_codes.csv")

# Log-transforming the Areacode predictor (adding 1 to avoid log(0) if necessary)
th_use_area_df <- th_use_area_df %>%
  mutate(log_Areacode = log10(Areacode + 1))

# # List of language columns
# languages <- c("totalviews_ar", "totalviews_de", "totalviews_en",
#                "totalviews_es", "totalviews_fr", "totalviews_it",
#                "totalviews_ja", "totalviews_pt", "totalviews_ru",
#                "totalviews_zh")
# 
# # Initialize list to store models
# models_range_int <- list()
# 
# # Store model results in a loop
# for (language in languages) {
#   
#   # Ensure ThreatStatus has 'Non-threatened' as the reference level
#   th_use_area_df$ThreatStatus <- fct_relevel(th_use_area_df$ThreatStatus, "Non-threatened")
#   
#   # Create the formula dynamically
#   formula <- as.formula(paste(language, "~ ThreatStatus * factored_use * Areacode"))
#   
#   # Fit the Poisson GLM
#   model <- glm(formula, data = th_use_area_df, family = poisson())
#   
#   # Store the model in the list using the language name as the key
#   models_range_int[[language]] <- model
# }
# 
# # Check model summary for one of the languages
# summary(models_range[["totalviews_en"]])

# Omitting NA s and subsetting the df
data_fit_ar <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_ar, log_Areacode))
data_fit_de <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_de, log_Areacode))
data_fit_en <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_en, log_Areacode))
data_fit_es <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_es, log_Areacode))
data_fit_fr <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_fr, log_Areacode))
data_fit_it <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_it, log_Areacode))
data_fit_ja <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_ja, log_Areacode))
data_fit_pt <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_pt, log_Areacode))
data_fit_ru <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_ru, log_Areacode))
data_fit_zh <- na.omit(th_use_area_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_zh, log_Areacode))

# Transforming intercept to non-threatened
data_fit_ar$ThreatStatus <- fct_relevel(data_fit_ar$ThreatStatus, "Non-threatened")
data_fit_de$ThreatStatus <- fct_relevel(data_fit_de$ThreatStatus, "Non-threatened")
data_fit_en$ThreatStatus <- fct_relevel(data_fit_en$ThreatStatus, "Non-threatened")
data_fit_es$ThreatStatus <- fct_relevel(data_fit_es$ThreatStatus, "Non-threatened")
data_fit_fr$ThreatStatus <- fct_relevel(data_fit_fr$ThreatStatus, "Non-threatened")
data_fit_it$ThreatStatus <- fct_relevel(data_fit_it$ThreatStatus, "Non-threatened")
data_fit_ja$ThreatStatus <- fct_relevel(data_fit_ja$ThreatStatus, "Non-threatened")
data_fit_pt$ThreatStatus <- fct_relevel(data_fit_pt$ThreatStatus, "Non-threatened")
data_fit_ru$ThreatStatus <- fct_relevel(data_fit_ru$ThreatStatus, "Non-threatened")
data_fit_zh$ThreatStatus <- fct_relevel(data_fit_zh$ThreatStatus, "Non-threatened")

# Fitting glm for each language
(model_ar <- glm(totalviews_ar ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_ar, family = poisson(), na.action = na.fail))
(model_de <- glm(totalviews_de ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_de, family = poisson(), na.action = na.fail))
(model_en <- glm(totalviews_en ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_en, family = poisson(), na.action = na.fail))
(model_es <- glm(totalviews_es ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_es, family = poisson(), na.action = na.fail))
(model_fr <- glm(totalviews_fr ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_fr, family = poisson(), na.action = na.fail))
(model_it <- glm(totalviews_it ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_it, family = poisson(), na.action = na.fail))
(model_ja <- glm(totalviews_ja ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_ja, family = poisson(), na.action = na.fail))
(model_pt <- glm(totalviews_pt ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_pt, family = poisson(), na.action = na.fail))
(model_ru <- glm(totalviews_ru ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_ru, family = poisson(), na.action = na.fail))
(model_zh <- glm(totalviews_zh ~ ThreatStatus + factored_use + log_Areacode,
                        data_fit_zh, family = poisson(), na.action = na.fail))

summary(model_range_ar)

# # Function to generate prediction plots for a given model
# range_prediction_plot <- function(model) {
#   plot <- sjPlot::plot_model(model, type = "pred", terms = c("log_Areacode", "ThreatStatus", "factored_use")) +
#     scale_y_log10() +
#     theme_bw()
#   return(plot)
# }
# 
# # Generating prediction plots for all models in the list
# plots_list <- lapply(models_range, range_prediction_plot)
# 
# # combine the plots
# combined_plot <- plot_grid(plotlist = plots_list, align = 'v', ncol = 3)  
# 
# # Display the combined plot
# print(combined_plot)
# 
# sjPlot::plot_model(model_range_zh_int, type="pred", terms=c("log_Areacode", "ThreatStatus", "factored_use")) + 
#   scale_y_log10() 
# 
# # Fitting all models and rank in a data frame
# range_AIC_en <- dredge(model_range_en, rank = "AIC") 
# 
# range_AIC_en

# # perform likelihood ratio tests for each fixed factor
# drop_test <- drop1(models_range[["totalviews_ja"]], test = "Chisq") 
# summary(drop_test)

# Running model for ThreatStatus:log_Areacode, factored_use:log_Areacode, ThreatStatus:factored_use 
# interaction for each language
model_range_ar <- glm(totalviews_ar ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use, data_fit_ar, 
                      family = quasipoisson(), na.action = na.fail)
model_range_de <- glm(totalviews_de ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_de, family = quasipoisson(), na.action = na.fail) 
model_range_en <- glm(totalviews_en ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_en, family = quasipoisson(), na.action = na.fail) 
model_range_es <- glm(totalviews_es ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_es, family = quasipoisson(), na.action = na.fail) 
model_range_fr <- glm(totalviews_fr ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_fr, family = quasipoisson(), na.action = na.fail) 
model_range_it <- glm(totalviews_it ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_it, family = quasipoisson(), na.action = na.fail) 
model_range_ja <- glm(totalviews_ja ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_ja, family = quasipoisson(), na.action = na.fail) 
model_range_pt <- glm(totalviews_pt ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_pt, family = quasipoisson(), na.action = na.fail) 
model_range_ru <- glm(totalviews_ru ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_ru, family = quasipoisson(), na.action = na.fail) 
model_range_zh <- glm(totalviews_zh ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                      + factored_use:log_Areacode + ThreatStatus:factored_use,
                      data_fit_zh, family = quasipoisson(), na.action = na.fail)

# Checking model summary
summary(model_range_en)

# saveRDS(model_range_en, "Quasimodel_en.rds")

# Function to create a Q-Q plot for a given model
qq_plot <- function(model, title) {
  qq <- ggplot(data = data.frame(sample = residuals(model, type = "deviance")), aes(sample = sample)) +
    stat_qq() +
    stat_qq_line() +
    ggtitle(title) +
    theme_ggdist(base_size = 45)
  
  return(qq)
}

# Create Q-Q plots for all models
plots <- list(
  qq_plot(model_range_ar, "Arabic"),
  qq_plot(model_range_de, "German"),
  qq_plot(model_range_en, "English"),
  qq_plot(model_range_es, "Spanish"),
  qq_plot(model_range_fr, "French"),
  qq_plot(model_range_it, "Italian"),
  qq_plot(model_range_ja, "Japanese"),
  qq_plot(model_range_pt, "Portuguese"),
  qq_plot(model_range_ru, "Russian"),
  qq_plot(model_range_zh, "Chinese")
)

# Combine the Q-Q plots into a single figure
combined_plot <- plot_grid(plotlist = plots, ncol = 2)

# Create a title
title <- ggdraw() + 
  draw_label(
    "Q-Q plots for Poisson GLMs by language", 
    fontface = 'bold', 
    size = 50, 
    x = 0.5, 
    hjust = 0.5
  )
# Combine the title and the combined plot
(final_plot <- plot_grid(
  title, 
  combined_plot, 
  ncol = 1,  # Stack them vertically
  rel_heights = c(0.1, 1)  # Adjust relative heights: title takes 10% of the height
))

# Checking over-dispersion
testOverdispersion(model_range_en)
par(mfrow = c(2,2))
simulateResiduals(fittedModel = model_en, plot = T)

# Performing model selection
# Define the custom family function
x.quasipoisson <- function(...) {
  res <- quasipoisson(...)
  res$aic <- poisson(...)$aic
  res
}

# Refit the model using the workaround family
qm_dredge_ar <- update(model_range_ar,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_de <- update(model_range_de,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_en <- update(model_range_en,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_es <- update(model_range_es,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_fr <- update(model_range_fr,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_it <- update(model_range_it,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_ja <- update(model_range_ja,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_pt <- update(model_range_pt,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_ru <- update(model_range_ru,family = "x.quasipoisson", na.action = na.fail)
qm_dredge_zh <- update(model_range_zh,family = "x.quasipoisson", na.action = na.fail)

# Get overdispersion parameter
(chat_ar <- deviance(model_range_ar) / df.residual(model_range_ar))
(chat_de <- deviance(model_range_de) / df.residual(model_range_de))
(chat_en <- deviance(model_range_en) / df.residual(model_range_en))
(chat_es <- deviance(model_range_es) / df.residual(model_range_es))
(chat_fr <- deviance(model_range_fr) / df.residual(model_range_fr))
(chat_it <- deviance(model_range_it) / df.residual(model_range_it))
(chat_ja <- deviance(model_range_ja) / df.residual(model_range_ja))
(chat_pt <- deviance(model_range_pt) / df.residual(model_range_pt))
(chat_ru <- deviance(model_range_ru) / df.residual(model_range_ru))
(chat_zh <- deviance(model_range_zh) / df.residual(model_range_zh))

# dredge/QAIC
QAIC(qm_dredge_ar, chat = chat_ar)
(dredge_ar <- dredge(qm_dredge_ar,rank = "QAIC", chat = chat_ar))

QAIC(qm_dredge_de, chat = chat_de)
(dredge_de <- dredge(qm_dredge_de,rank = "QAIC", chat = chat_de))

QAIC(qm_dredge_en, chat = chat_en)
(dredge_en <- dredge(qm_dredge_en,rank = "QAIC", chat = chat_en))

QAIC(qm_dredge_es, chat = chat_es)
(dredge_es <- dredge(qm_dredge_es,rank = "QAIC", chat = chat_es))

QAIC(qm_dredge_fr, chat = chat_fr)
(dredge_fr <- dredge(qm_dredge_fr,rank = "QAIC", chat = chat_fr))

QAIC(qm_dredge_it, chat = chat_it)
(dredge_it <- dredge(qm_dredge_it,rank = "QAIC", chat = chat_it))

QAIC(qm_dredge_ja, chat = chat_ja)
(dredge_ja <- dredge(qm_dredge_ja,rank = "QAIC", chat = chat_ja))

QAIC(qm_dredge_pt, chat = chat_pt)
(dredge_pt <- dredge(qm_dredge_pt,rank = "QAIC", chat = chat_pt))

QAIC(qm_dredge_ru, chat = chat_ru)
(dredge_ru <- dredge(qm_dredge_ru,rank = "QAIC", chat = chat_ru))

QAIC(qm_dredge_zh, chat = chat_zh)
(dredge_zh <- dredge(qm_dredge_zh,rank = "QAIC", chat = chat_zh))


# Convert dredge results to data frame
dredge_to_df <- function(dredge_result) {
  as.data.frame(dredge_result)
}

# Convert dredge results to data frames
dredge_ar_df <- dredge_to_df(dredge_ar)
dredge_de_df <- dredge_to_df(dredge_de)
dredge_en_df <- dredge_to_df(dredge_en)
dredge_es_df <- dredge_to_df(dredge_es)
dredge_fr_df <- dredge_to_df(dredge_fr)
dredge_it_df <- dredge_to_df(dredge_it)
dredge_ja_df <- dredge_to_df(dredge_ja)
dredge_pt_df <- dredge_to_df(dredge_pt)
dredge_ru_df <- dredge_to_df(dredge_ru)
dredge_zh_df <- dredge_to_df(dredge_zh)

# Write the data frames to CSV files
write.csv(dredge_ar_df, "Dredge-models_ar.csv", row.names = FALSE)
write.csv(dredge_de_df, "Dredge-models_de.csv", row.names = FALSE)
write.csv(dredge_en_df, "Dredge-models_en.csv", row.names = FALSE)
write.csv(dredge_es_df, "Dredge-models_es.csv", row.names = FALSE)
write.csv(dredge_fr_df, "Dredge-models_fr.csv", row.names = FALSE)
write.csv(dredge_it_df, "Dredge-models_it.csv", row.names = FALSE)
write.csv(dredge_ja_df, "Dredge-models_ja.csv", row.names = FALSE)
write.csv(dredge_pt_df, "Dredge-models_pt.csv", row.names = FALSE)
write.csv(dredge_ru_df, "Dredge-models_ru.csv", row.names = FALSE)
write.csv(dredge_zh_df, "Dredge-models_zh.csv", row.names = FALSE)

# Function to extract the top model with the lowest delta QAIC
extract_top_model <- function(dredge_df, language) {
  top_model <- dredge_df %>% filter(delta <= 2)
  top_model$Language <- language
  return(top_model)
}

# Extract the top models for each language
top_model_ar <- extract_top_model(dredge_ar_df, "Arabic")
top_model_de <- extract_top_model(dredge_de_df, "German")
top_model_en <- extract_top_model(dredge_en_df, "English")
top_model_es <- extract_top_model(dredge_es_df, "Spanish")
top_model_fr <- extract_top_model(dredge_fr_df, "French")
top_model_it <- extract_top_model(dredge_it_df, "Italian")
top_model_ja <- extract_top_model(dredge_ja_df, "Japanese")
top_model_pt <- extract_top_model(dredge_pt_df, "Portuguese")
top_model_ru <- extract_top_model(dredge_ru_df, "Russian")
top_model_zh <- extract_top_model(dredge_zh_df, "Chinese")

# Combine the top models into a single data frame
top_models_df <- bind_rows(
  top_model_ar,
  top_model_de,
  top_model_en,
  top_model_es,
  top_model_fr,
  top_model_it,
  top_model_ja,
  top_model_pt,
  top_model_ru,
  top_model_zh
)

# Saving df
write.csv(top_models_df, "Top selected dredge models.csv", row.names = F)

summary(glm(totalviews_ja ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
            + factored_use:log_Areacode, data_fit_ja, 
            family = quasipoisson(), na.action = na.fail))

#-------## Part3: Extracting model info and plotting---------

# Running selected best models for each language
model_sel_ar <- glm(totalviews_ar ~ ThreatStatus + factored_use + log_Areacode 
                    + ThreatStatus:log_Areacode + ThreatStatus:factored_use, data_fit_ar, 
                    family = quasipoisson(), na.action = na.fail)
model_sel_de <- glm(totalviews_de ~ ThreatStatus + factored_use + log_Areacode 
                    + ThreatStatus:log_Areacode, data_fit_de, 
                    family = quasipoisson(), na.action = na.fail) 
model_sel_en <- glm(totalviews_en ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                    + factored_use:log_Areacode + ThreatStatus:factored_use, data_fit_en, 
                    family = quasipoisson(), na.action = na.fail)
model_sel_es <- glm(totalviews_es ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                    + factored_use:log_Areacode + ThreatStatus:factored_use, data_fit_es, 
                    family = quasipoisson(), na.action = na.fail)
model_sel_fr <- glm(totalviews_fr ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                    + factored_use:log_Areacode + ThreatStatus:factored_use, data_fit_fr, 
                    family = quasipoisson(), na.action = na.fail)
model_sel_it <- glm(totalviews_it ~ ThreatStatus + factored_use + log_Areacode 
                    + ThreatStatus:log_Areacode, data_fit_it, 
                    family = quasipoisson(), na.action = na.fail)
model_sel_ja1 <- glm(totalviews_ja ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                     + factored_use:log_Areacode, data_fit_ja, 
                     family = quasipoisson(), na.action = na.fail)
model_sel_ja2 <- glm(totalviews_ja ~ ThreatStatus + factored_use + log_Areacode 
                     + ThreatStatus:log_Areacode, data_fit_ja, 
                     family = quasipoisson(), na.action = na.fail)
model_sel_pt <- glm(totalviews_pt ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                     + ThreatStatus:factored_use, data_fit_pt, 
                    family = quasipoisson(), na.action = na.fail)
model_sel_ru1 <- glm(totalviews_ru ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                     + factored_use:log_Areacode + ThreatStatus:factored_use, data_fit_ru, 
                     family = quasipoisson(), na.action = na.fail)
model_sel_ru2 <- glm(totalviews_ru ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                     + factored_use:log_Areacode, data_fit_ru, 
                     family = quasipoisson(), na.action = na.fail)
model_sel_zh <- glm(totalviews_zh ~ ThreatStatus + factored_use + log_Areacode + ThreatStatus:log_Areacode
                    + factored_use:log_Areacode + ThreatStatus:factored_use, data_fit_zh, 
                    family = quasipoisson(), na.action = na.fail)

# Function to calculate significance stars
add_significance_stars <- function(p_value) {
  ifelse(p_value < 0.001, "<0.001",
         ifelse(p_value < 0.01, "<0.01",
                ifelse(p_value < 0.05, "<0.05",
                       ifelse(p_value < 0.1, "n.s.", "n.s.")
                )
         )
  )
}
# Define a function to extract coefficients, standard errors, and p-values with significance stars
extract_model_info <- function(model, language) {
  summary_model <- summary(model)
  coefficients <- summary_model$coefficients
  df <- as.data.frame(coefficients)
  df$Significance <- add_significance_stars(df$`Pr(>|t|)`)
  df$Language <- language
  df$Variable <- rownames(df)
  rownames(df) <- NULL
  return(df)
}

# Extract information from each model
model_info_ar <- extract_model_info(model_sel_ar, "Arabic")
model_info_de <- extract_model_info(model_sel_de, "German")
model_info_en <- extract_model_info(model_sel_en, "English")
model_info_es <- extract_model_info(model_sel_es, "Spanish")
model_info_fr <- extract_model_info(model_sel_fr, "French")
model_info_it <- extract_model_info(model_sel_it, "Italian")
model_info_ja1 <- extract_model_info(model_sel_ja1, "Japanese#1")
model_info_ja2 <- extract_model_info(model_sel_ja2, "Japanese#2")
model_info_pt <- extract_model_info(model_sel_pt, "Portuguese")
model_info_ru1 <- extract_model_info(model_sel_ru1, "Russian#1")
model_info_ru2 <- extract_model_info(model_sel_ru2, "Russian#2")
model_info_zh <- extract_model_info(model_sel_zh, "Chinese")

# Combine all the data frames into one
all_model_info <- bind_rows(model_info_ar, model_info_de, model_info_en, model_info_es, 
                            model_info_fr, model_info_it, model_info_ja1, model_info_ja2,
                            model_info_pt, model_info_ru1, model_info_ru2, model_info_zh)

# Rename columns for clarity
colnames(all_model_info) <- c("Estimate", "StdError", "tValue", "pValue", "Significance", "Language", "Variable") 

# Convert all columns to character to avoid data type issues during pivoting
all_model_info <- all_model_info %>%
  mutate(across(c(Estimate, StdError, tValue, pValue, Significance), as.character))

# Combine pValue and Significance into one column
all_model_info <- all_model_info %>%
  mutate(pValue = paste0(pValue, Significance))

# Pivot the data to a wider format directly
model_table <- all_model_info %>%
  pivot_wider(names_from = c(Language), values_from = c(Estimate, StdError, tValue, pValue, Significance), 
              names_sep = "_")

# Saving the model table
write.csv(model_table, "Threat_use_range top models df.csv", row.names = F)

# Making table with only significance col
model_table_sig <- all_model_info[, !names(all_model_info) %in% c("Estimate", "StdError", "tValue", "pValue")]

# Pivot wide the df
model_table_sig <- model_table_sig %>% 
  pivot_wider(names_from = c(Language), values_from = Significance, names_sep = "_")

# Saving df
write.csv(model_table_sig, "Threat_use_range top models signif df.csv", row.names = F)

# Prepare the data for plotting
plot_data <- all_model_info %>%
  transmute(
    Language,
    Variable,
    Estimate = as.numeric(Estimate),
    StdError = as.numeric(StdError),
    LowerCI = Estimate - 1.96 * StdError,
    UpperCI = Estimate + 1.96 * StdError,
    pValue
  )

# Create the coefficient plot
ggplot(plot_data, aes(y = Estimate, x = Language, color = Variable)) +
  geom_point(position = position_dodge(width = 0.8)) +
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2, position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  theme_bw() +
  labs(
    title = "Coefficient Plot for GLM Models",
    y = "Estimate",
    color = "Variable",
    x = "Language"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
