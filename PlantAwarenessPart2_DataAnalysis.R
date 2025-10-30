#----# Loading necessary packages and libraries-----
install.packages("tidyverse")
library(tidyverse)
install.packages("MuMIn")
library(MuMIn)
install.packages("DHARMa")
library(DHARMa)
install.packages("emmeans")
library(emmeans)
install.packages("cowplot")
library(cowplot)
install.packages("gt")
library(gt)
install.packages("ggeffects")
library(ggeffects)
library(ggdist)
install.packages("ggh4x")
library(ggh4x)
install.packages("extrafont")
library(extrafont); if(is.null(fonts())) font_import();loadfonts(quiet = T)
install.packages("ggtext")
library(ggtext)

#-----# Part1: Preparing lang_spatial_sp_df-------

# Loading required df
lang_spatial_sp_df <- read.csv("Threat_use_area_spatial.csv")

# Log-transforming the Areacode predictor 
lang_spatial_sp_df <- lang_spatial_sp_df %>%
  group_by(scientificName, redlistCategory, ThreatStatus, total_use, 
           factored_use, Areacode, wcvpv12_id) %>% 
  summarise(across(Arabic:Chinese, function(x) max(x)),
            across(totalviews_ar:totalviews_zh, function(x) x[1])) %>% 
  ungroup() %>% 
  mutate(log_Areacode = log10(Areacode)) 


# Omitting NAs and subsetting the df
data_fit_ar <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_ar, log_Areacode, 
                                                             native_to_language_range = Arabic))
data_fit_de <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_de, log_Areacode,
                                                             native_to_language_range = German))
data_fit_en <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_en, log_Areacode,
                                                             native_to_language_range = English))
data_fit_es <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_es, log_Areacode,
                                                             native_to_language_range = Spanish))
data_fit_fr <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_fr, log_Areacode,
                                                             native_to_language_range =  French))
data_fit_it <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_it, log_Areacode,
                                                             native_to_language_range = Italian))
data_fit_ja <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_ja, log_Areacode,
                                                             native_to_language_range = Japanese))
data_fit_pt <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_pt, log_Areacode,
                                                             native_to_language_range = Portuguese))
data_fit_ru <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_ru, log_Areacode,
                                                             native_to_language_range = Russian))
data_fit_zh <- na.omit(lang_spatial_sp_df %>% dplyr::select (ThreatStatus, factored_use, totalviews_zh, log_Areacode,
                                                             native_to_language_range = Chinese))

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


# Running model for ThreatStatus:log_Areacode, ThreatStatus:factored_use and Language:ThreatStatus 
# interaction for each language
model_range_ar <- glm(totalviews_ar ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_ar, family = quasipoisson(), 
                      na.action = na.fail)
model_range_de <- glm(totalviews_de ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode + 
                        ThreatStatus:native_to_language_range + ThreatStatus:factored_use, data_fit_de, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_en <- glm(totalviews_en ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_en, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_es <- glm(totalviews_es ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode  
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_es, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_fr <- glm(totalviews_fr ~ ThreatStatus + factored_use + log_Areacode +native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_fr, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_it <- glm(totalviews_it ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_it, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_ja <- glm(totalviews_ja ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_ja, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_pt <- glm(totalviews_pt ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_pt, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_ru <- glm(totalviews_ru ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_ru, family = quasipoisson(), 
                      na.action = na.fail) 
model_range_zh <- glm(totalviews_zh ~ ThreatStatus + factored_use + log_Areacode + native_to_language_range + ThreatStatus:log_Areacode
                      + ThreatStatus:factored_use + ThreatStatus:native_to_language_range, data_fit_zh, family = quasipoisson(), 
                      na.action = na.fail)

# Checking model summary
summary(model_range_zh)

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
  top_model <- dredge_df %>% filter(delta <= 6) # changed to 6 on basis of Richards et al
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
write.csv(top_models_df, "Top selected dredge models - pre-nest-filter.csv", row.names = F)

# MANUALLY IDENTIFY NESTED MODELS AND REMOVE

#-----# Part2: Extracting model info and plotting---------

# I checked nesting manually, within the deltaAIC of 6 recommended by Richards

# Getting selected best models for each language
model_sel_ar <- get.models(dredge_ar, subset = 1)[[1]]
model_sel_de <- get.models(dredge_de, subset = 1)[[1]]
model_sel_en <- model.avg(dredge_en, subset = 1:4, fit=TRUE) #averaging four best
model_sel_es <- get.models(dredge_es, subset = 1)[[1]]
model_sel_fr <- model.avg(dredge_fr, subset = 1:2, fit=TRUE) #averaging two best
model_sel_it <- get.models(dredge_it, subset = 1)[[1]]
model_sel_ja <- get.models(dredge_ja, subset = 1)[[1]]
model_sel_pt <- model.avg(dredge_pt, subset = 1:2, fit=TRUE) #averaging two best
model_sel_ru <- get.models(dredge_ru, subset = 1)[[1]]
model_sel_zh <- model.avg(dredge_zh, subset = 1:2, fit=TRUE) #averaging two best



bestmodellist <- lapply(objects(pattern = "model_sel"), get)
languages <- c("Arabic","German","English", "Spanish","French",
               "Italian","Japanese","Portuguese","Russian","Chinese")  



em_th_df_all <- NULL

for (i in 1:length(bestmodellist)){
  model <- bestmodellist[[i]]
  
  id <- languages[i]
  
  
  if(id %in% c("English", "French", "Portuguese", "Chinese")) {
    
    if(id=="English") data=data_fit_en
    if(id=="French") data=data_fit_fr
    if(id=="Portuguese") data=data_fit_pt
    if(id=="Chinese") data=data_fit_zh
    
    emmeans_threat <- emmeans(model, revpairwise ~ ThreatStatus | factored_use + native_to_language_range + log_Areacode, 
                              at = list(log_Areacode = c(0, 1,2),
                                        ThreatStatus = c('Non-threatened', "Threatened")), 
                              weights = "cells",
                              adjust = "tukey", 
                              data=data)
  } else {
    emmeans_threat <- emmeans(model, revpairwise ~ ThreatStatus | factored_use + native_to_language_range + log_Areacode, 
                              at = list(log_Areacode = c(0, 1,2),
                                        ThreatStatus = c('Non-threatened', "Threatened")), 
                              weights = "cells",
                              adjust = "tukey")
  }
  
  #get contrasts - the difference in pageviews
  emmeans_threat_contrasts <- emmeans_threat$contrasts
  
  em_th_df <- as.data.frame(emmeans_threat_contrasts) %>% 
    mutate(
      sig = p.value < 0.05,
      dir = case_when(p.value >= 0.05 ~ "n.s.",
                      estimate > 0 ~ "+",
                      TRUE ~ "-"), 
      language = id
    )
  
  em_th_df_all <- bind_rows(em_th_df_all, em_th_df)
  
}

#Saving the emmeans result into a csv file
write.csv(em_th_df_all, "Top_models-emmeans.csv", row.names = F)

#Reading the csv
em_th_df_all <- read.csv("Top_models-emmeans.csv")

#Forming table of emmeans
(top_model_emmeans_gt <- em_th_df_all %>% 
    gt() %>% 
    tab_header(
      title = "Selected models from estimated means across languages"
    ) %>% 
    cols_label(
      contrast     = "Extinction risk contrast",
      factored_use = "Use",
      native_to_language_range = "Language nativity",
      log_Areacode = "Range",
      estimate = "Coefficient",
      SE = "Standard error",
      df = "Degrees of freedom",
      sig = "Significance",
      dir = "Direction"
    ) %>%
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "black",
        weight = px(2)
      ),
      locations = cells_body(
        rows = everything()
      )
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = c("top", "bottom"),
        color = "transparent",
        weight = px(1)
      ),
      locations = cells_body(
        rows = everything()
      )
    ) %>% 
    tab_style(
      style = cell_borders(
        sides = "bottom",
        color = "black",
        weight = px(2)
      ),
      locations = cells_body(
        rows = nrow(em_th_df_all)
      )
    ) %>%
    tab_style(
      style = cell_text(align = "center"),  # Center-align all text
      locations = cells_body(
        columns = everything(), 
        rows = everything()
      )
    ) %>%
    tab_style(
      style = cell_text(align = "center"),  # Center-align header text
      locations = cells_column_labels(
        columns = everything()
      )
    )
)

#### plot visualising difference in pageviews between threatened and non-threatened species across all covariates and languages
p2v2 <- ggplot(em_th_df_all %>% 
                 mutate(
                   log_Areacode = factor(10^log_Areacode),
                   factored_use = case_when(
                     factored_use ==1 ~ "Documented human use",
                     TRUE~ "No documented human use"
                   ),
                   native_to_language_range = case_when(
                     native_to_language_range==TRUE~"'Local'",
                     TRUE~"Not 'local'"
                   )), 
               aes(x=estimate, 
                   y=interaction(log_Areacode),
                   colour = dir))+
  geom_vline(xintercept=0, linetype="dotted")+
  geom_point(size=2)+
  geom_segment(aes(x=estimate-1.96*SE, xend=estimate+1.96*SE), linewidth=0.6)+
  facet_nested(language~factored_use+native_to_language_range, scales="free")+
  scale_colour_manual(values=c("+"= "#EE7733", "-"="#CA0020", "n.s."="grey60"), guide='none')+
  theme_ggdist()+
  theme(text=element_text(family="Arial"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        panel.border = element_rect(colour="gray70", fill=NA),
        axis.title.x = element_markdown(),
        axis.title.y = element_text(size=12),
        strip.text = element_text(face="bold"),
        strip.background.x = element_rect(colour="grey70", fill="white"),
        strip.background.y = element_rect(colour="grey70", fill="grey85"))+
  labs(y="Range size (number of botanical countries)",
       title = "Difference in pageviews between threatened and 
       non-threatened species across all covariates and languages")+
  xlab("<span style = 'font-size:12pt;'>Difference in mean pageviews</span><br><br><span style = 'color:#0571B0;'>Non-threatened species higher </span><span style = 'color:black;'> \u2190 <span style = 'color:#b3b3b3;' > n.s. </span><span style = 'color:black;'> \u2192 <span style = 'color:#EE7733;'> Threatened species higher  </span>") 

p2v2

ggsave("Threatened_effects_all_lang.png", plot=p2v2, height=9, width=7)

p2 <- ggplot(em_th_df_all %>% 
               mutate(log_Areacode = factor(10^log_Areacode),
                      factored_use = case_when(
                        factored_use ==1 ~ "Documented\nhuman use",
                        TRUE~ "No documented\nhuman use"
                      ),
                      native_to_language_range = case_when(
                        native_to_language_range==1~"'Local'",
                        TRUE~"Not 'local'"
                      ), 
                      specific_contrast = interaction(language,
                                                      factored_use, 
                                                      log_Areacode,
                                                      native_to_language_range)), 
             aes(y=log_Areacode, 
                 x=native_to_language_range ,
                 fill = estimate, alpha=dir))+
  geom_tile(aes(colour=dir))+
  facet_grid(language~factored_use)+
  scale_alpha_manual(values=c("-"= 1, "+"=1, "n.s."=0.3), guide="none")+
  scale_colour_manual(values=c("+"= "#034B75", "-"="#CA0020", "n.s."="transparent"), guide='none')+
  scale_fill_gradientn(colours=c(  "#CA0020", "#F4A582", "#F7F7F7", "#92C5DE", "#0571B0","#034B75", "#01253A", "#000000"), 
                       limits=c(-1, 2.5), name="Contrast\nbetween\nnon-threatened\nand threatened\nspecies\n(log-pageviews)")+
  ggdist::theme_ggdist()+
  labs(y="Range size (number of botanical countries)",
       x="Spatial overlap between species and language")+
  theme(legend.title = element_text(size=8))

p2

#-----# Part3: Visualising contrasts for English Wiki across covariates ####

plot_df <- data_fit_es %>% 
  #add jitter before log transform to space out the small-ranged ones
  mutate(x= log10(10^log_Areacode+runif(nrow(data_fit_es), min=-0.5, max=0.5))) %>% 
  rename(
    predicted=totalviews_es,
    use=factored_use, 
    nativelang=native_to_language_range) %>% 
  mutate(use = case_when(
    use ==1 ~ "Documented human use",
    TRUE~ "No documented human use"
  ),
  nativelang = case_when(
    nativelang==1~"Language and species ranges overlap",
    TRUE~"No overlap between language and species ranges"
  ))


#get ranges to trim preds to:
ranges <- plot_df %>% group_by(nativelang, use) %>% 
  summarise(maxrange = max(log_Areacode)) 

areacodes <- c( 0, 1, 2, ranges %>% pull(maxrange)) %>% unique() %>% str_sort() %>% as.numeric()

#get emmeans and trim to ranges of each facet

predicts_df <-
  emmeans(model_sel_es, c("ThreatStatus","factored_use", "native_to_language_range", "log_Areacode"), 
          at = list(log_Areacode = areacodes), 
          weights = "cells",
          type = "response",
          data=data_fit_es
  ) %>% 
  as.data.frame() %>% 
  rename(nativelang=native_to_language_range, use=factored_use,
         x=log_Areacode, predicted=rate) %>% 
  mutate( use = case_when(
    use ==1 ~ "Documented human use",
    TRUE~ "No documented human use"
  ),
  nativelang = case_when(
    nativelang==1~"Language and species ranges overlap",
    TRUE~"No overlap between language and species ranges"
  )) %>% 
  left_join(ranges) %>% 
  filter(x<=maxrange) 

# Ensure correct factor order for ThreatStatus
predicts_df <- predicts_df %>%
  mutate(ThreatStatus = factor(ThreatStatus, levels = c("Threatened", "Non-threatened", "Data Deficient")))

plot_df <- plot_df %>%
  mutate(ThreatStatus = factor(ThreatStatus, levels = c("Threatened", "Non-threatened", "Data Deficient")))


#plot
p1 <- ggplot(predicts_df,
             aes(x=x, y=predicted, colour=ThreatStatus, fill=ThreatStatus))+
  geom_point(data=plot_df, alpha=0.2, size=1)+
  geom_path(linewidth = 1)+
  geom_ribbon(aes(ymin=asymp.LCL, ymax=asymp.UCL), alpha=0.4, colour="transparent")+
  scale_y_log10(labels = scales::label_number())+
  scale_x_continuous(breaks = log10(c(1,3,10,30,100,300)),
                     labels = function(breaks) round(10^(breaks)))+
  ggdist::theme_ggdist()+
  scale_colour_manual(
    values = c("Threatened" = "#EE7733", 
               "Non-threatened" = "#009988", 
               "Data Deficient" = "grey"),
    name = "Extinction risk"
  ) +
  scale_fill_manual(
    values = c("Threatened" = "#EE7733", 
               "Non-threatened" = "#009988", 
               "Data Deficient" = "grey"),
    name = "Extinction risk"
  ) +
  labs(x="Range size (number of botanical countries)",
       y="Wikipedia page views",
       title="Model results scatterplot for Spanish edition")+
  facet_grid(nativelang~use, scales="free")+
  theme(text=element_text(family="Arial"),
        plot.title = element_text(size = 28, face = "bold", hjust = 0.5),
        panel.border = element_rect(colour="gray70", fill=NA),
        axis.title = element_text(size=22),
        axis.text.y = element_text(size = 10, angle = 90, vjust = 0.5),
        strip.text = element_text(size = 12.5, face="bold"),
        strip.background = element_rect(colour="grey70", fill="grey85"),
        legend.position="inside",
        legend.position.inside = c(0.92, 0.13),
        legend.background = element_rect(fill=NA, colour="gray"),
        legend.text = element_text(size = 12),       # Text inside the legend
        legend.title = element_text(size = 16, face="bold"),      # Legend title
        legend.key.size = unit(0.8, "cm"))

p1

ggsave(plot=p1, "FullModelResultsByRange_es.png", height=10, width=16)

#combine overall and english plots
cowplot::plot_grid(p1, p2v2, ncol=2)
ggsave("pageviews_threat_fig.png", height=10, width=16)

#-----# Part4: Checking to make sure that contrasts are same dir across model avg ####

es_contrasts <- NULL

for (i in 1:5){
  if(i<5) model <- get.models(dredge_es, subset = 1:4)[[i]]
  if (i==5) model <- model_sel_es
  
  if(i <4)  emmeans_threat <- emmeans(model, pairwise ~ ThreatStatus | factored_use + native_to_language_range + log_Areacode, 
                                      at = list(log_Areacode = c(0, 1,2),
                                                ThreatStatus = c('Non-threatened', "Threatened")), 
                                      weights = "cells",
                                      adjust = "tukey")
  
  if(i==4) emmeans_threat <- emmeans(model, pairwise ~ ThreatStatus | factored_use + log_Areacode, 
                                     at = list(log_Areacode = c(0, 1,2),
                                               ThreatStatus = c('Non-threatened', "Threatened")), 
                                     weights = "cells",
                                     adjust = "tukey")
  if(i==5) emmeans_threat <- emmeans(model, pairwise ~ ThreatStatus | factored_use + native_to_language_range + log_Areacode, 
                                     at = list(log_Areacode = c(0, 1,2),
                                               ThreatStatus = c('Non-threatened', "Threatened")), 
                                     weights = "cells",
                                     adjust = "tukey", 
                                     data=data_fit_es)
  
  #get contrasts - the difference in pageviews
  emmeans_threat_contrasts <- emmeans_threat$contrasts
  
  em_th_df <- as.data.frame(emmeans_threat_contrasts) %>% 
    mutate(
      sig = p.value < 0.05,
      dir = case_when(p.value >= 0.05 ~ "n.s.",
                      estimate > 0 ~ "+",
                      TRUE ~ "-"), 
      model = i
    )
  
  es_contrasts<- bind_rows(es_contrasts, em_th_df)
}

ggplot(es_contrasts %>% 
         mutate(log_Areacode = paste("range", log_Areacode),
                factored_use = paste("use", factored_use),
                native_to_language_range = case_when(
                  is.na(native_to_language_range)~"lang: 0",
                  TRUE~paste("lang:", native_to_language_range)), 
                specific_contrast = interaction(model,
                                                factored_use, 
                                                log_Areacode,
                                                native_to_language_range)), 
       aes(x=estimate, 
           y=interaction(model,
                         #factored_use, 
                         log_Areacode
           ),
           colour = factor(model), alpha=dir))+
  geom_point()+
  geom_segment(aes(x=estimate-1.96*SE, xend=estimate+1.96*SE, alpha=dir))+
  scale_alpha_manual(values=c("-"= 1, "+"=1, "n.s."=0.1))+
  facet_grid(native_to_language_range~factored_use)




#-----# Part5: Other covariates ####

em_df_all <- NULL

for (i in 1:length(bestmodellist)){
  model <- bestmodellist[[i]]
  
  id <- languages[i]
  
  
  if(id %in% c("English", "French", "Portuguese", "Chinese")) {
    
    if(id=="English") data=data_fit_en
    if(id=="French") data=data_fit_fr
    if(id=="Portuguese") data=data_fit_pt
    if(id=="Chinese") data=data_fit_zh
    
    emmeans_use <- emmeans(model, revpairwise ~ factored_use | ThreatStatus, 
                           weights = "cells",
                           data=data)
    emmeans_range <- emtrends(model, ~ThreatStatus,  var="log_Areacode", 
                              weights = "cells",
                              data=data)
    
    emmeans_local <- emmeans(model, revpairwise ~ native_to_language_range | ThreatStatus, 
                             weights = "cells",
                             data=data)
    
    
  } else {
    emmeans_use <- emmeans(model, revpairwise ~ factored_use | ThreatStatus, 
                           weights = "cells",
                           data=data)
    emmeans_range <- emtrends(model, ~ThreatStatus,  var="log_Areacode", 
                              weights = "cells",
                              data=data)
    
    emmeans_local <- emmeans(model, revpairwise ~ native_to_language_range | ThreatStatus, 
                             weights = "cells",
                             data=data)
  }
  
  #get contrasts - the difference in pageviews
  emmeans_use_df <- emmeans_use$contrasts %>% 
    as.data.frame() %>% 
    mutate(
      sig = p.value < 0.05,
      dir = case_when(p.value >= 0.05 ~ "n.s.",
                      estimate > 0 ~ "+",
                      TRUE ~ "-"), 
      language = id,
      covariate="Documented human use"
    )
  
  emmeans_local_df <- emmeans_local$contrasts %>% 
    as.data.frame() %>% 
    mutate(
      sig = p.value < 0.05,
      dir = case_when(p.value >= 0.05 ~ "n.s.",
                      estimate > 0 ~ "+",
                      TRUE ~ "-"), 
      language = id,
      covariate="Spatial overlap of species\nand language ('local' effect)"
    )
  
  emmeans_range_df <- emmeans_range %>% test() %>% 
    select(ThreatStatus:p.value) %>% 
    unique() %>% 
    as.data.frame() %>% 
    mutate(
      estimate = log_Areacode.trend,
      sig = p.value < 0.05,
      dir = case_when(p.value >= 0.05 ~ "n.s.",
                      log_Areacode.trend  > 0 ~ "+",
                      TRUE ~ "-"), 
      language = id,
      covariate="Range size effect (trend)"
    )
  
  em_df_all <- bind_rows(em_df_all, emmeans_use_df, emmeans_local_df, emmeans_range_df)
  
}


ggplot(em_df_all, aes(x=fct_relevel(ThreatStatus,"Non-threatened", "Threatened", "Data Deficient"), 
                      y=covariate, fill=estimate, alpha=dir))+
  geom_tile()+
  facet_grid(language~.)+
  scale_alpha_manual(values=c("-"= 1, "+"=1, "n.s."=0.3), guide="none")+
  scale_colour_manual(values=c("+"= "#034B75", "-"="#CA0020", "n.s."="transparent"), guide='none')+
  scale_fill_gradientn(colours=c( "#CA0020", "#F4A582", "white", "#92C5DE", "#0571B0"), limits=c(-5, 5), name="Change in estimated\nmeans/trends\n(log-pageviews)")+
  geom_text(data=em_df_all %>% 
              filter(sig==TRUE), label=("*"), aes(colour=dir))+
  theme_ggdist()+
  labs(x=NULL, 
       y=NULL)

lang_order <- em_df_all %>% 
  filter(ThreatStatus) %>% 
  group_by(language) %>% 
  summarise(mean=mean(estimate)) %>% 
  arrange(desc(mean)) 


p3 <- ggplot(em_df_all, 
             aes(x=estimate, 
                 y=language,
                 colour = dir))+
  geom_vline(xintercept=0, linetype="dotted")+
  geom_point(size=2)+
  geom_segment(aes(x=estimate-1.96*SE, xend=estimate+1.96*SE), linewidth=0.6)+
  facet_nested(covariate~fct_relevel(ThreatStatus,"Non-threatened", "Threatened", "Data Deficient"), switch="y")+
  scale_colour_manual(values=c("+"= "#0571B0", "-"="#CA0020", "n.s."="grey70"), guide='none')+
  theme_ggdist()+
  scale_y_discrete(position = "right")+
  theme(text=element_text(family="Arial"),
        panel.border = element_rect(colour="gray70", fill=NA),
        axis.title.x = element_markdown(),
        axis.title.y = element_text(size=12),
        strip.text = element_text(face="bold"),
        strip.background = element_rect(colour="grey70", fill="white"))+
  labs(y=NULL,
       x="Difference in mean pageviews (log-scale)")


ggsave("other_covariate_effects.png", plot=p3, height=9, width=7)
