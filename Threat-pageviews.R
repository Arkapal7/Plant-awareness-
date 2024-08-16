#----#Loading required packages and libraries-----

install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
install.packages("ggdist")
install.packages("rWCVP")
library(rWCVP)
remotes::install_github('matildabrown/rWCVPdata')
library(ggdist)
library(ggplot2)
library(tidyverse)
library(httr)
library(jsonlite)

#-----##Part1: Curation------   

# Downloading sp. checklist from wcvp
wcvp <- wcvp_checklist(taxon = NULL, 
                       synonyms = F,
                       extinct = F,
                       infraspecies = F,
                       introduced = F,
                       location_doubtful = F,
                       report_type = c("alphabetical", "taxonomic")) %>% 
  unique(wcvp$taxon_name)

# Reading dfs
sp_dataset <- read.csv("WCVPv12-RL2023-1.csv")
assessment <- read.csv("assessments.csv")

# Check for NA in redlist category column
clean_sp_data <- sp_dataset %>%
  filter(!is.na(redlistCategory))
str(clean_sp_data)

# Visualizizng the unique values in redlistCategory 
unique_values <- unique(sp_dataset$redlistCategory)
print(unique_values)

# Creating df with the redlist categories 
redlist_data <- data.frame(
  redlistCategory = c("Critically Endangered", "Endangered", "Vulnerable", "Near Threatened", 
                      "Least Concern", "Lower Risk/near threatened", 
                      "Lower Risk/conservation dependent", "Lower Risk/least concern", "Data Deficient"),
  other_column = 1:9
)
print(clean_sp_data)

th_nt_sp_data <- clean_sp_data %>%
  filter(redlistCategory %in% c("Critically Endangered", "Endangered", "Vulnerable", 
                                "Near Threatened", "Least Concern", "Lower Risk/near threatened", 
                                "Lower Risk/conservation dependent", "Lower Risk/least concern", 
                                "Data Deficient"))
# Removing non-required columns
th_nt_sp_data_cl <- th_nt_sp_data[, c("scientificName", "redlistCategory", "populationTrend",
                                     "scopes", "wcvpv12_id", "assessmentId", "internalTaxonId", 
                                     "ipni_id", "criteriaVersion")]
assessment_cl <- assessment[, c("internalTaxonId", "yearPublished")]

# Saving assessment df
write.csv(assessment_cl, "Assessment_clean_df.csv", row.names = F)

# Joining both dfs
th_nt_sp_data_cl <- inner_join(th_nt_sp_data_cl, assessment_cl, by = "internalTaxonId") 

# Filtering df with redlist category only
th_nt_sp_data_cl <- th_nt_sp_data_cl %>%
  filter(!is.na(redlistCategory))

# Filter to include only rows where criteriaVersion is 3.1
th_nt_sp_data_cl <- th_nt_sp_data_cl %>%
  filter(criteriaVersion == 3.1)

# Saving the dataset
write.csv(th_nt_sp_data_cl, "WCVP_clean_df.csv", row.names = F)

#-----##Part2: function for QID retrieval------

# Reading df if required
th_nt_sp_data_cl <- read.csv("WCVP_clean_df.csv")

# Same function, just to change the language = ""
get_wikidata_qid_via_search <- function(scientific_name) {
  
  # Define the base URL for the Wikidata search API
  base_url <- "https://www.wikidata.org/w/api.php"
  
  # Set up the parameters for the API request
  params <- list(
    action = "wbsearchentities",
    format = "json",
    language = "zh", #Change the language as required
    type = "item",
    search = scientific_name
  )
  
  # Make the API request using GET
  response <- GET(url = base_url, query = params)
  
  # Check the status code of the response
  if (status_code(response) != 200) {
    return(NA)  # Return NA if the response is not successful
  }
  
  # Parse the JSON response
  content <- fromJSON(rawToChar(response$content), flatten = TRUE)
  
  # Check the structure of the content
  print(content)
  
  # Check if any results were returned and extract the QID
  if (length(content$search) > 0 && !is.null(content$search$id[1])) {
    return(content$search$id[1])  # Accessing the 'id' field correctly
  } else {
    return(NA)  # Return NA if no QID is found
  }
}

# Example scientific names
sci_names <- th_nt_sp_data_cl$scientificName

# Retrieve QIDs using the new function
th_nt_sp_data_cl$QID_zh <- sapply(sci_names, get_wikidata_qid_via_search)

# Print the results
print(th_nt_sp_data_cl$QID_)

# Saving the updated dataset
write.csv(th_nt_sp_data_cl, "WCVP+QID_all_lang.csv", row.names = F)

#-----##Part3: Getting species titles of retrieved qid across all languages----

# Loading dataset if required
th_nt_sp_data_cl <- read.csv("WCVP+QID_all_lang.csv")

# Function to get wiki titles from QID
get_wikipedia_title_from_qid <- function(qid, language = "ja") {
  # Base URL for the Wikidata API
  base_url <- "https://www.wikidata.org/w/api.php"
  
  # Parameters for API request to get the Wikipedia title from QID
  params <- list(
    action = "wbgetentities",
    ids = qid,
    format = "json",
    props = "sitelinks",
    sitefilter = paste0(language, "wiki")
  )
  
  # Make the API request
  response <- httr::GET(url = base_url, query = params)
  content <- fromJSON(rawToChar(response$content), flatten = TRUE)
  
  # Extract the Wikipedia title if available
  if (!is.null(content$entities[[qid]]$sitelinks[[paste0(language, "wiki")]]$title)) {
    return(content$entities[[qid]]$sitelinks[[paste0(language, "wiki")]]$title)
  } else {
    return(NA)  # Return NA if no title is found
  }
}
# Example data frame setup (each for separate language)
title_ja <- data.frame(QID = th_nt_sp_data_cl$QID_ja) 

# Add a new column to the Dataframe with the Wikipedia titles (each for separate language)
title_ja <- title_ja %>%
  mutate(Title = map_chr(QID, ~ get_wikipedia_title_from_qid(.x)))

# Filtering NA in title column (each for separate language)
title_ja <- title_ja %>%
  filter(!is.na(Title))

# Saving dataframe (each for separate language)
write.csv(title_ja, "WCVP_QIDwithArticle_ja.csv", row.names = F)

#-----#Part4: Retrieving monthly page views and getting total views from the available titles-----

# Reading required datasets
title_ar <- read.csv("WCVP_QIDwithArticle_ar.csv")
title_de <- read.csv("WCVP_QIDwithArticle_de.csv")
title_en <- read.csv("WCVP_QIDwithArticle_en.csv")
title_es <- read.csv("WCVP_QIDwithArticle_es.csv")
title_fr <- read.csv("WCVP_QIDwithArticle_fr.csv")
title_it <- read.csv("WCVP_QIDwithArticle_it.csv")
title_ja <- read.csv("WCVP_QIDwithArticle_ja.csv")
title_pt <- read.csv("WCVP_QIDwithArticle_pt.csv")
title_ru <- read.csv("WCVP_QIDwithArticle_ru.csv")
title_zh <- read.csv("WCVP_QIDwithArticle_zh.csv")

# Defining articles
articles <- title_zh$Title 

# Function to download monthly page views
get_pageviews <- function(title, start_date, end_date, project = "zh.wikipedia") {
  # Encode the title to handle spaces and special characters
  encoded_title <- URLencode(title)
  
  # Construct the API URL
  url <- paste0("https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/",
                project, "/all-access/user/", encoded_title, 
                "/monthly/", start_date, "/", end_date)
  
  # Make the HTTP GET request
  response <- GET(url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response
    data <- fromJSON(content(response, "text"), flatten = TRUE)
    
    # Return the data frame of views
    return(data$items)
  } else {
    warning("Failed to fetch data for: ", title)
    return(data.frame())  # Return an empty data frame on failure
  }
}

# Date for download
start_date <- "20150701"
end_date <- "20231130"

# Get pageviews for each article
pageviews_list <- lapply(articles, get_pageviews, start_date, end_date)

# Combine all results into one data frame and save it
zh_sp_views <- do.call(rbind, pageviews_list)

# Loading dfs
ar_spviews <- read.csv("Monthlyviews_ar.csv")
de_spviews <- read.csv("Monthlyviews_de.csv")
en_spviews <- read.csv("Monthlyviews_en.csv")
es_spviews <- read.csv("Monthlyviews_es.csv")
fr_spviews <- read.csv("Monthlyviews_fr.csv")
ja_spviews <- read.csv("Monthlyviews_ja.csv")
it_spviews <- read.csv("Monthlyviews_it.csv")
pt_spviews <- read.csv("Monthlyviews_pt.csv")
ru_spviews <- read.csv("Monthlyviews_ru.csv")
zh_spviews <- read.csv("Monthlyviews_zh.csv")

# Getting total page views for each sp in each language
# Summarize total views by species
total_views_species_zh <- zh_spviews %>%
  group_by(article) %>%
  summarise(totalviews_zh = sum(views, na.rm = TRUE))

# Changing "article" column to "Title" to match qid df
total_views_species_zh <- total_views_species_zh %>%
  rename(Title = article)

# Changing title for title df
title_zh$Title <- gsub(" ", "_", title_zh$Title)

# Joining with qid df for every language
sp_totalviews_zh <- inner_join(title_zh, total_views_species_zh, by = "Title")

# Removing data with 0 page views
sp_totalviews_zh <- sp_totalviews_zh %>%
  filter(totalviews_zh!=0)

# Saving the dfs
write.csv(sp_totalviews_zh, "Totalviews_zh.csv", row.names = F)
write.csv(zh_sp_views, "Monthlyviews_zh.csv", row.names = F)

# Removing unnecessary files
remove(pageviews_list, ar_sp_views, de_sp_views, en_sp_views, es_sp_views, fr_sp_views, 
       it_sp_views, ja_sp_views, pt_sp_views, ru_sp_views, zh_sp_views, title_ar, title_de, 
       title_en, title_es, title_fr, title_it, title_ja, title_pt, title_ru, title_zh)

#-----##Part5: Preparing the dataset for analysis-----

# Reading dataset if required
th_nt_sp_data_cl <- read.csv("WCVP+QID_all_lang.csv")
sp_totalviews_ar <- read.csv("Totalviews_ar.csv")
sp_totalviews_de <- read.csv("Totalviews_de.csv")
sp_totalviews_en <- read.csv("Totalviews_en.csv")
sp_totalviews_es <- read.csv("Totalviews_es.csv")
sp_totalviews_fr <- read.csv("Totalviews_fr.csv")
sp_totalviews_it <- read.csv("Totalviews_it.csv")
sp_totalviews_ja <- read.csv("Totalviews_ja.csv")
sp_totalviews_pt <- read.csv("Totalviews_pt.csv")
sp_totalviews_ru <- read.csv("Totalviews_ru.csv")
sp_totalviews_zh <- read.csv("Totalviews_zh.csv")

# Conjoining all datasets to get only common species 
# List of all dfs
list_of_dfs <- list(th_nt_sp_data_cl, sp_totalviews_ar, sp_totalviews_de, sp_totalviews_en, 
                    sp_totalviews_es, sp_totalviews_fr, sp_totalviews_it, 
                    sp_totalviews_ja, sp_totalviews_pt, sp_totalviews_ru, sp_totalviews_zh)

# Join all data frames by 'QID'
th_views_allsp_df <- reduce(list_of_dfs, left_join, by = "QID")

# Removing unnecessary columns
th_views_allsp_df <- th_views_allsp_df %>%
  select(-c(Title.x, Title.y, Title.x.x, Title.y.y, Title.x.x.x, Title.y.y.y, 
            Title.x.x.x.x, Title.y.y.y.y, Title.x.x.x.x.x, Title.y.y.y.y.y))

# Mutate to create factors
th_views_allsp_df <- th_views_allsp_df %>%
  mutate(
    ThreatStatus = case_when(
      redlistCategory %in% c("Critically Endangered", "Endangered", "Vulnerable") ~ "Threatened",
      redlistCategory %in% c("Near Threatened", "Least Concern", "Lower Risk/near threatened", 
                    "Lower Risk/conservation dependent", "Lower Risk/least concern") ~ "Non-threatened",
      TRUE ~ "Data Deficient"
    )
  )

# Filtering df without QID, scientific name and duplicates
th_views_allsp_df <- th_views_allsp_df %>%
  filter(!is.na(scientificName)) %>%
  filter(!is.na(QID)) %>%
  distinct(QID, .keep_all = T) %>% 
  filter(!is.na())

# Saving the dataset
write.csv(th_views_allsp_df, "WCVP_views_threatstatus.csv", row.names = F)

#-----##Part6: Visualising the dataset-----

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

# Preparing annotation df 
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
colour_code <- c("Threatened" = "orangered2", 
                 "Non-threatened" = "palegreen3",
                 "Data Deficient" = "grey")

# Re-arranging legend order
legend_order <- c("Threatened", 
                  "Non-threatened",
                  "Data Deficient")

# Update your ggplot code
ggplot(th_views_allsp_long_df, aes(y = ThreatStatus, x = PageViews, group = LanguageFull, fill = ThreatStatus)) +
  ggdist::stat_halfeye(point_size = 6,
                       linewidth  = 10,
                       slab_linewidth = 0.5,
                       slab_color = "black") +
  scale_x_log10() +
  geom_text(data = annots, aes(label = label, x = adjusted_x, y = ThreatStatus),
            inherit.aes = FALSE, hjust = 1, vjust = 2, size = 6) +
  facet_wrap(~LanguageFull, scales = "free", ncol = 2, nrow = 5) +  # Faceting by language
  labs(x = "Log pageviews", y = "Threat status", 
       title = "Threat status against log pageviews") +
  scale_fill_manual(values = colour_code, name = "ThreatStatus", limits = legend_order) + 
  theme_ggdist(base_size = 25) +
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
    title = "Threat status",
    title.position = "top",
    title.hjust = 0.5, # Center the title
    label.position = "right", # Position labels to the right of the keys
    keyheight = unit(0.5, "lines"), # Increase the height of the legend keys
    keywidth = unit(1, "lines"), # Increase the width of the legend keys
    ncol = 1, # Arrange legend items in a single column
    byrow = TRUE # Arrange legend items by row
  ))


#-----------xxxxx---------------

















