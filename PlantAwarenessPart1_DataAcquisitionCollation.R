#----#Loading required packages and libraries-----

install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
install.packages("ggdist")
install.packages("rWCVP")
remotes::install_github('matildabrown/rWCVPdata')
library(rWCVP)
library(ggdist)
library(ggplot2)
library(tidyverse)
library(httr)
library(jsonlite)

#-----##Part1: Downloading and curating WCVP list------   

# Downloading sp. checklist from wcvp
wcvp <- wcvp_checklist(taxon = NULL, 
                       synonyms = F,
                       extinct = F,
                       infraspecies = F,
                       introduced = F,
                       location_doubtful = F,
                       report_type = c("alphabetical", "taxonomic")) 

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
    language = "ru", #Change the language as required
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
th_nt_sp_data_cl$QID_ru <- sapply(sci_names, get_wikidata_qid_via_search)

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
end_date <- "20250630"

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


#-----#Part6: Curating WCUP dataset-----

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

#-----#Part7: Merging WCUP with main df-----

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

#-----#Part8: Integrating range distribution------

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

# Saving df
write.csv(th_use_area_df, "Threat&use+area_codes.csv", row.names = F)


#-----#Part9: Preparing lang_spatial_sp_df-------

# Loading th_use_area_df
th_use_area_df <- read.csv("Threat&use+area_codes.csv")

# Downloading spatial data
wcvpsp_df <- wcvp_checklist(synonyms = F, hybrids = F, infraspecies = F) %>% 
  filter(plant_name_id %in% th_use_area_df$wcvpv12_id) %>% 
  group_by(plant_name_id) %>% 
  select(plant_name_id, continent_code_l1,	continent,	region_code_l2,	region,	area_code_l3,	area,	occurrence_type) %>% 
  rename(wcvpv12_id = plant_name_id)

# Saving df
write.csv(wcvpsp_df, "WCVP+lang-codes.csv", row.names = F)

# Loading lang_spat_df
lang_spat_df <- read.csv("wgsrpd_lang_df.csv")

# Curating df
lang_spat_df <- lang_spat_df %>% 
  rename(area_code_l3 = LEVEL3_COD,
         area = LEVEL3_NAM)

# Merging the dfs
lang_spat_combo_df <- left_join(wcvpsp_df, lang_spat_df, by = "area_code_l3", "area") %>% 
  select(-area.y, -country) %>% 
  rename(area = area.x)

lang_spatial_sp_df <- left_join(lang_spat_combo_df, th_use_area_df, by = "wcvpv12_id")

# Filtering df without QID, scientific name and duplicates
lang_spatial_sp_df <- lang_spatial_sp_df %>%
  filter(!is.na(scientificName)) %>%
  filter(!is.na(QID)) %>%
  distinct(QID, .keep_all = T) %>% 
  filter(!is.na())

# Saving dfs
write.csv(lang_spat_combo_df, "Lang+spatial_df.csv")
write.csv(lang_spatial_sp_df, "Threat_use_area_spatial.csv")

#-----------xxxxx---------------