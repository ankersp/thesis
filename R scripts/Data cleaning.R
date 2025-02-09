#Setting working directory
setwd("G:/My Drive/KU/kandidat/speciale/R")

#Loading packages
library("tidyverse")
library("quanteda")
library("conflicted")
library("factoextra")
library("gt")
library("patchwork")
library("quanteda")
library("quanteda.textplots")
library("quanteda.textstats")
library("dplyr")

#Loading data
data <- read_csv("ESG.csv")

#####DATA CLEANING########
#Cleaning data
#Function to remove all characters between every pair of <>
remove_html_tags <- function(text) {
  clean_text <- gsub("<[^>]*>", "", text)
  clean_text <- gsub("\\s+", " ", clean_text)
  return(trimws(clean_text))
}

#Applying function to remove html tags
data$Link <- sapply(data$Link, remove_html_tags)

#Function to remove observations that refer to a link
contains_link <- function(text) {
  grepl("https?://|www\\.", text)
}

#removing all observations that contains less than 100 characters
filtered_data <- data %>%
  dplyr::filter(nchar(Link) > 100)

#Filtering data
#Removing all observations that contain links within 500 characters
filtered_data <- filtered_data %>%
  dplyr::filter(!(nchar(Link) < 500 & contains_link(Link)))


# Function to check if text contains variations of "henvis*" or "refer*"
contains_keywords <- function(text) {
  grepl("(henvis(ning|er|e)?)|(refer(ence|s)?)", as.character(text), ignore.case = TRUE)
}

# Removing observations that contain "henvis*" or "refer*" and less than 500 characters
keyword_filtered_data <- filtered_data %>%
  dplyr::filter(contains_keywords(Link) & nchar(Link) < 500)

#removing keyword_filtered_data from filtered_data
filtered_data <- filtered_data %>%
  dplyr::filter(!(contains_keywords(Link) & nchar(Link) < 500))

#Adding doc_id
filtered_data <- filtered_data %>%
  mutate(doc_id = paste0("doc_", row_number()))



#æøå
# Function to check if text contains any of the characters "æ", "ø", or "å"
contains_special_characters <- function(text) {
  grepl("[æøå]", as.character(text), ignore.case = TRUE)
}

# Counting the number of observations that contain "æ", "ø", or "å"
count_special_characters <- filtered_data %>%
  dplyr::filter(contains_special_characters(Link)) %>%
  dplyr::count()

print(count_special_characters)

# Separate df for danish texts contianing æ, ø or å
danish_texts <- filtered_data %>%
  dplyr::filter(contains_special_characters(Link))

#######DUPLICATES#######
#Checking that CVR + year + result is unique
filtered_data %>%
  group_by(CVR, År, Resultat) %>%
  summarise(n = n()) %>%
  dplyr::filter(n > 1)

#separate df for duplicates
duplicates <- filtered_data %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(n() > 1) %>%
  ungroup()

# Define a function to check for Danish letters
contains_danish_letters <- function(text) {
  grepl("[æø]", text, ignore.case = TRUE)
}

# Filter duplicates by checking for Danish letters
cleaned_duplicates <- duplicates %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(if (any(!contains_danish_letters(Link))) {
    !contains_danish_letters(Link)
  } else {
    TRUE
  }) %>%
  ungroup()

#discarded duplicates
discarded <- duplicates %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(if (any(!contains_danish_letters(Link))) {
    contains_danish_letters(Link)
  } else {
    FALSE
  }) %>%
  ungroup() %>%
  select(doc_id)

#separate df for still duplicates again
duplicates2 <- cleaned_duplicates %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(n() > 1) %>%
  ungroup()

#Round 2 of filtering duplicates
# Define a function to check for the word "samfundsansvar"
contains_samfundsansvar <- function(text) {
  grepl("samfundsansvar", text, ignore.case = TRUE)
}

# Filter duplicates based on "samfundsansvar" presence
cleaned_dups2 <- cleaned_duplicates %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(n() > 1) %>%  # Only keep groups with more than one row
  dplyr::filter(if (any(contains_samfundsansvar(Link)) & !all(contains_samfundsansvar(Link))) {
    !contains_samfundsansvar(Link)
  } else {
    TRUE
  }) %>%
  ungroup()

#discarded duplicates2
discarded2 <- cleaned_duplicates %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(if (any(!contains_samfundsansvar(Link))) {
    contains_samfundsansvar(Link)
  } else {
    FALSE
  }) %>%
  ungroup() %>%
  select(doc_id)

#duplicates round 3
duplicates3 <- cleaned_dups2 %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(n() > 1) %>%
  ungroup()

#manually checking the 60 duplicates and listing the ones to be discarded by doc_id and creating df
discarded3 <- data.frame(doc_id = c("doc_252", "doc_965", "doc_1015", "doc_1313", "doc_1530", 
                                    "doc_1711", "doc_3462", "doc_3611", "doc_3770", "doc_3905", 
                                    "doc_4438", "doc_4786", "doc_5035", "doc_5742", "doc_6470", 
                                    "doc_6586", "doc_7072", "doc_7157", "doc_8642", "doc_8643", 
                                    "doc_9699", "doc_10062", "doc_10087", "doc_10351", "doc_10374", 
                                    "doc_10745", "doc_11527", "doc_11751", "doc_507", "doc_6461",
                                    "doc_7009"))


# Now bind rows of all the discarded docs
discarded_all <- bind_rows(discarded, discarded2, discarded3)


# Filter out the discarded docs from the cleaned data
cleaned_data <- filtered_data %>%
  anti_join(discarded_all, by = "doc_id")

#separate df for still duplicates again
duplicates4 <- filtered_data %>%
  group_by(CVR, År, Resultat) %>%
  dplyr::filter(n() > 1) %>%
  ungroup()

#######Combining non-duplicates##########
#Three pairs of docs should be combined as texts are non duplicates
# English text pairs
doc_pairs_eng <- list(
  c("doc_6453", "doc_6461")
)

# Danish text pairs
doc_pairs_dk <- list(
  c("doc_506", "doc_507"),
  c("doc_6999", "doc_7009")
)

concatenated_texts_eng <- lapply(doc_pairs_eng, function(pair) {
  # Filter the data for the current pair of document IDs
  pair_data <- filtered_data %>%
    dplyr::filter(doc_id %in% pair)
  
  # Concatenate the 'Link' text for both documents in the pair
  combined_text <- paste(pair_data$Link, collapse = " ")
  
  # Return the concatenated text
  return(combined_text)
})


#as df
combined_doc_pairs_eng <- data.frame(
  doc_id = sapply(doc_pairs_eng, function(pair) pair[1]),  # Extract the first doc_id from each pair
  Link = unlist(concatenated_texts_eng),  # Convert the list of concatenated texts to a vector
  stringsAsFactors = FALSE
)


#combined doc pairs replace existing rows in cleaned_data
cleaned_data <- cleaned_data %>%
  rows_update(combined_doc_pairs_eng, by = "doc_id")

####MERGE W Translation###########
#Translation is done in separate script
#Reading translated data
translated_data <- read_csv("translated_texts.csv")

#replacing concatenated texts in translated df
concatenated_texts_dk <- lapply(doc_pairs_dk, function(pair) {
  # Filter the data for the current pair of document IDs
  pair_data <- translated_data %>%
    dplyr::filter(doc_id %in% pair)
  
  # Concatenate the 'Link' text for both documents in the pair
  combined_text <- paste(pair_data$Link, collapse = " ")
  
  # Return the concatenated text
  return(combined_text)
})


#as df
combined_doc_pairs_dk <- data.frame(
  doc_id = sapply(doc_pairs_dk, function(pair) pair[1]),  # Extract the first doc_id from each pair
  Link = unlist(concatenated_texts_dk),  # Convert the list of concatenated texts to a vector
  stringsAsFactors = FALSE
)


#combined doc pairs replace existing rows in cleaned_data
translated_data <- translated_data %>%
  rows_update(combined_doc_pairs_dk, by = "doc_id")

# Replace the 'translation' column with the value from 'Link' for doc_506 and doc_6999
translated_data <- translated_data %>%
  mutate(translation = if_else(doc_id %in% c("doc_506", "doc_6999"), Link, translation))

#joining translated data with cleaned data by doc_id
eng_data <- cleaned_data %>%
  left_join(translated_data %>% 
              select(doc_id, translation), by = "doc_id")


#New column joining english and translated texts, if translation is NA, use original text
eng_data$final_text <- ifelse(is.na(eng_data$translation), eng_data$Link, eng_data$translation)

#Df with relevant columns
eng_data_final <- eng_data %>%
  select(doc_id, År, CVR, final_text, Omsætning, Resultat)

#writing to csv
write_csv(eng_data_final, "data_eng.csv")

#counting unique CVR
length(unique(eng_data_final$CVR))



######METADATA#####
#loading metadata, headers not true
metadata <- read_csv("metadata.csv", col_names = FALSE)

#removing last two columns
metadata <- metadata %>%
  select(-X9, -X10)

#colnames
colnames(metadata) <- c("CVR", "Corporate form", "Region", "Industry", "Year", "Class", "Result", "Revenue")

#reading data
data <- read_csv("data_eng.csv")

#renaming vars to match metadata
data$Year = data$År
data$År = NULL
data$Revenue = data$Omsætning
data$Omsætning = NULL
data$Result = data$Resultat
data$Resultat = NULL

#result as num
data$Result <- as.numeric(data$Result)
metadata$Result <- as.numeric(metadata$Result)

#remove duplicates in metadata
metadata <- metadata %>% distinct(CVR, Year, Result, .keep_all = TRUE)

#joining data
joined_data <- data %>%
  left_join(metadata, by = c("CVR" = "CVR", "Year" = "Year", "Result" = "Result"))

#checking for duplicates
joined_dups <- joined_data %>%
  group_by(doc_id) %>%
  summarise(n = n()) %>%
  dplyr::filter(n > 1)

# Remove all occurrences of doc_3635
joined_data <- joined_data %>%
  dplyr::filter(doc_id != "doc_3635")

# Keep only the first occurrence of doc_2118, doc_1220, and doc_5032
joined_data <- joined_data %>%
  group_by(doc_id) %>%
  dplyr::filter(!(doc_id %in% c("doc_2118", "doc_1220", "doc_5032")) | row_number() == 1) %>%
  ungroup()

#remove revenue.y and rename revnue.x to Revenue
joined_data <- joined_data %>%
  select(-Revenue.y) %>%
  rename(Revenue = Revenue.x)

#checking for duplicates by CVR, text and year
joined_dups <- joined_data %>%
  group_by(CVR, final_text, Year) %>%
  summarise(n = n()) %>%
  dplyr::filter(n > 1)

#remove doc_487
joined_data <- joined_data %>%
  dplyr::filter(doc_id != "doc_487")

#remove CVR 41416998
joined_data <- joined_data %>%
  dplyr::filter(CVR != 41416998)

#checking for duplicates by cvr and year
joined_dups <- joined_data %>%
  group_by(CVR, Year, Result) %>%
  summarise(n = n()) %>%
  dplyr::filter(n > 1)

#remove doc_4614, doc_4458, doc_3967, doc_7546, doc_11342, doc_3895, doc_10403, doc_6591, doc_1912, doc_3646, doc_2721, doc_2301, doc_207, doc_5765, doc_1103, doc_11151, doc_5032, doc_7926, doc_11031, doc_9670, doc_10567, doc_3278
joined_data <- joined_data %>%
  dplyr::filter(!(doc_id %in% c("doc_4614", "doc_4458", "doc_3967", "doc_7546", "doc_11342", "doc_3895", "doc_10403", "doc_6591", "doc_1912", "doc_3646", "doc_2721", "doc_2301", "doc_207", "doc_5765", "doc_1103", "doc_11151", "doc_5032", "doc_7926", "doc_11031", "doc_9670", "doc_10567", "doc_3278")))


#######Combining non-duplicates##########
#Three pairs of docs should be combined as texts are non duplicates
# Define the pairs of document IDs for 2023 and 2022
doc_pairs_2023 <- list(
  c("doc_1596", "doc_2796", "doc_3760")
)

doc_pairs_2022 <- list(
  c("doc_5172", "doc_5659", "doc_5713")
)

# Function to concatenate the 'Link' text for each group of document pairs
concatenate_docs <- function(doc_pairs, data) {
  concatenated_texts <- lapply(doc_pairs, function(pair) {
    # Filter the data for the current pair of document IDs
    pair_data <- data %>%
      dplyr::filter(doc_id %in% pair)
    
    # Concatenate the 'Link' text for the documents in the pair
    combined_text <- paste(pair_data$final_text, collapse = " ")
    
    # Return the concatenated text
    return(combined_text)
  })
  
  # Create a data frame to store the concatenated texts and document IDs
  combined_doc_pairs <- data.frame(
    doc_id = sapply(doc_pairs, function(pair) pair[1]),  # Keep the first doc_id from each pair
    final_text = unlist(concatenated_texts),  # Convert the list of concatenated texts to a vector
    stringsAsFactors = FALSE
  )
  
  return(combined_doc_pairs)
}

# Concatenate documents for 2023 and 2022
combined_doc_pairs_2023 <- concatenate_docs(doc_pairs_2023, joined_data)
combined_doc_pairs_2022 <- concatenate_docs(doc_pairs_2022, joined_data)

# Combine both 2023 and 2022 document merges into one data frame
combined_doc_pairs <- rbind(combined_doc_pairs_2023, combined_doc_pairs_2022)

#update joined data
joined_data <- joined_data %>%
  left_join(combined_doc_pairs %>% 
              select(doc_id, final_text), by = "doc_id")


# Replace final_text.y with final_text.x only if final_text.y is NA
joined_data <- joined_data %>%
  mutate(final_text = ifelse(is.na(final_text.y), final_text.x, final_text.y)) %>%
  select(-final_text.x, -final_text.y)  # Remove the old columns after merging

#remove doc_5659, doc_5713, doc_2796, doc_3760
joined_data <- joined_data %>%
  dplyr::filter(!(doc_id %in% c("doc_5659", "doc_5713", "doc_2796", "doc_3760")))

#checking for duplicates by cvr and year
joined_dups <- joined_data %>%
  group_by(final_text, Year, Result) %>%
  summarise(n = n()) %>%
  dplyr::filter(n > 1)

#writing final cleaned data to csv
write_csv(joined_data, "data_final.csv")


