
library(deeplr)
deeplr::usage("API-KEY")

# Initialize an empty column 'translation' to store the translated results
danish_texts$translation <- NA

# Your DeepL API key
api_key <- "API-KEY"

translate_text_safe <- function(text, row_num) {
  # Ensure the text is encoded in UTF-8
  text <- iconv(text, to = "UTF-8")
  
  tryCatch({
    # Translate the text
    translation <- deeplr::translate(
      text = text,
      target_lang = "EN",
      source_lang = "DA",
      auth_key = "API-KEY"
    )
    return(translation)
  }, error = function(e) {
    # Print a warning and return NA if the translation fails
    warning(paste("Error translating row", row_num, ":", e$message))
    return(NA)
  })
}



for (i in 5001:7517) {
  print(paste("Processing row", i))
  
  # Translate the text
  translation <- translate_text_safe(danish_texts$Link[i], i)
  
  # Store the translation in the dataframe
  danish_texts$translation[i] <- translation
  
  # Print status to track progress
  print(paste("Successfully processed row", i))
  
  # Delay between API calls to prevent rate limiting (adjust as needed)
  Sys.sleep(3)
}



#Check for NAs
sum(is.na(danish_texts$translation))

#identify doc id NA row
danish_texts[is.na(danish_texts$translation),]

#Dropping all rows with CVR "39551640" except doc_10485
#The rest use the exemption and refers to reports
danish_texts <- danish_texts %>%
  dplyr::filter(!(CVR == "39551640" & doc_id != "doc_10485"))

#Dropping observations that refer to the exemption and reports
#doc_4987, doc_5638, doc_10791, doc_11210, doc_11404, doc_11577
danish_texts <- danish_texts %>%
  dplyr::filter(!(doc_id %in% c("doc_4987", "doc_5638", "doc_10791", "doc_11210", "doc_11404", "doc_11577")))


#write csv
write.csv(danish_texts, "translated_texts.csv", row.names = FALSE)

#