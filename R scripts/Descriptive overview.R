setwd("G:/My Drive/KU/kandidat/speciale/R")

#read metadata
metadata <- read.csv("G:/My Drive/KU/kandidat/speciale/R/metadata.csv", header = TRUE)

#read classifications
classifications <- read.csv("G:/My Drive/KU/kandidat/speciale/R/esg_grouped.csv", header = TRUE)

#add classifications$total var to metadata by doc_id
metadata <- left_join(metadata, classifications[, c("doc_id", "sentences")], by = "doc_id")

#descriptive overview
summary(metadata$final_text)

descriptive_overview <- metadata %>%
  filter(Class != "Unknown") %>%
  mutate(
    text_length = nchar(final_text)  # Calculate text length
  ) %>%
  group_by(Class) %>%
  summarise(
    # Text statistics
    total_docs = n(),
    avg_chars = mean(text_length, na.rm = TRUE),
    max_chars = max(text_length, na.rm = TRUE),
    min_chars = min(text_length, na.rm = TRUE),
    std_dev_chars = sd(text_length, na.rm = TRUE),
    
    # Sentences statistics
    avg_sentences = mean(sentences, na.rm = TRUE),
    max_sentences = max(sentences, na.rm = TRUE),
    min_sentences = min(sentences, na.rm = TRUE),
    std_dev_sentences = sd(sentences, na.rm = TRUE)
  )

# Pivot the table to have rows as statistics and columns as classes
descriptive_pivot <- descriptive_overview %>%
  pivot_longer(
    cols = -Class,                          
    names_to = "Statistic",                 
    values_to = "Value"                   
  ) %>%
  pivot_wider(
    names_from = Class,                     
    values_from = Value                     
  )

# Round numerical values for better presentation
descriptive_pivot <- descriptive_pivot %>%
  mutate(across(where(is.numeric), ~ round(., 0)))  

# Generate LaTeX table
latex_table <- descriptive_pivot %>%
  knitr::kable(
    format = "latex",                       # Output format is LaTeX
    col.names = c("Statistic", unique(descriptive_overview$Class)),  # Use unique class names as column headers
    caption = "Descriptive Overview of Final Text by Class"  # Add caption
  )

# Output LaTeX
cat(latex_table)


#########################
# Sample 4 rows from the dataset
set.seed(123)  # Set seed for reproducibility
sampled_rows <- esg_classifications[sample(nrow(esg_classifications), 4), ]

# Truncate the text column to the first 50 characters and add "..."
sampled_rows$text <- paste0(substr(sampled_rows$text, 1, 100), "...")

# Combine labels into a single column
sampled_rows$Labels <- apply(sampled_rows[, c("environmental_label", "social_label", "governance_label")], 1, paste, collapse = ", ")

# Combine probabilities into a single column
sampled_rows$Probs. <- apply(sampled_rows[, c("environmental_score", "social_score", "governance_score")], 1, function(x) paste(sprintf("%.4f", x), collapse = ", "))

# Select and rename columns in the desired order
sampled_rows <- sampled_rows[, c("doc_id", "sentence_id", "text", "Labels", "Probs.")]
colnames(sampled_rows) <- c("Document ID", "Sentence ID", "Text", "Labels", "Probs.")

# Export to LaTeX
library(xtable)
latex_table <- xtable(sampled_rows)
print(latex_table, include.rownames = FALSE)


#############################
#Performance evaluation
#weighted averages
esg_evaluation <- read.csv("./esg_evaluation_metrics/esg_model_comparison_summary_weighted.csv")
extensive_evaluation <- read.csv("./esg_evaluation_metrics/extensive_model_comparison_summary_weighted.csv")

combined_df <- rbind(evaluation, extensive_evaluation)
colnames(combined_df) <- c("Model", "Precision", "Recall", "F1-Score", "Accuracy")
latex_table <- xtable(combined_df, caption = "Combined Evaluation Metrics, weighted averages")
print(latex_table, type = "latex", include.rownames = FALSE)

#macro averages
esg_evaluation_macro <- read.csv("./esg_evaluation_metrics/esg_model_comparison_summary_macro.csv")
extensive_evaluation_macro <- read.csv("./esg_evaluation_metrics/extensive_model_comparison_summary_macro.csv")

combined_df <- rbind(esg_evaluation_macro, extensive_evaluation_macro)
colnames(combined_df) <- c("Model", "Precision", "Recall", "F1-Score", "Accuracy")
latex_table <- xtable(combined_df, caption = "Combined Evaluation Metrics, macro averages")
print(latex_table, type = "latex", include.rownames = FALSE)

