#Setting working directory
setwd("G:/My Drive/KU/kandidat/speciale/R")

#install.packages(c("tidyverse", "quanteda", "conflicted", "factoextra", "gt", "patchwork", "quanteda", "quanteda.textplots", "quanteda.textstats", "dplyr", "seededlda", "stm", "stminsights", "ggplot2", "preText", "ggraph", "ldatuning", "tidytext", "reticulate", "keras"))


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
library("seededlda")
library("stm")
library("stminsights")
library("ggplot2")
library("preText") #pak::pkg_install("matthewjdenny/preText")
library("ggraph")
library("ldatuning")
library("tidytext")
library("xtable")
library("plm")
library("yardstick")
library("pROC")
library("extrafont")
library("fixest")
library("scales")
library("modelsummary")
library("pwr")
library("xtable")
conflicts_prefer(
  dplyr::filter,
  seededlda::terms
)

rm(list = ls())

#load esg classifications
esg <- read.csv("G:/My Drive/KU/kandidat/speciale/R/esg_classifications.csv", header = TRUE, sep = ",")
head(esg)

#load unclassified data
unclassified <- read.csv("G:/My Drive/KU/kandidat/speciale/R/sentences_long_format.csv", header = TRUE, sep = ",")
head(unclassified)

#adding doc_id, CVR, and year to classifications by id
esg <- merge(esg, unclassified[, c("id", "doc_id", "CVR", "Year")], by = "id")
head(esg)

#load action and specificity classifications
act_spec <- read.csv("G:/My Drive/KU/kandidat/speciale/R/environmental_action_specificity.csv", header = TRUE, sep = ",")

#adding action and specificity classifications to esg
esg <- left_join(esg, act_spec[, c("id", "action_label", "action_score", "specificity_label", "specificity_score")], by = "id")

#adding  a new column for specific actions at the sentence level
esg <- esg %>%
  mutate(
    specific_action = case_when(
      action_label == "LABEL_1" & specificity_label == "LABEL_1" ~ 1,  # Specific action
      action_label == "LABEL_1" & specificity_label == "LABEL_0" ~ 0,  # Non-specific action
      TRUE ~ NA_real_  # Keep NA for rows where action_label is not "LABEL_1"
    )
  )

#grouping classifications by doc_id and calculating percentages
esg_grouped <- esg %>%
  group_by(doc_id) %>%
  summarise(
    total = n(),  
    env = sum(env_label == "LABEL_1", na.rm = TRUE) / total,
    soc = sum(soc_label == "LABEL_1", na.rm = TRUE) / total,
    gov = sum(gov_label == "LABEL_1", na.rm = TRUE) / total,
    
    # Proportion of environmental sentences that contain actions
    action = ifelse(
      sum(env_label == "LABEL_1", na.rm = TRUE) == 0, 
      NA, 
      sum(action_label == "LABEL_1" & env_label == "LABEL_1", na.rm = TRUE) / 
        sum(env_label == "LABEL_1", na.rm = TRUE)
    ),
    
    # Specificity remains calculated based on environmental content, not actions
    specificity = ifelse(
      all(is.na(specificity_label)), 
      NA, 
      sum(specificity_label == "LABEL_1" & env_label == "LABEL_1", na.rm = TRUE) / 
        sum(env_label == "LABEL_1", na.rm = TRUE)
    ),
    
    # Cheap Talk Index (CTI) calculated relative to actions
    cti = ifelse(
      sum(action_label == "LABEL_1" & env_label == "LABEL_1", na.rm = TRUE) == 0, 
      NA, 
      sum(specific_action == 0, na.rm = TRUE) / 
        sum(action_label == "LABEL_1" & env_label == "LABEL_1", na.rm = TRUE)
    )
  )


head(esg_grouped)

#loading metadata
metadata <- read.csv("G:/My Drive/KU/kandidat/speciale/R/metadata.csv", header = TRUE)

#add selected metadata to esg_grouped
esg_grouped <- merge(esg_grouped, metadata[, c("doc_id", "CVR", "Year", "Revenue", "Industry", "Class", "Corporate_structure", "Region", "Employees")], by = "doc_id")

#rename "total" to "sentences" in the dataset
esg_grouped <- esg_grouped %>%
  rename(sentences = total)

#adding env-soc ratio to the dataset
esg_grouped <- esg_grouped %>%
  mutate(env_soc_ratio = (env + 1) / (soc + 1))

#write esg_grouped to csv
write.csv(esg_grouped, "G:/My Drive/KU/kandidat/speciale/R/esg_grouped.csv", row.names = FALSE)


######Descriptive trends plot 1#########
#loading data
data <- read.csv("G:/My Drive/KU/kandidat/speciale/R/esg_grouped.csv", header = TRUE)

#######plot 1
#setting ggplot theme
theme_set(
  theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
      axis.title.x = element_text(size = 16, margin = margin(t = 10), color = "black"),
      axis.title.y = element_text(size = 16, margin = margin(r = 10), color = "black"),
      axis.text.x = element_text(size = 14, color = "black"),  # Force black color for x-axis
      axis.text.y = element_text(size = 14, color = "black"),  # Force black color for y-axis
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA)
    )
)

# Use colorblind-friendly color palette
okabe_ito <- unname(palette.colors(palette = "Okabe-Ito")[c(2:8, 1)])
options(ggplot2.continuous.color = okabe_ito)
options(ggplot2.continuous.colour = okabe_ito)
options(ggplot2.continuous.fill = okabe_ito)
options(ggplot2.discrete.color = okabe_ito)
options(ggplot2.discrete.colour = okabe_ito)
options(ggplot2.discrete.fill = okabe_ito)


# Define Okabe-Ito colorblind-friendly palette and replace yellow for CTI
okabe_ito_adjusted <- unname(palette.colors(palette = "Okabe-Ito")[c(2:8, 1)])  # Excludes the first yellow color
okabe_ito_adjusted[which(names(okabe_ito_adjusted) == "yellow")] <- "purple"  # Replace yellow with purple

# Apply updated color palette globally
options(
  ggplot2.continuous.color = okabe_ito_adjusted,
  ggplot2.continuous.colour = okabe_ito_adjusted,
  ggplot2.continuous.fill = okabe_ito_adjusted,
  ggplot2.discrete.color = okabe_ito_adjusted,
  ggplot2.discrete.colour = okabe_ito_adjusted,
  ggplot2.discrete.fill = okabe_ito_adjusted
)

#plot folder
# Create a folder (if it doesn't already exist)
output_folder <- "plots"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}


# Filter data to exclude 2017 and 2024
filtered_data <- data %>%
  filter(!(Year %in% c(2017, 2024)))

# Aggregating data by Year with standard errors
data_by_year <- filtered_data %>%
  group_by(Year) %>%
  summarise(
    mean_env = mean(env, na.rm = TRUE),
    se_env = sd(env, na.rm = TRUE) / sqrt(n()),  
    mean_soc = mean(soc, na.rm = TRUE),
    se_soc = sd(soc, na.rm = TRUE) / sqrt(n()), 
    mean_gov = mean(gov, na.rm = TRUE),
    se_gov = sd(gov, na.rm = TRUE) / sqrt(n())
  )


#manually assigning colors
custom_colors <- c(
  "Environmental" = okabe_ito[3], # Green
  "Social" = okabe_ito[2],        # Orange
  "Governance" = okabe_ito[6]     # Blue
)

#plotting
ggplot(data_by_year, aes(x = Year)) +
  geom_line(aes(y = mean_env, color = "Environmental"), size = 1.2) +
  geom_ribbon(aes(ymin = mean_env - se_env, ymax = mean_env + se_env, fill = "Environmental"), alpha = 0.2) +
  geom_line(aes(y = mean_soc, color = "Social"), size = 1.2) +
  geom_ribbon(aes(ymin = mean_soc - se_soc, ymax = mean_soc + se_soc, fill = "Social"), alpha = 0.2) +
  geom_line(aes(y = mean_gov, color = "Governance"), size = 1.2) +
  geom_ribbon(aes(ymin = mean_gov - se_gov, ymax = mean_gov + se_gov, fill = "Governance"), alpha = 0.2) +
  
  # Labels
  labs(
    title = "",
    x = "Year",
    y = "Mean Percentage",
    color = NULL,
    fill = NULL
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1)))

# Export the plot
plot_path <- file.path(output_folder, "esg_trends_plot.pdf")
ggsave(
  filename = plot_path,
  plot = last_plot(),  
  width = 8,          
  height = 6,         
  dpi = 300          
)

######Descriptive trends plot 2##########
#plot 2
#plotting action and specificity
data_by_year <- filtered_data %>%
  filter(env > 0) %>%
  group_by(Year) %>%
  summarise(
    mean_action = mean(action, na.rm = TRUE),
    se_action = sd(action, na.rm = TRUE) / sqrt(n()),  
    mean_specificity = mean(specificity, na.rm = TRUE),
    se_specificity = sd(specificity, na.rm = TRUE) / sqrt(n())
  )


#manually assigning colors
custom_colors <- c(
  "action" = okabe_ito[4], # yellow
  "specificity" = okabe_ito[7] # rose
)

#plotting
ggplot(data_by_year, aes(x = Year)) +
  geom_line(aes(y = mean_action, color = "action"), size = 1.2) +
  geom_ribbon(aes(ymin = mean_action - se_action, ymax = mean_action + se_action, fill = "action"), alpha = 0.2) +
  geom_line(aes(y = mean_specificity, color = "specificity"), size = 1.2) +
  geom_ribbon(aes(ymin = mean_specificity - se_specificity, ymax = mean_specificity + se_specificity, fill = "specificity"), alpha = 0.2) +
  labs(
    title = "",
    x = "Year",
    y = "Mean Percentage",
    color = NULL,
    fill = NULL
  ) +
  scale_color_manual(values = custom_colors) +
  scale_fill_manual(values = custom_colors) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.5)))

# Export the plot
plot_path <- file.path(output_folder, "act_spec_trends_plot.pdf")
ggsave(
  filename = plot_path,
  plot = last_plot(),  
  width = 8,          
  height = 6,         
  dpi = 300          
)


######Descriptive overview of variables##########
#descriptive overview of variables

# Calculate descriptive statistics
descriptive_stats <- esg_grouped %>%
  summarise(
    Variable = c("env", "soc", "gov", "env_soc_ratio", "action", "specificity", "cti"),
    n = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), function(x) sum(!is.na(x))),
    mean = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), mean, na.rm = TRUE),
    sd = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), sd, na.rm = TRUE),
    min = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), min, na.rm = TRUE),
    p25 = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), quantile, 0.25, na.rm = TRUE),
    p50 = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), quantile, 0.50, na.rm = TRUE),
    p75 = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), quantile, 0.75, na.rm = TRUE),
    max = sapply(list(env, soc, gov, env_soc_ratio, action, specificity, cti), max, na.rm = TRUE)
  )

# Export to LaTeX
colnames(descriptive_stats) <- c("Variable", "N", "Mean", "SD", "Min", "P25", "P50", "P75", "Max")

latex_table <- xtable(descriptive_stats, caption = "Descriptive Statistics for variables", digits = 2)
print(latex_table)


#loading data
data <- read.csv("G:/My Drive/KU/kandidat/speciale/R/esg_grouped.csv", header = TRUE)


#plot folder
# Create a folder (if it doesn't already exist)
output_folder <- "plots"
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Excluding NA, 2017 and 2024
data <- data %>% filter(Class != "Unknown")
data <- data %>% filter(Year >= 2018 & Year <= 2023)

# Sorting data by CVR and Year
data <- data %>%
  arrange(CVR, Year)

# Creating the reporting_obligation variable based on Class
data$reporting_obligation <- ifelse(
  is.na(data$Class), "NA",  # If Class is NA, reporting obligation is NA
  ifelse(
    data$Class %in% c("Small (A-B)", "Medium (C)"), 0,  #voluntary
    ifelse(
      data$Class %in% c("Large (C+)", "Very Large (D)"), 1,  #mandatory
      NA  
    )
  )
)

table(data$reporting_obligation, useNA = "ifany")

# Converting to numeric
data$reporting_obligation <- as.numeric(data$reporting_obligation)


#treatment start year
data <- data %>%
  group_by(CVR) %>%
  mutate(
    treatment_start_year = ifelse(reporting_obligation == 1 & dplyr::lag(reporting_obligation) == 0, Year, NA),
    treatment_start_year = min(treatment_start_year, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    treatment_start_year = ifelse(is.infinite(treatment_start_year), NA, treatment_start_year),
    time_to_treatment = ifelse(!is.na(treatment_start_year), Year - treatment_start_year +1, 0)
  )

table(data$treatment_start_year, useNA = "ifany")
table(data$time_to_treatment, useNA = "ifany")
table(data$treatment_group)

#treatment group
data <- data %>% 
  group_by(CVR) %>%
  mutate(
    treatment_group = ifelse(!is.na(treatment_start_year), 1, 0)
  ) %>%
  ungroup()


table(data$treatment_group)

#NA for subsequent transition
data <- data %>%
  group_by(CVR) %>%
  mutate(
    treatment_group = ifelse(!is.na(treatment_start_year) & reporting_obligation == 0 & Year > treatment_start_year, NA, treatment_group),
    reporting_obligation = ifelse(is.na(treatment_group), NA, reporting_obligation)
  ) %>%
  ungroup()


table(data$treatment_group, useNA = "ifany")
table(data$reporting_obligation, useNA = "ifany")


#event study model
#cti normal generalized diff-in-diff model
cti_dd <- feols(cti ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data)
summary(cti_dd)

#cti event study model
cti_es_model <- feols(cti ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                      cluster = ~ CVR, data = data)

#pulling model parameters
cti <- iplot(cti_es_model, only.params = TRUE)$prms
iplot(cti_es_model)


#esr normal generalized diff-in-diff model
esr_dd <- feols(env_soc_ratio ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data)
summary(esr_dd)

#env_soc_ratio event study model
esr_es_model <- feols(env_soc_ratio ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                      cluster = ~ CVR, data = data)


#pulling model parameters
esr <- iplot(esr_es_model, only.params = TRUE)$prms
iplot(esr_es_model)


#act normal generalized diff-in-diff model
act_dd <- feols(action ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data)
summary(act_dd)

#action event study model
action_es_model <- feols(action ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                         cluster = ~ CVR, data = data)

#pulling model parameters
action <- iplot(action_es_model, only.params = TRUE)$prms
iplot(action_es_model)

#action normal generalized diff-in-diff model + unit trends
act_dd_ut <- feols(action ~ reporting_obligation | Year + CVR + CVR[Year] , cluster = ~CVR, data = data)
summary(act_dd_ut)


#specificity normal generalized diff-in-diff model
spec_dd <- feols(specificity ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data)
summary(spec_dd)

#specificity event study model
specificity_es_model <- feols(specificity ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                              cluster = ~ CVR, data = data)

#pulling model parameters
specificity <- iplot(specificity_es_model, only.params = TRUE)$prms
iplot(specificity_es_model)


#creating outcome indicator
cti <- cti %>% mutate(outcome = "CTI")
esr <- esr %>% mutate(outcome = "E-S-R")
action <- action %>% mutate(outcome = "Action")
specificity <- specificity %>% mutate(outcome = "Specificity")

#combining all into one
all_es <- bind_rows(cti, esr, action, specificity)

#factoring
all_es$outcome <- factor(all_es$outcome, levels = c("E-S-R", "Action", "Specificity", "CTI"))


# setting colors
okabe_ito_palette <- c(
  "E-S-R" = "#E69F00",  # Orange
  "Action" = "#56B4E9",  # Blue
  "Specificity" = "#009E73",  # Green
  "CTI" = "#CC79A7"  # Replacing yellow with Okabe-Ito purple/pink
)

#
# Define custom labels for the facets with a, b, c, d
facet_labels <- c(
  "E-S-R" = "a) Environmental-Social Ratio",
  "Action" = "b) Action",
  "Specificity" = "c) Specificity",
  "CTI" = "d) CTI"
)

#plotting
ggplot(all_es, aes(x = x, color = outcome)) +
  labs(
    x = "Time to transition (years)", 
    y = "Coefficient",
    title = ""
  ) +
  scale_x_continuous(
    breaks = unique(all_es$x),  # Ensure major x-axis ticks appear
    minor_breaks = NULL  # Remove minor grid lines
  ) +
  scale_y_continuous(
    breaks = seq(-0.4, 0.4, by = 0.1),  # Set ticks from -0.4 to 0.4
    labels = scales::number_format(accuracy = 0.1),  # Format labels to avoid scientific notation
    expand = expansion(mult = c(0.1, 0.1))  # Add 10% padding on both sides of the y-axis
  ) +
  scale_color_manual(values = okabe_ito_palette) +  # Apply colorblind-friendly palette
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "black") +
  geom_linerange(aes(ymin = ci_low, ymax = ci_high), size = 1) +  
  geom_point(aes(y = estimate), size = 4) +  
  facet_wrap(~ outcome, scales = "fixed", labeller = labeller(outcome = facet_labels)) + 
  theme_minimal() +
  theme(
    legend.position = "none",  
    strip.text = element_text(size = 35, face = "bold", hjust = 0.5),  # Center facet labels
    axis.text.x = element_text(size = 25, color = "black", margin = margin(t = 10)),  # Space between x-axis labels and axis line
    axis.text.y = element_text(size = 25, color = "black", margin = margin(r = 10)),  # Space between y-axis labels and axis line
    axis.ticks.length = unit(-0.25, "cm"),  # Move ticks inside the panels
    axis.ticks.x = element_line(color = "black", size = 1),  # Add x-axis tick marks inside
    axis.ticks.y = element_line(color = "black", size = 1),  # Add y-axis tick marks inside
    axis.title = element_text(size = 40, color = "black", hjust = 0.5),  
    axis.title.x = element_text(size = 40, color = "black", hjust = 0.5, margin = margin(t = 20)),  # Add space below bottom panels
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  
    panel.spacing = unit(2, "lines")  # Increase spacing between facets
  )

#export the plot
plot_path <- file.path(output_folder, "event_study.pdf")
ggsave(
  filename = plot_path,
  plot = last_plot(),  
  width = 18,          
  height = 14,         
  dpi = 300,          
  units = "in"
)

##table for appendix
# Create a list of models to export
models_list <- list(
  "CTI" = cti_es_model,
  "ESR" = esr_es_model,
  "Action" = action_es_model,
  "Specificity" = specificity_es_model
)

# Export regression results to LaTeX
modelsummary(models_list, 
             output = "latex", 
             stars = TRUE,  # Show significance stars
             gof_omit = "IC|RMSE",  # Omit unnecessary fit statistics
             title = "Event Study Regression Results")

##################
#Main regression table of all diff-in-diff models
# Create a list of models to export
models_list <- list(
  "ESR" = esr_dd,
  "action" = act_dd,
  "action_ut" = act_dd_ut,
  "spec" = spec_dd,
  "CTI" = cti_dd
)

# Export regression results to LaTeX
modelsummary(models_list, 
             output = "latex", 
             stars = TRUE,  # Show significance stars
             gof_omit = "IC|RMSE",  # Omit unnecessary fit statistics
             title = "Difference-in-Differences Regression Results")


#######ROBUSTNESS#########
#effect heterogeneity
#small firm transitions only
data_small <- data %>% filter(Class != "Medium (C)")

#medium firm transitions only
data_medium <- data %>% filter(Class != "Small (A-B)")

#remove very large firms
data_no_very_large <- data %>% filter(Class != "Very Large (D)")


#cti normal generalized diff-in-diff model
cti_dd_small <- feols(cti ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_small)
summary(cti_dd_small)

#cti event study model
cti_es_model_small <- feols(cti ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                         cluster = ~ CVR, data = data_small)
iplot(cti_es_model_small)

#esr normal generalized diff-in-diff model
esr_dd_small <- feols(env_soc_ratio ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_small)
summary(esr_dd_small)

#esr event study model
esr_es_model_small <- feols(env_soc_ratio ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                            cluster = ~ CVR, data = data_small)
iplot(esr_es_model_small)


#act normal generalized diff-in-diff model
act_dd_small <- feols(action ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_small)
summary(act_dd_small)

#act event study model
act_es_model_small <- feols(action ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                            cluster = ~ CVR, data = data_small)
iplot(act_es_model_small)


#specificity normal generalized diff-in-diff model
spec_dd_small <- feols(specificity ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_small)
summary(spec_dd_small)

#spec event study model
spec_es_model_small <- feols(specificity ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                            cluster = ~ CVR, data = data_small)
iplot(spec_es_model_small)

#spec normal generalized diff-in-diff model + unit trends
spec_dd_ut_small <- feols(specificity ~ reporting_obligation | Year + CVR + CVR[Year] , cluster = ~CVR, data = data_small)
summary(spec_dd_ut_small)


####medium
#cti normal generalized diff-in-diff model
cti_dd_medium <- feols(cti ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_medium)
summary(cti_dd_medium)

#cti event study model
cti_es_model_medium <- feols(cti ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                             cluster = ~ CVR, data = data_medium)
iplot(cti_es_model_medium)

#esr normal generalized diff-in-diff model
esr_dd_medium <- feols(env_soc_ratio ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_medium)
summary(esr_dd_medium)

#esr event study model
esr_es_model_medium <- feols(env_soc_ratio ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                             cluster = ~ CVR, data = data_medium)
iplot(esr_es_model_medium)


#act normal generalized diff-in-diff model
act_dd_medium <- feols(action ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_medium)
summary(act_dd_medium)

#act event study model
act_es_model_medium <- feols(action ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                             cluster = ~ CVR, data = data_medium)
iplot(act_es_model_medium)

#action normal generalized diff-in-diff model + unit trends
act_dd_ut_medium <- feols(action ~ reporting_obligation | Year + CVR + CVR[Year] , cluster = ~CVR, data = data_medium)
summary(act_dd_ut_medium)

#specificity normal generalized diff-in-diff model
spec_dd_medium <- feols(specificity ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_medium)
summary(spec_dd_medium)

#spec event study model
spec_es_model_medium <- feols(specificity ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                             cluster = ~ CVR, data = data_medium)
iplot(spec_es_model_medium)


####without very large
#cti normal generalized diff-in-diff model
cti_dd_no_very_large <- feols(cti ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_no_very_large)
summary(cti_dd_no_very_large)

#cti event study model
cti_es_model_no_very_large <- feols(cti ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                            cluster = ~ CVR, data = data_no_very_large)
iplot(cti_es_model_no_very_large)

#esr normal generalized diff-in-diff model
esr_dd_no_very_large <- feols(env_soc_ratio ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_no_very_large)
summary(esr_dd_no_very_large)

#esr event study model
esr_es_model_no_very_large <- feols(env_soc_ratio ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                            cluster = ~ CVR, data = data_no_very_large)
iplot(esr_es_model_no_very_large)


#act normal generalized diff-in-diff model
act_dd_no_very_large <- feols(action ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_no_very_large)
summary(act_dd_no_very_large)

#act event study model
act_es_model_no_very_large <- feols(action ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                            cluster = ~ CVR, data = data_no_very_large)
iplot(act_es_model_no_very_large)

#action normal generalized diff-in-diff model + unit trends
act_dd_ut_no_very_large <- feols(action ~ reporting_obligation | Year + CVR + CVR[Year] , cluster = ~CVR, data = data_no_very_large)
summary(act_dd_ut_no_very_large)

#specificity normal generalized diff-in-diff model
spec_dd_no_very_large <- feols(specificity ~ reporting_obligation | CVR + Year, cluster = ~ CVR, data = data_no_very_large)
summary(spec_dd_no_very_large)

#spec event study model
spec_es_model_no_very_large <- feols(specificity ~ i(time_to_treatment, treatment_group, ref = 0) | CVR + Year,
                             cluster = ~ CVR, data = data_no_very_large)
iplot(spec_es_model_no_very_large)


# Create a list of models to export
models_list_small <- list(
  "ESR" = esr_dd_small,
  "action" = act_dd_small,
  "spec" = spec_dd_small,
  "spec_ut" = spec_dd_ut_small,
  "CTI" = cti_dd_small
)

models_list_medium <- list(
  "ESR" = esr_dd_medium,
  "action" = act_dd_medium,
  "act_ut" = act_dd_ut_medium,
  "spec" = spec_dd_medium,
  "CTI" = cti_dd_medium
)

models_list_vl <- list(
  "ESR" = esr_dd_no_very_large,
  "action" = act_dd_no_very_large,
  "act_ut" = act_dd_ut_no_very_large,
  "spec" = spec_dd_no_very_large,
  "CTI" = cti_dd_no_very_large
)


# Export regression results to LaTeX
modelsummary(models_list_small, 
             output = "latex", 
             stars = TRUE,  # Show significance stars
             gof_omit = "IC|RMSE",  # Omit unnecessary fit statistics
             title = "Difference-in-Differences Regression Results - without Medium Firms")

# Export regression results to LaTeX
modelsummary(models_list_medium, 
             output = "latex", 
             stars = TRUE,  # Show significance stars
             gof_omit = "IC|RMSE",  # Omit unnecessary fit statistics
             title = "Difference-in-Differences Regression Results - without Small Firms")

# Export regression results to LaTeX
modelsummary(models_list_vl, 
             output = "latex", 
             stars = TRUE,  # Show significance stars
             gof_omit = "IC|RMSE",  # Omit unnecessary fit statistics
             title = "Difference-in-Differences Regression Results - without Very Large Firms")


########STAGGERED DIFF-IN-DIFF##############
#sun and abraham

#creating period
data$period <- data$Year - 2017
table(data$period)

#treatment start year
data <- data %>%
  group_by(CVR) %>%
  mutate(
    year_treated = ifelse(reporting_obligation == 1 & dplyr::lag(reporting_obligation) == 0, Year, NA),
    year_treated = min(year_treated, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    year_treated = ifelse(is.infinite(year_treated), NA, year_treated),
    time_to_treatment = ifelse(!is.na(year_treated), Year - year_treated +1, 0)
  )


#fixing year_treated to be consecutive from 0
data <- data %>%
  group_by(CVR) %>%
  mutate(year_treated = ifelse(!is.na(year_treated), 
                               ifelse(any(time_to_treatment == 0),
                                      min(period[time_to_treatment == 0], na.rm = TRUE),
                                      min(period[time_to_treatment == 1], na.rm = TRUE) - 1),
                               year_treated)) %>%
  ungroup()



#assigning 10000 to non-treated
data$year_treated <- ifelse(is.na(data$year_treated), 10000, data$year_treated)

table(data$year_treated)


#env soc ratio model
sun_abraham_esr <- feols(env_soc_ratio ~ sunab(year_treated, period) | CVR + period, data = data)
iplot(sun_abraham_esr, title = "E-S-R")

#action model
sun_abraham_act <- feols(action ~ sunab(year_treated, period) | CVR + period, data = data)
iplot(sun_abraham_act, title = "Action")


#action model + unit trends
sun_abraham_act_ut <- feols(action ~ sunab(year_treated, period) | CVR + period + CVR[period], data = data)
iplot(sun_abraham_act_ut, title = "Action + Unit Trends")

#spec model
sun_abraham_spec <- feols(specificity ~ sunab(year_treated, period) | CVR + period, data = data)
iplot(sun_abraham_spec, title = "Specificity")

#cti model
sun_abraham_cti <- feols(cti ~ sunab(year_treated, period) | CVR + period, data = data)
iplot(sun_abraham_cti, title = "CTI")


#cti model + unit trends
sun_abraham_cti_ut <- feols(cti ~ sunab(year_treated, period) | CVR + period + CVR[period], data = data)
iplot(sun_abraham_cti_ut, title = "Action + Unit Trends")
summary(sun_abraham_cti_ut, agg = "ATT")

#extract att
extract_att_model <- function(model) {
  summary(model, agg = "ATT")
}

#models list
models_list <- list(
  "ESR" = extract_att_model(sun_abraham_esr),
  "Action" = extract_att_model(sun_abraham_act),
  "Action + UT" = extract_att_model(sun_abraham_act_ut),
  "Specificity" = extract_att_model(sun_abraham_spec),
  "CTI" = extract_att_model(sun_abraham_cti),
  "CTI + UT" = extract_att_model(sun_abraham_cti_ut)
)


# Export full regression results to LaTeX
modelsummary(models_list, 
             output = "latex", 
             stars = TRUE,  # Show significance stars
             statistic = "std.error",  # Include standard errors
             gof_omit = "IC|RMSE",  # Omit unnecessary goodness-of-fit statistics
             title = "Difference-in-Differences Regression Results (Sun & Abraham, 2020)")

