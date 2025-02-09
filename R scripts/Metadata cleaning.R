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
library("sandwich")
library("lmtest")
library("tidytext")
library("plm")
library("cluster")
library("tm")
library("purrr")
library("FactoMineR")
conflicts_prefer(
  dplyr::filter,
  seededlda::terms
)


#clear R env
rm(list = ls())


###########Data Preprocessing###########
# Importing the dataset
data = read.csv("data_final.csv")

#Cleaning meta vars
#industry
table(data$Industry)
# Recode the Industry variable by collapsing categories
data <- data %>%
  mutate(Industry = case_when(
    Industry %in% c("Rejsebureauer, rengøring og anden operationel service", 
                    "Andre serviceydelser  mv.", 
                    "Hoteller og restauranter", 
                    "Kultur og fritid") ~ "Services",
    Industry == "Ejendomshandel og udlejning" ~ "Real Estate",
    Industry == "Bygge og anlæg" ~ "Construction",
    Industry == "Industri" ~ "Manufacturing",
    Industry == "Råstofindvinding" ~ "Extractive Industries",  # Separate
    Industry %in% c("Energiforsyning", "Vandforsyning og renovation") ~ "Utilities",
    Industry %in% c("Sundhed og socialvæsen", "Undervisning") ~ "Public and Social Services",
    Industry == "Handel" ~ "Trade",
    Industry == "Videnservice" ~ "Knowledge Services",
    Industry == "Finansiering og forsikring" ~ "Finance and Insurance",
    Industry == "Information og kommunikation" ~ "Information and Communication",
    Industry == "Landbrug, skovbrug og fiskeri" ~ "Agriculture",
    TRUE ~ Industry  # Keep other industries as is
  ))

#Manually reassigning 'others' to the correct industry
data <- data %>%
  mutate(Industry = case_when(
    CVR == 32556590 ~ "Information and Communication",  # CVR 32556590: Digital camera systems and imaging software
    CVR == 37444480 ~ "Finance and Insurance",  # CVR 37444480: Investment company
    CVR == 36016345 ~ "Real Estate",  # CVR 36016345: Real estate investment and management
    CVR == 34461600 ~ "Agriculture",  # CVR 34461600: Fishing and seafood production
    CVR == 32308783 ~ "Information and Communication",  # CVR 32308783: Digital imaging and software solutions
    CVR == 38220578 ~ "Real Estate",  # CVR 38220578: Real Estate company
    CVR == 32308813 ~ "Information and Communication",  # CVR 32308813: Digital imaging and software solutions
    TRUE ~ Industry  # Keep other industry classifications unchanged
  ))

#corporate form
table(data$Corporate.form)

#collapse Corporate Forms based on the refined categories
data <- data %>%
  mutate(Corporate_structure = case_when(
    Corporate.form %in% c("Aktieselskab", "Anpartsselskab", "Iværksætterselskab", "Selskab med begrænset ansvar") ~ "Limited Liability Companies",
    Corporate.form %in% c("Erhvervsdrivende fond", "Fonde og andre selvejende institutioner") ~ "Foundations",
    Corporate.form %in% c("Kommanditaktieselskab/Partnerselskab", "Kommanditselskab", "Interessentskab") ~ "Partnerships",
    Corporate.form %in% c("Andelsselskab (-forening) med begrænset ansvar", "Forening", "Forening med begrænset ansvar") ~ "Associations",
    Corporate.form == "Selvstændig offentlig virksomhed eller dattervirksomhed heraf" ~ "Government/Public Enterprises",
    TRUE ~ "Other"
  ))

# View the collapsed categories
table(data$Corporate_structure)

#checking the 'others'
other_corporate_forms <- data %>%
  filter(Corporate_structure == "Other")

#manually reassigning 'others' to the correct corporate form, indsutry, region and class
# Update dataset with classification for all CVRs using 'data' and updated variable names
data <- data %>%
  mutate(
    Corporate_structure = case_when(
      CVR == 74663028 ~ "Foundations",  # CVR 74663028: Foundation
      CVR == 62725214 ~ "Limited Liability Companies",  # CVR 62725214: NKT A/S, Limited Liability Company
      CVR == 28980671 ~ "Government/Public Enterprises",  # CVR 28980671: Energinet
      CVR == 29782792 ~ "Limited Liability Companies",  # CVR 29782792: Manufacturing holding company
      CVR == 28313519 ~ "Limited Liability Companies",  # CVR 28313519: Nycomed, Limited Liability Company
      CVR == 26902398 ~ "Limited Liability Companies",  # CVR 26902398: Missionpharma A/S
      CVR == 33593163 ~ "Limited Liability Companies",  # CVR 33593163: Financial intermediation company
      CVR == 37389609 ~ "Limited Liability Companies",  # CVR 37389609: Hotel company
      CVR == 41956712 ~ "Limited Liability Companies",  # CVR 41956712: Beverage and bottling company
      CVR == 38437011 ~ "Limited Liability Companies",  # CVR 38437011: Odense Havn, Commercial port management
      CVR == 30731735 ~ "Limited Liability Companies",  # CVR 30731735: Copenhagen Capital A/S, Financial holding company
      TRUE ~ Corporate_structure
    ),
    Region = case_when(
      CVR == 74663028 ~ "Region Nordjylland",  # CVR 74663028: Mariagerfjord
      CVR == 62725214 ~ "Region Hovedstaden",  # CVR 62725214: Brøndby
      CVR == 28980671 ~ "Region Syddanmark",  # CVR 28980671: Fredericia
      CVR == 29782792 ~ "Region Hovedstaden",  # CVR 29782792: Allerød
      CVR == 28313519 ~ "Region Hovedstaden",  # CVR 28313519: Vallensbæk
      CVR == 26902398 ~ "Region Hovedstaden",  # CVR 26902398: Lynge
      CVR == 33593163 ~ "Region Hovedstaden",  # CVR 33593163: Copenhagen
      CVR == 37389609 ~ "Region Syddanmark",  # CVR 37389609: Sønderborg
      CVR == 41956712 ~ "Region Sjælland",  # CVR 41956712: Faxe Municipality
      CVR == 38437011 ~ "Region Syddanmark",  # CVR 38437011: Odense
      CVR == 30731735 ~ "Region Hovedstaden",  # CVR 30731735: Copenhagen
      TRUE ~ Region
    ),
    Industry = case_when(
      CVR == 74663028 ~ "Public and Social Services",  # CVR 74663028: Residential care for persons with disabilities
      CVR == 62725214 ~ "Manufacturing",  # CVR 62725214: NKT A/S, Manufacturing (energy cables, optical components)
      CVR == 28980671 ~ "Utilities",  # CVR 28980671: Transmission of electricity and gas distribution
      CVR == 29782792 ~ "Manufacturing",  # CVR 29782792: Manufacturing holding company
      CVR == 28313519 ~ "Manufacturing",  # CVR 28313519: Pharmaceutical manufacturing
      CVR == 26902398 ~ "Trade",  # CVR 26902398: Wholesale of pharmaceuticals and medical goods
      CVR == 33593163 ~ "Finance and Insurance",  # CVR 33593163: Other financial intermediation
      CVR == 37389609 ~ "Services",  # CVR 37389609: Hotel industry
      CVR == 41956712 ~ "Manufacturing",  # CVR 41956712: Beverage manufacturing (soft drinks, beer, wholesale)
      CVR == 38437011 ~ "Transport",  # CVR 38437011: Commercial port management and real estate rental
      CVR == 30731735 ~ "Finance and Insurance",  # CVR 30731735: Financial holding company
      TRUE ~ Industry
    )
  )

#Class
table(data$Class)

# Collapse Class categories into broader categories
data <- data %>%
  mutate(
    Class = case_when(
      Class %in% c("Regnskabsklasse A", "Regnskabsklasse B", "Regnskabsklasse B, mikrovirksomhed") ~ "Small (A-B)",
      Class == "Regnskabsklasse C, mellemstor virksomhed" ~ "Medium (C)",
      Class == "Regnskabsklasse C, stor virksomhed" ~ "Large (C+)",
      Class == "Regnskabsklasse D" ~ "Very Large (D)",
      Class == "Ukendt regnskabsklasse" ~ "Unknown",
      TRUE ~ "Unknown"  
    )
  )

#view the new collapsed categories
table(data$Class)

#read new csr file to add number of employees
csrv2 <- read.csv("Anker CSR v2.csv", header = FALSE)

#changing "V9" to "Employees
names(csrv2)[names(csrv2) == "V9"] <- "Employees"
names(csrv2)[names(csrv2) == "V1"] <- "CVR"
names(csrv2)[names(csrv2) == "V5"] <- "Year"
names(csrv2)[names(csrv2) == "V8"] <- "Revenue"
names(csrv2)[names(csrv2) == "V7"] <- "Result"
as.numeric(csrv2$Employees)
as.numeric(csrv2$Year)
as.numeric(csrv2$CVR)

#manually checking conflicts where number of employees differs for duplicates
conflicts <- csrv2 %>%
  group_by(CVR, Year) %>%
  filter(n_distinct(Employees) > 1) %>%
  arrange(CVR, Year)

#as no conflicts overlapped the thresholds, I just go with first instances
#ensure `csrv2` contains only the first instance of each (CVR, Year) combination
csrv2_unique <- csrv2 %>%
  arrange(CVR, Year) %>%
  distinct(CVR, Year, .keep_all = TRUE)


##mismatched rows
# Perform the left join to add Employees
metadata <- left_join(data, csrv2_unique[, c("CVR", "Year", "Employees")], by = c("CVR", "Year"))

#loading classifications
classifications <- read.csv("esg_grouped.csv")

#identify rows in metadata that were not classified
mismatched_rows <- anti_join(metadata, classifications, by = "doc_id")

#remove the mismatched rows form metadata
metadata <- metadata[!metadata$doc_id %in% mismatched_rows$doc_id, ]

##employees
#making variable numeric
metadata <- metadata %>%
  mutate(
    Employees = as.numeric(Employees)
  )


####class identification


#generate a less strict class variable based on Employees for Unknowns
metadata <- metadata %>%
  group_by(CVR) %>%
  mutate(
    Observation_Count = n() 
  ) %>%
  ungroup() %>%
    mutate(
    Class_temp = case_when(
      Observation_Count == 1 ~ "Unknown",  # Keep as Unknown for single observations
      Class == "Unknown" & Employees < 50 ~ "Small",    # Small: Employees < 50
      Class == "Unknown" & Employees >= 50 & Employees <= 250 ~ "Medium", # Medium: 50 <= Employees <= 250
      Class == "Unknown" & Employees > 250 ~ "Large",  # Large: Employees > 250
      TRUE ~ Class 
    )
  ) %>%
    arrange(CVR, Year) %>%
  group_by(CVR) %>%
  mutate(
    Consecutive_Flag = ifelse(
      lag(Class_temp) == Class_temp & !is.na(lag(Class_temp)) & Class_temp %in% c("Small", "Medium", "Large"),
      TRUE, 
      FALSE
    )
  ) %>%
    mutate(
    Relaxed_Class = ifelse(
      (Observation_Count > 1 & Consecutive_Flag == TRUE & Class == "Unknown"),
      Class_temp, 
      Class 
    )
  ) %>%
    ungroup() %>%
  select(-Class_temp, -Consecutive_Flag, -Observation_Count)

#write metadata
write.csv(metadata, "metadata.csv")
