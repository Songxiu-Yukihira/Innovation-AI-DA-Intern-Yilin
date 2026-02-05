
# =====================================
# Innovation AI - Week1 EDA
# File: eda_data_overview.R
# Author: Yilin Wang
# Date: 04/02/2026
# Purpose: Load dataset and overview sheets
# =====================================

rm(list = ls())  # Clear workspace

library(tidyverse)
library(readxl)
library(janitor)
library(skimr)

data_path = "../data/ai_interview_dataset_1129.xlsx"

sheets <- excel_sheets(data_path)
print("Sheets in the dataset:")
sheets           # List all sheet names

# Read each sheet from the excel file
data_list <- list()
for (sheet in sheets) {
  data_list[[sheet]] <- read_excel(data_path, sheet = sheet)
}

# Overview of each sheet
overview <- list()
for (sheet in sheets) {
    df <- data_list[[sheet]]
    overview[[sheet]] <- tibble(
        rows = nrow(df),
        cols = ncol(df),
        col_names = paste(names(df), collapse = ", "),
        missing_rate = sum(is.na(df)) / (nrow(df) * ncol(df))
)
}
print("Sheet overview:")
overview

summary_list <- list()
for (sheet in sheets) {
    df <- data_list[[sheet]]
    
    # Select only numeric columns for summary
    numeric_df <- df %>% select(where(is.numeric)) 
    summary_list[[sheet]] <- skim(numeric_df)
}
summary_list


