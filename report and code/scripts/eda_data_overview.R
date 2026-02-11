
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
library(ggplot2)

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
     # Print sheet name as a header
    cat("\n## Sheet:", sheet, "\n")   
    # Generates information such as mean, median, standard deviation, minimum, maximum, and missing values.
    if (ncol(numeric_df) > 0) {
        summary_list[[sheet]] <- skim(numeric_df)
        print(summary_list[[sheet]])
    }
    else{
        cat("No numeric columns to summarize in this sheet.\n")
    }
}

# Create histograms for each numeric column in each sheet
for (sheet in sheets) {
    df <- data_list[[sheet]]
    numeric_df <- df %>% select(where(is.numeric))
    if (ncol(numeric_df) > 0) {
        for (col_name in names(numeric_df)){
            p.hist <- ggplot(numeric_df, aes_string(x = col_name)) + 
                geom_histogram(binwidth = 1, fill = "blue", color = "black") +
                labs(title = paste("Histogram of", col_name, "in sheet", sheet), x = col_name, y = "Frequency")
            print(p.hist)
        }
} }

# Create boxplots for each numeric column in each sheet
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
  # Create boxplots for each numeric column in the sheet (without grouping by "track")
  if (ncol(numeric_df) > 0) {
    for (col_name in names(numeric_df)){
      p.box.single <- ggplot(numeric_df, aes_string(x = "factor(1)", y = col_name)) + 
        geom_boxplot(fill = "blue", color = "black") +
        stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "red") +   # Add median point
        stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "green") +   # Add mean point
        labs(title = paste("Boxplot of", col_name, "in sheet", sheet), x = "", y = col_name)
      print(p.box.single)
    }
  }
}
    
    
    
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
    # Check if "track" column exists and there are numeric columns to plot
  if ("track" %in% names(df) & ncol(numeric_df) > 0) {
    for (col_name in names(numeric_df)){
      p.box.track <- ggplot(df, aes_string(x = "track", y = col_name)) + 
        stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "red") +   # Add median point
        stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "green") +   # Add mean point
        geom_boxplot(fill = "blue", color = "black") +
        labs(title = paste("Boxplot of", col_name, "by track in sheet", sheet), x = "Track",y = col_name)
      print(p.box.track)
    }
  } 
}
       