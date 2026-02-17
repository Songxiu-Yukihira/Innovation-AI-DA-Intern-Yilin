
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

clean_for_plot <- function(df, x, y = NULL){
    # This function filters out rows with non-finite values in the specified columns for plotting.
    if (is.null(y)) {
        df %>% filter( is.finite(.data[[x]]))  # Filter out rows where the x column has non-finite values (NA, Inf, -Inf)
    } else {
        df %>% filter( is.finite(.data[[x]]), is.finite(.data[[y]]))   # Filter out rows where either x or y column has non-finite values
    }
}
# Data cleaning
cleaned_users <- data_list[["users"]] %>%
    mutate(total_experience_years = ifelse(total_experience_years > 5, 5, total_experience_years))   # Cap experience at 5 years (upper limit is 5 years)
data_list[["users"]] <- cleaned_users

cleaned_technical_questions <- data_list[["technical_questions"]] %>%
    mutate(time_to_answer_sec = ifelse(time_to_answer_sec > 360, 360, time_to_answer_sec))           # Cap time at 360 seconds (upper limit is 360 seconds)
data_list[["technical_questions"]] <- cleaned_technical_questions

# After cleaning, we can check the summary statistics again to see the effect of capping
summary_list_clean <- list()
for (sheet in sheets) {
    df <- data_list[[sheet]]
    numeric_df <- df %>% select(where(is.numeric)) 
    cat("\n## Sheet:", sheet, "after cleaning\n")   
    if (ncol(numeric_df) > 0) {
        summary_list_clean[[sheet]] <- skim(numeric_df)
        print(summary_list_clean[[sheet]])
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
            df2 <- clean_for_plot(df, col_name)  # Clean data for plotting
            p.hist <- ggplot(df2, aes(x = .data[[col_name]])) + 
                geom_histogram(binwidth = 1, fill = "blue", color = "black") +
                labs(title = paste("Histogram of", col_name, "in sheet", sheet), x = col_name, y = "Count")
            print(p.hist)
        }
} }

# Create boxplots for each numeric column in each sheet
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
  # Create boxplots for each numeric column in the sheet (without grouping)
  if (ncol(numeric_df) > 0) {
    for (col_name in names(numeric_df)){
        df2 <- clean_for_plot(df, col_name)  # Clean data for plotting
        p.box.single <- ggplot(df2, aes(x = factor(1), y = .data[[col_name]])) + 
        geom_boxplot(outlier.colour = "purple", fill = "blue", color = "black") +
        stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "red") +   # Add median point
        stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "green") +   # Add mean point
        labs(title = paste("Boxplot of", col_name, "in sheet", sheet), x = "", y = col_name)
      print(p.box.single)
    }
  }
}

      
# Create boxplots for each numeric column grouped by "track" if "track" column exists
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
 # Check if "track" column exists and there are numeric columns to plot
  if ("track" %in% names(df) & ncol(numeric_df) > 0) {
    # Convert "track" to factor for better plotting
    df$track <- as.factor(df$track)        # Making sure "track" is treated as a categorical variable
    for (col_name in names(numeric_df)){
        df2 <- clean_for_plot(df, col_name)  # Clean data for plotting
        df2 <- df2 %>% filter(!is.na(track))  # Filter out rows where "track" is NA
        p.box.track <- ggplot(df2, aes(x = track, y = .data[[col_name]])) +
        geom_boxplot(aes(fill = track), outlier.colour = "purple") +
        stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "red") +   # Add median point
        stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "green") +   # Add mean point
        labs(title = paste("Boxplot of", col_name, "by track in sheet", sheet), x = "Track",y = col_name)
      print(p.box.track)
    }
  } 
}

# Create boxplots for each numeric column grouped by major groups if they exist
major_groups <- c("track", "device_type", "timezone") # Define major groups for analysis
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
  # Create boxplots for each numeric column grouped by major groups if they exist
  if (ncol(numeric_df) > 0) {   # Only create boxplots if there are numeric columns to plot
    for (col_name  in names(numeric_df)) { 
        # Loop through each major group and create boxplots if the group column exists in the dataframe 
        for (group in major_groups) {
            if (group %in% names(df)) {
                df2 <- clean_for_plot(df, col_name)  # Clean data for plotting
                df2 <- df2 %>% filter(!is.na(.data[[group]]))  # Filter out rows where the group variable is NA

          p.box.group <- ggplot(df2, aes(x = .data[[group]], y = .data[[col_name]])) +
          geom_boxplot(aes(fill = .data[[group]]), outlier.colour = "purple") +
          stat_summary(fun = median, geom = "point", shape = 20, size = 3, color = "red") +   # Add median point
          stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "green") +   # Add mean point
          labs(title = paste("Boxplot of", col_name, "by", group, "in sheet", sheet), x = group, y = col_name)
        print(p.box.group)
            }
        }
    }
}
}
   

# Create scatter plots for each pair of numeric columns in each sheet, colored by "track" if it exists, and calculate correlation
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
  if (ncol(numeric_df) > 1) {  # Only create scatter plots if there are at least 2 numeric columns
    numeric_cols <- names(numeric_df)
    
    for (i in 1:(length(numeric_cols)-1)) {
      for (j in (i+1):length(numeric_cols)) {
        x <- numeric_cols[i]
        y <- numeric_cols[j]
        
        df2 <- clean_for_plot(df, x, y)  # Clean data for plotting
        cor_val <- cor(df2[[x]], df2[[y]], use = "complete.obs")  # Calculate correlation, excluding NA values
        
        if ("track" %in% names(df2)) {
          df2 <- df2 %>% filter(!is.na(track))  # Filter out rows where "track" is NA
          df2$track <- as.factor(df2$track)     # Ensure "track" is treated as a categorical variable
          
          if (length(levels(df2$track)) > 1) {
            p.scatter <- ggplot(df2, aes(x = .data[[x]], y = .data[[y]])) +
              geom_point(aes(color = track)) +
              geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
              labs(color = "Track", title = paste("Scatter plot of", x, "vs", y, "in sheet", sheet, "\nCorrelation:", round(cor_val, 2)),
                   x = x, y = y
              )
          } else {
            p.scatter <- ggplot(df2, aes(x = .data[[x]], y = .data[[y]])) +
              geom_point() +
              geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
              labs(color = "Device Type",
                   title = paste("Scatter plot of", x, "vs", y, "by device_type in sheet", sheet,
                                 "\nCorrelation:", round(cor_val, 2)),
                   x = x, y = y)
          }
          print(p.scatter)
        }
      }
    }
  }
}

# Scatter plots colored by device_type
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
  if (ncol(numeric_df) > 1) {  
    numeric_cols <- names(numeric_df)
    for (i in 1:(length(numeric_cols)-1)) {
      for (j in (i+1):length(numeric_cols)) {
        x <- numeric_cols[i]
        y <- numeric_cols[j]
        df2 <- clean_for_plot(df, x, y)
        
        if ("device_type" %in% names(df2)) {
          df2 <- df2 %>% filter(!is.na(device_type))
          df2$device_type <- as.factor(df2$device_type)
          
          if (length(levels(df2$device_type)) > 1) {
            cor_val <- cor(df2[[x]], df2[[y]], use = "complete.obs")
            p.scatter <- ggplot(df2, aes(x = .data[[x]], y = .data[[y]])) +
              geom_point(aes(color = device_type)) +
              geom_smooth(method = "lm", se = FALSE) +
              labs(color = "Device Type",
                   title = paste("Scatter plot of", x, "vs", y, "by device_type in sheet", sheet,
                                 "\nCorrelation:", round(cor_val, 2)),
                   x = x, y = y)
            print(p.scatter)
          }
        }
      }
    }
  }
}


# Scatter plots colored by timezone
for (sheet in sheets) {
  df <- data_list[[sheet]]
  numeric_df <- df %>% select(where(is.numeric))
  
  if (ncol(numeric_df) > 1) {  
    numeric_cols <- names(numeric_df)
    for (i in 1:(length(numeric_cols)-1)) {
      for (j in (i+1):length(numeric_cols)) {
        x <- numeric_cols[i]
        y <- numeric_cols[j]
        df2 <- clean_for_plot(df, x, y)
        
        if ("timezone" %in% names(df2)) {
          df2 <- df2 %>% filter(!is.na("timezone"))
          df2$timezone <- as.factor(df2$timezone)
          
          if (length(levels(df2$timezone)) > 1) {
            cor_val <- cor(df2[[x]], df2[[y]], use = "complete.obs")
            p.scatter <- ggplot(df2, aes(x = .data[[x]], y = .data[[y]])) +
              geom_point(aes(color = timezone)) +
              geom_smooth(method = "lm", se = FALSE) +
              labs(color = "Timezone",
                   title = paste("Scatter plot of", x, "vs", y, "by timezone in sheet", sheet,
                                 "\nCorrelation:", round(cor_val, 2)),
                   x = x, y = y)
            print(p.scatter)
          }
        }
      }
    }
  }
}

# Create a new dataframe "user_session" by performing a left join between the "users" dataframe and the "sessions" dataframe based on the "user_id" column. This will combine user information with their session data, allowing for analysis of user behavior in relation to their sessions.
user_session <- data_list[["users"]] %>%   
  left_join(
    data_list[["sessions"]],
    by = "user_id"
  )
# Group the combined user-session data by device type and calculate summary statistics for each device type, including average technical score, average behavior score, pass rate, average latency at 50th and 95th percentiles, and average number of messages exchanged.
device_summary <- user_session %>%    
  group_by(device_type) %>%    
  summarise(
    n = n(),   # Count the number of observations for each device type

    avg_tech_score = mean(overall_tech_score, na.rm = TRUE),

    avg_behavior_score = mean(weighted_behavior_score, na.rm = TRUE),

    pass_rate = mean(pass_flag, na.rm = TRUE),

    avg_latency_p50 = mean(latency_ms_p50, na.rm = TRUE),

    avg_latency_p95 = mean(latency_ms_p95, na.rm = TRUE),

    avg_messages = mean(messages_exchanged, na.rm = TRUE)
  )

print(device_summary)


t.test(overall_tech_score ~ device_type,    # Perform a t-test to compare the average technical scores between different device types, testing the null hypothesis that there is no difference in technical scores based on device type.
       data = user_session)
t.test(weighted_behavior_score ~ device_type,  # Perform a t-test to compare the average behavior scores between different device types, testing the null hypothesis that there is no difference in behavior scores based on device type.
       data = user_session)

tbl <- table(       # Create a contingency table to examine the relationship between device type and pass/fail status, which will be used for a chi-squared test to determine if there is a significant association between these two categorical variables.
  user_session$device_type,
  user_session$pass_flag
)

chisq.test(tbl)

# Calculate the timeout rate for each device type by filtering the "system_events" dataframe for events of type "timeout", then joining with the "sessions" and "users" dataframes to get device type information, and finally grouping by device type to count the number of timeouts for each device type.
timeout_rate <- data_list[["system_events"]] %>%
  filter(event_type == "timeout") %>%
  left_join(
    data_list[["sessions"]],
    by = "session_id"
  ) %>%
  left_join(
    data_list[["users"]],
    by = "user_id"
  ) %>%
  group_by(device_type) %>%
  summarise(timeout_count = n())

timeout_rate

# Calculate the average time to answer technical questions for each device type by joining the "technical_questions" dataframe with the "sessions" and "users" dataframes to get device type information, then grouping by device type and calculating the mean time to answer for each device type.
tech_time <- data_list[["technical_questions"]] %>%
  left_join(
    data_list[["sessions"]],
    by = "session_id"
  ) %>%
  left_join(
    data_list[["users"]],
    by = "user_id"
  ) %>%
  group_by(device_type) %>%
  summarise(
    avg_time = mean(time_to_answer_sec)
  )

tech_time

# Create a density plot to visualize the distribution of latency at the 95th percentile for each device type, allowing for comparison of latency performance across different device types.
ggplot(user_session,
       aes(x = latency_ms_p95, fill = device_type)) +
  geom_density(alpha = 0.4) +
  labs(title = "Latency Distribution by Device Type")