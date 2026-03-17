
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

# Calculate the session duration in seconds for each user-session by taking the difference between the session end timestamp and the session start timestamp, and converting it to seconds. This will allow for analysis of how long users are spending in their sessions, which can be an important factor in understanding user engagement and performance.
user_session <- user_session %>%
  mutate(
    session_duration_sec = as.numeric(difftime(session_end_ts, session_start_ts, units = "secs"))
  )

# Group the combined user-session data by version and calculate summary statistics for each version, including average technical score, average behavior score, pass rate, average latency at 50th and 95th percentiles, and average session time. This will allow for analysis of how different versions of the system perform in terms of user outcomes and session characteristics.
version_summary <- user_session %>%
  group_by(version_tag) %>%
  summarise(
    n = n(),    # Count the number of observations for each version
    avg_tech = mean(overall_tech_score, na.rm = TRUE),
    avg_behavior = mean(weighted_behavior_score, na.rm = TRUE),
    pass_rate = mean(pass_flag, na.rm = TRUE),
    avg_latency_p50 = mean(latency_ms_p50, na.rm = TRUE),
    avg_latency_p95 = mean(latency_ms_p95, na.rm = TRUE),
    avg_session_time = mean(session_duration_sec, na.rm = TRUE)
  )

version_summary

# Perform an ANOVA test to compare the average technical scores across different versions, testing the null hypothesis that there is no difference in technical scores based on version.
tech_anova <- aov(overall_tech_score ~ version_tag, data = user_session)   # Because there are three versions: 1.3, 1.4, and 1.5, it is not possible to use a disposable t. test
summary(tech_anova)
# Perform an ANOVA test to compare the average behavior scores across different versions, testing the null hypothesis that there is no difference in behavior scores based on version.
behavior_anova <- aov(weighted_behavior_score ~ version_tag, data = user_session)
summary(behavior_anova)

tbl_v <- table(       # Create a contingency table to examine the relationship between version and pass/fail status, which will be used for a chi-squared test to determine if there is a significant association between these two categorical variables.
  user_session$version_tag,     
  user_session$pass_flag
)
chisq.test(tbl_v)

# Calculate the timeout rate for each version by filtering the "system_events" dataframe for events of type "timeout", then joining with the "sessions" and "users" dataframes to get version information, and finally grouping by version to count the number of timeouts for each version.
timeout_version <- data_list[["system_events"]] %>%
  filter(event_type == "timeout") %>%
  left_join(
    data_list[["sessions"]], 
    by = "session_id"
  ) %>%
  group_by(
    version_tag
  ) %>%
  summarise(timeout_count = n())
timeout_version



# Calculate the average score by school level
school_summary <- user_session %>%
  group_by(school_tier_usnews) %>%
  summarise(
    n = n(),
    avg_resume = mean(overall_resume, na.rm = TRUE),
    avg_tech = mean(overall_tech_score, na.rm = TRUE),
    avg_behavior = mean(weighted_behavior_score, na.rm = TRUE),
    pass_rate = mean(pass_flag, na.rm = TRUE)
  )

school_summary

# Calculate the average score by degree level
degree_summary <- user_session %>%
  group_by(highest_degree) %>%
  summarise(
    n = n(),
    avg_resume = mean(overall_resume, na.rm = TRUE),
    avg_tech = mean(overall_tech_score, na.rm = TRUE),
    avg_behavior = mean(weighted_behavior_score, na.rm = TRUE),
    pass_rate = mean(pass_flag, na.rm = TRUE)
  )

degree_summary

# tech_score by school level
tech_school_aov <- aov(overall_tech_score ~ school_tier_usnews, data = user_session)
summary(tech_school_aov)

# behavior_score by school level
behavior_school_aov <- aov(weighted_behavior_score ~ school_tier_usnews, data = user_session)
summary(behavior_school_aov)

# tech_score by degree level
tech_degree_aov <- aov(overall_tech_score ~ highest_degree, data = user_session)
summary(tech_degree_aov)

# behavior_score by degree level
behavior_degree_aov <- aov(weighted_behavior_score ~ highest_degree, data = user_session)
summary(behavior_degree_aov)


tbl_school <- table(  # Create a contingency table to examine the relationship between school tier and pass/fail status, which will be used for a chi-squared test to determine if there is a significant association between these two categorical variables.
    user_session$school_tier_usnews, 
    user_session$pass_flag
)
chisq.test(tbl_school)

tbl_degree <- table(  # Create a contingency table to examine the relationship between degree level and pass/fail status, which will be used for a chi-squared test to determine if there is a significant association between these two categorical variables.
    user_session$highest_degree, 
    user_session$pass_flag
)
chisq.test(tbl_degree)

# Create a boxplot to visualize the distribution of technical scores across different school tiers, allowing for comparison of technical performance based on the tier of the school attended.
ggplot(user_session, aes(x = school_tier_usnews, y = overall_tech_score)) +
  geom_boxplot() +
  labs(title = "Technical Score by School Tier", x = "School Tier", y = "Tech Score")

# Create a boxplot to visualize the distribution of behavior scores across different school tiers, allowing for comparison of behavior performance based on the tier of the school attended.
ggplot(user_session, aes(x = school_tier_usnews, y = weighted_behavior_score)) +
  geom_boxplot() +
  labs(title = "Behavior Score by School Tier", x = "School Tier", y = "Behavior Score")

# Create a boxplot to visualize the distribution of technical scores across different degree levels, allowing for comparison of technical performance based on the highest degree attained.
ggplot(user_session, aes(x = highest_degree, y = overall_tech_score)) +
  geom_boxplot() +
  labs(title = "Technical Score by Highest Degree", x = "Highest Degree", y = "Tech Score")

# Create a boxplot to visualize the distribution of behavior scores across different school tiers, allowing for comparison of behavior performance based on the tier of the school attended.
ggplot(user_session, aes(x = highest_degree, y = weighted_behavior_score)) +
  geom_boxplot() +
  labs(title = "Behavior Score by Highest Degree", x = "Highest Degree", y = "Behavior Score")


# Group the combined user-session data by MBTI personality type and calculate summary statistics for each MBTI type, including average technical score, average behavior score, and pass rate. This will allow for analysis of how different personality types perform in terms of technical and behavior scores, as well as their likelihood of passing the interview.
mbti_summary <- user_session %>%   
  group_by(mbti) %>%
  summarise(
    n = n(),
    avg_tech = mean(overall_tech_score, na.rm = TRUE),
    avg_behavior = mean(weighted_behavior_score, na.rm = TRUE),
    pass_rate = mean(pass_flag, na.rm = TRUE)
  )

mbti_summary

# Perform an ANOVA test to compare the average technical scores across different MBTI personality types, testing the null hypothesis that there is no difference in technical scores based on MBTI type.
tech_aov <- aov(overall_tech_score ~ mbti, data = user_session)
summary(tech_aov)

# Perform an ANOVA test to compare the average behavior scores across different MBTI personality types, testing the null hypothesis that there is no difference in behavior scores based on MBTI type.
behavior_aov <- aov(weighted_behavior_score ~ mbti, data = user_session)
summary(behavior_aov)

tbl <- table(user_session$mbti, user_session$pass_flag)   # Create a contingency table to examine the relationship between MBTI personality type and pass/fail status
chisq.test(tbl)

# Create a boxplot to visualize the distribution of technical scores across different MBTI personality types, allowing for comparison of technical performance based on personality type.
ggplot(user_session, aes(x = mbti, y = overall_tech_score)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Technical Score by MBTI Type",
       x = "MBTI Type",
       y = "Technical Score")

# Create a boxplot to visualize the distribution of behavior scores across different MBTI personality types, allowing for comparison of behavior performance based on personality type.
ggplot(user_session, aes(x = mbti, y = weighted_behavior_score)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Behavior Score by MBTI Type",
       x = "MBTI Type",
       y = "Behavior Score")

# Calculate the distribution of users across different tracks for each MBTI personality type by grouping the user-session data by MBTI type and track, counting the number of users in each group, and calculating the proportion of users in each track for each MBTI type. This will allow for analysis of whether certain personality types are more likely to be associated with specific tracks.
track_distribution <- user_session %>%
  group_by(mbti, track.y) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(mbti) %>%
  mutate(prop = count / sum(count))
track_distribution


tbl_mbti_track <- table(   # Create a contingency table to examine the relationship between MBTI personality type and track, which will be used for a chi-squared test to determine if there is a significant association between these two categorical variables.
  user_session$mbti,
  user_session$track.y
)

chisq.test(tbl_mbti_track)

# Create a bar plot to visualize the distribution of tracks across different MBTI personality types, allowing for comparison of track preferences based on personality type.
ggplot(user_session, aes(x = mbti, fill = track.y)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Track Distribution by MBTI Type",
       x = "MBTI Type",
       y = "Proportion")

tbl_mbti_device <- table(  # Create a contingency table to examine the relationship between MBTI personality type and device type, which will be used for a chi-squared test to determine if there is a significant association between these two categorical variables.
  user_session$mbti,
  user_session$device_type
)
chisq.test(tbl_mbti_device)

# Create a bar plot to visualize the distribution of device types across different MBTI personality types, allowing for comparison of device usage based on personality type.
ggplot(user_session, aes(x = mbti, fill = device_type)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Device Usage by MBTI Type",
       x = "MBTI Type",
       y = "Proportion")


library(caret)
library(pROC)

# Calculate summary statistics for behavioral questions by session_id, including average response time, average response length, average sentiment score, hesitation rate, and average scores for communication clarity, teamwork, and leadership signals. This will allow for analysis of how different behavioral factors are associated with interview outcomes at the session level.
behavior_summary <- data_list[["behavioral_questions"]] %>%
  group_by(session_id) %>%
  summarise(
    avg_response_time = mean(response_time_sec, na.rm = TRUE),
    avg_response_length = mean(response_length_tokens, na.rm = TRUE),
    avg_sentiment = mean(response_sentiment_score, na.rm = TRUE),
    hesitation_rate = mean(hesitation_flag, na.rm = TRUE),
    avg_clarity = mean(communication_clarity_score, na.rm = TRUE),
    avg_teamwork = mean(teamwork_score, na.rm = TRUE),
    avg_leadership = mean(leadership_signal_score, na.rm = TRUE)
  )

user_session <- user_session %>%
  left_join(
    behavior_summary,
    by = "session_id"
  )

# Create the modeling dataset by selecting relevant features from the combined user-session data, including the target variable "pass_flag" and various features related to the user's resume/profile, behavior during the session, and session/system characteristics. This dataset will be used for building predictive models to understand factors influencing interview outcomes.
model_data <- user_session %>%
  select(
    pass_flag,
    # Resume / Profile features
    school_tier_usnews,
    highest_degree,
    stem_degree_flag,
    graduation_year,
    total_experience_years,
    internship_count,
    project_count,
    leadership_experience_flag,
    overall_resume,
    skill_match_score,
    
    # Behavioral features
    avg_response_time,
    avg_response_length,
    avg_sentiment,
    hesitation_rate,
    avg_clarity,
    avg_teamwork,
    avg_leadership,
    
    # Session / System features
    latency_ms_p50,
    latency_ms_p95,
    interview_type,
    messages_exchanged
  )

# Convert the target variable "pass_flag" to a factor with levels "fail" and "pass", and convert categorical features to factors as well. Additionally, remove any rows with missing values from the modeling dataset to ensure that the data is clean and ready for modeling.
model_data <- model_data %>%
  mutate(
    pass_flag = factor(pass_flag, levels = c(0,1), labels = c("fail","pass")),
    
    # Resume features
    school_tier_usnews = as.factor(school_tier_usnews),
    highest_degree = as.factor(highest_degree),
    stem_degree_flag = as.factor(stem_degree_flag),
    
    # Session features
    interview_type = as.factor(interview_type)
  ) %>%
  na.omit() # Remove rows with missing values for modeling

# Check the structure of the modeling dataset to ensure that the target variable and features are correctly formatted for modeling.
str(model_data)

# 80/20 Train-Test Split
set.seed(123)

train_index <- createDataPartition(
  model_data$pass_flag,
  p = 0.8,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Check the distribution of the target variable in the training and testing datasets to ensure that they are similar and that the split has not introduced any bias in terms of class distribution.
prop.table(table(train_data$pass_flag))
prop.table(table(test_data$pass_flag))

# Remove factors with only one level
train_data <- train_data %>%
  select(where(~ !(is.factor(.) && nlevels(.) < 2)))

test_data <- test_data %>%
  select(names(train_data))

# 5-Fold Cross Validation + Logistic Regression
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

cv_model <- train(
  pass_flag ~ .,
  data = train_data,
  method = "glm",
  family = "binomial",
  trControl = control,
  metric = "ROC"
)

print(cv_model)

# Train Final Logistic Regression on Training Set
final_model <- glm(
  pass_flag ~ .,
  data = train_data,
  family = binomial,
  weights = ifelse(train_data$pass_flag == "fail", 1.5, 1)
)

summary(final_model)

# Predict probabilities on the test set using the final logistic regression model, which will allow for evaluation of the model's performance in terms of its ability to discriminate between pass and fail outcomes based on the predicted probabilities.
test_prob <- predict(
  final_model,
  newdata = test_data,
  type = "response"
)

# Calculate the ROC curve and AUC for the test set predictions to evaluate the performance of the logistic regression model in distinguishing between pass and fail outcomes. The ROC curve will show the trade-off between sensitivity and specificity at different probability thresholds, while the AUC will provide a single metric summarizing the overall discriminatory ability of the model.
roc_obj <- roc(test_data$pass_flag, test_prob, levels=c("fail","pass"))
auc_value <- auc(roc_obj)
print(auc_value)

plot(roc_obj, main = "ROC Curve - Test Set")

# Create a confusion matrix to evaluate the performance of the logistic regression model on the test set by comparing the predicted class labels (based on a probability threshold of 0.5) with the actual class labels in the test data. This will allow for assessment of the model's accuracy, sensitivity, specificity, and other performance metrics.
coords(roc_obj, "best", ret="threshold", best.method="closest.topleft")
test_pred <- ifelse(test_prob > 0.5711332, "pass", "fail")
test_pred <- factor(test_pred, levels = c("fail","pass"))

confusionMatrix(test_pred, test_data$pass_flag)


set.seed(123)
# Train-test split
train_index <- createDataPartition(model_data$pass_flag, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Remove factors with only 1 level from the training data, and ensure that the test data has the same columns as the training data after removing any factors with only one level. This step is important to prevent issues during modeling, as factors with only one level do not provide any discriminatory power and can cause errors in certain modeling algorithms.
train_data <- train_data %>% select(where(~ !(is.factor(.) && nlevels(.) < 2)))
test_data <- test_data %>% select(names(train_data))

# Cross-validation control
control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummary
)

# Train Decision Tree
dt_model <- train(
  pass_flag ~ .,
  data = train_data,
  method = "rpart",
  trControl = control,
  metric = "ROC"
)

# Print the results of the decision tree model, including the cross-validation performance metrics and the final model structure. This will allow for evaluation of how well the decision tree model performs in terms of its ability to discriminate between pass and fail outcomes, as well as understanding which features are most important in the decision-making process of the tree.
print(dt_model$finalModel)

# Predict probabilities on the test set using the trained decision tree model, which will allow for evaluation of the model's performance in terms of its ability to discriminate between pass and fail outcomes based on the predicted probabilities.
dt_prob <- predict(dt_model, newdata = test_data, type = "prob")[, "pass"]
roc_dt <- roc(test_data$pass_flag, dt_prob, levels = c("fail","pass"))
print(auc(roc_dt))
plot(roc_dt, main = "Decision Tree ROC")

library(randomForest)
# Train Random Forest
rf_model <- train(
  pass_flag ~ .,
  data = train_data,
  method = "rf",
  trControl = control,
  metric = "ROC",
  importance = TRUE
)

# Print the results of the random forest model, including the cross-validation performance metrics and the variable importance plot. This will allow for evaluation of how well the random forest model performs in terms of its ability to discriminate between pass and fail outcomes, as well as understanding which features are most important in the ensemble of decision trees that make up the random forest.
rf_prob <- predict(rf_model, newdata = test_data, type = "prob")[, "pass"]
roc_rf <- roc(test_data$pass_flag, rf_prob, levels = c("fail","pass"))
print(auc(roc_rf))
plot(roc_rf, main = "Random Forest ROC")

# Random Forest Feature Importance
rf_imp <- varImp(rf_model)
print(rf_imp)
plot(rf_imp, top=15, main="Top 15 Features")


library(xgboost)
library(lightgbm)
library(Matrix)
library(pROC)
library(dplyr)

# Data preprocessing
model_data_boost <- model_data %>%
  mutate(
    pass_flag = ifelse(pass_flag == "pass", 1, 0),
    school_tier_usnews = as.factor(school_tier_usnews),
    highest_degree = as.factor(highest_degree),
    stem_degree_flag = as.factor(stem_degree_flag),
    interview_type = as.factor(interview_type)
  ) %>%
  na.omit()

# Train/Test split (80/20)
set.seed(123)
# Use sample() to create a random index for the training set, ensuring that the split is reproducible by setting a seed. The training set will consist of 80% of the data, while the remaining 20% will be used as the test set for evaluating model performance.
train_idx <- sample(seq_len(nrow(model_data_boost)), size = 0.8 * nrow(model_data_boost))
train_data <- model_data_boost[train_idx, ]
test_data <- model_data_boost[-train_idx, ]

train_x <- train_data %>% select(-pass_flag)
train_y <- train_data$pass_flag
test_x <- test_data %>% select(-pass_flag)
test_y <- test_data$pass_flag


# XGBoost
# Convert to numeric matrix format for XGBoost, ensuring that all factor variables are converted to numeric values. This is necessary because XGBoost requires input data to be in a numeric matrix format, and factors need to be encoded as numeric values (e.g., using as.numeric()) to be used in the model training process.
train_matrix <- as.matrix(train_x %>% mutate(across(where(is.factor), as.numeric)))
test_matrix <- as.matrix(test_x %>% mutate(across(where(is.factor), as.numeric)))
dtrain <- xgb.DMatrix(data = train_matrix, label = train_y)
dtest <- xgb.DMatrix(data = test_matrix, label = test_y)

# Set XGBoost parameters, including the objective function for binary classification, evaluation metric (AUC), maximum tree depth, learning rate, subsample ratio, and column sample ratio. These parameters will control the behavior of the XGBoost model during training and can be tuned to optimize performance.
xgb_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  learning_rate = 0.1,
  subsample = 0.8,
  colsample_bytree = 0.8
)

set.seed(123)
# 5-fold CV
xgb_cv <- xgb.cv(
  params = xgb_params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 10,
  verbose = 0,
  stratified = TRUE
)
# Extract the best number of rounds from cross-validation results, which will be used to train the final XGBoost model. If early stopping was triggered during cross-validation, the best number of rounds will be the iteration at which the best performance was achieved. If early stopping did not trigger, we will use the full number of rounds specified (500 in this case) for training the final model.
best_nrounds <- xgb_cv$best_iteration
# If early stopping did not trigger, use the full number of rounds
if (is.null(best_nrounds) || best_nrounds == 0) best_nrounds <- 100 

# train final XGBoost model using the best number of rounds determined from cross-validation, which will allow for training a model that is optimized based on the performance observed during cross-validation. The final model will be trained on the entire training dataset using the specified parameters and the optimal number of boosting rounds.
xgb_model <- xgb.train(
  params = xgb_params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 0
)

# predict
xgb_pred <- predict(xgb_model, dtest)
roc_xgb <- pROC::roc(test_y, xgb_pred)
cat("XGBoost Test AUC:", pROC::auc(roc_xgb), "\n")
pROC::plot.roc(roc_xgb, main = "XGBoost ROC")

# important features
xgb_imp <- xgb.importance(feature_names = colnames(train_matrix), model = xgb_model)
xgb.plot.importance(xgb_imp, top_n = 15, main = "XGBoost Top 15 Features")


# LightGBM
# LightGBM can handle categorical features directly, so we need to identify which features are categorical and pass that information to the model. This will allow LightGBM to apply appropriate encoding and handling for categorical variables during model training, which can improve performance and interpretability.
categorical_feats <- which(sapply(train_x, is.factor))
# Convert to numeric matrix format for LightGBM, ensuring that all factor variables are converted to numeric values. This is necessary because LightGBM requires input data to be in a numeric matrix format, and factors need to be encoded as numeric values (e.g., using as.numeric()) to be used in the model training process.
lgb_train <- lgb.Dataset(
  data = train_matrix,
  label = train_y,
  categorical_feature = categorical_feats
)
# Create a validation dataset for LightGBM using the test set, which will allow for evaluation of the model's performance on unseen data during training. This validation dataset will be used to monitor the model's performance and apply early stopping if the performance does not improve after a certain number of iterations.
lgb_test <- lgb.Dataset.create.valid(
  lgb_train,
  data = test_matrix,
  label = test_y
)
# Set LightGBM parameters, including the objective function for binary classification, evaluation metric (AUC), maximum tree depth, number of leaves, learning rate, feature fraction, bagging fraction, and bagging frequency. These parameters will control the behavior of the LightGBM model during training and can be tuned to optimize performance.
lgb_params <- list(
  objective = "binary",
  metric = "auc",
  max_depth = 4,
  num_leaves = 7,
  learning_rate = 0.1,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 1
)
# Train
set.seed(123)
lgb_model <- lgb.train(
  params = lgb_params,
  data = lgb_train,
  nrounds = 500,
  valids = list(test = lgb_test),
  early_stopping_rounds = 10,
  verbose = 0
)

# predict
lgb_pred <- predict(lgb_model, test_matrix)
roc_lgb <- pROC::roc(test_y, lgb_pred)
cat("LightGBM Test AUC:", pROC::auc(roc_lgb), "\n")
pROC::plot.roc(roc_lgb, main = "LightGBM ROC")

# Importance features
lgb_imp <- lgb.importance(lgb_model)
lgb.plot.importance(lgb_imp, top_n = 15)