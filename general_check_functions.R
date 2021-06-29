
# loading libraries -------------------------------------------------------

library(dplyr)
library(readxl)
library(lubridate)
source("audit_function.R")


# Reading raw data --------------------------------------------------------
# Available in REACH Dropbox and link is given in googlesheet
df <- read_excel("AGORA_Endline_2021_HH_Final_Version_-_all_versions_-_False_-_2021-06-06-11-58-36.xlsx")





# Global variables --------------------------------------------------------
assessment_start_date <- as.Date("2021-06-09")

time_min <- 15

time_max <- 60
max_interv <- 10


# calling time_check_audit function --------------------------------------------------
df <- time_check_audit(df, "_uuid", time_min, time_max)



# time_check without audit ------------------------------------------------
df <- time_check(df, time_min,time_max)



# Survey made before first day of data collection -------------------------
started_before <-function(df,df_date_today,assessment_start_date,var_list= c()){
  df %>% 
    filter(df_date_today < assessment_start_date) %>%
    select(var_list)
}



# Survey with short interview duration ------------------------------------
short_interview <- function(df,time_min, var_list = c()){
  
  #checking if interview_duration exist in df
  if("interview_duration" %in% names(df)){
  df %>% 
    filter(interview_duration < time_min) %>% 
    select(var_list)
  }else{
    # to get interview duration we are calling time_check_audit function
    df <- time_check_audit(df,"_uuid",time_min, time_max)
    df %>% 
      filter(interview_duration < time_min) %>% 
      select(var_list)
  }

}




# Duplicated survey- UUID -------------------------------------------------
duplicated_check <- function(df,uuid){
  
  df %>% mutate(duplicated_uuid = case_when(
    duplicated(uuid, fromLast = T) ~ "yes",
    TRUE ~ "no"))

}


# listing duplicated surveys
duplicate_surveys <- function(df,uuid, var_list=c()){
  
  df[duplicated(df[[uuid]]) | duplicated(df[[uuid]], fromLast = T),] %>%
    select(var_list)
}



# Deleted interview -------------------------------------------------------

deleted_surveys <- function(df){
  
  if("duplicated_uuid" %in% names(df) & "CHECK_interview_duration" %in% names(df) ){
  df %>% mutate(deleted= case_when( 
    duplicated_uuid == "yes" | CHECK_interview_duration == "Too short" ~ "yes",
    TRUE ~ "no"
  ))
  }else{
    df <- duplicated_check(df, "_uuid")
    df <- time_check_audit(df,"_uuid",time_min, time_max)
    df %>% mutate(deleted= case_when( 
      duplicated_uuid == "yes" | CHECK_interview_duration == "Too short" ~ "yes",
      TRUE ~ "no"
    ))
    
  }
}

# Surveys made in the future  ---------------------------------------------
future_survey <- function(df,date_today,var_list=c()){
  df %>%
    filter(date_today > as.Date(`_submission_time`)) %>%
    select(var_list)
}



# Survey with different start and end date --------------------------------
survey_different_start_end <- function(df,start,end,var_list=c()){
  df %>% 
    filter(as.Date(end) != as.Date(start)) %>% 
    select (var_list)
}



# Surveys ending before they start ----------------------------------------
ending_before_start <- function(df,start,end,var_list=c()){
  
  df %>%
    filter(as.POSIXct(start) > as.POSIXct(end)) %>%
    select(var_list)
    
}


# Number of surverys submitted per day ------------------------------------

survey_status_per_day <- function(df,date){ 
  if("deleted" %in% names(df)){
  df %>%
  group_by(Date = date) %>%
  summarise(Accepted = sum(deleted == "no", na.rm = T),
            Deleted = sum(deleted == "yes", na.rm = T ),
            Total_submissions = n()
  )
  }else{
    df <- deleted_surveys(df)
    df %>%
      group_by(Date = date) %>%
      summarise(Accepted = sum(deleted == "no", na.rm = T),
                Deleted = sum(deleted == "yes", na.rm = T ),
                Total_submissions = n()
      )
  }
}





# Number of Survey submitted per day by enumerator ----------------------------------------------------
survey_per_day_enumerator <-function(df,date,enumerator_uuid) {
  if("deleted" %in% names(df)){
  df %>%
    group_by(Date = date, Enumerator = enumerator_uuid) %>%
    summarise(Accepted = sum(deleted == "no", na.rm = T),
              Deleted = sum(deleted== "yes", na.rm = T ),
              Total_submissions = n(),
              Avergage_survey = round(mean(sum(Accepted, Deleted, na.rm = T), na.rm = T),0)
    )
  }else{
    df <- deleted_surveys(df)
    df %>%
      group_by(Date = date, Enumerator = enumerator_uuid) %>%
      summarise(Accepted = sum(deleted == "no", na.rm = T),
                Deleted = sum(deleted== "yes", na.rm = T ),
                Total_submissions = n(),
                Avergage_survey = round(mean(sum(Accepted, Deleted, na.rm = T), na.rm = T),0)
      )
  }
}
# Number of worked day per enumerator  ------------------------------------

no_worked_per_day <- function(df,enumerator_id){
  df %>% group_by(enumerator_id) %>% 
    summarise(worked_days= n())
}


# Average interview duration by enumerator --------------------------------
avg_survey_length <-function(df,enumerator_uuid){
  if("interview_duration" %in% names(df)){
  df %>%
    group_by(Enumerator = enumerator_uuid) %>%
    summarise(
      Average_duration = round(mean(interview_duration, na.rm = T),0),
      Total_submissions = n()
    )
  }else{
    df <- time_check_audit(df, time_min,time_max)
    df %>%
      group_by(Enumerator = enumerator_uuid) %>%
      summarise(
        Average_duration = round(mean(interview_duration, na.rm = T),0),
        Total_submissions = n()
      ) 
  }
}




# Calling functions  ------------------------------------------------------

#calling started_before function to flag if interviews are started before first day of assessment
start_b4 <- started_before(df,df$today,assessment_start_date,var_list = c(
                                                                          "Date" = "today",
                                                                          "Province" = "province_name",
                                                                          "Manteqa" = "manteqa_name",
                                                                          "Settlement" = "village_name",
                                                                          "Enumerator" = "enumerator_id",
                                                                          "Start" = "start",
                                                                          "End" = "end",
                                                                          
                                                                          "UUID "= "_uuid"
                                                                          ))

#flagging short interviews usually those interviews which are under 20 or 15 minutes
short <- short_interview(df,time_min,var_list = c("Date" = "today",
                                                  "Enumerator" = "enumerator_id",
                                                  "Province" = "province_name",
                                                  "Manteqa" = "manteqa_name",
                                                  "Settlement" = "village_name",
                                                  "Start" = "start",
                                                  "End" = "end",
                                                  "Duration" = "interview_duration",
                                                  "UUID" = "_uuid"))

# creating a duplicated_uuid columns to flag  duplicated survey
df <-  duplicated_check(df,'_uuid')

#listing and displaying  duplicated surveys
dup <- duplicate_surveys(df,"_uuid",var_list = c("Date" = "today",
                                         "Enumerator" = "enumerator_id",
                                         "Start" = "start",
                                         "End" = "end",
                                         "UUID "= "_uuid"))

# Flagging surveys made in future
future <- future_survey(df,df$today,var_list = c(
                                        "Enumerator" = "enumerator_id",
                                         "Start" = "start",
                                         "End" = "end",
                                         "UUID "= "_uuid",
                                        "Date" = "today",
                                        "Submission_date" = "_submission_time"
                                        ) 
                        )





#creating deleted column to flag surveys to be deleted
df <- deleted_surveys(df)





survey_different_start_end <- survey_different_start_end(df,start,end, var_list = c("Date" = "today",
                                                                                    "Enumerator" = "enumerator_id",
                                                                                    "Start" = "start",
                                                                                    "End" = "end",
                                                                                    "UUID "= "_uuid"))

ending_b4_start <- ending_before_start(df,start,end,var_list = c("Date" = "today",
                                                                 "Enumerator" = "enumerator_id",
                                                                 "Start" = "start",
                                                                 "End" = "end",
                                                                 "UUID "= "_uuid"))

#calling survey_status_per_day
survey_status <- survey_status_per_day(df,df$today)

#average_duration
avg_duration <- avg_survey_length(df,df$enumerator_id)

#calling survey status per day by enumerator
survey_status_enumerator <- survey_per_day_enumerator(df,df$today,df$enumerator_id)

#number of days enumerator worked
no_work <- no_worked_per_day(df,enumerator_id)

#adding number of days enumerator worked to survey status per day by enumerator
survey_status_enumerator <- left_join(survey_status_enumerator,no_work, by=c("Enumerator"= "enumerator_id"))
