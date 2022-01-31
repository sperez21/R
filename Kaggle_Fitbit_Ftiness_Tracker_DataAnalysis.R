#################################################################
# Created this document to capture process for the Captstone project
#################################################################


################################################################################################
#
# Step 1: Ask - Install packages, load library, and Importing Data
#
################################################################################################

# Install Packages
install.packages("readr")        # To read csv data
install.packages("ggplot2")      # To plot data
install.packages("dplyr")        # To summarize data
install.packages("data.table")   # To modify data tables

# Load library         
library("readr")
library("ggplot2")
library("dplyr")
library("data.table")


# Import the data 

##Import all daily activity data
fitness_tracker_data <- read_csv(file = "C:\\Users\\Sam\\Downloads\\Fitabase\\dailyActivity_merged.csv")

##Import daily intensities merged data
activity_min_tracker <- read_csv(file = "C:\\Users\\Sam\\Downloads\\Fitabase\\dailyIntensities_merged.csv")

##Import modifed activity intensity data
act_narrow <- read_csv(file = "C:\\Users\\Sam\\Downloads\\Fitabase\\dailiyIntensities_Narrow.csv")

###Import sleep data
sleep_day <- read_csv(file = "C:\\Users\\Sam\\Downloads\\Fitabase\\sleepDay_merged.csv")

##Import weight log data
weight_log <- read_csv(file = "C:\\Users\\Sam\\Downloads\\Fitabase\\weightLogInfo_merged.csv")



#################################################################################################
#
# Step 2: Prepare and Preview Data
#
#################################################################################################

# Look at a sample of your data

##Sample of daily activites data
print("Printing Fitness Tracker Data Column Names:")
colnames(fitness_tracker_data)
print("Printing Fitness Tracker Data Frame Structure:")
str(fitness_tracker_data)

##Sample of intensities data
print("Printing Total Activity Minute Tracker Column Names:")
colnames(activity_min_tracker)
print("Printing Total Activity Minute Tracker Data Frame Structure:")
str(activity_min_tracker)

##Sample of sleep data
print("Printing Activity Tracker Breakdown Column Names:")
colnames(act_narrow)
print("Printing Activity Tracker Breakdown Data Frame Structure:")
str(act_narrow)

##Sample of weight log data
print("Printing Weight Log Column Names:")
colnames(weight_log)
print("Printing Wieght Log Data FrameStructure")
str(weight_log)


##################################################################################################
#
# Step 3 + 4: Process the data & Analyze data 
#
##################################################################################################


## Summarize average total steps of users by by activity date 
user_step_summaries <- 
  fitness_tracker_data %>%
  group_by(ActivityDate) %>%
  summarise(avg_steps=mean(TotalSteps))

# Calculate mean of step data to whole number
user_step_means <- format(round(mean(user_step_summaries$avg_steps),0), nsmall = 0)

# Change "user_step_means" variable from character type to numeric 
user_step_means <- as.numeric(user_step_means)

# Print the user average daily steps 
print(paste("User Daily Step Average: ", user_step_means))


###########################################################################################################################
# Minutes the device is used 

## Summarize the total amount of time per day that participants use fitbit
act_min_tracker_sum <- 
  activity_min_tracker %>%
  summarize(tot_sendentary_act = sum(SedentaryMinutes),
            tot_lightlyact = sum(LightlyActiveMinutes),
            tot_fairlyact = sum(FairlyActiveMinutes),
            tot_veryact = sum(VeryActiveMinutes)
            )

# Transpose data to simplify plotting bar chart
act_min_tracker_sum_2 <- transpose(act_min_tracker_sum)

# Name the new rows in the transposed data frame
rownames(act_min_tracker_sum_2) <- colnames(act_min_tracker_sum)

# Name the new column in the tranposed data frame
colnames(act_min_tracker_sum_2) <- "Total_Time"


## Summarize the total amount of participation by users in different activites (Intensity, Weight, or Sleep log)

# Set count variables for distinct participants in activity intensity data
fit_tracker_participant_count <- n_distinct(fitness_tracker_data$Id)

# Set count variable for distinct participants in sleep data
sleep_tracker_particant_count <- n_distinct(sleep_day$Id)

# Set count variable for distinct participants in weight data
weight_tracker_participant_count <- n_distinct(weight_log$Id)

# Create vector for participant data for new data frame  
participant_data <- c(fit_tracker_participant_count, 
                      sleep_tracker_particant_count,
                      weight_tracker_participant_count) 

# Create vector of categories of participant data for new data frame
participant_row_name <- c("ActivityLog",
                          "SleepLog",
                          "WeightLog")

# Create new data frame of particpant data with particant_data and participant_row_name vectors
participant_df <- data.frame(participant_row_name, participant_data)


###########################################################################################################################
#
# Step 5: Build and Share Visualizations 
#
###########################################################################################################################

## Initializing GGPLOT layer to plot average daily user step count 
step_total <- ggplot(data = user_step_summaries,              #Data average daily user steps by day
                     aes(x = avg_steps))                      #Set x-axis global mapping var to data average steps


step_total + 
  geom_histogram(col = "white", bins = 25) +                  #Add histogram plot to GGPLOT layer
  geom_vline(xintercept = user_step_means,                    #Add vertical line to show step mean
             col = "red",                                     #Make x-intercept line red
             lwd = 1) + 
  annotate("text",                                            #Add annotation to plot mean
           x = user_step_means * 0.75,                        #Place text 0.75 space from mean on x
           y = 4,                                             #Place text 4 on y-intercept
           label = paste("Mean =", user_step_means),          #Label x-intercept "Mean = {mean}"
           col = "red",                                       #Text color set to red
           size = 8) +                                        #Set text size to 8
  labs(title = "Average Steps Tracked on Fitbit Daily",       #Set plot title
       y = "# of Users",                                      #Set y-axis title
       x = "Average Daily Steps")                             #Set x-axis title


###########################################################################################################################

## Initializing GGPLOT layer to track total intensity acivity logged by user 
act_plot <- ggplot(data = act_min_tracker_sum_2,                         #Data uses intensity activity log tracker
                   mapping = aes(x = colnames(act_min_tracker_sum),      #Set x-axis global mapping var categories intensity levels
                                 y = Total_Time,                         #Set y-axis global mapping var to total time logged
                                 fill = colnames(act_min_tracker_sum)))  #Fill bar color by category of intensity level
                              

act_plot +
  geom_col() +                                                           #Add column plot to GGPLOT layer
  labs(title = "Total Time in Activity Intensity (minutes)",             #Set plot title
       x = "Intensity Level",                                            #Set x-axis title
       y = "Time (minutes)")                                             #Set y-axis title


###########################################################################################################################

## Initializing GGPLOT layer to track intenstiy level broken down intensity level by user 
act_user_plot <- ggplot(data = act_narrow,                               #Data uses modified activity log tracker 
                        aes(x = Minutes,                                 #Set x-axis global mapping var to minutes logged
                            y = Intensity,                               #Set y-axis global mapping var to intensity category 
                            fill = Intensity))                           #Fill bar color by intensity level        
                        

act_user_plot + 
  geom_col() +                                                           #Add column plot to GGPLOT layer
  facet_wrap(~Id) +                                                      #Add multi-panel plots by user 
  labs(title = "Intensity Level Recorded by User (minutes)",             #Set plot title
       x = "Minutes",                                                    #Set x-axis title
       y = "Intensity Level")                                            #Set y-axis title

###########################################################################################################################

## Initialzing GGPLOT layer to track participant activity data
participant_plot <- ggplot(data = participant_df,                        #Data used participant data frame
                           aes(x = "",                                   #Set x-axis global mapping var to empty
                               y = participant_data,                     #Set y-axis global mapping var to participant data 
                               fill = participant_row_name))             #Fill color category data by activity tracked


participant_plot + 
  geom_col(width = 1) +                                                      #Add column data to GGPLOT layer
  coord_polar("y", start = 0) +                                              #Set to polar coordinates for pie chart
  geom_text(aes(label = participant_data),                                   #Add data labels to the pie chart
            position = position_stack(vjust = 0.5)) +                        #Select position of the labels in the pie chart
  theme_void() +                                                             #Remove unecessary layers only show pie chart
  labs(title = "Pie chart of Number Particpants by Activity (Total = 33)")   #Add plot title 

