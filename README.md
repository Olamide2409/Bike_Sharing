# Case Study 1: Transforming occasional(casual) cyclists into yearly subscribers(members).
# Introduction
This case study showcases a data analysis project undertaken within the framework of the Google Data Analytics Professional Certificate program. The project centers on Cyclistic, a bike-sharing company, and serves as the capstone project for the course. This case study will immerse you in the real-world tasks that a junior data analyst encounters. As an analyst in the marketing team working alongside different characters and team members. The aim is to tackle significant business questions by following the data analysis process, which includes stages such as asking, preparing, processing, analyzing, sharing, and taking action. 
# Tools used in analysis
Data Cleaining: R
Data analysis: R, Microsoft Excel
Visualization: Tableau
# Data Analysis Process
# Ask Phase: 
In the ask phase phase, it involved formulating clear and specific questions or objectives that would be addressed. This phase sets the foundation for the entire analysis and includes the following questions:
How do annual members and casual riders use Cyclistic bikes differently? 
This is further broken down into:
1.  Average ride length (Seconds)
2.  Frequency of Trips per month
3.  Total Trips Per day
4.  Trip Duration per month(Seconds)

# Prepare Phase:
The dataset used for this case study is a public data set made available by Motivate International Inc. It contains quarterly data covering the year 2018 from the first quarter to the fourth quarter. They all contain 12 columns providing information related to the Bike rides.
# Process Phase:
# Data Cleaning in R:
Bike_Sharing
Install required packages
tidyverse for data import and wrangling
lubridate for visualization

# Helps wrangle data
library(tidyverse)
# Helps wrangle data attributes
library(lubridate)
# Helps to import data
library(readr)

# Collect the data, Upload Divvy datasets (csv files here)
q1_2018 <- read_csv(“Downloads/Divvy_Trips_2018_Q1.csv”) 
q2_2018 <- read_csv(“Downloads/Divvy_Trips_2018_Q2.csv”)
q3_2018 <- read_csv(“Downloads/Divvy_Trips_2018_Q3.csv”) 
q4_2018 <- read_csv(“Downloads/Divvy_Trips_2018_Q4.csv”)

View(q1_2018) 
View(q2_2018) 
View(q3_2018) 
View(q4_2018)

# Inspect column names for consistency
colnames(q1_2018) 
colnames(q2_2018) 
colnames(q3_2018) 
colnames(q4_2018)

# Rename column to be consistent as the entire data frames for uniformity
colnames(q1_2018)[1] = “trip_id” 
colnames(q1_2018)[2] = “start_time” 
colnames(q1_2018)[3] = “end_time” 
colnames(q1_2018)[4] = “bikeid” 
colnames(q1_2018)[5] = “tripduration” 
colnames(q1_2018)[6] = “from_station_id” 
colnames(q1_2018)[7] = “from_station_name” 
colnames(q1_2018)[8] = “to_station_id” 
colnames(q1_2018)[9] = “to_station_name” 
colnames(q1_2018)[10] = “usertype” 
colnames(q1_2018)[11] = “gender” 
colnames(q1_2018)[12] = “birthyear”

str(q1_2018)
# Inspect the dataframes and look for inconguencies
str(q1_2018) 
str(q2_2018) 
str(q3_2018) 
str(q4_2018)

# Convert trip_id to character so that they can stack correctly across the data frames
q1_2018 <- mutate(q1_2018, trip_id = as.character(trip_id)) 
q2_2018 <- mutate(q2_2018, trip_id = as.character(trip_id)) 
q3_2018 <- mutate(q3_2018, trip_id = as.character(trip_id)) 
q4_2018 <- mutate(q4_2018, trip_id = as.character(trip_id))

# Stack individual quarter’s data frames into one big data frame
all_2018trips <- bind_rows(q1_2018, q2_2018, q3_2018, q4_2018)
View(all_2018trips)

# Remove columns that wont be considered during the analysis 
all_2018trips <- all_2018trips %>% select(-c(birthyear, gender))
View(all_2018trips)

# Inspect the new dataframe that has been created
colnames(all_2018trips) 
nrow(all_2018trips) 
dim(all_2018trips) 
head(all_2018trips)
tail(all_2018trips) 
str(all_2018trips) 
summary(all_2018trips)

# Check for column uniformity
(all_2018trips$trip_id)

# Adding a column of ride length
all_2018trips <- all_2018trips %>% add_column(ride_length = ” “)

# The difference to get the ride length in unit seconds
all_2018tripsr i dele n g t h<− d i f f t im e¿end_time,all_2018trips$start_time, units = “secs”)

# Changing the datatype in the ride length column for ease in data calculations
all_2018trips <- mutate(all_2018trips, ride_length = as.numeric(ride_length))

# Changing the data type for the start time and end time column
all_2018trips_v1 <- mutate(all_2018trips, start_time = as.Date(start_time), end_time = as.Date(end_time))View(all_2018trips_v1)

# Addition of columns to the table to split the start time column 
all_2018trips <- all_2018trips %>% add_column(date = ” “, month =”“, day =”“, year =”“)

# Formatting the newly created column and splitting the start time into appropriate column for data aggregation
all_2018tripsdate<-as.Date(all_2018 tripsstart_time) 
all_2018tripsmonth<-format(as.Date(all_2018 tripsdate), “%m”) 
all_2018tripsday<-format(as.Date(all_2018 tripsdate), “%d”) 
all_2018tripsyear<-format(as.Date(all_2018 tripsdate), “%Y”) 
all_2018tripsday_o f_w eek<-format(as.Date(all_2018 tripsdate), “%A”)

# Data cleaning for the ride_length column to remove negative data
all_2018trips_v2 <- all_2018trips[!(all_2018trips$ride_length<0),] View(all_2018trips_v2)

# Analyze Phase:
We dig deep into the data to discover patterns and address the key findings on how annual members and casual riders use Cyclistic bikes differently. To point out the findings, the analyses were performed in R Studio. 
# CONDUCT DESCRIPTIVE ANALYSIS
mean(all_2018trips_v2ride_l ength)
median(all_2018 trips_v 2ride_length) 
max(all_2018trips_v2ride_l ength)
min(all_2018 trips_v 2ride_length) 
summary(all_2018trips_v2$ride_length)

# AGGREGATING RELATIONSHIP
(all_2018trips_v2ride_l ength all_2018 trips_v 2usertype + all_2018trips_v2$day_of_week)

# Order the days of the week from Sunday to Saturday
all_2018trips_v2day_o f_w eek<-ordered(all_2018 trips_v 2day_of_week, levels=c(“Sunday”, “Monday”, “Tuesday”, “Wednesday”, “Thursday”, “Friday”, “Saturday”))

# Average ride time by each day for usertype(customers/subscribers)
(all_2018trips_v2ride_l ength all_2018 trips_v 2usertype + all_2018trips_v2$day_of_week)

# Analyzing ridership data by type and weekday(ends at the true statement)
# Creates weekday field using wday()(ends at weekday)
# Groupby usertype and weekday, average number of trips by weekday and usertype.
all_2018trips_v2 %>% mutate(weekdays = wday(start_time, label = TRUE)) %>% group_by(usertype, weekdays) %>% summarise(trip_id = n())

# Total number of rides per month
all_2018trips_v4 <- all_2018trips_v2 %>% group_by(usertype, month) %>% summarise(trip_id = n()) View(all_2018trips_v4)

# Average ride length per usertype and day of the week
aggregate(all_2018trips_v2ride_l ength all_2018 trips_v 2usertype + all_2018trips_v2$day_of_week, FUN = mean)

# Average trip duration per month
aggregate(all_2018trips_v2tripduration all_2018 trips_v 2usertype + all_2018trips_v2month,FUN=mean)trips_m onth<-aggregate(all_2018 trips_v 2tripduration ~ all_2018trips_v2usertype+all_2018 trips_v 2month, FUN = mean)

# Average trip duration per week day
aggregate(all_2018trips_v2tripduration all_2018 trips_v 2usertype + all_2018trips_v2$day_of_week, FUN = mean)

# Total number of rides per usertype
sum(all_2018trips_v2$usertype == "Subscriber") sum(all_2018trips_v2$usertype == “Customer”)

# Average duration of trips
mean(all_2018trips_v2$tripduration)

# EXPORTING SUMMARY OF KEY FINDINGS
# Average ride length
counts <- aggregate(all_2018trips_v2ride_l ength all_2018 trips_v 2usertype + all_2018trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = “Downloads/average_ride_length.csv”)

# Total number of trips per week day by usertype
all_2018trips_v3 <- all_2018trips_v2 %>% mutate(weekdays = wday(start_time, label = TRUE)) %>% group_by(usertype, weekdays) %>% summarise(trip_id = n())
View(all_2018trips_v3)
write.csv(all_2018trips_v3, file = “Downloads/all_trips_2018v3.csv”)

# Total number of trips per month for each usertype
trips_month <- aggregate(all_2018trips_v2tripduration all_2018 trips_v 2usertype + all_2018trips_v2$month, FUN = mean)
View(trips_month)
write.csv(trips_month, file = “Downloads/trips_month.csv”)

# Frequeny of the trips grouped by month
all_2018trips_v5 <- all_2018trips_v2 %>% group_by(month, usertype) %>% summarise(freq = n())
View(all_2018trips_v5)
write.csv(all_2018trips_v5, file = “Downloads/frequency_month.csv”)

# Share Phase:
# Number of Trips per Day
During the weekdays between Monday through Friday, Member riders had the highest number of trips per day with the highest day been Wednesday while Casual riders had their highest number of trips during the weekend.
![Number of Trips per Day](https://github.com/Olamide2409/Bike_Sharing/assets/147916748/ce75b8ee-62cc-4ff7-b3e1-23a8f3427f6a)

# Frequency of Trips per Month
Over the Summer, the frequency of the trips got to the highest level. Casual riders got to the highest level in July while Member riders got to the highest level in August.
![Frequency of Trips per Month](https://github.com/Olamide2409/Bike_Sharing/assets/147916748/1851f062-3790-44e6-9917-f5811ec93cf5)

#Trip Duration per Month
In January/February the trip duration was highest for casual riders while for member riders it was the highest between June through to September. The casual riders spend longer times in between rides than the member riders.
![Trips Duration per Month](https://github.com/Olamide2409/Bike_Sharing/assets/147916748/f9d99cf1-3bd4-42c7-8d8c-c237d6a2d2b6)

# Average Trip Duration per Day (Seconds)
The casual riders spend more timehave its highest period on Sunday and this is similar to the member riders. On average, casual riders experience notably lengthier ride durations in comparison to riders who are members.
![Average Trips Duration per Month](https://github.com/Olamide2409/Bike_Sharing/assets/147916748/b125b40c-af70-4eee-8dd0-0dc639a952b1)

# Act Phase: Findings
Differences between Casual and Member Riders:
1. In comparison to casual riders, member riders have completed more trips an average six times more often throughout the week than they have during the weekends.
   The weekend happpens to be when casual riders made the most journeys, with Sunday recording the longest duration for a day. 
2. The frequency of trips peaked over the summer. While Member riders peaked in August, Casual riders peaked in July. Despite the fact that they both reached their minimal values in February
3. The trip time was longest for casual riders in January/February, whereas it was longest for member riders during the summer (June-September). Compared to member rides, casual riders have significantly longer ride durations.
4. Similar to its member riders, the casual riders' peak activity of bike riding occurs during the weekend, with Sunday being the peak day. Comparing casual riders and member riders, the casual riders use their bikes an average of four times per day.

# Strategies to Convert Casual Riders to Member Riders:
**Free Trial Period:** Offer a free trial period for potential subscribers. Allow them to experience the benefits of a subscription for a limited time before committing, so they can see its value.
**Easy Sign-up Process:** Simplify the subscription sign-up process, making it quick and hassle-free. A complex sign-up can deter potential subscribers.
**Subscription Plans:** Create tiered subscription plans that offer various levels of benefits, such as unlimited bike usage, premium maintenance, or exclusive discounts. This gives customers options that cater to their needs and encourages them to subscribe. Customers are encouraged to commit for a longer period by offering significant discounts for annual subscriptions. This reduces the perceived cost of subscribing.
**Referral Programs/Loyalty Programs:** Implement a referral program where current subscribers can earn rewards or discounts by referring new subscribers. This leverages the existing subscriber base to bring in new customers. Reward frequent customers with loyalty points or benefits, and then introduce subscription offers with exclusive perks that can be redeemed using these points.
**Exclusive Content and Events:** Offer exclusive content or access to special biking events for subscribers. This can create a sense of belonging and community among subscribers.
Educational Content: Share content that educates customers on the long-term benefits of subscribing, such as cost savings, convenience, and the positive impact on their biking experience.
**Feedback and Improvement:** Actively seek feedback from customers to continuously improve subscription offerings. This shows that you value their input and are committed to enhancing their experience.
**Email Marketing:** Use targeted email marketing to inform customers about subscription options and benefits. Regularly update them about new features and promotions.
**Partnerships:** Collaborate with other local businesses or services that align with biking, and offer joint subscription deals. For example, partner with a local gym for fitness-oriented subscriptions.
**Community Engagement:** Foster a biking community through social media groups or forums and use these platforms to engage potential subscribers and promote the benefits of membership.
 



