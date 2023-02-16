# %% [markdown]
# Install packages and load packages

# %% [markdown]
# Reading data from csv file which is the combined data of all 12 months.

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:03:41.701329Z","iopub.execute_input":"2023-02-16T17:03:41.702955Z","iopub.status.idle":"2023-02-16T17:03:55.816460Z"}}
df<-read_csv("../input/12-months-merged-data/New_Combined_File.csv",show_col_types = "FALSE")

# %% [markdown]
# Check number of rows and columns in the data frame

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:04:01.266032Z","iopub.execute_input":"2023-02-16T17:04:01.267615Z","iopub.status.idle":"2023-02-16T17:04:01.290354Z"}}
print(ncol(df))
print(nrow(df))

# %% [markdown]
# Add columns ride_length, day_of_week and month to the data frame

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:04:06.304466Z","iopub.execute_input":"2023-02-16T17:04:06.306072Z","iopub.status.idle":"2023-02-16T17:04:09.719093Z"}}
df$ride_length <- abs(difftime(df$ended_at,df$started_at,units="mins"))
df$day_of_week <- weekdays(df$started_at)
df$month <- month(df$started_at)

# %% [markdown]
# Check rows and columns again

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:04:16.032058Z","iopub.execute_input":"2023-02-16T17:04:16.033534Z","iopub.status.idle":"2023-02-16T17:04:16.050600Z"}}
print(ncol(df))
print(nrow(df))

# %% [markdown]
# 

# %% [markdown]
# Filter data if ride_length not equal to 0

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:04:19.829535Z","iopub.execute_input":"2023-02-16T17:04:19.831089Z","iopub.status.idle":"2023-02-16T17:04:21.627403Z"}}
# remove this data with 0 min ride_length and calculate max,min ride_length again
delete_Rows_with_zero_mins <- df%>%
filter(!if_any(ride_length,~.==0))
# Calculate max, min and mean rider length
max <- max(df$ride_length)
print(max)
min<- min(df$ride_length)
print(min)
mean <- mean(df$ride_length)
print(mean)

# %% [markdown]
# 

# %% [markdown]
# Create table for casual,annual members,Bike type

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:04:25.215521Z","iopub.execute_input":"2023-02-16T17:04:25.217034Z","iopub.status.idle":"2023-02-16T17:04:27.400336Z"}}
#Create table for casual and annual members
table(delete_Rows_with_zero_mins["member_casual"] )
# table for rideable_type
table(delete_Rows_with_zero_mins["rideable_type"])

# %% [markdown]
# Rename the data frame

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:04:34.806303Z","iopub.execute_input":"2023-02-16T17:04:34.807829Z","iopub.status.idle":"2023-02-16T17:04:34.820639Z"}}
#assing new data fram to rename it
df_clear_data<- delete_Rows_with_zero_mins

# %% [markdown]
# Create graph for number of rides per day for casual and annual member

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:04:38.471147Z","iopub.execute_input":"2023-02-16T17:04:38.472694Z","iopub.status.idle":"2023-02-16T17:04:57.421763Z"}}
ggplot(data=df_clear_data,aes(x= day_of_week))+
  geom_bar(mapping= aes (fill=rideable_type))+
stat_count(geom="text",aes(label=..count..),size= 2.5,position= position_stack(vjust=1.1))+
labs(title="Number of rides per day for both type members", x="Day of Week",y="Total Riders",fill="Bike Type")+
scale_x_discrete(limits= c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))

# %% [markdown]
# This graph shows that on Saturday users are more active than other days.

# %% [code]
Now Split number of rider for cassual and annual users

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T16:49:33.450407Z","iopub.execute_input":"2023-02-16T16:49:33.452082Z","iopub.status.idle":"2023-02-16T16:49:56.142961Z"}}
# split Number of rides for casual members and annaual members
ggplot(data= df_clear_data,aes(x= day_of_week))+
geom_bar(mapping =aes(fill= rideable_type))+
stat_count(geom="text",aes(label=..count..),size= 2.5,position = position_stack(vjust=1.1))+
labs(tittle= "Number of rides per day",x= "Day of Week",y="Total Rides",fill ="Bike Type")+
facet_grid(~member_casual)+
scale_y_continuous(labels= comma)+
scale_x_discrete(limits= c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
theme(axis.text.x = element_text(angle=90))

# %% [markdown]
# This graph shows that cassual users are more active on Saturday while annual users are more active on Thursday and annual users not using docked bike.

# %% [markdown]
# Calculate number of rider per month

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:05:19.851724Z","iopub.execute_input":"2023-02-16T17:05:19.853353Z","iopub.status.idle":"2023-02-16T17:05:42.277489Z"}}
# Calculate number of rides per month
ggplot(data=df_clear_data,aes(x= month.abb[month]))+
geom_bar(mapping= aes(fill=rideable_type))+
stat_count(geom="text",aes(label=..count..),size= 2.5,position = position_stack(vjust=1.3))+
labs(tittle= "Number of rides per month",x= "Months",y = "Total Rides",fill= "Bike Type")+
facet_grid(~member_casual)+
scale_y_continuous(labels = comma)+
scale_x_discrete(limits= c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))


# %% [markdown]
# So we can we that June, July,August months have more rides. Users are more active in summer season.

# %% [markdown]
# Calculate the ride duration per month

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:06:44.249875Z","iopub.execute_input":"2023-02-16T17:06:44.251554Z","iopub.status.idle":"2023-02-16T17:06:44.933971Z"}}
monthly_ride_duration_casual_member<- df_clear_data %>% filter(member_casual=="casual")%>% group_by(month)%>% summarise(duration= sum(ride_length))

ggplot(data= monthly_ride_duration_casual_member)+ geom_col(aes(x= month,y=duration),
fill= "green")+ labs(title="Monthly Casual Rider Duration", y = "Ride duration in mins",x="Month", fill= "Bike Type")+
scale_y_continuous(labels = comma)+ 
scale_x_discrete(limits= c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+ 
theme_classic()

# %% [markdown]
# Weekly ride dureation for casual users

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T17:05:09.621656Z","iopub.execute_input":"2023-02-16T17:05:09.623304Z","iopub.status.idle":"2023-02-16T17:05:10.577865Z"}}
# Weekly cassual ride duration
weekly_ride_duration_casual_member <- df_clear_data%>%
filter(member_casual=="casual")%>%
group_by(day_of_week)%>%
summarise(duration =sum(ride_length))

# plot graph
ggplot(data= weekly_ride_duration_casual_member )+
geom_col(aes(x= day_of_week,y=duration),fill= "light blue")+
#geom_text(aes(label= duration,x= day_of_week,y= duration), position = position_dodge(width = 0.9), vjust = -0.5)+
labs(title="Weekly ride duration for casual members",x="Day of week", y = "Ride duration in mins")+
scale_y_continuous(labels= comma)+
scale_x_discrete(limits =  c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))+
theme_classic()


# %% [markdown]
# So, June and July has more durations for rides.

# %% [markdown]
# Calculate the popular stations for users

# %% [code] {"execution":{"iopub.status.busy":"2023-02-16T16:50:19.835875Z","iopub.execute_input":"2023-02-16T16:50:19.837347Z","iopub.status.idle":"2023-02-16T16:50:20.524195Z"}}
# Popular start stations for member types
df_routes_taken <- df_clear_data%>% 
  count(member_casual, start_station_name, sort = TRUE) %>% 
filter(start_station_name!= "NA") %>%
top_n(10)
ggplot(df_routes_taken) +
geom_bar(mapping = aes(y = start_station_name, x = n, fill = member_casual), stat = "identity") + 
theme_classic() + 
scale_x_continuous(label=comma)+
labs(x = "No. of trips started", y = "start station name")


# %% [markdown]
# Here, We can see that Streeter Dr & Grand Ave has more users.

# %% [markdown]
# **Conclusion**

# %% [markdown]
# Now, we can see that we need to focus on weekends during the summer season to attract more users. so that they can take out an annual membership.

# %% [markdown]
# For further analysis, we can check the timing to attract more users.