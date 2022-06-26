install.packages("ggplot2")
install.packages("tidyverse")
install.packages("sqldf")
install.packages("gsubfn")
install.packages("proto")
install.packages("RSQLite")
install.packages("caTools")
install.packages("xtable")
install.packages("DescTools")
install.packages("Hmisc")
install.packages("stringi")
install.packages("corrplot")
library(corrplot)
library(Hmisc)
library(stringi)
library(DescTools)
library(sqldf)
library(gsubfn)
library(proto)
library(RSQLite)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caTools)
library(readr)


#read the data
accidents <-read.csv("Accidents.csv")
#rm(accidents)
#1)
#get column(there is no NA for this two column)
data1=accidents[,c(5,6)]
#change the column name
data1$Accident_Severity <- ifelse(data1$Accident_Severity == '1', 'Fatal', data1$Accident_Severity)
data1$Accident_Severity <- ifelse(data1$Accident_Severity == '2', 'Serious', data1$Accident_Severity)
data1$Accident_Severity <- ifelse(data1$Accident_Severity == '3', 'Slight', data1$Accident_Severity)
#plot 
data1 <- data1 %>%
  mutate(Accident_Severity=as.factor(Accident_Severity))
plot1 <- ggplot(data=data1)+
  geom_bar(mapping=aes(x=Police_Force,  fill=Accident_Severity))
plot1 + scale_y_continuous("The number of accidents")+
  labs(title = bquote("Bar chart"))+
  theme(plot.title = element_text(hjust = 0.5))
#table
df1=ftable(data1, data1$Accident_Severity, data1$Police_Force)
#AnovaTest
anova1 = aov(data1$Police_Force ~ data1$Accident_Severity, data=data1)
summary(anova1)
#impoet table
write.ftable(df1,"/Users/vivian/Desktop/研究所/Foundation of Business Analytics and Management Science/assignment/ftable.csv",row.names=T,sep=",")

#2)
#get column
df_week <- subset(accidents,Day_of_Week>0, select=(Day_of_Week), drop=FALSE)
df_week
(df_weekday<-subset(df_week,Day_of_Week>1 & Day_of_Week<7,select=(Day_of_Week),drop=FALSE))
(df_weekday$Accident_count<-1)
df_weekday
(df_weekend<-subset(df_week,Day_of_Week==1 | Day_of_Week==7,select=(Day_of_Week),drop=FALSE))
(df_weekend$Accident_count<-1)
df_weekend
#calculating
(weekday_row_count <- nrow(df_weekday))
weekday_row_count_mean<-as.integer(weekday_row_count/5)
weekday_row_count_mean
#Number of rows is number of accidents
weekend_row_count <- nrow(df_weekend) 
weekend_row_count
weekend_row_count_mean= as.integer(weekend_row_count/2)
weekend_row_count_mean
#plot
vec<- append(weekday_row_count_mean,weekend_row_count_mean)
vec
M<- c('Accidents in Weekdays', 'Accidents in Weekends')
barplot(vec,names.arg =M,ylim = c(0,20000), col='blue')
df_weekday_group<- df_weekday%>%
  group_by(Day_of_Week)%>%
  summarise(Accident_Group=sum(Accident_count))
df_weekday_group
df_weekend_group<- df_weekend%>%
  group_by(Day_of_Week)%>%
  summarise(Accident_Group=sum(Accident_count))
df_weekend_group
#t-test
t.test(df_weekday_group$Accident_Group, df_weekend_group$Accident_Group)

#3)
#gets column 
data6 <- accidents[,c(6,11)] 
data6
#change name of column Accident_Severity(fatal)
data6$Accident_Severity <- ifelse(data6$Accident_Severity == '1', 'Fatal', data6$Accident_Severity) 
data6
#only take the fatal data
data7 <- filter(data6,Accident_Severity %in% c("Fatal")) 
fatal_accidents_time <- data7[order(data7$Time, decreasing = F),] 
#separate the time in to hr and min
hr_min = str_split_fixed(fatal_accidents_time$Time, ":", 2) 
#merge the data with only hr(column)
new_fatal_accidents_time <- cbind(fatal_accidents_time, hr_min[,c(1)])
#change the columns
colnames(new_fatal_accidents_time) <- c("Accident_Severity","Time","Hour") 
#set the rules: day(6am-17pm),night(18pm-0am & 1am-5am)
day_night_time <- ifelse(as.integer(new_fatal_accidents_time$Hour) >= 6 & as.integer(new_fatal_accidents_time$Hour) <= 17, "day","night")
#new data frame
new_fatal_accidents_time <- cbind(new_fatal_accidents_time, day_night_time)
new_fatal_accidents_time1 <- new_fatal_accidents_time[,c(1,4)]
#table
fatal_num <- table(new_fatal_accidents_time1)
#1=day 2=night
data_mod <- data_frame(fatal_num = as.vector(fatal_num), day_night= c(1,2)) 
#plot
fatal_num_polt <- ggplot(data= new_fatal_accidents_time1 )+geom_bar(mapping= aes(x= day_night_time))
fatal_num_polt + scale_y_continuous("The number of accidents")
#Chi-square test
chisq.test(fatal_num)
#import table
write.table(fatal_num,"/Users/vivian/Desktop/研究所/Foundation of Business Analytics and Management Science/assignment/fatal_num.csv",row.names=T,sep=",")


#4)
#select the column to use
data2=accidents[,c(6,15)]
#clear data
data2[data2==-1]<-NA
data2=na.omit(data2)
#convert data
data2$Accident_Severity <- ifelse(data2$Accident_Severity == '1', 'Fatal', data2$Accident_Severity)
data2$Accident_Severity <- ifelse(data2$Accident_Severity == '2', 'Serious', data2$Accident_Severity)
data2$Accident_Severity <- ifelse(data2$Accident_Severity == '3', 'Slight', data2$Accident_Severity)
data2$Road_Type <- ifelse(data2$Road_Type == '1', 'Roundabout', data2$Road_Type)
data2$Road_Type <- ifelse(data2$Road_Type == '2', 'One way street', data2$Road_Type)
data2$Road_Type <- ifelse(data2$Road_Type == '3', 'Dual carriageway', data2$Road_Type)
data2$Road_Type <- ifelse(data2$Road_Type == '6', 'Single carriageway', data2$Road_Type)
data2$Road_Type <- ifelse(data2$Road_Type == '7', 'Slip road', data2$Road_Type)
data2$Road_Type <- ifelse(data2$Road_Type == '9', 'Unknown', data2$Road_Type)
data2$Road_Type <- ifelse(data2$Road_Type == '12', 'One way street/Slip road', data2$Road_Type)
#make a histogram
data2 <- data2 %>%
  mutate(Accident_Severity=as.factor(Accident_Severity))
plot2 <- ggplot(data=data2)+
  geom_bar(mapping=aes(x=Road_Type,  fill=Accident_Severity))
plot2 + scale_y_continuous("The number of accidents")+
  labs(title = bquote("The number of accidents per road type"))+
  theme(plot.title = element_text(hjust = 0.5))
#make a table
data2$accident_Severity<-recode(data2$Accident_Severity, "1"="Fatal","2"="Serious","3"="Slight")
data2$road_Type<-recode(data2$Road_Type, "1"="Roundabout","2"="One way street","3"="Dual carriageway","6"="Single carriageway","7"="Slip road","9"="Unknown")
data3=data2[,c(3,4)]
table(data3$road_Type,data3$accident_Severity)

#5)
#get the column
df_accident_factors<- subset(accidents, select=c(Day_of_Week, Road_Class_1st, Road_Type,Speed_limit, Junction_Detail, Junction_Control, Road_Class_2nd, Pedestrian_Crossing_Human_Control,Pedestrian_Crossing_Physical_Facilities, Light_Conditions, Weather_Conditions, Road_Surface_Conditions, Special_Conditions_at_Site, Carriageway_Hazards, Urban_or_Rural_Area))
df_accident_factors
#clean the data
df_accident_factors[df_accident_factors<0] <-NA
df_accident_factors2<-df_accident_factors

df_accident_factors_clean <-na.omit(df_accident_factors)
df_accident_factors_clean

#correlation_matrix
(accident_factors_matrix<- as.matrix(df_accident_factors_clean))

accident_correlation_matrix<-cor(df_accident_factors_clean)
round(accident_correlation_matrix,2)

corrplot(accident_correlation_matrix, method = "square" )

#each variable's grouping with total number of accidents and significant relation with number of Accidents#
df_accident_factors_clean$Accident_count<-1

df_road_type_group <- df_accident_factors_clean%>%
  group_by(Road_Type)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_road_type<- lm(No_of_Accidents_grouped ~ Road_Type, data=df_road_type_group)
summary(lm_road_type)

df_day_of_week_group<-df_accident_factors_clean%>%
  group_by(Day_of_Week)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_day_of_week<- lm(No_of_Accidents_grouped ~ Day_of_Week, data=df_day_of_week_group)
summary(lm_day_of_week)

df_road_surface_group<-df_accident_factors_clean%>%
  group_by(Road_Surface_Conditions)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_road_surface_conditions<- lm(No_of_Accidents_grouped ~ Road_Surface_Conditions, data=df_road_surface_group)
summary(lm_road_surface_conditions)

df_weather_group<-df_accident_factors_clean%>%
  group_by(Weather_Conditions)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_weather_conditions<- lm(No_of_Accidents_grouped ~ Weather_Conditions, data=df_weather_group)
summary(lm_weather_conditions)

df_urban_rural_group<-df_accident_factors_clean%>%
  group_by(Urban_or_Rural_Area)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_urban_rural<- lm(No_of_Accidents_grouped ~ Urban_or_Rural_Area, data=df_urban_rural_group)
summary(lm_urban_rural)

df_pedestrian_physical_group<-df_accident_factors_clean%>%
  group_by(Pedestrian_Crossing_Physical_Facilities)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_pedestrian_physical_group<- lm(No_of_Accidents_grouped ~ Pedestrian_Crossing_Physical_Facilities, data=df_pedestrian_physical_group)
summary(lm_pedestrian_physical_group)

df_Road_Class_1st_group<-df_accident_factors_clean%>%
  group_by(Road_Class_1st)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_road_class_1st<- lm(No_of_Accidents_grouped ~ Road_Class_1st, data=df_Road_Class_1st_group)
summary(lm_road_class_1st)

df_speed_limit_group<-df_accident_factors_clean%>%
  group_by(Speed_limit)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_speed_limit<- lm(No_of_Accidents_grouped ~ Speed_limit, data=df_speed_limit_group)
summary(lm_speed_limit)

df_junction_detail_group<-df_accident_factors_clean%>%
  group_by(Junction_Detail)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_junction_detail<- lm(No_of_Accidents_grouped ~ Junction_Detail, data=df_junction_detail_group)
summary(lm_junction_detail)

df_junction_control_group<-df_accident_factors_clean%>%
  group_by(Junction_Control)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_junction_control<- lm(No_of_Accidents_grouped ~ Junction_Control, data=df_junction_control_group)
summary(lm_junction_control)

df_road_class2_group<-df_accident_factors_clean%>%
  group_by(Road_Class_2nd)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_road_class2<- lm(No_of_Accidents_grouped ~ Road_Class_2nd, data=df_road_class2_group)
summary(lm_road_class2)

df_ped_human_group<-df_accident_factors_clean%>%
  group_by(Pedestrian_Crossing_Human_Control)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_ped_human_group<- lm(No_of_Accidents_grouped ~ Pedestrian_Crossing_Human_Control, data=df_ped_human_group)
summary(lm_ped_human_group)

df_special_condition_group<-df_accident_factors_clean%>%
  group_by(Special_Conditions_at_Site)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_special_conditions<- lm(No_of_Accidents_grouped ~ Special_Conditions_at_Site, data=df_special_condition_group)
summary(lm_special_conditions)

df_light_conditions_group<-df_accident_factors_clean%>%
  group_by(Light_Conditions)%>%
  summarise('No_of_Accidents_grouped'=sum(Accident_count))

lm_light_conditions<- lm(No_of_Accidents_grouped ~ Light_Conditions, data=df_light_conditions_group)
summary(lm_light_conditions)

#plotting_light_conditions#
df_light_conditions_group_graph<-df_light_conditions_group%>%
  mutate(Light_Conditions_Name=case_when(
    (Light_Conditions==1) ~ "Daylight",
    (Light_Conditions==4) ~ "Darkness-lights lit",
    (Light_Conditions==5) ~ "Darkness-lights unlit",
    (Light_Conditions==6) ~ "Darkness-No lighting",
    (Light_Conditions==7) ~ "Darkness- Lighting Unknown"
  ))
df_light_conditions_group_graph
install.packages("plotrix")
library(plotrix)

slices<-(df_light_conditions_group_graph$No_of_Accidents_grouped)
label=df_light_conditions_group_graph$No_of_Accidents_grouped
pie3D(slices, labels=label,explode=0.2,radius=1,
      main= "Light Condition's imapct on Number of Accidents")
legend("topright", c("Daylight", "Darkness-lights lit","Darkness-lights unlit","Darkness-No lighting","Darkness- Lighting Unknown"),cex=0.5, fill=rainbow(length(df_light_conditions_group_graph$Light_Conditions_Name)))

ggplot(data = df_light_conditions_group_graph,aes(x=Light_Conditions_Name, y=No_of_Accidents_grouped))+
  geom_bar(stat="identity",fill="blue")+
  #geom_smooth(method = "loess")
  geom_text(aes(label=No_of_Accidents_grouped),position = position_stack(vjust=1.5))

#plotting_Weather_Conditions#
df_weather_group_graph<-df_weather_group%>%
  mutate(Weather_Conditions_Name=case_when(
    (Weather_Conditions==1) ~ "Fine no high winds",
    (Weather_Conditions==2) ~ "Raining no high winds",
    (Weather_Conditions==3) ~ "Snowing no high winds",
    (Weather_Conditions==4) ~ "Fine+ high winds",
    (Weather_Conditions==5) ~ "Raining + high winds",
    (Weather_Conditions==6) ~ "Snowing+ high winds",
    (Weather_Conditions==7) ~ "Fog or mist",
    (Weather_Conditions==8) ~ "Other",
    (Weather_Conditions==9) ~ "Unknown",
  ))


ggplot(data = df_weather_group_graph,aes(Weather_Conditions_Name,No_of_Accidents_grouped))+
  geom_point(shape=19, colour="Green", size=6)+
  geom_smooth(method = "loess")+
  geom_text(aes(label=No_of_Accidents_grouped))

ggplot(df_weather_group_graph, aes(x=Weather_Conditions_Name, y=No_of_Accidents_grouped))+
  geom_bar(stat='identity', colour='Blue')+
  geom_text(aes(label=No_of_Accidents_grouped),position = position_stack(vjust=1.0))



##plotting_road_surfaces#
df_road_surface_group_graph<-df_road_surface_group%>%
  mutate(Road_Surface_Conditions_Name=case_when(
    (Road_Surface_Conditions==1) ~ "Dry",
    (Road_Surface_Conditions==2) ~ "Wet or Damp",
    (Road_Surface_Conditions==3) ~ "Snow",
    (Road_Surface_Conditions==4) ~ "Frost or Ice",
    (Road_Surface_Conditions==5) ~ "Flood over 3cm. deep",
    (Road_Surface_Conditions==6) ~ "Oil or Diesel",
    (Road_Surface_Conditions==7) ~ "Mud"
  ))

ggplot(df_road_surface_group_graph, aes(x=Road_Surface_Conditions_Name, y=No_of_Accidents_grouped))+
  geom_bar(stat='identity', colour="blue")+
  geom_text(aes(label=No_of_Accidents_grouped),position = position_stack(vjust=1.0))


#6)
#get the column
df_accident_severity<- subset(accidents, select=c(Day_of_Week, Accident_Severity,Road_Class_1st, Road_Type,Speed_limit, Junction_Detail, Junction_Control, Road_Class_2nd, Pedestrian_Crossing_Human_Control,Pedestrian_Crossing_Physical_Facilities, Light_Conditions, Weather_Conditions, Road_Surface_Conditions, Special_Conditions_at_Site, Carriageway_Hazards, Urban_or_Rural_Area))
df_accident_severity
#clean the data
df_accident_severity[df_accident_severity<0] <-NA
df_accident_severity_clean <-na.omit(df_accident_severity)
df_accident_severity_clean
#Making all categorical variables as factors to run multinomial logistic regression#
df_accident_severity_clean$Accident_Severity<- as.factor(df_accident_severity_clean$Accident_Severity)
df_accident_severity_clean
#convert variable into factor
df_accident_severity_clean$Road_Type<-as.factor(df_accident_severity_clean$Road_Type)
df_accident_severity_clean$Day_of_Week<-as.factor(df_accident_severity_clean$Day_of_Week)
df_accident_severity_clean$Road_Class_1st<-as.factor(df_accident_severity_clean$Road_Class_1st)
df_accident_severity_clean$Road_Class_2nd<-as.factor(df_accident_severity_clean$Road_Class_2nd)
df_accident_severity_clean$Junction_Control<-as.factor(df_accident_severity_clean$Junction_Control)
df_accident_severity_clean$Junction_Detail<-as.factor(df_accident_severity_clean$Junction_Detail)
df_accident_severity_clean$Pedestrian_Crossing_Human_Control<-as.factor(df_accident_severity_clean$Pedestrian_Crossing_Human_Control)
df_accident_severity_clean$Pedestrian_Crossing_Physical_Facilities<-as.factor(df_accident_severity_clean$Pedestrian_Crossing_Physical_Facilities)
df_accident_severity_clean$Light_Conditions<-as.factor(df_accident_severity_clean$Light_Conditions)
df_accident_severity_clean$Weather_Conditions<-as.factor(df_accident_severity_clean$Weather_Conditions)
df_accident_severity_clean$Special_Conditions_at_Site<-as.factor(df_accident_severity_clean$Special_Conditions_at_Site)
df_accident_severity_clean$Road_Surface_Conditions<-as.factor(df_accident_severity_clean$Road_Surface_Conditions)
df_accident_severity_clean$Carriageway_Hazards<-as.factor(df_accident_severity_clean$Carriageway_Hazards)
df_accident_severity_clean$Urban_or_Rural_Area<-as.factor(df_accident_severity_clean$Urban_or_Rural_Area)
str(df_accident_severity_clean)
#multinomial logistic regression
library(nnet)
df_accident_severity_clean$Accident_Severity<-relevel(df_accident_severity_clean$Accident_Severity, ref = '1')
acc_severity_model<- multinom(Accident_Severity~.,data=df_accident_severity_clean)
summary(acc_severity_model)
#2-tailed z-test
z_test_acc_severity<- summary(acc_severity_model)$coefficients/summary(acc_severity_model)$standard.errors
z_test_acc_severity
p_value_acc_severity<- (1-pnorm(abs(z_test_acc_severity),0,1))*2
p_value_acc_severity
#to calculate exponential values of coefficients calculated from model. Exponential values will be used to find odd ratio of each variable with levels of accident severity
exp(coefficients(acc_severity_model))















