library(corrplot)
library(Hmisc)
library(stringi)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggplot)
library(readr)
install.packages('readxl')
library(readxl)

data<- read_excel('Exam_Q1_data.xlsx')
data
data_q1<- subset(data,select=c(Mean_Ticket_Price, Total_Attendance))
data_q1

iteration<- function(){
  x <- data_q1$Mean_Ticket_Price
  is.numeric(x)
  descdist(x, discrete = TRUE)
  y <- data_q1$Total_Attendance
  is.numeric(y)
  di_x=dnorm(x,mean(x),sd(x))
  di_y=dnorm(y,mean(y),sd(y))
  normal_dist=di_x*di_y
  plot(normal_dist)
}

normal_dist

dist_output<- data.frame(t(replicate(500, iteration())))
names(dist_output)<- c('x', 'y')
a <-table(dist_output$X, dist_output$Y)/500

########Ques 1B#####################

data_q1<- subset(data,select=c(Mean_Ticket_Price,Total_Attendance))
matrix_price_attendance<- as.matrix(data_q1)
matrix_price_attendance
cor_matrix_price_attendance<-cor(matrix_price_attendance)
cor_matrix_price_attendance

ggplot(data_q1, aes(Mean_Ticket_Price, Total_Attendance, colour= 'Mean_Ticket_Price'))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)+
  labs(title='Attendance with respect to Mean Ticket Price',  x='Mean Ticket Price',y='Attendance')

var(data_q1$Mean_Ticket_Price)
var(data_q1$Total_Attendance)

sd(data_q1$Mean_Ticket_Price)
sd(data_q1$Total_Attendance)

lm_attendance_price<-lm(Total_Attendance~Mean_Ticket_Price,data=data_q1)
summary(lm_attendance_price)


###Ques 1C#######################

ggplot(data_q1, aes(Mean_Ticket_Price, Total_Attendance, colour= 'Total_Attendance'))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)+
  labs(title='Total Attendance Based on Price of Tickets',  x='Mean Ticket Price',y='Attendance')

cor (data_q1$Mean_Ticket_Price, data_q1$Total_Attendance)

model_q1<- lm(Total_Attendance~ Mean_Ticket_Price, data= data_q1) 
summary (model_q1)

df_Predictor_Price<- data.frame(Mean_Ticket_Price=c(10,15,20,41,50))

predicted_model_q1<-predict(model_q1, newdata= df_Predictor_Price)
predicted_model_q1
