#load the packages
library(baseballr)
library(ggplot2)
library(tidyverse)

#create the functions to scrape the data
speed_arsenal=function(x){
  scrape_savant_leaderboards(leaderboard = "pitch_arsenal",
                             year = x,
                             min_pitches = 100,
                             arsenal_type = "avg_speed")
}
general_arsenal=function(x){
  scrape_savant_leaderboards(leaderboard = "pitch_arsenal",
                             year = x,
                             min_pitches = 100)
}

#create each D.F.
#2017
speed_2017=speed_arsenal(2017)
general_2017=general_arsenal(2017)
#2018
speed_2018=speed_arsenal(2018)
general_2018=general_arsenal(2018)
#2019
speed_2019=speed_arsenal(2019)
general_2019=general_arsenal(2019)
#2020
speed_2020=speed_arsenal(2020)
general_2020=general_arsenal(2020)


#join the datasets 
full_2017=inner_join(speed_2017,general_2017,by="pitcher")
full_2018=inner_join(speed_2018,general_2018,by="pitcher")
full_2019=inner_join(speed_2019,general_2019,by="pitcher")
full_2020=inner_join(speed_2020,general_2020,by="pitcher")


#now combine them into one massive one by season
complete_data=do.call("rbind",list(full_2017,full_2018,full_2019,full_2020))

#now remove the duplicate year, last name, first name
complete_data=complete_data %>% select(-c(year.y,last_name.y,first_name.y))
complete_data=complete_data %>% rename(year=year.x) %>% rename(last_name=last_name.x) %>% rename(first_name=first_name.x)

#check missing values - make zeros = NA
complete_data[complete_data==0.0]=NA
#the rationale here is that as it stands, if a pitcher doesn't throw a pitch...
#a pitcher has NA velocity readings (that's good)
#but 0 usage readings (that's also good but not consistent)
#so we are finding all the 0s in the dataset and making them NAs (so they won't be averaged)

#remove knuckeballers :(
complete_data=complete_data %>% select(-c(kn_avg_speed,n_kn))

#check summary
summary(complete_data)
str(complete_data)
colnames(complete_data)
#make sure the data matches (important to use na.rm)
mean(complete_data$ff_avg_speed, na.rm = TRUE)
#it does because summary automatically ignores NAs

#establish the data as the high-level plot
p1=ggplot(data=complete_data)

#look at changeup distribution extremely high-level
p1+geom_histogram(aes(x=n_ch), fill = "blue")+xlab("Changeup Usage")+ylab("# of Players")+ggtitle("Changeup Usage Within Arsenal")
#note - NAs removed (not as insightful)
# you can see that in the warning message this displays


#let's try that with 0s

#now trying in one go
#don't want to remove the NAs from the master dataset because that is helpful for velo
complete_data %>% mutate(n_ch=replace_na(n_ch,0)) %>%
 ggplot(aes(x=n_ch))+geom_histogram(fill = "blue")+xlab("Changeup Usage")+ylab("# of Players")+ggtitle("Changeup Usage as a Percentage of Pitches Thrown")

#altering the bin width
complete_data %>% mutate(n_ch=replace_na(n_ch,0)) %>%
  ggplot(aes(x=n_ch))+
  geom_histogram(fill = "blue", bins = 10)+xlab("Changeup Usage")+
  ylab("# of Players")+
  ggtitle("Changeup Usage as a Percentage of Pitches Thrown")

#check who is above 50% usage
x=sample_na_replace$n_ch[sample_na_replace$n_ch>=50]
#19 guys- can see that in the values as well as below:
table(x)
as.data.frame(x)
#btw - this is saying, "let x equal the values of sample_na_replace n_ch where sample_na_replace n_ch >=50"
y=complete_data %>% mutate(n_ch=replace_na(n_ch,0)) %>%
filter(n_ch>=50) %>% select(n_ch)

#easily select the player data relevant to those over 50% changeup usage:
positions=c(1:3,12:18)
over_50=complete_data %>% filter(n_ch>=50) %>% select(positions)
table1=cbind(over_50$year,over_50$first_name,over_50$last_name,over_50$n_ch)
print(table1)
#but also - we kinda just want everything
alternate_over_50=complete_data %>% filter(n_ch>=50)

#plot
p2=ggplot(data=over_50)
p3=ggplot(data=alternate_over_50)
#add label string
p4=ggplot(data=(alternate_over_50 %>% mutate(name.string=paste(last_name,year,sep=" "))))

#note where things are put into AES and where they are just put into geom_label
#plus, removed geom_point
p4+geom_label((aes(x=n_ch,y=ch_avg_speed,label=last_name,vjust=1,fill=year)),label.size = .25)+
  xlab("Changeup Usage")+
  ylab("Average Speed of Changeup")+
  ggtitle("Usage to Velocity for Heavy Changeup Users")

#other working ideas:
p4+geom_label((aes(x=ch_avg_speed,y=n_ch,label=last_name,vjust=1,fill=year)),label.size = .25)+
  xlab("Average Speed of Changeup")+
  ylab("Changeup Usage")+
  ggtitle("Velocity to Usage for Heavy Changeup Users")

#or
p4+geom_text((aes(x=ch_avg_speed,y=n_ch,label=last_name,vjust=1)))+
  xlab("Average Speed of Changeup")+
  ylab("Changeup Usage")+
  ggtitle("Velocity to Usage for Heavy Changeup Users")

#or
p4+geom_label((aes(x=n_ch,y=ch_avg_speed,label=name.string,vjust=1)),label.size = .25)+
  xlab("Changeup Usage")+
  ylab("Average Speed of Changeup")+
  ggtitle("Usage to Velocity for Heavy Changeup Users")








