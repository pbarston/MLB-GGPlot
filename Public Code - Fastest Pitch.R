#load the packages
library(baseballr)
library(ggplot2)
library(tidyverse)
library(lazyeval)

#create the functions
#scrape the data
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


#join the datasets - to work on getting a join function
#individual seasons
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

#remove knuckeballers :(
complete_data=complete_data %>% select(-c(kn_avg_speed,n_kn))

#check summary
summary(complete_data)
str(complete_data)
colnames(complete_data)
#make sure the data matches (important to use na.rm)
mean(complete_data$ff_avg_speed, na.rm = TRUE)

#to start - find fastest and slowest pitch
complete_data=complete_data %>% 
  mutate(fast_pitch=do.call(pmax,c(complete_data[5:11],list(na.rm=TRUE)))) %>%
  mutate(slow_pitch=do.call(pmin,c(complete_data[5:11],list(na.rm=TRUE))))
#The second argument of do.call needs to be a list of arguments to function. 
  #df is already list so we concatenate it with na.rm=TRUE argument (converted to list).

#distro of fastest pitch by year
mu=complete_data %>%
  group_by(year) %>%
  summarise(grp.mean=mean(fast_pitch))

ggplot(complete_data,aes(x=fast_pitch))+geom_density(fill="blue")+facet_wrap(~year,ncol = 1)+
  labs(title="Average Speed of a Player's Fastest Pitch",
       x="Speed (MPH)",
       y="Density") +
  geom_vline(data=mu,aes(xintercept=grp.mean),colour="red",linetype="dashed", size=1)

#how many guys in each bucket?
#create the function
combined_fastpitch=function(x) {
  if(is.na(x)){return(NA)
  }else if (x>=100) {return("100mph+")
  }else if (x>=98) {return("98mph+")
  }else if (x>=95) {return("95mph+")
  }else if (x>=93) {return("93mph+")
  }else if (x>=90) {return("90mph+")  
  }else {return("<90mph")}
}
#add it to the data
complete_data$bucketfast=sapply(complete_data$fast_pitch,combined_fastpitch)
#make the table by year
countofbuckets=complete_data %>% group_by(year,bucketfast) %>% count()
#make the name of the column slightly more intuitive
names(countofbuckets)[3]="colofbucket"
#get the table with rel.freq
countofbuckets=countofbuckets %>%
  group_by(year) %>%
  mutate(incidence=round(100*colofbucket/sum(colofbucket),1))

#creating a manual reorder of variables
#create the function
reordervariables=function(x) {
  if(is.na(x)){return(NA)
  }else if (x=="100mph+") {return("5")
  }else if (x=="98mph+") {return("4")
  }else if (x=="95mph+") {return("3")
  }else if (x=="93mph+") {return("2")
  }else if (x=="90mph+") {return("1")  
  }else {return("0")}
}
#add it to the data
countofbuckets$arrangement=sapply(countofbuckets$bucketfast,reordervariables)

#make the chart
library(forcats)
countofbuckets %>%
  mutate(name=fct_relevel(bucketfast,"<90mph",
                          "90mph+",
                          "93mph+",
                          "95mph+",
                          "98mph+",
                          "100mph+")) %>%
  ggplot(aes(x=name,y=incidence)) +geom_line(aes(group=year,color=factor(year)),size=1)+
  labs(title="Percent of Pitchers to Throw X MPH",
       subtitle = "By Year",
       color="Season",
       x="Min MPH Bucket",
       y="Percent",
       caption = "Buckets are non-inclusive")
