####################################################################################
#R/ggplot Bootcamp Fall 2017
#Data Exploration using Visualization
#  files used: georgia_complete.csv.gz, mpg
#  created by M Guillet
#  March 13, 2017
#  Modified by Y Cheng
#  modified Sept 13, 2017
####################################################################################
#rm(list=ls()) #this remove or clears objects from environment 

setwd("~/../../Volumes/insight/American Red Cross/georgia sites/georgia_complete/")

#Sample Red Cross donation data
gwinnett<-read.csv("gwinnett_county_site.csv") # only do this if file isn't listed on right (global environment)

# Getting a glimpse of the data
View(gwinnett) # 
colnames(gwinnett) # see column names
summary(gwinnett) #
head(gwinnett) # first part of data
tail(gwinnett) # last part of data
str(gwinnett) # structure


#boxplot
ggplot(gwinnett, aes(age,blood_type))+geom_boxplot()
# http://stackoverflow.com/questions/8522527/position-dodge-warning-with-ggplot-boxplot
ggplot(gwinnett, aes(blood_type,age))+geom_boxplot()# make sure dimension is first (x axis) to display box plot
ggplot(gwinnett, aes(blood_type,age))+geom_boxplot()+ coord_flip()

#histogram
ggplot(gwinnett,aes(x=year))+geom_histogram()
ggplot(gwinnett,aes(x=year))+geom_histogram(binwidth = 1) #does it tell you something different? 
ggplot(gwinnett,aes(x=year))+geom_histogram(binwidth = 1)+facet_grid(gender~.) #using facets. what other factors to look at?
ggplot(gwinnett,aes(x=year))+geom_histogram(binwidth = 1)+facet_grid(race~., scales="free") #independent y axis
ggplot(gwinnett,aes(x=year))+geom_histogram(binwidth = 1)+facet_wrap(~race, scales="free",ncol=2) #more readable axes
ggplot(gwinnett,aes(x=year,fill=gender))+geom_histogram(binwidth = 1)+facet_wrap(~race, scales="free",ncol=2) #gender by color
ggplot(gwinnett,aes(x=year,fill=gender))+geom_histogram(binwidth = 1)+facet_wrap(~race, scales="free",ncol=2)+ggtitle("histogram matrix") #with title


ggplot(gwinnett, aes(year, col=gender, fill=gender, alpha=.25))+geom_histogram(binwidth = 1) #stacked bar
ggplot(gwinnett, aes(year, fill=gender, alpha=.15))+geom_histogram(position="identity",binwidth = 1)#overlay
ggplot(gwinnett, aes(year, fill=gender, alpha=.15))+geom_histogram(position="dodge",binwidth = 1)#adjacent
ggplot(gwinnett, aes(year, fill=gender))+geom_histogram(position="dodge",binwidth = 1)+scale_fill_manual(values=c("black", "pink","blue"))

#bin2d see distribution or find outliers
ggplot(gwinnett,aes(x=age,y=race))+geom_bin2d()
ggplot(gwinnett,aes(x=age,y=race))+geom_bin2d()+scale_fill_continuous(low="blue",high="red")

#line graph
by_year<-group_by(gwinnett, year,sponsor_category)

Ga_summary<-summarize(by_year,
                 Total_donations=sum(donation_ind),
                 Total_deferrals=sum(deferral_ind),
                 Total_first_donations=sum(first_donat_ind),
                 Attempts=Total_deferrals+Total_donations)

ggplot(Ga_summary, aes(year,Total_donations))+geom_line()#hacksaw line graph error.
ggplot(Ga_summary, aes(year,Total_donations,color=sponsor_category))+geom_line() ## color by sponsor cat solve the hacksaw problem above.
ggplot(Ga_summary, aes(year,Total_donations,color=sponsor_category))+geom_line()+geom_point()
ggplot(Ga_summary, aes(year,Total_donations,color=sponsor_category))+geom_step()

#scatterplot
by_donor<-group_by(gwinnett, arc_id,sponsor_name,race, gender)
Ga_summary_donor<-summarize(by_donor,Total_donations=sum(donation_ind),
                      Total_deferrals=sum(deferral_ind),
                      Total_first_donations=sum(first_donat_ind),
                      Attempts=Total_deferrals+Total_donations)

ggplot(Ga_summary_donor,aes(Total_donations,Total_deferrals))+geom_point()
ggplot(Ga_summary_donor,aes(Total_donations,Total_deferrals,shape=gender,color=race))+geom_point()
ggplot(Ga_summary_donor,aes(Total_donations,Total_deferrals,shape=gender,color=race))+geom_point()+scale_color_brewer(palette="Set1")
ggplot(Ga_summary_donor,aes(Total_donations,Total_deferrals,shape=gender,color=race))+geom_point(alpha=.5)+scale_color_brewer(palette="Set1")
ggplot(Ga_summary_donor,aes(Total_donations,Total_deferrals,color=race))+geom_point(alpha=.5,shape=1)+scale_color_brewer(palette="Set1")


by_year<-group_by(gwinnett,year,race)
Ga_summary_year<-summarize(by_year,Total_donations=sum(donation_ind),
                            Total_deferrals=sum(deferral_ind),
                            Total_first_donations=sum(first_donat_ind),
                            Attempts=Total_deferrals+Total_donations)

#barchart
ggplot(data=Ga_summary_year, aes(x=year,y=Attempts,fill=race)) +
  geom_bar(position="dodge",stat="identity")+ 
  coord_flip()+
  scale_fill_brewer(palette = "Paired") +
  scale_y_continuous(breaks = seq(0, 3000, by = 500)) +
  ylab("Ratings")  +
  theme(axis.text.y = element_text(size = 10))

