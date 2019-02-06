
######################
# CRICKET DATA CUMULATIVE
######################
# Last updated 1 December 2018
# This is good reading: https://thenewstack.io/r-package-drawing-layered-plots-with-ggplot2/
##################
# GATHER THE DATA
##################
# Clear all variables in R's memory
rm(list=ls())
#install.packages("tidyverse")
#install.packages("ggrepel")
library(tidyverse)
library(gridExtra)
#library(scales) # for pretty breaks
library(ggplot2)
library(dplyr)  #<--in tidyverse?
library(ggrepel)  # repel labels for better positioning.  use geom_text_repel() instead of geom_text()

#START HERE
#read in match data
#mymatch <- read_csv("24-Nov-2018-Black-v-Navy_gamedata.csv", col_names = TRUE,skip=0) #predict columns without first 2
#graphtitle="U11 Black (Team 1) v Navy (Team 2) \n24 November 2018 at Manning PS"
#team1name="T1(Black)"
#team2name="T2(Navy)"
# 3-Nov-18-Navy-v-Gold_gamedata
#mymatch <- read_csv("3-Nov-18-Navy-v-Gold_gamedata.csv", col_names = TRUE,skip=0)
#graphtitle="U11 Gold (Team 1) v Navy (Team 2) \n3 November 2018 at Harold Rossiter"
#team1name="T1(Gold)"
#team2name="T2(Navy)"
# Round 7 data
#3mymatch <- read_csv("1-Dec-18-Navy-v-Green_gamedata.csv", col_names = TRUE,skip=0)
#graphtitle="U11 Navy (Team 1) v Green (Team 2) \n1 December 2018 at Bentley PS"
#team1name="T1(Navy)"
#team2name="T2(Green)"
# Round 10 data
mymatch <- read_csv("2 Feb_gamedata.csv", col_names = TRUE,skip=0)
graphtitle="U11 Gold (Team 1) v Navy (Team 2) \n2 February 2019 at Harold Rossiter"
team1name="T1(Gold)"
team2name="T2(Navy)"
# first innings data
balls=mymatch$Ball[1:120]
#from round 10 need capital letters on field names
score1=mymatch$Runs[1:120]+mymatch$Wides[1:120]+mymatch$Noballs[1:120]+mymatch$Byes[1:120]+mymatch$Legbyes[1:120]
tscore1=cumsum(score1) # cumulative of score
innball1=mymatch$Ball[1:120]
wickets1=mymatch$Wickets[1:120]+mymatch$Runouts[1:120]
# from round 10 this is striker not strikername
strikers1=mymatch$Striker[1:120]
batlist<-c() #create a vector
#make a vector with the indexes that match these conditions
for (x in 1:120) {
  if (mymatch$Runouts[x]==1 && mymatch$whoRO[x]==2) {
    batlist<-c(batlist,x)  #append x to vector
  }
}
batlist
#replace those vector values only
strikers1[batlist]<-mymatch$Nonstriker[batlist]
# ave of first team
rise=tscore1[120]
ave=rise/120
cumave=balls*ave

#make dataframe (tibble)
scoredata1<-tibble(balls,score1,innball1,tscore1,strikers1,wickets1,cumave)

# isolate wickets as a subset of data frame (preserves x axis refs for later)
mywickets<-subset(scoredata1,wickets1=='1') # on dataframes subset works on rows.  Choose only entries with wickets taken
mywickets

# second innings data
balls=mymatch$Ball[1:120]
score2=mymatch$Runs[121:240]+mymatch$Wides[121:240]+mymatch$Noballs[121:240]+mymatch$Byes[121:240]+mymatch$Legbyes[121:240]
tscore2=cumsum(score2) # cumulative of score
innball2=mymatch$Ball[121:240]
wickets2=mymatch$Wickets[121:240]+mymatch$Runouts[121:240]
strikers2=mymatch$Striker[121:240]

strikers2
matchlist=c()
#make a vector with the indexes that match these conditions
for (u in 121:240) {
  if (mymatch$Runouts[u]==1 && mymatch$whoRO[u]==2) {
    matchlist<=c(matchlist,u)
  }
}
#replace only matchlist vectors
strikers2[matchlist]<-mymatch$Nonstriker[matchlist] 
scoredata2<-tibble(balls,score2,innball2,tscore2,strikers2,wickets2,cumave)

# isolate wickets as a subset of data frame (preserves x axis refs for later)
mywickets2<-subset(scoredata2,wickets2=='1') # on dataframes subset works on rows.  Choose only entries with wickets taken
mywickets2


# specify the ggplot
# specify the data at the geom--- level and leave default as NULL in the ggplot if you want more than 1 layer
# each layer added to plot is denoted by a + geom ...()
# although labels could be put inito the geom_text layer, if using geom_text_repel put then into ggplot if general
g<-ggplot(NULL) + ggtitle(graphtitle)+
scale_y_continuous((name="Cumulative score"),limits=c(0,120),breaks=seq(0,120,by=10))+
scale_x_continuous((name="Overs"), limits=c(0,120),breaks=seq(0,120,by=6),labels=seq(0,20,by=1))+
#ave
geom_line(data=scoredata1,aes(x=balls, y=cumave, colour="ave"))

#first innings lines
g1<-g +
#move colour inside aes to build a legend automatically, use 'scale' to adjust labels for the legeng
geom_line(data=scoredata1,aes(x=balls, y=tscore1, colour="T1 (Navy)",label=team1name))+
# the manual scale is needed to change the colour of the lines themselves: 
# this will apply vector of colours, used in order of the lines being applied as layers
scale_color_manual(name = "Teams", labels = c("ave",team1name, team2name),values=c("red", "gold","blue"))
#scale_color_manual(name = "Teams", labels = c("ave",team1name, team2name),values=c("red", "blue","dark green"))
#scale_color_discrete(name = "Teams", labels = c("ave",team1name, team2name),values=c("red", "blue","dark green"))
 
#first innings wicket marks/labels - including colour of dots used to mark them
g2<-g1+
geom_point(data=mywickets,x=mywickets$balls,y=mywickets$tscore1, colour="orange")+
# instead of actual y value, just displace by distance from the ave line not from the tscore
#geom_text(data=mywickets,x=mywickets$balls,y=mywickets$tscore1+(mywickets$tscore1-mywickets$cumave),label=paste(mywickets$strikers1,"(fow",mywickets$tscore1,")"), size=2.7, colour="dark green")+

#colour of text for the text labels here
geom_text()+
geom_text_repel(data=mywickets,x=mywickets$balls,y=mywickets$tscore1, colour="black",label=paste(mywickets$strikers1,"(fow",mywickets$tscore1,")"),size=2.7, colour="blue", direction="y",force=4,box.padding='0.5')+
# add a line for 10 overs (vertical and dashed)
geom_vline(xintercept=c(60), linetype="dotted") # vector has ball number

# add some lines for second innings
g3<-g2+
  geom_line(data=scoredata2,aes(x=balls, y=tscore2, colour=team2name))

#second innings wicket marks/labels
g4<-g3+
geom_point(data=mywickets2,x=mywickets2$balls,y=mywickets2$tscore2, colour="blue")+
#geom_text(data=mywickets2,x=mywickets2$balls,y=mywickets2$tscore2-(mywickets2$tscore2-mywickets2$cumave),label=paste(mywickets2$strikers2,"(fow",mywickets2$tscore2,")"), size=2.7)+
geom_text()+
#allow movement in y direction only (default is both x and y)  
# repel: for animation add : force = 3, max.iter = n
geom_text_repel(data=mywickets2,x=mywickets2$balls,y=mywickets2$tscore2,colour="blue",label=paste(mywickets2$strikers2,"(fow",mywickets2$tscore2,")"), size=2.7, colour = "dark green",direction="both",force=2,box.padding = '0.5')
#output the cumulative layers for this plot
g4

  

#TO DO: cumulative balls faced

#summarise
# https://dabblingwithdata.wordpress.com/2018/01/02/my-favourite-r-package-for-summarising-data/
