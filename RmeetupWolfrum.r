#
# CODE FOR R MEETUP MARCH 20 2012
#

setwd("c:blahblahblah/RMeetupTalk/")
source("functionsforMeetup.r")
windows(record=TRUE)	

require(plyr)
require(twitteR)
require(RODBC)
require(RgoogleMaps)

#
# 1st example - connecting to a database
#

odbcDataSources()

jDB	<-odbcConnect("mySQL")
#jEX	<-odbcConnect("Excel Files")

#
# simple SQL commands to get the names of the available databases, and the names of the tables
#
sqlQuery(jDB,'show databases')
sqlQuery(jDB,'use miscanthus')
sqlQuery(jDB,'show tables')

#
# grab the NIR data
#
theNIR		<-sqlFetch(jDB,"nirspectra")
thewetchem	<-sqlFetch(jDB,"wetchemistry")

#
# smooth the spectra, do a PCA, and plot the data
#
theS	<-4:dim(theNIR)[2]
theX	<-gsub("_",".",colnames(theNIR))
theX	<-as.numeric(theX[4:length(theX)])
j<-SplineSmoothDF(theNIR[1:500,theS],1,theX,the.spar=0.3)
jj<-prcomp(j,retx=TRUE,scale.=TRUE,center=TRUE)
plot(jj$x,pch=19,col="red")

#
# histograms of the compositional data
#
par(mfrow=c(2,2))
hist(thewetchem$glucan,main="glucan")
hist(thewetchem$xylan,main="xylan")
hist(thewetchem$lignin, main="lignin")
hist(thewetchem$ash,main="ash")


#
# 2nd example - using Twitter
#

#
# read in the positive and negative sentiment files from Bo Liu
# http://www.cs.uic.edu/~liub/
#
hu.liu.pos 	<-scan('positive-words.txt', what='character', comment.char=';') 
hu.liu.neg	<-scan('negative-words.txt', what='character', comment.char=';')

# the # of tweets to return per search
#
theNumber	<- 500

# search Twitter for tweets with the string, returning a 'list'
#
solarenergy.tweets	<-searchTwitter("solar energy",	theNumber,lang='en')
windenergy.tweets	<-searchTwitter("wind energy",	theNumber,lang='en')
bioenergy.tweets	<-searchTwitter("bioenergy",	theNumber,lang='en')
frack.tweets		<-searchTwitter("fracking",		theNumber,lang='en')

# extract the text & put it into an array
#
solar.text			<-laply(solarenergy.tweets,	function(t) t$getText() )
wind.text			<-laply(windenergy.tweets,	function(t) t$getText() )
bioenergy.text		<-laply(bioenergy.tweets,	function(t) t$getText()  )
frack.text			<-laply(frack.tweets,		function(t) t$getText()  )


# clean the text & list it out..
#
solarcleanTweets		<- clean(solar.text)
windcleanTweets			<- clean(wind.text)
bioenergycleanTweets	<- clean(bioenergy.text)
frackcleanTweets		<- clean(frack.text)

wind_score		<-score.sentiment(windcleanTweets,		hu.liu.pos,hu.liu.neg)
solar_score		<-score.sentiment(solarcleanTweets,		hu.liu.pos,hu.liu.neg)
bioenergy_score	<-score.sentiment(bioenergycleanTweets,	hu.liu.pos,hu.liu.neg)
frack_score		<-score.sentiment(frackcleanTweets,		hu.liu.pos,hu.liu.neg)

#
# plot the histograms
#
par(mfrow=c(2,2))
hist(wind_score$score,		main=c("wind",		mean(wind_score$score)),		col="wheat")
hist(solar_score$score,		main=c("solar",		mean(solar_score$score)),		col="wheat")
hist(bioenergy_score$score,	main=c("bioenergy",	mean(bioenergy_score$score)),	col="wheat")
hist(frack_score$score,		main=c("fracking",	mean(frack_score$score)),		col="wheat")


#
# 3rd example - using RGoogleMaps
#

theLatlong	<-c(39.676247541279,-105.0955752856)
theSize		<-c(640,640)
theZoom		<-15
theFile1	<-paste("map1",theZoom,".png")
theFile2	<-paste("map2",theZoom,".png")
theFile3	<-paste("map3",theZoom,".png")

# get a roadmap
j<-GetMap(theLatlong,zoom=theZoom,destfile=theFile1,sensor="false",size=theSize,format="png", maptype="roadmap",RETURNIMAGE = FALSE, GRAYSCALE = FALSE)
# get a satellite map
j<-GetMap(theLatlong,zoom=theZoom,destfile=theFile2,sensor="false",size=theSize,format="png", maptype="satellite",RETURNIMAGE = FALSE, GRAYSCALE = FALSE)

theZoom		<-16
theMarkers	<- paste('&markers=color:blue|',theLatlong[1],",",theLatlong[2])
theMarkers	<-gsub(" ","",theMarkers)
j<-GetMap(center=theLatlong,markers=theMarkers,zoom=theZoom,destfile=theFile3,size=theSize,format="png", maptype="roadmap",RETURNIMAGE = FALSE, GRAYSCALE = FALSE)



#
# RMeetup RTOTM code
#
tD	<-read.table("RTOTM-Legends2.txt")

par(mfrow=c(1,1))
plot(tD$x,tD$y, pch=19,col=tD$type,main="RTOTM-Legends",xlab="something",ylab="something else")

theColors	<-palette()[unique(tD$type)]
#theColors2	<-palette()[1:length(levels(tD$type))]

# two ways to define legend; both “work”, but give different results
#
theL_RIGHT	<-as.character(unique(tD$type))
theL_WRONG	<-as.character(levels(tD$type))

legend(.04,.07,legend=theL_RIGHT,pch=19,col=theColors,title="RIGHT")
legend(.14,.07,legend=theL_WRONG,pch=19, col=theColors, title="WRONG")
text(.20,.075, labels="unique() gives order of use-RIGHT")
text(.20,.072, labels="levels() gives alpha order-WRONG")

