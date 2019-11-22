
#
# misc. helper functions I need for the talk (from various files)
#
SplineSmoothDF <- function (the.df,the.deriv.order=0,the.X = as.numeric(colnames(the.df)),the.spar=0){
#
# this function takes a dataframe of spectra (row-ordered) andd
# returns the spline-smoothed data
#
# arguments:
# the.df           the dataframe containing the spectra (1 per row)
# the.X            a vector of X-data (defaults to the colnames of the dataframe
# the.deriv.order  the order of the derivative (0,1, or 2)
# the.spar         parameter passed to spline smoother, typically 0; >0.5 loses resolution
#
# returns:         dataframe with the same dimensions as the.df with colnames the.X and the
#                  same rownames as the.df
#
#
temp=NULL
for (j in 1:dim(the.df)[1]) {
	asdf.1 <- smooth.spline(x=the.X,y=the.df[j,],spar=the.spar)
	asdf.2 <- predict(asdf.1,x=the.X,deriv=the.deriv.order)
	temp<- rbind(temp,asdf.2$y)
	}
colnames(temp)<-the.X
rownames(temp)<-rownames(the.df)
SplineSmoothDF<-temp
}

#
# taken directly from Jeffrey Breen's example
# http://www.inside-r.org/howto/mining-twitter-airline-consumer-sentiment
# https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107
#
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
	require(plyr)
	require(stringr)
 
	# we got a vector of sentences. plyr will handle a list
	# or a vector as an "l" for us
	# we want a simple array of scores back, so we use 
	# "l" + "a" + "ply" = "laply":
	scores = laply(sentences, function(sentence, pos.words, neg.words) {
 
		# clean up sentences with R's regex-driven global substitute, gsub():
		sentence = gsub('[[:punct:]]', '', sentence)
		sentence = gsub('[[:cntrl:]]', '', sentence)
		sentence = gsub('\\d+', '', sentence)
		# and convert to lower case:
		sentence = tolower(sentence)
 
		# split into words. str_split is in the stringr package
		word.list = str_split(sentence, '\\s+')
		# sometimes a list() is one level of hierarchy too much
		words = unlist(word.list)
 
		# compare our words to the dictionaries of positive & negative terms
		pos.matches = match(words, pos.words)
		neg.matches = match(words, neg.words)
 
		# match() returns the position of the matched term or NA
		# we just want a TRUE/FALSE:
		pos.matches = !is.na(pos.matches)
		neg.matches = !is.na(neg.matches)
 
		# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
		score = sum(pos.matches) - sum(neg.matches)
 
		return(score)
	}, pos.words, neg.words, .progress=.progress )
 
	scores.df = data.frame(score=scores, text=sentences)
	return(scores.df)
}

#
# after code in Jeffrey Breen's example...
#
clean	<- function(sentence) {
	sentence	<- gsub('[[:punct:]]','',sentence)
	sentence	<- gsub('[[:cntrl:]]','',sentence)
	sentence	<- gsub('\\d+','',sentence)
	sentence	<- tolower(sentence)
return(sentence)
}
