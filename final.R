# Calling librares
> library(twitteR)
> library(ggplot2)
> library(plyr)
> library(stringr)
> library(tm)
> library(RColorBrewer)
> library(ROAuth)

> library(RCurl)
> options(RCurlOptions = list(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl")))
> require(twitteR)
> requestURL <- "https://api.twitter.com/oauth/request_token"
> accessURL <- "https://api.twitter.com/oauth/access_token"
> authURL <- "https://api.twitter.com/oauth/authorize"
> consumerKey <- #<consumer key obtained from twitter api when you create your app>
  > consumerSecret <- #<consumer secret key obtained from twitter api when you create your app>
    > cred <- OAuthFactory$new(consumerKey = consumerKey, consumerSecret = consumerSecret, requestURL = requestURL, accessURL = accessURL, authURL = authURL)
    > accessToken <- #< accessToken obtained from twitter api when you create your app>
      > accessTokenSecret <- #<access token secret key obtained from twitter api when you create your app>
        
        > setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)
      
      > baltimoreU <- searchTwitter('#BaltimoreUprising', n = 2000) #Reading tweets from twitter
      > bu_df <- twListToDF(baltimoreU)
      > write.csv(bu_df, file="bu.csv", row.names=FALSE)
      
      > library(maptools)
      > install.packages("classInt")
      
      > colors <- brewer.pal(9, "YlOrRd")
      
      > baltimore.shp <- readShapePoly("nhood_2010.shp")  #Reading Shapefile
      > nbr_desc <- baltimore.shp@data$LABEL
      
      #Reading excel files into R. 
      > crime_b <- read.csv(file="Crime_by_Neighborhood.csv", header=TRUE)
      > crime_nbr <- crime_b[,c(1:4,10)] #Extracting only selected columns
      
      # Using ddply function from library 'plyr' to group by data
      > crime_nbr2 <- ddply(crime_nbr, c("CrimeYear", "Neighborhood"), summarize, count = sum(!is.na(c(CrimeYear,Neighborhood))))
      > crime_nbr1 <- ddply(crime_nbr, c("CrimeMonthYear", "Neighborhood"), summarize, count = sum(!is.na(c(CrimeMonthYear,Neighborhood)))
                            > crime_nbr <- ddply(crime_b, c("CrimeMonthYear", "Neighborhood"), summarize, count = sum(!is.na(Neighborhood))))
      > crime_victim <- read.csv(file="BPD_Part_1_Victim_Based_Crime_Data.csv", header = TRUE)
      > write.csv(nbr_desc, file="nbr_desc.csv", row.names=F)
      > nbr_desc <- unique(crime_victim[,9]) # Selecting uniques neighborhood names
      > nbr_desc <- nbr_desc[order(nbr_desc)] # Sorting names by A-Z
      > nbr_desc <- data.frame(nbr_desc)
      > nbr_desc<- nbr_desc[-1, ]
      > baltimore_crm <- ddply(crime_b, c("Neighborhood"), summarize, count = sum(!is.na(Neighborhood)))
      > baltimore_crm <- baltimore_crm[-1, ]
      
      # Renaming names of all the columns in crime_nbr2
      # subsetting crm_nbr2 by year
      # Join created tables from crm_nbr2 and joining with nbr_desc to maintain consisteny of data 
      > names(crime_nbr2) <- c("YEAR", "LABEL", "COUNT")
      > crm_2010 <- subset(crime_nbr2, YEAR == 2010 & LABEL !="", select=c("YEAR", "LABEL", "COUNT"))
      > crm_2010 <- merge(x = crm_2010, y = nbr_desc, by = "LABEL", all.y=TRUE)
      > crm_2011 <- subset(crime_nbr2, YEAR == 2011 & LABEL !="", select=c("YEAR", "LABEL", "COUNT"))
      > crm_2011 <- merge(x = crm_2011, y = nbr_desc, by = "LABEL", all.y=TRUE)
      > crm_2012 <- subset(crime_nbr2, YEAR == 2012 & LABEL !="", select=c("YEAR", "LABEL", "COUNT"))
      > crm_2012 <- merge(x = crm_2012, y = nbr_desc, by = "LABEL", all.y=TRUE)
      > crm_2013 <- subset(crime_nbr2, YEAR == 2013 & LABEL !="", select=c("YEAR", "LABEL", "COUNT"))
      > crm_2013 <- merge(x = crm_2013, y = nbr_desc, by = "LABEL", all.y=TRUE)
      > crm_2014 <- subset(crime_nbr2, YEAR == 2014 & LABEL !="", select=c("YEAR", "LABEL", "COUNT"))
      > crm_2014 <- merge(x = crm_2014, y = nbr_desc, by = "LABEL", all.y=TRUE)
      > crm_2015 <- subset(crime_nbr2, YEAR == 2015 & LABEL != "", select=c("YEAR", "LABEL", "COUNT"))
      > crm_2015 <- merge(x = crm_2015, y = nbr_desc, by = "LABEL", all.y=TRUE)
      
      # Generating multiple copies of shapefile for each year
      # Merging data of each year in to individual shape file for generating map plots
      > bshp_2010 <- baltimore.shp
      > bshp_2010@data <- data.frame(bshp_2010@data, crm_2010[match(bshp_2010@data$LABEL, crm_2010$LABEL),])
      > bshp_2011 <- baltimore.shp
      > bshp_2011@data <- data.frame(bshp_2011@data, crm_2011[match(bshp_2011@data$LABEL, crm_2011$LABEL),])
      > bshp_2012 <- baltimore.shp
      > bshp_2012@data <- data.frame(bshp_2012@data, crm_2012[match(bshp_2012@data$LABEL, crm_2012$LABEL),])
      > bshp_2013 <- baltimore.shp
      > bshp_2013@data <- data.frame(bshp_2013@data, crm_2013[match(bshp_2013@data$LABEL, crm_2013$LABEL),])
      > bshp_2014 <- baltimore.shp
      > bshp_2014@data <- data.frame(bshp_2014@data, crm_2014[match(bshp_2014@data$LABEL, crm_2014$LABEL),])
      > bshp_2015 <- baltimore.shp
      > bshp_2015@data <- data.frame(bshp_2015@data, crm_2015[match(bshp_2015@data$LABEL, crm_2015$LABEL),])
      > bshp <- baltimore.shp
      > names(baltimore_crm)  <- c("LABEL", "COUNT", "PERCENTILE", "Rank")
      > bshp@data <- data.frame(bshp@data, baltimore_crm[match(bshp@data$LABEL, baltimore_crm$LABEL),])
      
      # Writing files into csv
      > write.csv(crm_2010, file="crm_2010.csv")
      > write.csv(crm_2011, file="crm_2011.csv")
      > write.csv(crm_2012, file="crm_2012.csv")
      > write.csv(crm_2013, file="crm_2013.csv")
      > write.csv(crm_2014, file="crm_2014.csv")
      > write.csv(crm_2015, file="crm_2015.csv")
      > write.csv(baltimore_crm, file="baltimore_crm.csv")
      
      # reading th csv files into R
      > crm_2010 <- read.csv(file = "crm_2010.csv", header = TRUE)
      > crm_2011 <- read.csv(file = "crm_2011.csv", header = TRUE)
      > crm_2012 <- read.csv(file = "crm_2012.csv", header = TRUE)
      > crm_2013 <- read.csv(file = "crm_2013.csv", header = TRUE)
      > crm_2014 <- read.csv(file = "crm_2014.csv", header = TRUE)
      > crm_2015 <- read.csv(file = "crm_2015.csv", header = TRUE)
      > baltimore_crm <- read.csv(file = "baltimore_crm.csv", header = TRUE)
      
      # Creating class intervals
      > brks2010<-classIntervals(crm_2010$Rank, n=6, style="quantile")
      > brks2011<-classIntervals(crm_2011$Rank, n=6, style="quantile")
      > brks2012<-classIntervals(crm_2012$Rank, n=6, style="quantile")
      > brks2013<-classIntervals(crm_2013$Rank, n=6, style="quantile")
      > brks2014<-classIntervals(crm_2013$Rank, n=6, style="quantile")
      > brks2015<-classIntervals(crm_2015$Rank, n=6, style="quantile")
      > brks<-classIntervals(baltimore_crm$Rank, n=5, style="quantile")
      > brks2010 <- brks2010$brks
      > brks2011 <- brks2011$brks
      > brks2012 <- brks2012$brks
      > brks2013 <- brks2013$brks
      > brks2014 <- brks2014$brks
      > brks2015 <- brks2015$brks
      > brks <- brks$brks
      
      # Plotting crime data for each year
      > plot(bshp, col=colors[findInterval(bshp@data$Rank, brks,all.inside=TRUE)], axes=F)
      > title(paste ("Baltimore Crime Classification 2010-2015"))
      > legend("bottomleft", legend = unique(bshp@data$Rank[order(bshp@data$Rank)]), title = "Rank", fill = colors, cex = .90, bty = "n")
      
      > plot(bshp_2010, col=colors[findInterval(bshp_2010@data$Rank, brks2010,all.inside=TRUE)], axes=F)
      > title(paste ("Baltimore Crime Classification 2010"))
      > legend("bottomleft", legend = unique(bshp_2010@data$Rank[order(bshp_2010@data$Rank)]), title = "Rank", fill = colors, cex = .90, bty = "n")
      
      > plot(bshp_2011, col=colors[findInterval(bshp_2011@data$Rank, brks2011,all.inside=TRUE)], axes=F)
      > title(paste ("Baltimore Crime Classification 2011"))
      > legend("bottomleft", legend = unique(bshp_2011@data$Rank[order(bshp_2011@data$Rank)]), title = "Rank", fill = colors, cex = .90, bty = "n")
      
      > plot(bshp_2012, col=colors[findInterval(bshp_2012@data$Rank, brks2012,all.inside=TRUE)], axes=F)
      > title(paste ("Baltimore Crime Classification 2012"))
      > legend("bottomleft", legend = unique(bshp_2012@data$Rank[order(bshp_2012@data$Rank)]), title = "Rank", fill = colors, cex = .90, bty = "n")
      
      > plot(bshp_2013, col=colors[findInterval(bshp_2013@data$Rank, brks2013,all.inside=TRUE)], axes=F)
      > title(paste ("Baltimore Crime Classification 2013"))
      > legend("bottomleft", legend = unique(bshp_2013@data$Rank[order(bshp_2013@data$Rank)]), title = "Rank", fill = colors, cex = .90, bty = "n")
      
      > plot(bshp_2014, col=colors[findInterval(bshp_2014@data$Rank, brks2014,all.inside=TRUE)], axes=F)
      > title(paste ("Baltimore Crime Classification 2014"))
      > legend("bottomleft", legend = unique(bshp_2014@data$Rank[order(bshp_2014@data$Rank)]), title = "Rank", fill = colors, cex = .90, bty = "n")
      
      > plot(bshp_2015, col=colors[findInterval(bshp_2015@data$Rank, brks2015,all.inside=TRUE)], axes=F)
      > title(paste ("Baltimore Crime Classification 2015"))
      > legend("bottomleft", legend = unique(bshp_2015@data$Rank[order(bshp_2015@data$Rank)]), title = "Rank", fill = colors, cex = .90, bty = "n")
      
      
      # Crating score sentiment function for sentiment analysis
      >score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
      {
        require(plyr)
        require(stringr)
        
        # we got a vector of sentences. plyr will handle a list
        # or a vector as an "l" for us
        # we want a simple array ("a") of scores back, so we use 
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
      
      # load positive and negative words for sentiment analysis
      > pos <- scan('positive_words.txt', what='character', comment.char = ';')
      Read 2006 items
      > neg <- scan('negative_words.txt', what='character', comment.char = ';')
      Read 4783 items
      
      # add words to a list
      > pos.words <- c(pos, 'upgrade')
      > neg.words <- c(neg, 'wtf', 'wait', 'waiting', 'epicfall', 'mechanical')
      
      # import csv file
      > ds_bu <- read.csv(file = "bu.csv", header = TRUE)
      > ds_bu$text <- as.factor(ds_bu$text)
      
      # Score all tweets
      > n.tweets <- length(baltimoreU)
      > bu.scores <- score.sentiment(ds_bu$text, pos.words, neg.words, .progress = 'text')
      > write.csv(bu.scores, file=paste("buScores.csv", sep = ""), row.names=T)
      
      # plotting using qplot
      > hist(bu.scores$score, xlab = "Score of Tweets", col=brewer.pal(9, "Set3"))
      > qplot(bu.scores$score, xlab = "Score of Tweets")
      
      # text analysis via data mining
      > library(tm)
      > install.packages("wordcloud")
      > library(wordcloud)
      > myCorpus <- Corpus(VectorSource(bu_df$text))
      > myCorpus <- tm_map(myCorpus, stripWhitespace)
      > myCorpus <- tm_map(myCorpus, removePunctuation)
      > myCorpus <- tm_map(myCorpus, removeNumbers)
      > myCorpus <- tm_map(myCorpus, tolower)
      > myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
      > myCorpus <- tm_map(myCorpus, stemDocument)
      > myCorpus <- tm_map(myCorpus, stemDocument)
      > myCorpus <- tm_map(myCorpus, PlainTextDocument)
      > tdm <- TermDocumentMatrix(myCorpus, control = list(wordLengths = c(1, Inf)))
      > freq.terms1 <- findFreqTerms(tdm, lowfreq = 15)
      > term.freq1 <- rowSums(as.matrix(tdm))
      > term.freq1 <- subset(term.freq1, term.freq1>=15)
      > df1 <-  data.frame(term = names(term.freq1), freq = term.freq1)
      > library(ggplot2)
      > ggplot(df1, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
      
      > findAssocs(tdm, "baltimoreuprising", 0.2)
      baltimoreuprising.deray 
      0.22 
      
      > findAssocs(tdm, "protestors", 0.2)
      protestors
      lovemyopinions          0.94
      hall                    0.84
      httptcoxgwqsyvdiu       0.82
      demanding               0.69
      released                0.66
      city                    0.48
      demonstration           0.47
      httptcok.               0.47
      lnonblonde              0.47
      dviyer                  0.33
      now                     0.33
      petition                0.33
      signed                  0.33
      turn                    0.33
      vs                      0.33
      > findAssocs(tdm, "baltimore", 0.2)
      baltimore
      clear               0.40
      militarized         0.40
      police              0.40
      fine                0.38
      iamyaokhari         0.35
      stop                0.31
      just                0.30
      baltimoreupris      0.29
      frankly             0.27
      return              0.27
      sucked              0.27
      telling             0.27
      normal              0.26
      beattylaw           0.25
      black               0.25
      emails              0.22
      leaks               0.22
      opbaltimore         0.22
      passwords           0.22
      anonymous           0.21
      white               0.21
      httptc.             0.20
      
      # Downloading libraries 'graph', 'Rgraphviz', 'Rcpp', 'wordcloud', 'RColorBrewer'
      > source("http://bioconductor.org/biocLite.R")
      > biocLite("graph")
      > biocLite("Rgraphviz")
      > plot(tdm, term = freq.terms1, corThreshold = 0.12, weighting = T)
      > library(wordcloud)
      > install.packages("Rcpp")
      > library(Rcpp)
      > install.packages("wordcloud")
      > library(wordcloud)
      > install.packages("RColorBrewer")
      > library(RColorBrewer)
      
      > m <- as.matrix(tdm)
      > word.freq <- sort(rowSums(m), decreasing = T) # Sorting words
      # Generating wordcloud
      > wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F)
      > library(tm)
      > tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
      > m2 <- as.matrix(tdm2)
      > distMatrix <- dist(scale(m2))
      > fit <- hclust(distMatrix, method = "ward")
      The "ward" method has been renamed to "ward.D"; note new "ward.D2"
      > fit <- hclust(distMatrix, method = "ward.D")
      > plot(fit)
      > rect.hclust(fit, k = 6)
      > m3 <- t(m2)
      > set.seed(22)
      > k <- 6
      > install.packages("fpc")
      > library(fpc)
      > pamResult <- pamk(m3, metric="manhattan")
      > k <- pamResult$ncpam
      > pamResult <- pamResult$pamobject
      > layout(matrix(c(1,2), 1, 2))
      > plot(pamResult, col.p = pamResult$clustering)
      > install.packages("topicmodels")
      > library(topicmodels)
      > lda <- LDA(dtm, k = 8)
      > term <- terms(lda, 4)
      > term
      Topic 1             Topic 2             Topic 3             Topic 4             Topic 5            
      [1,] "rt"                "rt"                "rt"                "baltimoreuprising" "baltimoreuprising"
      [2,] "baltimoreuprising" "deray"             "baltimoreuprising" "baltimoreriots"    "rt"               
      [3,] "baltimore"         "baltimoreuprising" "aimnplease"        "rt"                "baltimore"        
      [4,] "state"             "httptcosyxujdhsj"  "image"             "blacklivesmatter"  "baltimoreupris"   
      Topic 6             Topic 7             Topic 8            
      [1,] "rt"                "baltimoreuprising" "rt"               
      [2,] "baltimoreuprising" "rt"                "baltimoreuprising"
      [3,] "deray"             "freddiegray"       "america"          
      [4,] "baltimorepolice"   "protest"           "bipartisanism" 
      > term <- apply(term, MARGIN = 2, paste, collapse = ", ")
      > term
      Topic 1 
      "rt, baltimoreuprising, baltimore, state" 
      Topic 2 
      "rt, deray, baltimoreuprising, httptcosyxujdhsj" 
      Topic 3 
      "rt, baltimoreuprising, aimnplease, image" 
      Topic 4 
      "baltimoreuprising, baltimoreriots, rt, blacklivesmatter" 
      Topic 5 
      "baltimoreuprising, rt, baltimore, baltimoreupris" 
      Topic 6 
      "rt, baltimoreuprising, deray, baltimorepolice" 
      Topic 7 
      "baltimoreuprising, rt, freddiegray, protest" 
      Topic 8 
      "rt, baltimoreuprising, america, bipartisanism" 
      > topic <- topics(lda, 1)
      > topics <- data.frame(date = as.IDate(bu_df$created), topic)
      > library(ggplot2)
      > qplot(date, ..count.., data = topics, geom="density", fill=term[topic], position="stack")
      > mapdata <- read.csv(file="Officer_Involved_Shooting_View.csv", header =TRUE)
      > View(off_shoot)
      > off_shoot1 <- ddply(off_shoot, c("CrimeYear", "Neighborhood"), summarize, count = sum(!is.na(c(CrimeYear,Neighborhood))))
      > library(plyr)
      > off_shoot1 <- ddply(off_shoot, c("Year", "X..LONG", "Y..LAT"), summarize, count = sum(!is.na(c(Year,X..LONG, Y..LAT))))
      > install.packages("rworldmap")
      > library(rworldmap)
      > newmap <- getMap(resolution = "low")
      > plot(newmap)
      > plot(newmap, xlim = range(off_shoot1$X..LONG.), ylim = range(off_shoot1$Y..LAT.), asp = 1)
      
      > library("maps")
      # Creating ggplot
      > ggplot() +  geom_point(data=off_shoot1, aes(x=X..LONG., y=Y..LAT., color = "red"))
      > ggplot() +  geom_polygon(data=baltimore.shp, aes(x=long, y=lat, group=group))
      Regions defined for each Polygons
      > ggplot()+geom_polygon(data=baltimore.shp, aes(x=long, y=lat, group=group)) +  geom_point(data=off_shoot1, aes(x=X..LONG., y=Y..LAT.), color="red")
      
      # Renaming specific fields of mapdata
      > names(mapdata)[names(mapdata)=="X..LONG."]<-"x"
      > names(mapdata)[names(mapdata)=="Y..LAT."]<-"y"
      > ggplot() +geom_polygon(data=baltimore.shp, aes(x=long, y=lat, group=group))+  geom_point(data=mapdata, aes(x=x, y=y), color="red")
      
      > library(rgdal)
      > neighborhoods <- readOGR(".", "nhood_2010")
      > neighborhoods <- spTransform(neighborhoods, CRS("+proj=longlat +datum=WGS84"))
      > library(ggplot2)
      > neighborhoods <- fortify(neighborhoods)
      
      # Using Rgoogle maps to plot offcier shooting for years 2013-2015 in baltimore
      > install.packages("RgoogleMaps")
      > library(ggmap)
      Google Maps API Terms of Service: http://developers.google.com/maps/terms.
      Please cite ggmap if you use it: see citation('ggmap') for details.
      > library(RgoogleMaps)
      > CenterOfMap <- geocode("Baltimore, MD")
      > CenterOfMap <- geocode(" 39.299768,-76.614929")
      > Baltimore <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 12, maptype = "terrain", source = "google")
      > Baltimore <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 12, maptype = "toner", source = "stamen")
      > BaltimoreMap <- ggmap(Baltimore)
      > BaltimoreMap 
      > Baltimore <- get_map(c(lon=CenterOfMap$lon, lat=CenterOfMap$lat),zoom = 12,source = "osm")
      > BaltimoreMap <- ggmap(Baltimore)
      > BaltimoreMap
      > BaltimoreMap <- BaltimoreMap + geom_polygon(aes(x=long, y=lat, group=group), fill='grey', size=.2,color='black', data=neighborhoods, alpha=0) + geom_point(data=mapdata, aes(x=x, y=y), colour="Red", fill="Red",pch=21, size=9, alpha=I(0.7))