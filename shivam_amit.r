library(twitteR)
library(ROAuth)

consumer_key <- "gjGvdPkNdzk7c0zREdCjKlxjL"
consumer_secret <- "mnsElyP0vIDz5SdslOObC3XSrOWMtPZKwkp0vzsS50gjTsr9oV"
access_token <- "1088344258036228096-AMnabPutMyPLQzQQeE6mMWNNyTDWEy"
access_secret <- "XcIuwW052avY62MQABP0NA5cM7Qg5yLBXGUpyCPrYtXZt"

download.file(url="http://curl.haxx.se/ca/cacert.pem",destfile="cacert.pem")

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

cred <- OAuthFactory$new(consumerKey=consumer_key,
                         consumerSecret=consumer_secret,
                         requestURL='https://api.twitter.com/oauth/request_token',
                         accessURL='https://api.twitter.com/oauth/access_token',
                         authURL='https://api.twitter.com/oauth/authorize')

cred$handshake(cainfo="cacert.pem")

*************************************************************************************************************************************************************************************
  
  #Cleaning tweets
#Extract tweets
NarendraModi.tweets = searchTwitter("NarendraModi",n=250)

#convert it to data frame
df <- do.call("rbind", lapply(NarendraModi.tweets,as.data.frame))

df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub=""))
df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+","",df$text)
sample <- df$text

**************************************************************************************************************************************************************************************
  
  #word_database.r
  
pos.words = scan('C:/Users/admin/Documents/sentiment/positive-words.txt', what='character', comment.char=';')
neg.words = scan('C:/Users/admin/Documents/sentiment/negative-words.txt', what='character', comment.char=';')

pos.words = c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader')
neg.words = c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not') 

  
tail(sample)

library(plyr)
library(stringr)

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]',' ',sentence)
    sentence = gsub('[[:cntrl:]]','',sentence)
    sentence = gsub('\\d+','',sentence)  #removes decimal number
    sentence = gsub('\n','',sentence)    #removes new lines
    
    sentence = tolower(sentence)
    word.list = str_split(sentence,'\\s+')
    words = unlist(word.list)  #changes a list to character vector
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp = sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1 = c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new = lapply(list, `[[`, 1)
  pp1 = lapply(list, `[[`, 2)
  nn1 = lapply(list, `[[`, 3)
  
  scores.df = data.frame(score = score_new, text=sentences)
  positive.df = data.frame(Positive = pp1, text=sentences)
  negative.df = data.frame(Negative = nn1, text=sentences)
  
  list_df = list(scores.df, positive.df, negative.df)
  return(list_df)
}

 
  # Clean the tweets and returns merged data frame
result = score.sentiment(sample, pos.words, neg.words)

library(reshape)
#Create copy of result data frame
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creating three different data frames for Score, Positive and Negative
#Removing text column from data frame
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Storing the first row(Containing the sentiment scores) in variable q
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creating data frame
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Merging three data frames into one
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)

#################################################################################
  
head(table_final)

#Positive Percentage

#Renaming
posSc=table_final$Positive
negSc=table_final$Negative

#Adding column
table_final$PosPercent = posSc/ (posSc+negSc)

#Replacing Nan with zero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage

#Adding column
table_final$NegPercent = negSc/ (posSc+negSc)

#Replacing Nan with zero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

#Histogram
#hist(table_final$Positive)
hist(table_final$Positive, col=rainbow(10))
hist(table_final$Negative, col=rainbow(10))
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")
library(plotrix)

#pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")

#Word Cloud

NarendraModi_text = sapply(NarendraModi.tweets, function(x) x$getText()) #sapply returns a vector 

#head(NarendraModi_text)
df <- do.call("rbind", lapply(NarendraModi.tweets, as.data.frame)) #lapply returns a list
#head(df)
NarendraModi_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
str(NarendraModi_text) #gives the summary/internal structure of an R object

library(tm) #tm: text mining
NarendraModi_corpus <- Corpus(VectorSource(NarendraModi_text)) #corpus is a collection of text documents
NarendraModi_corpus
inspect(NarendraModi_corpus[1])

#clean text
library(wordcloud)
NarendraModi_clean <- tm_map(NarendraModi_corpus, removePunctuation)
NarendraModi_clean <- tm_map(NarendraModi_clean, removeWords, stopwords("english"))
NarendraModi_clean <- tm_map(NarendraModi_clean, removeNumbers)
NarendraModi_clean <- tm_map(NarendraModi_clean, stripWhitespace)
wordcloud(NarendraModi_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(3.5,1))

#################################

#a_trends = availabeTrendLocations()
#a_trends

########################3

#Top Trends

#assuming input = Ottawa

a_trends = availableTrendLocations()
woeid = a_trends[which(a_trends$name=="Ottawa"),3]
canada_trend = getTrends(woeid)
trends = canada_trend[1:2]

#To clean data and remove Non English words: 
dat <- cbind(trends$name)
dat2 <- unlist(strsplit(dat, split=", "))
dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
dat4 <- dat2[-dat3]
dat4

#Top 10 Hashtags

library(twitteR)
tw = userTimeline("BarackObama", n = 3200)
tw = twListToDF(tw)
vec1 = tw$text

#Extract the hashtags:

hash.pattern = "#[[:alpha:]]+"
have.hash = grep(x = vec1, pattern = hash.pattern) #stores the indices of the tweets which have hashes

hash.matches = gregexpr(pattern = hash.pattern,
                        text = vec1[have.hash])
extracted.hash = regmatches(x = vec1[have.hash], m = hash.matches) #the actual hashtags are stored here

df = data.frame(table(tolower(unlist(extracted.hash)))) #dataframe formed with var1(hashtag), freq of hashtag
colnames(df) = c("tag","freq")
df = df[order(df$freq,decreasing = TRUE),]


dat = head(df,50)
dat2 = transform(dat,tag = reorder(tag,freq)) #reorder it so that highest freq is at the top


library(ggplot2)

p = ggplot(dat2, aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the Obama team (@BarackObama)")


##############################################################



#Server .R


# Installing package if not already installed (Stanton 2013)
EnsurePackage<-function(x)
{x <- as.character(x)
if (!require(x,character.only=TRUE))
{
  install.packages(pkgs=x,repos="http://cran.r-project.org")
  require(x,character.only=TRUE)
}
}

#Identifying packages required  (Stanton 2013)
PrepareTwitter<-function()
{
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("RJSONIO")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  #EnsurePackage("gplots") Not required... ggplot2 is used
  EnsurePackage("plyr")
  EnsurePackage("e1071")
  EnsurePackage("RTextTools")
}

PrepareTwitter()

shinyServer(function(input, output) {
  
  #Search tweets and create a data frame -Stanton (2013)
  # Clean the tweets
  TweetFrame<-function(twtList)
  {
    
    df<- do.call("rbind",lapply(twtList,as.data.frame))
    #removes emoticons
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    df$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", df$text)
    return (df$text)
  }
  
  
  # Function to create a data frame from tweets
  
  pos.words = scan('C:/Users/hp/Documents/positive-words.txt', what='character', comment.char=';')
  neg.words = scan('C:/Users/hp/Documents/negative-words.txt', what='character', comment.char=';')
  
  wordDatabase<-function()
  {
    pos.words<<-c(pos.words, 'Congrats', 'prizes', 'prize', 'thanks', 'thnx', 'Grt', 'gr8', 'plz', 'trending', 'recovering', 'brainstorm', 'leader', 'power', 'powerful', 'latest')
    neg.words<<-c(neg.words, 'Fight', 'fighting', 'wtf', 'arrest', 'no', 'not')
  }
  
  score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
  {
    require(plyr)
    require(stringr)
    list=lapply(sentences, function(sentence, pos.words, neg.words)
    {
      sentence = gsub('[[:punct:]]',' ',sentence)
      sentence = gsub('[[:cntrl:]]','',sentence)
      sentence = gsub('\\d+','',sentence)
      sentence = gsub('\n','',sentence)
      
      sentence = tolower(sentence)
      word.list = str_split(sentence, '\\s+')
      words = unlist(word.list)
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      pp=sum(pos.matches)
      nn = sum(neg.matches)
      score = sum(pos.matches) - sum(neg.matches)
      list1=c(score, pp, nn)
      return (list1)
    }, pos.words, neg.words)
    score_new=lapply(list, `[[`, 1)
    pp1=score=lapply(list, `[[`, 2)
    nn1=score=lapply(list, `[[`, 3)
    
    scores.df = data.frame(score=score_new, text=sentences)
    positive.df = data.frame(Positive=pp1, text=sentences)
    negative.df = data.frame(Negative=nn1, text=sentences)
    
    list_df=list(scores.df, positive.df, negative.df)
    return(list_df)
  }
  
  #TABLE DATA	
  
  library(reshape)
  sentimentAnalyser<-function(result)
  {
    #Creating a copy of result data frame
    test1=result[[1]]
    test2=result[[2]]
    test3=result[[3]]
    
    #Creating three different data frames for Score, Positive and Negative
    #Removing text column from data frame
    test1$text=NULL
    test2$text=NULL
    test3$text=NULL
    #Storing the first row(Containing the sentiment scores) in variable q
    q1=test1[1,]
    q2=test2[1,]
    q3=test3[1,]
    qq1=melt(q1, ,var='Score')
    qq2=melt(q2, ,var='Positive')
    qq3=melt(q3, ,var='Negative') 
    qq1['Score'] = NULL
    qq2['Positive'] = NULL
    qq3['Negative'] = NULL
    #Creating data frame
    table1 = data.frame(Text=result[[1]]$text, Score=qq1)
    table2 = data.frame(Text=result[[2]]$text, Score=qq2)
    table3 = data.frame(Text=result[[3]]$text, Score=qq3)
    
    #Merging three data frames into one
    table_final=data.frame(Text=table1$Text, Positive=table2$value, Negative=table3$value, Score=table1$value)
    return(table_final)
  }
  
  percentage<-function(table_final)
  {
    #Positive Percentage
    
    #Renaming
    posSc=table_final$Positive
    negSc=table_final$Negative
    
    #Adding column
    table_final$PosPercent = posSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    pp = table_final$PosPercent
    pp[is.nan(pp)] <- 0
    table_final$PosPercent = pp*100
    
    #Negative Percentage
    
    #Adding column
    table_final$NegPercent = negSc/ (posSc+negSc)
    
    #Replacing Nan with zero
    nn = table_final$NegPercent
    nn[is.nan(nn)] <- 0
    table_final$NegPercent = nn*100
    
    return(table_final)
  }
  
  wordDatabase()
  
  twtList<-reactive({twtList<-searchTwitter(input$searchTerm, n=input$maxTweets, lang="en") })
  tweets<-reactive({tweets<-TweetFrame(twtList() )})
  
  result<-reactive({result<-score.sentiment(tweets(), pos.words, neg.words, .progress='none')})
  
  table_final<-reactive({table_final<-sentimentAnalyser(  result() )})
  table_final_percentage<-reactive({table_final_percentage<-percentage(  table_final() )})
  
  output$tabledata<-renderTable(table_final_percentage())	
  
  #WORDCLOUD
  wordclouds<-function(text)
  {
    library(tm)
    library(wordcloud)
    corpus <- Corpus(VectorSource(text))
    #clean text
    clean_text <- tm_map(corpus, removePunctuation)
    #clean_text <- tm_map(clean_text, content_transformation)
    clean_text <- tm_map(clean_text, content_transformer(tolower))
    clean_text <- tm_map(clean_text, removeWords, stopwords("english"))
    clean_text <- tm_map(clean_text, removeNumbers)
    clean_text <- tm_map(clean_text, stripWhitespace)
    return (clean_text)
  }
  text_word<-reactive({text_word<-wordclouds( tweets() )})
  
  output$word <- renderPlot({ wordcloud(text_word(),random.order=F,max.words=80, col=rainbow(100), scale=c(4.5, 1)) })
  
  #HISTOGRAM
  output$histPos<- renderPlot({ hist(table_final()$Positive, col=rainbow(10), main="Histogram of Positive Sentiment", xlab = "Positive Score") })
  output$histNeg<- renderPlot({ hist(table_final()$Negative, col=rainbow(10), main="Histogram of Negative Sentiment", xlab = "Negative Score") })
  output$histScore<- renderPlot({ hist(table_final()$Score, col=rainbow(10), main="Histogram of Score Sentiment", xlab = "Overall Score") })	
  
  #Pie
  slices <- reactive ({ slices <- c(sum(table_final()$Positive), sum(table_final()$Negative)) })
  labels <- c("Positive", "Negative")
  library(plotrix)
  output$piechart <- renderPlot({ pie3D(slices(), labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis") })
  
  #Top trending tweets
  toptrends <- function(place)
  {
    a_trends = availableTrendLocations()
    woeid = a_trends[which(a_trends$name==place),3]
    trend = getTrends(woeid)
    trends = trend[1:2]
    
    dat <- cbind(trends$name)
    dat2 <- unlist(strsplit(dat, split=", "))
    dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
    dat4 <- dat2[-dat3]
    return (dat4)
  }
  
  trend_table<-reactive({ trend_table<-toptrends(input$trendingTable) })
  output$trendtable <- renderTable(trend_table())
  
  #TOP TWEETERS
  
  # Top tweeters for a particular hashtag (Barplot)
  toptweeters<-function(tweetlist)
  {
    tweets <- twListToDF(tweetlist)
    tweets <- unique(tweets)
    # Make a table of the number of tweets per user
    d <- as.data.frame(table(tweets$screenName)) 
    d <- d[order(d$Freq, decreasing=T), ] #descending order of tweeters according to frequency of tweets
    names(d) <- c("User","Tweets")
    return (d)
  }
  
  # Plot the table above for the top 20
  
  d<-reactive({d<-toptweeters(  twtList() ) })
  output$tweetersplot<-renderPlot ( barplot(head(d()$Tweets, 20), names=head(d()$User, 20), horiz=F, las=2, main="Top Tweeters", col=1) )
  output$tweeterstable<-renderTable(head(d(),20))
  
  #TOP 10 HASHTAGS OF USER
  
  tw1 <- reactive({ tw1 = userTimeline(input$user, n = 3200) })
  tw <- reactive({ tw = twListToDF(tw1()) })
  vec1<-reactive ({ vec1 = tw()$text })
  
  extract.hashes = function(vec){
    
    hash.pattern = "#[[:alpha:]]+"
    have.hash = grep(x = vec, pattern = hash.pattern)
    
    hash.matches = gregexpr(pattern = hash.pattern,
                            text = vec[have.hash])
    extracted.hash = regmatches(x = vec[have.hash], m = hash.matches)
    
    df = data.frame(table(tolower(unlist(extracted.hash))))
    colnames(df) = c("tag","freq")
    df = df[order(df$freq,decreasing = TRUE),]
    return(df)
  }
  
  dat<-reactive({ dat = head(extract.hashes(vec1()),50) })
  dat2<- reactive ({ dat2 = transform(dat(),tag = reorder(tag,freq)) })
  
  p<- reactive ({ p = ggplot(dat2(), aes(x = tag, y = freq)) + geom_bar(stat="identity", fill = "blue")
  p + coord_flip() + labs(title = "Hashtag frequencies in the tweets of the tweeter") })
  output$tophashtagsplot <- renderPlot ({ p() })	
}) #shiny server



########################################################################################


#Ui.R

library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Twitter Sentiment Analysis"),
  
  # Getting User Inputs
  
  sidebarPanel(
    
    textInput("searchTerm", "Enter data to be searched with '#'", "#"),
    sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=1000,value=500), 
    submitButton(text="Analyse")
    
  ),
  
  mainPanel(
    
    
    tabsetPanel(
      
      tabPanel("Top Trending Tweets Today",HTML("<div>Top Trending Tweets according to location</div>"),
               
               selectInput("trendingTable","Choose location to extract trending tweets",c("Worldwide" ,  "Abu Dhabi" ,"Acapulco" , "Accra" , 
                                                                                          "Adana" , "Adela", "Aguascalientes" , "Ahmedabad" ,         
                                                                                          "Ahsa" , "Albuquerque" , "Alexandria" , "Algeria" , "Algiers" , "Amman" , "Amritsar" , "Amsterdam",  "Ankara" , "Ansan" ,
                                                                                          "Antalya" , "Antipolo" , "Argentina" ,  "Athens" ,  
                                                                                          "Atlanta" ,             "Auckland" ,            "Austin" ,              "Australia" ,           "Austria"  ,            "Bahrain"     ,         "Baltimore"  ,         
                                                                                          "Bandung"   ,           "Bangalore" ,           "Bangkok",              "Barcelona" ,           "Barcelona",            "Barquisimeto",         "Barranquilla"  ,      
                                                                                          "Baton Rouge" ,         "Bekasi"    ,           "Belarus",              "Belem"     ,           "Belfast"  ,            "Belgium"     ,         "Belo Horizonte",      
                                                                                          "Benin City"  ,         "Bergen"    ,           "Berlin" ,              "Bhopal"    ,           "Bilbao"   ,            "Birmingham"  ,         "Birmingham"    ,      
                                                                                          "Blackpool"   ,         "Bogota"    ,           "Bologna",              "Bordeaux"  ,           "Boston"   ,            "Bournemouth" ,         "Brasilia"      ,      
                                                                                          "Brazil"      ,         "Bremen"    ,           "Brest"  ,              "Brighton"  ,           "Brisbane" ,            "Bristol"     ,         "Bucheon"       ,      
                                                                                          "Buenos Aires",         "Bursa"     ,           "Busan"  ,              "Cagayan de Oro" ,      "Cairo"    ,            "Calgary"     ,         "Cali"      ,          
                                                                                          "Calocan"     ,         "Campinas"  ,           "Can Tho",              "Canada"    ,           "Canberra"  ,           "Cape Town"   ,         "Caracas"   ,          
                                                                                          "Cardiff"     ,         "Cebu City" ,           "Changwon" ,            "Charlotte" ,           "Chelyabinsk" ,         "Chennai"     ,         "Chiba"     ,          
                                                                                          "Chicago"     ,         "Chihuahua" ,           "Chile"    ,            "Cincinnati",           "Ciudad Guayana" ,      "Ciudad Juarez",        "Cleveland" ,          
                                                                                          "Cologne"     ,         "Colombia"  ,           "Colorado Springs",     "Columbus"  ,           "Concepcion" ,          "Cordoba"      ,        "Cork"      ,          
                                                                                          "Coventry"    ,         "Culiacan"  ,           "Curitiba"    ,         "Da Nang"   ,           "Daegu"      ,          "Daejeon"      ,        "Dallas-Ft. Worth" ,   
                                                                                          "Dammam"  , "Darwin" ,"Davao City", "Delhi", "Den Haag" , "Denmark" ,"Denver" ,  "Depok" , "Derby" , "Detroit" , "Diyarbakir" , "Dnipropetrovsk" ,"Dominican Republic","Donetsk",
                                                                                          "Dortmund"  ,           "Dresden" ,             "Dubai"         ,       "Dublin"      ,         "Durban"       ,        "Dusseldorf"    ,       "Ecatepec de Morelos", 
                                                                                          "Ecuador"       ,       "Edinburgh" ,           "Edmonton"      ,       "Egypt"       ,         "El Paso"      ,        "Eskisehir"     ,       "Essen"    ,           
                                                                                          "Faisalabad"    ,       "Fortaleza"  ,          "France"        ,       "Frankfurt"   ,         "Fresno"       ,        "Fukuoka"       ,       "Galway"   ,           
                                                                                          "Gaziantep"    ,        "Gdansk"      ,         "Geneva"       ,        "Genoa"       ,         "Germany"      ,        "Ghana"         ,       "Giza"     ,           
                                                                                          "Glasgow"      ,        "Goiania"     ,         "Gomel"        ,        "Gothenburg"  ,         "Goyang"       ,        "Greece"        ,       "Greensboro" ,         
                                                                                          "Grodno"       ,        "Guadalajara" ,         "Guarulhos"    ,        "Guatemala"   ,         "Guatemala City"  ,     "Guayaquil"     ,       "Gwangju"  ,           
                                                                                          "Hai Phong"    ,        "Haifa"       ,         "Hamamatsu"    ,        "Hamburg"     ,         "Hanoi"      ,          "Harrisburg"    ,       "Hermosillo"     ,     
                                                                                          "Hiroshima"    ,        "Ho Chi Minh City" ,    "Honolulu"     ,        "Houston"     ,         "Hull"       ,          "Hulu Langat"   ,       "Hyderabad"      ,     
                                                                                          "Ibadan"       ,        "Incheon"      ,        "India"        ,        "Indianapolis",         "Indonesia" ,           "Indore"        ,       "Ipoh"           ,     
                                                                                          "Ireland"      ,        "Irkutsk"       ,       "Israel"       ,        "Istanbul"    ,         "Italy"     ,           "Izmir"         ,       "Jackson"        ,     
                                                                                          "Jacksonville" ,        "Jaipur"        ,       "Jakarta"      ,        "Japan"       ,         "Jeddah"    ,           "Jerusalem"     ,       "Johannesburg"   ,     
                                                                                          "Johor Bahru"  ,        "Jordan"        ,       "Kaduna"       ,        "Kajang"      ,         "Kano"      ,           "Kanpur"        ,       "Kansas City"    ,     
                                                                                          "Karachi"      ,        "Kawasaki"      ,       "Kayseri"      ,        "Kazan"       ,         "Kenya"     ,           "Khabarovsk"    ,       "Kharkiv"        ,     
                                                                                          "Kitakyushu"   ,        "Klang"         ,       "Kobe"         ,        "Kolkata"      ,        "Konya"     ,           "Korea"         ,       "Krakow"         ,     
                                                                                          "Krasnodar"    ,        "Krasnoyarsk"   ,       "Kuala Lumpur" ,        "Kumamoto"    ,         "Kumasi"    ,           "Kuwait"        ,       "Kyiv"           ,     
                                                                                          "Kyoto" ,               "Lagos"    ,            "Lahore"       ,        "Las Palmas",           "Las Vegas"   ,         "Latvia" ,              "Lausanne"       ,     
                                                                                          "Lebanon" ,              "Leeds"   ,             "Leicester",            "Leipzig" ,             "Leon"        ,         "Lille"  ,              "Lima" ,               
                                                                                          "Liverpool" ,           "Lodz"     ,            "London"      ,         "Long Beach" ,          "Los Angeles"  ,        "Louisville"       ,    "Lucknow"  ,           
                                                                                          "Lviv"       ,          "Lyon"          ,       "Madrid"       ,        "Makassar"    ,         "Makati"      ,         "Malaga"          ,     "Malaysia"  ,          
                                                                                          "Manaus"     ,          "Manchester"  ,         "Manila"       ,        "Maracaibo"   ,         "Maracay"   ,           "Marseille"     ,       "Maturin"   ,   
                                                                                          "Mecca"       ,         "Medan"      ,          "Medellin"      ,       "Medina"       ,        "Melbourne"  ,          "Memphis"      ,        "Mendoza"    ,         
                                                                                          "Merida"       ,        "Mersin"    ,           "Mesa"           ,      "Mexicali"      ,       "Mexico"    ,           "Mexico City" ,         "Miami"       ,        
                                                                                          "Middlesbrough" ,       "Milan"    ,            "Milwaukee"       ,     "Minneapolis"    ,      "Minsk"      ,          "Mombasa"    ,          "Monterrey"    ,       
                                                                                          "Montpellier"     ,     "Montreal" ,            "Morelia"           ,   "Moscow"           ,    "Multan"    ,           "Mumbai"     ,          "Munich" ,     
                                                                                          "Murcia"  ,             "Muscat" ,              "Nagoya"       ,    "Nagpur"            ,   "Nairobi"  ,            "Nantes"    ,           "Naples"          ,    
                                                                                          "Nashville" , "Netherlands",  "New Haven" , "New Orleans" , "New York","New Zealand" , "Newcastle", "Nigeria" , "Niigata" ,"Nizhny Novgorod" , "Norfolk", "Norway", 
                                                                                          "Nottingham"   ,        "Novosibirsk"      ,    "Odesa"         ,       "Okayama"      ,        "Okinawa"         ,     "Oklahoma City"     ,   "Omaha"    ,           
                                                                                          "Oman"          ,       "Omsk"            ,     "Orlando"        ,      "Osaka"        ,        "Oslo"              ,   "Ottawa"           ,    "Pakistan"  ,          
                                                                                          "Palembang"      ,      "Palermo"        ,      "Palma"            ,    "Panama"        ,       "Paris"            ,    "Pasig"           ,     "Patna"      ,         
                                                                                          "Pekanbaru"       ,     "Perm"          ,       "Perth"           ,     "Peru"           ,      "Petaling"        ,     "Philadelphia"   ,      "Philippines" ,        
                                                                                          "Phoenix"    ,          "Pittsburgh"   ,        "Plymouth"  ,           "Poland"          ,     "Port Elizabeth" ,      "Port Harcourt" ,       "Portland"     ,       
                                                                                          "Porto Alegre" ,        "Portsmouth"  ,         "Portugal"   ,          "Poznan"           ,    "Preston"       ,       "Pretoria"     ,        "Providence"    ,      
                                                                                          "Puebla"        ,       "Puerto Rico"    ,      "Pune"        ,         "Qatar"             ,   "Quebec"       ,        "Queretaro"           , "Quezon City"    ,     
                                                                                          "Quito"    ,            "Rajkot"       ,        "Raleigh"     ,         "Ranchi"            ,   "Rawalpindi" ,          "Recife"            ,   "Rennes"         ,     
                                                                                          "Richmond"   ,          "Riga"         ,        "Rio de Janeiro",       "Riyadh"      ,         "Rome"       ,          "Rosario"           ,   "Rostov-on-Don"    ,   
                                                                                          "Rotterdam"  ,          "Russia"     ,          "Sacramento"    ,       "Sagamihara"  ,         "Saint Petersburg",     "Saitama"         ,     "Salt Lake City"  ,    
                                                                                          "Saltillo"     ,        "Salvador"   ,          "Samara"          ,     "San Antonio"   ,       "San Diego"       ,     "San Francisco"   ,     "San Jose"  ,  
                                                                                          "San Luis Potosi",      "Santiago"  ,           "Santo Domingo"    ,  "Sao Paulo"      ,      "Sapporo"        ,      "Saudi Arabia"       , 
                                                                                          "Seattle"   ,           "Semarang"      ,       "Sendai"            ,   "Seongnam"        ,     "Seoul"         ,       "Seville"       ,       "Sharjah"   ,          
                                                                                          "Sheffield" ,           "Singapore"   ,         "Singapore"         ,   "South Africa"    ,     "Soweto"      ,         "Spain"       ,         "Srinagar"  ,          
                                                                                          "St. Louis"  ,          "Stockholm"  ,          "Stoke-on-Trent"     ,  "Strasbourg"       ,    "Stuttgart"  ,          "Surabaya"   ,          "Surat"      ,         
                                                                                          "Suwon"        ,        "Swansea"    ,          "Sweden"     ,          "Switzerland"        ,  "Sydney"     ,          "Taguig"     ,          "Takamatsu"    ,       
                                                                                          "Tallahassee"  ,        "Tampa"    ,            "Tangerang"  ,          "Tel Aviv"           ,  "Thailand" ,            "Thane"    ,            "Thessaloniki" ,       
                                                                                          "Tijuana"        ,      "Tokyo"    ,            "Toluca"       ,        "Toronto"   ,           "Toulouse"          ,   "Tucson"   ,            "Turin"          ,     
                                                                                          "Turkey"    ,           "Turmero"     ,         "Ufa"           ,       "Ukraine"    ,          "Ulsan"            ,    "United Arab Emirates", "United Kingdom"   ,   
                                                                                          "United States" ,       "Utrecht"   ,           "Valencia"      ,       "Valencia"   ,          "Valparaiso"     ,      "Vancouver"   ,         "Venezuela"      ,     
                                                                                          "Vienna"      ,         "Vietnam"   ,           "Virginia Beach"  ,     "Vladivostok"  ,        "Volgograd"      ,      "Voronezh"    ,         "Warsaw"  ,            
                                                                                          "Washington"  ,         "Winnipeg",  "Wroclaw"      ,        "Yekaterinburg",        "Yokohama"  ,  "Yongin",              
                                                                                          "Zamboanga City" ,      "Zapopan",              "Zaporozhye"       ,    "Zaragoza"       ,      "Zurich"  ), selected = "Worldwide", selectize = FALSE),
               submitButton(text="Search"),HTML("<div><h3> The table below shows the top trending 
                                                hashtags on Twitter of the location you have chosen. These are the hot topics today! </h3></div>"),
               tableOutput("trendtable"),
               HTML
               ("<div> </div>")),
      
      
      tabPanel("WordCloud",HTML("<div><h3>Most used words associated with the hashtag</h3></div>"),plotOutput("word"),
               HTML
               ("<div><h4> A word cloud is a visual representation of text data, typically used to depict keyword metadata (tags) on websites, or to visualize free form text.
                 This format is useful for quickly perceiving the most prominent terms and for locating a term alphabetically to determine its relative prominence.
                 </h4></div>")),
      
      
      tabPanel("Histogram",HTML
               ("<div><h3> Histograms graphically depict the positivity or negativity of peoples' opinion about of the hashtag
                 </h3></div>"), plotOutput("histPos"), plotOutput("histNeg"), plotOutput("histScore")
               ),
      
      
      tabPanel("Pie Chart",HTML("<div><h3>Pie Chart</h3></div>"), plotOutput("piechart"),HTML
               ("<div><h4> A pie chart is a circular statistical graphic, which is divided into slices to illustrate the sentiment of the hashtag. In a pie chart, the arc length 
                 of each slice (and consequently its central angle and area), is proportional to the quantity it represents.</h4></div>")
               
               ),
      
      tabPanel("Table",HTML( "<div><h3> Depicting sentiment in a tablular form on a scale of 5 </h3></div>"), tableOutput("tabledata"),
               HTML ("<div><h4> The table depicts the sentiment (positive, negative or neutral) of the tweets 
                     associated with the search hashtag by showing the score for each type of sentiment. </h4></div>")),
      
      
      tabPanel("Top tweeters",HTML
               ("<div><h3> Top 20 tweeters of hastag</h3></div>"),plotOutput("tweetersplot"), tableOutput("tweeterstable")),
      
      tabPanel("Top Hashtags of User",textInput("user", "Enter User Name", "@"),submitButton(text="Search"),plotOutput("tophashtagsplot"),HTML
               ("<div> <h3>Hastag frequencies in the tweets of the tweeter</h3></div>"))
               )#end of tabset panel
               )#end of main panel
  
      ))#end of shinyUI





