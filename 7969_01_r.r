library(dplyr)
library(binr)
library(descr)
library(readr)
library(stringr)
library(tidyr)
library(imputeTS)
library(assertr)
library(lubridate)
library(readxl)
library(varhandle)

imdb=read_delim('movie_metadata.csv', delim=',', escape_double=FALSE, escape_backslash=TRUE)
d=read.csv('diamonds.csv',skipNul = TRUE,stringsAsFactors = FALSE)
df1=read_delim('imdb.csv', delim=',', escape_double=FALSE, escape_backslash=TRUE)

df<-read_xlsx("SaleData.xlsx")

least_sales<-function(df)
{
  ls<-df%>% group_by(Item) %>% summarise((least_sales=min(Sale_amt)))
  return(ls)
}

sales_year_region<-function(df)
{ 
  ls<-df%>% group_by(year,Region) %>% summarise((sum_sales=sum(Sale_amt)))
  return(ls)
}

days_diff<-function(df,ref.date)
{ 
  df$days_diff<-round(difftime(ref.date,df$OrderDate))
  return(df)
  
}

mgr_slsmn<-function(df)
{ a<-df %>%group_by(Manager) %>%arrange(SalesMan) %>% unique() %>%  summarise(Sale=list(unique(SalesMan))) %>%ungroup()
return (a)
}

slsmn_units<-function(df)
{  
  return(df%>%group_by(Region)%>%summarise(Salesman_count=n_distinct(SalesMan),total_sales=sum(Sale_amt))%>%ungroup())
}

sales_pct<-function(df)
{ 
  t=sum(df$Sale_amt)
  return(df%>%group_by(Manager)%>%summarise(percentage=(sum(Sale_amt)/t)*100)%>%ungroup())
}

df.2<-read.csv("imdb.csv")

fifth_movie<-function(df.2)
{
  return(df.2[5,6])
}

movies<-function(df.2)
{
  return(arrange(df.2,duration)[c(1,nrow(df.2)),'title'])
}

sort_df<-function(df.2)
{
  return(arrange(df.2,year,desc(imdbRating)))
}

subset_df<-function(df.2)
{
  return(subset(df.2,between(duration,30,180)))
}

df.3<-read.csv("Diamonds.csv")

dupl_rows<-function(df.3)
{
  return(sum(duplicated(df.3)))
}  

drop_row<-function(df.3)
{
  df.3<-drop_na(df.3,c("carat","cut"))
  return(df.3)
}

sub_numeric<-function(df.3)
{
  return(df.3[sapply(df.3,is.numeric)])
}

volume<-function(df.3)
{
  x<-na.omit(df.3)
  x<-unfactor(x)
  x$volume <- ifelse(x$depth>60,(x$x)*(x$y)*(as.integer(x$z)), 8)
  return(x)
}

impute<-function(df.3)
{
  df.3$price[is.na(df.3$price)]<-mean(df.3$price,na.rm=T)
  return (df.3)
}


#Bonus 1:
#Generate a report that tracks the various Genere combinations for each type year on year. The result
#data frame should contain type, Genere_combo, year, avg_rating, min_rating, max_rating,
#total_run_time_mins


Bonus1<-function(df){
  
  df<-na.omit(df)
  df1 <- df %>% select(16:44)
  df['genre_combo'] <- apply(df1, 1, function(x) paste(names(x[x==1]), collapse=" "))
  df1 <- df %>% group_by(year,type,genre_combo) %>% summarise(avg_rating=mean(imdbRating),min_rating=min(imdbRating),max_rating=max(imdbRating),total_run_time_mins=(sum(duration)/60))
  return(df1)
}
Bonus1(df1)

#Bonus 2:
#The results should contain year, min_length,
#max_length, num_videos_less_than25Percentile, num_videos_25_50Percentile ,
#num_videos_50_75Percentile, num_videos_greaterthan75Precentile
bonus2<-function(df)
{ 
  x<-na_mean(df)
  x$year=floor(x$year)
  x$len=nchar(x$wordsInTitle)
  x[is.na(x$wordsInTitle),"len"]=nchar(as.character(unlist(x[is.na(x$wordsInTitle),"title"])))
  x$percentile<-bin_data(x$duration,bins=4,binType = "quantile")
  d<-as.data.frame.matrix(table(x$year,x$percentile))
  colnames(d)<-c("num_videos_less_than25Percentile","num_videos_25_50Percentile ","num_videos_50_75Percentile","num_videos_greaterthan75Precentile")
  y<-x%>%group_by(year)%>%summarise(min=min(len),max=max(len))
  print(cbind(y,d))
}
bonus2(df1)

#df=read_delim('diamonds.csv', delim=',', escape_double=FALSE, escape_backslash=TRUE)

bonus3<- function(d){
  d$z=as.numeric(d$z)
  d['volume']<-apply(select(df,depth,x,y,z),1,function(r) if(r[1]>=60) r[2]*r[3]*r[4] else 8)
  qc=bin_data(df$volume,bins=3)
  d['bins']=as.numeric(qc)
  d=na.omit(d)
  ct<-crosstab(d$bins,d$cut,prop.t=TRUE)
  return(ct)
}
compute_crosstab(d)



bonus4<-function(imdb){
imdb<-na.omit(imdb)
imdb1<-imdb %>% group_by(title_year)
imdb1=imdb1[with(imdb1,order(-gross)),]
imdb1<-imdb1 %>% group_map(~ head(.x, ifelse(nrow(.x)<10,1,as.numeric(0.1*nrow(.x)))),keep=TRUE) %>% bind_rows()
top10<-imdb1 %>% group_by(title_year,genres) %>% summarise(avgimdb=mean(imdb_score),count_movies=n())
return(top10)
}
bonus4(imdb)

bonus5<-function(df)
{
  df<-na.omit(df,cols="duration")
  df$decile=as.numeric(bin_data(df$duration,bins=10,binType="quantile",boundaryType = "(lorc"))
  a<-df[,17:45]
  b<-a%>%group_by(decile)%>%summarise_all(sum)
  x<-df%>%group_by(decile)%>%summarise(nominations=sum(nrOfNominations),wins=sum(nrOfWins),count=n())
  x$top_genres=top_genre=col_concat(t(as.data.frame(apply(b,1,function(x) head(names(b)[order(-x)],3)),stringsAsFactors = FALSE)),sep="|")
  print(x)
}
bonus5(df)
