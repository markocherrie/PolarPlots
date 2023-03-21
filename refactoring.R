createdonuts<-function(numberofbands, ...){
  
  numberofcatsband <- unlist(list(...))

xx1 <- car::recode(numberofcatsband, '1=6; 2=3; 3=2; 4=1.5; 5=1.2; 6=1')
xx2 <- numberofcatsband

yy1 <- seq(1, 200, by = 2)[1:numberofbands]
yy2 <- seq(3, 200, by = 2)[1:numberofbands]



argss<-data.frame(xx1=xx1, xx2=xx2, yy1=yy1, yy2=yy2)

makethecoords<-function(xx1, xx2, yy1, yy2){
  x1<- c(seq(0, 10/6 * pi, xx1*pi/3))
  y1<- c(rep(yy1, xx2))
  x2<- c(seq(0, 10/6 * pi, xx1*pi/3) + xx1*pi/3)
  y2<- c(rep(yy2, xx2))
  
  n <- length(x1)
  group<-letters[c(1:n)]
  alpha<-0.9
  
  df<-data.frame(x1,x2,y1,y2, group, alpha)
  return(df)
}

df <-plyr::mdply(argss, makethecoords)
df <- df[order(df$y1),]

}





d<-df[!duplicated(df$y1),][,3:4]

d<-df %>%
  summarise(x=count(y1))


labels<-c("a","b" ,"c", "d")

makethelabels<-function(y1, y2){

  
  y1a<-rep(2, length(lbs[lbs %in% labels[1:d$x.freq]]))
  
  x1<- seq(0, 2 * pi, length = i)
  y1 = rep(2, i)
  
  
  
  
  textdf<-data.frame(x1 = x1,
                     y1 = y1,
                     label = lbs)
  
}