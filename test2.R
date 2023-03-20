library(ggplot2)
library(geomtextpath)

# create the donut dataframe
createdonuts<-function(numberofbands, numberofcatsband1, numberofcatsband2, numberofcatsband3){
  if(numberofcatsband1==3){
    NCB1=2
  }else if(numberofcatsband1==4){
    NCB1=1.5
  } else if(numberofcatsband1==5){
    NCB1=1.2
  } else if(numberofcatsband1==6){
    NCB1=1
  }
  
  if(numberofcatsband2==3){
    NCB2=2
  } else if(numberofcatsband2==4){
    NCB2=1.5
  } else if(numberofcatsband2==5){
    NCB2=1.2
  } else if(numberofcatsband2==6){
    NCB2=1
  }
  
  if(numberofcatsband3==3){
    NCB3=2
  } else if(numberofcatsband3==4){
    NCB3=1.5
  } else if(numberofcatsband3==5){
    NCB3=1.2
  } else if(numberofcatsband3==6){
    NCB3=1
  }

if(numberofbands==1){
  x1<- c(seq(0, 10/6 * pi, NCB1*pi/3))
  y1<- c(rep(1, numberofcatsband1))
  x2<- c(seq(0, 10/6 * pi, NCB1*pi/3) + NCB1*pi/3)
  y2<- c(rep(3, numberofcatsband1))
}else if(numberofbands==2){
  x1<- c(seq(0, 10/6 * pi, NCB1*pi/3), seq(0, 10/6 * pi, NCB2*pi/3))
  y1<- c(rep(1, numberofcatsband1), rep(3, numberofcatsband2))
  x2<- c(seq(0, 10/6 * pi, NCB1*pi/3) + NCB1*pi/3, seq(0, 10/6 * pi, NCB2*pi/3) + NCB2*pi/3)
  y2<- c(rep(3, numberofcatsband1), rep(5, numberofcatsband2))
  
}else if(numberofbands==3){
  x1<- c(seq(0, 10/6 * pi, NCB1*pi/3), seq(0, 10/6 * pi, NCB2*pi/3), seq(0, 10/6 * pi, NCB3*pi/3))
  y1<- c(rep(1, numberofcatsband1), rep(3, numberofcatsband2), rep(5, numberofcatsband3))
  x2<- c(seq(0, 10/6 * pi, NCB1*pi/3) + NCB1*pi/3, seq(0, 10/6 * pi, NCB2*pi/3) + NCB2*pi/3, seq(0, 10/6 * pi, NCB3*pi/3) + NCB3*pi/3)
  y2<- c(rep(3, numberofcatsband1), rep(5, numberofcatsband2), rep(7, numberofcatsband3))

}
  
  # group and alpha - for colours 
  # add labels here too
  group<-letters[c(1)]
  alpha<-1

  df<-data.frame(x1,x2,y1,y2, group, alpha)
  df
}

df<-createdonuts(3, 6, 6, 6)
df<-createdonuts(3, 6, 4, 3)
df<-createdonuts(2, 6, 4, 3)

# polar plotter
polarplotter<-function(df, ...){

  # get the labels
  labels <- unlist(list(...))
  labels
  print(labels)
  
  # get the right labels for the right donut
  lbs<-rep(c(labels), each = round(300/nrow(df)))
  
  # to get 300 
  if(length(lbs) != 300){
    diff<- 300-length(lbs)
    if(diff<0){
      lbs<-lbs[-c(1:30)]
    }else{
      lbsadd<-rep(c(labels[length(labels)]), each = diff)
      lbs<-c(lbs, lbsadd)
    }
  }
  
  # get
  if(length(unique(df$y1))>1){
    
  y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))
  
  x1a<-seq(0,2 * pi, length = length(y1a))
  
  y1b<-rep(4, length(lbs[
                            lbs %in% labels[
                                        (table(df$y1)[1]+1):
                                           (table(df$y1)[1]+table(df$y1)[2])
                         ]]))
  
  x1b<-seq(0,2 * pi, length = length(y1b))
  x1<-c(x1a, x1b)
  y1<-c(y1a,y1b)
  textdf<-data.frame(x1 = x1,
             y1 = y1,
             label = lbs)
  
  }else{
    x1<-seq(0,2 * pi, length = length(y1b))
    y1 = rep(2, 300)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)
    
  }

  # plot
p <- ggplot(df, aes(x1, y1)) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group,
                alpha = alpha),
            color = "white", size = 2) +
  geom_textpath(data = textdf,
                aes(label = label), 
                linetype = 0, 
                size = 4.6, 
                color = "white",
                upright = TRUE) +
  scale_y_continuous(limits = c(-5, 10)) +
  scale_x_continuous(limits = c(0, 2*pi)) +
  scale_fill_manual(values = c("deepskyblue3", "deepskyblue4",
                               "green3", "green4","tomato", "tomato2")) +
  scale_alpha_identity() +
  theme_void() +
  theme(legend.position = "none") 

p
p + coord_polar()
}



# testing
text11<-c("a", "s", "e", "r", "y", "g", "p","i", "ew", "qq", "ii")
text10<-c("a", "s", "e", "r", "y", "g", "p", "i", "ew", "qq")
text9<-c("a", "s", "e", "r", "y", "g", "p", "i", "ew")

df<-createdonuts(2, 6, 4, 3)
polarplotter(df,text10)


df<-createdonuts(2, 6, 3, 3)
polarplotter(df, text9)


df<-createdonuts(2, 6, 5, 3)
polarplotter(df, text11)


df<-createdonuts(2, 3, 6, 3)
polarplotter(df,text9)



