library(ggplot2)
library(geomtextpath)
library(RColorBrewer)


# create the  dataframe
createdf<-function(numberofbands, ...){
  
  numberofcatsband <- unlist(list(...))
  


# make the bands
if(numberofbands==1){
  
  if(numberofcatsband[1]==2){
    NCB1=3
  }else if(numberofcatsband[1]==3){
    NCB1=2
  }else if(numberofcatsband[1]==4){
    NCB1=1.5
  } else if(numberofcatsband[1]==5){
    NCB1=1.2
  } else if(numberofcatsband[1]==6){
    NCB1=1
  }
  
  
  x1<- c(seq(0, 10/6 * pi, NCB1*pi/3))
  y1<- c(rep(1, numberofcatsband[1]))
  x2<- c(seq(0, 10/6 * pi, NCB1*pi/3) + NCB1*pi/3)
  y2<- c(rep(3, numberofcatsband[1]))

}else if(numberofbands==2){
  
  
  if(numberofcatsband[1]==2){
    NCB1=3
  }else if(numberofcatsband[1]==3){
    NCB1=2
  }else if(numberofcatsband[1]==4){
    NCB1=1.5
  } else if(numberofcatsband[1]==5){
    NCB1=1.2
  } else if(numberofcatsband[1]==6){
    NCB1=1
  }
  
  
  if(numberofcatsband[2]==2){
    NCB2=3
  }else if(numberofcatsband[2]==3){
    NCB2=2
  } else if(numberofcatsband[2]==4){
    NCB2=1.5
  } else if(numberofcatsband[2]==5){
    NCB2=1.2
  } else if(numberofcatsband[2]==6){
    NCB2=1
  }
  
  x1<- c(seq(0, 10/6 * pi, NCB1*pi/3), seq(0, 10/6 * pi, NCB2*pi/3))
  y1<- c(rep(1, numberofcatsband[1]), rep(3, numberofcatsband[2]))
  x2<- c(seq(0, 10/6 * pi, NCB1*pi/3) + NCB1*pi/3, seq(0, 10/6 * pi, NCB2*pi/3) + NCB2*pi/3)
  y2<- c(rep(3, numberofcatsband[1]), rep(5, numberofcatsband[2]))
  
}else if(numberofbands==3){
  
  
  if(numberofcatsband[1]==2){
    NCB1=3
  }else if(numberofcatsband[1]==3){
    NCB1=2
  }else if(numberofcatsband[1]==4){
    NCB1=1.5
  } else if(numberofcatsband[1]==5){
    NCB1=1.2
  } else if(numberofcatsband[1]==6){
    NCB1=1
  }
  
  
  if(numberofcatsband[2]==2){
    NCB2=3
  }else if(numberofcatsband[2]==3){
    NCB2=2
  } else if(numberofcatsband[2]==4){
    NCB2=1.5
  } else if(numberofcatsband[2]==5){
    NCB2=1.2
  } else if(numberofcatsband[2]==6){
    NCB2=1
  }
  
  
  if(numberofcatsband[3]==2){
    NCB3=3
  } else if(numberofcatsband[3]==3){
    NCB3=2
  } else if(numberofcatsband[3]==4){
    NCB3=1.5
  } else if(numberofcatsband[3]==5){
    NCB3=1.2
  } else if(numberofcatsband[3]==6){
    NCB3=1
  }
  
  x1<- c(seq(0, 10/6 * pi, NCB1*pi/3), seq(0, 10/6 * pi, NCB2*pi/3), seq(0, 10/6 * pi, NCB3*pi/3))
  y1<- c(rep(1, numberofcatsband[1]), rep(3, numberofcatsband[2]), rep(5, numberofcatsband[3]))
  x2<- c(seq(0, 10/6 * pi, NCB1*pi/3) + NCB1*pi/3, seq(0, 10/6 * pi, NCB2*pi/3) + NCB2*pi/3, seq(0, 10/6 * pi, NCB3*pi/3) + NCB3*pi/3)
  y2<- c(rep(3, numberofcatsband[1]), rep(5, numberofcatsband[2]), rep(7, numberofcatsband[3]))

}
  
  # group and alpha - for colours 
  # add labels here too
  n <- length(x1)
  group<-letters[c(1:n)]
  alpha<-0.9

  df<-data.frame(x1,x2,y1,y2, group, alpha)
  df
}

# polar plotter
polarplotter<-function(df, shape, ...){

  # get the labels
  labels <- unlist(list(...))
  #labels
  #print(labels)
  
  # make the text in the middle
  i<-300
  while(i %% nrow(df) != 0 ){
    i = i+1
  }
  
  # get the right labels for the right donut
  lbs<-rep(c(labels), each = round(i/nrow(df)))
  
  # get
  if(length(unique(df$y1))==2){
    
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
  }else if(length(unique(df$y1))==3){
    
    y1a<-rep(2, length(lbs[lbs %in% labels[1:table(df$y1)[1]]]))
    
    x1a<-seq(0,2 * pi, length = length(y1a))
    
    y1b<-rep(4, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+1):
          (table(df$y1)[1]+table(df$y1)[2])
      ]]))
    
  
    x1b<-seq(0,2 * pi, length = length(y1b))
    
    
    y1c<-rep(6, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3])
      ]]))
    
    
    x1c<-seq(0,2 * pi, length = length(y1c))
    
    
    x1<-c(x1a, x1b, x1c)
    y1<-c(y1a,y1b, y1c)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)
    
    
  }else{
    x1<-seq(0, 2 * pi, length = i)
    y1 = rep(2, i)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)
    
  }

# for semi-circle
  if(shape=="semi-circle"){
    df$x1<-df$x1/2
    df$x2<-df$x2/2 
    textdf$x1<-textdf$x1/2
    
    startval=-(pi / 2)
  } else{
    startval=0
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
  scale_x_continuous(limits = c(0, pi*2)) +
  #scale_fill_manual(values = c("deepskyblue3", "deepskyblue4",
  #                             "green3", "green4","tomato", "tomato2")) +
  scale_alpha_identity() +
  theme_void() +
  theme(legend.position = "none") 

# semi-circle shape

  plotout<- p + coord_polar(start = startval)
  return(plotout)


}


#####
df<-createdf(1, c(4))
polarplotter(df, shape="semi-circle", letters[1:4])


df<-createdf(2, c(6, 5))
polarplotter(df, shape="semi-circle", letters[1:11])


df<-createdf(3, c(3, 5, 6))
polarplotter(df, shape="circle", letters[1:14])


df<-createdf(3, c(5, 5, 5))
polarplotter(df, shape="circle", letters[1:15])


df<-createdf(3, c(6, 6, 6))
polarplotter(df, shape="circle", paste0(letters[1:18]))

# it doesn't work with non-unique names for the cats
# text is breaking at the higher number of cats

