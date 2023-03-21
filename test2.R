# load the libraries
library(ggplot2)
library(geomtextpath)
library(RColorBrewer)
library(car)
library(plyr)

# create the  dataframe
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
  
    # return the dataframe
    df<-data.frame(x1,x2,y1,y2)
    return(df)
  }
  
  df <-plyr::mdply(argss, makethecoords)
  df <- df[order(df$y1),]
  
}

# polar plotter
donutplot<-function(df, shape, ...){

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
  
  # NEED TO REFACTOR THIS>>>>
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
    
    
  }else if(length(unique(df$y1))==4){
    
  
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
    
    y1d<-rep(8, length(lbs[
      lbs %in% labels[
        (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
          (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
      ]]))
    x1d<-seq(0,2 * pi, length = length(y1d))
    
  
    x1<-c(x1a, x1b, x1c, x1d)
    y1<-c(y1a,y1b, y1c, y1d)
    textdf<-data.frame(x1 = x1,
                       y1 = y1,
                       label = lbs)
    
    
  
    
    }else if(length(unique(df$y1))==5){
      
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
      
      y1d<-rep(8, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
        ]]))
      x1d<-seq(0,2 * pi, length = length(y1d))
      
      
      y1e<-rep(10, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5])
        ]]))
      x1e<-seq(0,2 * pi, length = length(y1e))
      
      x1<-c(x1a, x1b, x1c, x1d, x1e)
      y1<-c(y1a,y1b, y1c, y1d, y1e)
      textdf<-data.frame(x1 = x1,
                         y1 = y1,
                         label = lbs)
      
      
    }else if(length(unique(df$y1))==6){
      
      
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
      
      y1d<-rep(8, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
        ]]))
      x1d<-seq(0,2 * pi, length = length(y1d))
      
      
      y1e<-rep(10, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5])
        ]]))
      x1e<-seq(0,2 * pi, length = length(y1e))
      
      
      y1f<-rep(12, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6])
        ]]))
      x1f<-seq(0,2 * pi, length = length(y1f))
      
      
      x1<-c(x1a, x1b, x1c, x1d, x1e, x1f)
      y1<-c(y1a,y1b, y1c, y1d, y1e, y1f)
      textdf<-data.frame(x1 = x1,
                         y1 = y1,
                         label = lbs)
      
      
      
    }else if(length(unique(df$y1))==7){
        
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
      
      y1d<-rep(8, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4])
        ]]))
      x1d<-seq(0,2 * pi, length = length(y1d))
      
      
      y1e<-rep(10, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5])
        ]]))
      x1e<-seq(0,2 * pi, length = length(y1e))
      
      
      y1f<-rep(12, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6])
        ]]))
      x1f<-seq(0,2 * pi, length = length(y1f))
      
      
      
      y1g<-rep(14, length(lbs[
        lbs %in% labels[
          (table(df$y1)[1]+table(df$y1)[2]+ table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6]+1):
            (table(df$y1)[1]+table(df$y1)[2]+table(df$y1)[3]+ table(df$y1)[4]+ table(df$y1)[5]+ table(df$y1)[6] + table(df$y1)[7])
        ]]))
      x1g<-seq(0,2 * pi, length = length(y1g))
      
      
      x1<-c(x1a, x1b, x1c, x1d, x1e, x1f, x1g)
      y1<-c(y1a,y1b, y1c, y1d, y1e, y1f, y1g)
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
  
  # aesthetics
  n <- length(df$x1)
  df$group<-letters[c(1:n)]
  df$alpha<-0.9
  

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
  scale_y_continuous(limits = c(-5, 20)) +
  scale_x_continuous(limits = c(0, pi*2)) +
  #scale_fill_manual(values = c("deepskyblue3", "deepskyblue4",
  #                             "green3", "green4","tomato", "tomato2")) +
  scale_alpha_identity() +
  theme_void() +
  theme(legend.position = "none") 

# semi-circle shape

  plotout<- p + coord_polar(start = startval)
  
  # add the text here
  
  return(plotout)


}


#####
df<-createdonuts(7, c(6,6,6,5,1,1,1))
donutplot(df, shape="semi-circle", c(1:26))



df<-createdonuts(1, c(1))
donutplot(df, shape="semi-circle", letters[1:1])


df<-createdonuts(2, c(6, 5))
donutplot(df, shape="semi-circle", letters[1:11])


df<-createdonuts(3, c(3, 5, 6))
donutplot(df, shape="circle", letters[1:14])


df<-createdonuts(3, c(6, 6, 6))
donutplot(df, shape="circle", letters[1:18])


# 
donutplot(createdonuts(3, c(1, 1, 1)), shape="semi-circle", c(" ", "  ", "   "))

# it doesn't work with non-unique names for the cats

# rainbow - 7


### wider determinants of health
# missing social and community networks
labelsWDH<-c("Individual 'lifestyle' factors",
              "Living and Working Conditions",
              "General Socioeconomic, cultural and environmental conditions")

df<-createdf(3, c(1, 1, 1))
polarplotter(df, shape="semi-circle", labelsWDH)


### DEFRA
labelsDEFRA<-c("Preg.", "Home", "Exercise", "Religion", "Job",
             "Socioeconomic deprivation", "Dense Urban Area",
             "COVID-19")

df<-createdf(3, c(5,2,1))
polarplotter(df, shape="semi-circle", labelsDEFRA)


### DEFRA
labelsTARGET<-c("Outdoor Act. - 2h/week","Job outdoors - 12h/week",
               "Living in most deprived LSOA", "Living in London",
               "Post COVID-19")

df<-createdf(3, c(2,2,1))
polarplotter(df, shape="semi-circle", labelsTARGET)



