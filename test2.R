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
df<-createdonuts(2, 4, 6, 3)

# polar plotter
polarplotter<-function(df, ...){

  # get the labels
  labels <- unlist(list(...))
  labels
  print(labels)
  
  # get the right labels for the right donut
  
  
  # plot
p <- ggplot(df, aes(x1, y1)) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group,
                alpha = alpha),
            color = "white", size = 2) +
  #geom_textpath(data = data.frame(x1 = seq(0, 2 * pi, 
   #                                        length = 300),
    #                              y1 = rep(1.5, 300),
     #                             label = rep(c(labels), 
      #                                        each = (300/nrow(df)))),
       #         aes(label = label), linetype = 0, size = 4.6, color = "white",
        #        upright = TRUE) +
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

polarplotter(df, c("d", "s", "e", "r"))