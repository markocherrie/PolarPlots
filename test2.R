library(ggplot2)
library(geomtextpath)

createdonuts<-function(numberofbands, numberofcatsband1, numberofcatsband2){
  if(numberofcatsband1==3 | numberofcatsband2==3){
    NCB=2
  }else if(numberofcatsband1==4| numberofcatsband2==4){
    NCB=2
  }

  x1<- c(seq(0, 10/6 * pi, NCB*pi/3))
  y1<- c(rep(2, numberofcatsband1))
  x2<- c(seq(0, 10/6 * pi, NCB*pi/3) + NCB*pi/3)
  y2<- c(rep(4, numberofcatsband1))
  group<-letters[c(1)]
  alpha<-1
  
  
  df<-data.frame(x1,x2,y1,y2, group, alpha)
  
}

df<-createdonuts(1,3)


polarplotter<-function(df){
p <- ggplot(df, aes(x1, y1)) +
  geom_rect(aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = group,
                alpha = alpha),
            color = "white", size = 2) +
  geom_textpath(data = data.frame(x1 = seq(0, 2 * pi, 
                                           length = 300),
                                  y1 = rep(3, 300),
                                  label = rep(c("density", "smooth", "unique"), 
                                              each = 100)),
                aes(label = label), linetype = 0, size = 4.6, color = "white",
                upright = TRUE) +
  scale_y_continuous(limits = c(-5, 4)) +
  scale_x_continuous(limits = c(0, 2*pi)) +
  scale_fill_manual(values = c("deepskyblue3", "deepskyblue4",
                               "green3", "green4","tomato", "tomato2")) +
  scale_alpha_identity() +
  theme_void() +
  theme(legend.position = "none") 

p
p + coord_polar()
}

polarplotter(df)