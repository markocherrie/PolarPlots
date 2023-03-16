#install.packages('geomtextpath')

browsers<-structure(list(browser = structure(c(3L, 3L, 3L, 3L, 2L, 2L, 
                                               2L, 1L, 5L, 5L, 4L), .Label = c("Chrome", "Firefox", "MSIE", 
                                                                               "Opera", "Safari"), class = "factor"), version = structure(c(5L, 
                                                                                                                                            6L, 7L, 8L, 2L, 3L, 4L, 1L, 10L, 11L, 9L), .Label = c("Chrome 10.0", 
                                                                                                                                                                                                  "Firefox 3.5", "Firefox 3.6", "Firefox 4.0", "MSIE 6.0", "MSIE 7.0", 
                                                                                                                                                                                                  "MSIE 8.0", "MSIE 9.0", "Opera 11.x", "Safari 4.0", "Safari 5.0"
                                                                                                                                            ), class = "factor"), share = c(10.85, 7.35, 33.06, 2.81, 1.58, 
                                                                                                                                                                            13.12, 5.43, 9.91, 1.42, 4.55, 1.65), ymax = c(10.85, 18.2, 51.26, 
                                                                                                                                                                                                                           54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 90.08, 91.73), ymin = c(0, 
                                                                                                                                                                                                                                                                                            10.85, 18.2, 51.26, 54.07, 55.65, 68.77, 74.2, 84.11, 85.53, 
                                                                                                                                                                                                                                                                                            90.08)), .Names = c("browser", "version", "share", "ymax", "ymin"
                                                                                                                                                                                                                                                                                            ), row.names = c(NA, -11L), class = "data.frame")

library(ggplot2)                                                                                                                                                                                                                                                                                                                
ggplot(browsers) + 
  geom_rect(aes(fill=version, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(aes(fill=browser, ymax=ymax, ymin=ymin, xmax=3, xmin=0)) +
  xlim(c(0, 4)) + 
  theme(aspect.ratio=1) 


ggplot(browsers) + 
  geom_rect(aes(fill=version, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(aes(fill=browser, ymax=ymax, ymin=ymin, xmax=3, xmin=2)) +
  geom_rect(aes(fill=version, ymax=ymax, ymin=ymin, xmax=2, xmin=1)) +
  geom_text(aes(x=(4+3)/2, y=(ymax+ymin)/2, label="test"), size=4) +
  geom_text(aes(x=(3+2)/2, y=(ymax+ymin)/2, label="test"), size=4) +
  geom_text(aes(x=(2+1)/2, y=(ymax+ymin)/2, label="test"), size=4) +
  xlim(c(0, 4)) + 
  theme(aspect.ratio=1) +
  coord_polar(theta="y") 