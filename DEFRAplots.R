# DEFRA 


refs<-readxl::read_excel("data/tbl_REFS.xlsx")
abstn<-readxl::read_excel("data/tbl_abstn.xlsx")
exappr<-readxl::read_excel("data/tbl_exappr.xlsx")

refsasbtn<-merge(refs, abstn, by="rec_no")
refsasbtnexappr<-merge(refsasbtn, exappr, by="rec_no")
write.csv(refsasbtnexappr, "data/refsasbtnexappr.csv", row.names = F)



d<-unlist(refsasbtnexappr$q5.x)
d<-stringr::str_split(d, ";")

library(purrr)

cfull<-NULL
for(i in 1:max(lengths(d))){
c1<-map(d, i)
cfull<-cbind(cfull,c1)
}

cleanup<-function(x){
  stringr::str_trim(x, side = c("both"))
  }

d<-as.data.frame(apply(cfull, 2, cleanup))


# consolidate the groups
d[d=="athletes"]<-"Physical activity"
d[d=="outdoor"| d=="outdoor/indoor" | d=="Outdoor"]<-"General Outdoors"
d[d=="school children"| d=="schoolchildren" | d=="school" | d=="children"| d=="Children at school"| d=="school(children)"]<-"Children at school"
d[d=="sociodemographic disparities"| d=="sociodemographic" | d=="socioeconomic status"]<-"Socioeconomically deprived"
d[d=="workers"]<-"Outdoor Workers"


# Children, Work-related, Home-related location, Commuting

table(d$c1)




### wider determinants of health
# missing social and community networks
labelsWDH<-c("Time-Activity behaviour",
             "Living and Working Conditions",
             "Location - General socioeconomic, cultural and environmental factors")

df<-createdonuts(1, 1, 1)
donutplot(df, shape="semi-circle", colourby="band", colourpal="",labels=labelsWDH,
          innercircletext = "Individual Susceptibility Factors")


# EVIDENCE FOR: pregnant, children, physical activity, outdoor workers

# LITTLE EVIDENCE FOR: Gender, ethnicity

# NO EVIDENCE FOR: migrants, travellers, homeless

### DEFRA - pregnant 
labelsDEFRA<-c("d;lm;l",
               "Socieconomic Status",
               "COVID-19")

df<-createdonuts(1,1,1)
donutplot(df, shape="semi-circle", colourby="band", colourpal="", labels=labelsDEFRA, 
          innercircletext = c("Pregnant"))








### DEFRA - targeted 1
labelsDEFRA<-c("Preg.", "Home", "Exercise", "Religion", "Job",
               "Socioeconomic deprivation", "Dense Urban Area")

df<-createdonuts(5,2)
donutplot(df, shape="semi-circle", colourby="band", colourpal="", labels=labelsDEFRA, 
          innercircletext = c("Age Sex Ethinicity"))



labelsTARGET<-c("Outdoor Act. - 2h/week","Job outdoors - 12h/week",
                "Living in most deprived LSOA", "Living in London",
                "Post COVID-19")

df<-createdonuts(2,2,1)
donutplot(df, shape="semi-circle", 
          colourby="band", 
          colourpal="",
          labels=labelsTARGET)