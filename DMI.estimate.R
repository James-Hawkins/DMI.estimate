

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library('readxl')
library('ggplot2')
library('dplyr')
library('ggpubr')
library('stringi')
library('stringr')
library('lme4')
library('lmerTest')
library('MuMIn')
library('sjPlot')
library('sjmisc')
library('sjlabelled')
library(sjstats)



save.image('dmi.estimate.RData')



d <<- read_excel('Full_Feed_intake_data.xlsx'
                 , sheet = 'Full_Feed_Intake_data' 
                 , col_types = "text")


d <<- read_excel('feed_intake_data.xlsx'
                 , sheet = 'feed_intake_raw' 
                 , col_types = "text")

colnames(d)


# Data prep
{
names(d)[2] <- 'diet.code'



convert.numeric <- c(
   'feed_intake_value'
  , 'bw_kg'
  , 'adg_g_day'
  , 'NDF_nutrition'
  ,  'ADF_nutrition'
  , 'EE_nutrition'
  ,'Ash_nutrition'
  ,  'ME_nutrition'
  , 'DM_digest'
  , 'CP_nutrition'
  , 'milk_kg_day'
  , 'T.Animals'
  
)
d <- as.data.frame(d)


for (v in convert.numeric){
  
  d[,v] <- as.numeric(d[,v] )
  
  
}



na.value <- 'NA'

unq.spcs <- unique(d$Species)

species.cattle <- c(   unq.spcs[3], unq.spcs[9]  )
species.shoat <- c(unq.spcs[1] ,unq.spcs[2]  )
species.goat <- c(unq.spcs[2]  )
species.sheep <- c(unq.spcs[1]  )


unq.intake.units <- unique(d$feed_intake_unit)

d[d$feed_intake_unit == unq.intake.units[1] , 'feed_intake_kg_d'] <- d[d$feed_intake_unit == unq.intake.units[1] , 'feed_intake_value'] 



d <- d[d$feed_intake_unit == unq.intake.units[1] , ] 


d$feed_intake_g_d <- d$feed_intake_kg_d * 1000

print(paste('Quantity of studies before outlier removal: ', length(unique(d$B.Code))))
d <- d[ !is.na(d$feed_intake_kg_d) , ]
d <- d[ !is.na(d$T.Animals) , ]
print(paste('Quantity of studies after outlier removal: ', length(unique(d$B.Code))))


d <- data.frame(d)

}

# disaggregated data analysis






{

bovines <- d[  d$Species %in% species.cattle  , ]
  unq.brdss <- unique(bovines$Variety)
  
  
# -- Dairy
breeds.dairy <- c(
  unq.brdss[1]
  ,  unq.brdss[3]
  ,  unq.brdss[4]
  
  
  ,  unq.brdss[12]
  ,  unq.brdss[14]
  ,  unq.brdss[15]
  ,  unq.brdss[16]
  ,  unq.brdss[17]
  ,  unq.brdss[18]
  ,  unq.brdss[20]
  ,  unq.brdss[21]
  ,  unq.brdss[23]
  ,  unq.brdss[24]
  ,  unq.brdss[25]
  ,  unq.brdss[29]
  ,  unq.brdss[30]
  ,  unq.brdss[32]
  ,  unq.brdss[33]
  ,  unq.brdss[34]
  ,  unq.brdss[37]
  ,  unq.brdss[38]
  ,  unq.brdss[39]
)



dairy <- bovines[d$Variety %in% breeds.dairy , ]


# Subsetting based on nutrition

dairy <- dairy[  dairy$NDF_nutrition != na.value  , ]

dairy <- dairy[  dairy$CP_nutrition != na.value  , ]

dairy <- dairy[dairy$DM_digest != na.value  , ]




dairy <- dairy[dairy$ADF_nutrition != na.value  , ]

dairy <- dairy[dairy$NE_nutrition != na.value  , ]

dairy <- dairy[dairy$ME_nutrition != na.value  , ]


# Subsetting based on physiology
dairy <- dairy[  dairy$bw_kg != 'NA'  , ]

dairy <- dairy[  dairy$adg_g_day != 'NA'  , ]

dairy <- dairy[  dairy$milk_kg_day != 'NA'  , ]

dairy <- dairy[  dairy$Stage != 'NA'  , ]

dairy[,'B.Code']



length(unique(dairy$B.Code))
length(unique(dairy$A.Level.Name))
sum(as.numeric(dairy$T.Animals))




length(unique(dairy[   dairy$NDF_nutrition  != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$DM_digest != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$CP_nutrition != 'NA' , 'B.Code'])) # 18 


length(unique(dairy[   dairy$NE_nutrition != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$ME_nutrition != 'NA' , 'B.Code'])) # 18


length(unique(dairy[   dairy$milk_kg_day  != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$Stage != 'NA' , 'B.Code'])) # 18 
length(unique(dairy[   dairy$adg_g_day != 'NA' , 'B.Code'])) # 18 




# BOS INDCUS
breeds.indicus <- c(
  unq.brdss[1]
  ,  unq.brdss[3]
  ,  unq.brdss[4]
  
  
  ,  unq.brdss[12]
  ,  unq.brdss[14]
  ,  unq.brdss[15]
  ,  unq.brdss[16]
  ,  unq.brdss[17]
  ,  unq.brdss[18]
  ,  unq.brdss[20]
  ,  unq.brdss[21]
  ,  unq.brdss[23]
  ,  unq.brdss[24]
  ,  unq.brdss[25]
  ,  unq.brdss[30]
  ,  unq.brdss[32]
  ,  unq.brdss[37]
  ,  unq.brdss[38]
  ,  unq.brdss[39]
)



# Beef 
breeds.beef <- c(
  unq.brdss[35]
  ,  unq.brdss[36]
)



beef <- d[d$Variety %in% breeds.beef , ]



length(unique(beef$B.Code))
length(unique(beef$A.Level.Name))
sum(as.numeric(beef$T.Animals))



# Breed indicus
breeds.indicus <- unq.brdss[!(unq.brdss %in% breeds.dairy) ]
  
breeds.indicus <-
  c(
  unq.brdss[5]
  ,  unq.brdss[6]
  ,  unq.brdss[4]
  
  ,  unq.brdss[7]
  ,  unq.brdss[8]
  ,  unq.brdss[9]
  ,  unq.brdss[11]
  ,  unq.brdss[13]
  ,  unq.brdss[19]
  ,  unq.brdss[26]
  ,  unq.brdss[27]
  ,  unq.brdss[28]
)


zebu <- d[d$Variety %in% breeds.indicus , ]

View(zebu)

# SUbsetting based on nutrition
zebu <- zebu[zebu$DM_digest != na.value  , ]

zebu <- zebu[zebu$NDF_nutrition != na.value  , ]

zebu <- zebu[zebu$CP_nutrition != na.value  , ]


zebu <- zebu[zebu$ADF_nutrition != na.value  , ]

zebu <- zebu[zebu$DM_digest != na.value  , ]

zebu <- zebu[zebu$NE_nutrition != na.value  , ]

zebu <- zebu[zebu$ME_nutrition != na.value  , ]


# Subsetting based on physiology
zebu <- zebu[  zebu$bw_kg != 'NA'  , ]


zebu <- zebu[  zebu$milk_kg_day != 'NA'  , ]

zebu <- zebu[  zebu$Stage != 'NA'  , ]

zebu <- zebu[  zebu$adg_g_day != 'NA'  , ]


length(unique(zebu$B.Code))
length(unique(zebu$A.Level.Name))
sum(as.numeric(zebu$T.Animals))

}



# Goats
goats <- d[  d$Species %in% species.goat  , ]



goats <- goats[goats$CP_nutrition != na.value  , ]
goats <- goats[goats$NDF_nutrition != na.value  , ]

goats <- goats[goats$bw_kg != na.value  , ]
goats <- goats[goats$adg_g_day != na.value  , ]
goats <- goats[goats$EE_nutrition != na.value  , ]
goats <- goats[goats$Ash_nutrition != na.value  , ]

length(unique(goats$B.Code))
length(unique(goats$diet.code))


goats$T.Animals <- as.numeric(goats$T.Animals)
mean.samp.sz <- mean(na.omit(goats$T.Animals))

goats[, 'reg.weight'] <- goats[, 'T.Animals'] / mean.samp.sz 


hist(goats$feed_intake_g_d)
hist(goats$NDF_nutrition)
hist(goats$CP_nutrition)
hist(goats$EE_nutrition)
hist(goats$Ash_nutrition)
hist(goats$CP_nutrition)
hist(goats$bw_kg)
hist(goats$adg_g_day)


plot(goats$NDF_nutrition , goats$feed_intake_kg_d , )



# Outlier removal
{
  
# Handle outliers
min.feed.intake <- 0.1
max.feed.intake <- 5
goats <- goats[ goats$feed_intake_kg_d > min.feed.intake & goats$feed_intake_kg_d <= max.feed.intake , ]

min.adg.g.d <- -1000
max.adg.g.d <- 5000
goats <- goats[goats$adg_g_day >  min.adg.g.d & goats$adg_g_day <= max.adg.g.d , ]

min.ndf.g.kg <- 100
max.ndf.g.kg <- 1000
goats <- goats[goats$NDF_nutrition >  min.ndf.g.kg & goats$NDF_nutrition <= max.ndf.g.kg  , ]

  
}


gt.mod.1 <- lmer( 
  feed_intake_g_d  ~  bw_kg 
  +  adg_g_day
  + CP_nutrition 
  +  NDF_nutrition
  + Ash_nutrition
  + (
     - 1 
   + bw_kg 
   #  + adg_g_day 
   #  + CP_nutrition 
    # +  NDF_nutrition
   #  + Ash_nutrition
    #  + EE_nutrition
    | B.Code)
  , weights = reg.weight
  , data = goats#[goats$CP_nutrition >= 150 , ]
)

r.squaredGLMM(gt.mod.1)

goats$residuals <- residuals(gt.mod.1 )

rmse(gt.mod.1) / (mean(na.omit(goats$  feed_intake_g_d )))


gt.mod.1


summary(gt.mod.1)
coef(gt.mod.1)
confint(gt.mod.1)


r2.ffx <- r.squaredGLMM(gt.mod.1)[1]
r2.cbined <- r.squaredGLMM(gt.mod.1)[2]
r2.rfx <- r2.cbined - r2.rfx
r2.rfx


mod.sum <- tab_model(
  gt.mod.1
  #, logit.boran.ext 
  #  , pred.labels = pred.labels ,
  #  dv.labels = c("Zebu", "Boran"),
  , string.pred = "Coefficient"
  #  string.ci = "Conf. Int (95%)"
  # string.p = "P-Value"
)

mod.sum


# --- SHEEP
sheep <- d[  d$Species %in% species.sheep  , ]

sheep <- sheep[!is.na(sheep$feed_intake_value) , ]

sheep <- sheep[!is.na(sheep$DM_digest)   , ]
sheep <- sheep[!is.na(sheep$CP_nutrition)  , ]
sheep <- sheep[!is.na(sheep$NDF_nutrition)   , ]

sheep <- sheep[!is.na(sheep$bw_kg )  , ]
sheep <- sheep[!is.na(sheep$adg_g_day)  , ]
sheep <- sheep[!is.na(sheep$milk_kg_day)  , ]
sheep <- sheep[!is.na(sheep$Stage)  , ]


length(unique(sheep$B.Code))
length(unique(sheep$diet.code))


# Handle outliers
min.feed.intake <- 0.1
max.feed.intake <- 5
sheep <- sheep[sheep$feed_intake_value > min.feed.intake & sheep$feed_intake_value <= max.feed.intake , ]


# Variable checks
hist(sheep$feed_intake_g_d)
hist(sheep$NDF_nutrition)
hist(sheep$CP_nutrition)

hist(sheep$bw_kg)
hist(sheep$adg_g_day)

cor(na.omit(sheep$bw_kg) , na.omit(sheep$CP_nutrition))


variables.to.check <- c(
  'bw_kg'
  , 'adg_g_day'
  , 'NDF_nutrition'
  , 'CP_nutrition'
  )

for (v in variables.to.check){
  
  for (v2 in variables.to.check[ -c(which(variables.to.check == "CP_nutrition"))]  ){
  
cor <- cor(
sheep[complete.cases(sheep[,c(v, v2)]),v]
,sheep[complete.cases(sheep[,c(v , v2)]),v2]
)
  
print(paste('correlation between ' ,v ,'and ',v2,': ' ,cor ))    
    
}
}




# Apply weights
sp.mean.samp.sz <- mean(na.omit(sheep$T.Animals))

sheep[, 'reg.weight'] <-  .125 * sheep[, 'T.Animals']# / sp.mean.samp.sz 


vars.1 <- c(
  'feed_intake_g_d'
  ,  'bw_kg'
  ,   'CP_nutrition' 
  ,  'NDF_nutrition'
  ,'Ash_nutrition'
)

sheep.cc.f1 <- sheep[complete.cases(sheep[,vars.1]),]
sheep.cc.f1 <- sheep.cc.f1[  !(sheep.cc.f1$diet.code %in% ol.ids) , ]


sheep.cc.f1$reg.weight <- sheep.cc.f1[, 'T.Animals'] / sp.mean.samp.sz 


sp.mod.1 <- lmer( 
  feed_intake_g_d  ~  bw_kg 
 # +  adg_g_day
  + CP_nutrition 
  +  NDF_nutrition
  + Ash_nutrition
 # + EE_nutrition
  + (
    1 
   # + bw_kg 
   # + adg_g_day 
   # + CP_nutrition 
   # +  NDF_nutrition
   # + Ash_nutrition
  #  + EE_nutrition
      | B.Code)
 , weights = reg.weight
  , data = sheep.cc.f1
 # , data = sheep[sheep$bw_kg > mean(na.omit(sheep$bw_kg )), ]  
  )

sheep.cc.f1$resds <- residuals(sp.mod.1)

View(sheep.cc.f1)


ol.ids <- sheep.cc.f1[ abs(sheep.cc.f1$resds) > 170 , 'diet.code'  ]



r.squaredGLMM(sp.mod.1)


r2.ffx <- r.squaredGLMM(sp.mod.1)[1]
r2.cbined <- r.squaredGLMM(sp.mod.1)[2]
r2.rfx <- r2.cbined - r2.rfx
r2.rfx

summary(sp.mod.1)
coef(sp.mod.1)
confint(sp.mod.1)







mod.sum <- tab_model(
  sp.mod.1
  #, logit.boran.ext 
#  , pred.labels = pred.labels ,
#  dv.labels = c("Zebu", "Boran"),
   , string.pred = "Coefficient"
#  string.ci = "Conf. Int (95%)"
 # string.p = "P-Value"
)

mod.sum

