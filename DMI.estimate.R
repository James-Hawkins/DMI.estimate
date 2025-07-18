

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

library('readxl')
library('ggplot2')
library('dplyr')
library('ggpubr')
library('stringi')
library('stringr')



d <<- read_excel('feed_intake_data+JH.xlsx'
                                , sheet = 'feed_intake_raw' 
                                , col_types = "text")



na.value <- 'NA'

unq.spcs <- unique(d$Species)
unq.brdss <- unique(d$Variety)


colnames(d)


d <- d[  d$Species   , ]

species.cattle <- c(   unq.spcs[3], unq.spcs[9]         )


d <- d[  d$Species %in% species.cattle  , ]

d <- data.frame(d)


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
  ,  unq.brdss[30]
  ,  unq.brdss[32]
  ,  unq.brdss[37]
  ,  unq.brdss[38]
  ,  unq.brdss[39]
)



dairy <- d[d$Variety %in% breeds.dairy , ]


# SUbsetting based on nutrition

dairy <- dairy[dairy$NDF_nutrition != na.value  , ]

dairy <- dairy[dairy$CP_nutrition != na.value  , ]



dairy <- dairy[dairy$ADF_nutrition != na.value  , ]

dairy <- dairy[dairy$DM_digest != na.value  , ]

dairy <- dairy[dairy$NE_nutrition != na.value  , ]

dairy <- dairy[dairy$ME_nutrition != na.value  , ]


# Subsetting based on physiology
dairy <- dairy[  dairy$milk_kg_day != 'NA'  , ]

dairy <- dairy[  dairy$Stage != 'NA'  , ]

dairy <- dairy[  dairy$adg_g_day != 'NA'  , ]



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




