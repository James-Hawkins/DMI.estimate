

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

d <<- read_excel('Full_Feed_intake_data.xlsx'
                 , sheet = 'Full_Feed_Intake_data' 
                 , col_types = "text")


na.value <- 'NA'

unq.spcs <- unique(d$Species)

species.cattle <- c(   unq.spcs[3], unq.spcs[9]  )
species.shoat <- c(unq.spcs[1] ,unq.spcs[2]  )

# disaggregated data analysis
d <- d[  d$Species %in% species.cattle  , ]






unq.brdss <- unique(d$Variety)





d <- data.frame(d)

length(unique(d[, 'B.Code']))

length(unique(d[  d$bw_kg != na.value , 'B.Code']))


length(unique(d[  d$bw_kg != na.value & d$feed_intake_value != na.value & d$milk_kg_day == na.value, 'B.Code']))

length(unique(d[  d$bw_kg != na.value & d$feed_intake_value != na.value & d$milk_kg_day == na.value , 'B.Code']))

length(unique(d[ 
  d$bw_kg != na.value 
  & d$feed_intake_value != na.value 
  & d$milk_kg_day != na.value 
 # & d$NDF_nutrition != na.value 
  & d$DM_digest != na.value 
  , "A.Level.Name"]))
  
  
 

length(unique(d[  d$bw_kg != na.value & d$feed_intake_value != na.value & d$milk_kg_day == na.value, 'T.Animals']))



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



dairy <- d[d$Variety %in% breeds.dairy , ]


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



# SHOATS
shoats <- d[  d$Species %in% species.shoat  , ]

length(unique(shoats$B.Code))


shoats <- shoats[shoats$DM_digest != na.value  , ]
shoats <- shoats[shoats$CP_nutrition != na.value  , ]
shoats <- shoats[shoats$NDF_nutrition != na.value  , ]

shoats <- shoats[shoats$bw_kg != na.value  , ]
shoats <- shoats[shoats$adg_g_day != na.value  , ]
shoats <- shoats[shoats$milk_kg_day != na.value  , ]
shoats <- shoats[shoats$Stage != na.value  , ]


# Goats
goats <- d[  d$Species %in% species.shoat[2]  , ]


goats <- goats[goats$DM_digest != na.value  , ]
goats <- goats[goats$CP_nutrition != na.value  , ]
goats <- goats[goats$NDF_nutrition != na.value  , ]

goats <- goats[goats$bw_kg != na.value  , ]
goats <- goats[goats$adg_g_day != na.value  , ]
goats <- goats[goats$milk_kg_day != na.value  , ]
goats <- goats[goats$Stage != na.value  , ]

length(unique(goats$B.Code))
length(unique(goats$A.Level.Name))

# Sheep
sheep <- d[  d$Species %in% species.shoat[1]  , ]

sheep <- sheep[sheep$DM_digest != na.value  , ]
sheep <- sheep[sheep$CP_nutrition != na.value  , ]
sheep <- sheep[sheep$NDF_nutrition != na.value  , ]

sheep <- sheep[sheep$bw_kg != na.value  , ]
sheep <- sheep[sheep$adg_g_day != na.value  , ]
sheep <- sheep[sheep$milk_kg_day != na.value  , ]
sheep <- sheep[sheep$Stage != na.value  , ]


length(unique(sheep$B.Code))
length(unique(sheep$A.Level.Name))




