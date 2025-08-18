

sheep.cc.lo.NDF$adg_g_day
sheep.cc.lo.NDF$bw_kg.sqd <- sheep.cc.lo.NDF$bw_kg^2

sheep.cc.lo.NDF$dummy <- 1


pred.vars <- c(
  'bw_kg'
  ,  'adg_g_day'
  
  ,'NDF_nutrition'
  ,'CP_nutrition'
  , 'Ash_nutrition'
  , 'EE_nutrition'
)

suffixes <- c('.sqt' , '.sqd', '.cbd' , '.log')

for (v in pred.vars){
  
  v.sqt <- str_c( suffixes[1]  , '.sqt')
  v.sqd <- str_c( suffixes[2]  , '.sqd')
  v.cbd <- str_c( suffixes[3]  , '.cbd')
  v.log <- str_c( suffixes[4]  , '.log')
  
  
  sheep.cc.lo.NDF[, v.sqt] <-  sheep.cc.lo.NDF[,v]^0.5
  sheep.cc.lo.NDF[,v.sqd] <-  sheep.cc.lo.NDF[,v]^2
  sheep.cc.lo.NDF[,v.cbd] <-  sheep.cc.lo.NDF[,v]^3
  sheep.cc.lo.NDF[,v.log] <-  log(sheep.cc.lo.NDF[,v])
  
}

sheep.cc.lo.NDF$NDF_n


v.list.BW <- c('dummy' , 'bw_kg' )   
v.list.DG <- c('dummy' , 'adg_g_day' ) 

v.list.NDF <- c('dummy' , 'NDF_nutrition' ) 
v.list.EE <- c('dummy' , 'EE_nutrition' )  
v.list.ASH <- c('dummy' , 'Ash_nutrition' )  

for (s in suffixes){ 
  
  new.var.BW <- str_c( v.list.BW[2] ,s )
  v.list.BW <- append(v.list.BW , new.var.BW )
  
  new.var.DG <- str_c( v.list.DG[2] ,s )
  v.list.DG <- append(v.list.DG , new.var.DG )
  
  new.var.NDF <- str_c( v.list.NDF[2] ,s )
  v.list.NDF <- append(v.list.NDF , new.var.NDF )
  
  new.var.EE <- str_c( v.list.EE[2] ,s )
  v.list.EE <- append(v.list.EE , new.var.EE )
  
  new.var.ASH <- str_c( v.list.ASH[2] ,s )
  v.list.ASH <- append(v.list.ASH , new.var.ASH )


}



# Formula settings

plus.sign <- ' + '
rd.intercept <- '+ (1 '
rd.unit <- '| B.Code)'


# Data frame to store output variables
stepw.out <- data.frame(
  
   model = factor()
  , variable.1 = character()
  , variable.2 = character()
  
)





# Nested for loops
r <- 0


for ( v1 in v.list.BW ) {
  
  

for ( v2 in v.list.DG) {


formula <- as.formula(
  paste(
    "feeding.level.g.d.kg.bw ~" 
    
    # fixed
    , v1
    , plus.sign 
    , v2
    
    # random
    , rd.intercept
    , rd.unit
  )
)
  
  

model <- lmer( 
  formula
, weights = .1 * sheep.cc.lo.NDF$T.Animals / (mean(na.omit(sheep.cc.lo.NDF$T.Animals)) )
  , data = sheep.cc.lo.NDF
)


r2.mrg <- r.squaredGLMM(model)[1]
r2.cnd <- r.squaredGLMM(model)[2]

 <- AIC(model)
 <- BIC(model)


#stepw.out[ r , 'model'] <- model 
stepw.out[ r , 'variable.1'] <- v1
stepw.out[ r , 'variable.2'] <- v2

r <- r + 1

}
}



fixef(sp.mod.1.lo.NDF)
ranef(sp.mod.1.lo.NDF)
confint(sp.mod.1.lo.NDF)
coeffs(sp.mod.1.lo.NDF)
summary(sheep.cc.lo.NDF)
r.squaredGLMM(sp.mod.1.lo.NDF)
