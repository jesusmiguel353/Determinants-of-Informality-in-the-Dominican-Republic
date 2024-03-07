#################################################
"Determinants of Informality in the Dominican Republic"
"Jesus Miguel Garcia Pena"
"Research Final Work "
#################################################


#Uploading the databases
library(readxl)
Base16 <- read_excel("Base ENCFT 20161 - 20164.xlsx")

Base19 <- read_excel("Base ENCFT 20191 - 20194 entera.xlsx")

Base22 <- read_excel("Base ENCFT 20221 - 20223.xlsx")


#Filtering the databases for only include the employed people

table(Base16$OCUPADO)

library(tidyverse)

Base16 <- Base16 %>% filter(OCUPADO==1)
Base19 <- Base19 %>% filter(OCUPADO==1)
Base22 <- Base22 %>% filter(OCUPADO==1)



#Variables Creation

#Informal workers
table(Base16$GRUPO_EMPLEO)
library(car)
#2016
Base16$infor <- car:: recode(Base16$GRUPO_EMPLEO, "'Empleo Informal'=1; 'Empleo Formal'=0 ; 'Sin empleo'=NA " )
table(Base16$infor)

#2019
Base19$infor <- recode(Base19$GRUPO_EMPLEO, "'Empleo Informal'=1; 'Empleo Formal'=0 ; 'Sin empleo'=NA " )
table(Base19$infor)

#2022
Base22$infor <- recode(Base22$GRUPO_EMPLEO, "'Empleo Informal'=1; 'Empleo Formal'=0 ; 'Sin empleo'=NA " )
table(Base22$infor)



#Educational years

#Getting out individuals who do not have educational information
library(tidyverse)
Base16= filter(Base16,!is.na(NIVEL_ULTIMO_ANO_APROBADO))
Base19= filter(Base19,!is.na(NIVEL_ULTIMO_ANO_APROBADO))
Base22= filter(Base22,!is.na(NIVEL_ULTIMO_ANO_APROBADO))


#Creation of the variable 

#2016
Base16$educ <- NA

Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==10] <-0 #Quisqueya learns with you Program.
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==9] <-0 #none

Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==1] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==1,]$ULTIMO_ANO_APROBADO #Preschool
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==2] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==2,]$ULTIMO_ANO_APROBADO +1 #Primary school   
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==3] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==3,]$ULTIMO_ANO_APROBADO +9 #Highschool   
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==4] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==4,]$ULTIMO_ANO_APROBADO +9 #Tecnic highschool   
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==5] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==5,]$ULTIMO_ANO_APROBADO +13 #Bachelor  
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==6] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==6,]$ULTIMO_ANO_APROBADO +17 #Master's degree
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==7] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==7,]$ULTIMO_ANO_APROBADO +17 #Postgraduate    
Base16$educ[Base16$NIVEL_ULTIMO_ANO_APROBADO==8] <- Base16[Base16$NIVEL_ULTIMO_ANO_APROBADO==8,]$ULTIMO_ANO_APROBADO +19 #PHD   


table(Base16$educ)


#2019
Base19$educ <- NA

Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==10] <- 0 #Quisqueya learns with you Program
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==9] <- 0 #none

Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==1] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==1,]$ULTIMO_ANO_APROBADO #Preschool
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==2] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==2,]$ULTIMO_ANO_APROBADO + 1 #Primary school
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==3] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==3,]$ULTIMO_ANO_APROBADO + 9 #Highschool
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==4] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==4,]$ULTIMO_ANO_APROBADO + 9 #Tecnic highschool
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==5] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==5,]$ULTIMO_ANO_APROBADO + 13 #Bachelor
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==6] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==6,]$ULTIMO_ANO_APROBADO + 17 #Master's degree
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==7] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==7,]$ULTIMO_ANO_APROBADO + 17 #Postgraduate 
Base19$educ[Base19$NIVEL_ULTIMO_ANO_APROBADO==8] <- Base19[Base19$NIVEL_ULTIMO_ANO_APROBADO==8,]$ULTIMO_ANO_APROBADO + 19 #PHD 




#2022
Base22$educ <- NA

Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==10] <- 0 #Quisquella learns with you Program
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==9] <- 0 #none

Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==1] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==1,]$ULTIMO_ANO_APROBADO #Preschool
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==2] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==2,]$ULTIMO_ANO_APROBADO + 1 #Primary school
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==3] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==3,]$ULTIMO_ANO_APROBADO + 9 #Highschool
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==4] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==4,]$ULTIMO_ANO_APROBADO + 9 #Tecnic highschool
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==5] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==5,]$ULTIMO_ANO_APROBADO + 13 #Bachelor
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==6] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==6,]$ULTIMO_ANO_APROBADO + 17 #Master's degree
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==7] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==7,]$ULTIMO_ANO_APROBADO + 17 #Postgraduate
Base22$educ[Base22$NIVEL_ULTIMO_ANO_APROBADO==8] <- Base22[Base22$NIVEL_ULTIMO_ANO_APROBADO==8,]$ULTIMO_ANO_APROBADO + 19 #PHD 

#Descriptive Statistics

#Educational years
summary(Base16$educ)
summary(Base19$educ)
summary(Base22$educ)






#Age

summary(Base16$EDAD)
summary(Base19$EDAD)
summary(Base22$EDAD)










#Urban zone

#2016
table(Base16$ZONA)
Base16$urb <- car:: recode(Base16$ZONA, "1=1; 2=0; "  )


table(Base16$urb)

#2019
table(Base19$ZONA)
library(car)
Base19$urb <- car:: recode(Base19$ZONA, "1=1; 2=0; "  )
table(Base19$urb)



#2022
table(Base22$ZONA)
Base22$urb <- car:: recode(Base22$ZONA, "1=1; 2=0; "  )


table(Base22$urb)







#Remittances

table(Base16$RECIBIO_REMESA_EXT1)


#2016
Base16$rem <- car:: recode(Base16$RECIBIO_REMESA_EXT1, "1=1; 2=0; "  )

table(Base16$rem)

#2019
table(Base19$RECIBIO_REMESA_EXT1)
Base19$rem <- car:: recode(Base19$RECIBIO_REMESA_EXT1, "1=1; 2=0; 3=1; "  )

table(Base19$rem)

#2022
Base22$rem <- car:: recode(Base22$RECIBIO_REMESA_EXT1, "1=1; 2=0; 3=1; "  )

table(Base22$rem)






#Literacy 
table(Base16$SABE_LEER_ESCRIBIR)

#2016
table(Base16$SABE_LEER_ESCRIBIR)


Base16$alf <- car:: recode(Base16$SABE_LEER_ESCRIBIR, "1=1; 2=0; "  )

table(Base16$alf)

#2019
table(Base19$SABE_LEER_ESCRIBIR)


Base19$alf <- car:: recode(Base19$SABE_LEER_ESCRIBIR, "1=1; 2=0; "  )

table(Base19$alf)




#2022

table(Base22$SABE_LEER_ESCRIBIR)


Base22$alf <- car:: recode(Base22$SABE_LEER_ESCRIBIR, "1=1; 2=0; "  )

table(Base22$alf)





#Men
table(Base16$SEXO)

#2016
table(Base16$SEXO)


Base16$hom <- car:: recode(Base16$SEXO, "1=1; 2=0; "  )

table(Base16$hom)

#2019
table(Base19$SEXO)


Base19$hom <- car:: recode(Base19$SEXO, "1=1; 2=0; "  )

table(Base19$hom)


#2022
table(Base22$SEXO)


Base22$hom <- car:: recode(Base22$SEXO, "1=1; 2=0; "  )

table(Base22$hom)




#Agriculture

#2016
table(Base16$GRUPO_RAMA)

Base16$agro <- car:: recode(Base16$GRUPO_RAMA, "  'Agrícultura y ganadería'=1 ;    
                            'Administración pública y defensa'=0; 'Construcción'=0;
                            'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                            'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                            'Industrias'=0;   'Población sin rama de actividad'=0;
                            'Comercio'=0;   'Enseñanza'=0;
                            'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base16$agro)


#2019

table(Base19$GRUPO_RAMA)

Base19$agro <- car:: recode(Base19$GRUPO_RAMA, "  'Agrícultura y ganadería'=1 ;    
                            'Administración pública y defensa'=0; 'Construcción'=0;
                            'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                            'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                            'Industrias'=0;   'Población sin rama de actividad'=0;
                            'Comercio'=0;   'Enseñanza'=0;
                            'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base19$agro)





#2022

table(Base22$GRUPO_RAMA)

Base22$agro <- car:: recode(Base22$GRUPO_RAMA, "  'Agrícultura y ganadería'=1 ;    
                            'Administración pública y defensa'=0; 'Construcción'=0;
                            'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                            'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                            'Industrias'=0;   'Población sin rama de actividad'=0;
                            'Comercio'=0;   'Enseñanza'=0;
                            'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base22$agro)








#Construction

#2016
table(Base16$GRUPO_RAMA)

Base16$cons <- car:: recode(Base16$GRUPO_RAMA, "  'Agrícultura y ganadería'=0 ;    
                            'Administración pública y defensa'=0; 'Construcción'=1;
                            'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                            'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                            'Industrias'=0;   'Población sin rama de actividad'=0;
                            'Comercio'=0;   'Enseñanza'=0;
                            'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base16$cons)


#2019

table(Base19$GRUPO_RAMA)

Base19$cons <- car:: recode(Base19$GRUPO_RAMA, "  'Agrícultura y ganadería'=0 ;    
                            'Administración pública y defensa'=0; 'Construcción'=1;
                            'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                            'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                            'Industrias'=0;   'Población sin rama de actividad'=0;
                            'Comercio'=0;   'Enseñanza'=0;
                            'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base19$cons)





#2022

table(Base22$GRUPO_RAMA)

Base22$cons <- car:: recode(Base22$GRUPO_RAMA, "  'Agrícultura y ganadería'=0 ;    
                            'Administración pública y defensa'=0; 'Construcción'=1;
                            'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                            'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                            'Industrias'=0;   'Población sin rama de actividad'=0;
                            'Comercio'=0;   'Enseñanza'=0;
                            'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base22$cons)






#Commerce

#2016
table(Base16$GRUPO_RAMA)

Base16$com <- car:: recode(Base16$GRUPO_RAMA, "  'Agrícultura y ganadería'=0 ;    
                           'Administración pública y defensa'=0; 'Construcción'=0;
                           'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                           'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                           'Industrias'=0;   'Población sin rama de actividad'=0;
                           'Comercio'=1;   'Enseñanza'=0;
                           'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base16$com)


#2019

table(Base19$GRUPO_RAMA)

Base19$com <- car:: recode(Base19$GRUPO_RAMA, "  'Agrícultura y ganadería'=0 ;    
                           'Administración pública y defensa'=0; 'Construcción'=0;
                           'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                           'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                           'Industrias'=0;   'Población sin rama de actividad'=0;
                           'Comercio'=1;   'Enseñanza'=0;
                           'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base19$com)





#2022

table(Base22$GRUPO_RAMA)

Base22$com <- car:: recode(Base22$GRUPO_RAMA, "  'Agrícultura y ganadería'=0 ;    
                           'Administración pública y defensa'=0; 'Construcción'=0;
                           'Hoteles, bares y restaurantes'=0;    'Otros servicios'=0;        
                           'Transporte y comunicaciones'=0;   'Electricidad y agua'=0; 
                           'Industrias'=0;   'Población sin rama de actividad'=0;
                           'Comercio'=1;   'Enseñanza'=0;
                           'Intermediarios y financieras'=0;   'Salud y asistencia social'=0; "  )


table(Base22$com)







#Head of Household
table(Base16$PARENTESCO)
#1 Head of Household

#2016
table(Base16$PARENTESCO)

Base16$jef <- car:: recode(Base16$PARENTESCO, "1=1; 2=0; 3=0; 4=0; 5=0; 6=0 
                           ;7=0; 8=0; 9=0; 10=0; 11=0; 12=0   ")

table(Base16$jef)


#2019
table(Base19$PARENTESCO)

Base19$jef <- car:: recode(Base19$PARENTESCO, "1=1; 2=0; 3=0; 4=0; 5=0; 6=0 
                           ;7=0; 8=0; 9=0; 10=0; 11=0; 12=0   ")

table(Base19$jef)


#2022
table(Base22$PARENTESCO)

Base22$jef <- car:: recode(Base22$PARENTESCO, "1=1; 2=0; 3=0; 4=0; 5=0; 6=0 
                           ;7=0; 8=0; 9=0; 10=0; 11=0; 12=0   ")

table(Base22$jef)







#Married
table(Base16$ESTADO_CIVIL)

#2016
table(Base16$ESTADO_CIVIL)
Base16$cas <- car:: recode(Base16$ESTADO_CIVIL, "1=1; 2=1; 3=0; 4=0; 5=0; 6=0")
table(Base16$cas)

#2019
table(Base19$ESTADO_CIVIL)
Base19$cas <- car:: recode(Base19$ESTADO_CIVIL, "1=1; 2=1; 3=0; 4=0; 5=0; 6=0")
table(Base19$cas)


#2022
table(Base22$ESTADO_CIVIL)
Base22$cas <- car:: recode(Base22$ESTADO_CIVIL, "1=1; 2=1; 3=0; 4=0; 5=0; 6=0")
table(Base22$cas)






#South

#2016
table(Base16$ORDEN_REGION)

Base16$sur <- car:: recode(Base16$ORDEN_REGION, "1=0; 2=0; 3=1; 4=0;")

table(Base16$sur)

#2019
table(Base19$ORDEN_REGION)

Base19$sur <- car:: recode(Base19$ORDEN_REGION, "1=0; 2=0; 3=1; 4=0;")

table(Base19$sur)



#2022
table(Base22$ORDEN_REGION)

Base22$sur <- car:: recode(Base22$ORDEN_REGION, "1=0; 2=0; 3=1; 4=0;")

table(Base22$sur)

#1 Ozama
#2 North o Cibao
#3 Southwest
#4 Southeste






#Ozama 

#2016
table(Base16$ORDEN_REGION)

Base16$ozam <- car:: recode(Base16$ORDEN_REGION, "1=1; 2=0; 3=0; 4=0;")

table(Base16$ozam)

#2019`
table(Base19$ORDEN_REGION)

Base19$ozam <- car:: recode(Base19$ORDEN_REGION, "1=1; 2=0; 3=0; 4=0;")

table(Base19$ozam)



#2022
table(Base22$ORDEN_REGION)

Base22$ozam <- car:: recode(Base22$ORDEN_REGION, "1=1; 2=0; 3=0; 4=0;")

table(Base22$ozam)






#East 


#2016
table(Base16$ORDEN_REGION)

Base16$este <- car:: recode(Base16$ORDEN_REGION, "1=0; 2=0; 3=0; 4=1;")

table(Base16$este)

#2019`
table(Base19$ORDEN_REGION)

Base19$este <- car:: recode(Base19$ORDEN_REGION, "1=0; 2=0; 3=0; 4=1;")

table(Base19$este)



#2022
table(Base22$ORDEN_REGION)

Base22$este <- car:: recode(Base22$ORDEN_REGION, "1=0; 2=0; 3=0; 4=1;")

table(Base22$este)


#Quadratic educational years
Base16$educ2 <- Base16$educ**2
Base19$educ2 <- Base19$educ**2
Base22$educ2 <- Base22$educ**2


#Quadratic age
Base16$edad2 <- Base16$EDAD**2
Base19$edad2 <- Base19$EDAD**2
Base22$edad2 <- Base22$EDAD**2


#Regressions


#2016


#logit 
library(AER)
logit16n <-glm(infor~educ+  educ2 + EDAD+ edad2  + urb+ rem+ hom+ 
                 agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base16,
               family= binomial(link='logit'))

logit16nsm <-glm(infor~educ + EDAD  + urb+ rem+ hom+ 
                   agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base16,
                 family= binomial(link='logit'))

summary(logit16n)

#probit
probit16n <-glm(infor~educ+  educ2 + EDAD+ edad2  + urb+ rem+ hom+ 
                  agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base16,
                family= binomial(link='probit'))
probit16nsm <-glm(infor~educ + EDAD + urb+ rem+ hom+ 
                    agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base16,
                  family= binomial(link='probit'))


summary(probit16n)


#2019

#logit
logit19n <-glm(infor~educ+  educ2 + EDAD+ edad2  + urb+ rem+ hom+ 
                 agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base19,
               family= binomial(link='logit'))

logit19nsm <-glm(infor~educ + EDAD  + urb+ rem+ hom+ 
                   agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base19,
                 family= binomial(link='logit'))

summary(logit19n)

#probit
probit19n <-glm(infor~educ+  educ2 + EDAD+ edad2  + urb+ rem+ hom+ 
                  agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base19,
                family= binomial(link='probit'))
probit19nsm <-glm(infor~educ + EDAD + urb+ rem+ hom+ 
                    agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base19,
                  family= binomial(link='probit'))


summary(probit19n)



#2022

#logit
logit22n <-glm(infor~educ+  educ2 + EDAD+ edad2  + urb+ rem+ hom+ 
                 agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base22,
               family= binomial(link='logit'))

logit22nsm <-glm(infor~educ + EDAD  + urb+ rem+ hom+ 
                   agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base22,
                 family= binomial(link='logit'))

summary(logit22n)

#probit
probit22n <-glm(infor~educ+  educ2 + EDAD+ edad2  + urb+ rem+ hom+ 
                  agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base22,
                family= binomial(link='probit'))
probit22nsm <-glm(infor~educ + EDAD + urb+ rem+ hom+ 
                    agro+ cons+ com+ jef+ cas+ sur+ ozam + este+ alf, data=Base22,
                  family= binomial(link='probit'))


summary(probit22n)

#Choice of the model

library(pscl)

#2016

pR2(logit16n)

pR2(probit16n)

#2019 
pR2(logit19n)

pR2(probit19n)



#2022
pR2(logit22n)

pR2(probit22n)



# OLS assumptions

# Homoscedasticity 
library(lmtest)
bptest(probit16n)
bptest(logit16n)


bptest(logit19n)
bptest(probit19n)


bptest(logit22n)
bptest(probit22n)




#Robust inference to heteroscedasticity
library(vcov)
library(sandwich)

logit16nr <- coeftest(logit16n, vcov=vcovHC(logit16n,type = "HC0") )
probit16nr <- coeftest(probit16n, vcov=vcovHC(probit16n,type = "HC0") )


logit19nr <- coeftest(logit19n, vcov=vcovHC(logit19n,type = "HC0") )
probit19nr <- coeftest(probit19n, vcov=vcovHC(probit19n,type = "HC0") )



logit22nr <- coeftest(logit22n, vcov=vcovHC(logit22n,type = "HC0") )
probit22nr <- coeftest(probit22n, vcov=vcovHC(probit22n,type = "HC0") )



#Multicollinearity

vif(logit16nsm)
vif(probit16nsm)


vif(logit19nsm)
vif(probit19nsm)


vif(logit22nsm)
vif(probit22nsm)



#Marginal effects

library(margins)

margins(logit16n)

margins(probit16n)


margins(logit19n)
margins(probit19n)

margins(logit22n)
margins(probit22n)



# Exporting the regressions

#2016
library(stargazer)
stargazer(logit16nr, probit16nr, type = "latex", 
          out= "Estimation16latex.tex",
          dep.var.labels = "Probability informal worker ",
          covariate.labels = c("Years of education", "Years of education2", "Age", "Age2",
                               "Urban" ,"Remittances",
                               "Men", "Agriculture", "Construction",
                               "Comemerce", "Head of Household", "Married",
                               "South", "Ozama", "East","Literacy "  , "Constant"))


#2019
stargazer(logit19nr, probit19nr, type = "latex", 
          out= "Estimation19latex.tex",
          dep.var.labels = "Probability informal worker ",
          covariate.labels = c("Years of education", "Years of education2", "Age", "Age2",
                               "Urban" ,"Remittances",
                               "Men", "Agriculture", "Construction",
                               "Comemerce", "Head of Household", "Married",
                               "South", "Ozama", "East","Literacy "  , "Constant"))





#2022
stargazer(logit22nr, probit22nr, type = "latex", 
          out= "Estimation22latex.tex",
          dep.var.labels = "Probability informal worker",
          covariate.labels = c("Education", "Education2", "Age", "Age2",
                               "Urban" ,"Remittances",
                               "Men", "Agriculture", "Construction",
                               "Comemerce", "Head of Household", "Married",
                               "South", "Ozama", "East","Literacy", "Constant"))


