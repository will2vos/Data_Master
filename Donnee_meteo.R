#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

#importation jeu de données
Meteo<-read.csv("Donnees_meteo_20221212.csv",header = TRUE, sep=";",dec = ",", stringsAsFactors = TRUE)
str(Meteo)

Meteo$date<-as.Date(Meteo$date,format="%Y-%m-%d")



################### Comparaison de météo entre les sites #####################

# période mai à aout 2020

Meteo$JD20<- julian(Meteo$date, origin= as.Date("2020-05-01"),format="%Y-%m-%d")

Meteo20<-  subset(Meteo, JD20 >0 & JD20<124) 

### boxplot
## rh humidité relative
layout(matrix(1:1, nrow=1, ncol=1))
boxplot(Meteo20$rh ~ Meteo20$bloc, ylab = "Humidité relative (proportion)",
        xlab = "Blocs", main = "Humidité relative en fonction des blocs, 
        mai à août 2020",cex.axis = 1.5, cex.lab = 1.5)

## airtemp température de l'air
boxplot(Meteo20$airtemp ~ Meteo20$bloc, ylab = "Température (°C)",
        xlab = "Blocs", main = "Température de l'air (°C) en fonction des blocs, 
        mai à août 2020",cex.axis = 1.5, cex.lab = 1.5)

## vwc contenue en eau du sol
boxplot(Meteo20$vwc ~ Meteo20$bloc, ylab = "Contenu en eau (proportion)",
        xlab = "Blocs", main = "Contenu en eau du sol en fonction des blocs, 
        mai à août 2020",cex.axis = 1.5, cex.lab = 1.5)

## soltemp température du sol
boxplot(Meteo20$soltemp ~ Meteo20$bloc, ylab = "Température (°C)",
        xlab = "Blocs", main = "Température du sol (°C) en fonction des blocs, 
        mai à août 2020",cex.axis = 1.5, cex.lab = 1.5)




# période mai à aout 2021

Meteo$JD21<- julian(Meteo$date, origin= as.Date("2021-05-01"),format="%Y-%m-%d")

Meteo21<-  subset(Meteo, JD21 >0 & JD21<124) 

## rh humidité relative
layout(matrix(1:4, nrow=2, ncol=2))
boxplot(Meteo21$rh ~ Meteo21$bloc, ylab = "Humidité relative (proportion)",
        xlab = "Blocs", main = "Humidité relative en fonction des blocs, 
        mai à août 2021",cex.axis = 1.5, cex.lab = 1.5)

## airtemp température de l'air
boxplot(Meteo21$airtemp ~ Meteo21$bloc, ylab = "Température (°C)",
        xlab = "Blocs", main = "Température de l'air (°C) en fonction des blocs, 
        mai à août 2021",cex.axis = 1.5, cex.lab = 1.5)

## vwc contenue en eau du sol

boxplot(Meteo21$vwc ~ Meteo21$bloc, ylab = "Contenu en eau du sol (proportion)",
        xlab = "Blocs", main = "Contenu en eau du sol en fonction des blocs, 
        mai à août 2021",cex.axis = 1.5, cex.lab = 1.5)

## soltemp température du sol
boxplot(Meteo21$soltemp ~ Meteo21$bloc, ylab = "Température (°C)",
        xlab = "Blocs", main = "Température du sol (°C) en fonction des blocs, 
        mai à août 2021",cex.axis = 1.5, cex.lab = 1.5)



# selection de la periode mai-aout 2022

Meteo$JD22<- julian(Meteo$date, origin= as.Date("2022-05-01"),format="%Y-%m-%d")

Meteo22<-  subset(Meteo, JD22 >0 & JD22<124) 
Meteo22_CT<- Meteo22[Meteo22$cut == "CT",]
Meteo22_CT$bloc <- as.character( Meteo22_CT$bloc)
Meteo22_CT_CD <- Meteo22_CT[Meteo22_CT$bloc == "C" | Meteo22_CT$bloc == "D",] 

layout(matrix(1:1, nrow=1, ncol=1))
boxplot(Meteo22_CT_CD$soltemp ~ Meteo22_CT_CD$bloc, ylab = "Précipitation (mm)",
        xlab = "Blocs", main = "Précipitation en fonction des blocs, 
        mai à août 2022",cex.axis = 1.5, cex.lab = 1.5)

#moyenne des données meteos 2022

Meteo22_CD <- Meteo22[Meteo22$bloc == "C" | Meteo22$bloc == "D",] 

anyNA(Meteo22_CD$rh)
anyNA(Meteo22_CD$airtemp)
anyNA(Meteo22_CD$vwc)
anyNA(Meteo22_CD$soltemp)

#regrouper les données du bloc C et D en moyennes journalières
Meteo22_CD_mean<-as.data.frame(aggregate
                               (x = list(Meteo22_CD$rh,Meteo22_CD$airtemp,Meteo22_CD$vwc,Meteo22_CD$soltemp),by = list(Meteo22_CD$date,Meteo22_CD$bloc)
                                 ,FUN = mean ))

colnames(Meteo22_CD_mean)[]<- c("date","bloc","rh", "airtemp", "vwc","soltemp")

mean(Meteo22_CD_mean$rh, na.rm = TRUE)
mean(Meteo22_CD$rh,na.rm = TRUE)

mean(Meteo22_CD_mean$airtemp, na.rm = TRUE)
mean(Meteo22_CD$airtemp,na.rm = TRUE)

mean(Meteo22_CD_mean$vwc, na.rm = TRUE)
mean(Meteo22_CD$vwc,na.rm = TRUE)

mean(Meteo22_CD_mean$soltemp, na.rm = TRUE)
mean(Meteo22_CD$soltemp,na.rm = TRUE)

# verification des correlation entre les variable meteo
## a partir des données brute du bloc C et D

#precipitation x température
#precipitation x rh

Meteo22_CD_narm<-na.omit(Meteo22_CD)
#cor(Meteo22_CD_narm$airtemp,Meteo22_CD_narm$soltemp)
#plot(Meteo22_CD_narm$airtemp,Meteo22_CD_narm$soltemp)

plot(~ airtemp + soltemp , data = Meteo22_CD_narm)

#cor(Meteo22_CD_narm$rh, Meteo22_CD_narm$vwc)
#plot(Meteo22_CD_narm$rh, Meteo22_CD_narm$vwc)

plot(~ rh + vwc , data = Meteo22_CD_narm)

## a partir des moyenne journalière des données journalières
Meteo22_CD_mean_narm<-na.omit(Meteo22_CD_mean)

cor(Meteo22_CD_mean_narm$airtemp,Meteo22_CD_mean_narm$soltemp)
plot(Meteo22_CD_mean_narm$airtemp,Meteo22_CD_mean_narm$soltemp)

cor(Meteo22_CD_mean_narm$rh, Meteo22_CD_mean_narm$vwc)
plot(Meteo22_CD_mean_narm$rh, Meteo22_CD_mean_narm$vwc)

#précipitation x 

plot(~ airtemp + rh + precip + JD22 + soltemp , data = Meteo22_CD_narm)

#airtemp
plot(Meteo22_CD_narm$airtemp,Meteo22_CD_narm$rh) #oui
plot(Meteo22_CD_narm$airtemp,Meteo22_CD_narm$precip) #non
plot(Meteo22_CD_narm$airtemp,Meteo22_CD_narm$JD22) #non

#RH
plot(Meteo22_CD_narm$rh,Meteo22_CD_narm$precip) #oui
plot(Meteo22_CD_narm$airtemp,Meteo22_CD_narm$JD) #non

#precip
plot(Meteo22_CD_narm$precip,Meteo22_CD_narm$JD22) #non



#Extraire l'information sur les visites (facteurs influençant la détection)
meteo_visit <- unique(Meteo22_CD[,c("rh", "airtemp","vwc","soltemp","precip","bloc",
                                    "cut","date","JD22")])

#ordonner en fonction de date et heure
data_order<-meteo_visit[order(meteo_visit$bloc,as.Date(meteo_visit$date)),]

# selection des journée d'interet entre mai et aout 2022
data_subset<- subset(data_order,JD22 == 19 | JD22==47 | JD22==74 |JD22==108 )

data_aggre<-as.data.frame(aggregate
                               (x = list(data_subset$rh,data_subset$airtemp,data_subset$vwc,data_subset$soltemp),by = list(data_subset$date,data_subset$cut,data_subset$JD22)
                                 ,FUN = mean, na.rm =TRUE))

colnames(data_aggre)[]<- c("date","cut","Jday","rh","airtemp", "vwc", "soltemp")

data_precip<-as.data.frame(aggregate
                          (x = list(data_subset$precip),by = list(data_subset$date,data_subset$cut,data_subset$JD22)
                            ,FUN = sum, na.rm =TRUE))

colnames(data_precip)[]  <- c("date","cut","Jday","precip")


data_aggre <- merge(x=data_aggre,y=data_precip, 
             by=c("date","cut","Jday"), all.x = TRUE)



#identification de chaque visite pour chaque site
data_aggre$visit <- ifelse(data_aggre$Jday <=22,1,
                           ifelse(data_aggre$Jday<=50,2,
                                  ifelse(data_aggre$Jday<=75,3,4)))


#######Transposer les données pour la date d'enregistrement############
Data<-read.csv("Data.csv",header = TRUE, sep=";", stringsAsFactors = TRUE)

Data2<-unique(Data[,c("ID_site","bloc","coupe","exclos","analogue")])
Data2$coupe<-ifelse(Data2$coupe == "totale","CT","50")
colnames(Data2)[3]  <- "cut"

Data3 <- merge(x=Data2,y=data_aggre, 
                    by="cut", all = TRUE)

# boucle

Data3$ID_site<-as.character(Data3$ID_site)
jday <- as.data.frame(matrix(ncol = 5, nrow = 60))
jday[,1] <- as.character(unique(Data3$ID_site))


for(i in 1:nrow(jday)){
  subset_jday <- subset(Data3, ID_site == jday$V1[i])
  visites_jday <- subset_jday$Jday
  for(j in 1:length(visites_jday)){
    jday[i,j+1] <- visites_jday[j]
  }
}

colnames(jday)[] <- c("ID", "1", "2", "3", "4")

write.csv(jday,"pcin_jday.csv",row.names = FALSE)

#rh

Data3$ID_site<- as.character(Data3$ID_site)
Rh <- as.data.frame(matrix(ncol = 5, nrow = 60))
Rh[,1] <- unique(Data3$ID_site)

for(i in 1:nrow(Rh)){
  subset_rh <- subset(Data3, ID_site == Rh$V1[i])
  visites_rh <- subset_rh$rh
  for(j in 1:length(visites_rh)){
    Rh[i,j+1] <- visites_rh[j]
  }
}

colnames(Rh)[] <- c("ID", "1", "2", "3", "4")

write.csv(Rh,"pcin_rh.csv",row.names = FALSE)



#airtemp


Airtemp <-as.data.frame(matrix(ncol = 5, nrow = 60))
Airtemp[,1] <- unique(Data3$ID_site)

for(i in 1:nrow(Airtemp)){
  subset_airtemp <- subset(Data3, ID_site == Airtemp$V1[i])
  visites_airtemp <- subset_airtemp$airtemp
  for(j in 1:length(visites_airtemp)){
    Airtemp[i,j+1] <- visites_airtemp[j]
  }
}

colnames(Airtemp)[] <- c("ID", "1", "2", "3", "4")

write.csv(Airtemp,"pcin_airtemp.csv",row.names = FALSE)

#matrice précipitation 

## il manque l'information des précipitatiuons du 14 juin pour les blocs C et D
## je regarde donc pour voir si le bloc A et B détiennent cette info

precip_19<-Meteo22[Meteo22$JD22== 19,]
unique(precip_19$JD22)
unique(precip_19$precip)

precip_18<-Meteo22[Meteo22$JD22== 18,]
unique(precip_18$JD22)
unique(precip_18$precip)

precip_17<-Meteo22[Meteo22$JD22== 17,]
unique(precip_17$JD22)
unique(precip_17$precip)

#les seules valeurs de précipitaiton pour le 14 sont de 0 mm ou NA

#remplacer les NA pour la journée du 14 par la valeur de 0

Meteo22_CT_CD$precip<- replace(Meteo22_CT_CD$precip, Meteo22_CT_CD$JD22 == 19,0)
Meteo22_CT_CD$precip<- replace(Meteo22_CT_CD$precip, Meteo22_CT_CD$JD22 == 18,0)
Meteo22_CT_CD$precip<- replace(Meteo22_CT_CD$precip, Meteo22_CT_CD$JD22 == 17,0)

precip22<-subset(Meteo22_CT_CD, Meteo22_CT_CD$precip != "NA")

precip_v1<- precip22[precip22$JD22 >= 17 & precip22$JD22 <=19,]
precip_v2<- precip22[precip22$JD22 >= 45 & precip22$JD22 <=47,]
precip_v3<- precip22[precip22$JD22 >= 72 & precip22$JD22 <=74,]
precip_v4<- precip22[precip22$JD22 >= 106 & precip22$JD22 <=108,]

#tab_v1<-as.data.frame(aggregate(x= list(precip_v1$precip), by= list(precip_v1$JD22), FUN = sum))

#precip_agg<-as.data.frame(aggregate(x= list(precip22$precip), by = list(precip22$JD22), FUN = sum))
#colnames(precip_agg)[]<- c("JD22","precip")

precip_agg_JD_bloc<-as.data.frame(aggregate(x= list(precip22$precip), by = list(precip22$JD22, precip22$bloc), FUN = sum))
colnames(precip_agg_JD_bloc)[]<- c("JD22","bloc","precip")

precip_agg<-as.data.frame(aggregate(x= list(precip_agg_JD_bloc$precip), by = list(precip_agg_JD_bloc$JD22), FUN = mean))
colnames(precip_agg)[]<- c("JD22","precip")

v_20<- precip_agg[precip_agg$JD22 >= 18 & precip_agg$JD22<= 20,]
sum(v_20$precip)

v_21<- precip_agg[precip_agg$JD22 >= 19 & precip_agg$JD22<= 21,]
sum(v_21$precip)

v_22<- precip_agg[precip_agg$JD22 >= 20 & precip_agg$JD22<= 22,]
sum(v_22$precip)

v_23<- precip_agg[precip_agg$JD22 >= 21 & precip_agg$JD22<= 23,]
sum(v_23$precip)

v_47<- precip_agg[precip_agg$JD22 >= 45 & precip_agg$JD22<= 47,]
sum(v_47$precip)

v_48<- precip_agg[precip_agg$JD22 >= 46 & precip_agg$JD22<= 48,]
sum(v_48$precip)

v_49<- precip_agg[precip_agg$JD22 >= 47 & precip_agg$JD22<= 49,]
sum(v_49$precip)

v_50<- precip_agg[precip_agg$JD22 >= 48 & precip_agg$JD22<= 50,]
sum(v_50$precip)

v_72<- precip_agg[precip_agg$JD22 >= 70 & precip_agg$JD22<= 72,]
sum(v_72$precip)

v_73<- precip_agg[precip_agg$JD22 >= 71 & precip_agg$JD22<= 73,]
sum(v_73$precip)

v_74<- precip_agg[precip_agg$JD22 >= 72 & precip_agg$JD22<= 74,]
sum(v_74$precip)

v_75<- precip_agg[precip_agg$JD22 >= 73 & precip_agg$JD22<= 75,]
sum(v_75$precip)

v_106<- precip_agg[precip_agg$JD22 >= 104 & precip_agg$JD22<= 106,]
sum(v_106$precip)

v_107<- precip_agg[precip_agg$JD22 >= 105 & precip_agg$JD22<= 107,]
sum(v_107$precip)

v_108<- precip_agg[precip_agg$JD22 >= 106 & precip_agg$JD22<= 108,]
sum(v_108$precip)

v_109<- precip_agg[precip_agg$JD22 >= 107 & precip_agg$JD22<= 109,]
sum(v_109$precip)



#####
mat_precip_3D<- as.data.frame(matrix(ncol=5, nrow = 60))
Site<-unique(Data$ID_site)
mat_precip_3D[,1]<-Site
mat_precip_3D[,2]<-sum(precip_v1$precip)
mat_precip_3D[,3]<-sum(precip_v2$precip)
mat_precip_3D[,4]<-sum(precip_v3$precip)
mat_precip_3D[,5]<-sum(precip_v4$precip)

colnames(mat_precip_3D)[]<-c("Jday","1","2","3","4")

write.csv(mat_precip_3D,"pcin_precip3D.csv",row.names = FALSE)

rownames(mat_precip_3D)<-mat_precip_3D[,"Jday"]
mat_precip_3D<-mat_precip_3D[,c("1","2","3","4")]

# transformation des valeurs de précipitation en valeur binaire ( 0= peu, 1= beaucoup)
precip_3D_bin<-as.data.frame(ifelse(mat_precip_3D <= 0.5,0,1))

write.csv(precip_3D_bin,"precip.3D.bin.csv")


precip2<-as.data.frame(aggregate(x = list(precip22$precip),by = list(precip22$JD22)
                                 ,FUN = mean ))
colnames(precip2)[]<-c("Jday","mean_precip")

Data3<-merge(x=Data3, y=precip2, by= "Jday", all.x = TRUE )

for(i in 1:nrow(mat_precip)){
  subset_precip <- subset(Data3, ID_site == mat_precip$V1[i])
  visites_precip <- subset_precip$mean_precip
  for(j in 1:length(visites_precip)){
    mat_precip[i,j+1] <- visites_precip[j]
  }
}

#write.csv(mat_precip,"pcin_precip.csv",row.names = FALSE)

# création de dataframe pour les précipitation de deux jours avant l'inventaire

# mois de mai
meteo_1823<- Meteo22[Meteo22$JD22 >= 18 & Meteo22$JD22 < 24, ]
unique(meteo_1823$precip) # NA ou 0
sum(meteo_1823$precip, na.rm = TRUE) # le niveau de precipitation et de 0 pour toute la période

# mois de juin
meteo_4550<- Meteo22[Meteo22$JD22 >= 45 & Meteo22$JD22 <= 50 &  Meteo22$cut == "CT", ]
meteo_4550_agg<-as.data.frame(aggregate(x = list(meteo_4550$precip), by = list(meteo_4550$JD22, meteo_4550$bloc), FUN = sum))
colnames(meteo_4550_agg)[]<- c("JD22", "bloc", "precip")
meteo_4550_agg_JD<-as.data.frame(aggregate(x = list(meteo_4550_agg$precip), by = list(meteo_4550_agg$JD22), FUN = mean))
colnames(meteo_4550_agg_JD)[]<- c("JD22", "precip")

precip_4547<- meteo_4550_agg_JD[meteo_4550_agg_JD$JD22 >= 45 & meteo_4550_agg_JD$JD22 <= 47,]
juin_47<-sum(precip_4547$precip)

precip_4648<- meteo_4550_agg_JD[meteo_4550_agg_JD$JD22 >= 46 & meteo_4550_agg_JD$JD22 <= 48,]
juin_48<-sum(precip_4648$precip)

precip_4749<- meteo_4550_agg_JD[meteo_4550_agg_JD$JD22 >= 47 & meteo_4550_agg_JD$JD22 <= 49,]
juin_49<-sum(precip_4749$precip)

precip_4850<- meteo_4550_agg_JD[meteo_4550_agg_JD$JD22 >= 48 & meteo_4550_agg_JD$JD22 <= 50,]
juin_50<-sum(precip_4850$precip)

# mois de juillet
# correction de la periode 78-81 pour 72-75 car ce n'était pas les bons jours juliens de visite
meteo_7881<- Meteo22[Meteo22$JD22 >= 70 & Meteo22$JD22 <= 75 &  Meteo22$cut == "CT", ]
meteo_7881_agg<-as.data.frame(aggregate(x = list(meteo_7881$precip), by = list(meteo_7881$JD22, meteo_7881$bloc), FUN = sum))
colnames(meteo_7881_agg)[]<- c("JD22", "bloc", "precip")
meteo_7881_agg_JD<-as.data.frame(aggregate(x = list(meteo_7881_agg$precip), by = list(meteo_7881_agg$JD22), FUN = mean))
colnames(meteo_7881_agg_JD)[]<- c("JD22", "precip")

precip_7678<- meteo_7881_agg_JD[meteo_7881_agg_JD$JD22 >= 70 & meteo_7881_agg_JD$JD22 <= 72,]
juillet_78<-sum(precip_7678$precip)

precip_7779<- meteo_7881_agg_JD[meteo_7881_agg_JD$JD22 >= 71 & meteo_7881_agg_JD$JD22 <= 73,]
juillet_79<-sum(precip_7779$precip)

precip_7880<- meteo_7881_agg_JD[meteo_7881_agg_JD$JD22 >= 72 & meteo_7881_agg_JD$JD22 <= 74,]
juillet_80<-sum(precip_7880$precip)

precip_7981<- meteo_7881_agg_JD[meteo_7881_agg_JD$JD22 >= 73 & meteo_7881_agg_JD$JD22 <= 75,]
juillet_81<-sum(precip_7981$precip)

# mois d'aout

meteo_106109<- Meteo22[Meteo22$JD22 >= 104 & Meteo22$JD22 <= 109 &  Meteo22$cut == "CT", ]
meteo_106109_agg<-as.data.frame(aggregate(x = list(meteo_106109$precip), by = list(meteo_106109$JD22, meteo_106109$bloc), FUN = sum))
colnames(meteo_106109_agg)[]<- c("JD22", "bloc", "precip")
meteo_106109_agg_JD<-as.data.frame(aggregate(x = list(meteo_106109_agg$precip), by = list(meteo_106109_agg$JD22), FUN = mean))
colnames(meteo_106109_agg_JD)[]<- c("JD22", "precip")

precip_104106<- meteo_106109_agg_JD[meteo_106109_agg_JD$JD22 >= 104 & meteo_106109_agg_JD$JD22 <= 106,]
aout_106<-sum(precip_104106$precip)

precip_105107<- meteo_106109_agg_JD[meteo_106109_agg_JD$JD22 >= 105 & meteo_106109_agg_JD$JD22 <= 107,]
aout_107<-sum(precip_105107$precip)

precip_106108<- meteo_106109_agg_JD[meteo_106109_agg_JD$JD22 >= 106 & meteo_106109_agg_JD$JD22 <= 108,]
aout_108<-sum(precip_106108$precip)

precip_107109<- meteo_106109_agg_JD[meteo_106109_agg_JD$JD22 >= 107 & meteo_106109_agg_JD$JD22 <= 109,]
aout_109<-sum(precip_107109$precip)


# création d'un dataframe avec les nouvelle inforamtion de précipitation

precip_mai<-sum(meteo_1823$precip, na.rm = TRUE)
precip_juin <- (juin_47+juin_48+juin_49+juin_50)/4
precip_juillet<- (juillet_78+juillet_79+juillet_80+juillet_81)/4
precip_aout<- (aout_106+aout_107+aout_108+aout_109)/4

mat_precip_3D<- as.data.frame(matrix(ncol=5, nrow = 60))
Site<-unique(Data$ID_site)
mat_precip_3D[,1]<-Site
mat_precip_3D[,2]<-precip_mai
mat_precip_3D[,3]<-precip_juin
mat_precip_3D[,4]<-precip_juillet
mat_precip_3D[,5]<-precip_aout

colnames(mat_precip_3D)[]<- c("Site","V1","V2","V3","V4")

write.csv(mat_precip_3D,"precip_3D.csv",row.names = FALSE)

# dataframe de précipitation cumulé de trois jours - standardisé

row.names(mat_precip_3D)<-mat_precip_3D$Site
mat_precip_3D<-mat_precip_3D[,c("V1","V2","V3","V4")]


precip_3D <- as.matrix(mat_precip_3D)
precip3D_mean <- mean(precip_3D, na.rm = TRUE)
precip3D_sd <- sd(precip_3D, na.rm = TRUE)
precip3D_std <- as.data.frame((precip_3D - precip3D_mean)/precip3D_sd)

write.csv(precip3D_std,"precip3D_std.csv")





# pas de donnée pour le bloc A
meteo_4550_B<-meteo_4550[meteo_4550$bloc == "B" & meteo_4550$cut == "CT",]
unique(meteo_4550_B$precip)
meteo_4550_B_day<-as.data.frame(aggregate(x = list(meteo_4550_B$precip), by = list(meteo_4550_B$JD22, meteo_4550_B$bloc), FUN = mean))




sum(meteo_4550$precip[meteo_4550$bloc == "A",], na.rm = TRUE)

# regroupement des precipitation par mois avec les jours juliens
meteo_0522<- Meteo22_CT_CD[Meteo22_CT_CD$JD22 >= 18 & Meteo22_CT_CD$JD22 < 24, ]


precip.data<-read.csv(file = "pcin_precip.csv", header = TRUE, stringsAsFactors = TRUE)
