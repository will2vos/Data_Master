#Packages
library(unmarked)
library(AICcmodavg)
library(dplyr)


load("SEM_data_JAGS.RData")


# vérification identification de collemboles

test<- read.csv("cov_visite.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")

test4<- test[,-1]
test<-test4[order(test4$Site),]
colnames(test)[1]<-"sites"


# write.csv(test,"Pcin_abundance.csv",row.names = F)

# test3<-test[,c("ID_site","Bloc", "Coupe", "Exclos", "Analogue", "Diam_bas", "Diam_api",
#                "Longueur", "Classe")]
# 
# colnames(test3)[1]<-"sites"
# 
# test3<-test2[order(test2$X),]
# 
# test4<-merge()
# 
# test2<- test %>% arrange(sites)
# write.csv(test3,"cwd_mesures.csv",row.names = F)
# 
test2<- read.csv("cov_jday.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")

test3<- read.csv("cov_precip.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")



######

id.coll<- read.csv("ID_COLL.csv", header = TRUE, stringsAsFactors = TRUE)

data.site<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE)

# sum.coll<- table(id.coll$ord.fam)
# sum.coll<- as.data.frame(sum.coll)

#colnames(data.site)[1]<-c("site")

# ajouter les information des sites pour chaque collemboles

#id.coll<- merge(x=id.coll, y=data.site, by="site", all.x = TRUE)

table(id.coll$order)

par(mar = c(7.5, 5, 4, 2.1))
plot(table(id.coll$family), las= 2, cex.axis = 1, ylab= "Fréquences", 
     main= "Fréquences des familles de collemboles", ylim = c(0,200))

freq_coll<-as.data.frame( table(id.coll$family))

text(x = 1 ,
     y = freq_coll[1,2],
     labels = "6",
     cex = 1, pos = 3)

text(x = 2 ,
     y = freq_coll[2,2],
     labels = "62",
     cex = 1, pos = 3)

text(x = 3 ,
     y = freq_coll[3,2],
     labels = "92",
     cex = 1, pos = 3)

text(x = 4 ,
     y = freq_coll[4,2],
     labels = "182",
     cex = 1, pos = 3)

text(x = 5 ,
     y = freq_coll[5,2],
     labels = "6",
     cex = 1, pos = 3)

text(x = 6 ,
     y = freq_coll[6,2],
     labels = "11",
     cex = 1, pos = 3)

text(x = 7 ,
     y = freq_coll[7,2],
     labels = "23",
     cex = 1, pos = 3)

text(x = 8 ,
     y = freq_coll[8,2],
     labels = "33",
     cex = 1, pos = 3)

text(x = 9 ,
     y = freq_coll[9,2],
     labels = "10",
     cex = 1, pos = 3)

text(x = 10 ,
     y = freq_coll[10,2],
     labels = "11",
     cex = 1, pos = 3)
text(x = 11 ,
     y = freq_coll[11,2],
     labels = "8",
     cex = 1, pos = 3)

text(x = 12 ,
     y = freq_coll[12,2],
     labels = "24",
     cex = 1, pos = 3)

# répartition des collemboles en fonctions des traitements

table(id.coll$family, id.coll$Coupe)

table(id.coll$family, id.coll$Bloc)

table(id.coll$family, id.coll$Exclos)

table(id.coll$family, id.coll$Analogue)



freq_coll<-as.data.frame(table(id.coll$family))
freq_coll

par(mar=c(8,5,3,1))
plot(id.coll$family,las = 2, ylim = c(0,230), main = "Dénombrement des collemboles en fonction de la  famille"
     , ylab = "Fréquences")

text(x = 1-.25,
     y = freq_coll[1,2],
     labels = "6",
     cex = 1, pos = 3)

text(x = 2-.10,
     y = freq_coll[2,2],
     labels = "62",
     cex = 1, pos = 3)

text(x = 3+.10,
     y = freq_coll[3,2],
     labels = "92",
     cex = 1, pos = 3)

text(x = 4+.25,
     y = freq_coll[4,2],
     labels = "182",
     cex = 1, pos = 3)

text(x = 6+.70,
     y = freq_coll[6,2],
     labels = "6",
     cex = 1, pos = 3)

text(x = 8-0.10,
     y = freq_coll[7,2],
     labels = "11",
     cex = 1, pos = 3)

text(x = 9+.10,
     y = freq_coll[8,2],
     labels = "23",
     cex = 1, pos = 3)

text(x = 10+.25,
     y = freq_coll[9,2],
     labels = "33",
     cex = 1, pos = 3)

text(x = 11+.5,
     y = freq_coll[10,2],
     labels = "10",
     cex = 1, pos = 3)

text(x = 12+.70,
     y = freq_coll[11,2],
     labels = "11",
     cex = 1, pos = 3)

text(x = 14-.10,
     y = freq_coll[12,2],
     labels = "8",
     cex = 1, pos = 3)

text(x = 15+.15,
     y = freq_coll[13,2],
     labels = "24",
     cex = 1, pos = 3)

######
fam_coll_tab <- table(id.coll$site, id.coll$family)
fam_coll <- as.data.frame.matrix(fam_coll_tab)

data60<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")

det_fam_coll<-as.data.frame(ifelse(fam_coll > 0, 1, 0))

names<-rownames(det_fam_coll)

det_fam_coll$ID<-names

det_fam_coll_m<- merge(x=data60, y=det_fam_coll, by="ID", all.x = TRUE)

det_fam_coll<- det_fam_coll_m[,c("Dicyrtomidae","Entomobryidae", "Hypogastruridae",
                                 "Isotomidae", "Katiannidae", "Neanuridae",
                                 "Neelidae", "Onychiuridae", "Sminthuridae",  
                                 "Sminthurididae", "Tomoceridae", "Tullbergidae" )]

det_fam_coll[is.na(det_fam_coll)]<-0

row.names(det_fam_coll)<- data60$ID

apply(det_fam_coll,2, FUN = sum)/nrow(det_fam_coll)
nrow(det_fam_coll)

######

ord_coll_tab <- table(id.coll$site, id.coll$order)
ord_coll <- as.data.frame.matrix(ord_coll_tab)
ord_coll<- ord_coll[,c("mai","juin","juillet", "aout")]

data60<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

det_ord_coll<-as.data.frame(ifelse(ord_coll > 0, 1, 0))

names<-rownames(det_ord_coll)

det_ord_coll$ID<-names

det_ord_coll_m<- merge(x=data60, y=det_ord_coll, by="ID", all.x = TRUE)

det_ord_coll<- det_ord_coll_m[,c("Entomobryomorpha", "Poduromorpha", "Symphypleona" )]

det_ord_coll[is.na(det_ord_coll)]<-0

apply(det_ord_coll,2, FUN = sum)/nrow(det_ord_coll)
nrow(det_ord_coll)

##### séparation des dataframe pour chaque famille

fam.hypo<- id.coll[id.coll$family =="Hypogastruridae",]
fam.iso<- id.coll[id.coll$family =="Isotomidae",]
fam.ony<- id.coll[id.coll$family =="Onychiuridae",]
fam.tull<- id.coll[id.coll$family =="Tullbergidae",]
fam.dicy<- id.coll[id.coll$family =="Dicyrtomidae",]
fam.ento<- id.coll[id.coll$family =="Entomobryidae",]
fam.kati<- id.coll[id.coll$family =="Katiannidae",]
fam.neel<- id.coll[id.coll$family =="Neelidae",]
fam.nean<- id.coll[id.coll$family =="Neanuridae",]
fam.smind<- id.coll[id.coll$family =="Sminthuridae",]
fam.smindd<- id.coll[id.coll$family =="Sminthurididae",]
fam.tomo<- id.coll[id.coll$family =="Tomoceridae",]

## matrice de detection pour chaque famille


# Hypogastruridae

abun.hypo <- table(fam.hypo$site, fam.hypo$month)
det.hypo<- as.data.frame( ifelse(abun.hypo != 0, 1, 0))
det.hypo<- det.hypo[,c("mai","juin", "juillet", "aout")]

det.hypo$ID<-row.names(det.hypo)
det.hypo.m<- merge(x= data.site,y= det.hypo, by= "ID", all.x = TRUE)
det.hypo<- det.hypo.m[,c("ID","mai","juin", "juillet", "aout")]
det.hypo[is.na(det.hypo)]<-0

write.csv(det.hypo,file = "det_hypo.csv", row.names = FALSE)

# Isotomidae

abun.iso <- table(fam.iso$site, fam.iso$month)
det.iso<- as.data.frame( ifelse(abun.iso != 0, 1, 0))
det.iso<- det.iso[,c("mai","juin", "juillet", "aout")]

det.iso$ID<-row.names(det.iso)
det.iso.m<- merge(x= data.site,y= det.iso, by= "ID", all.x = TRUE)
det.iso<- det.iso.m[,c("ID","mai","juin", "juillet", "aout")]
det.iso[is.na(det.iso)]<-0

write.csv(det.iso,file = "det_iso.csv", row.names = FALSE)

# Onychiuridae

abun.ony <- table(fam.ony$site, fam.ony$month)
det.ony<- as.data.frame( ifelse(abun.ony != 0, 1, 0))
det.ony<- det.ony[,c("mai","juin", "juillet", "aout")]

det.ony$ID<-row.names(det.ony)
det.ony.m<- merge(x= data.site,y= det.ony, by= "ID", all.x = TRUE)
det.ony<- det.ony.m[,c("ID","mai","juin", "juillet", "aout")]
det.ony[is.na(det.ony)]<-0

write.csv(det.ony,file = "det_ony.csv", row.names = FALSE)

# Tullbergidae

abun.tull <- table(fam.tull$site, fam.tull$month)
det.tull<- as.data.frame( ifelse(abun.tull != 0, 1, 0))
det.tull<- det.tull[,c("mai","juin", "juillet", "aout")]

det.tull$ID<-row.names(det.tull)
det.tull.m<- merge(x= data.site,y= det.tull, by= "ID", all.x = TRUE)
det.tull<- det.tull.m[,c("ID","mai","juin", "juillet", "aout")]
det.tull[is.na(det.tull)]<-0

write.csv(det.tull,file = "det_tull.csv", row.names = FALSE)

# Dicyrtomidae

abun.dicy <- table(fam.dicy$site, fam.dicy$month)
det.dicy<- as.data.frame( ifelse(abun.dicy != 0, 1, 0))
det.dicy<- det.dicy[,c("mai","juin", "juillet", "aout")]

det.dicy$ID<-row.names(det.dicy)
det.dicy.m<- merge(x= data.site,y= det.dicy, by= "ID", all.x = TRUE)
det.dicy<- det.dicy.m[,c("ID","mai","juin", "juillet", "aout")]
det.dicy[is.na(det.dicy)]<-0

write.csv(det.dicy,file = "det_dicy.csv", row.names = FALSE)

# Entomobryidae

abun.ento <- table(fam.ento$site, fam.ento$month)
det.ento<- as.data.frame( ifelse(abun.ento != 0, 1, 0))
det.ento<- det.ento[,c("mai","juin", "juillet", "aout")]

det.ento$ID<-row.names(det.ento)
det.ento.m<- merge(x= data.site,y= det.ento, by= "ID", all.x = TRUE)
det.ento<- det.ento.m[,c("ID","mai","juin", "juillet", "aout")]
det.ento[is.na(det.ento)]<-0

write.csv(det.ento,file = "det_ento.csv", row.names = FALSE)

# Katiannidae

abun.kati <- table(fam.kati$site, fam.kati$month)
det.kati<- as.data.frame( ifelse(abun.kati != 0, 1, 0))
det.kati<- det.kati[,c("mai","juin", "juillet", "aout")]

det.kati$ID<-row.names(det.kati)
det.kati.m<- merge(x= data.site,y= det.kati, by= "ID", all.x = TRUE)
det.kati<- det.kati.m[,c("ID","mai","juin", "juillet", "aout")]
det.kati[is.na(det.kati)]<-0

write.csv(det.kati,file = "det_kati.csv", row.names = FALSE)

# Neelidae

abun.neel <- table(fam.neel$site, fam.neel$month)
det.neel<- as.data.frame( ifelse(abun.neel != 0, 1, 0))
det.neel<- det.neel[,c("mai","juin", "juillet", "aout")]

det.neel$ID<-row.names(det.neel)
det.neel.m<- merge(x= data.site,y= det.neel, by= "ID", all.x = TRUE)
det.neel<- det.neel.m[,c("ID","mai","juin", "juillet", "aout")]
det.neel[is.na(det.neel)]<-0

write.csv(det.neel,file = "det_neel.csv", row.names = FALSE)

# Neanuridae

abun.nean <- table(fam.nean$site, fam.nean$month)
det.nean<- as.data.frame( ifelse(abun.nean != 0, 1, 0))
det.nean<- det.nean[,c("mai","juin", "juillet", "aout")]

det.nean$ID<-row.names(det.nean)
det.nean.m<- merge(x= data.site,y= det.nean, by= "ID", all.x = TRUE)
det.nean<- det.nean.m[,c("ID","mai","juin", "juillet", "aout")]
det.nean[is.na(det.nean)]<-0

write.csv(det.nean,file = "det_nean.csv", row.names = FALSE)

# Sminthuridae

abun.smind <- table(fam.smind$site, fam.smind$month)
det.smind<- as.data.frame( ifelse(abun.smind != 0, 1, 0))
det.smind<- det.smind[,c("mai","juin", "juillet", "aout")]

det.smind$ID<-row.names(det.smind)
det.smind.m<- merge(x= data.site,y= det.smind, by= "ID", all.x = TRUE)
det.smind<- det.smind.m[,c("ID","mai","juin", "juillet", "aout")]
det.smind[is.na(det.smind)]<-0

write.csv(det.smind,file = "det_smind.csv", row.names = FALSE)

# Sminthurididae

abun.smindd <- table(fam.smindd$site, fam.smindd$month)
det.smindd<- as.data.frame( ifelse(abun.smindd != 0, 1, 0))
det.smindd<- det.smindd[,c("mai","juin", "juillet", "aout")]

det.smindd$ID<-row.names(det.smindd)
det.smindd.m<- merge(x= data.site,y= det.smindd, by= "ID", all.x = TRUE)
det.smindd<- det.smindd.m[,c("ID","mai","juin", "juillet", "aout")]
det.smindd[is.na(det.smindd)]<-0

write.csv(det.smindd,file = "det_smindd.csv", row.names = FALSE)

# Tomoceridae

abun.tomo <- table(fam.tomo$site, fam.tomo$month)
det.tomo<- as.data.frame( ifelse(abun.tomo != 0, 1, 0))
det.tomo<- det.tomo[,c("mai","juin", "juillet", "aout")]

det.tomo$ID<-row.names(det.tomo)
det.tomo.m<- merge(x= data.site,y= det.tomo, by= "ID", all.x = TRUE)
det.tomo<- det.tomo.m[,c("ID","mai","juin", "juillet", "aout")]
det.tomo[is.na(det.tomo)]<-0

write.csv(det.tomo,file = "det_tomo.csv", row.names = FALSE)


## construction de dataframe de detection des trois ordres de collemboles

ord.podu<- id.coll[id.coll$order =="Poduromorpha",]
ord.ento<- id.coll[id.coll$order =="Entomobryomorpha",]
ord.symp<- id.coll[id.coll$order =="Symphypleona",]

# Poduromorpha

abun.podu <- table(ord.podu$site, ord.podu$month)
det.podu<- as.data.frame( ifelse(abun.podu != 0, 1, 0))
det.podu<- det.podu[,c("mai","juin", "juillet", "aout")]

det.podu$ID<-row.names(det.podu)
det.podu.m<- merge(x= data.site,y= det.podu, by= "ID", all.x = TRUE)
det.podu<- det.podu.m[,c("ID","mai","juin", "juillet", "aout")]
det.podu[is.na(det.podu)]<-0

write.csv(det.podu,file = "det_podu_ord.csv", row.names = FALSE)

# Entomobryomorpha

abun.ento.ord <- table(ord.ento$site, ord.ento$month)
det.ento.ord<- as.data.frame( ifelse(abun.ento.ord != 0, 1, 0))
det.ento.ord<- det.ento.ord[,c("mai","juin", "juillet", "aout")]

det.ento.ord$ID<-row.names(det.ento.ord)
det.ento.ord.m<- merge(x= data.site,y= det.ento.ord, by= "ID", all.x = TRUE)
det.ento.ord<- det.ento.ord.m[,c("ID","mai","juin", "juillet", "aout")]
det.ento.ord[is.na(det.ento.ord)]<-0

write.csv(det.ento.ord,file = "det_ento_ord.csv", row.names = FALSE)


# Symphypleona

abun.symp <- table(ord.symp$site, ord.symp$month)
det.symp<- as.data.frame( ifelse(abun.symp != 0, 1, 0))
det.symp<- det.symp[,c("mai","juin", "juillet", "aout")]

det.symp$ID<-row.names(det.symp)
det.symp.m<- merge(x= data.site,y= det.symp, by= "ID", all.x = TRUE)
det.symp<- det.symp.m[,c("ID","mai","juin", "juillet", "aout")]
det.symp[is.na(det.symp)]<-0

write.csv(det.symp,file = "det_symp_ord.csv", row.names = FALSE)




######### REGROUPEMENT DES COLLEMBOLES PAR TRAIT ECOLOGIQUE #################

# colonne Micro-habitats (MH)
id.coll$MH[id.coll$family == "Tomoceridae" | id.coll$family == "Dicyrtomidae" | 
             id.coll$family == "Entomobryidae"] <- "EPI"

id.coll$MH[id.coll$family == "Sminthuridae" | id.coll$family == "Katiannidae"] <- "EPI_HEMI"

id.coll$MH[id.coll$family == "Sminthurididae" | id.coll$family == "Hypogastruridae"] <- "HEMI"

id.coll$MH[id.coll$family == "Hypogastruridae" | id.coll$family == "Onychiuridae" | 
             id.coll$family == "Neanuridae"| id.coll$family == "Isotomidae"] <- "HEMI-EU"

id.coll$MH[id.coll$family == "Tullbergidae" | id.coll$family == "Neelidae"] <- "EU"

# dataframe par MH

MH.EPI<- id.coll[id.coll$MH =="EPI",]
MH.EPI.HEMI<- id.coll[id.coll$MH =="EPI_HEMI",]
MH.HEMI<- id.coll[id.coll$MH =="HEMI",]
MH.HEMI.EU<- id.coll[id.coll$MH =="HEMI-EU",]
MH.EU<- id.coll[id.coll$MH =="EU",]

# matrice de detectio par MH

#EPI
abun.epi <- table(MH.EPI$site, MH.EPI$month)
det.epi<- as.data.frame( ifelse(abun.epi != 0, 1, 0))
det.epi<- det.epi[,c("mai","juin", "juillet", "aout")]

det.epi$ID<-row.names(det.epi)
det.epi.m<- merge(x= data.site,y= det.epi, by= "ID", all.x = TRUE)
det.epi<- det.epi.m[,c("ID","mai","juin", "juillet", "aout")]
det.epi[is.na(det.epi)]<-0

write.csv(det.epi,file = "det_epi.csv", row.names = FALSE)


#EPI-HEMI
abun.epi.hemi <- table(MH.EPI.HEMI$site, MH.EPI.HEMI$month)
det.epi.hemi<- as.data.frame( ifelse(abun.epi.hemi != 0, 1, 0))
det.epi.hemi<- det.epi.hemi[,c("mai","juin", "juillet", "aout")]

det.epi.hemi$ID<-row.names(det.epi.hemi)
det.epi.hemi.m<- merge(x= data.site,y= det.epi.hemi, by= "ID", all.x = TRUE)
det.epi.hemi<- det.epi.hemi.m[,c("ID","mai","juin", "juillet", "aout")]
det.epi.hemi[is.na(det.epi.hemi)]<-0

write.csv(det.epi.hemi,file = "det_epi_hemi.csv", row.names = FALSE)


#HEMI
abun.hemi <- table(MH.HEMI$site, MH.HEMI$month)
det.hemi<- as.data.frame( ifelse(abun.hemi != 0, 1, 0))
det.hemi<- det.hemi[,c("mai","juin", "juillet", "aout")]

det.hemi$ID<-row.names(det.hemi)
det.hemi.m<- merge(x= data.site,y= det.hemi, by= "ID", all.x = TRUE)
det.hemi<- det.hemi.m[,c("ID","mai","juin", "juillet", "aout")]
det.hemi[is.na(det.hemi)]<-0

write.csv(det.hemi,file = "det_hemi.csv", row.names = FALSE)


# #HEMI-EU
# abun.hemi.eu<- table(MH.HEMI$site, MH.HEMI$month)
# det.hemi<- as.data.frame( ifelse(abun.hemi != 0, 1, 0))
# det.hemi<- det.hemi[,c("mai","juin", "juillet", "aout")]
# 
# det.hemi$ID<-row.names(det.hemi)
# det.hemi.m<- merge(x= data.site,y= det.hemi, by= "ID", all.x = TRUE)
# det.hemi<- det.hemi.m[,c("ID","mai","juin", "juillet", "aout")]
# det.hemi[is.na(det.hemi)]<-0
# 
# write.csv(det.hemi,file = "det_hemi.csv", row.names = FALSE)