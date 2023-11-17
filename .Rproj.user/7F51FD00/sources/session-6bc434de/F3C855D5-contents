# données brutes famille carabes

#positionnement
setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

# importation du data frame d'identification de carabes

ID_car<- read.csv("ID_CAR.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

#frequence de carabes par sp.

ID_car$gen.sp<-paste(ID_car$genus,ID_car$species)

par(mar = c(9, 5, 4, 2.1))
plot(table(ID_car$gen.sp), las= 2, cex.axis = .75, ylab= "Fréquences", 
     main= "Fréquence des espèces de carabes", ylim = c(0,40))

freq_car<-as.data.frame( table(ID_car$gen.sp))

text(x = 1 ,
     y = freq_car[1,2],
     labels = "8",
     cex = 1, pos = 3)

text(x = 2 ,
     y = freq_car[2,2],
     labels = "3",
     cex = 1, pos = 3)

text(x = 3 ,
     y = freq_car[3,2],
     labels = "2",
     cex = 1, pos = 3)

text(x = 4 ,
     y = freq_car[4,2],
     labels = "3",
     cex = 1, pos = 3)

text(x = 5 ,
     y = freq_car[5,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 6 ,
     y = freq_car[6,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 7 ,
     y = freq_car[7,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 8 ,
     y = freq_car[8,2],
     labels = "2",
     cex = 1, pos = 3)

text(x = 9 ,
     y = freq_car[9,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 10 ,
     y = freq_car[10,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 11 ,
     y = freq_car[11,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 12 ,
     y = freq_car[12,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 13 ,
     y = freq_car[13,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 14 ,
     y = freq_car[14,2],
     labels = "5",
     cex = 1, pos = 3)

text(x = 15 ,
     y = freq_car[15,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 16 ,
     y = freq_car[16,2],
     labels = "2",
     cex = 1, pos = 3)

text(x = 17 ,
     y = freq_car[17,2],
     labels = "3",
     cex = 1, pos = 3)

text(x = 18 ,
     y = freq_car[18,2],
     labels = "7",
     cex = 1, pos = 3)

text(x = 19 ,
     y = freq_car[19,2],
     labels = "27",
     cex = 1, pos = 3)

text(x = 20 ,
     y = freq_car[20,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 21 ,
     y = freq_car[21,2],
     labels = "32",
     cex = 1, pos = 3)

text(x = 22 ,
     y = freq_car[22,2],
     labels = "3",
     cex = 1, pos = 3)

text(x = 23 ,
     y = freq_car[23,2],
     labels = "10",
     cex = 1, pos = 3)

text(x = 24 ,
     y = freq_car[24,2],
     labels = "18",
     cex = 1, pos = 3)

text(x = 25 ,
     y = freq_car[25,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 26 ,
     y = freq_car[26,2],
     labels = "25",
     cex = 1, pos = 3)

text(x = 27 ,
     y = freq_car[27,2],
     labels = "6",
     cex = 1, pos = 3)

text(x = 28 ,
     y = freq_car[28,2],
     labels = "1",
     cex = 1, pos = 3)

text(x = 29 ,
     y = freq_car[29,2],
     labels = "20",
     cex = 1, pos = 3)

text(x = 30 ,
     y = freq_car[30,2],
     labels = "1",
     cex = 1, pos = 3)





## DEUX GUILDES
# création de la colonne MH (microhabitat) pour  2 guildes (foret, ouvert)

ID_car$MH2<- NA

ID_car$MH2[ID_car$gen.sp == "agonum palustre"
          | ID_car$gen.sp == "agonum retractum"
          | ID_car$gen.sp == "bradycellus lugubris"
          | ID_car$gen.sp == "bradycellus semipubescens"
          | ID_car$gen.sp == "calathus gregarius"
          | ID_car$gen.sp == "harpalus providens"
          | ID_car$gen.sp == "loricera pilicornis"
          | ID_car$gen.sp == "platynus decentis"
          | ID_car$gen.sp == "pseudamara arenaria"
          | ID_car$gen.sp == "pterostichus adstrictus"
          | ID_car$gen.sp == "pterostichus coracinus"
          | ID_car$gen.sp == "pterostichus diligendus"
          | ID_car$gen.sp == "pterostichus mutus"
          | ID_car$gen.sp == "pterostichus pensylvanicus"
          | ID_car$gen.sp == "pterostichus punctatissimus"
          | ID_car$gen.sp == "pterostichus tristis"
          | ID_car$gen.sp == "sphaeroderus canadensis"
          | ID_car$gen.sp == "sphaeroderus nitidicollis"
          | ID_car$gen.sp == "synuchus impunctatus"
          | ID_car$gen.sp == "trechus apicalis"] <- "forest"

ID_car$MH2[ID_car$gen.sp == "agonum affine"
          | ID_car$gen.sp == "agonum punctiforme"
          | ID_car$gen.sp == "chlaenius sericeus"
          | ID_car$gen.sp == "harpalus erythropus"
          | ID_car$gen.sp == "harpalus faunus"
          | ID_car$gen.sp == "harpalus rufipes"
          | ID_car$gen.sp == "notiobia terminata"
          | ID_car$gen.sp == "patrobus longicornis"
          | ID_car$gen.sp == "poecilus lucublandus"
          | ID_car$gen.sp == "pterostichus commutabilis" ] <- "open"

anyNA(ID_car$MH2)

# Création data.frame pour chaque guilde

det_for2<- ID_car[ID_car$MH2 == "forest",]
det_open2<- ID_car[ID_car$MH2 == "open",]

# Création des matrices de détéction pour chaque guilde
data60<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")

# Forest
site_for2<- table(det_for2$site,det_for2$month)
site_for2_det<- as.data.frame( ifelse(site_for2 != 0, 1, 0))
site_for2_det<- site_for2_det[,c("mai","juin","juillet","aout")]

names_for2<-rownames(site_for2_det)
site_for2_det$ID<-names_for2
site_for2_det_m<- merge(x=data60, y=site_for2_det, by="ID", all.x = TRUE)
site_for2_det<- site_for2_det_m[,c("ID","mai","juin","juillet","aout")]
site_for2_det[is.na(site_for2_det)]<-0

write.csv(site_for2_det,file = "car_det_for_2.csv", row.names = FALSE)


# Open
site_open2<- table(det_open2$site,det_open2$month)
site_open2_det<- as.data.frame( ifelse(site_open2 != 0, 1, 0))
site_open2_det<- site_open2_det[,c("mai","juin","juillet","aout")]

names_open2<-rownames(site_open2_det)
site_open2_det$ID<-names_open2
site_open2_det_m<- merge(x=data60, y=site_open2_det, by="ID", all.x = TRUE)
site_open2_det<- site_open2_det_m[,c("ID","mai","juin","juillet","aout")]
site_open2_det[is.na(site_open2_det)]<-0

write.csv(site_open2_det,file = "car_det_open_2.csv", row.names = FALSE)





## TROIS GUILDES
# création de la colonne MH (microhabitat) pour guildes (foret, ouvert, aquatique)

ID_car$MH3<- NA

ID_car$MH3[ID_car$gen.sp == "agonum palustre"          
           | ID_car$gen.sp == "agonum retractum"
           | ID_car$gen.sp == "calathus gregarius"
           | ID_car$gen.sp == "harpalus providens"
           | ID_car$gen.sp == "pseudamara arenaria"
           | ID_car$gen.sp == "pterostichus adstrictus"
           | ID_car$gen.sp == "pterostichus coracinus"
           | ID_car$gen.sp == "pterostichus diligendus"
           | ID_car$gen.sp == "pterostichus mutus"
           | ID_car$gen.sp == "pterostichus pensylvanicus"
           | ID_car$gen.sp == "pterostichus punctatissimus"
           | ID_car$gen.sp == "pterostichus tristis" 
           | ID_car$gen.sp == "sphaeroderus canadensis" 
           | ID_car$gen.sp == "sphaeroderus nitidicollis" 
           | ID_car$gen.sp == "synuchus impunctatus" 
           | ID_car$gen.sp == "trechus apicalis"] <- "forest"

ID_car$MH3[ID_car$gen.sp == "agonum punctiforme"
           | ID_car$gen.sp == "harpalus erythropus"
           | ID_car$gen.sp == "harpalus faunus"
           | ID_car$gen.sp == "harpalus rufipes"
           | ID_car$gen.sp == "notiobia terminata"
           | ID_car$gen.sp == "poecilus lucublandus"] <- "open"

ID_car$MH3[ID_car$gen.sp == "agonum affine"
           | ID_car$gen.sp == "bradycellus lugubris"
           | ID_car$gen.sp == "bradycellus semipubescens"
           | ID_car$gen.sp == "chlaenius sericeus"
           | ID_car$gen.sp == "loricera pilicornis"
           | ID_car$gen.sp == "patrobus longicornis"
           | ID_car$gen.sp == "platynus decentis"
           | ID_car$gen.sp == "pterostichus commutabilis"] <- "aqua"

anyNA(ID_car$MH3)

# Création data.frame pour chaque guilde

det_for3<- ID_car[ID_car$MH3 == "forest",]
det_open3<- ID_car[ID_car$MH3 == "open",]
det_aqua3<- ID_car[ID_car$MH3 == "aqua",]


# Création des matrices de détéction pour chaque guilde
data60<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")

# Forest
site_for3<- table(det_for3$site,det_for3$month)
site_for3_det<- as.data.frame( ifelse(site_for3 != 0, 1, 0))
site_for3_det<- site_for3_det[,c("mai","juin","juillet","aout")]

names_for3<-rownames(site_for3_det)
site_for3_det$ID<-names_for3
site_for3_det_m<- merge(x=data60, y=site_for3_det, by="ID", all.x = TRUE)
site_for3_det<- site_for3_det_m[,c("ID","mai","juin","juillet","aout")]
site_for3_det[is.na(site_for3_det)]<-0

write.csv(site_for3_det,file = "car_det_for_3.csv", row.names = FALSE)


# Open
site_open3<- table(det_open3$site,det_open3$month)
site_open3_det<- as.data.frame( ifelse(site_open3 != 0, 1, 0))
site_open3_det<- site_open3_det[,c("mai","juin","juillet","aout")]

names_open3<-rownames(site_open3_det)
site_open3_det$ID<-names_open3
site_open3_det_m<- merge(x=data60, y=site_open3_det, by="ID", all.x = TRUE)
site_open3_det<- site_open3_det_m[,c("ID","mai","juin","juillet","aout")]
site_open3_det[is.na(site_open3_det)]<-0

write.csv(site_open3_det,file = "car_det_open_3.csv", row.names = FALSE)

# Aquatic
site_aqua3<- table(det_aqua3$site,det_aqua3$month)
site_aqua3_det<- as.data.frame( ifelse(site_aqua3 != 0, 1, 0))
site_aqua3_det<- site_aqua3_det[,c("mai","juin","juillet","aout")]

names_aqua3<-rownames(site_aqua3_det)
site_aqua3_det$ID<-names_aqua3
site_aqua3_det_m<- merge(x=data60, y=site_aqua3_det, by="ID", all.x = TRUE)
site_aqua3_det<- site_aqua3_det_m[,c("ID","mai","juin","juillet","aout")]
site_aqua3_det[is.na(site_aqua3_det)]<-0

write.csv(site_aqua3_det,file = "car_det_aqua_3.csv", row.names = FALSE)



### GUILDE par taille

ID_car$size<- NA

ID_car$size[ID_car$gen.sp == "chlaenius sericeus"
           | ID_car$gen.sp == "harpalus erythropus"
           | ID_car$gen.sp == "harpalus faunus"
           | ID_car$gen.sp == "harpalus rufipes"
           | ID_car$gen.sp == "patrobus longicornis"
           | ID_car$gen.sp == "platynus decentis"
           | ID_car$gen.sp == "poecilus lucublandus"
           | ID_car$gen.sp == "pterostichus adstrictus"
           | ID_car$gen.sp == "pterostichus coracinus"
           | ID_car$gen.sp == "pterostichus diligendus"
           | ID_car$gen.sp == "pterostichus mutus"
           | ID_car$gen.sp == "pterostichus pensylvanicus"
           | ID_car$gen.sp == "pterostichus punctatissimus"
           | ID_car$gen.sp == "pterostichus tristis" 
           | ID_car$gen.sp == "sphaeroderus canadensis" 
           | ID_car$gen.sp == "sphaeroderus nitidicollis"] <- "large"

ID_car$size[ID_car$gen.sp == "agonum affine"
           |ID_car$gen.sp == "agonum palustre"          
           |ID_car$gen.sp == "agonum punctiforme"
           | ID_car$gen.sp == "agonum retractum"
           | ID_car$gen.sp == "bradycellus lugubris"
           | ID_car$gen.sp == "bradycellus semipubescens"
           | ID_car$gen.sp == "calathus gregarius"
           | ID_car$gen.sp == "harpalus providens"
           | ID_car$gen.sp == "loricera pilicornis"
           | ID_car$gen.sp == "notiobia terminata"
           | ID_car$gen.sp == "pseudamara arenaria"
           | ID_car$gen.sp == "pterostichus commutabilis"
           | ID_car$gen.sp == "synuchus impunctatus" 
           | ID_car$gen.sp == "trechus apicalis"] <- "small"

anyNA(ID_car$size)

# Création data.frame pour chaque guilde

det_large<- ID_car[ID_car$size == "large",]
det_small<- ID_car[ID_car$size == "small",]


# Création des matrices de détéction pour chaque guilde

# Large
site_large<- table(det_large$site,det_large$month)
site_large_det<- as.data.frame( ifelse(site_large != 0, 1, 0))
site_large_det<- site_large_det[,c("mai","juin","juillet","aout")]

names_large<-rownames(site_large_det)
site_large_det$ID<-names_large
site_large_det_m<- merge(x=data60, y=site_large_det, by="ID", all.x = TRUE)
site_large_det<- site_large_det_m[,c("ID","mai","juin","juillet","aout")]
site_large_det[is.na(site_large_det)]<-0

write.csv(site_large_det,file = "car_det_large.csv", row.names = FALSE)

# Small
site_small<- table(det_small$site,det_small$month)
site_small_det<- as.data.frame( ifelse(site_small != 0, 1, 0))
site_small_det<- site_small_det[,c("mai","juin","juillet","aout")]

names_small<-rownames(site_small_det)
site_small_det$ID<-names_small
site_small_det_m<- merge(x=data60, y=site_small_det, by="ID", all.x = TRUE)
site_small_det<- site_small_det_m[,c("ID","mai","juin","juillet","aout")]
site_small_det[is.na(site_small_det)]<-0

write.csv(site_small_det,file = "car_det_small.csv", row.names = FALSE)


## GUILD PAR TAILLE ( 3 guildes)

ID_car$size3<- NA

ID_car$size3[ID_car$gen.sp == "chlaenius sericeus"
            | ID_car$gen.sp == "patrobus longicornis"
            | ID_car$gen.sp == "pterostichus coracinus"
            | ID_car$gen.sp == "pterostichus diligendus"
            | ID_car$gen.sp == "pterostichus punctatissimus"
            | ID_car$gen.sp == "sphaeroderus nitidicollis"] <- "grand"

          
ID_car$size3[ID_car$gen.sp == "calathus gregarius"
             | ID_car$gen.sp == "harpalus erythropus"
             | ID_car$gen.sp == "harpalus faunus"
             | ID_car$gen.sp == "harpalus providens"
             | ID_car$gen.sp == "harpalus rufipes"
             | ID_car$gen.sp == "notiobia terminata"
             | ID_car$gen.sp == "platynus decentis"
             | ID_car$gen.sp == "poecilus lucublandus"
             | ID_car$gen.sp == "pterostichus adstrictus"
             | ID_car$gen.sp == "pterostichus mutus"
             | ID_car$gen.sp == "pterostichus pensylvanicus"
             | ID_car$gen.sp == "pterostichus tristis" 
             | ID_car$gen.sp == "sphaeroderus canadensis" 
             | ID_car$gen.sp == "synuchus impunctatus"] <- "moyen"


ID_car$size3[ID_car$gen.sp == "agonum affine"
            |ID_car$gen.sp == "agonum palustre"          
            |ID_car$gen.sp == "agonum punctiforme"
            | ID_car$gen.sp == "agonum retractum"
            | ID_car$gen.sp == "bradycellus lugubris"
            | ID_car$gen.sp == "bradycellus semipubescens"
            | ID_car$gen.sp == "loricera pilicornis"
            | ID_car$gen.sp == "pseudamara arenaria"
            | ID_car$gen.sp == "pterostichus commutabilis"
            | ID_car$gen.sp == "trechus apicalis"] <- "petit"

anyNA(ID_car$size3)

# Création data.frame pour chaque guilde

det_grand<- ID_car[ID_car$size3 == "grand",]
det_moyen<- ID_car[ID_car$size3 == "moyen",]
det_petit<- ID_car[ID_car$size3 == "petit",]


# Création des matrices de détéction pour chaque guilde

# Grand
site_grand<- table(det_grand$site,det_grand$month)
site_grand_det<- as.data.frame( ifelse(site_grand != 0, 1, 0))
site_grand_det<- site_grand_det[,c("mai","juin","juillet","aout")]

names_grand<-rownames(site_grand_det)
site_grand_det$ID<-names_grand
site_grand_det_m<- merge(x=data60, y=site_grand_det, by="ID", all.x = TRUE)
site_grand_det<- site_grand_det_m[,c("ID","mai","juin","juillet","aout")]
site_grand_det[is.na(site_grand_det)]<-0

write.csv(site_grand_det,file = "car_det_grand.csv", row.names = FALSE)

# Moyen
site_moyen<- table(det_moyen$site,det_moyen$month)
site_moyen_det<- as.data.frame( ifelse(site_moyen != 0, 1, 0))
site_moyen_det<- site_moyen_det[,c("mai","juin","juillet","aout")]

names_moyen<-rownames(site_moyen_det)
site_moyen_det$ID<-names_moyen
site_moyen_det_m<- merge(x=data60, y=site_moyen_det, by="ID", all.x = TRUE)
site_moyen_det<- site_moyen_det_m[,c("ID","mai","juin","juillet","aout")]
site_moyen_det[is.na(site_moyen_det)]<-0

write.csv(site_moyen_det,file = "car_det_moyen.csv", row.names = FALSE)

# Petit
site_petit<- table(det_petit$site,det_petit$month)
site_petit_det<- as.data.frame( ifelse(site_petit != 0, 1, 0))
site_petit_det<- site_petit_det[,c("mai","juin","juillet","aout")]

names_petit<-rownames(site_petit_det)
site_petit_det$ID<-names_petit
site_petit_det_m<- merge(x=data60, y=site_petit_det, by="ID", all.x = TRUE)
site_petit_det<- site_petit_det_m[,c("ID","mai","juin","juillet","aout")]
site_petit_det[is.na(site_petit_det)]<-0

write.csv(site_petit_det,file = "car_det_petit.csv", row.names = FALSE)


# # diviser les guilde en deux MH
# 
# #MH forestier
# data_for<- ID_car[ ID_car$MH == "forest",]
# str(data_for)
# 
# abun_for<- table(data_for$site,data_for$month)
# det_for<- as.data.frame(ifelse(abun_for != 0,1,0))
# colnames(det_for)<-c("mai","juin", "juillet","aout")
# 
# names.for<-rownames(det_for)
# det_for$ID<-names.for
# 
# det_for_m<- merge(x=data60, y=det_for, by="ID", all.x = TRUE)
# det_for<- det_for_m[,c("mai","juin","juillet","aout")]
# det_for[is.na(det_for)]<-0
# rownames(det_for)<-data60$ID
# 
# # write.csv(det_for, file = "car_guild_for.csv")


# #MH open
# data_open<- ID_car[ ID_car$MH == "open",]
# str(data_open)
# 
# abun_open<- table(data_open$site,data_open$month)
# det_open<- as.data.frame(ifelse(abun_open != 0,1,0))
# colnames(det_open)<-c("mai","juin", "juillet","aout")
# 
# names.open<-rownames(det_open)
# det_open$ID<-names.open
# 
# det_open_m<- merge(x=data60, y=det_open, by="ID", all.x = TRUE)
# det_open<- det_open_m[,c("mai","juin","juillet","aout")]
# det_open[is.na(det_open)]<-0
# rownames(det_open)<-data60$ID
# 
# write.csv(det_open, file = "car_guild_open.csv")






# # création de la colonne Habitat pour guildes (foret, ouvert, aquatique)
# 
# 
# ID_car$habitat<- NA
# 
# ID_car$habitat[ID_car$gen.sp == "agonum affine" | ID_car$gen.sp == "agonum palustre"
#                | ID_car$gen.sp == "chlaenius sericeus" | ID_car$gen.sp == "patrobus longicornis"
#                | ID_car$gen.sp == "pterostichus commutabilis" | ID_car$gen.sp == "loricera pilicornis"
#                | ID_car$gen.sp == "bradycellus lugubris" | ID_car$gen.sp == "bradycellus semipubescens"] <- "aquatique"
# 
# ID_car$habitat[ID_car$gen.sp == "pterostichus tristis" | ID_car$gen.sp == "agonum retractum"
#                | ID_car$gen.sp == "pseudamara arenaria" | ID_car$gen.sp == "pterostichus mutus"
#                | ID_car$gen.sp == "pterostichus pensylvanicus" | ID_car$gen.sp == "pterostichus coracinus"
#                | ID_car$gen.sp == "synuchus impunctatus" | ID_car$gen.sp == "pterostichus adstrictus" 
#                | ID_car$gen.sp == "pterostichus punctatissimus" | ID_car$gen.sp == "pterostichus diligendus"
#                | ID_car$gen.sp == "calathus gregarius" | ID_car$gen.sp == "sphaeroderus canadensis" | ID_car$gen.sp == "platynus decentis"
#                | ID_car$gen.sp == "sphaeroderus nitidicollis" | ID_car$gen.sp == "trechus apicalis"] <- "forestier"
# 
# ID_car$habitat[ID_car$gen.sp == "harpalus providens" | ID_car$gen.sp == "harpalus erythropus"
#                | ID_car$gen.sp == "harpalus faunus" | ID_car$gen.sp == "harpalus rufipes"
#                | ID_car$gen.sp == "notiobia terminata"| ID_car$gen.sp == "poecilus lucublandus" 
#                | ID_car$gen.sp == "agonum punctiforme"] <- "ouvert"
# 
# anyNA(ID_car$habitat)

#####
data60<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")

site_hab<- table(ID_car$site,ID_car$habitat)

site_hab_det<- as.data.frame( ifelse(site_hab != 0, 1, 0))

names<-rownames(site_hab_det)

site_hab_det$ID<-names

site_hab_det_m<- merge(x=data60, y=site_hab_det, by="ID", all.x = TRUE)

site_hab_det<- site_hab_det_m[,c("aquatique","forestier","ouvert")]

site_hab_det[is.na(site_hab_det)]<-0


######
apply(site_hab_det,2, FUN = sum)/nrow(site_hab_det)

# diviser le data frame des identifications en trois dataframe selon les habitats

#habitat aquatique
data_aqua<- ID_car[ ID_car$habitat == "aquatique",]
str(data_aqua)

abun_aqua<- table(data_aqua$site,data_aqua$month)
det_aqua<- as.data.frame(ifelse(abun_aqua != 0,1,0))
colnames(det_aqua)<-c("mai","juin", "juillet","aout")

names.aqua<-rownames(det_aqua)
det_aqua$ID<-names.aqua

det_aqua_m<- merge(x=data60, y=det_aqua, by="ID", all.x = TRUE)
det_aqua<- det_aqua_m[,c("mai","juin","juillet","aout")]
det_aqua[is.na(det_aqua)]<-0
rownames(det_aqua)<-data60$ID

#write.csv(det_aqua, file = "detect_aquatique.csv")

#habitat forestier
data_for<- ID_car[ ID_car$habitat == "forestier",]
str(data_for)

abun_for<- table(data_for$site,data_for$month)
det_for<- as.data.frame(ifelse(abun_for != 0,1,0))
colnames(det_for)<-c("mai","juin", "juillet","aout")

names.for<-rownames(det_for)
det_for$ID<-names.for

det_for_m<- merge(x=data60, y=det_for, by="ID", all.x = TRUE)
det_for<- det_for_m[,c("mai","juin","juillet","aout")]
det_for[is.na(det_for)]<-0
rownames(det_for)<-data60$ID

#write.csv(det_for, file = "detect_forestier.csv")

#habitat ouvert
data_open<- ID_car[ ID_car$habitat == "ouvert",]
str(data_open)

abun_open<- table(data_open$site,data_open$month)
det_open<- as.data.frame(ifelse(abun_open != 0,1,0))
colnames(det_open)<-c("mai","juin", "juillet","aout")

names.open<-rownames(det_open)
det_open$ID<-names.open

det_open_m<- merge(x=data60, y=det_open, by="ID", all.x = TRUE)
det_open<- det_open_m[,c("mai","juin","juillet","aout")]
det_open[is.na(det_open)]<-0
rownames(det_open)<-data60$ID

#write.csv(det_open, file = "detect_ouvert.csv")
