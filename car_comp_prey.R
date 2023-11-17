# positionnement

setwd(dir = "/Users/williamdevos/Documents/Maitrise/Data")

# importation donnée des carabes

id.car<- read.csv("ID_CAR.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")
id.car$gen.sp<-paste(id.car$genus,id.car$species)

# création data frame pour informatiuon de l'histogramme

hist.car<- as.data.frame(sort(unique(id.car$gen.sp)))
colnames(hist.car)<-"gen.sp"

hist.car$Min<- NA
hist.car$Min<- c(7.1, 5.8, 7.6, 6.1, 5.3, 4.6, 
                 8.3, 14.4, 9.8, 8.5, 8.7, 10.0, 7.0, 
                 8.7, 9.3, 9.1, 9.3, 5.1, 9.6, 7.2, 
                 12.7, 11.0, 9.6, 9.6, 15.3, 10.7, 10.3,
                 12.5, 8.8, 4.0)

hist.car$Max<- NA
hist.car$Max<- c(9.2, 7.4, 8.0, 7.6, 6.1, 4.9, 10.6, 16.0, 11.4, 12.7, 
                 9.0, 16.8, 8.3, 9.0, 15.0, 13.8, 13.8, 5.9, 13.1, 8.5, 
                 18.0, 14.0, 12.9, 12.1, 18.2, 13.4, 13.0, 13.8, 11.3, 4.7)

hist.car$Mean<- (hist.car$Min + hist.car$Max)/2 

car2<- merge(id.car, hist.car, by = "gen.sp", all.x = TRUE)




# hist.car2 <- hist.car[order(hist.car$Mean),]
# plot 
hist(car2$Mean, main = "Fréquence des carabes en fonction de la longueur (mm)",
     xlab = "Longueur (mm)",
     ylab = "Fréquence")

abline(v = 8, col = "red", lwd = 2)
abline(v = 12, col = "red", lwd = 2)

xvals <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30)

segments(x0 = xvals, 
         x1 = xvals,
         y0 = hist.car$Min, 
         y1 = hist.car$Max,
         lwd = 2)


# catégorisation des carabes en groupe roie ou competiteur

car2$size<- NA

car2$size <- ifelse(car2$Mean <= 8.3, "prey", "competitor") 

anyNA(car2$size)

# matrice de detection pour groupe de carabes divisé par taille

car.prey<- car2[car2$size == "prey",]
car.comp<- car2[car2$size == "competitor",]


# Création des matrices de détéction pour chaque guilde
data60<- read.csv("Data_60.csv", header = TRUE, stringsAsFactors = TRUE, sep = ",")

# carabes = proie
abun_prey<- table(car.prey$site,car.prey$month)
det_prey<- as.data.frame( ifelse(abun_prey != 0, 1, 0))
det_prey<- det_prey[,c("mai","juin","juillet","aout")]

site_prey<-rownames(det_prey)
det_prey$ID<-site_prey
det_prey_m<- merge(x=data60, y=det_prey, by="ID", all.x = TRUE)
det_prey<- det_prey_m[,c("ID","mai","juin","juillet","aout")]
det_prey[is.na(det_prey)]<-0

write.csv(det_prey,file = "car_det_prey.csv", row.names = FALSE)

# carabes = compétiteur

abun_comp<- table(car.comp$site,car.comp$month)
det_comp<- as.data.frame( ifelse(abun_comp != 0, 1, 0))
det_comp<- det_comp[,c("mai","juin","juillet","aout")]

site_comp<-rownames(det_comp)
det_comp$ID<-site_comp
det_comp_m<- merge(x=data60, y=det_comp, by="ID", all.x = TRUE)
det_comp<- det_comp_m[,c("ID","mai","juin","juillet","aout")]
det_comp[is.na(det_comp)]<-0

write.csv(det_comp,file = "car_det_comp.csv", row.names = FALSE)

# nombre de site avec un detection

car.comp$sum<- rowSums(car.comp)
car.comp$det<-ifelse(car.comp$sum == 0,0,1)
