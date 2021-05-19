setwd ("C:/Users/szabo/OneDrive/Dokumentumok/Károli 2020-21-2/R/RStudo/Beadandó")
data <- read.csv ("master.csv")
data
data$country <- data$ď.żcountry

library(ggplot2)

#1.abra
idosor <- aggregate (suicides.100k.pop ~ country.year,data=data, mean, na.rm=T)
idosor_alt <- aggregate(suicides.100k.pop ~ country+year, data=data, mean, na.rm=T)
idosor_alt

png(file="idosor.png", width = 2048, height = 768)
ggplot(idosor_alt, aes(x = year, y = suicides.100k.pop, colour = country)) + geom_line() + scale_color_discrete(name="ország") + ggtitle("Idősor") + xlab ("év") + ylab ("öngyilkosság/100.000 fő")
dev.off()                                                                                                     

#2.abra
idosor_nemek <- aggregate ( suicides.100k.pop ~ country+year+sex, data = data, mean, na.rm=T)
idosor_nemek

png (file="idosor_ffi.png", width = 2048, height = 768)
ggplot(idosor_nemek[idosor_nemek$sex == "male",], aes(x = year, y = suicides.100k.pop, colour = country )) + geom_line() + scale_color_discrete(name="ország") + ggtitle("Idősor férfiak") + xlab ("év") + ylab ("öngyilkosság/100.000 fő")
dev.off()

#3.abra
png (file="idosor_no.png", width = 2048, height = 768)
ggplot(idosor_nemek[idosor_nemek$sex == "female",], aes(x = year, y = suicides.100k.pop, colour = country )) + geom_line() + scale_color_discrete(name="ország") + ggtitle("Idősor nők") + xlab ("év") + ylab ("öngyilkosság/100.000 fő")
dev.off()

#4.abra

idosor_gen <- aggregate(suicides.100k.pop ~ generation+year, data=data, mean, na.rm=T)                                                                                                    

png(file = "idosor_gen.png", width = 2048, height = 768)
ggplot(idosor_gen, aes(x = year, y= suicides.100k.pop, colour = generation)) + geom_line() + scale_color_discrete(name="generációk") + ggtitle("Idősor generációk") + xlab ("év") + ylab ("öngyilkosság/100.000 fő")
dev.off()

#5.abra
idosor_orszag_ongyilk <- aggregate(suicides.100k.pop ~ country, data = data, mean,  na.rm=T)
idosor_orszag_ongyilk
orsz_sorb <- idosor_orszag_ongyilk[order(-idosor_orszag_ongyilk$suicides.100k.pop),]
orsz_sorb
 
hatorszag <- orsz_sorb$country[1:6]
hatorszag

hat_teljes <- idosor_alt[idosor_alt$country %in% hatorszag,]
hat_teljes
png(file="6orszag_evek.png", width=2048, height = 768)
ggplot(hat_teljes, aes(fill = factor(year), x= country, y=suicides.100k.pop )) + geom_col(position = "dodge")  + scale_fill_discrete(name = "év") + ggtitle("6 ország év") + xlab ("ország") + ylab ("öngyilkosság/100.000 fő")
dev.off()

#6.abra
idosor_gen_orszag <- aggregate(suicides.100k.pop ~ country+generation, data=data, mean, na.rm=T)

hat_teljes2 <- idosor_gen_orszag[idosor_gen_orszag$country %in% hatorszag,]
hat_teljes2
png(file="6orszag_gen.png", width = 2048, height = 768)
ggplot(hat_teljes2, aes(fill = factor(generation), x= country, y=suicides.100k.pop )) + geom_col(position = "dodge")+ scale_fill_discrete(name="generáció") + ggtitle("6 ország generáció") + xlab ("ország") + ylab ("öngyilkosság/100.000 fő")
dev.off()

#összerakás
idosor_ffi_plot <- ggplot(idosor_nemek[idosor_nemek$sex == "male",], aes(x = year, y = suicides.100k.pop, colour = country )) + geom_line() + scale_color_discrete(name="ország") + ggtitle("Idősor férfiak") + xlab ("év") + ylab ("öngyilkosság/100.000 fő") + theme(legend.position = "none")
idosor_gen_plot <-  ggplot(idosor_gen, aes(x = year, y= suicides.100k.pop, colour = generation)) + geom_line() + scale_color_discrete(name="generációk") + ggtitle("Idősor generációk") + xlab ("év") + ylab ("öngyilkosság/100.000 fő") + theme(legend.position = "none")
idosor_no_plot <-  ggplot(idosor_nemek[idosor_nemek$sex == "female",], aes(x = year, y = suicides.100k.pop, colour = country )) + geom_line() + scale_color_discrete(name="ország") + ggtitle("Idősor nők") + xlab ("év") + ylab ("öngyilkosság/100.000 fő") + theme(legend.position = "none")
hatorszag_evek_plot <- ggplot(hat_teljes, aes(fill = factor(year), x= country, y=suicides.100k.pop )) + geom_col(position = "dodge")  + scale_fill_discrete(name = "év") + ggtitle("6 ország év") + xlab ("ország") + ylab ("öngyilkosság/100.000 fő") + theme(legend.position = "none")
hatorszag_gen_plot <- ggplot(hat_teljes2, aes(fill = factor(generation), x= country, y=suicides.100k.pop )) + geom_col(position = "dodge")+ scale_fill_discrete(name="generáció") + ggtitle("6 ország generáció") + xlab ("ország") + ylab ("öngyilkosság/100.000 fő") + theme(legend.position = "none")
idosor_plot <- ggplot(idosor_alt, aes(x = year, y = suicides.100k.pop, colour = country)) + geom_line() + scale_color_discrete(name="ország") + ggtitle("Idősor") + xlab ("év") + ylab ("öngyilkosság/100.000 fő") + theme(legend.position = "none")

install.packages("gridExtra")
library("gridExtra")
png(file = "osszes.png", width = 2048, height = 768)
grid.arrange(idosor_ffi_plot,idosor_gen_plot,idosor_no_plot,idosor_plot,hatorszag_evek_plot,hatorszag_gen_plot, ncol=2, nrow=3, top = "Beadanó összes")
dev.off()

#Értelmezés:
## Generációk alapján megállítható, hogy a legfiatalabb korosztályok kevésbé hajlamosak az öngyilkosságra, mint az idősebb generációk, és jól látszik, hogy 75 év felettieknél a legmagasabb az öngyilkosságok aránya. (G.I.Generation)
## A 6 legnagyobb arányú öngyilkossági ráátával rendelkező országokról mind elmondható, hogy nagyjából a 2000 évek óta csökkenő tendencia mutatkozik az öngyilkosságok számában.
## Magyarországon összesítve nagyon kiemelkedik, hogy a 75 év felettiek öngyilkossági aránya a többi generációhoz képest.
