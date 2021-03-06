getwd()
transakcie = read.csv("BreadBasket_DMS.csv")
Bread = summary(droplevels(transakcie[which(transakcie$Item=="Bread"),4]))
Cake = summary(droplevels(transakcie[which(transakcie$Item=="Cake"),4]))
Coffee= summary(droplevels(transakcie[which(transakcie$Item=="Coffee"),4]))
Medialuna= summary(droplevels(transakcie[which(transakcie$Item=="Medialuna"),4]))
NONE= summary(droplevels(transakcie[which(transakcie$Item=="NONE"),4]))
Pastry= summary(droplevels(transakcie[which(transakcie$Item=="Pastry"),4]))
Sandwich= summary(droplevels(transakcie[which(transakcie$Item=="Sandwich"),4]))
Tea= summary(droplevels(transakcie[which(transakcie$Item=="Tea"),4]))

A = summary(transakcie$Item)
B = as.logical(summary(transakcie$Item)> 590)
C = levels(transakcie$Item)
pomocne = data.frame(A,B,C)


Nesledovane = sum(pomocne[pomocne$B==FALSE,1])
sum(pomocne[pomocne$B==FALSE,1])

for (i in 1:7008) {
  Nesledovane[i]= "Ostatne"
}
Nesledovane = as.factor(Nesledovane)

Nesledovane = summary(Nesledovane) 

#summary(pomocne)

pomocne[which(pomocne$B == TRUE),3]
sledovane = pomocne[which(pomocne$B == TRUE),3]
sledovane = droplevels(sledovane)
#sledovane
#sledovane = c(Bread,Cake,Coffee,Medialuna,NONE,Pastry,Sandwich,Tea)
sledovane = c(Bread,Cake,Coffee,Medialuna,NONE,Pastry,Sandwich,Tea,Nesledovane)
#View(sledovane)
#sledovane
pct = round(sledovane/sum(sledovane)*100)
sledovane_1 = data.frame(A = c("Bread","Cake","Coffee","Medialuna","NONE","Pastry","Sandwich","Tea","Ostatne"), B=sledovane, C=pct)
j = paste(sledovane_1$A, sledovane_1$C, sep = " ")
j = paste(j, "%", sep = "")
pie(sledovane_1$B, labels = j, col = rainbow(length(sledovane_1$B)))
barplot(sledovane, col = rainbow(length(sledovane)))
transakcie$Date
#installed.packages()
#install.packages("lubridate")
#install.packages("yaml")
library(lubridate)
#install.packages("tidyverse")
transakcie_1 = transakcie
summary(transakcie$Date)
levels(transakcie$Date)
Dni = levels(transakcie$Date)
day(as.POSIXlt(Dni, format="%Y-%m-%d"))
Dni
index = c(7 , rep_len(1:7,158))
index
Datumy = data.frame(Dni,index)
names(Datumy) = c("Date","Ind")
spojeneTabulky = merge(transakcie_1,Datumy, by="Date")
View(spojeneTabulky)
index_1 = 1:7
Dni = c("Pondelok", "Utorok","Streda","Stvrtok","Piatok","Sobota","Nedela")
pomocna_1 = data.frame(index_1,Dni)
names(pomocna_1) = c("Ind", "Dni")
spojeneTabulky = merge(spojeneTabulky, pomocna_1, by="Ind")
spojeneTabulky_1 = spojeneTabulky[,c(2,3,4,5,6,1)]
spojeneTabulky_1 = spojeneTabulky_1[order(spojeneTabulky_1$Date),]
summary(spojeneTabulky_1$Dni)
spojeneTabulky_1$Dni = relevel(spojeneTabulky_1$Dni, "Nedela")
spojeneTabulky_1$Dni = relevel(spojeneTabulky_1$Dni, "Sobota")
spojeneTabulky_1$Dni = relevel(spojeneTabulky_1$Dni, "Piatok")
spojeneTabulky_1$Dni = relevel(spojeneTabulky_1$Dni, "Stvrtok")
spojeneTabulky_1$Dni = relevel(spojeneTabulky_1$Dni, "Streda")
spojeneTabulky_1$Dni = relevel(spojeneTabulky_1$Dni, "Utorok")
spojeneTabulky_1$Dni = relevel(spojeneTabulky_1$Dni, "Pondelok")

Dni_2 = summary(spojeneTabulky_1$Dni)
barplot(Dni_2,col = rainbow(length(Dni_2)))


library(clusterSim)
library(party)

#levels(spojeneTabulky_1$Item)
#day(as.POSIXlt(Dni, format="%Y-%m-%d"))

Hodiny = spojeneTabulky_1$Time
#View(Hodiny)
index_casu = 1:21293
Cas = data.frame(Hodiny, index_casu)
#View(hms(Hodiny))
Hodiny_1 = data.frame(hms(Hodiny)@hour,hms(Hodiny)@minute, index_casu)
#View(Hodiny_1)
spojeneTabulky_1$index_casu = 1:21293
spojeneTabulky_2 = merge(spojeneTabulky_1, Hodiny_1, by = "index_casu")
spojeneTabulky_1 = spojeneTabulky_2[,c(2:9)]
#corrgram(spojeneTabulky_1)
opilun = data.frame(table(spojeneTabulky_1$hms.Hodiny..hour,spojeneTabulky_1$Dni))
#View(opilun)
library(lattice)
xyplot(Freq~ Var1| Var2, data=opilun)
table(spojeneTabulky_1$Item,spojeneTabulky_1$hms.Hodiny..hour)
najpredavanejsie_casy = table(spojeneTabulky_1$Item,spojeneTabulky_1$hms.Hodiny..hour)
najpredavanejsie_casy = data.frame(najpredavanejsie_casy)
plot(najpredavanejsie_casy$Var1,najpredavanejsie_casy$Freq)
paste(najpredavanejsie_casy$Var1, najpredavanejsie_casy$Freq)

levels(najpredavanejsie_casy$Var2)
najpredavanejsie_casy[which(najpredavanejsie_casy$Var2 == "7"),c(1,3)]

najpredavanejsie_casy[which(levels(najpredavanejsie_casy$Var1) == 1),1:3]
#polozky =levels(najpredavanejsie_casy$Var1)
#polozky[1]  
levels(najpredavanejsie_casy$Var1)
G = najpredavanejsie_casy[which(najpredavanejsie_casy$Freq>33),1:3]
G = najpredavanejsie_casy[which(najpredavanejsie_casy$Var1 == polozky[1]),1:3]
G$Var1 = droplevels(G$Var1)
#levels(G$Var1)
#plot(x =najpredavanejsie_casy$Var2,y = najpredavanejsie_casy$Freq)
xyplot(Freq~ Var2| Var1, data=G)
#mean(G$Freq) 
View(spojeneTabulky_1)
View(summary(as.factor(spojeneTabulky_1$Transaction)))
month(spojeneTabulky_1$Date)
spojeneTabulky_1$Mesiace = month(spojeneTabulky_1$Date)
year(spojeneTabulky_1$Date)
spojeneTabulky_1$Roky= year(spojeneTabulky_1$Date)
group(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10), "Transaction"])
grouping(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10), "Transaction"])
plot(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10), "Transaction"])
barplot(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10), "Transaction"])
View(summary(as.factor(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10), "Transaction"])))
View(spojeneTabulky_1)
spojeneTabulky_1 = spojeneTabulky_1[order(spojeneTabulky_1$Transaction),]
barplot(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10),"Transaction"])
#View(days(spojeneTabulky_1$Date))
Dni_4 = days(spojeneTabulky_1$Date)@day

spojeneTabulky_1$IndexDni = Dni_4
Celkovy_Index = 1:21293
spojeneTabulky_1$Celkovy_Index = Celkovy_Index
model = glm(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10),"Transaction"] ~spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10),"Celkovy_Index"])
plot(spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10),"Transaction"] ~spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10),"Celkovy_Index"])
abline(model)


#Alpha = spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10),"Celkovy_Index"]
#Beta = spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 10),"Transaction"]

Celkovy_Index = 1:21293
spojeneTabulky_1$Celkovy_Index = Celkovy_Index
Alpha = spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 11),"Celkovy_Index"]
Beta = spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 11),"Transaction"]
Alpha_pred=spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 12),"Celkovy_Index"]
#Beta_pred=
#Beta_skut=spojeneTabulky_1 =spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 12),"Transaction"]
spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 12),"Celkovy_Index"]
Napilon = data.frame(Alpha,Beta)
model_1 = lm(Beta~Alpha,data=Napilon)
model_1
Napilon_2 = data.frame(Alpha = Alpha_pred) 
predpoved = predict(model_1,newdata = Napilon_2)
#View(predpoved)
dlzka = length(Napilon$Alpha)
dlzka_1 =length(Napilon_2$Alpha)
style = c(rep(1,dlzka),rep(2,dlzka_1))
plot(x=c(Napilon$Alpha,Napilon_2$Alpha),y= c(Napilon$Beta,predpoved),pch=style,col=style)
Napilon_2 = data.frame(Alpha = Alpha_pred,Beta_pred=spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 12),"Transaction"],predpoved)
plot(x=c(Napilon$Alpha,Napilon_2$Alpha),y= c(Napilon$Beta,predpoved),pch=style,col=style)
plot(x=c(Napilon$Alpha,Napilon_2$Alpha),y= c(Napilon$Beta,Napilon_2$Beta_pred),col=style)
legend(x="bottomright", legend = c("Ziskane v novembri","Ziskane v decembri"),pch = style, col = c("black","red"))
chyba = c(NULL)
for (jndex in 1:length(predpoved)) {
  chyba[jndex] = predpoved[jndex]-spojeneTabulky_1[which(spojeneTabulky_1$Mesiace == 12),"Transaction"][jndex]
}
priemerna_chyba_modelu = mean(chyba)
priemerna_chyba_modelu


library(corrgram)
corrgram(spojeneTabulky_1,lower.panel = panel.shade, upper.panel = panel.pie)

spojeneTabulky_4=spojeneTabulky_1[,c(3,4,5,9,10)]
spojeneTabulky_4 = spojeneTabulky_4[,c(1,3,4,5,2)]
spojeneTabulky_4
set.seed(200)
vzorkovanie_3 = sample(2,nrow(spojeneTabulky_4),replace = TRUE, prob = c(0.7,0.3))
trenovacie_2 = spojeneTabulky_4[vzorkovanie_3==1,1:4]
testovacie_2= spojeneTabulky_4[vzorkovanie_3==2,1:4]
trenovacie_2_spoj = spojeneTabulky_4[vzorkovanie_3==1,5]
testovacie_2_spoj = spojeneTabulky_4[vzorkovanie_3==2,5]
library(e1071)
klasifikacia.trenovacia = naiveBayes(trenovacie_2,trenovacie_2_spoj)
View(table(predict(klasifikacia.trenovacia,trenovacie_2),trenovacie_2_spoj))
uspechKlasifikacie = data.frame(table(predict(klasifikacia.trenovacia,trenovacie_2),trenovacie_2_spoj))

uspechKlasifikacie [which(uspechKlasifikacie$Var1== uspechKlasifikacie$trenovacie_2_spoj & uspechKlasifikacie$Freq>0),]
vyhodnotenie = uspechKlasifikacie [which(uspechKlasifikacie$Var1== uspechKlasifikacie$trenovacie_2_spoj & uspechKlasifikacie$Freq>0),]
uspesnost = sum(vyhodnotenie$Freq)/sum(uspechKlasifikacie$Freq)*100
uspesnost


summary(as.factor(month(spojeneTabulky_2$Date)))
saveRDS(spojeneTabulky_1, "Data_3.rda")
niecovym = readRDS(file = "Data_3.rda")
saveRDS(sledovane_1, "Sledovane_1.rda")
saveRDS(sledovane, "Sledovane.rda")
