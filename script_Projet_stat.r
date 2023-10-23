#Tache1:Importation de la base de donnée
Abalone = read.table(file=file.choose(),header=TRUE,sep=",",dec=".")
names(Abalone)
dim(Abalone)
#afficher la base
View(Abalone)
#structure de la base
str(Abalone)
summary(Abalone)
#verification de la duplication
Abalone<-Abalone[!duplicated(Abalone),]
sum(duplicated(Abalone))

#Tache2:Pré-traitement des données

#1/Valeurs aberantes :
#on peut détecter graphiquement par plusieurs méthodes
head(Abalone)
tail(Abalone)
#Analyser la colonne Sex par un histogramme pour verifier l'existance des valeurs aberrants
library(ggplot2)
ggplot(data=Abalone,aes(x=Sex,fill=Sex))+geom_bar()#la courbe verifie l'existance des VA
#analyse les colonnes de la base par boxplot pour verifier l'existance des valeurs aberrants
#J'ai divisé les colonnes sur 4 figures pour bien vrifier correctement l'existance des valeus aberantes
boxplot(Abalone$Length,Abalone$Diameter,Abalone$Height, col = "red")
boxplot(Abalone$Whole_Weight, col = "pink")
boxplot(Abalone$Shucked_Weight,Abalone$Viscera_Weight,Abalone$SHell_Weight, col = "green")
boxplot(Abalone$Rings)
#Analyse les colonnes de la base par la plot pour verifier l'existance des valeurs aberrants
plot(Abalone$Length)#pas de VA
plot(Abalone$Diameter)#pas de VA
plot(Abalone$Height)#existe VA
plot(Abalone$Whole_Weight)#pas de VA
plot(Abalone$Shucked_Weight)#pas de VA
plot(Abalone$Viscera_Weight)#existe VA
plot(Abalone$SHell_Weight)#pas de VA
plot(Abalone$Rings)#pas de VA

#Imputer les valeurs aberrantes en utilisant la kNN imputer

## on va changer les outliers par NA colonne par colonne 

#height
which(Abalone$Height>0.4)
# 1418 2052
Abalone$Height[1418] <- NA
Abalone$Height[2052] <- NA

which(Abalone$Height == 0)
# 1258 3997
Abalone$Height[1258] <- NA
Abalone$Height[3997] <- NA

#Viscera_Weight
which(Abalone$Viscera_Weight>0.6)
## 1763 1764
Abalone$Viscera_Weight[1763] <- NA
Abalone$Viscera_Weight[1764] <- NA

## on va utiliser knn
"""
#autre facon pour detecter outliers et les suprimer
outliers <- function(x) {

  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1

 upper_limit = Q3 + (iqr*1.5)
 lower_limit = Q1 - (iqr*1.5)

 x > upper_limit | x < lower_limit
}

remove_outliers <- function(Abalone,cols) {
  for (col in cols) {
    Abalone <- df[!outliers(df[[col]]),]
  }
  Abalone
}
...........
no_missing_abalone = Abalone[complete.cases(Abalone), ]
num_only = select(no_missing_abalone,-Sex)
Abalone <- remove_outliers(no_missing_abalone, colnames(num_only))
"""


#2/Valeurs manquantes :

#les NA sont deja traitee et remplacee par KNN imputuer
#on cherche les champs vides
#na.fail(Abalone)#Error in na.fail.default(Abalone) : valeurs manquantes dans l'objet
sum(is.na(Abalone) | Abalone=="")#exist 25 champs vides.
taux = sum(is.na(Abalone) | Abalone=="")/prod(dim(Abalone)) 
taux
##Alors va utiliser knn imputer 
#on remplacer les "" (vides) par NA
Abalone[Abalone==""]<-NA
sum(is.na(Abalone))

#methode de kNN
install.packages("VIM")
library(VIM)
Abalone = kNN(Abalone)
dim(Abalone)
Abalone = subset(Abalone, select = -c(Sex_imp,Length_imp,
                            Diameter_imp, Height_imp, Whole_Weight_imp,
                            Shucked_Weight_imp, Viscera_Weight_imp, SHell_Weight_imp, Rings_imp))

#Tache 3 : Analyse univariee :

## 1 : Quantitative
#on va tester la normalité par la methode de shapiro.test
shapiro.test(Abalone$Length) 
shapiro.test(Abalone$Diameter) 
shapiro.test(Abalone$Height) 
shapiro.test(Abalone$Whole_Weight)
shapiro.test(Abalone$Shucked_Weight)
shapiro.test(Abalone$Viscera_Weight)
shapiro.test(Abalone$SHell_Weight)
shapiro.test(Abalone$Rings)
#on teste la normalité par les histogramme
hist(Abalone$Length, prob=T)
curve(dnorm(x,mean(Abalone$Length),sd(Abalone$Length)),add=T,col="red")
hist(Abalone$Diameter, prob=T)
curve(dnorm(x,mean(Abalone$Diameter),sd(Abalone$Diameter)),add=T,col="red")
hist(Abalone$Height, prob=T)
curve(dnorm(x,mean(Abalone$Height),sd(Abalone$Height)),add=T,col="red")
hist(Abalone$Whole_Weight, prob=T)
curve(dnorm(x,mean(Abalone$Whole_Weight),sd(Abalone$Whole_Weight)),add=T,col="red")
hist(Abalone$Shucked_Weight, prob=T)
curve(dnorm(x,mean(Abalone$Shucked_Weight),sd(Abalone$Shucked_Weight)),add=T,col="red")
hist(Abalone$Viscera_Weight, prob=T)
curve(dnorm(x,mean(Abalone$Viscera_Weight),sd(Abalone$Viscera_Weight)),add=T,col="red")
hist(Abalone$SHell_Weight, prob=T)
curve(dnorm(x,mean(Abalone$SHell_Weight),sd(Abalone$SHell_Weight)),add=T,col="red")
hist(Abalone$Rings, prob=T)
curve(dnorm(x,mean(Abalone$Rings),sd(Abalone$Rings)),add=T,col="red")

## 2 : Qualitative
prop.table(table_Species)
barplot(table_Species, main="Répartition des espèces",col="green")

#Tache 4:Analyse bivariee :
#Correlation entre tous les colonnes 
install.packages("GGally")
library(ggplot2)
ggcorr(Abalone, method = c("everything","spearman"),label=TRUE)#corélation rouge
library(corrplot)
mcor <- cor(Abalone[,2:9])
corrplot(mcor,type="upper",order="hclust",tl.col = "black",tl.srt = 45)#corélation bleu

#Correlation entre les colonnes quantitative 
#avec le diameter : 

res<-cor.test(Abalone$Length,Abalone$Diameter, method="spearman")
res

res1<-cor.test(Abalone$Height,Abalone$Diameter,method = "spearman")
res1

res2<-cor.test(Abalone$Whole_Weight,Abalone$Diameter, method = "spearman")
res2
cor.test(Abalone$Shucked_Weight,Abalone$Diameter)
cor.test(Abalone$Viscera_Weight,Abalone$Diameter)
cor.test(Abalone$SHell_Weight,Abalone$Diameter)
cor.test(Abalone$Rings,Abalone$Diameter)
# avec le Length : 
cor.test(Abalone$Height,Abalone$Length)
cor.test(Abalone$Whole_Weight,Abalone$Length)
cor.test(Abalone$Shucked_Weight,Abalone$Length)
cor.test(Abalone$Viscera_Weight,Abalone$Length)
cor.test(Abalone$SHell_Weight,Abalone$Length)
cor.test(Abalone$Rings,Abalone$Length)
# avec le Height : 
cor.test(Abalone$Whole_Weight,Abalone$Height)
cor.test(Abalone$Shucked_Weight,Abalone$Height)
cor.test(Abalone$Viscera_Weight,Abalone$Height)
cor.test(Abalone$SHell_Weight,Abalone$Height)
cor.test(Abalone$Rings,Abalone$Height)
# avec le Whole_Weight :
cor.test(Abalone$Shucked_Weight,Abalone$Whole_Weight)
cor.test(Abalone$Viscera_Weight,Abalone$Whole_Weight)
cor.test(Abalone$SHell_Weight,Abalone$Whole_Weight)
cor.test(Abalone$Rings,Abalone$Whole_Weight)
# avec le Shucked_Weight :
cor.test(Abalone$Viscera_Weight,Abalone$Shucked_Weight)
cor.test(Abalone$SHell_Weight,Abalone$Shucked_Weight)
cor.test(Abalone$Rings,Abalone$Shucked_Weight)
# avec le SHell_Weight :
cor.test(Abalone$Viscera_Weight,Abalone$SHell_Weight)
cor.test(Abalone$SHell_Weight,Abalone$SHell_Weight)
cor.test(Abalone$Rings,Abalone$SHell_Weight)
#correlation deux a deux par scatterplot
plot(Abalone[,2:9])

#qualitative
kruskal.test(Abalone$Sex, Abalone$Diameter)
kruskal.test(Abalone$Sex, Abalone$Length)
kruskal.test(Abalone$Sex, Abalone$Height)
kruskal.test(Abalone$Sex, Abalone$Whole_Weight)
kruskal.test(Abalone$Sex, Abalone$Shucked_Weight)
kruskal.test(Abalone$Sex, Abalone$Viscera_Weight)
kruskal.test(Abalone$Sex, Abalone$SHell_Weight)
kruskal.test(Abalone$Sex, Abalone$Rings)




#Tache 5:Régression linéaire :
#1. Régresser la variable cible quantitative de la base de donnees en fonctions des autres.
regCible <- lm(Diameter~Length+Height+Whole_Weight+Shucked_Weight+Viscera_Weight+SHell_Weight+Rings,data=Abalone)
summary(regCible)
plot(regCible$fitted.values,regCible$residuals, xlab="Valeurs prédites par le modéle",ylab="Residus",pch=16,cex=0.75,col="green")
curve(dnorm(x,mean(regCible$residuals),sd(regCible$residuals)),add=T,col="red")
#test de normalité de résidus
shapiro.test(regCible$residuals)
hist(regCible$residuals, prob=T)
curve(dnorm(x,mean(regCible$residuals),sd(regCible$residuals)),add=T,col="red")
#2
library(dplyr)
library(mltools)
library(data.table)
library(devtools)
Abalone$Sex <- as.factor(Abalone$Sex)
encoded_data <- one_hot(as.data.table(Abalone))
encoded_data
diameter.lm = lm(Diameter ~ ., data=encoded_data)
summary(diameter.lm)
step(diameter.lm , direction = "backward")#elimine colonne par colonne selon AIC
nouv_model = lm(formula = Diameter ~ Sex_F + Sex_I + Length + Height + Shucked_Weight + 
                  Viscera_Weight + SHell_Weight + Rings , data = encoded_data)
summary(nouv_model)
"""
random forest
#Loading library
library('randomForest')

# Using random forest for variable selection
data=Abalone[,-1]
rfModel <-randomForest(Abalone$Diameter ~ ., data =data[,-2] )

# Getting the list of important variables
importance(rfModel)
"""
#3
names(Abalone)
Abalone=Abalone[,-1]
set.seed(111)
ind <- sample(2, nrow(Abalone),
              replace = TRUE,
              prob = c(0.8, 0.2))
training <- Abalone[ind==1,]
testing <- Abalone[ind==2,]

library(psych)
pairs.panels(training[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[training$Diameter],
             pch=21)
pc <- prcomp(training[,-2],
             center = TRUE,
             scale. = TRUE)
attributes(pc)
pc$center
print(pc)
summary(pc)
pairs.panels(pc$x,
             gap=0,
             bg = c("red", "yellow", "blue")[training$Sex],
             pch=21)

trg <- predict(pc, training)
trg <- data.frame(trg, training[2])
tst <- predict(pc, testing)
tst <- data.frame(tst, testing[2])

reg<- lm(Diameter~PC1+PC2,data=trg)
summary(reg)
#Tache 6 : Régression linéaire généralisée :
#1
#2
res.glm.Gamma.log <- glm(formula =  Diameter ~ Sex_F + Sex_I + Length + Height + Shucked_Weight + 
                           Viscera_Weight + SHell_Weight + Rings,
                         family  = Gamma(link = "identity"),
                         data    = encoded_data)
summary(res.glm.Gamma.log)
