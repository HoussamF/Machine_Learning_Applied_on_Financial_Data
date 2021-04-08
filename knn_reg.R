
donnees5<-read.csv("C:/Users/HOUSSAM/Desktop/EURUSD.csv",stringsAsFactors = FALSE)

donnees5<-donnees5[-1]
donnees5<-donnees5[-1]

plot(x=donnees5$prix_debut,y=donnees5$prix_fin,xlab = "Prix au debut de la journee",ylab = "Prix à la fin de la journée")


#correlation entre prix_debut,prix_fin
cor(donnees5$prix_debut, donnees5$prix_fin)


donnees5.subset<-donnees5[,c(1:4)]
cor(donnees5.subset)

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

donnees5.subset.norm<-as.data.frame(lapply(donnees5.subset, normalize))

set.seed(1234)

#Training set + test set:

dat.d <- sample(1:nrow(donnees5.subset.norm),
                size=nrow(donnees5.subset.norm)*0.7,
                replace = FALSE)
train <- donnees5.subset.norm[dat.d,] # 70% training data

test <- donnees5.subset.norm[-dat.d,] # 30% test data

train_labels <- donnees5.subset[dat.d,4]

test_labels  <- donnees5.subset[-dat.d,4] 

library(class)
library(varhandle)

donnees5_pred <- unfactor(knn(train = train, test = test, cl = train_labels, k=2))

testLabels <- data.frame(test_labels)

# fusionner `donnees5_pred` et `testLabels` 
fusion <- data.frame(donnees5_pred, testLabels,stringsAsFactors = FALSE)

# noms des colonnes
names(fusion) <- c("Predite", "Observe")

fusion$Difference<-(fusion$Predite-fusion$Observe)
head(fusion)

hist(fusion$Difference,col = "red",breaks = 20,main="Histogramme des differences",xlab = "Difference",ylab = "Effectif")




#linear regression

donnees5.subset1<-donnees5[,c(1,4)]
donnees5.regression<-lm(prix_fin ~ prix_debut,data=donnees5.subset1)
summary(donnees5.regression)

plot(x=donnees5$prix_debut,y=donnees5$prix_fin,xlab="Prix au debut de la journee",ylab="Prix a la fin de la journee")
abline(donnees5.regression,col="red")

