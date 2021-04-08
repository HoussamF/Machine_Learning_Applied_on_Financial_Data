donnees<-read.csv("C:/Users/HOUSSAM/Desktop/EURUSD1440.csv")
head(donnees)

donnees<-donnees[-2]

donnees
#savoir les directions :
donnees$direction <- 1
donnees$direction <- ifelse(donnees$prix_debut > donnees$prix_fin,0,donnees$direction)

#compter les jours consecutives ayant une seule direction :
donnees$compteur<-1
for (i in 2:nrow(donnees)) 
  {
    if(donnees[i,]$direction == donnees[i-1,]$direction)
    {
      donnees[i,]$compteur<-donnees[i-1,]$compteur+1
    }
}
#######################################################
#méthode 1 :                                         
#######################################################

#savoir la direction du jour suivant:
shift<-function(x,n)
{
  c(x[-(seq(n))],rep(NA,n))
}
donnees$direction_suiv<-shift(donnees$compteur,1)
head(donnees)

#tableau des evenements :
table<-table(donnees$compteur,donnees$direction_suiv)
table
#probabilites des evenements:
table.pourcentage<-prop.table(table,1)
#table de probabilite:
print.table(local({table.pourcentage[table.pourcentage==0]<-NA;table.pourcentage}))

#apres x jours consecutives en une seule direction 

donnees$prix_debut_suiv<-shift(donnees$prix_debut,1)
donnees$prix_fin_suiv<-shift(donnees$prix_fin,1)
head(donnees)




############################################
#methode 2
############################################

donnees$ligne<-seq(1,nrow(donnees),1)

donnees$smooth<-ksmooth(donnees$ligne,donnees$prix_fin,"normal",bandwidth = 10)$y
plot(donnees[1:200,]$prix_fin,xlab="Jour",ylab="Prix a la fin de la journée")
lines(donnees[1:200,]$smooth,col="red")
