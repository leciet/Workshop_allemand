Spiel <- data.frame(
  Preis_P = c(5, 0, 0, 0, 0),
  Nachfrage_N = c(0, 8, 7, 12, 15),
  Zufall_Z = c(0, -0.25, -0.05, 0.15, 0.1),
  SeuilEqui_S = c(0, 7, 15, 5, 15)
)


Groupes <- data.frame(
  Round = rep(1:4, each = 6),
  Team = rep(c("Team 1","Team 2","Team 3"), times = 8),
  Situation = rep(c("Reale Situation","Ganz Verkauft"),each=3,times=4),
  Geld = rep(0,24)
)

#Round 1----
R <- 2
Compteur <- R-2

Achat_G1 <- 
Vente_G1 <-
Stock_G1 <- Achat_G1 - Vente_G1


Achat_G2 <- 
Vente_G2 <-
Stock_G2 <- Achat_G2 - Vente_G2


Achat_G3 <- 
Vente_G3 <-
Stock_G3 <- Achat_G3 - Vente_G3

Spiel$A[R] <- Achat_G1 + Achat_G2 + Achat_G3

Spiel$Preis_P[R] <- Spiel$Preis_P[R-1] * (1 + 0.1 *(Spiel$A[R]-Spiel$SeuilEqui_S[R])/Spiel$SeuilEqui_S[R] + 0.2 * (Spiel$Nachfrage_N[R] - Spiel$A[R])/Spiel$A[R] + Spiel$Zufall_Z[R])

#Mise à jour pour G1
Groupes$Geld[1 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2
Groupes$Geld[5 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3
Groupes$Geld[6 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G3 - Spiel$Preis_P[R-1] * Achat_G3



#Round 2----
R <- 3
Compteur <- R-2


Achat_G1 <- 
Vente_G1 <-
Stock_G1 <- Stock_G1 + Achat_G1 - Vente_G1

Achat_G2 <- 
Vente_G2 <-
Stock_G2 <- Stock_G2 + Achat_G2 - Vente_G2


Achat_G3 <- 
Vente_G3 <-
Stock_G3 <- Stock_G3 + Achat_G3 - Vente_G3


Spiel$A[R] <- Achat_G1 + Achat_G2 + Achat_G3

Spiel$Preis_P[R] <- Spiel$Preis_P[R-1] * (1 + 0.1 *(Spiel$A[R]-Spiel$SeuilEqui_S[R])/Spiel$SeuilEqui_S[R] + 0.2 * (Spiel$Nachfrage_N[R] - Spiel$A[R])/Spiel$A[R] + Spiel$Zufall_Z[R])


#Mise à jour pour G1
Groupes$Geld[1 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2
Groupes$Geld[5 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3
Groupes$Geld[6 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G3 - Spiel$Preis_P[R-1] * Achat_G3



#Round 3----
R <- 4
Compteur <- R-2

Achat_G1 <- 
Vente_G1 <-
Stock_G1 <- Stock_G1 + Achat_G1 - Vente_G1

Achat_G2 <- 
Vente_G2 <-
Stock_G2 <- Stock_G2 + Achat_G2 - Vente_G2

Achat_G3 <- 
Vente_G3 <-
Stock_G3 <- Stock_G3 + Achat_G3 - Vente_G3


Spiel$A[R] <- Achat_G1 + Achat_G2 + Achat_G3

Spiel$Preis_P[R] <- Spiel$Preis_P[R-1] * (1 + 0.1 *(Spiel$A[R]-Spiel$SeuilEqui_S[R])/Spiel$SeuilEqui_S[R] + 0.2 * (Spiel$Nachfrage_N[R] - Spiel$A[R])/Spiel$A[R] + Spiel$Zufall_Z[R])


#Mise à jour pour G1
Groupes$Geld[1 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2
Groupes$Geld[5 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3
Groupes$Geld[6 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G3 - Spiel$Preis_P[R-1] * Achat_G3



#Round 4----
R <- 5
Compteur <- R-1


Achat_G1 <- 
Vente_G1 <-
Stock_G1 <- Stock_G1 + Achat_G1 - Vente_G1

Achat_G2 <- 
Vente_G2 <-
Stock_G2 <- Stock_G2 + Achat_G2 - Vente_G2


Achat_G3 <- 
Vente_G3 <-
Stock_G3 <- Stock_G3 + Achat_G3 - Vente_G3


Spiel$A[R] <- Achat_G1 + Achat_G2 + Achat_G3

Spiel$Preis_P[R] <- Spiel$Preis_P[R-1] * (1 + 0.1 *(Spiel$A[R]-Spiel$SeuilEqui_S[R])/Spiel$SeuilEqui_S[R] + 0.2 * (Spiel$Nachfrage_N[R] - Spiel$A[R])/Spiel$A[R] + Spiel$Zufall_Z[R])


#Mise à jour pour G1
Groupes$Geld[1 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2
Groupes$Geld[5 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 6*Compteur] <- Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3
Groupes$Geld[6 + 6*Compteur] <- Spiel$Preis_P[R] * Achat_G3 - Spiel$Preis_P[R-1] * Achat_G3
