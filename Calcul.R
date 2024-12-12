Spiel <- data.frame(
  Preis_P = c(5, 0, 0, 0, 0),
  Nachfrage_N = c(0, 8, 7, 12, 15),
  Zufall_Z = c(0, -0.25, -0.05, 0.15, 0.1),
  SeuilEqui_S = c(0, 7, 15, 5, 15),
  A = rep(0,times = 5)
)


Groupes <- data.frame(
  Round = rep(1:4, each = 3),
  Team = rep(c("Team 1","Team 2","Team 3"), times = 4),
  Geld = rep(0,12)
)

##### ROOOUUNNNNNDDDDDD 1 ######
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
Groupes$Geld[1 + 3*Compteur] <- Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 3*Compteur] <- Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 3*Compteur] <- Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3



windows()
ggplot(data = Spiel[1:R, ], aes(x = 1:R, y = Preis_P)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(x = "Round", y = "Preis") +
  theme_minimal()+
  scale_x_continuous(breaks = 1:5, labels = 0:4, limits = c(1,5)) +
  theme(
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.line = element_line(color = "black", size = 0.8)
  )

Groupes_filtered <- Groupes[Groupes$Round <= R-1, ]

windows()

ggplot(data = Groupes_filtered, aes(x = Round, y = Geld, color = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Geld per round per Team", x = "Round", y = "Geld") +
  theme_minimal() +
  xlim(1,5)+
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 0.8) +
  ylim(min(0,Groupes_filtered$Geld)-1,max(Groupes_filtered$Geld)+1) +
  theme(axis.line = element_line(color = "black", size = 0.8),
        axis.title = element_text(size = 14 , face = "bold"),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 18, face = "bold")
  )


##### ROOOUUNNNNNDDDDDD 2 ######
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
Groupes$Geld[1 + 3*Compteur] <- Groupes$Geld[1 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 3*Compteur] <- Groupes$Geld[2 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 3*Compteur] <- Groupes$Geld[3 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3


windows()
ggplot(data = Spiel[1:R, ], aes(x = 1:R, y = Preis_P)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(x = "Round", y = "Preis") +
  theme_minimal()+
  scale_x_continuous(breaks = 1:5, labels = 0:4, limits = c(1,5)) +
  theme(
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.line = element_line(color = "black", size = 0.8)
  )

Groupes_filtered <- Groupes[Groupes$Round <= R-1, ]

windows()

ggplot(data = Groupes_filtered, aes(x = Round, y = Geld, color = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Geld per round per Team", x = "Round", y = "Geld") +
  theme_minimal() +
  xlim(1,5)+
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 0.8) +
  ylim(min(0,Groupes_filtered$Geld)-1,max(Groupes_filtered$Geld)+1) +
  theme(axis.line = element_line(color = "black", size = 0.8),
        axis.title = element_text(size = 14 , face = "bold"),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 18, face = "bold")
  )



##### ROOOUUNNNNNDDDDDD 3 ######
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
Groupes$Geld[1 + 3*Compteur] <- Groupes$Geld[1 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 3*Compteur] <- Groupes$Geld[2 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 3*Compteur] <- Groupes$Geld[3 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3


windows()
ggplot(data = Spiel[1:R, ], aes(x = 1:R, y = Preis_P)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(x = "Round", y = "Preis") +
  theme_minimal()+
  scale_x_continuous(breaks = 1:5, labels = 0:4, limits = c(1,5)) +
  theme(
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.line = element_line(color = "black", size = 0.8)
  )

Groupes_filtered <- Groupes[Groupes$Round <= R-1, ]

windows()
ggplot(data = Groupes_filtered, aes(x = Round, y = Geld, color = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Geld per round per Team", x = "Round", y = "Geld") +
  theme_minimal() +
  xlim(1,5)+
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 0.8) +
  ylim(min(0,Groupes_filtered$Geld)-1,max(Groupes_filtered$Geld)+1) +
  theme(axis.line = element_line(color = "black", size = 0.8),
        axis.title = element_text(size = 14 , face = "bold"),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 18, face = "bold")
  )



##### ROOOUUNNNNNDDDDDD 4 ######
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
Groupes$Geld[1 + 3*Compteur] <- Groupes$Geld[1 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G1  - Spiel$Preis_P[R-1] * Achat_G1 

#Mise à jour pour G2
Groupes$Geld[2 + 3*Compteur] <- Groupes$Geld[2 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G2 - Spiel$Preis_P[R-1] * Achat_G2

#Mise à jour pour G3
Groupes$Geld[3 + 3*Compteur] <- Groupes$Geld[3 + 3*(Compteur-1)] + Spiel$Preis_P[R] * Vente_G3 - Spiel$Preis_P[R-1] * Achat_G3



windows()
ggplot(data = Spiel[1:R, ], aes(x = 1:R, y = Preis_P)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(x = "Round", y = "Preis") +
  theme_minimal()+
  scale_x_continuous(breaks = 1:5, labels = 0:4, limits = c(1,5)) +
  theme(
    axis.title = element_text(size = 14 , face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.line = element_line(color = "black", size = 0.8)
  )

Groupes_filtered <- Groupes[Groupes$Round <= R-1, ]

windows()

ggplot(data = Groupes_filtered, aes(x = Round, y = Geld, color = Team)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Geld per round per Team", x = "Round", y = "Geld") +
  theme_minimal() +
  xlim(1,5)+
  geom_hline(yintercept = 0, color = "black", linetype = "dotted", size = 0.8) +
  ylim(min(0,Groupes_filtered$Geld)-1,max(Groupes_filtered$Geld)+1) +
  theme(axis.line = element_line(color = "black", size = 0.8),
        axis.title = element_text(size = 14 , face = "bold"),
        axis.text.x = element_text(size = 12),
        title = element_text(size = 18, face = "bold")
  )
