#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Définition du serveur
shinyServer(function(input, output, session) {
  # Variable pour stocker le nombre de joueurs
  numberOfPlayers <- reactiveVal(NA)
  
  # Afficher la fenêtre pop-up au démarrage
  observeEvent(session, {
    shinyalert(
      title = "Choisissez le nombre de joueurs",
      text = "Veuillez entrer le nombre de joueurs entre 1 et 10 :",
      type = "input",
      inputType = "number",
      inputValue = 2,
      inputPlaceholder = "Nombre de joueurs",
      callbackR = function(value) {
        # Valider l'entrée
        if (!is.na(as.numeric(value)) && as.numeric(value) >= 1 && as.numeric(value) <= 10) {
          numberOfPlayers(as.numeric(value))
        } else {
          # Afficher un message d'erreur si l'entrée est invalide
          shinyalert(
            title = "Entrée invalide",
            text = "Veuillez entrer un nombre valide entre 1 et 10.",
            type = "error",
            callbackR = function(x) session$reload()
          )
        }
      }
    )
  }, once = TRUE)
  
  # Générer dynamiquement le selectizeInput en fonction du nombre de joueurs
  output$playerSelectorUI <- renderUI({
    if (!is.na(numberOfPlayers())) {
      selectizeInput(
        inputId = "selectedPlayer",
        label = "Choisissez un joueur",
        choices = paste("Joueur", 1:numberOfPlayers()),
        selected = NULL,
        options = list(placeholder = "Sélectionnez un joueur")
      )
    }
  })
  
  # Afficher le joueur sélectionné
  output$selectedPlayerText <- renderText({
    if (!is.null(input$selectedPlayer)) {
      paste("Joueur sélectionné :", input$selectedPlayer)
    } else {
      "Aucun joueur sélectionné."
    }
  })
})

