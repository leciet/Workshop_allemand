#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(shinyalert)


# Définition de l'interface utilisateur
shinyUI(
  fluidPage(
    fluidRow(column(10,
                    titlePanel("Apfelstrudel")),
             column(1,
                    numericInput('periode','période',value = 0,min = 0,max = 4)),
             column(1,
                    h1("\n"),
                    actionButton('changperiode','Go'))),
    tabsetPanel(
               tabPanel(title = 'Coût',
                        uiOutput("playerSelectorUI"), # Interface pour sélectionner un joueur
                         ),
               tabPanel(title = 'Marché'),
               tabPanel(title = 'Gains')
               )
  )
)
