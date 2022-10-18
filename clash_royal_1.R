#appel de la libraire readxl
#librairie shiny
#installation et chargement du package DT
#install.packages("DT")
#install.packages("ggplot2")
#install.packages("dplyr")
library(readxl)
library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(shinydashboard)


#import des donnÃ©es des matchs
match <- read_excel("match.xlsx")
#import des donnÃ©es du classement des joueurs
ranking <- read_excel("ranking.xlsx")
#import des donnÃ©es des joueurs
Joueurs <- read_excel("Joueurs.xlsx")
#import des donnÃ©es concernant les cartes
troupes <- read_excel("type_troupe.xlsx")


ui<-fluidPage(
  
  #titre de l'application shiny
  titlePanel("Clash royal Stats"),

  conditionalPanel(
    'input.dataset === "ranking"',),
  
    #Panel principal
    mainPanel(
    #crÃ©ation des diffÃ©rentes pages de l'application
    tabsetPanel(
      id = 'dataset',
      #page ranking
      #ajout tableau ranking
      tabPanel("ranking", DT::dataTableOutput("mytable2"),
      plotOutput("arene")),
      
      #page Plot
      #ajout de 3 graphiques : barplot arene, barplot raretÃ© des troupes, barplot type de troupe
      tabPanel("Plot", 
               plotOutput("tr"),
               plotOutput("type"),
               dataTableOutput(outputId = "table")),
      
      #page "votre deck" qui permet Ã l'utilisateur de crÃ©er son deck
      tabPanel("votre deck",
                 #crÃ©ation d'une bar permettant de faire son choix de troupe
                  sidebarPanel("Votre deck",
                 #crÃ©ation des 8 sÃ©lect input permettant de choisir les 8 cartes de son deck
                  selectInput("choix1","choix1",troupes$name),
                  selectInput("choix2","choix2",troupes$name),
                  selectInput("choix3","choix3",troupes$name),
                  selectInput("choix4","choix4",troupes$name),
                  selectInput("choix5","choix5",troupes$name),
                  selectInput("choix6","choix6",troupes$name),
                  selectInput("choix7","choix7",troupes$name),
                  selectInput("choix8","choix8",troupes$name),
                  verbatimTextOutput("mean"))),
        )
    ))
#code serveur
server <- function(input, output) {
  
  #rÃ©alisation du tableau ranking via le package DT
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(ranking, options = list(orderClasses = TRUE))
  })
  
  #rÃ©alisation des 3 graphiques (tri Ã plat)
  output$arene <- renderPlot(barplot(table(ranking$name_arena),main = "Graphique de la répartition des joueurs par arène"))
  output$tr <- renderPlot(barplot(table(troupes$rarity),main = "répartition de la rareté des cartes"))
  output$type <- renderPlot(barplot(table(troupes$type),main = "répartition du type de carte"))
  
  output$mean<-renderText({
    lscri <- c(input$choix1,input$choix2,input$choix3,input$choix4,input$choix5,input$choix6,input$choix7,input$choix8)
    mean(filter(troupes,name %in% lscri)$elixir,na.rm = TRUE)
})
  output$table <- renderDataTable({troupes})
}

shinyApp(ui, server)

# Run l'app
shinyApp(ui = ui, server = server)