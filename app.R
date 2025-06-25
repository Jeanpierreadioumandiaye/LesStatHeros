library(shiny)
library(ggplot2)
library(dplyr)
library(bslib)

# Données simulées
cours_data <- data.frame(
  Classe = rep(c("ISEP 1", "ISEP 2", "AS 1", "AS 2", "AS 3", "ISEP 3", "ISE 2", "ISE 3", "MASTER ADEPP"), each = 3),
  Cours = rep(c("Statistiques Descriptives", "Analyse des Données", "Gestion de Projet MS-Project"), times = 9),
  Volume_Prevu = c(30, 28, 32, 30, 30, 24, 30, 25, 30, 30, 24, 28, 28, 26, 30, 32, 30, 30, 30, 28, 30, 32, 30, 36, 30, 30, 30),
  Volume_Realise = c(18, 20, 24, 22, 30, 18, 26, 20, 15, 30, 18, 26, 25, 24, 27, 28, 22, 29, 30, 20, 25, 32, 30, 30, 15, 26, 30)
)

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "minty"),
  
  titlePanel("📚 Suivi de l’avancement des cours à l’ENSAE"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("classe", "📌 Choisir une classe :", 
                  choices = unique(cours_data$Classe),
                  selected = "MASTER ADEPP")
    ),
    
    mainPanel(
      fluidRow(
        column(4,
               div(class = "card text-white bg-success mb-3", style = "padding:10px;",
                   h5("📈 Moyenne d’avancement"),
                   textOutput("moyenne_text"))
        ),
        column(4,
               div(class = "card text-white bg-primary mb-3", style = "padding:10px;",
                   h5("✅ Cours le plus avancé"),
                   textOutput("max_cours"))
        ),
        column(4,
               div(class = "card text-white bg-danger mb-3", style = "padding:10px;",
                   h5("⚠️ Cours le moins avancé"),
                   textOutput("min_cours"))
        )
      ),
      
      fluidRow(
        column(12,
               h4("📊 Tableau de suivi"),
               tableOutput("summary_table"))
      ),
      
      fluidRow(
        column(12,
               h4("📉 Graphique d’avancement par matière"),
               plotOutput("progress_plot"))
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  
  data_classe <- reactive({
    filter(cours_data, Classe == input$classe) %>%
      mutate(Avancement = round(100 * Volume_Realise / Volume_Prevu, 1))
  })
  
  output$summary_table <- renderTable({
    data_classe() %>%
      select(Cours, Volume_Prevu, Volume_Realise, Avancement)
  })
  
  output$progress_plot <- renderPlot({
    df <- data_classe()
    ggplot(df, aes(x = reorder(Cours, Avancement), y = Avancement)) +
      geom_col(fill = "#0072B2") +
      coord_flip() +
      labs(x = "Cours", y = "Avancement (%)") +
      ylim(0, 100) +
      theme_minimal()
  })
  
  # Statistiques
  output$moyenne_text <- renderText({
    df <- data_classe()
    avg <- round(mean(df$Avancement), 1)
    paste(avg, "%")
  })
  
  output$max_cours <- renderText({
    df <- data_classe()
    df <- df[which.max(df$Avancement), ]
    paste0(df$Cours, " (", df$Avancement, "%)")
  })
  
  output$min_cours <- renderText({
    df <- data_classe()
    df <- df[which.min(df$Avancement), ]
    paste0(df$Cours, " (", df$Avancement, "%)")
  })
}

# Lancement de l'application
shinyApp(ui = ui, server = server)
