library(shiny)

# Define UI for Network app ----
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Authors Network on Social Norms"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    
    # Input: Selector for Game Types ----
    selectInput("Group_type", "Game Type:",
                c("Dictator Game" = "DG",
                  "Public Good Game" = "PGG",
                  "Prisoner's Dilemma Game" = "PDG",
                  "Trust Games" = "TG",
                  "Ultimatum Game" = "UG",
                  "Gift Exchange Game" = "GEG",
                  "Take or Give Game" = "ToG",
                  "All Games" = "All"
                  ),
               ),
    
    checkboxInput("WP", "Show Working Paper?", FALSE)
    ),
  
  # Main panel for displaying outputs ----
  mainPanel()
)

# Define server logic to plot various variables against Network App ----
server <- function(input, output) {
  
}


shinyApp(ui, server)
