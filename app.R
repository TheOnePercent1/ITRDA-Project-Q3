library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Preferred Tools by Graduates (DashBoard)"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("User Guide", tabName = "guide", icon = icon("book")),
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      selected = "guide"
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Preferred Tool Table:", status = "primary",
                  solidHeader = TRUE, width = 4,
                  selectInput("tool_option", "Select option:",
                              choices = c("Programming Languages" = "prog_lang",
                                          "Databases" = "databases",
                                          "AI Search Tools" = "aisearch",
                                          "AI Tools" = "aitool",
                                          "Web Frameworks" = "framework",
                                          "Platforms" = "platform"),
                              selected = "prog_lang"),
                  tableOutput('tool_table')
                ),
                box(
                  title = "Plot:", status = "primary",
                  solidHeader = TRUE, width = 8,
                  plotlyOutput("tool_plot", height = 450)
                )
              )
      ),
      tabItem(tabName = "guide",
              fluidRow(
                box(
                  title = "User Guide:", status = "info", solidHeader = TRUE, width = 6,
                  h1("User Guide:"),
                  h3("How to use the Dashboard:"),
                  tags$ul(
                    tags$li("Click on the Dashboard tab on the left hand side to view the Tool plots and tables."),
                    tags$li("On the dashboard tab there will be a dropdown list where you will be able to chose the Tool to view."),
                    tags$li("On the left the table of the chosen tool will appear with the top 10 most used tools and on the right there will be a plot to visualise these numbers."),
                    tags$li("You can switch the Tool you have chosen to look at different Tools.")
                  ),
                  h3("The Different types of Tools you will be able to view are:"),
                  tags$ul(
                    tags$li("Programming Languages"),
                    tags$li("Databases"),
                    tags$li("AI Search Tools"),
                    tags$li("AI Tools"),
                    tags$li("Web Frameworks"),
                    tags$li("Platforms")
                  )
                )
              )
        
      )
    )
  )
)

server <- function(input, output, session) {
  
  graduate_selected_final <- read.csv("graduate_survey_final.csv")
  
  top10_prog_lang <- reactive({
    graduate_selected_final %>% 
      mutate(ProgLang = str_split(ProgLang, ";")) %>% 
      unnest(c(ProgLang)) %>% 
      group_by(ProgLang = str_trim(ProgLang)) %>% 
      summarise(count = n()) %>% 
      slice_max(count, n = 10) %>% 
      arrange(desc(count))
  })
  
  top10_databases <- reactive({
    graduate_selected_final %>% 
      mutate(Databases = str_split(Databases, ";")) %>% 
      unnest(c(Databases)) %>% 
      group_by(Databases = str_trim(Databases)) %>% 
      summarise(count = n()) %>% 
      slice_max(count, n = 10) %>% 
      arrange(desc(count))
  })
  
  top10_aisearch <- reactive({
    graduate_selected_final %>% 
      mutate(AISearch = str_split(AISearch, ";")) %>% 
      unnest(c(AISearch)) %>% 
      group_by(AISearch = str_trim(AISearch)) %>% 
      summarise(count = n()) %>% 
      slice_max(count, n = 10) %>% 
      arrange(desc(count))
  })
  
  top10_aitool <- reactive({
    graduate_selected_final %>% 
      mutate(AITool = str_split(AITool, ";")) %>% 
      unnest(c(AITool)) %>% 
      group_by(AITool = str_trim(AITool)) %>% 
      summarise(count = n()) %>% 
      slice_max(count, n = 10) %>% 
      arrange(desc(count))
  })
  
  top10_framework <- reactive({
    graduate_selected_final %>% 
      mutate(WebFramework = str_split(WebFramework, ";")) %>% 
      unnest(c(WebFramework)) %>% 
      group_by(WebFramework = str_trim(WebFramework)) %>% 
      summarise(count = n()) %>% 
      slice_max(count, n = 10) %>% 
      arrange(desc(count))
  })
  
  top10_platform <- reactive({
    graduate_selected_final %>% 
      mutate(Platform = str_split(Platform, ";")) %>% 
      unnest(c(Platform)) %>% 
      mutate(Platform = str_trim(Platform),
             Platform = case_when(
               Platform == "Amazon Web Services (AWS)" ~ "Amazon Web Services",
               TRUE ~ Platform)) %>% 
      group_by(Platform) %>% 
      summarise(count = n()) %>% 
      slice_max(count, n = 10) %>% 
      arrange(desc(count))
  })
  
  output$tool_table <- renderTable({
    switch(input$tool_option,
           "prog_lang" = top10_prog_lang(),
           "databases" = top10_databases(),
           "aisearch" = top10_aisearch(),
           "aitool" = top10_aitool(),
           "framework" = top10_framework(),
           "platform" = top10_platform())
  })
  
  output$tool_plot <- renderPlotly({
    switch(input$tool_option,
           "prog_lang" = {
             plot <- ggplot(top10_prog_lang(), aes(x = reorder(ProgLang, -count), y = count, fill = ProgLang)) +
              geom_col() +
              labs(title = "Top Programming Languages",
                    x = "Programming Languages",
                    y = "Count") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(size = 14),
                    plot.title = element_text(size = 16))
             ggplotly(plot, tooltip = "y")
           },
           
           "databases" = {
             plot <- ggplot(top10_databases(), aes(x = reorder(Databases, -count), y = count, fill = Databases)) +
              geom_col() +
              labs(title = "Top Database Tools",
                    x = "Database Tools",
                    y = "Count") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(size = 14),
                    plot.title = element_text(size = 16))
             ggplotly(plot, tooltip = "y")
           },
           
           "aisearch" = {
             plot <- ggplot(top10_aisearch(), aes(x = reorder(AISearch, -count), y = count, fill = AISearch)) +
              geom_col() +
              labs(title = "Top AI Search Tools", 
                    x = "AI Search Tools",
                    y = "Count") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(size = 14),
                    plot.title = element_text(size = 16))
             ggplotly(plot, tooltip = "y")
           },
           
           "aitool" = {
             plot <- ggplot(top10_aitool(), aes(x = reorder(AITool, -count), y = count, fill = AITool)) +
              geom_col() +
              labs(title = "Top AI Tools",
                    x = "AI Tools",
                    y = "Count") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(size = 14),
                    plot.title = element_text(size = 16))
             ggplotly(plot, tooltip = "y")
           },
           
           "framework" = {
             plot <- ggplot(top10_framework(), aes(x = reorder(WebFramework, -count), y = count, fill = WebFramework)) +
              geom_col() +
              labs(title = "Top Frameworks",
                    x = "Frameworks",
                    y = "Count") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(size = 14),
                    plot.title = element_text(size = 16))
             ggplotly(plot, tooltip = "y")
           },
           
           "platform" = {
             plot <- ggplot(top10_platform(), aes(x = reorder(Platform, -count), y = count, fill = Platform)) +
              geom_col() +
              labs(title = "Top Platforms",
                    x = "Platforms",
                    y = "Count") +
              theme_minimal() +
              theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
                    axis.text.y = element_text(size = 12),
                    axis.title = element_text(size = 14),
                    plot.title = element_text(size = 16))
             ggplotly(plot, tooltip = "y")
           }
    )
  })
}
  
shinyApp(ui, server)