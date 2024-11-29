library(shiny)
library(dotenv)
library(httr)
library(jsonlite)
library(DT)
library(ggplot2)

# CARREGA VARIAVIES DE AMBIENTE
dotenv::load_dot_env(file = ".env")
REDCAP_API_URL <- Sys.getenv("REDCAP_API_URL")
REDCAP_API_KEY <- Sys.getenv("REDCAP_API_KEY")

# INCLUI ARQUIVOS NECESSARIOS
source("functions.r")

# INTERFACE DO USUÀRIO
ui <- fluidPage(
    titlePanel("Teste Shiny + REDCAp"),
    includeCSS("css/styles.css"),
    sidebarLayout(
        sidebarPanel(
            tags$div(
                style = "display: flex; align-items: center; gap: 4rem; justify-content: center; margin: 40px 0 20px 0;", 
                tags$img(src = "/assets/images/shiny.png", height = "100px", width = "auto"), 
                tags$img(src = "/assets/images/redcap.png", height = "50px", width = "auto"),
            ),
            selectInput("gender_filter", 
                        "Filtrar por Gênero:",
                        choices = c("Todos" = "Todos", "Masculino" = 1, "Feminino" = 0),
                        selected = "Todos"),
            sliderInput("age_filter", 
                        "Filtrar por Faixa de Idade:", 
                        min = 0, max = 100, value = c(0, 100), step = 1),
            actionButton("reset_filters", "Reiniciar Filtros", class = "reset")
        ),
        mainPanel(
            fluidRow(
                column(6, plotOutput("gender_plot")), 
                column(6, plotOutput("age_plot"))    
            ),
            fluidRow(
                column(12, 
                    tagList(
                        h3("DataSet Analisado"), 
                        dataTableOutput("data_preview")
                    )
                )   
            )
        )
    )
)

# SERVER
server <- function(input, output, session) {

    # BUSCA DADOS NA API
    dataset <- reactive({
        fetch_redcap_data(REDCAP_API_URL, REDCAP_API_KEY)
    })

    # FILTRA DADOS RETORNADOS PELA API
    filtered_data <- reactive({

        data <- dataset()  

        # CONVERTE COLUNA AGE PARA NUMERICO
        data$age <- as.numeric(data$age)

        # FILTRO POR SEXO
        if (input$gender_filter != "Todos") {
            data <- data[data$gender == input$gender_filter, ]
        }

        # FILTRO POR FAIXA DE IDADE
        data <- data[data$age >= input$age_filter[1] & data$age <= input$age_filter[2], ]
 
        # SELECIONA AS COLUNAS QUE QUEREMOS EXIBIR
        data <- data[, c("full_name", "birth_date", "age", "gender")]

        # TRATA A DATA DE NASCIMENTO (DD/MM/YYYY)
        data$birth_date <- as.Date(data$birth_date, format = "%Y-%m-%d") 
        data$birth_date <- format(data$birth_date, "%d/%m/%Y") 

        # TRATA O CAMPO GENDER 0/1 PARA "Feminino" E "Masculino"
        data$gender <- ifelse(data$gender == 0, "Feminino", "Masculino")

        return(data)
    })

    # GRÁFICO DE GÊNERO
    output$gender_plot <- renderPlot({
        data <- filtered_data()
        if (nrow(data) > 0) {
            ggplot(data, aes(x = gender)) +
                geom_bar(fill = "#BB86FC", color = "#1E1E1E") +
                labs(title = "Distribuição por Gênero", x = "Gênero", y = "Quantidade") +
                theme_minimal() +
                theme(
                    plot.title = element_text(color = "#E1E1E1", size = 16, face = "bold", hjust = 0.5),
                    axis.title.x = element_text(color = "#E1E1E1", size = 14, face = "bold"),
                    axis.title.y = element_text(color = "#E1E1E1", size = 14, face = "bold"),
                    axis.text.x = element_text(color = "#E1E1E1", size = 13),  
                    axis.text.y = element_text(color = "#E1E1E1", size = 13),
                    panel.background = element_rect(fill = "#1E1E1E"), 
                    plot.background = element_rect(fill = "#1E1E1E", color = "black") 
                )
        }
    })

    # GRÁFICO DE IDADE
    output$age_plot <- renderPlot({
        data <- filtered_data()
        if (nrow(data) > 0) {
            ggplot(data, aes(x = factor(age))) + 
                geom_bar(fill = "#BB86FC", color = "#1E1E1E") +
                labs(title = "Distribuição por Idade", x = "Idade", y = "Quantidade") +
                theme_minimal() +
                theme(
                    plot.title = element_text(color = "#E1E1E1", size = 16, face = "bold", hjust = 0.5),
                    axis.title.x = element_text(color = "#E1E1E1", size = 14, face = "bold"),
                    axis.title.y = element_text(color = "#E1E1E1", size = 14, face = "bold"),
                    axis.text.x = element_text(color = "#E1E1E1", size = 13),  
                    axis.text.y = element_text(color = "#E1E1E1", size = 13 ),
                    panel.background = element_rect(fill = "#1E1E1E"),  
                    plot.background = element_rect(fill = "#1E1E1E", color = "#1E1E1E") 
                )
        }
    })
 
    # MOSTRA DADOS FILTRADOS USANDO DT
    output$data_preview <- renderDataTable({
        datatable(
            filtered_data(),
            options = list(
                pageLength = 10,
                language = list(
                    url = "https://cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json"
                )
            )
        )
    })

    # REINICIA OS FILTROS QUANDO O BOTÃO FOR CLICADO
    observeEvent(input$reset_filters, {
        updateSelectInput(session, "gender_filter", selected = "Todos")
        updateSliderInput(session, "age_filter", value = c(0, 100))
    })

}

shinyApp(ui, server)
