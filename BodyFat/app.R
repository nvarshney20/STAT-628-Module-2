library(shiny)

data <- read.csv('BodyFat.csv')
data <- data[-c(39, 172, 182),]
data$HEIGHT[41] <- 69.5

linreg <- lm(BODYFAT ~ ABDOMEN + WRIST + HEIGHT, data = data)

ui <- fluidPage(
    titlePanel("Body Fat Percentage Calculator (ADULT MALES ONLY)"),
    sidebarLayout(
        sidebarPanel(
            textInput("abdomen", "Abdomen Circumference (cm)"),
            textInput("wrist", "Wrist Circumference (cm)"),
            textInput("height", "Height (in)")
        ),
        mainPanel(
            h3("Point Estimate"),
            textOutput("point_est"),
            h3("Interval"),
            textOutput("pred_int")
        )
    )
)

server <- function(input, output) {
    output$point_est <- renderText(
        predict(
            linreg,
            data.frame(
                ABDOMEN = as.numeric(input$abdomen),
                WRIST = as.numeric(input$wrist),
                HEIGHT = as.numeric(input$height)
            )
        )
    )
    output$pred_int <- renderText({
        prediction <- predict(
            linreg,
            data.frame(
                ABDOMEN = as.numeric(input$abdomen),
                WRIST = as.numeric(input$wrist),
                HEIGHT = as.numeric(input$height)
            ),
            interval = "prediction"
        )
        paste("[", prediction[, "lwr"], ", ", prediction[, "upr"], "]", sep = "")
    })
}

shinyApp(ui = ui, server = server)