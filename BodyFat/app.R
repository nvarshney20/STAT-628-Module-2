library(shiny)

data <- read.csv('BodyFat.csv')
data <- data[-c(39, 172, 182),]
data$HEIGHT[41] <- 69.5
data$HEIGHT <- round(2.54 * data$HEIGHT, 1)

linreg <- lm(BODYFAT ~ ABDOMEN + WRIST + HEIGHT, data = data)

ui <- fluidPage(
    titlePanel("Body Fat Percentage Calculator (ADULT MALES ONLY)"),
    sidebarLayout(
        sidebarPanel(
            textInput("abdomen", "Abdomen Circumference (cm)"),
            textInput("wrist", "Wrist Circumference (cm)"),
            textInput("height", "Height (cm)")
        ),
        mainPanel(
            h3("Body Fat Percentage Estimate"),
            textOutput("point_est"),
            h3("Category"),
            textOutput("bfp_cat"),
        )
    )
)

server <- function(input, output) {
    output$point_est <- renderText({
        prediction <- predict(
            linreg,
            data.frame(
                ABDOMEN = as.numeric(input$abdomen),
                WRIST = as.numeric(input$wrist),
                HEIGHT = as.numeric(input$height)
            )
        )
        if(is.na(prediction)) "Enter information"
        else if(prediction < 0) 0
        else if(prediction > 100) 100
        else round(prediction, 2)
    })
    output$bfp_cat <- renderText({
        prediction <- predict(
            linreg,
            data.frame(
                ABDOMEN = as.numeric(input$abdomen),
                WRIST = as.numeric(input$wrist),
                HEIGHT = as.numeric(input$height)
            )
        )
        if(is.na(prediction)) "Enter information"
        else if(prediction < 2) "Too low. Check if input is correct."
        else if(prediction < 6) "Dangerously low"
        else if(prediction < 14) "Athletic"
        else if(prediction < 18) "Fit"
        else if(prediction < 25) "Average"
        else if(prediction < 50) "Obese"
        else "Too high. Check if input is correct."
    })
}

shinyApp(ui = ui, server = server)