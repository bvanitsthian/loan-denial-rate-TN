#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Chances of denial"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Race",
                        "select your race",
                        list("White",
                             "Asian",
                             "Black or African American",
                             "American Indian or Alaska Native",
                             "Native Hawaiian or Other Pacific Islander")),
            selectInput("DTI",
                        "select your Debt to Income group",
                        list("<20%",
                             "20%-<30%",
                             "30%-<40%",
                             "40%-<50%",
                             "50%-60%",
                             ">60%")),
            selectInput("Age",
                        "select your age group",
                        list("<25",
                             "25-34",
                             "35-44",
                             "45-54",
                             "55-64",
                             "65-74",
                             ">74")),
            numericInput("Income",
                        "select your annual income in 1000 USD",
                        50),
            selectInput("Sex",
                        "select your birth sex",
                        list("Male",
                             "Female",
                             "Joint",
                             "Sex Not Available"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
     load('/Users/thidathornvanitsthian/Documents/Data Science 2022/loan-denial-rate-TN/Data/glm_ft')
    
    output$text <- renderText({
      
      user_data <- data.frame(derived_race = factor(input$Race),
                              debt_to_income_group = factor(input$DTI),
                              derived_sex = factor(input$Sex),
                              applicant_age = factor(input$Age),
                              income = input$Income)

      x <-
        predict(glm_ft, user_data, type = "response")
      paste0("You have a ", 100*round(x, digits = 2),"% probability of being denied a mortgage loan in TN")


    })
    
  
    
    
    
    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
