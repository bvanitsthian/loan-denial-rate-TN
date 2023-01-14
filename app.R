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
    titlePanel("Mortgage Loan Approval Probability Calculator in Tennessee"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Race",
                        "Select your race",
                        list("White",
                             "Asian",
                             "Black or African American",
                             "American Indian or Alaska Native",
                             "Native Hawaiian or Other Pacific Islander")),
            selectInput("DTI",
                        "Select your Debt to Income group",
                        list("<20%",
                             "20%-<30%",
                             "30%-<40%",
                             "40%-<50%",
                             "50%-60%",
                             ">60%")),
            selectInput("Age",
                        "Select your age group",
                        list("<25",
                             "25-34",
                             "35-44",
                             "45-54",
                             "55-64",
                             "65-74",
                             ">74")),
            numericInput("Income",
                        "Select your annual income (x 1000 USD)",
                        50),
            selectInput("Sex",
                        "Select your birth sex",
                        list("Male",
                             "Female",
                             "Joint",
                             "Sex Not Available")),
            selectInput("Graph_Var",
                        "Select your variable",
                        list("derived_race",
                             "applicant_age",
                             "derived_sex"))
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("text"),
            plotOutput("distPlot")
        )
         
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
     load('/Users/thidathornvanitsthian/Documents/Data Science 2022/loan-denial-rate-TN/Data/glm_ft')
     load('/Users/thidathornvanitsthian/Documents/Data Science 2022/loan-denial-rate-TN/Data/mortgage_loan_denial_reasons')
    
    output$text <- renderText({
      
      user_data <- data.frame(derived_race = factor(input$Race),
                              debt_to_income_group = factor(input$DTI),
                              derived_sex = factor(input$Sex),
                              applicant_age = factor(input$Age),
                              income = input$Income)

      x <-
        predict(glm_ft, user_data, type = "response", se.fit = T)
      #paste0("You have a ", 100*round(1-x$fit, digits = 2),"% probability of being approved a mortgage loan in TN")
      paste0("You have between a ", 100*round(1-(x$fit+x$se.fit), digits = 2), 
      " and a ", 100*round(1-(x$fit-x$se.fit), digits = 2),
      "% probability of being approved a mortgage loan in TN")
      

    })

    output$distPlot <- renderPlot({
        ggplot(mortgage_loan_denial_reasons) +
        geom_col(aes(x=denial_reason_code, fill=factor(input$Graph_Var)), position="dodge")

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
