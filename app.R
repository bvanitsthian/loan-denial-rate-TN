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
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Race",
                        "select your race",
                        list("White",
                             "Asian",
                             "Black or African American",
                             "American Indian or Alaska Native",
                             "Native Hawaiian or Other Pacific Islander"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("text")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    loan <- read_csv("~/Documents/Data Science 2022/Mid Course Project/Data/state_TN_actions_taken.csv")
    mortgage_loan <- loan %>%
      filter(loan_purpose == 1, 
             business_or_commercial_purpose == 2,
             occupancy_type == 1,
             applicant_age != "8888") %>%
      mutate(loan_app_denied = ifelse(action_taken==3,1,0)) %>%
      mutate(debt_to_income_group = case_when(
        debt_to_income_ratio == "30%-<36%" ~ "30%-<40%",
        as.numeric(debt_to_income_ratio) %in% 36:39 ~ "30%-<40%",
        as.numeric(debt_to_income_ratio) %in% 40:49 ~ "40%-<50%",
        TRUE ~ debt_to_income_ratio
      ))
    mortgage_loan_glm <- 
      mortgage_loan %>%
      filter(debt_to_income_group != "Exempt" & !is.na(debt_to_income_group),
             income > 0 & income < 200)
    
    glm_ft <- 
      glm(loan_app_denied ~ 
           # factor(applicant_age) + factor(debt_to_income_group) + income + factor(derived_sex) +
            factor(derived_race) -1, 
          family = binomial(link = "logit"),
          data = mortgage_loan_glm)
    #summary(glm_ft)
    
    current_race <- reactive(input$Race)
    user_data <- data.frame(derived_race = factor(current_race))
    
    mortgage_loan_glm$glm_ft_predict <- 
      predict(glm_ft, user_data, type = "response") 
    
    output$text <- renderText({
      paste0("You have a ", x ," probability of being denied a mortgage loan in TN")
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
