# Childhood Trauma Questionnaire (CTQ) to Adverse Childhood Experience (ACE) transformation
# For a sample that has taken both questionnaires, this app displays the number of true/false positives and negatives
# as well as the sensitivity, specificity, positive, and negative predictive value for a variety of thresholding options

# load packages
library(tidyverse)
library(plotly)
library(ggplot2)
library(shiny)
source("~/Box\ Sync/code_cheng_ohsu/ctq-to-aces/shiny/helper.R")

# load data and information about each subscale
df_items <- readRDS("~/Box\ Sync/code_cheng_ohsu/ctq-to-aces/shiny/df_items.rds")
subscale_info <- readRDS("~/Box\ Sync/code_cheng_ohsu/ctq-to-aces/shiny/subscale_info.rds")

# describe the app options
pred_ops <- c("Item-based thresholding", "Subscale-based thresholding", "Logistic regression")
#subscale_options <- c("numeric", "cost-ratio", "max")
items <- subscale_info[[1]]


ui <- shinyUI(
  fluidPage( # fluid page -- app adjusts to browser window size, this is a want!
  
    titlePanel(
      h3("Predicting ACE endorsement from CTQ scores")
      ),
  
    sidebarLayout(
      position = "right", 
      sidebarPanel(
        #fileInput("File", "Choose csv file to upload", accept = ".csv"),
       # p("Each subscale can be subse"),
        checkboxGroupInput("subscale", 
                           h4("Subscale"), 
                           choices = list("Emotional abuse" = 1,   # subscale_info[[1]][1], 
                                          "Physical abuse" = 2,   # subscale_info[[1]][2], 
                                          "Sexual abuse" = 3,     # subscale_info[[1]][3],
                                          "Emotional neglect" = 4,  # subscale_info[[1]][4],
                                          "Physical neglect" =  5),
                                          selected = 1), # subscale_info[[1]][5], selected = 1))),
        selectInput("pred_ops", h4("Prediction method"), pred_ops, selected=pred_ops[1]),
        h6("If using \"Item-based thresholding\", choose a threshold below:"),
        sliderInput("item_thresh", h4("Item threshold"), 
                    min = 1, max = 5, value = 3),
        h6("If using \"Subscale-based thresholding\", choose a threshold below:"),
        sliderInput("subscale_thresh", h4("Subscale threshold"),
                    min = 5, max = 15, value = 10)),
      
      mainPanel(
        p("Two of the most common surveys used to evaluate childhood adversity are the Childhood Trauma Questionaire (CTQ) and Adverse Childhood Experience (ACE) questionnaire. For each of five maltreatment subscales, the CTQ has multiple items corresponding to a single ACE item."),
        p("To test the correspondence of these scales, we apply different transformations of CTQ scores into a predicted ACE score and evaluate the quality of these predictions. The data are from a sample of 449 young adults who took both scales (Teicher & Parriger, 2015)."),
        tags$li("Item-based thresholding: we predict ACE endorsement if "),
        tags$li("Subscale-based thresholding:"),
        tags$li("Logistic regression:"),
        br(),
        h4("Tables & Figures"),
        tableOutput("TF_pos_neg")
        #plotOutput("SSPN"),
        #plotOutput("ROC"))
        ))))


server <- function(input, output) {
  
    output$TF_pos_neg <- renderTable({
      idx = as.numeric(input$subscale)
      subscale = subscale_info[[1]][idx]
      ace_items = subscale_info[[2]][idx]
      ctq_items = subscale_info[[3]][[idx]]
      thresh_num = input$item_thresh
      
      make_confMatrix_table(df_items, ace_items, ctq_items, thresh_num)
      })
    
}

shinyApp(ui, server)
