# Childhood Trauma Questionnaire (CTQ) to Adverse Childhood Experience (ACE) transformation
# For a sample that has taken both questionnaires, this app displays the number of true/false positives and negatives
# as well as the sensitivity, specificity, positive, and negative predictive value for a variety of thresholding options

# load packages, install as needed
#devtools::install_github("sachsmc/plotROC")
packages = c("tidyverse", "shiny", "wesanderson", "ggplot2", "kableExtra", "plotROC")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE) }})

# source helper functions
source("~/Box\ Sync/ctq-to-aces/ctq_to_aces_shiny/helper.R")

# load data and information about each subscale
df_items <- readRDS("~/Box\ Sync/ctq-to-aces/ctq_to_aces_shiny/df_items.rds")
subscale_info <- readRDS("~/Box\ Sync/ctq-to-aces/ctq_to_aces_shiny/subscale_info.rds")

# options
items <- subscale_info[[1]]
pred_ops <- c("Item-based thresholding", "Subscale-based thresholding", "Logistic regression")


ui <- shinyUI(
  fluidPage( # fluid page -- app adjusts to browser window size, this is a want!
    titlePanel(h3("Predicting ACE endorsement from CTQ scores")),
  
    sidebarLayout(
      position = "right", 
      sidebarPanel(
        selectInput("subscale", h4("Subscale"), 
                     choices = list("Emotional abuse" = 1,  
                                    "Physical abuse" = 2,   
                                    "Sexual abuse" = 3,   
                                    "Emotional neglect" = 4,
                                    "Physical neglect" =  5),
                                    selected = 1), 
        selectInput("pred_ops", h4("Prediction method"), pred_ops, selected=pred_ops[1]),
          conditionalPanel(
            condition = "input.pred_ops == \"Item-based thresholding\"",
              sliderInput("item_thresh", h4("Item threshold"), 
                          min = 1, max = 5, value = 3)),
          conditionalPanel(
            condition = "input.pred_ops == \"Subscale-based thresholding\"",
              sliderInput("subscale_thresh", h4("Subscale threshold"),
                          min = 5, max = 15, value = 10)),
          conditionalPanel(
            condition = "input.pred_ops == \"Logistic regression\"",
              sliderInput("lr_thresh", h4("Logistic regression threshold"),
                          min = 0, max = 1, value = .4))),

      mainPanel(
        p("Two of the most common surveys used to evaluate childhood adversity are the Childhood Trauma Questionaire (CTQ) and Adverse Childhood Experience (ACE) questionnaire. For each of five maltreatment subscales, the CTQ has multiple items corresponding to a single ACE item."),
        p("To test the correspondence of these scales, we apply different transformations of CTQ scores into a predicted ACE score and evaluate the quality of these predictions. The data are from a sample of 449 young adults who took both scales (Teicher & Parriger, 2015)."),
        tags$li("Item-based thresholding: we predict ACE endorsement if "),
        tags$li("Subscale-based thresholding:"),
        tags$li("Logistic regression:"),
        br(),
        h4("Tables & Figures"),
        splitLayout(
          plotOutput("SSPN"),
          tableOutput("TF_pos_neg")),
        plotOutput("ROC"))
  )))


server <- function(input, output) {
  
  dataInput <- reactive({
    eval_subscale <- subscale_info[[1]][as.numeric(input$subscale)]
    ace_items <- subscale_info[[2]][as.numeric(input$subscale)]
    ctq_items <- subscale_info[[3]][[as.numeric(input$subscale)]]

    if (input$pred_ops == "Item-based thresholding") {
      thresh_num = input$item_thresh
      confMatrixInput <- make_confMatrix_item(df_items, ace_items, ctq_items, thresh_num)
    } else if (input$pred_ops == "Subscale-based thresholding") {
      thresh_num = input$subscale_thresh
      confMatrixInput <- make_confMatrix_subscale(df_items, ace_items, ctq_items, thresh_num)
    } else if (input$pred_ops == "Logistic regression"){
      thresh_num = input$lr_thresh
      confMatrixInput <- make_confMatrix_LR(df_items, ace_items, ctq_items, thresh_num, subscale)
    }
  })
  
  output$TF_pos_neg <- function () {
    dataInput()
    
    # create a matrix
    confMatrix_cro <- matrix(c(confMatrix$Number[4], confMatrix$Number[2], confMatrix$Number[3], confMatrix$Number[1]), ncol = 2, byrow = TRUE)
    colnames(confMatrix_cro) <- c("ACE: TRUE", "ACE: FALSE")
    rownames(confMatrix_cro) <- c("Pred ACE: TRUE", "Pred ACE: FALSE")
    
    # display in html
    kable(as.table(confMatrix_cro), "html") %>%
      kable_styling("striped", full_width = F)
    }

  output$SSPN <- renderPlot({
    subscale <- subscale_info[[1]][as.numeric(input$subscale)]
    dataInput()
    sens_spec_ppv_npv(confMatrix, subscale, return_df = 1)
    
    ggplot(stats, aes(x = V1, y = Number, fill = V1)) + 
      geom_col(width = .95) +
      coord_flip() +
      xlab("Statistic") + ylab("Percent") + 
      scale_x_discrete(breaks = c("sens", "spec", "PPV", "NPV"), labels = c("Sens", "Spec", "PPV", "NPV"), expand = c(.01,.01)) +
      scale_y_continuous(breaks = seq(0, 1, by = .1), labels = seq(0, 100, by = 10), expand = c(0, 0)) +
      expand_limits(y=1.03) +
      theme(legend.position = "none",
            text = element_text(face = "bold", size = 14), 
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0)), 
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0)), 
            panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gainsboro")) + 
      scale_fill_manual(values = wes_palette("Zissou1"))
  })

  output$ROC <- renderPlot({
    dataInput()
    sens_spec_ppv_npv(confMatrix, subscale, return_df = 1)
    
    if (input$pred_ops == "Subscale-based thresholding") {
      roc_plot_clean <- roc_plot(df = df_items, subscale = subscale_info[[1]][as.numeric(input$subscale)], ace_items = subscale_info[[2]][as.numeric(input$subscale)], ctq_items = subscale_info[[3]][[as.numeric(input$subscale)]])
      roc_plot_clean + 
        geom_vline(xintercept = (1-stats[[2,2]]), colour="#3B9AB2", linetype="dashed", size = 1.5)
    }
})
}


shinyApp(ui, server)

# general tutorial: https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
# cheat sheet: https://www.rstudio.com/wp-content/uploads/2016/01/shiny-cheatsheet.pdf
# conditional panels: https://shiny.rstudio.com/articles/dynamic-ui.html 