# Childhood Trauma Questionnaire (CTQ) to Adverse Childhood Experience (ACE) transformation
# For a sample that has taken both questionnaires, this app displays the number of true/false positives and negatives as well as the sensitivity, specificity, positive, and negative predictive value for a variety of thresholding options

# load packages, install as needed
#devtools::install_github("sachsmc/plotROC")
#library("plotROC")
# library("dplyr")
#library("tidyr")
#library("shiny")
#library("wesanderson")
#library("ggplot2")
#library("kableExtra")
#library("plotROC")
#library("caret")
#library("data.table")

packages = c("dplyr", "tidyr", "shiny", "wesanderson", "ggplot2", "kableExtra", "plotROC", "caret", "data.table")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    #install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE) }})

library("plotROC")

setwd("/Users/theresacheng/Box/ctq-to-aces/ctq_to_aces_shiny")

# source helper functions
source("helper.R") #/Users/theresacheng/Box/ctq-to-aces/ctq_to_aces_shiny

# load data and information about each subscale
df_items <- readRDS("df_items_relabeled.rds")
df_test <- readRDS("teicher_validation_long.rds")
subscale_info <- readRDS("subscale_info_relabeled.rds")

# options
items <- subscale_info[[1]]
pred_ops <- c("Item-based thresholding", "Subscale-based thresholding", "Linear support-vector machine")

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
        selectInput("pred_ops", h4("Prediction method"), pred_ops, selected=pred_ops[2]),
        conditionalPanel(
          condition = "input.pred_ops == \"Item-based thresholding\"",
          sliderInput("item_thresh", h4("Item threshold"), 
                      min = 1, max = 5, value = 3)),
        conditionalPanel(
          condition = "input.pred_ops == \"Subscale-based thresholding\"",
          sliderInput("subscale_thresh", h4("Subscale threshold"),
                      min = 5, max = 20, value = 10)),
        conditionalPanel(
          condition = "input.pred_ops == \"Linear support-vector machine\""),
        checkboxInput("showTestSample", "Display validation sample", FALSE)),
    
      mainPanel(
        p("Two of the most common surveys used to evaluate childhood adversity are the Childhood Trauma Questionaire (CTQ) and Adverse Childhood Experience (ACE) questionnaire. For each of five maltreatment subscales, the CTQ has multiple items corresponding to a single ACE item."),
        p("To test the correspondence of these scales, we apply different transformations of CTQ scores into a predicted ACE score and evaluate the quality of these predictions. The data are from a sample of 449 young adults who took both scales (Teicher & Parriger, 2015)."),
        tags$li("Item-based thresholding: Predict ACE endorsement if participants respond at or above threshold on one or more items on the corresponding CTQ subscale; thresholds are: (1) Never, (2) Rarely, (3) Sometimes, (4) Often, (5) Very Often"),
        tags$li("Subscale-based thresholding: Predict ACE endorsement if participants respond at or above a summed value on the corresponding CTQ subscale"),
        tags$li("Linear support-vector machine: Predict ACE endorsement from a machine learning algorithm based on items from the corresponding CTQ subscale"),
        br(),
        h3("Tables & Figures"),
        
        conditionalPanel(
          condition = "input.showTestSample == true",
          h4("Validation Dataset"),
          splitLayout(
            plotOutput("SSPN_test"),
            tableOutput("TF_pos_neg_test"))),
        
        h4("Original Dataset"),
        splitLayout(
          plotOutput("SSPN"),
          tableOutput("TF_pos_neg")),
        br(),
        plotOutput("ROC")
    ))))

server <- function(input, output) {
  
  dataInput <- reactive({
    eval_subscale <- subscale_info[[1]][as.numeric(input$subscale)]
    ace_items <- subscale_info[[2]][as.numeric(input$subscale)]
    ctq_items <- subscale_info[[3]][[as.numeric(input$subscale)]]

    if (input$pred_ops == "Item-based thresholding") {
      thresh_num = input$item_thresh
      make_confMatrix_item(df_items, ace_items, ctq_items, thresh_num, "train") #confMatrixInput <- 
      make_confMatrix_item(df_test, ace_items, ctq_items, thresh_num, "test") #confMatrixInput_test <- 
      
    } else if (input$pred_ops == "Subscale-based thresholding") {
      thresh_num = input$subscale_thresh
      make_confMatrix_subscale(df_items, ace_items, ctq_items, thresh_num, "train")
      make_confMatrix_subscale(df_test, ace_items, ctq_items, thresh_num, "test")
      
    } else if (input$pred_ops == "Linear support-vector machine"){
      train_set <- gen_train_set(df_items, ace_items, ctq_items)
      model_svm <- gen_svm_model(train_set, ace_items, ctq_items)
      conf_matrix_train <- gen_accuracy(model_svm, train_set, ace_items, ctq_items)
      gen_confMatrix_SVM(conf_matrix_train, "train")
      
      test_set <- gen_test_set(df_test, ace_items, ctq_items)
      conf_matrix_test <- gen_accuracy(model_svm, test_set, ace_items, ctq_items)
      gen_confMatrix_SVM(conf_matrix_test, "test")
    }
  })
  
  output$TF_pos_neg <- function () {
    dataInput()
    
    # create a matrix
    confMatrix_cro <- matrix(c(confMatrix$Number[4], confMatrix$Number[2], confMatrix$Number[3], confMatrix$Number[1]), ncol = 2, byrow = TRUE)
    colnames(confMatrix_cro) <- c("ACE: TRUE", "ACE: FALSE")
    rownames(confMatrix_cro) <- c("Predicted ACE: TRUE", "Predicted ACE: FALSE")
    
    # display in html
    kable(as.data.frame(confMatrix_cro), "html", caption = paste("Number of subjects per category for predicted", subscale_info[[1]][as.numeric(input$subscale)])) %>%
      kable_styling("striped", full_width = F)
    }

  output$SSPN <- renderPlot({
    subscale <- subscale_info[[1]][as.numeric(input$subscale)]
    dataInput()
    sens_spec_ppv_npv(confMatrix, subscale, return_df = 1)
    
    stats$V1 = factor(stats$V1, levels = c("NPV", "PPV", "spec", "sens"))
    #stats$V1 = factor(stats$V1, levels = c("sens", "spec", "PPV", "NPV"), labels = c("Sens", "Spec", "PPV", "NPV"))
    
    ggplot(stats, aes(x = V1, y = Number, fill = V1)) + 
      geom_col(width = .95) +
      coord_flip() +
      xlab("Statistic") + ylab("Percent") + 
      scale_x_discrete(breaks = c("sens", "spec", "PPV", "NPV"), labels = c("Sens", "Spec", "PPV", "NPV"), expand = c(.01,.01)) +
      scale_y_continuous(breaks = seq(0, 1, by = .1), labels = seq(0, 100, by = 10), expand = c(0, 0)) +
      expand_limits(y=1.12) +
      theme(legend.position = "none",
            text = element_text(face = "bold", size = 14), 
            title = element_text(size = 12, face = "plain", color = "gray30"),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 16), 
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 16), 
            panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gainsboro")) + 
      scale_fill_manual(values = wes_palette("Zissou1")) + 
      geom_text(
        aes(label = round(Number,4)*100 , y = Number + 0.08),
        position = position_dodge(0.9),
        vjust = 0,
        size = 4
      ) + 
      ggtitle(paste("Sensitivity, specificity, positive and negative \npredictive value (PPV, NPV) for", subscale))
  })

  output$SSPN_test <- renderPlot({
    subscale <- subscale_info[[1]][as.numeric(input$subscale)]
    dataInput()
    sens_spec_ppv_npv(confMatrix_test, subscale, return_df = 1)
    
    stats$V1 = factor(stats$V1, levels = c("NPV", "PPV", "spec", "sens"))
    #stats$V1 = factor(stats$V1, levels = c("sens", "spec", "PPV", "NPV"), labels = c("Sens", "Spec", "PPV", "NPV"))
    
    ggplot(stats, aes(x = V1, y = Number, fill = V1)) + 
      geom_col(width = .95) +
      coord_flip() +
      xlab("Statistic") + ylab("Percent") + 
      scale_x_discrete(breaks = c("sens", "spec", "PPV", "NPV"), labels = c("Sens", "Spec", "PPV", "NPV"), expand = c(.01,.01)) +
      scale_y_continuous(breaks = seq(0, 1, by = .1), labels = seq(0, 100, by = 10), expand = c(0, 0)) +
      expand_limits(y=1.12) +
      theme(legend.position = "none",
            text = element_text(face = "bold", size = 14), 
            title = element_text(size = 12, face = "plain", color = "gray30"),
            axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 16), 
            axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 16), 
            panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "gainsboro")) + 
      scale_fill_manual(values = wes_palette("Zissou1")) + 
      geom_text(
        aes(label = round(Number,4)*100 , y = Number + 0.08),
        position = position_dodge(0.9),
        vjust = 0,
        size = 4
      ) + 
      ggtitle(paste("Sensitivity, specificity, positive and negative \npredictive value (PPV, NPV) for", subscale))
  })
  
  output$TF_pos_neg_test <- function () {
    dataInput()
    
    # create a matrix
    confMatrix_cro_test <- matrix(c(confMatrix_test$Number[4], confMatrix_test$Number[2], confMatrix_test$Number[3], confMatrix_test$Number[1]), ncol = 2, byrow = TRUE)
    colnames(confMatrix_cro_test) <- c("ACE: TRUE", "ACE: FALSE")
    rownames(confMatrix_cro_test) <- c("Predicted ACE: TRUE", "Predicted ACE: FALSE")
    
    # display in html
    kable(as.data.frame(confMatrix_cro_test), "html", caption = paste("Number of subjects per category for predicted", subscale_info[[1]][as.numeric(input$subscale)])) %>%
      kable_styling("striped", full_width = F)
  }
  
  output$ROC <- renderPlot({
    dataInput()
    sens_spec_ppv_npv(confMatrix, subscale = subscale_info[[1]][as.numeric(input$subscale)], return_df = 1)
    
    if (input$pred_ops == "Subscale-based thresholding") {
      roc_plot_clean <- roc_plot(df = df_items, subscale = subscale_info[[1]][as.numeric(input$subscale)], ace_items = subscale_info[[2]][as.numeric(input$subscale)], ctq_items = subscale_info[[3]][[as.numeric(input$subscale)]])
      roc_plot_clean + 
        geom_vline(xintercept = (1-stats[[2,2]]), colour="#3B9AB2", linetype="dashed", size = 1.5) +
        ggtitle(paste("ROC plot for", subscale_info[[1]][as.numeric(input$subscale)])) + 
        theme(title = element_text(size = 12, face = "plain", color = "gray30"))
    }
})
}

shinyApp(ui, server)

# general tutorial: https://shiny.rstudio.com/tutorial/written-tutorial/lesson1/
# cheat sheet: https://www.rstudio.com/wp-content/uploads/2016/01/shiny-cheatsheet.pdf
# conditional panels: https://shiny.rstudio.com/articles/dynamic-ui.html 