# functions needed to run ctq-to-aces script
# T Cheng | Written 042018, separated into new script 05102018

# generates predicted and endorsed ace scores for each participant
# prediction is based on exceeding a certain threshold on one or more items from that subscale

## for testing
#subscale = subscale_info[[1]][1]
#ace_items = subscale_info[[2]][1]
#ctq_items = subscale_info[[3]][[1]]
#thresh = 4

gen_item_thresh <- function(df, ace_items, ctq_items, thresh){
  
  # subset ace and ctq dataframes to the specified items
  ace_temp <- filter(df, item %in% ace_items)
  ctq_temp <-filter(df, item %in% ctq_items)
  
  # defined CTQ endorsement as score above threshold
  df$endorsed = NA
  ctq_temp$endorsed <- ifelse(ctq_temp$score >= thresh, 1, 0)
  ace_temp$endorsed <- ace_temp$score
  
  # temporary dataframe defined predicted_ace as endorsement on any relevant ctq item
  ctq_temp <- ctq_temp %>% 
    group_by(Username) %>%
    dplyr::summarise(sum = sum(endorsed, na.rm = TRUE)) %>%
    mutate(predicted_ace = ifelse(sum >= 1, 1, 0))
  
  df_item_thresh <- left_join(ace_temp, ctq_temp, by="Username")[c("Username","endorsed","predicted_ace")]
  colnames(df_item_thresh) = c("Username", "endorsed_ace", "predicted_ace")
  df_item_thresh$Username = as.character(df_item_thresh$Username)
  
  # transform ACE_endorsed from number --> 0/1 factor --> logical
  # as.logical only takes FALSE/TRUE character inputs
  df_item_thresh$endorsed_ace <- as.logical(factor(df_item_thresh$endorsed_ace, levels = c("0", "1"), labels = c("FALSE", "TRUE")))
  df_item_thresh$predicted_ace <- as.logical(factor(df_item_thresh$predicted_ace, levels = c("0", "1"), labels = c("FALSE", "TRUE")))
  
  df_item_thresh <<- df_item_thresh
}

# generates predicted and endorsed ace scores for each participant
# prediction is based on exceeding a certain threshold on the entire subscale
gen_subscale_thresh <- function(df, ace_items, ctq_items, thresh, inc_total_score) {
  ace_temp <- filter(df, item %in% ace_items)
  ctq_temp <-filter(df, item %in% ctq_items)
  ctq_temp$Username <- as.character(ctq_temp$Username)
  
  # generate predicted_ace score based on exceeding subscale score threshold
  ctq_mean <- ctq_temp %>% 
    dplyr::group_by(Username) %>%
    dplyr::summarise(mean_score = mean(score, na.rm = TRUE))
  
  username_temp <- ctq_temp[is.na(ctq_temp$score), ]$Username
  ctq_temp[is.na(ctq_temp$score), ]$score <- round(ctq_mean[ctq_mean$Username == username_temp, ]$mean_score, 0)
  
  ctq_temp <- ctq_temp %>% 
    dplyr::group_by(Username) %>%
    dplyr::summarise(total_score = sum(score, na.rm = TRUE)) %>% 
    dplyr::mutate(predicted_ace = ifelse(total_score >= thresh, TRUE, 
                                         ifelse(total_score == 0, NA, 
                                                ifelse(total_score < thresh, FALSE, NA))))
  
  df_subscale <- left_join(ace_temp, ctq_temp, by= "Username")
  
  df_subscale$endorsed_ace <- ifelse(df_subscale$score == 1, TRUE, 
                                     ifelse(df_subscale$score == 0, FALSE, NA))
  
  if (inc_total_score == TRUE) {
    df_subscale_thresh <- df_subscale[c("Username", "total_score", "endorsed_ace", "predicted_ace")]
  } else {
    df_subscale_thresh <- df_subscale[c("Username", "endorsed_ace", "predicted_ace")]
  }
  
  df_subscale_thresh <<- df_subscale_thresh
}

gen_logit_thresh <- function(df, ace_items, ctq_items, thresh, subscale, thresh_process){
  ace_temp <- filter(df, item %in% ace_items)
  ctq_temp <- filter(df, item %in% ctq_items) %>% 
    spread(item, score)
  
  df_wide <- left_join(ctq_temp, ace_temp, by = "Username")
  df_wide <- df_wide[complete.cases(df_wide), ] # remove NAs
  
  formula= as.formula(paste("score ~ ", paste(ctq_items, collapse= " + ", sep = " ")))
  
  model <- glm(formula = formula, family = "binomial", data = df_wide, na.action = na.omit)
  
  df_logit_thresh <- data.frame(
    Username = df_wide$Username, 
    endorsed_ace = as.logical(df_wide$score),
    predicted_ace = predict(model, type = "response"))
  
  df_logit_thresh$predicted_ace <- ifelse(df_logit_thresh$predicted_ace >= thresh, TRUE, FALSE)
  
  df_logit_thresh <<- df_logit_thresh
}

gen_confMatrix <- function(df_thresh, subscale, thresh_process){
  # Note that tbl list dimension 1 = endorsed_aces ; tbl list dimension 2 = predicted_aces
  confMatrix <<- data.table::as.data.table(table(df_thresh$predicted_ace, df_thresh$endorsed_ace))
  data.table::setnames(confMatrix,c("V1", "V2", "N"), c("Predicted ACE", "Actual ACE", "Number"))
  
  # print the table
  #full_caption = paste0("Thresholding based on ", thresh_process, " score for ", subscale) #"Thresholding by ", df_thresh, " for ", subscale
  #return(pandoc.table(confMatrix, caption = full_caption))
} 

# this function evaluates the sensitivity and specificity of a particular subscale given a confusion matrix formatted as a [1:2][1:2] matrix
sens_spec_ppv_npv <- function(confMatrix, subscale, thresh_process = "subscale", return_df = 1){
  # Label each cell of the table
  D <- confMatrix[1,3]
  C <- confMatrix[3,3]
  B <- confMatrix[2,3]
  A <- confMatrix[4,3]
  
  sens <- A/(A+C)# if subj endorses on ACES, how often will a CTQ score >= thresh? // if TRUE ACES, proprtion of TRUE CTQ 
  spec <- D/(D+B)# if subj does not endorse on ACES, how often will a CTQ score >= thresh?
  PPV <- A/(A+B)# if CTQ score >= thresh, what is the probability that subj endorsed on ACES? // TRUE CTQ, proportion of TRUE ACES
  NPV <- D/(D+C)# if CTQ score <=thresh, what is the probability that the subj did NOT endorse on ACES?
  
  # print sensitivity and specificity
  stats= cbind(rbind("sens", "spec", "PPV", "NPV"), rbind(sens, spec, PPV, NPV))
  full_caption = paste0("Thresholding based on ", thresh_process, " for ", subscale)
  
  if (return_df == 1) {
    stats <<- stats
  } else 
    return(pandoc.table(stats, caption = full_caption))
}

# creates ROC plots (options: per subscale or item)
roc_plot <- function(df, subscale, ace_items, ctq_items){
  # filter existing df_ACE and df_CTQ dataframes to subset the relevant questions
  ace_temp <- filter(df, item %in% ace_items)
  ctq_temp <-filter(df, item %in% ctq_items)
  #ctq_temp$Username <- as.character(ctq_temp$Username)
  
  ctq_mean <- ctq_temp %>% 
    dplyr::group_by(Username) %>%
    dplyr::summarise(mean_score = mean(score, na.rm = TRUE))
  
  username_temp <- ctq_temp[is.na(ctq_temp$score), ]$Username
  ctq_temp[is.na(ctq_temp$score), ]$score <- round(ctq_mean[ctq_mean$Username == username_temp, ]$mean_score, 0)
  
  # generate predicted_ace score based on exceeding subscale score threshold
  ctq_temp <- ctq_temp %>% 
    dplyr::group_by(Username) %>% 
    dplyr::summarise(total_score = sum(score, na.rm = TRUE))
  
  df_subscale <- left_join(ace_temp, ctq_temp, by = "Username")
  
  basicPlot <-  ggplot(df_subscale, aes(d = score, m = total_score)) + 
    geom_roc()
  
  roc_plot_clean <- basicPlot +
    annotate("text", x = .75, y = .25, 
          label = paste("AUC =", round(calc_auc(basicPlot)$AUC, 3)), size = 5) + 
    xlab("False positive fraction (1 - Spec)") + ylab("True positive fraction (Sens)") + 
    theme(text = element_text(face = "bold", size = 14), 
          axis.title.x = element_text(margin = margin(t = 15, r = 0, b = 0, l = 0), size = 16), 
          axis.title.y = element_text(margin = margin(t = 0, r = 15, b = 0, l = 0), size = 16), 
          panel.background = element_rect(fill = "white", size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "gainsboro"))
    
}

make_confMatrix_item <- function(df_items, ace_items, ctq_items, thresh){
  gen_item_thresh(df_items, ace_items, ctq_items, thresh=thresh)
  gen_confMatrix(df_thresh = df_item_thresh, subscale, thresh_process = paste("one or more items >=", thresh))
  confMatrix$`Predicted ACE` <- as.factor(confMatrix$`Predicted ACE`)
  confMatrix$`Actual ACE` <- as.factor(confMatrix$`Actual ACE`)
}

make_confMatrix_subscale <- function(df_items, ace_items, ctq_items, thresh){
  gen_subscale_thresh(df_items, ace_items, ctq_items, thresh=thresh, inc_total_score = 0)
  gen_confMatrix(df_thresh = df_subscale_thresh, subscale, thresh_process = paste("one or more items >=", thresh))
  confMatrix$`Predicted ACE` <- as.factor(confMatrix$`Predicted ACE`)
  confMatrix$`Actual ACE` <- as.factor(confMatrix$`Actual ACE`)
}

#thresh <- .4 # if ACE value estimated from the equation exceeds this threshold, predicted_ACE = 1

make_confMatrix_LR <- function(df, ace_items, ctq_items, thresh, subscale){
  gen_logit_thresh(df, ace_items, ctq_items, thresh, subscale, thresh_process = "logistic regression")
  gen_confMatrix(df_thresh = df_logit_thresh, subscale, thresh_process = "logistic regression")
  confMatrix$`Predicted ACE` <- as.factor(confMatrix$`Predicted ACE`)
  confMatrix$`Actual ACE` <- as.factor(confMatrix$`Actual ACE`)
}

### NOT USED IN THE APP

# create a table that summarises all the data via means + standard deviations
# numEndorsed <- function(df, item, score, caption){ 
#   tbl <- with(df, table(item, score))
#   return(pander(tbl, caption=caption))
# }
# 
# 
# mean_impute <- function(df, ctq_items){
#   
#   ctq_temp <-filter(df, item %in% ctq_items)
#   ctq_temp$Username <- as.character(ctq_temp$Username)
#   ctq_temp$score <- as.numeric(ctq_temp$score)
#   
#   ctq_mean <- ctq_temp %>% 
#     dplyr::group_by(Username) %>%
#     dplyr::summarise(mean_score = round(mean(score, na.rm = TRUE),0))
#   
#   username_temp <- ctq_temp[is.na(ctq_temp$score), ]$Username
#   ctq_temp[is.na(ctq_temp$score), ]$score <- round(ctq_mean[ctq_mean$Username == username_temp, ]$mean_score, 0)
#   
#   ctq_temp <<- ctq_temp
# }

# # creates ppv vs. sens plots by threshold
# ppv_sens_plot <- function(subscale, ACE_items, CTQ_items){
#   n = 10
#   
#   df_plot = data.frame (
#     thresh = numeric(n),
#     sens = numeric(n),
#     spec = numeric(n),
#     ppv = numeric(n),
#     npv = numeric(n)
#   )
#   
#   for (i in 1:n){
#     exceeds_subscale_thresh(subscale, ACE_items, CTQ_items, thresh = i)
#     ctq_to_aces_confMatrix(subscale, df_converted)
#     stats = sens_spec_ppv_npv(subscale, confMatrix)
#     df_plot$thresh[i] = i
#     df_plot$sens[i] = stats[[2]][1]
#     df_plot$spec[i] = stats[[2]][2]
#     df_plot$ppv[i] = stats[[2]][3]
#     df_plot$npv[i] = stats[[2]][4]
#   }
#   
#   # double check these!!
#   ggplot(df_plot, aes(x= ppv, y = sens, color = thresh)) +
#     geom_point() + 
#     labs(x = "ppv: P(True ACES | True CTQ)", y = "sens: P(True CTQ | True ACES)")
#   
#   # sens <- A/(A+C)# if subj endorses on ACES, how often will a CTQ score >= thresh? // if TRUE ACES, proprtion of TRUE CTQ 
#   # spec <- D/(D+B)# if subj does not endorse on ACES, how often will a CTQ score >= thresh?
#   # PPV <- A/(A+B)# if CTQ score >= thresh, what is the probability that subj endorsed on ACES? // TRUE CTQ, proportion of TRUE ACES
#   # NPV <- D/(D+C)# if CTQ score <=thresh, what is the probability that the subj did NOT endorse on ACES?
# }

# gen_combo_thresh <- function(df_item_thresh, df_thresh){
#   colnames(df_item_thresh) = c("Username", "i_endorsed_ace", "i_predicted_ace")
#   colnames(df_subscale_thresh) = c("Username", "s_endorsed_ace", "s_predicted_ace")
#   temp <- left_join(df_item_thresh, df_subscale_thresh, by= "Username")
#   
#   colnames(temp) = c("Username", "endorsed_ace", "item_predicted_ace", "endorsed_ace_copy", "other_predicted_ace")
#   temp$predicted_ace <- ifelse((temp$item_predicted_ace | temp$other_predicted_ace), TRUE, FALSE)
#   
#   df_combo_thresh <<- temp
# }