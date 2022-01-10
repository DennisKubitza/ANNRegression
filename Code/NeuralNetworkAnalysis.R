
# Author: Dennis Kubitza (dennis.kubitza@bibb.de)
# Version: 03-Nov-2021

#############################################----- Start PART 1 ----#############################################
library("neuralnet")
library(NeuralNetTools)
library(utils)

normalize <- function(df){
  nnchoice_maxs <- apply(df, 2, max)
  nnchoice_mins <- apply(df, 2, min)
  nnchoice_scaled <-
    as.data.frame(
      scale(
        df,
        center = nnchoice_mins,
        scale = nnchoice_maxs - nnchoice_mins
      )
    )
  return(nnchoice_scaled)
}

compute_best_NN <- function(df, target, low = 2, high = 10, repititions=10, verbose=FALSE){
  lm_model_variables <- names(df)
  # Determine Dependet variable
  lm_formula1 <-
    as.formula(paste(
      target,
      paste(lm_model_variables[lm_model_variables != target], collapse =
              " + "),
      sep = " ~ "
    ))
  
  nnchoice_scaled <- normalize(df)
  nnchoice_range_first = low:high # Neurons in First layer
#nnchoice_range_second = 1:1 # Neurons in Second layer
  nnchoice_range_itteration = 1:20 # Number of Samples for determing the best structure
  
  #Create Dataframe with 5 columns
  nnchoice_all_results <- vector(mode = "numeric", 3)
  
  for (num_first in nnchoice_range_first) {
  #   for (num_second in nnchoice_range_second) {
        nnchoice_itteration_results_train <- c()
        nnchoice_itteration_results_test <- c()
        for (itteration in nnchoice_range_itteration)
        {
          # Split Datasets in Training and Test
          ##############################
          set.seed(8188 + itteration)
          sample <-
            sample.int(
              n = nrow(nnchoice_scaled),
              size = round(0.8 * nrow(nnchoice_scaled)),
              replace = F
            )
          nnchoice_scaled_train <- nnchoice_scaled[sample, ]
          nnchoice_scaled_test  <- nnchoice_scaled[-sample, ]
          
          #Train the model based on training Dataset
          ##############################
          model = neuralnet(
            formula = lm_formula1,
            data = nnchoice_scaled_train,
            hidden = c(num_first),
            # hidden = c(num_first, num_second),
            threshold = 0.1,
            act.fct = "logistic"
          )
          
          
          #Compute RSE for Neural Network on trained Dataset
          ##############################
          
          #Check the data - actual and predicted (we need to reverse the scaling)
          final_output = cbind (
            nnchoice_scaled_train[,c(target)] * (
              max(df[,c(target)]) - min(df[,c(target)])
            ) + min(df[,c(target)]),
            as.data.frame(model$net.result) * (
              max(df[,c(target)]) - min(df[,c(target)])
            ) + min(df[,c(target)])
          )
          colnames(final_output) = c("Expected", "Neural")
          
          #Append result for current Itteration
          nnchoice_itteration_results_train = c(nnchoice_itteration_results_train, sqrt(mean((final_output$Expected -
                                                                                                final_output$Neural) ^ 2
          )))
          
          #Compute RSE for Neural Network on test Dataset
          ##############################
          
          #Compute Model Results for Test Dataset
          test_results <- compute(model, nnchoice_scaled_test)
          #Check the data - actual and predicted (we need to reverse the scaling)
          final_output_test = cbind (
            nnchoice_scaled_test[,c(target)] * (
              max(df[,c(target)]) - min(df[,c(target)])
            ) + min(df[,c(target)]),
            as.data.frame((test_results$net.result)) * (
              max(df[,c(target)]) - min(df[,c(target)])
            ) + min(df[,c(target)])
          )
          
          colnames(final_output_test) = c("Expected", "Neural")
          
          #Append result for current Itteration
          nnchoice_itteration_results_test = c(nnchoice_itteration_results_test, sqrt(mean((
            final_output_test$Expected - final_output_test$Neural
          ) ^ 2
          )))
          if(!verbose)
          {
            message(paste0("Itteration ",(num_first-low)*20+itteration," of ", (high-low+1)*10, ",", Sys.time()))
          }
        }
        
        # Mean over all Iterations and Save to nn_choice_all_results
        nnchoice_all_results = rbind(nnchoice_all_results,
                                     c(
                                       num_first,
                                       #num_second,
                                       mean(nnchoice_itteration_results_train),
                                       mean(nnchoice_itteration_results_test)
                                     ))
        
      
    #}
  }
  
  # Give Readable Column Names
  colnames(nnchoice_all_results) = c(
    "Neurons_First_Layer",
 #  "Neurons_Second_Layer",
    "Result_Training_Dataset",
    "Result_Test_Dataset"
  )
  
  nnchoice_all_results <- as.data.frame(nnchoice_all_results)
  #Drop first line, which is empty
  nnchoice_all_results <- nnchoice_all_results[-1,]
  
  nnchoice_best <- nnchoice_all_results[which.min(nnchoice_all_results$Result_Test_Dataset),]

    nnchoice_model = neuralnet(
      
    formula = lm_formula1,
    data = nnchoice_scaled,
    hidden = c(nnchoice_best$Neurons_First_Layer),
    threshold = 0.1,
    rep = repititions,
    act.fct = "logistic"
  )
  result <- list(table=nnchoice_all_results, model=nnchoice_model)
  return(result)
}



#Create Dat




