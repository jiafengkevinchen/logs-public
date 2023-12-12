add_prop_effect_to_poisson <-
  function(poisResults,
           treatmentVar,
           covariatesToKeep = NULL,
           covName = "cov.scaled",
           extraStatsDF = data.frame(Covariates = "Y")){
    
    
    #If covariatesToKeep is null, keep all of them
    if(is.null(covariatesToKeep)){
      covariatesToKeep <- labels(coefficients(poisResults))
    }
    
    #Extract the covariance matrix
    vcov <- poisResults[[covName]]

    
    #Compute the ATE percent and its standard errors
    implied_ATEpercent <- exp(coefficients(poisResults)[treatmentVar])-1
    se_ATEpercent <- exp(coefficients(poisResults)[treatmentVar]) * 
                      sqrt(vcov[treatmentVar,treatmentVar])
    
    tidy_results <- data.frame(term = c(covariatesToKeep, "Prop. Effect"),
                               estimate = c(coefficients(poisResults)[covariatesToKeep], 
                                            implied_ATEpercent ),
                                          std.error = c(sqrt(diag(as.matrix(vcov[covariatesToKeep, covariatesToKeep]))),
                                                        se_ATEpercent) )
    
    mod  <- list(tidy = tidy_results, glance = extraStatsDF)
    class(mod) <- "modelsummary_list"
    return(mod)
  }