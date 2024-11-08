# Filter data
specdata_as <- specdata[!is.na(specdata$abundance) & specdata$abundance != 0, ]
subsites <- unique(specdata_as$SUBSITE)

# Initialize result list
anova_sr <- list()

# Loop over subsites
for (k in 1:length(subsites)) {
  specdatas <- specdata_as[specdata_as$SUBSITE == subsites[k],]
  anovaresults <- list()
  spec <- unique(specdatas$species)
  
  # Loop over species
  for (i in 1:length(spec)) { 
    subset <- specdatas[specdatas$species == spec[i],]
    ltreat <- length(unique(subset$TREATMENT))
    lyear <- length(unique(subset$YEAR))
    
    # Initialize list for models and AIC values
    models <- list()
    aic_values <- numeric()
    
    # Check if enough data points are available to fit models
    n <- nrow(subset)
    
    # Fit models based on conditions
    if ((ltreat > 1) & (lyear > 1) & (n > 3)) { 
      model <- lm(data = subset, formula(paste("abundance", "~", "TREATMENT * YEAR")))
      if (length(model$residuals) > 0 && sum(model$residuals) != 0) {
        models[["TREATMENT * YEAR"]] <- model
        aic_values <- c(aic_values, AIC(model))
      }
    }
    if ((ltreat > 1) & (lyear > 1) & (n > 3)) { 
      model <- lm(data = subset, formula(paste("abundance", "~", "TREATMENT + YEAR")))
      if (length(model$residuals) > 0 && sum(model$residuals) != 0) {
        models[["TREATMENT + YEAR"]] <- model
        aic_values <- c(aic_values, AIC(model))
      }
    }
    if ((lyear > 1) & (n > 2)) { 
      model <- lm(data = subset, formula(paste("abundance", "~", "YEAR")))
      if (length(model$residuals) > 0 && sum(model$residuals) != 0) {
        models[["YEAR"]] <- model
        aic_values <- c(aic_values, AIC(model))
      }
    }
    if ((ltreat > 1) & (n > 2)) { 
      model <- lm(data = subset, formula(paste("abundance", "~", "TREATMENT")))
      if (length(model$residuals) > 0 && sum(model$residuals) != 0) {
        models[["TREATMENT"]] <- model
        aic_values <- c(aic_values, AIC(model))
      }
    }
    
    # Find best model based on AIC
    if (length(aic_values) > 0) {
      best_model_index <- which.min(aic_values)
      best_model_name <- names(models)[best_model_index]
      best_model <- models[[best_model_name]]
      anovaresults[[spec[i]]] <- summary(best_model)
    } else {
      anovaresults[[spec[i]]] <- NA
    }
  }
  
  names(anovaresults) <- spec
  anova_sr[[k]] <- anovaresults
}

names(anova_sr) <- subsites
capture.output(anova_sr, file = "./models/anova_results_spec.txt")
