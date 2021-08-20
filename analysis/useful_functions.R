## useful_functions.R

##some functions that will let us check whether we are missing all the data for a country
getPercentMissingByCountry <- function(var, dataset=pew) {
  return(100*round(tapply(is.na(var),dataset$country,sum)/table(dataset$country),3))
}

getCountriesMissing <- function(var, dataset=pew) {
  x <- getPercentMissingByCountry(var, dataset)
  return(names(x)[x==100.0])
}

#create a set of grand-mean centered dummies from a categorical variable
createDummiesGrandMeanCentered <- function(variable, dataset, varname.prepend="") {
  lvls <- levels(variable)
  #ignore the first level as it is the reference
  lvls <- lvls[2:length(lvls)]
  newvars <- NULL
  for(lvl in lvls) {
    temp <- !is.na(variable) & variable==lvl
    temp[is.na(variable)] <- NA
    temp <- temp-mean(temp, na.rm=TRUE)
    newvars <- cbind(newvars,temp)
  }
  varnames <- gsub(" ","",paste(varname.prepend,lvls))
  varnames <- gsub("-","_", varnames)
  colnames(newvars) <- varnames
  return(cbind(dataset, newvars))
}

#shorten some country names for display
shortenCountryNames <- function(countries) {
  countries <- gsub("Democratic Republic of the Congo","DR Congo", countries)
  countries <- gsub("Palestinian Territories","Palestine", countries)
  countries <- gsub("Bosnia and Herzegovina","Bosnia", countries)
  countries <- gsub("Guinea Bissau", "Guinea-Bissau", countries)
  return(countries)
}

#divide individual level variable by twice the standard deviation and center
scaleIndVariable <- function(variable) {
  return((variable-mean(variable))/(2*sd(variable)))
}

scale_group_center <- function(variable, group) {
  group_means <- tapply(variable, group, mean)
  return((variable-group_means[as.character(group)])/(2*sd(variable)))
}

#find region based on shortened country name
findRegion <- function(country_names) {
  region <- factor(c("South Asia","Europe","MENA","Central Asia","South Asia",
                     "Europe","Africa", "Africa","MENA","MENA","Africa", 
                     "Africa","Africa","East Asia","MENA","MENA", 
                     "Central Asia","Africa","Europe","Central Asia","MENA","Africa",
                     "East Asia","Africa","Africa","Africa","South Asia","MENA",
                     "Central Asia","Africa","Central Asia","Africa","MENA","Europe",
                     "Africa"))
  names(region) <- c("Afghanistan","Albania","Algeria","Azerbaijan","Bangladesh",
                     "Bosnia","Cameroon","Chad","Djibouti","Egypt","Ethiopia",
                     "Ghana","Guinea Bissau","Indonesia","Iraq","Jordan","Kazakhstan",
                     "Kenya","Kosovo","Kyrgyzstan","Lebanon","Liberia","Malaysia",
                     "Mali","Niger","Nigeria","Pakistan","Palestine","Russia","Senegal",
                     "Tajikistan","Tanzania","Tunisia","Turkey","Uganda")
  return(region[country_names])
}

#function to update model if convergence fails
check.convergence <- function(model, current_data, times=4) {
  updates <- 0
  while(!is.null(model@optinfo$conv$lme4$code) & updates<times) {
    ss <- getME(model,c("theta","fixef"))
    model <- update(model, data=current_data, start=ss)
    updates <- updates+1
  }
  if(!is.null(model@optinfo$conv$lme4$code)) {
    cat("\nModel failed to converge.")
    cat(paste(model@call))
    cat("\n")
  }
  return(model)
}

#wrapper for lmer that will check for convergence of model
lmer.converge <- function(formula, data) {
  #I will also use bobyqa optimizer and longer iterations
  #to help convergence
  model <- lmer(formula, data=data,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
  model <- check.convergence(model, data)
  return(model)
}

#wrapper for glmer that will check for convergence of model
glmer.converge <- function(formula, data) {
  #I will also use bobyqa optimizer and longer iterations
  #to help convergence
  model <- glmer(formula, data=data,
                control=lmerControl(optimizer="bobyqa",
                                    optCtrl=list(maxfun=2e5)))
  model <- check.convergence(model, data)
  return(model)
}


#A function to pool the random effects models and produce results for the fixed effects
pool_re_models <- function(models) {
  
  b <- sapply(models, function(x) {summary(x)$coef[,"Estimate"]})
  se <- sapply(models, function(x) {summary(x)$coef[,"Std. Error"]})
  
  if(!is.null(nrow(b))) {
    beta <- apply(b,1,mean)
    between.var <- apply(b,1,var)
    within.var <- apply(se^2,1,mean)
    sterror <- sqrt(within.var+between.var+between.var/length(models))
    tstat <- beta/sterror
    pvalue <- (1-pnorm(abs(tstat)))*2
    fixed <- list(beta=beta,se=sterror,tstat=tstat,pvalue=pvalue)
  } else {
    beta <- mean(b)
    between.var <- var(b)
    within.var <- mean(se^2)
    sterror <- sqrt(within.var+between.var+between.var/length(models))
    tstat <- beta/sterror
    pvalue <- (1-pnorm(abs(tstat)))*2
    names(beta) <- names(sterror) <- names(tstat) <- names(pvalue) <- "(Intercept)"
    fixed <- list(beta=beta,se=sterror,tstat=tstat,pvalue=pvalue)
  }
  
  rsd <- apply(sapply(models,function(x) {attr(VarCorr(x)$country,"stddev")}),1,mean)
  n_groups <- mean(sapply(models, function(model) {nrow(ranef(model)$country)}))
  
  #average random effecs
  ranefs <- lapply(models, function(x) {ranef(x)$country})
  se.ranefs <- lapply(models, function(x) {se.ranef(x)$country})
  
  #average effects across imputations. The use of Reduce comes from:
  #https://stackoverflow.com/questions/7651539/mean-of-elements-in-a-list-of-data-frames
  reffects <- Reduce('+', ranefs)/length(ranefs)
  var.reffects.within <- (Reduce('+', se.ranefs)/length(se.ranefs))^2
  var.reffects.between <- Reduce('+', lapply(ranefs, function(x) {(x-reffects)^2}))/(length(ranefs)-1)
  sd.reffects <- sqrt(var.reffects.within+var.reffects.between+var.reffects.between/length(ranefs))
  
  #get the full country intercepts and slopes by adding fixed parameters to random effect
  country_effects <- t(beta[colnames(reffects)]+t(reffects))
  
  rcorr <- Reduce('+',lapply(models,function(x) {
    attr(VarCorr(x)$country,"correlation")}))/length(ranefs)
  
  
  #get summary statistics
  n <- nobs(models[[1]])
  ngrp <- ngrps(models[[1]])
  BIC <- mean(sapply(models, BIC))
  
  sumStats <- list(n=n,ngrp=ngrp,BIC=BIC)
  
  return(list(fixef=fixed,
              ranef_sd=rsd, n_groups=n_groups, ranef_cor=rcorr,
              country_effects=country_effects, country_sd=sd.reffects,
              sumStats=sumStats)
  )
}

pool_fe_model <- function(formula, imputations) {
  
  #setting up null objects allows us to easily add results
  #later
  b <- se <- rsq <- NULL
  
  #now loop through our imputations and run the model
  for(imputation in imputations) {
    #run the model
    model <- lm(formula, data=imputation)
    #collect the results
    b <- cbind(b, coef(model))
    se <- cbind(se, summary(model)$coef[,2])
    #We want R squared relative to the model with just country effects
    model.null <- lm(update(formula, .~country), data=imputation)
    rsq.null <- summary(model.null)$r.squared
    rsq <- c(rsq, (summary(model)$r.squared-rsq.null)/(1-rsq.null))
  }
  
  #now pool the results
  b.pool <- apply(b, 1, mean)
  between.var <- apply(b, 1, var)
  within.var <- apply(se^2, 1, mean)
  se.pool <- sqrt(within.var+between.var+between.var/length(analytic_samples)) 
  t.pool <- b.pool/se.pool 
  pvalue.pool <- (1-pnorm(abs(t.pool)))*2 
  coefficients <- data.frame(b.pool, se.pool, t.pool, pvalue.pool)
  
  #lets take the mean R2 value
  r.squared <- mean(rsq)
  #we can also grap n and p from the last model since 
  #they should be the same across all iterations
  n <- nobs(model)
  p <- length(model$coefficients)-1
  #go ahead and calculate BIC.null
  bic.null <- n*log(1-r.squared)+p*log(n)
  
  #return everything in a list
  return(list(coef=coefficients,
              n=n,
              r.squared=r.squared,
              bic.null=bic.null))
}

#a function for texreg to get FE results nicely displayed
convert_model_fe <- function(model) {
  tr <- createTexreg(
    coef.names = rownames(model$coef), 
    coef = model$coef$b.pool, 
    se = model$coef$se.pool, 
    pvalues = model$coef$pvalue.pool,
    gof.names = c("Within country R2","BIC (vs. null)","N"), 
    gof = c(model$r.squared, model$bic.null, model$n), 
    gof.decimal = c(T,T,F)
  )
}