library(rjags)
library(runjags)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(plyr)
library(dplyr)
library(GGally)
library(ggh4x)
library(pracma)
library(brms)
library(posterior)
conversion_rate = 6 / 8 # USD to GBP
base_rate = 6
BRMS_ITER = 6000 #4000 # 8000 
BRMS_WARMUP = 3000 #2000 # 4000 
NCHAINS = 10
NCORES = 10
ALGORITHM = "sampling" # meanfield
ADAPT_DELTA = 0.95

statslist = list()
convergelist = list()

par_labels =  c("b.drift.intercept.p" = "Drift intercept",
                "b.drift.amount.p" = "Drift\nsensitivity",
                "bias.p" = "Bias",
                "nondectime.p" = "Non-decision\ntime",
                #"boundary.p" = "Boundary")                
                "noise.p" = "Noise")

########################################################################################################
# some aux functions
########################################################################################################
#outliers

markoutliersIQR = function(x) { 
  y = ifelse(x %in% boxplot(x, plot = F)$out, T, F)
  print(paste("There was ", 100*round(mean(y), digits = 3), "percent of outliers"))
  return(y)
}

remove_brackets = function(x) {
  if (is.character(x)|| is.factor(x) ) {
    x = gsub('\\[', '',  x)
    x = gsub('\\]', '',  x)
    return(as.numeric(x))
  }
  else
    return(x)
}

sem = function(x)
  sd(x, na.rm = T) / sqrt(length(x))

reformat = function(x) {
  ifelse(!is.na(as.numeric(x)), round(as.numeric(x), 3), x)
}

########################################################################################################
# plot ratings vs delayed amount
########################################################################################################
plot_ratings = function(ratingdata) {
  myplot = ggplot(NULL) +
    geom_line(data = ratingdata, aes(x = amount_later, y = bid.high)) +
    geom_line(data = ratingdata, aes(x = amount_later, y = bid.low)) + geom_point() + facet_grid(. ~ context) +
    ggtitle(ratingdata$subjID[1]) + xlab('Delayed reward') + ylab('Offered price')
  
  print(myplot)
}


########################################################################################################
# organize data to fit model
########################################################################################################
organize_data = function(rawdata, grouping_var, long = F) {
  
  subjList <-
    unique(unlist(rawdata[grouping_var]))  # list of subjects x blocks
  numSubjs <- length(subjList)  # number of subjects
  Tsubj <-
    as.vector(rep(0, numSubjs)) # number of trials for each subject
  #View(rawdata)
  #View(subjList)
  #browser()
  for (i in 1:numSubjs)  {
    curSubj  <- subjList[i]
    Tsubj[i] <- sum(rawdata[grouping_var] == curSubj)
  }

  # Setting maxTrials
  maxTrials <- max(Tsubj)
  
  delay_later   <- array(0, c(numSubjs, maxTrials))
  amount_later  <- array(0, c(numSubjs, maxTrials))
  delay_sooner  <- array(0, c(numSubjs, maxTrials))
  amount_sooner <- array(0, c(numSubjs, maxTrials))
  choice <- array(0, c(numSubjs, maxTrials))
  RT <- array(0, c(numSubjs, maxTrials))
  iscalibration <- array(0, c(numSubjs, maxTrials))
  
  minRT <-
    with(rawdata, aggregate(rt, by = list(y = subjID), FUN = min)[["x"]])

  for (i in 1:numSubjs) {
    curSubj      <- subjList[i]
    useTrials    <- Tsubj[i]
    w = which(rawdata [, grouping_var ]  == curSubj)
    tmp          <- rawdata[w, ]
    delay_later[i, 1:useTrials]   <- tmp$delay_later
    amount_later[i, 1:useTrials]  <- tmp$amount_later
    delay_sooner[i, 1:useTrials]  <- tmp$delay_sooner
    amount_sooner[i, 1:useTrials] <- tmp$amount_sooner
    
    if ("iscalibration" %in% colnames(tmp)) 
      iscalibration[i, 1:useTrials] <- tmp$iscalibration
    
    if ("choice" %in% colnames(tmp))
      choice[i, 1:useTrials] <- tmp$choice

    if ("rt" %in% colnames(tmp))
      RT[i, 1:useTrials] <- tmp$rt
  }
  
  minRT <- apply(RT, 1, function(x)
    min(x[x > 0]))
  
  dataList <- list(
    N             = numSubjs,
    T             = maxTrials,
    Tsubj         = Tsubj,
    amount_later  = amount_later,
    delay_later   = delay_later,
    amount_sooner = amount_sooner,
    delay_sooner  = delay_sooner,
    choice        = choice,
    iscalibration = iscalibration,
    minRT         = minRT,
    RT            = RT,
    RTbound = 0.1
  )
  
  if (long){
    make_long = function(x){
      melt(x)$value
    }

    xx = melt(dataList$RT, varnames = c("instance", "trial"))
    dataList$RT = make_long(dataList$RT)
    keep = dataList$RT > 0
    dataList$instance = xx$instance[keep]
    dataList$trial = xx$trial[keep]
    dataList$amount_later = make_long(dataList$amount_later)[keep]
    dataList$delay_later = make_long(dataList$delay_later)[keep]
    dataList$amount_sooner = make_long(dataList$amount_sooner)[keep]
    dataList$delay_sooner = make_long(dataList$delay_sooner)[keep]
    dataList$choice = make_long(dataList$choice)[keep]
    dataList$iscalibration = make_long(dataList$iscalibration)[keep]
    dataList$RT = dataList$RT[keep]
    dataList$M = sum(keep)
    dataList$subjList = subjList
  }

  return(dataList)
}


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

########################################################################################################
# check stan chains and posteriors
########################################################################################################

check_stan_model = function(myfit) {
  print(traceplot(myfit, pars = c("mu_p", "sigma_p", "mu_slope_p", "sigma_slope_p"), inc_warmup = T))
  print(traceplot(myfit, pars = c("mu_p", "sigma_p", "mu_slope_p", "sigma_slope_p"), inc_warmup = F))
  print(pairs(myfit, pars = c("mu_p", "sigma_p")))
  print(pairs(myfit, pars = c("mu_p", "sigma_p", "mu_slope_p", "sigma_slope_p")))
  print(plot(myfit, pars = c("mu_p", "mu_slope_p")))
  print(plot(myfit, pars = c("drift_intercept_gen")))
  print(plot(myfit, pars = c("drift_slope_gen")))
  print(plot(myfit, pars = c("nondectime_gen")))
  print(plot(myfit, pars = c("boundary_gen")))
  samples = extract(myfit)
  cc = cor(cbind(samples$mu_p, samples$sigma_p, samples$mu_slope_p, samples$sigma_slope_p))
  print(cc)
  rhat = summary(myfit)$summary[, "Rhat"]
  n_eff = summary(myfit)$summary[, "n_eff"]
  print(sort(rhat[1:10], decreasing = T))
  print(range(rhat))
  print(sort(n_eff[1:10]))
  loo.fit = loo(myfit)
  waic.fit = 0 # waic(myfit)
  ics = c(loo.fit, waic.fit)
  names(ics) = c('loo', 'waic')
  return(ics)
}

########################################################################################################
# check jags chains and posteriors
########################################################################################################

check_jags_model = function(myfit, myfit_rjags, myfit_samples) {
  
  myfit.MCMCsummary = MCMCsummary(myfit_samples, round = 2)
  
  rhat = myfit.MCMCsummary$Rhat 
  n.eff = myfit.MCMCsummary$n.eff
  
  # myfit.MCMCpstr = MCMCpstr(myfit_rjags, func = mean,
  #          type = 'summary')
  
  MCMCtrace(myfit_samples, 
            params = pars,
            ISB = FALSE,
            pdf = FALSE)
  #  MCMCplot(myfit_samples, 
  #         params = pars[-length(pars)] ,
  #         ref_ovl = TRUE)
  
  MCMCplot(myfit_samples, 
           params = 'b.drift.intercept.p',
           ref_ovl = TRUE)
  
  MCMCplot(myfit_samples, 
           params = 'b.drift.amount.p',
           ref_ovl = TRUE)

  MCMCplot(myfit_samples, 
           params = 'noise.p',
           ref_ovl = TRUE)

  MCMCplot(myfit_samples, 
           params = 'bias.p',
           ref_ovl = TRUE)

  MCMCplot(myfit_samples, 
           params = 'nondectime.p',
           ref_ovl = TRUE)

    MCMCplot(myfit_samples, 
           params = 'SNR.p',
           ref_ovl = TRUE)
  
  print(sort(rhat, decreasing = T)[1:20])
  print(sort(n.eff, decreasing = F)[1:20])
  
}  


########################################################################################################
# get parameters
########################################################################################################


get_par_stan = function(par, var, ylimit=NULL){
  mypar = parVals[par] 
  allIndPars$x = as.factor(unlist(allIndPars[var]))
  par.melt = melt(mypar, varnames = c("sample", "index"))
  par.melt = merge(par.melt, allIndPars, by = "index")
  par.melt = subset(par.melt, subjID %in% include_subjects & x != "NotRecorded")
  par.melt = par.melt %>% group_by(x, sample, context_order) %>% summarise(value = mean(value)) %>% ungroup() 
  myplot = ggplot(par.melt, aes(x = x, y = value, fill = x)) + geom_violin() + ggtitle(par) + xlab(var) + labs(fill = var) + facet_grid(. ~ context_order)
  if(!is.null(ylimit)) myplot = myplot + ylim(ylimit)
  print(myplot)
}

get_par_jags = function(par, var, var2=NULL, ylimit=NULL){
  mypar = parVals[[par]]
  rownames(mypar) = seq(nrow(mypar))
  allIndPars$x = as.factor(unlist(allIndPars[[var]]))
  if (!is.null(var2)) allIndPars$z = as.factor(unlist(allIndPars[[var2]]))
  else allIndPars$z = NA
  par.melt = melt(mypar, varnames = c("index", "sample"))
  par.melt = merge(par.melt, allIndPars, by = "index", suffixes = c('par', ''))
  par.melt = subset(par.melt, subjID %in% include_subjects)
  par.melt = par.melt %>% group_by(x, z, sample, context_order) %>% summarise(value = mean(value)) %>% ungroup() %>% filter(x != "NotRecorded")
  myplot = ggplot(par.melt, # %>% filter(context_order <3),
    aes(x = x, y = value, fill = x)) + geom_violin() + ggtitle(par) + xlab(var) + labs(fill = var)  
  if (!is.null(var2)) myplot = myplot + facet_grid(. ~ z)
  if(!is.null(ylimit)) myplot = myplot + ylim(ylimit)
  print(myplot)
}


########################################################################################################
# get statistics
########################################################################################################

get_stats = function(model, param, family=NULL, add_posterior = TRUE) {
  print("------------")
  print(param)
  if (inherits(model, "brmsfit")) {
    param_clean = gsub(" ", "", param)
    summ <- model$posterior_summary_unstd
    row_idx <- which(rownames(summ) == paste0("b_", param_clean))
    if (length(row_idx) == 0) stop(paste("Parameter", param, "not found in model summary"))
    vals <- as.numeric(summ[row_idx, ])

    # vals: mean, est.error, l-95%, u-95%, Rhat, Bulk_ESS, Tail_ESS
    if (add_posterior) {
      # Use modern posterior extraction method
      posterior_draws <- model$posterior_draws_unstd 
      param_col <- paste0("b_", param_clean)
      
      if (param_col %in% names(posterior_draws)) {
        # Compute posterior probability that value is positive or negative
        if (vals[1] > 0) {
          post_prob <- mean(posterior_draws[[param_col]] > 0)
            if (1 - post_prob < 1e-3) {
            out = sprintf("b = %.3f, 95%% CI = [%.3f, %.3f], P(b<0) < 1e-3", vals[1], vals[3], vals[4])
            } else {
            out = sprintf("b = %.3f, 95%% CI = [%.3f, %.3f], P(b<0) = %.3f", vals[1], vals[3], vals[4], 1 - post_prob)
            }
        } else {
          post_prob <- mean(posterior_draws[[param_col]] < 0)
            if (1 - post_prob < 1e-3) {
            out = sprintf("b = %.3f, 95%% CI = [%.3f, %.3f], P(b>0) < 1e-3", vals[1], vals[3], vals[4])
            } else {
            out = sprintf("b = %.3f, 95%% CI = [%.3f, %.3f], P(b>0) = %.3f", vals[1], vals[3], vals[4], 1 - post_prob)
            }

        }
      } else {
        warning(paste("Parameter", param_col, "not found in posterior draws"))
        out = sprintf("b = %.3f, 95%% CI = [%.3f, %.3f]", vals[1], vals[3], vals[4])
      }
    } else {
      out = sprintf("b = %.3f, 95%% CI = [%.3f, %.3f]", vals[1], vals[3], vals[4])
    }

    # p-value is not directly available for brmsfit, so not reported
  } else {
    vals = summary(model)$coefficients[param,]
    if (is.null(family)) {
      pval = vals[5]
      out = sprintf(
        "b = %s, t = %s, df = %s,",
        round(vals[1], digits = 3),
        round(vals[4],  digits = 3),
        round(vals[3], digits = 1)
      )
    } else {
      pval = vals[4]
      out = sprintf(
        "b = %s, z = %s,",
        round(vals[1], digits = 3),
        round(vals[3],  digits = 3)
      )
    }
    if (pval < 1e-10) {
      pstr = "p < 1e-10"
    }
    else if (pval  < 1e-3) {
      pstr = paste("p =", formatC(pval ,  digits = 1, format = "e"))
    }
    else {
      pstr = paste("p =", round(pval,  digits = 3))
    }
    out = paste(out, pstr)
  }
  print(names(param))
  statslist[names(param)] <<- out
  print(out)
}

get_fixef <- function(model) {
    # Get posterior summary for all fixed effects
    summ <- model$posterior_summary_unstd
    # Only keep fixed effects (start with "b_")
    fe_idx <- grep("^b_", rownames(summ))
    fe_names <- gsub("^b_", "", rownames(summ)[fe_idx])
    # Get posterior draws
    draws <- model$posterior_draws_unstd 
    out <- round(summ[fe_idx, c("Estimate", "Est.Error", "Q2.5", "Q97.5")], 3)
    rownames(out) <- fe_names
    # Compute two-sided Bayesian p-value for each parameter
    for (i in seq_along(fe_names)) {
      col <- paste0("b_", fe_names[i])
      if (col %in% names(draws)) {
        draws_col <- draws[[col]]
        # Two-sided: probability that sign is opposite to mean
        mean_val <- mean(draws_col, na.rm = TRUE)
        pval <- 2 * min(mean(draws_col > 0, na.rm = TRUE), mean(draws_col < 0, na.rm = TRUE))
        #out$`P(|b|)`[i] <- round(pval, 3)
      }
    }
    return(out)
}

########################################################################################################
# model fitting
########################################################################################################

fitmodel = function(ff,
          data.sub,
          params = NULL,
          family = NULL,
          uselmer = FALSE,
          cores = NCORES,
          chains = NCHAINS,
          iter = BRMS_ITER,
          warmup = BRMS_WARMUP,
          set_priors = TRUE, 
          thin = 2,
          seed = 13)
{
  print("============")
  print(ff)
  
  if (uselmer){
      model = glmer(as.formula(ff), data = data.sub, family = family,
        control = glmerControl(optimizer ='optimx', optCtrl=list(method='nlminb')))    
  } else{
  
  family_ <- family
  if (is.null(family)) {
      family_ <- gaussian()
  } else if (family == "binomial") {
      family_ <- bernoulli(link = "logit")
  }
#browser()
  ff_formula <- as.formula(ff)

  if (set_priors){
  # Extract predictor variable names from formula
  # Remove random effects from formula before extracting terms
  ff_formula_str <- deparse1(ff_formula)
  # Remove everything after the last "+ (" (including "+ (")
  # Remove random effects terms like (1|group) from formula string
  ff_formula_str_clean <- gsub("\\([^\\)]*\\|[^\\)]*\\)", "", ff_formula_str)
  # Remove trailing "+" if present
  ff_formula_str_clean <- gsub("\\s+$", "", ff_formula_str_clean) # remove trailing spaces
  ff_formula_str_clean <- gsub("\\+$", "", ff_formula_str_clean)  # remove trailing plus
  ff_formula_norand <- as.formula(ff_formula_str_clean)

  terms_obj <- terms(ff_formula_norand, data = data.sub)
  pred_vars <- attr(terms_obj, "term.labels")
  # Remove interaction terms and keep only main effects
  pred_vars_main <- unique(unlist(strsplit(pred_vars, "[:*]")))
  # Standardize predictors (excluding intercept and factors)
  data.sub.std <- data.sub
  std_info <- list()
  # Standardize dependent variable
  dep_var <- as.character(ff_formula[[2]])

  print(dep_var)
  print(pred_vars_main)

  # Check if dependent variable exists in data
  if (!dep_var %in% names(data.sub)) {
    stop(paste("Dependent variable", dep_var, "not found in data"))
  }

  std_info[[dep_var]] <- list(mean = 0, sd = 1)
  print(sprintf("Standardizing dependent variable: %s", dep_var))
  
  if (is.null(family) || family != "binomial") {
    # Check if the column contains valid numeric data
    if (!is.numeric(data.sub[[dep_var]]) || all(is.na(data.sub[[dep_var]]))) {
      stop(paste("Dependent variable", dep_var, "is not numeric or contains only NA values"))
    }
    
    mu_y <- mean(data.sub[[dep_var]], na.rm = TRUE)
    sd_y <- sd(data.sub[[dep_var]], na.rm = TRUE)
    
    # Check for constant variables (sd = 0)
    if (sd_y == 0 || is.na(sd_y)) {
      warning(paste("Dependent variable", dep_var, "has zero variance or is constant"))
      data.sub.std[[dep_var]] <- data.sub[[dep_var]]
      std_info[[dep_var]] <- list(mean = mu_y, sd = 1)
    } else {
      data.sub.std[[dep_var]] <- (data.sub[[dep_var]] - mu_y) / sd_y
      std_info[[dep_var]] <- list(mean = mu_y, sd = sd_y)
    }
  }
  #browser()
  # Standardize predictors
  for (v in pred_vars_main) {
    if (is.numeric(data.sub[[v]])) {
      print(sprintf("Standardizing variable: %s", v))
      mu <- mean(data.sub[[v]], na.rm = TRUE)
      sigma <- sd(data.sub[[v]], na.rm = TRUE)
      data.sub.std[[v]] <- (data.sub[[v]] - mu) / sigma
      if (grepl("centered", v)) {
        data.sub.std[[v]] <- data.sub.std[[v]] + mu
      } 
      std_info[[v]] <- list(mean = mu, sd = sigma)
    }
  }


  priors <- c(
    set_prior("normal(0, 10)", class = "b"),
    set_prior("cauchy(0, 10)", class = "sd"),
    set_prior("normal(0, 10)", class = "Intercept")
  )
    
  #if (family == "binomial") {
  #  priors <- c(priors, set_prior("beta(1, 1)", class = "Intercept"))
  #} else {
  #  priors <- c(priors, set_prior("normal(0, 5)", class = "Intercept"))
  #}
  #browser()
  model <- brm(
    formula = ff_formula,
    data = data.sub.std,
    family = family_,
    prior = priors,
    cores = cores,
    chains = chains,
    iter = iter,
    warmup = warmup,
    thin = thin,
    seed = seed,
    control = list(adapt_delta = ADAPT_DELTA), #, max_treedepth = 12),
    sample_prior = "yes",
#    save_pars = save_pars(all = FALSE, group = F),
    algorithm = ALGORITHM
    
  )

  # Unstandardize posterior draws and summary for fixed effects
  unstd_draws <- as_draws_df(model)
  unstd_summary <- as.data.frame(posterior_summary(model))
  #browser()

  # Unstandardize main effects
  for (v in pred_vars_main) {
    
    if (!is.null(std_info[[v]])) {
      print(sprintf("Unstandardizing variable: %s", v))
      # Unstandardize: multiply by sd_y / sd_x
      colname <- paste0("b_", v)
      if (colname %in% names(unstd_draws)) {
        unstd_draws[[colname]] <- unstd_draws[[colname]] * std_info[[dep_var]]$sd / std_info[[v]]$sd
        if (colname %in% rownames(unstd_summary)) {
          unstd_summary[colname, c("Estimate", "Est.Error", "Q2.5", "Q97.5")] <-
            unstd_summary[colname, c("Estimate", "Est.Error", "Q2.5", "Q97.5")] * std_info[[dep_var]]$sd / std_info[[v]]$sd
        }
      }
    }
  }
  
  #browser()
  # Unstandardize interaction terms
  interaction_terms <- grep("^b_.*:", rownames(unstd_summary), value = TRUE)
  for (term in interaction_terms) {
    # Extract variable names from interaction term
    vars <- strsplit(sub("^b_", "", term), ":")[[1]]
    
    # Only unstandardize variables in std_info
    vars <- intersect(vars, names(std_info))
    if (length(vars)== 0) next
    print(sprintf("Unstandardizing interaction: %s", term))
    scale_factor <- std_info[[dep_var]]$sd / prod(sapply(vars, function(v) std_info[[v]]$sd))
    if (term %in% names(unstd_draws)) {
      unstd_draws[[term]] <- unstd_draws[[term]] * scale_factor
    }
    if (term %in% rownames(unstd_summary)) {
      unstd_summary[term, c("Estimate", "Est.Error", "Q2.5", "Q97.5")] <-
        unstd_summary[term, c("Estimate", "Est.Error", "Q2.5", "Q97.5")] * scale_factor
    }
  
  }
  # Unstandardize intercept
  if ("b_Intercept" %in% names(unstd_draws)) {
    unstd_draws[["b_Intercept"]] <- unstd_draws[["b_Intercept"]] * std_info[[dep_var]]$sd + std_info[[dep_var]]$mean
    if ("b_Intercept" %in% rownames(unstd_summary)) {
      unstd_summary["b_Intercept", c("Estimate", "Est.Error", "Q2.5", "Q97.5")] <-
        unstd_summary["b_Intercept", c("Estimate", "Est.Error", "Q2.5", "Q97.5")] * std_info[[dep_var]]$sd + std_info[[dep_var]]$mean
    }
  }
  model$posterior_draws_unstd <- unstd_draws
  model$posterior_summary_unstd <- unstd_summary


  # Visualize prior vs posterior for fixed effects
  if (ALGORITHM == "sampling"){
  prior_post_plot <- plot(model, ask = FALSE, plot = TRUE, combo = c("dens_overlay", "hist", "trace"), prior = TRUE)
  print(prior_post_plot)
  }

  } else  {
      # Example: fit a brms model with default priors (no standardization)
      model <- brm(
        formula = ff_formula,
        data = data.sub,
        family = family_,
        cores = cores,
        chains = chains,
        iter = iter,
        warmup = warmup,
        thin = thin,
        seed = seed,
        sample_prior = "yes",
        save_pars = save_pars(all = FALSE, group = F)

      )
      # Visualize prior vs posterior for fixed effects
      prior_post_plot <- plot(model, ask = FALSE, plot = TRUE, combo = c("dens_overlay", "hist", "trace"), prior = TRUE)
      print(prior_post_plot)
  }

  }

  print(ff)
  summ <- as.data.frame(summary(model)$fixed)
  rhat_max <- max(summ$Rhat, na.rm = TRUE)
  ess_min <- min(summ$Bulk_ESS, na.rm = TRUE)
  tail_ess_min <- min(summ$Tail_ESS, na.rm = TRUE)
  cat(sprintf("Largest Rhat: %.3f\n", rhat_max))
  cat(sprintf("Smallest Bulk_ESS: %.0f\n", ess_min))
  cat(sprintf("Smallest Tail_ESS: %.0f\n", tail_ess_min))

  convergelist <<- c(convergelist, list(list(formula = ff, rhat_max = rhat_max, ess_min = ess_min, tail_ess_min = tail_ess_min)))

  if (!is.null(params)) {
    for (i in seq_along(params)) {
      get_stats(model, params[i], family)
    }
  }
  return(model)
}

########################################################################################################
# add column with CIs
########################################################################################################

add_ci_column <- function(df, lower = "Q2.5", upper = "Q97.5", colname = "95% CI") {
  df[[colname]] <- apply(df[, c(lower, upper)], 1, function(x) {
    ci_str <- sprintf("[%.3f, %.3f]", as.numeric(x[1]), as.numeric(x[2]))
    # Significant if interval does not include zero
    if (as.numeric(x[1]) > 0 | as.numeric(x[2]) < 0) {
      ci_str <- paste0(ci_str, "*")
    }
    ci_str
  })
  df <- df %>% select(-all_of(c(lower, upper)))
  return(df)
}


tab_to_str = function(mytable){
  if (is.data.frame(mytable)) {

    # Unlist any columns that are lists
    for (col in names(mytable)) {
      if (is.list(mytable[[col]])) {
        mytable[[col]] <- unlist(mytable[[col]])
      }
    }
    write.table(mytable, "../results/tmp.csv", sep = ";", row.names = FALSE)
  } else {
    write.table(data.frame(t(unlist(mytable))), "../results/tmp.csv", sep = ";", row.names = FALSE)
  }
  return(gsub("\"","", paste(readLines('../results/tmp.csv'), collapse = '\n')))
} 



limit_mean <- function(ev.cut){
  ev = mean(as.numeric(strsplit(substr(as.character(ev.cut), 2, nchar(as.character(ev.cut)) - 1), ",")[[1]]))
}
