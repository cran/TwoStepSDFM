## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE,
                      comment = "#>",
                      fig.width = 8,
                      fig.height = 5,
                      dev = "png",
                      dpi = 150,
                      out.width = "100%"
)

## ----setup, include = FALSE---------------------------------------------------
library(TwoStepSDFM)

## ----simulation, eval = TRUE, message = FALSE---------------------------------

seed <- 02102025
set.seed(seed)
no_of_obs <- 300
no_of_vars <- 50
no_of_factors <- 3
trans_error_var_cov <- diag(1, no_of_factors)
loading_matrix <- matrix(rnorm(no_of_vars * no_of_factors), no_of_vars, no_of_factors)
meas_error_mean <- rep(0, no_of_vars)
meas_error_var_cov <- diag(1, no_of_vars)
trans_var_coeff <- cbind(diag(0.5, no_of_factors), -diag(0.25, no_of_factors))
factor_lag_order <- 2
delay <- c(2, 1, floor(rexp(no_of_vars - 2, 1)))
quarterfy <- TRUE
quarterly_variable_ratio <- 0.1
corr <- TRUE
beta_param <- 2
burn_in <- 999
starting_date <- "1970-01-01"
rescale <- FALSE
check_stationarity <- TRUE
stationarity_check_threshold <- 1e-10
factor_model <- simFM(no_of_obs = no_of_obs, no_of_vars = no_of_vars,
                      no_of_factors = no_of_factors, loading_matrix = loading_matrix,
                      meas_error_mean = meas_error_mean, meas_error_var_cov = meas_error_var_cov,
                      trans_error_var_cov = trans_error_var_cov, trans_var_coeff = trans_var_coeff,
                      factor_lag_order = factor_lag_order, delay = delay, quarterfy = quarterfy,
                      quarterly_variable_ratio  = quarterly_variable_ratio, corr = corr,
                      beta_param = beta_param, seed = seed, burn_in = burn_in, 
                      starting_date = starting_date, rescale = rescale, 
                      check_stationarity = check_stationarity, 
                      stationarity_check_threshold = stationarity_check_threshold)
spca_plots <- plot(factor_model, axis_text_size = 10, legend_title_text_size = 5)
spca_plots$`Data Plots`
spca_plots$`Factor Time Series Plots`
spca_plots$`Loading Matrix Heatmap`

## ----no_of_fact_test, eval = TRUE, message = FALSE----------------------------
mtly_data <- scale(factor_model$data[, which(factor_model$frequency == 12)])
no_of_fact_test <- noOfFactors(mtly_data, min_no_factors = 1, max_no_factors = 7,
                                  confidence_threshold = 0.2)
print(no_of_fact_test)

## ----cross_val, eval = TRUE, message = FALSE----------------------------------
no_of_mtly_vars <- sum(factor_model$frequency == 12)
all_data <- scale(factor_model$data)
cross_val_res <- crossVal(data = all_data, variable_of_interest = 1, fcast_horizon = 0,
                                 delay = factor_model$delay, frequency = factor_model$frequency, 
                                 no_of_factors = no_of_fact_test$test$no_of_factors, seed = seed, 
                                 min_ridge_penalty = 1e-6, max_ridge_penalty = 1e+5, cv_repetitions = 5,
                                 cv_size = 100, lasso_penalty_type = "selected", min_max_penalty = c(5, no_of_mtly_vars),
                                 verbose = FALSE)
print(cross_val_res)
cross_val_plot <- plot(cross_val_res)
cross_val_plot$`CV Results`

## ----estim_sparse_dfm, eval = TRUE, message = FALSE---------------------------
mtly_data <- scale(factor_model$data[, which(factor_model$frequency == 12)])
mtly_delay <- factor_model$delay[which(factor_model$frequency == 12)]
inferred_no_of_selected <- cross_val_res$CV$`Min. CV`[3:(3 + no_of_fact_test$test$no_of_factors - 1)]
inferred_ridge_penalty <- cross_val_res$CV$`Min. CV`[1]
sdfm_fit <- twoStepSDFM(data = mtly_data, delay = mtly_delay, selected = inferred_no_of_selected, 
                        no_of_fact_test$test$no_of_factors, ridge_penalty = inferred_ridge_penalty)
print(sdfm_fit)
sdfm_fit_plot <- plot(sdfm_fit)
sdfm_fit_plot$`Factor Time Series Plots`
sdfm_fit_plot$`Loading Matrix Heatmap`

## ----predict, eval = TRUE, message = FALSE------------------------------------
all_data <- scale(factor_model$data)
inferred_no_of_selected <- cross_val_res$CV$`Min. CV`[3:(3 + no_of_fact_test$test$no_of_factors - 1)]
inferred_ridge_penalty <- cross_val_res$CV$`Min. CV`[1]
nc_results <- nowcast(data = all_data, variables_of_interest = c(1, 2), max_fcast_horizon = 2, 
                      delay = factor_model$delay, ridge_penalty = inferred_ridge_penalty, selected = inferred_no_of_selected, frequency = factor_model$frequency, no_of_factors = no_of_fact_test$test$no_of_factors)
nc_plot <- plot(nc_results)
nc_plot$`Single Pred. Fcast Density Plots Series 1`
nc_plot$`Single Pred. Fcast Density Plots Series 2`

