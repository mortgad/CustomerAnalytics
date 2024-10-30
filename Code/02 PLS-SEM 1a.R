rm(list=ls())
#install.packages("seminr")
library(seminr)
# Load data
satisfaction <- read.csv(file = "Code/Data/satisfaction.csv", header = TRUE, sep = " ")

## Original model
# Specifying the measurement model 
simple_mm <- constructs(
  composite("IMAG", multi_items("imag", 1:5), weights = mode_B),
  composite("EXPE", c("expe1", "expe2", "expe3", "expe4", "expe5"), weights = mode_A), #Alternative
  composite("QUAL", multi_items("qual", 1:5), weights = mode_A),
  composite("VAL", multi_items("val", 1:4), weights = mode_A),
  composite("SAT", multi_items("sat", 1:4), weights = mode_A),
  composite("LOY", multi_items("loy", 1:4), weights = mode_A))
# Specifying the structural model 
simple_sm <- relationships(
  paths(from = c("IMAG"), to = c("EXPE", "SAT", "LOY")),
  paths(from = c("EXPE"), to = c("QUAL","VAL","SAT")),
  paths(from = c("QUAL"), to = c("VAL", "SAT")),
  paths(from = c("VAL"), to = c("SAT")),
  paths(from = c("SAT"), to = c("LOY")))
# Estimate the model
sat_model <- estimate_pls(data = satisfaction,
                          measurement_model = simple_mm,
                          structural_model = simple_sm,
                          inner_weights = path_weighting,
                          missing = mean_replacement,
                          missing_value = "-99")
# Summarize the model results
summary_sat_model <- summary(sat_model)
# Inspect the indicator loadings
summary_sat_model$loadings
# Inspect the model's path coefficients and the R^2 values
summary_sat_model$paths

## Model 1
# Specifying the measurement model 
simple_mm <- constructs(
  composite("IMAG", multi_items("imag", 1:5), weights = mode_B),
  composite("EXPE", multi_items("expe", 1:5), weights = mode_A),
  composite("QUAL", multi_items("qual", 1:5), weights = mode_A),
  composite("VAL", multi_items("val", 1:4), weights = mode_A),
  composite("SAT", multi_items("sat", 1:4), weights = mode_A),
  composite("LOY", multi_items("loy", 1:4), weights = mode_A))
# Specifying the structural model 
simple_sm <- relationships(
  paths(from = c("IMAG"), to = c("EXPE", "SAT")),
  paths(from = c("EXPE"), to = c("QUAL", "SAT")),
  paths(from = c("QUAL"), to = c("VAL")),
  paths(from = c("VAL"), to = c("SAT")),
  paths(from = c("SAT"), to = c("LOY")))
# Estimate the model
sat_model <- estimate_pls(data = satisfaction,
                          measurement_model = simple_mm,
                          structural_model = simple_sm,
                          inner_weights = path_weighting,
                          missing = mean_replacement,
                          missing_value = "-99")
# Summarize the model results
summary_sat_model <- summary(sat_model)
# Inspect the indicator loadings
summary_sat_model$loadings
# Inspect the model's path coefficients and the R^2 values
summary_sat_model$paths

## Model 2
# Specifying the measurement model 
simple_mm <- constructs(
  composite("IMAG", multi_items("imag", 1:5), weights = mode_B),
  composite("EXPE", c("val1","expe1", "expe2", "expe3", "expe4", "expe5"), weights = mode_A),
  composite("QUAL", multi_items("qual", 1:4), weights = mode_A),
  composite("VAL", multi_items("val", 2:4), weights = mode_A),
  composite("SAT", c("sat1","sat2", "sat3", "sat4", "loy1"), weights = mode_A),
  composite("LOY", multi_items("loy", 2:4), weights = mode_A))
# Specifying the structural model 
simple_sm <- relationships(
  paths(from = c("IMAG"), to = c("EXPE", "SAT", "LOY")),
  paths(from = c("EXPE"), to = c("QUAL", "SAT")),
  paths(from = c("QUAL"), to = c("VAL")),
  paths(from = c("VAL"), to = c("SAT")),
  paths(from = c("SAT"), to = c("LOY")))
# Estimate the model
sat_model <- estimate_pls(data = satisfaction,
                          measurement_model = simple_mm,
                          structural_model = simple_sm,
                          inner_weights = path_weighting,
                          missing = mean_replacement,
                          missing_value = "-99")
# Summarize the model results
summary_sat_model <- summary(sat_model)
# Inspect the indicator loadings
summary_sat_model$loadings
# Inspect the model's path coefficients and the R^2 values
summary_sat_model$paths

## Model 3
# Specifying the measurement model 
simple_mm <- constructs(
  composite("IMAG", multi_items("imag", c(1,2,4,5)), weights = mode_A),
  composite("EXPE", multi_items("expe", 1:3), weights = mode_A),
  composite("QUAL", multi_items("qual", 1:5), weights = mode_A),
  composite("VAL", multi_items("val", 2:4), weights = mode_A),
  composite("SAT", c("sat1","sat2", "sat3", "sat4"), weights = mode_A),
  composite("LOY", c("val1","loy1","loy2", "loy3", "loy4"), weights = mode_A))
# Specifying the structural model 
simple_sm <- relationships(
  paths(from = c("IMAG"), to = c("EXPE", "SAT")),
  paths(from = c("EXPE"), to = c("QUAL", "SAT", "VAL")),
  paths(from = c("QUAL"), to = c("VAL")),
  paths(from = c("VAL"), to = c("SAT")),
  paths(from = c("SAT"), to = c("LOY")))
# Estimate the model
sat_model <- estimate_pls(data = satisfaction,
                          measurement_model = simple_mm,
                          structural_model = simple_sm,
                          inner_weights = path_weighting,
                          missing = mean_replacement,
                          missing_value = "-99")
# Summarize the model results
summary_sat_model <- summary(sat_model)
# Inspect the indicator loadings
summary_sat_model$loadings
# Inspect the model's path coefficients and the R^2 values
summary_sat_model$paths
