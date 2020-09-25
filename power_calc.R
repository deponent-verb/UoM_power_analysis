#' power_calc function
#' 
#' Computes the power of detecting a fixed intervention effect giving a set of modelling parameters.
#'
#' @param seasonal_eff: A  tibble of vectors containing the shape1 and shape2 parameters for the Beta
#' distribution. This represents seasonal effects which are ~Beta(shape1,shape2). Name each class. 
#' @param city_eff: A  tibble of vectors containing the shape1 and shape2 parameters for the Beta
#' distribution. This represents city effects which are ~Beta(shape1,shape2).Name each class.
#' @param community_eff: A  tibble of vectors containing the shape1 and shape2 parameters for the Beta
#' distribution. This represents community effects which are ~Beta(shape1,shape2).Name each class.
#' @param intervention_eff: A multiplicative modifier indicating the effect of the intervention on the 
#' infection probability, relative to no intervention.
#' @param num_ppl Number of individuals to simulate for each combination of predictors.
#' @param runs: Number of times to sample each of the predictor effects to make a final dataframe.
#' @param sig: Significance level.
#' @param num_sims: Number of dataframes to simulate and fit the model, in order to conduct for estimating power.
#'
#' @return The estimated power for detecting the intervention effect in a standard logistic regression model.
#' as a tibble. 
#' @export
#'
#' @examples
#' 
#' Dependency: source("./pandemic_sim.R")
power_calc <- function(seasonal_eff, city_eff, community_eff, intervention_eff,num_ppl,runs, sig, num_sim){
  res = rep(NA,num_sim)
  for(i in 1:num_sims){
    #generate data
    sim = purrr::rerun(runs, pandemic_sim(seasonal_eff = seasonal_eff,
                                           city_eff = city_eff,
                                           community_eff = community_eff,
                                           intervention_eff = tibble::tibble(name = c("yes","no"), 
                                                                             value = c(intervention_eff,1)),
                                           num_ppl = num_ppl))
    #bind data into single dataframe
    df = do.call(rbind,sim)
    #fit logistic model
    model = glm(disease ~., data = df, family = "binomial")
    res[i] = summary(model)$coefficients["interventionyes","Pr(>|z|)"] < alpha
  }
  power = sum(res)/length(res)
  result = tibble::tibble(intervention_eff = intervention_eff, power = power)
  return(result)
}