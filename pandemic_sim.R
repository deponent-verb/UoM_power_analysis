#'pandemic_sim function
#'
#' Generates a dataframe to describe a pandemic across different communities over time. Each
#' observation is a single individual. The response varible is binary, indicating whether 
#' they have the disease (1) or not (0). The predictors are the month(high or low), city (categorical)
#',community(categorical) and intervention (yes or no). We assume that each individual observation
#' is conditionally independent, given we know all these other predictors. Note that the effects
#' of each predictor apply additively to the probability that an individual gets infected. The exception
#' is the intervention effect which acts multiplicatively. 
#'
#' @param seasonal_eff: A  list of vectors containing the shape1 and shape2 parameters for the Beta
#' distribution. This represents seasonal effects which are ~Beta(shape1,shape2).
#' @param city_eff A  list of vectors containing the shape1 and shape2 parameters for the Beta
#' distribution. This represents city effects which are ~Beta(shape1,shape2).
#' @param community_eff A  list of vectors containing the shape1 and shape2 parameters for the Beta
#' distribution. This represents community effects which are ~Beta(shape1,shape2).
#' @param intervention_eff A  list of vectors containing the shape1 and shape2 parameters for the Beta
#' distribution. This represents intervention effects which are ~Beta(shape1,shape2).
#' @param num_ppl Number of individuals to simulate for each combination of predictors 
#'
#' @return a dataframe with columns disease(response) and predictors (month, city, community, intervention)
#' @export
#'
#' @examples 
pandemic_sim <- function(seasonal_eff, city_eff, community_eff, intervention_eff, num_ppl){
  
  sim_list = list()
  sim_count = 1
  
  #loop over seasonal effects
  for(m in 1:nrow(seasonal_eff)){
    dist_s = seasonal_eff[m, ]
    eff_m = rbeta(n = 1, shape1 = dist_s$shape1, shape2 = dist_s$shape2)
    #loop over city effects
    for(c in 1:nrow(city_eff)){
      dist_c = city_eff[c,]
      eff_c = rbeta(n = 1, shape1 = dist_c$shape1, shape2 = dist_c$shape2)
      #loop over community effects
      for(com in 1:nrow(community_eff)){
        dist_com = community_eff[com,]
        eff_com = rbeta(n = 1, shape1 = dist_com$shape1, shape2 = dist_com$shape2)
        #loop over intervention effects
        for(i in 1:nrow(intervention_eff)){
          dist_int = intervention_eff[i,]
          eff_int = rbeta(n = 1, shape1 = dist_int$shape1, shape2 = dist_int$shape2)
          
          #add effects and make tibble for one individual
          eff_total = (eff_m + eff_c + eff_com) * eff_int
          
          #simulate individuals by generating a small tibble for each one
          for(ppl in 1:num_ppl){
            
            if(eff_total > 1){
              new_row = tibble::tibble(disease = 1, season = dist_s$name, 
                                       city = dist_c$name, community = dist_com$name,
                                       intervention = dist_int$name)
            } else {
              sick_roll = rbinom(n=1, size = 1, prob = eff_total)
              new_row = tibble::tibble(disease = sick_roll, season = dist_s$name, 
                                       city = dist_c$name, community = dist_com$name,
                                       intervention = dist_int$name)
            }
            sim_list[[sim_count]] = new_row
            sim_count = sim_count + 1
          }
        }
      }
    }
  }
  
  df = data.table::rbindlist(sim_list)
  df = as.data.frame(df)
  df[colnames(df)] <- lapply(df[colnames(df)],factor)
  #pandemic_data[colnames(pandemic_data)] <- lapply(pandemic_data[colnames(pandemic_data)], factor)
  
  return(df)
}