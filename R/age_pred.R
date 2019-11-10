data(.sysdata, envir=environment())

predict_age <- function(x){
  #create weights
  vector_age <- age_dict$weight[-1]
  names(vector_age) <-  age_dict$term[-1]

  #create document-feature matrix of the dictionary
  dfm_text <- dfm(x, select = age_dict$term[-1],
                  valuetype = "fixed")

  #apply weights
  dfm_text_wighted <- dfm_text %>%
    dfm_weight(scheme = "prop") %>%
    dfm_weight(weights = vector_age)

  #create a vector of ages and add the intercept of the model
  age <-  rowSums(dfm_text_wighted)
  age <- age + age_dict$weight[1]
  names(age) <- NULL
  return(age)
}
