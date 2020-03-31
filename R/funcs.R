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

predict_open <- function(x){
  #create weights
  vector_open<- open_dict$ope
  names(vector_open) <-  open_dict$term

  #create document-feature matrix of the dictionary
  dfm_text <- dfm(tolower(x), select = open_dict$term, ngram = c(1:3),
                  valuetype = "fixed")

  #apply weights
  dfm_text_wighted <- dfm_text %>%
    dfm_weight(scheme = "boolean") %>%
    dfm_weight(weights = vector_open, force = T)

  #create a vector of opens and add the intercept of the model
  open <-  rowSums(dfm_text_wighted)
  names(open) <- NULL
  return(scales::rescale(open, to = c(1,5)))
}

predict_conc <- function(x){
  #create weights
  vector_conc<- conc_dict$con
  names(vector_conc) <-  conc_dict$term

  #create document-feature matrix of the dictionary
  dfm_text <- dfm(tolower(x), select = conc_dict$term, ngram = c(1:3),
                  valuetype = "fixed")

  #apply weights
  dfm_text_wighted <- dfm_text %>%
    dfm_weight(scheme = "boolean") %>%
    dfm_weight(weights = vector_conc, force = T)

  #create a vector of concs and add the intercept of the model
  conc <-  rowSums(dfm_text_wighted)
  names(conc) <- NULL
  return(scales::rescale(conc, to = c(1,5)))
}

predict_ext <- function(x){
  #create weights
  vector_ext<- ext_dict$ext
  names(vector_ext) <-  ext_dict$term

  #create document-feature matrix of the dictionary
  dfm_text <- dfm(tolower(x), select = ext_dict$term, ngram = c(1:3),
                  valuetype = "fixed")

  #apply weights
  dfm_text_wighted <- dfm_text %>%
    dfm_weight(scheme = "boolean") %>%
    dfm_weight(weights = vector_ext, force = T)

  #create a vector of exts and add the intercept of the model
  ext <-  rowSums(dfm_text_wighted)
  names(ext) <- NULL
  return(scales::rescale(ext, to = c(1,5)))
}

predict_agr <- function(x){
  #create weights
  vector_agr<- agr_dict$agr
  names(vector_agr) <-  agr_dict$term

  #create document-feature matrix of the dictionary
  dfm_text <- dfm(tolower(x), select = agr_dict$term, ngram = c(1:3),
                  valuetype = "fixed")

  #apply weights
  dfm_text_wighted <- dfm_text %>%
    dfm_weight(scheme = "boolean") %>%
    dfm_weight(weights = vector_agr, force = T)

  #create a vector of agrs and add the intercept of the model
  agr <-  rowSums(dfm_text_wighted)
  names(agr) <- NULL
  return(scales::rescale(agr, to = c(1,5)))
}

predict_neu <- function(x){
  #create weights
  vector_neu<- neu_dict$neu
  names(vector_neu) <-  neu_dict$term

  #create document-feature matrix of the dictionary
  dfm_text <- dfm(tolower(x), select = neu_dict$term, ngram = c(1:3),
                  valuetype = "fixed")

  #apply weights
  dfm_text_wighted <- dfm_text %>%
    dfm_weight(scheme = "boolean") %>%
    dfm_weight(weights = vector_neu, force = T)

  #create a vector of neus and add the intercept of the model
  neu <-  rowSums(dfm_text_wighted)
  names(neu) <- NULL
  return(scales::rescale(neu, to = c(1,5)))
}

