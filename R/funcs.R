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
  #make tri-grams
  open_dict$term <- stringr::str_replace_all(open_dict$term, " ", "_")
  #create weights
  vector_open<- open_dict$ope
  names(vector_open) <-  open_dict$term

  #create document-feature matrix of the dictionary
  toks <- tokens(x) %>%
    tokens_tolower() %>% 
    tokens_ngrams(n = 1:3)
  
  dfm_text <- dfm(toks, select = open_dict$term, valuetype = "fixed")

  #apply weights
  dfm_text_wighted <- suppressWarnings({dfm_weight(dfm_text, weights = vector_open)})
    
  #create a vector of opens
  open <-  rowSums(dfm_text_wighted)
  names(open) <- NULL
  return(return(open))
}

predict_conc <- function(x){
  #make tri-grams
  conc_dict$term <- stringr::str_replace_all(conc_dict$term, " ", "_")
  
  #create weights
  vector_conc<- conc_dict$con
  names(vector_conc) <-  conc_dict$term

  #create document-feature matrix of the dictionary
  toks <- tokens(x) %>%
    tokens_tolower() %>% 
    tokens_ngrams(n = 1:3)
  
  dfm_text <- dfm(toks, select = conc_dict$term, valuetype = "fixed")
  
  #apply weights
  dfm_text_wighted <- suppressWarnings({dfm_weight(dfm_text, weights = vector_conc)})
  
  #create a vector of concs
  conc <-  rowSums(dfm_text_wighted)
  names(conc) <- NULL
  return(conc)
}

predict_ext <- function(x){
  #make tri-grams
  ext_dict$term <- stringr::str_replace_all(ext_dict$term, " ", "_")
  
  #create weights
  vector_ext<- ext_dict$ext
  names(vector_ext) <-  ext_dict$term
  
  #create document-feature matrix of the dictionary
  toks <- tokens(x) %>%
    tokens_tolower() %>% 
    tokens_ngrams(n = 1:3)
  
  dfm_text <- dfm(toks, select = ext_dict$term, valuetype = "fixed")
  
  #apply weights
  dfm_text_wighted <- suppressWarnings({dfm_weight(dfm_text, weights = vector_ext)})
  
 #create a vector of exts
  ext <-  rowSums(dfm_text_wighted)
  names(ext) <- NULL
  return(ext)
}

predict_agr <- function(x){
  #make tri-grams
  agr_dict$term <- stringr::str_replace_all(agr_dict$term, " ", "_")
  
  #create weights
  vector_agr<- agr_dict$agr
  names(vector_agr) <-  agr_dict$term

  #create document-feature matrix of the dictionary
  toks <- tokens(x) %>%
    tokens_tolower() %>% 
    tokens_ngrams(n = 1:3)
  
  dfm_text <- dfm(toks, select = agr_dict$term, valuetype = "fixed")
  
  #apply weights
  dfm_text_wighted <- suppressWarnings({dfm_weight(dfm_text, weights = vector_agr)})
  
  #create a vector of agrs
  agr <-  rowSums(dfm_text_wighted)
  names(agr) <- NULL
  return(agr)
}

predict_neu <- function(x){
  #make tri-grams
  neu_dict$term <- stringr::str_replace_all(neu_dict$term, " ", "_")
  
  #create weights
  vector_neu<- neu_dict$neu
  names(vector_neu) <-  neu_dict$term

  #create document-feature matrix of the dictionary
  toks <- tokens(x) %>%
    tokens_tolower() %>% 
    tokens_ngrams(n = 1:3)
  
  dfm_text <- dfm(toks, select = neu_dict$term, valuetype = "fixed")
  
  #apply weights
  dfm_text_wighted <- suppressWarnings({dfm_weight(dfm_text, weights = vector_neu)})
  
  #create a vector of neus
  neu <-  rowSums(dfm_text_wighted)
  names(neu) <- NULL
  return(neu)
}

