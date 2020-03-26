#' Title
#'
#' @param data 
#' @param varnames 
#'
#' @return
#'
#' @examples
#' 
#' @rdname fit_functions
#' @export
fit_medlyn <- function(data,
                       varnames = list(k = "k",
                                       Temp = "Temp")){
  data$k <- data[, varnames$k]
  data$Temp <- data[, varnames$Temp] + 273.15
  start_pars <- nlsLM(k ~ Arrh_base(k25,
                                    Ea,
                                    Temp = Temp),
                      data,
                      start = list(k25 = mean(data$k, na.rm = TRUE),
                                   Ea = coef(lm(k ~ Temp, data))[2]),
                      lower = c(0, 0),
                      control = nls.lm.control(maxiter = 100))

  model_cand <- vector("list", 1000)
  for (i in 1:1000){
    model_cand[[i]] <- 
      tryCatch(nlsLM(k ~ Arrh_medlyn(k25,
                                     delS,
                                     Ea,
                                     Hd,
                                     Temp = Temp),
                     data,
                     start = list(k25 = coef(start_pars)[[1]],
                                  Ea = coef(start_pars)[[2]],
                                  delS = 0.650,
                                  Hd = i),
                     lower = c(0, 0, 0, 0),
                     upper = c(2 * max(data$k),
                               3 * coef(start_pars)[[2]],
                               100, 5000),
                     control = nls.lm.control(maxiter = 500,
                                              maxfev = 2000)),
               error = function(e) paste(NA))
  }
  bics <- data.frame(cbind(1:1000, NA))
  colnames(bics) <- c("i", "BIC")
  for(i in 1:length(model_cand)){
  bics$BIC[i] <- tryCatch(BIC(model_cand[[i]]),
                          error = function(e) paste(NA))
  }
  j <- bics[bics$BIC == min(bics$BIC, na.rm = TRUE),]$i
  return(model_cand[[j]])
}

#' @rdname t_functions
#' @export
fit_new <- function(data,
                       varnames = list(k = "k",
                                       Temp = "Temp")){
  data$k <- data[, varnames$k]
  data$Temp <- data[, varnames$Temp] + 273.15
  start_pars <- nlsLM(k ~ Arrh_base(k25,
                                    Ea,
                                    Temp = Temp),
                      data,
                      start = list(k25 = mean(data$k, na.rm = TRUE),
                                   Ea = coef(lm(k ~ Temp, data))[2]),
                      lower = c(0, 0),
                      control = nls.lm.control(maxiter = 100))
  
  model_cand <- vector("list", 1000)
  for (i in 1:1000){
    model_cand[[i]] <- 
      tryCatch(nlsLM(k ~ Arrh_new(k25,
                                     delS,
                                     Ea,
                                     Hd,
                                     Temp = Temp),
                     data,
                     start = list(k25 = coef(start_pars)[[1]],
                                  Ea = coef(start_pars)[[2]],
                                  delS = 0.650,
                                  Hd = i),
                     lower = c(0, 0, 0, 0),
                     upper = c(2 * max(data$k),
                               3 * coef(start_pars)[[2]],
                               100, 5000),
                     control = nls.lm.control(maxiter = 500,
                                              maxfev = 2000)),
               error = function(e) paste(NA))
  }
  bics <- data.frame(cbind(1:1000, NA))
  colnames(bics) <- c("i", "BIC")
  for(i in 1:length(model_cand)){
    bics$BIC[i] <- tryCatch(BIC(model_cand[[i]]),
                            error = function(e) paste(NA))
  }
  j <- bics[bics$BIC == min(bics$BIC, na.rm = TRUE),]$i
  return(model_cand[[j]])
}

