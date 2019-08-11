# Credit to Sebastian Sauer's blog for this function
# https://sebastiansauer.github.io/convert_logit2prob/
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

# Function that reverses logit2prob
prob2logit <- function(prob) {
  odds <- prob / (1 - prob)
  logit <- log(odds)
  return(logit)
}

#' Allow Cross-Origin Acess
#' This code from plumber github
#' @filter cors
cors <- function(req, res) {

  res$setHeader("Access-Control-Allow-Origin", "*")

  if (req$REQUEST_METHOD == "OPTIONS") {
    res$setHeader("Access-Control-Allow-Methods","*")
    res$setHeader("Access-Control-Allow-Headers", req$HTTP_ACCESS_CONTROL_REQUEST_HEADERS)
    res$status <- 200
    return(list())
  } else {
    plumber::forward()
  }

}

#' Return Logistic Regression Coefficients
#' @param x The IV
#' @param y The DV
#' @post /logReg
function(x, y, p) {
	x <- as.numeric(unlist(strsplit(x,split = ",")))
	y <- as.numeric(unlist(strsplit(y, split = ",")))

	logReg <- glm(y ~ x, family = "binomial")

	intercept <- logReg$coefficients[1]
	coef_log <- logReg$coefficients[2]

	rh <- prob2logit(as.numeric(p)) - intercept
	threshold <- rh / coef_log

	toString(threshold)
	#logits <- intercept + coef_log * as.numeric(p)
	#toString(paste("Prob ", logit2prob(logits)))

}

