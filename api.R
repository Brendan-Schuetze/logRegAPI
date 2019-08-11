logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

prob2logit <- function(prob) {
  odds <- prob / (1 - prob)
  logit <- log(odds)
  return(logit)
}


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

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#' @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#' Plot out data from the iris dataset
#' @param spec If provided, filter the data to only this species (e.g. 'setosa')
#' @get /plot
#' @png
function(spec){
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)){
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }

  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
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

