library(plumber)
pr <- plumb("api.R")
pr$run(port=80, host="0.0.0.0")

