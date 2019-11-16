library(plumber)
r <- plumb("deploy_model.R")
r$run(host='138.251.16.15', port=6767)
