library(plumber)
r <- plumb("deploy_model.R")
#r$run(host='138.251.16.15', port=6767)
r$run(host='127.0.0.1', port=6767)
