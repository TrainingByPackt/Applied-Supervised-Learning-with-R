
pr <- plumber::plumb("model.R")
pr$run(port=8081)
