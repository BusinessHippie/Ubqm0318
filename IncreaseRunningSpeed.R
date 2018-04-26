#Speed up the computer

library(doParallel)
detectCores()
registerDoParallel(3)
