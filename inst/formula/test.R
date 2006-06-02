
library(Biobase)
source("code.R")
library(golubEsets)
data(Golub_Train)

library(RWeka)

gg = J48(ALL.AML~., data=Golub_Train[1:2,])

