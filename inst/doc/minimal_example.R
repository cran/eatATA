## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installation, eval = FALSE-----------------------------------------------
#  install.packages("eatATA")

## ----library, message = FALSE-------------------------------------------------
# loading eatATA
library(eatATA)

# item pool structure
str(items_sim)

# calculate and append IIF
items_sim[, "IIF"] <- calculateIIF(B = items_sim$difficulty, theta = 0)

## ----objFun, message = FALSE--------------------------------------------------
testInfo <- maxConstraint(nForms = 1, itemValues = items_sim$IIF,
                          itemIDs = items_sim$id)

## ----constraints, message = FALSE---------------------------------------------
itemNumber <- itemsPerFormConstraint(nForms = 1, operator = "=", 
                                     targetValue = 10, 
                                     itemIDs = items_sim$id)

itemUsage <- itemUsageConstraint(nForms = 1, operator = "<=", 
                                 targetValue = 1, 
                                 itemIDs = items_sim$id)

testTime <- itemValuesDeviation(nForms = 1, 
                                itemValues = items_sim$mean_time,
                                targetValue = 8 * 60, 
                                allowedDeviation = 5,
                                relative = FALSE, 
                                itemIDs = items_sim$id)

## ----test time, message = TRUE------------------------------------------------
testTime2 <- autoItemValuesMinMax(nForms = 1, 
                                itemValues = items_sim$mean_time,
                                testLength = 10, 
                                allowedDeviation = 5,
                                relative = FALSE, 
                                itemIDs = items_sim$id)

## ----solver, message = FALSE--------------------------------------------------
solver_out <- useSolver(list(itemNumber, itemUsage, testTime, testInfo),
                        solver = "GLPK")

## ----inspect, message = FALSE-------------------------------------------------
inspectSolution(solver_out, items = items_sim, idCol = "id")

## ----append, message = FALSE--------------------------------------------------
appendSolution(solver_out, items = items_sim, idCol = "id")

