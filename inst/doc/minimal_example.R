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
str(items_mini)

# calculate and append IIF
items_mini[, "IIF_0"] <- calculateIIF(B = items_mini$difficulty, theta = 0)

## ----item pool, eval = TRUE, results = 'asis', echo = FALSE-------------------
# create content bounderies in advance
knitr::kable(items_mini[1:5,], digits = 3, caption = "Table 1. First 5 Items of the Item Pool")

## ----objFun, message = FALSE--------------------------------------------------
testInfo <- maxObjective(nForms = 1, itemValues = items_mini$IIF,
                          itemIDs = items_mini$item)

## ----constraints, message = FALSE---------------------------------------------
itemNumber <- itemsPerFormConstraint(nForms = 1, operator = "=", 
                                     targetValue = 10, 
                                     itemIDs = items_mini$item)

itemUsage <- itemUsageConstraint(nForms = 1, operator = "<=", 
                                 targetValue = 1, 
                                 itemIDs = items_mini$item)

testTime <- itemValuesDeviationConstraint(nForms = 1, 
                                itemValues = items_mini$time,
                                targetValue = 8 * 60, 
                                allowedDeviation = 5,
                                relative = FALSE, 
                                itemIDs = items_mini$item)

## ----test time, message = TRUE------------------------------------------------
testTime2 <- autoItemValuesMinMaxConstraint(nForms = 1, 
                                itemValues = items_mini$time,
                                testLength = 10, 
                                allowedDeviation = 5,
                                relative = FALSE, 
                                itemIDs = items_mini$item)

## ----solver, message = FALSE--------------------------------------------------
solver_out <- useSolver(list(itemNumber, itemUsage, testTime, testInfo),
                        solver = "GLPK")

## ----inspect, message = FALSE-------------------------------------------------
inspectSolution(solver_out, items = items_mini, idCol = "item")

## ----append, message = FALSE--------------------------------------------------
appendSolution(solver_out, items = items_mini, idCol = "item")

