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

## ----import items, message = FALSE--------------------------------------------
items_path <- system.file("extdata", "items.xlsx", package = "eatATA")

items <- as.data.frame(readxl::read_excel(path = items_path), stringsAsFactors = FALSE)

## ----inspect items, message = FALSE-------------------------------------------
head(items)

## ----dummies to factors, error = TRUE-----------------------------------------
# clean data set (categorical dummy variables must contain only 0 and 1)
items <- dummiesToFactor(items, dummies = c("MC", "CMC", "short_answer", "open"), facVar = "itemFormat")
items <- dummiesToFactor(items, dummies = paste0("diff_", 1:5), facVar = "itemDiff")
items[c(24, 33, 37, 47, 48, 54, 76), ]

## ----data cleaning, message = FALSE-------------------------------------------
# make new factor with three levels: "MC", "open" and "else"
items <- dummiesToFactor(items, dummies = c("MC", "open"), facVar = "MC_open_none")
# clean data set (NA should be 0)
for(ty in c(paste0("diff_", 1:5), "CMC", "short_answer")){
  items[, ty] <- ifelse(is.na(items[, ty]), yes = 0, no = items[, ty])
}
# make factors of CMC dummi
items$f_CMC <- factor(items$CMC, labels = paste("CMC", c("no", "yes"), sep = "_"))

# example item format
table(items$short_answer)

## ----fixed variables, message = FALSE-----------------------------------------
# set up fixed variables
nItems <- nrow(items)  # number of items
nForms <- 14           # number of blocks

## ----target constraints-------------------------------------------------------
# optimize average time
av_time <- minimaxObjective(nForms, itemValues = items$time, targetValue = 10,
                             itemIDs = items$item)

## ----item usage constraints---------------------------------------------------
itemOverlap <- itemUsageConstraint(nForms, targetValue = 1, 
                                   operator = "=", itemIDs = items$item) 

## ----categorical constraints--------------------------------------------------
# item formats
mc_openItems <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$MC_open_none, 
                                     itemIDs = items$item)
cmcItems <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$f_CMC, itemIDs = items$item)
saItems <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$short_answer, 
                                allowedDeviation = 1, itemIDs = items$item)

# difficulty categories
Items1 <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$diff_1, 
                               allowedDeviation = 1, itemIDs = items$item)
Items2 <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$diff_2, 
                               allowedDeviation = 1, itemIDs = items$item)
Items3 <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$diff_3, 
                               allowedDeviation = 1, itemIDs = items$item)
Items4 <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$diff_4, 
                               allowedDeviation = 1, itemIDs = items$item)
Items5 <- autoItemValuesMinMaxConstraint(nForms = nForms, itemValues = items$diff_5, 
                               allowedDeviation = 1, itemIDs = items$item)

## ----exclusion demo-----------------------------------------------------------
# item exclusions variable
items$exclusions[1:5]

## ----exclusion constraints----------------------------------------------------
# item exclusions
exclusionTuples <- itemTuples(items, idCol = "item", 
                                       infoCol = "exclusions", sepPattern = ", ")
excl_constraints <- itemExclusionConstraint(nForms = 14, itemTuples = exclusionTuples, 
                                            itemIDs = items$item)

## ----item numbers constraints-------------------------------------------------
# number of items per test form
min_Nitems <- floor(nItems / nForms) - 3
noItems <- itemsPerFormConstraint(nForms = nForms, operator = ">=", 
                                  targetValue = min_Nitems, itemIDs = items$item)


## ----prepare constraints, eval = T--------------------------------------------
# Prepare constraints
constr_list <- list(itemOverlap, mc_openItems, cmcItems, saItems, 
                      Items1, Items2, Items3, Items4, Items5, 
                      excl_constraints,
                      av_time)

## ----solver_local, eval = FALSE, echo = FALSE---------------------------------
#  # Local optimization chunk; prevents occasional errors on servers if no feasible solution is found
#  output_local <- capture.output(solver_raw_local <- useSolver(constr_list, nForms = nForms, nItems = nItems,
#                          itemIDs = items$item, solver = "GLPK", timeLimit = 30))
#  saveRDS(solver_raw_local, "use_case_solver_out.RDS")
#  saveRDS(output_local, "use_case_output.RDS")
#  

## ----solver, eval = FALSE, message=TRUE---------------------------------------
#  # Optimization
#  solver_raw <- useSolver(constr_list, nForms = nForms, nItems = nItems,
#                          itemIDs = items$item, solver = "GLPK", timeLimit = 10)
#  

## ----solver_load, eval = TRUE, echo = FALSE-----------------------------------
solver_raw <- readRDS("use_case_solver_out.RDS")
output_local <- readRDS("use_case_output.RDS")
output_local
message("The solution is feasible, but may not be optimal")

## ----inspect solution---------------------------------------------------------
out_list <- inspectSolution(solver_raw, items = items, idCol = "item", colSums = TRUE,
                            colNames = c("time", "subitems", 
                                         "MC", "CMC", "short_answer", "open",
                                         paste0("diff_", 1:5)))

# first two booklets
out_list[1:2]

## ----block exclusions---------------------------------------------------------
analyzeBlockExclusion(solverOut = solver_raw, item = items, idCol = "item", 
                      exclusionTuples = exclusionTuples)


## ----append solution----------------------------------------------------------
out_df <- appendSolution(solver_raw, items = items, idCol = "item")

## ----export solution to excel, eval = FALSE-----------------------------------
#  devtools::install_github("beckerbenj/eatAnalysis")
#  
#  eatAnalysis::write_xlsx(out_df, filePath = "example_excel.xlsx",
#                          row.names = FALSE)

