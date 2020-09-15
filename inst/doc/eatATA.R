## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----installation, eval = FALSE-----------------------------------------------
#  devtools::install_github("beckerbenj/eatATA")

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

## ----item usage constraints---------------------------------------------------
itemOverlap <- itemUsageConstraint(nForms, nItems = nItems, operator = "=") 

## ----categorical constraints--------------------------------------------------
# item formats
mc_openItems <- autoItemValuesMinMax(nForms = nForms, itemValues = items$MC_open_none)
cmcItems <- autoItemValuesMinMax(nForms = nForms, itemValues = items$f_CMC)
saItems <- autoItemValuesMinMax(nForms = nForms, itemValues = items$short_answer, allowedDeviation = 1)

# difficulty categories
Items1 <- autoItemValuesMinMax(nForms = nForms, itemValues = items$diff_1, allowedDeviation = 1)
Items2 <- autoItemValuesMinMax(nForms = nForms, itemValues = items$diff_2, allowedDeviation = 1)
Items3 <- autoItemValuesMinMax(nForms = nForms, itemValues = items$diff_3, allowedDeviation = 1)
Items4 <- autoItemValuesMinMax(nForms = nForms, itemValues = items$diff_4, allowedDeviation = 1)
Items5 <- autoItemValuesMinMax(nForms = nForms, itemValues = items$diff_5, allowedDeviation = 1)

## ----exclusion demo-----------------------------------------------------------
# item exclusions variable
items$exclusions[1:5]

## ----exclusion constraints----------------------------------------------------
# item exclusions
exclusionTuples <- itemExclusionTuples(items, idCol = "Item_ID", 
                                       exclusions = "exclusions", sepPattern = ", ")
excl_constraints <- itemExclusionConstraint(nForms = 14, exclusionTuples = exclusionTuples, 
                                            itemIDs = items$Item_ID)

## ----item numbers constraints-------------------------------------------------
# number of items per test form
min_Nitems <- floor(nItems / nForms) - 3
noItems <- itemsPerFormConstraint(nForms = nForms, nItems = nItems, 
                                  operator = ">=", min_Nitems)


## ----target constraints-------------------------------------------------------
# optimize average time
av_time <- itemTargetConstraint(nForms, nItems = nItems, itemValues = items$RT_in_min, targetValue = 10)

## ----prepare constraints, eval = T--------------------------------------------
# Prepare constraints
gurobi_constr <- list(itemOverlap, mc_openItems, cmcItems, saItems, 
                      Items1, Items2, Items3, Items4, Items5, excl_constraints,
                      av_time)

gurobi_rdy <- prepareConstraints(gurobi_constr, nForms = nForms, nItems = nItems)

## ----solver, eval = FALSE-----------------------------------------------------
#  library(gurobi)
#  
#  # Optimization
#  solver_raw <- gurobi(gurobi_rdy, params = list(TimeLimit = 30))
#  

## ----feasible output, eval = TRUE, echo = FALSE-------------------------------
out_path <- system.file("extdata", "gurobi_out_feasible.RDS", package = "eatATA")
cat(readRDS(out_path))

## ----infeasible output, eval = TRUE, echo = FALSE-----------------------------
out_path <- system.file("extdata", "gurobi_out_infeasible.RDS", package = "eatATA")
cat(readRDS(out_path))

## ----load solver output, eval = TRUE, echo = FALSE----------------------------
solver_raw <- gurobiExample

## ----inspect solution---------------------------------------------------------
out_list <- processGurobiOutput(solver_raw, items = items, nForms = nForms, output = "list")

## first two booklets
out_list[1:2]

## ----block exclusions---------------------------------------------------------
analyzeBlockExclusion(processedObj = out_list, idCol = "Item_ID", exclusionTuples = exclusionTuples)


## ----export solution to excel, eval = FALSE-----------------------------------
#  devtools::install_github("beckerbenj/eatAnalysis")
#  out_df <- processGurobiOutput(solver_raw, items = items, nForms = nForms, output = "data.frame")
#  
#  eatAnalysis::write_xlsx(out_df, filePath = "example_excel.xlsx",
#                          row.names = FALSE)

