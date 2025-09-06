library(testthat)
library(ape)

test_that("check_models_data_tree stops if models have different variables", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2"))),
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var3")))
  )
  data <- data.frame(Var1 = 1:3, Var2 = 1:3, Var3 = 1:3, row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- FALSE

  expect_error(check_models_data_tree(model_set, data, tree, na.rm),
               "All causal models need to include the same variables")
})

test_that("check_models_data_tree forces character columns to factors and checks binary factors", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = c("a", "b", "a"), Var2 = c("x", "x", "y"), row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- FALSE

  result <- check_models_data_tree(model_set, data, tree, na.rm)
  expect_true(is.factor(result$data$Var1))
  expect_true(is.factor(result$data$Var2))
})

test_that("check_models_data_tree stops if factors have more than two levels", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = c("a", "b", "c"), Var2 = c("x", "x", "y"), row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- FALSE

  expect_error(check_models_data_tree(model_set, data, tree, na.rm),
               "Variable 'Var1' is expected to binary, but has 3 levels")
})

test_that("check_models_data_tree warns if numeric columns are binary", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = c(0, 1, 0), Var2 = c(1, 0, 1), row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- FALSE

  expect_warning(check_models_data_tree(model_set, data, tree, na.rm),
                 "Column Var1 appears to have binary data, but was not recognized as binary")
})

test_that("check_models_data_tree stops if tree is multiPhylo", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = 1:3, Var2 = 1:3, row.names = letters[1:3])
  tree <- list(rtree(3), rtree(3))
  class(tree) <- "multiPhylo"
  na.rm <- FALSE

  expect_error(check_models_data_tree(model_set, data, tree, na.rm),
               "You are passing several trees")
})

test_that("check_models_data_tree stops if tree is not phylo", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = 1:3, Var2 = 1:3, row.names = letters[1:3])
  tree <- "not_a_tree"
  na.rm <- FALSE

  expect_error(check_models_data_tree(model_set, data, tree, na.rm),
               "The tree needs to be of class `phylo`")
})

test_that("check_models_data_tree removes rows with NA if na.rm is TRUE", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = c(1, NA, 3), Var2 = 1:3, row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- TRUE

  result <- check_models_data_tree(model_set, data, tree, na.rm)
  expect_equal(nrow(result$data), 2)
})

test_that("check_models_data_tree stops if there are NA values and na.rm is FALSE", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = c(1, NA, 3), Var2 = 1:3, row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- FALSE

  expect_error(check_models_data_tree(model_set, data, tree, na.rm),
               "NA values were found in the variables of interest")
})

test_that("check_models_data_tree stops if data and tree tip labels do not match", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = 1:3, Var2 = 1:3, row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[4:6]
  na.rm <- FALSE

  expect_error(check_models_data_tree(model_set, data, tree, na.rm),
               "Make sure that species in your data have rownames that are exactly matched by name with tips in the tree")
})

test_that("check_models_data_tree prunes tree if necessary", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = 1:2, Var2 = 1:2, row.names = letters[1:2])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- FALSE

  result <- check_models_data_tree(model_set, data, tree, na.rm)
  expect_equal(length(result$tree$tip.label), 2)
})

test_that("check_models_data_tree adds names to models if not provided", {
  model_set <- list(
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2"))),
    matrix(1, nrow = 1, ncol = 2, dimnames = list(NULL, c("Var1", "Var2")))
  )
  data <- data.frame(Var1 = 1:3, Var2 = 1:3, row.names = letters[1:3])
  tree <- rtree(3)
  tree$tip.label <- letters[1:3]
  na.rm <- FALSE

  result <- check_models_data_tree(model_set, data, tree, na.rm)
  expect_equal(names(result$model_set), c("A", "B"))
})

