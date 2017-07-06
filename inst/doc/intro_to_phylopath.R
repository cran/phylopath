## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(dev = "png", fig.height = 5, fig.width = 5, dpi = 300, out.width = "450px")

## ------------------------------------------------------------------------
library(phylopath)

models <- list(
  one   = DAG(LS ~ BM, NL ~ BM, DD ~ NL, RS ~ DD),
  two   = DAG(LS ~ BM, NL ~ BM, DD ~ NL, RS ~ LS + DD),
  three = DAG(LS ~ BM, NL ~ BM, DD ~ NL, RS ~ NL),
  four  = DAG(LS ~ BM, NL ~ BM, DD ~ NL, RS ~ BM + NL),
  five  = DAG(LS ~ BM, NL ~ BM, DD ~ NL, RS ~ BM + NL + DD),
  six   = DAG(LS ~ BM, NL ~ BM + RS, DD ~ NL, RS ~ BM),
  seven = DAG(LS ~ BM, NL ~ BM + RS, DD ~ NL, RS ~ LS + BM),
  eight = DAG(LS ~ BM, NL ~ BM + RS, DD ~ NL),
  nine  = DAG(LS ~ BM, NL ~ BM + RS, DD ~ NL, RS ~ LS)
)

## ------------------------------------------------------------------------
models$one

## ---- fig.height = 5, fig.width = 5, dpi = 300---------------------------
plot(models$one)

## ---- fig.height=8, fig.width=8, out.width = "600px"---------------------
plot_model_set(models)

## ------------------------------------------------------------------------
result <- phylo_path(models, data = rhino, tree = rhino_tree, 
                     order = c('BM', 'NL', 'DD', 'LS', 'RS'))

## ------------------------------------------------------------------------
result

## ------------------------------------------------------------------------
summary(result)

## ------------------------------------------------------------------------
(best_model <- best(result))

## ---- warning = FALSE, fig.width = 6-------------------------------------
plot(best_model)

## ---- fig.width = 7------------------------------------------------------
average_model <- average(result)
plot(average_model, algorithm = 'mds', curvature = 0.1) # increase the curvature to avoid overlapping edges

## ---- fig.width = 7------------------------------------------------------
average_model_full <- average(result, method = "full")
plot(average_model_full, algorithm = 'mds', curvature = 0.1)

## ------------------------------------------------------------------------
coef_plot(best_model)

## ---- fig.height=3.5-----------------------------------------------------
coef_plot(average_model_full, reverse_order = TRUE) + 
  ggplot2::coord_flip() + 
  ggplot2::theme_bw()

## ------------------------------------------------------------------------
result$d_sep$one

