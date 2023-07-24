library("mlr3")
library("mlr3fairness")

# https://github.com/mlr-org/mlr3/blob/HEAD/R/MeasureClassif.R
# https://github.com/mlr-org/mlr3/blob/HEAD/R/Measure.R

# create classification measure
MeasureClassifP <- R6::R6Class("MeasureClassifP",
  inherit = mlr3::MeasureClassif,
  public = list(
    initialize = function(name, param_set = NULL) {
      super$initialize(id = paste0("classif.", name), predict_type = "prob", task_properties = "twoclass", range = c(0, Inf), minimize = NA)
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      p_total = prediction$score(msr("classif.tp")) + prediction$score(msr("classif.fp")) + prediction$score(msr("classif.tn")) + prediction$score(msr("classif.fn"))
      if(self$id == "classif.pp") {
        (prediction$score(msr("classif.tp")) + prediction$score(msr("classif.fp"))) / p_total
      }else{
        (prediction$score(msr("classif.tn")) + prediction$score(msr("classif.fn"))) / p_total
      }
    }
  )
)

# define fairness metric
# https://mlr3fairness.mlr-org.com/articles/measures-vignette.html#fairness-measures---an-in-depth-look
fairness_pp <- MeasureFairness$new(base_measure = MeasureClassifP$new(name = "pp"), minimize = TRUE, range = c(0,Inf), operation = function(x) {(x[1] - x[2]) * log(x[1] / x[2])})
fairness_np <- MeasureFairness$new(base_measure = MeasureClassifP$new(name = "np"), minimize = TRUE, range = c(0,Inf), operation = function(x) {(x[1] - x[2]) * log(x[1] / x[2])})

# create composite metric
# https://mlr3fairness.mlr-org.com/articles/measures-vignette.html#composite-fairness-measures
mlr3::mlr_measures$add("fairness.gui", MeasureFairnessComposite$new(minimize = TRUE, range = c(0,Inf), measures = list(fairness_pp, fairness_np), aggfun = sum))