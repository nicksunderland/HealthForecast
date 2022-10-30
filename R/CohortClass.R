#' Title
#'
#' @slot start_date T
#' @slot comorb_dx T
#' @slot index_dx T
#' @slot outcome_dx T
#' @slot patient_lst T
#' @slot end_date T
#'
#' @return T
#' @import methods
#' @include PatientClass.R DiagnosisClass.R
#' @export
Cohort <- setClass(

  # The class name
  Class = "Cohort",

  # Class member variables, others inherited from Patient class
  representation(
    start_date = "Date",
    end_date = "Date",
    comorb_dx = "Diagnosis",
    index_dx = "Diagnosis",
    outcome_dx = "Diagnosis",
    patient_lst = "Patient"
  ),

  prototype(
    start_date  = as.Date(NA),
    end_date = as.Date(NA),
    comorb_dx = Diagnosis(),
    index_dx = Diagnosis(),
    outcome_dx = Diagnosis(),
    patient_lst = Patient())

)

setMethod(
  f = "initialize",
  signature = "Cohort",
  definition = function(.Object, ...){
    .Object@patient_lst = Patient(...)
    return(.Object)
  }
)

setGeneric("dateWindow", function(object) standardGeneric("dateWindow"))
setMethod(
  f = "dateWindow",
  signature = "Cohort",
  definition = function(object){
    win = c("start_date" = object@start_date, "end_date" = object@end_date)
    return(win)
  }
)

setGeneric("dateWindow<-", function(object, value) standardGeneric("dateWindow<-"))
setMethod(
  f = "dateWindow<-",
  signature = "Cohort",
  definition = function(object, value){
    object@start_date <- value[1]
    object@end_date   <- value[2]
    validObject(object)
    return(object)
  }
)

setGeneric("idxDiagnoses", function(object) standardGeneric("idxDiagnoses"))
setMethod(
  f = "idxDiagnoses",
  signature = "Cohort",
  definition = function(object){ object@index_dx }
)

setGeneric("comorbDiagnoses", function(object) standardGeneric("comorbDiagnoses"))
setMethod(
  f = "comorbDiagnoses",
  signature = "Cohort",
  definition = function(object){ object@comorb_dx }
)

setGeneric("outcomeDiagnoses", function(object) standardGeneric("outcomeDiagnoses"))
setMethod(
  f = "outcomeDiagnoses",
  signature = "Cohort",
  definition = function(object){ object@outcome_dx }
)

setGeneric("setDiagnoses<-", function(object, dx_type, value) standardGeneric("setDiagnoses<-"))
setMethod(
  f = "setDiagnoses<-",
  signature = c("Cohort", "character"),
  definition = function(object, dx_type, value){

    if(!is(value, "Diagnosis") & file.exists(value)){
      dx = Diagnosis(file_path = value)
    }else{
      dx = value
    }

    if(dx_type == "index"){
      object@index_dx <- dx
    }else if(dx_type == "comorbidity"){
      object@comorb_dx <- dx
    }else if(dx_type == "outcome"){
      object@outcome_dx <- dx
    }

    validObject(object)
    return(object)
  }
)

setValidity("Cohort", function(object) {
  if(!is.na(object@start_date) & !is.na(object@end_date) & object@start_date >= object@end_date){
    "Cohort time window @start_date must be before @end_date"
  }else{
    TRUE
  }
})
