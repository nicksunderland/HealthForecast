#' Title
#' @slot file_path file path to diagnosis details
#' @slot code character.
#' @slot type character.
#' @slot descr_1 character.
#' @slot descr_2 character.
#' @slot descr_3 character.
#' @slot descr_4 character.
#' @importFrom tidyr tibble
#' @importFrom readr read_csv
#' @return Obj
#' @import methods
#' @export
Diagnosis <- setClass(

  # The class name
  Class = "Diagnosis",

  # Class variables
  representation(
    file_path = "character",
    code    = "character",
    type    = "character",
    descr_1 = "character",
    descr_2 = "character",
    descr_3 = "character",
    descr_4 = "character"),

  prototype(
    file_path = NA_character_,
    code    = NA_character_,
    type    = NA_character_,
    descr_1 = NA_character_,
    descr_2 = NA_character_,
    descr_3 = NA_character_,
    descr_4 = NA_character_)
)

setMethod(
  f = "initialize",
  signature = "Diagnosis",
  definition = function(.Object, ..., file_path=NA_character_){

    if(!is.na(file_path)){

      t = readr::read_csv(file_path, show_col_types=F)
      .Object@code    = as.character(t$code)
      .Object@type    = as.character(t$type)
      .Object@descr_1 = as.character(t$descr_1)
      .Object@descr_2 = as.character(t$descr_2)
      .Object@descr_3 = as.character(t$descr_3)
      .Object@descr_4 = as.character(t$descr_4)

    }else{

      .Object <- callNextMethod()

    }

    return(.Object)
  }
)

setValidity("Diagnosis", function(object) {
  lens  = sapply(list(object@code, object@type, object@descr_1, object@descr_2, object@descr_3, object@descr_4), length)
  max_l = max(lens, na.rm=T)
  if(any(!(lens %in% c(1, max_l)))){
    "Diagnosis code:description vectors must be the same length, or length 1. If length 1 they will be recycled and applied to all diagnoses."
  }else{
    TRUE
  }
})

setGeneric("initDxMatrix", function(object) standardGeneric("initDxMatrix"))
setMethod("initDxMatrix", "Diagnosis",
          function(object){
            tidyr::tibble("code" = object@code,
                          "type" = object@type,
                          "descr_1" = object@descr_1,
                          "descr_2" = object@descr_2,
                          "descr_3" = object@descr_3,
                          "descr_4" = object@descr_4)
          }
)

setGeneric("diagnosisMatrix", function(object) standardGeneric("diagnosisMatrix"))
setMethod("diagnosisMatrix", "Diagnosis",
          function(object){
            initDxMatrix(object)
          }
)

setMethod("show", "Diagnosis", function(object) print(initDxMatrix(object)))
