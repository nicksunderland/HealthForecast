#' @title Patient Class
#' @field id character
#' @field dob Date
#' @return a new Patient object
#' @import methods
#' @export
Patient <- setClass(

  # The class name
  Class = "Patient",

  # Class variables
  representation(
    id  = "character",
    ethnicity = "character",
    dob = "Date",
    hosp_adm = "tbl"),

  prototype(
    id  = NA_character_,
    ethnicity = NA_character_,
    dob = as.Date(NA),
    hosp_adm = tidyr::tibble())

)


setMethod(
  f = "initialize",
  signature = "Patient",
  definition = function(.Object, ...){
    .Object <- callNextMethod()
    return(.Object)
  }
)

setGeneric("ethnicity",   function(x) standardGeneric("ethnicity"))
setMethod(
  f = "ethnicity",
  signature = "Patient",
  definition = function(x){ x@ethnicity })

setGeneric("ethnicity<-", function(x, value) standardGeneric("ethnicity<-"))
setMethod(
  f = "ethnicity<-",
  signature = "Patient",
  definition = function(x, value){
    if(length(x@id)==length(value)){
      x@ethnicity <- value
      return(x)
    }
    else if(length(x@id)!=length(value) & length(value)==1)
    {
      warning("Ethnicity vector length must match ID vector length, recycling ethnicity")
      x@ethnicity <- rep(value, length(x@id))
      return(x)
    }
    else
    {
      return(x)
    }
  }
)


#' Title
#' @param object r
#' @param date r
#' @param ... r
#' @return T
#' @export
setGeneric("age", function(object, date, ...) {standardGeneric("age")})

#' Title
#' @param object .
#' @param date .
#' @param units .
#' @return S4 object
#' @importFrom lubridate interval time_length
#' @export
setMethod("age", signature(object = "Patient", date = "Date"),
          function(object, date, units="years"){
            interval = lubridate::interval(object@dob, date)
            age      = lubridate::time_length(interval, unit=units)
            return(age)
          }
)


