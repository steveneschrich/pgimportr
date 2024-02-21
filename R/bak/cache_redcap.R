
REDCapRCache <- R6::R6Class(
  "REDCapRCache",
  #inherit = Cache,
  public = list(
    initialize = function(uri, token) {
      #super.initialize(makename(uri, token))
    },
    hash = function(uri, token) {
      paste0(uri, "_", token)
    },
    is_cached = function(uri, token) {
      super$is_cached(self$hash(uri, token))
    }
  )
)
