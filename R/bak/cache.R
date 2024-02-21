# cache.R: caching of redcap imports
#

read_redcap <- function(uri, token) {

  cachedb <- Redcap$new()

  if ( cachedb$is_cached(uri, token) && ! cachedb$is_expired(uri, token) ) {
    res <- cachedb$load(localname)
  } else {
    res <- dothework()
    cachedb$store(res, name = localname)
  }
  res
}

# RedcapR_cache:
#   get(filecache, uri, key);filecache.get(hash(uri,key))
# filecache:
#   get(realcache0, key);readrds(resultwhichisfilename)
#.  put(realcache0, key, obj);writerds()
#
# cache:
#  get(cache, key). # Find a row, extract to lsit
#  put(cache, key, obj) # Given a list,
#. has(cache, key)
#  is_expired(cache, key)

# serialize
#.  store(cache)
#.  initialize()
#   load(cache)

## create an environment with a data frame


FileCache <- R6::R6Class(
  "FileCache",
  public = list(
    # Retrieve object based on key, or NA if not in cache.
    load = function(key) {
      ch <- private$cache$get(key)
      cached_file <- NULL
      if (is.na(ch) || utils::hasName(ch, "filename")) {
        return(NULL)
      }
      if ( ! base::file.exists(ch$filename) ) {
        cli::cli_alert_warning("File {ch$filename} was cached, but not found!")
        return(NULL)
      }
      readRDS(ch$filename)
    },
    # Store object indexed by key.
    store = function(key, obj) {

    },
    # Check if object linked to key is cached
    is_cached = function(key) {
      private$cache$is_cached(key)
    },
    # Check if object linked to key has expired (if cached)
    is_expired = function(key, days = 1) {
      ch <- private$cache$get(key)
      if ( !is.na(ch) && hasName(ch, "date_created")) {
        Sys.Date() - ch$date_created > 1
      }
    }
  ),
  private = list(
    cache = list() # Init Cache$new()
  )
)


# A REDCapFileCache is a key/value store where the key
# is a uri/token combination (suitable for REDCapR package)
# and the value is a file storing the contents of the request.

# A FileCache is a key/value store where the value
# refers to a file. Getting the value may mean getting
# file metadata or file contents (depending on call).
# An additional feature is the notion of an expiration date
# for the cache contents.

# A Cache is a key/value store where the value is
# a list. It can be serialized/de-serialized from
# disk.
Cache <- R6::R6Class(
  "Cache",

  public = list(
    initialize = function(...) {
      private$cachetbl <- list()
      if (  base::file.exists(private$cachefile()) ) {
         private$cachetbl <- self$load()
      }
    },
    has = function(key) {
      !is.null(self$get(key))
    },
    #' Get cached entry
    get = function(key) {
      private$cachetbl[[private$hash(key)]]
    },
    #' Put obj into cache by key
    put = function(key, obj, replace = TRUE) {
      if ( self$has(key) ) {
        if ( !replace )
          return(self)
        else
          private$remove(key)
      }
      private$cachetbl[[key]] <- obj

      self
    },
    #' Remove obj/key
    remove = function(key) {
      i <- private$hash(key)
      private$cachetbl <- private$cachetbl[!names(private$cachetbl)==i]
      self
    },
    #' Load cache table
    load = function() {
      private$cachetbl <- readRDS(private$cachefile())
      self
    },
    #' Store the cache table
    store = function() {
      cachedir <- basename(private$cachefile())
      if ( ! base::dir.exists(cachedir) ) {
        dir.create(cachedir, recursive = TRUE)
        cli::cli_alert_info("Created cache directory: {cachedir}.")
      }
      saveRDS(private$cachetbl, file = private$cachefile())
      cli::cli_alert_success("Stored cache file {private$cachefile()}.")
    }
  ),
  private = list(
    # Cache table (list): key indexes to a value (which can be a list).
    cachetbl = list(),
    #' Hash a file/uri to internal key
    hash = function(x) {
      rlang::hash(x)
    },
    #' Match key in cache, returning index
    match = function(key) {
      key <- private$hash(key)
      key
    },
    #' Extract cache results for key
    pluck = function(key) {
      m <- private$match(key)
      private$cachetbl[[m]]
    },
    #' Return serialized cache path
    cachefile = function() {
      file.path(
        rappdirs::user_data_dir(appname = "pgimportr", appauthor = "org.moffitt"),
        "pgimportr_cachedb.rds"
      )
    }
  )
)




