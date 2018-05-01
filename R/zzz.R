
.onLoad <- function(libname, pkgname) {
  # Runs when loaded but not attached to search() path; e.g., when a package
  # just Imports (not Depends on) sl3
  # Set options for the speed boost in v1.8.0 by avoiding 'default' arg of
  # getOption(,default=)
  opts <- list(
    "longbowtools.cluster.username" = NULL,
    "longbowtools.cluster.password" = NULL,
    "longbowtools.cluster.ip" = NULL,
    "longbowtools.longbow.token" = NULL,
    "longbowtools.longbow.base.url" = "https://www.longbowapp.com")
  # for (i in setdiff(names(opts),names(options()))) {
  #   browser()
  #   eval(parse(text=paste("options(",i,"=",as.character(opts[i]),")",sep="")))
  # }
  toset <- !(names(opts) %in% names(options()))
  if (any(toset)) options(opts[toset])
  invisible()
}