.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    "www",
    system.file(
      "www",
      package = "stateofasylum"
    )
  )
}

.onUnload <- function(libname, pkgname) {
   shiny::removeResourcePath("www")
}