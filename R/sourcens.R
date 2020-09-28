pacman::p_load(namespace)

#' @title Load a file into a corresponding namespace.
#' @description Load a file into a corresponding namespace.
#' Errors are shown but execution is continued.
#' usage: sourceNs("filename.R") - sources file into namespace  'filename::'
#' @param path the path of the file to be parsed
#' @export
sourcens <- function(path) {


  nsName <- gsub('.R$', '', path)
  ns0 <- namespace::getRegisteredNamespace(nsName)
  base::unloadNamespace(ns0)

  ns <- namespace::makeNamespace(nsName)

  ll <- parse(file = path)

  for (i in seq_along(ll)) {
    print(i)
    tryCatch(eval(ll[[i]], envir = ns),
             error = function(e) message("Oops!  ", as.character(e)))

  }


  base::namespaceExport(ns, ls(ns))
}
