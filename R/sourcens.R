pacman::p_load(namespace, magrittr, data.table)

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
    #print(i)
    tryCatch(eval(ll[[i]], envir = ns),
             error = function(e) message("Oops!  ", as.character(e)))

  }

  base::namespaceExport(ns, ls(ns))

  invisible()
}

#' navigation Addin:

searchSourceNSFuncAddin <- function() {
  pacman::p_load(data.table, magrittr, dplyr)

  row <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[1] %>% unname
  col <- rstudioapi::getActiveDocumentContext()$selection[[1]]$range$start[2] %>% unname

  ss <- rstudioapi::getActiveDocumentContext()$contents[row]

  ss1 <- substr(ss, 1, col)
  ss2 <- substr(ss, col+1, nchar(ss))
  sym <- paste0(strsplit(ss1, '[^A-Za-z:_]') %>% unlist %>% tail(1),
                strsplit(ss2, '[^A-Za-z:_]') %>% unlist %>% head(1))
  sym
  # strToSearch <- deparse(eval(parse(text = sym))) %>% gsub('[[:space:]]', '', .) %>% paste(collapse = '[0-9]')
  # strToSearch

  strToSearchSimple <- paste0('[\n][0-9]+',
                              strsplit(sym, '::') %>% unlist %>% .[2],
                              '[\n0-9]*<-[\n0-9]*function[\n0-9]*[(]')
  strToSearchSimple

  searchFile <- paste0(strsplit(sym, '::') %>% unlist %>% .[1], '.R')
  searchFile

  if (!file.exists(searchFile)) {
    return(invisible(0))
  }

  fileLines <- data.frame(Line = readLines(searchFile, warn=F)) %>% as.data.table
  fileContents <-
    # fileLines[, Line := paste0(.I, Line) %>% gsub('[[:space:]]', '', .)]$Line %>%
    fileLines %>% mutate(Line = paste0(row_number(), Line) %>% gsub('[[:space:]]', '', .)) %>%
    .$Line %>%
    paste(collapse = '\n')
  fileContents

  pacman::p_load(stringr)

  functLine <- str_extract(fileContents, strToSearchSimple)
  rowNew <- functLine %>% str_extract('[0-9]+') %>% as.numeric()
  #

  rstudioapi::navigateToFile(searchFile, rowNew, 0)
}
