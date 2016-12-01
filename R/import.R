#' @importFrom magrittr "%>%"
#' @importFrom pkgmaker "sVariable"
#' @importFrom purrr "map"
#' @importFrom modules "import_"


importsStack <- pkgmaker::sVariable(c("."))

shortPath <- function(path) {
  parts <- strsplit(path, "/") %>% unlist
  if (length(parts) <= 2) {
    return(path)
  }
  for(i in 2:(length(parts) - 1)) {
    a <- parts[i]
    b <- parts[i+1]
    if (a == ".") {
      join <- as.list(c(parts[1:(i-1)], parts[(i+1):length(parts)]))
      join$sep <- "/"
      shorter <- do.call(paste, join)
      return(shortPath(shorter))
    }
    if (a != "." && a != ".." && b == "..") {
      join <- as.list(c(parts[1:(i-1)], parts[(i+2):length(parts)]))
      join$sep <- "/"
      shorter <- do.call(paste, join)
      return(shortPath(shorter))
    }
  }
  return(path)
}

#' import - imports modules aloowing for relavtive path
#'
#' @param relativePath - string with relative path to the module
#'
#' @export
import <- function(relativePath) {
  pathParts <- strsplit(relativePath, "/") %>% unlist
  filename <- pathParts[length(pathParts)]
  dirname <- pathParts[1:length(pathParts) - 1] %>% purrr::map(~ paste0(., "/")) %>% do.call(paste0, .)
  importsStack <- c(importsStack(), dirname)
  path <- importsStack %>% as.list %>% c(filename) %>% do.call(file.path, .)
  path <- gsub("//","/", path)

  simplePath <- shortPath(path)
  result <- suppressPackageStartupMessages(modules::import_(simplePath))
  importsStack <- importsStack()[1:length(importsStack()) - 1]
  result
}
