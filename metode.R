# Pronalaženje datoteka koje se nalaze u istom direktoriju.
sourceDirectory <- function(path, recursive = FALSE, local = TRUE) {
  if (!dir.exists(path)) {
    warning(paste(path, "direktorij nije valjan!"))
    return(NULL)
  }

  # Funkcija (local)
  if (is.logical(local) && local) { env <- parent.frame() }
  # Funkcija unutar Global Environment
  else if (is.logical(local) && !local) { env <- globalenv() }
  else if (is.environment(local)) { env <- local }
  else { stop("'local' must be TRUE, FALSE or an environment") }

  files <- list.files(path = path, pattern = ".*\\.R", all.files = F, full.names = TRUE, recursive = recursive)
  for (fileToSource in files) {
    tryCatch(
      {
      source(fileToSource, local = env)
      cat(fileToSource, "sourced.\n")
    },

      error = function(cond) {
        message("Učitavanje datoteke \"", fileToSource, "\" nije uspjelo.")
        message(cond)
      }

    )
  }
}
