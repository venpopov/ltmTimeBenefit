Sys.setenv(
  RENV_CONFIG_RSPM_ENABLED = FALSE,
  RENV_CONFIG_SANDBOX_ENABLED = FALSE
)

if (requireNamespace("rprofile", quietly = TRUE)) {
  rprofile::load(dev = quote(reload()))
} else {
  source("renv/activate.R")
}

reload <- function() {
  if (interactive()) {
    devtools::document()
    devtools::load_all(quiet = TRUE)
    try(rd2md_docs(md_dir = "docs/reference/"))
  }
}
