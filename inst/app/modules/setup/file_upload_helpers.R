getRoots <- function() {
  # Set up roots for Windows, macOS, and Linux systems
  roots <- if (.Platform$OS.type == "windows") {
    drives <- system("wmic logicaldisk get name", intern = TRUE)
    drives <- gsub("\\s+", "", drives)  # Trim whitespace
    drives <- drives[nchar(drives) > 0] # Remove empty strings
    drives <- drives[grepl(":", drives)]
    set_names(drives, drives)
  } else {
    c(home = "~", root = "/") # For macOS and Linux
  }

  return(roots)
}
