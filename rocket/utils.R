extract.filename <- function(file) {
  filename <- unlist(str_split(file, '/'))
  filename <- filename[length(filename)]
}

get.data.dump.filepath <- function(filename) {
  data_dump_dir <- './data_dump/'
  paste0(c(data_dump_dir, filename), collapse="")
}
