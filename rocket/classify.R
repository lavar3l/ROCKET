library(reticulate)
use_python("../python/venv/Scripts/python.exe")

builtins <- import_builtins()
main <- import_main()

python_pickle.dump <- function(object, filename) {
  pickler <- import("pickle")
  file <- builtins$open(filename, "wb")
  pickler$dump(object, file)
  file$close()
}
