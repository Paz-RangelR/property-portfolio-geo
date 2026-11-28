# Tests for R/auth.R. No dependencies beyond openssl.
#   Rscript tests/test_auth.R        (from the repository root)

source("R/auth.R")

h <- hash_password("correct horse")
usuarios <- cargar_usuarios(paste0(
  "ana@company.example:", h, "; luis@company.example:", hash_password("other"), ";"
))

stopifnot(
  # hashes are salted and never contain the password
  startsWith(h, "pbkdf.16."),
  h != hash_password("correct horse"),
  !grepl("correct horse", h, fixed = TRUE),

  # parsing tolerates spaces and a trailing separator
  setequal(names(usuarios), c("ana@company.example", "luis@company.example")),

  # verification
  verificar_credenciales("ana@company.example", "correct horse", usuarios),
  !verificar_credenciales("ana@company.example", "Correct horse", usuarios),
  !verificar_credenciales("luis@company.example", "correct horse", usuarios),
  !verificar_credenciales("nobody@company.example", "correct horse", usuarios)
)

falla <- function(expr) inherits(try(expr, silent = TRUE), "try-error")
stopifnot(
  falla(cargar_usuarios("")),                               # not configured
  falla(cargar_usuarios("ana@company.example:plaintext")),  # passwords rejected
  falla(cargar_usuarios("no-separator"))                    # malformed
)

cat("auth: all tests passed\n")
