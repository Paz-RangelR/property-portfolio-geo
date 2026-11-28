# ==========================================================================
# Authentication shared by the Shiny apps
# ==========================================================================
#
# No credential lives in this repository. The apps read their users from the
# APP_USERS environment variable, and what it holds is never a password:
# each entry is a salted bcrypt-pbkdf hash, so a leaked .env or a log line
# does not give anyone a working login.
#
#   APP_USERS = "user@company.example:pbkdf.16.<salt>.<hash>;other@company.example:pbkdf.16.<salt>.<hash>"
#
# Generate an entry once, from an R console, and paste it into .env:
#
#   source("R/auth.R"); hash_password("the-password")
#
# The format records its own cost (rounds), so it can be raised later without
# invalidating the hashes already issued. `.` and `;` never occur in base64
# and `:` never occurs in an email address, so the separators are unambiguous.

hash_password <- function(password, rounds = 16L) {
  stopifnot(is.character(password), length(password) == 1L, nzchar(password))
  salt <- openssl::rand_bytes(16L)
  hash <- openssl::bcrypt_pbkdf(password, salt, rounds = rounds, size = 32L)
  paste("pbkdf", rounds,
        openssl::base64_encode(salt), openssl::base64_encode(hash), sep = ".")
}

# Parses APP_USERS into a named list: user -> stored hash.
# Fails at startup, not at first login, if the variable is missing or malformed.
cargar_usuarios <- function(raw = Sys.getenv("APP_USERS")) {
  if (!nzchar(raw)) {
    stop("APP_USERS is not set. See .env.example.", call. = FALSE)
  }
  entradas <- trimws(strsplit(raw, ";", fixed = TRUE)[[1]])
  entradas <- entradas[nzchar(entradas)]
  sep <- regexpr(":", entradas, fixed = TRUE)
  if (any(sep < 2L)) {
    stop("APP_USERS: every entry must be user:hash.", call. = FALSE)
  }
  usuarios <- substr(entradas, 1L, sep - 1L)
  hashes   <- substr(entradas, sep + 1L, nchar(entradas))
  if (!all(startsWith(hashes, "pbkdf."))) {
    stop("APP_USERS: hashes must come from hash_password(); ",
         "plain-text passwords are not accepted.", call. = FALSE)
  }
  setNames(as.list(hashes), usuarios)
}

# TRUE only if `usuario` exists and `password` matches its stored hash.
# An unknown user still pays for one hash computation, so response time does
# not reveal which accounts exist; the final comparison is constant-time.
verificar_credenciales <- function(usuario, password, usuarios) {
  almacenado <- usuarios[[usuario]]
  existe <- !is.null(almacenado)
  if (!existe) almacenado <- .hash_decoy

  partes <- strsplit(almacenado, ".", fixed = TRUE)[[1]]
  if (length(partes) != 4L) return(FALSE)
  rounds   <- as.integer(partes[2])
  salt     <- openssl::base64_decode(partes[3])
  esperado <- openssl::base64_decode(partes[4])

  calculado <- openssl::bcrypt_pbkdf(password, salt, rounds = rounds,
                                     size = length(esperado))
  iguales <- sum(bitwXor(as.integer(calculado), as.integer(esperado))) == 0L
  existe && iguales
}

.hash_decoy <- hash_password(rawToChar(as.raw(sample(65:90, 24, TRUE))))
