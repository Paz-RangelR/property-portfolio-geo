library(shiny)
library(shinyjs)
library(leaflet)
library(sf)
library(DBI)
library(RJDBC)
library(rJava)
library(DT)
library(dplyr)
library(openxlsx)
library(jose)    
library(openssl) 

# Users come from APP_USERS (salted hashes, never passwords); see R/auth.R.
source(file.path(if (dir.exists("R")) "R" else ".", "auth.R"))
usuarios_validos <- cargar_usuarios()

tryCatch({
  source("Excel.R")
}, error = function(e) {
  cat("❌ Error al cargar Excel.R:", e$message, "\n")
})

# ============================================================================
# CACHE GLOBAL: DRIVER JDBC Y LLAVE PRIVADA (se inicializan una sola vez)
# ============================================================================
cat("Inicializando driver JDBC y llave privada...\n")

# Driver JDBC - se instancia una sola vez
SNOWFLAKE_JDBC_DRIVER <- tryCatch({
  ruta_jar <- file.path(getwd(), "snowflake-jdbc-3.25.1.jar")
  RJDBC::JDBC("net.snowflake.client.jdbc.SnowflakeDriver", 
               classPath = ruta_jar, 
               identifier.quote = "`")
}, error = function(e) {
  cat("Error al inicializar driver JDBC:", e$message, "\n")
  NULL
})

# Llave privada Java - se parsea una sola vez
SNOWFLAKE_PRIVATE_KEY <- tryCatch({
  ruta_pem <- file.path(getwd(), "SVC_ANALYTICS_key_pk8.pem")
  lineas <- readLines(ruta_pem, warn = FALSE)
  lineas <- lineas[!grepl("^-----", lineas)]
  clave_base64 <- paste(lineas, collapse = "")
  decoder <- .jcall("java/util/Base64", "Ljava/util/Base64$Decoder;", "getDecoder")
  bytes_clave <- .jcall(decoder, "[B", "decode", clave_base64)
  key_spec <- .jnew("java/security/spec/PKCS8EncodedKeySpec", bytes_clave)
  key_factory <- .jcall("java/security/KeyFactory", "Ljava/security/KeyFactory;", 
                        "getInstance", "RSA")
  .jcall(key_factory, "Ljava/security/PrivateKey;", "generatePrivate", 
         .jcast(key_spec, "java/security/spec/KeySpec"))
}, error = function(e) {
  cat("Error al parsear llave privada:", e$message, "\n")
  NULL
})

# Llave privada R (para JWT REST API) - se lee una sola vez
SNOWFLAKE_R_KEY <- tryCatch({
  openssl::read_key(file.path(getwd(), "SVC_ANALYTICS_key_pk8.pem"))
}, error = function(e) {
  cat("Error al leer llave R:", e$message, "\n")
  NULL
})

cat("Driver y llaves inicializados.\n")

# ============================================================================
# FUNCIÓN GLOBAL: Conexión a Snowflake (usa driver y llave cacheados)
# Usada por funciones de scope global como cargar_cache_inpc
# ============================================================================
crear_conexion_snowflake_global <- function() {
  tryCatch({
    driver <- SNOWFLAKE_JDBC_DRIVER
    if (is.null(driver)) stop("Driver JDBC no inicializado")
    
    private_key <- SNOWFLAKE_PRIVATE_KEY
    if (is.null(private_key)) stop("Llave privada no inicializada")
    
    url_conexion <- paste0("jdbc:snowflake://", Sys.getenv("SNOWFLAKE_ACCOUNT"), ".snowflakecomputing.com/")
    
    propiedades <- .jnew("java.util.Properties")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "user", "SVC_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "warehouse", "WH_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "db", "DB_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "schema", "SCH_CORE")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "role", "ROLE_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "authenticator", "SNOWFLAKE_JWT")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "tracing", "OFF")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "JDBC_QUERY_RESULT_FORMAT", "JSON")
    
    .jcall(propiedades, "Ljava/lang/Object;", "put", 
           .jcast(.jnew("java/lang/String", "privateKey"), "java/lang/Object"),
           .jcast(private_key, "java/lang/Object"))
    
    jconn <- .jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url_conexion, propiedades)
    
    if (is.jnull(jconn)) stop("La conexión retornó null")
    
    conexion <- new("JDBCConnection", jc = jconn, identifier.quote = driver@identifier.quote)
    
    if (!DBI::dbIsValid(conexion)) stop("La conexión no es válida")
    
    cat("✅ Conexión global exitosa a Snowflake (JWT)\n")
    return(conexion)
    
  }, error = function(e) {
    stop(paste("❌ Error conexión global:", e$message))
  })
}

# ============================================================================
# FUNCIONES ORIGINALES DE ACTUALIZACION
# ============================================================================
cargar_datos <- function() {
  if (file.exists("inmuebles_data.rds")) {  
    tryCatch({
      datos_existentes <- readRDS("inmuebles_data.rds")
      
      if (!is.data.frame(datos_existentes)) {
        warning("El archivo RDS no contiene un data frame válido. Creando estructura nueva.")
        return(crear_estructura_vacia())
      }
      
      # Agregar columna EDITADO 
      if(!"EDITADO" %in% names(datos_existentes)) {
        datos_existentes$EDITADO <- rep(FALSE, nrow(datos_existentes))
      }
      
      if(!"FECHA_MODIFICACION" %in% names(datos_existentes)) {
        datos_existentes$FECHA_MODIFICACION <- rep(as.POSIXct(NA), nrow(datos_existentes))
      }
      
      # Validar que las columnas requeridas existan
      columnas_requeridas <- c("ID", "INMUEBLE", "EMPRESA", "ALIAS", "DIRECCION", 
                               "MUNICIPIO", "ESTADO", "PAIS", "FECHA_ESCRITURA", 
                               "PRECIO_ADQUISICION", "TERRENO_M2", "PRECIO_TERRENO_M2", 
                               "CONSTRUCCION_M2", "PRECIO_CONSTRUCCION_M2", "EDITADO",
                               "FECHA_MODIFICACION")  
      
      columnas_faltantes <- setdiff(columnas_requeridas, names(datos_existentes))
      
      if(length(columnas_faltantes) > 0) {
        warning(paste("Columnas faltantes detectadas:", paste(columnas_faltantes, collapse = ", ")))
        for(col in columnas_faltantes) {
          if(col == "FECHA_ESCRITURA") {
            datos_existentes[[col]] <- rep(as.Date(NA), nrow(datos_existentes))
          } else if(col == "EDITADO") {
            datos_existentes[[col]] <- rep(FALSE, nrow(datos_existentes))
          } else if(col == "FECHA_MODIFICACION") {  
            datos_existentes[[col]] <- rep(as.POSIXct(NA), nrow(datos_existentes))
          } else if(col %in% c("PRECIO_ADQUISICION", "TERRENO_M2", "PRECIO_TERRENO_M2", 
                               "CONSTRUCCION_M2", "PRECIO_CONSTRUCCION_M2")) {
            datos_existentes[[col]] <- rep(0, nrow(datos_existentes))
          } else {
            datos_existentes[[col]] <- rep("", nrow(datos_existentes))
          }
        }
      }
      
      return(datos_existentes)
      
    }, error = function(e) {
      warning(paste("Error al cargar inmuebles_data.rds:", e$message, ". Creando estructura nueva."))
      return(crear_estructura_vacia())
    })
  } else {
    return(crear_estructura_vacia())
  }
}

es_numerico_decimal <- function(texto) {
  if (is.null(texto) || texto == "") return(TRUE)
  return(grepl("^[0-9]+(\\.[0-9]+)?$", texto))
}

validar_fecha <- function(fecha_texto) {
  if (is.null(fecha_texto) || fecha_texto == "") return(TRUE)
  tryCatch({
    as.Date(fecha_texto)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Función auxiliar para crear estructura vacía
crear_estructura_vacia <- function() {
  data.frame(
    ID = character(0),
    INMUEBLE = character(0),
    EMPRESA = character(0),
    ALIAS = character(0),
    DIRECCION = character(0),
    MUNICIPIO = character(0),
    ESTADO = character(0),
    PAIS = character(0),
    FECHA_ESCRITURA = as.Date(character(0)),
    PRECIO_ADQUISICION = numeric(0),
    TERRENO_M2 = numeric(0),
    PRECIO_TERRENO_M2 = numeric(0),
    CONSTRUCCION_M2 = numeric(0),
    PRECIO_CONSTRUCCION_M2 = numeric(0),
    EDITADO = logical(0),
    FECHA_MODIFICACION = as.POSIXct(character(0)),
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# FUNCIÓN: Convertir PEM a PrivateKey para JWT (AUTENTICACIÓN SNOWFLAKE)
# ============================================================================
convertir_pem_a_privatekey <- function(ruta_pem) {
  tryCatch({
    # Leer el contenido del archivo .pem
    lineas <- readLines(ruta_pem, warn = FALSE)
    
    # Remover headers y footers del PEM (BEGIN/END PRIVATE KEY)
    lineas <- lineas[!grepl("^-----", lineas)]
    
    # Unir todas las líneas sin saltos de línea
    clave_base64 <- paste(lineas, collapse = "")
    
    # Decodificar de Base64 a bytes
    decoder <- .jcall("java/util/Base64", "Ljava/util/Base64$Decoder;", "getDecoder")
    bytes_clave <- .jcall(decoder, "[B", "decode", clave_base64)
    
    # Crear PKCS8EncodedKeySpec
    key_spec <- .jnew("java/security/spec/PKCS8EncodedKeySpec", bytes_clave)
    
    # Obtener KeyFactory para RSA
    key_factory <- .jcall("java/security/KeyFactory", "Ljava/security/KeyFactory;", 
                          "getInstance", "RSA")
    
    # Generar la PrivateKey
    private_key <- .jcall(key_factory, "Ljava/security/PrivateKey;", 
                          "generatePrivate", .jcast(key_spec, "java/security/spec/KeySpec"))
    
    return(private_key)
  }, error = function(e) {
    stop(paste("Error al convertir la clave privada:", e$message))
  })
}

# Función para validar que un texto sea solo numérico
es_numerico <- function(texto) {
  if (is.null(texto) || texto == "") return(TRUE)
  return(grepl("^[0-9]+$", texto))
}

# Función para validar longitud del identificador
validar_longitud_identificador <- function(identificador) {
  if (is.null(identificador) || identificador == "") return(TRUE)
  return(nchar(identificador) <= 18)
}

# ============================================================================
# FUNCIONES: INTEGRACIÓN CORTEX AGENT - MAESTRO INMUEBLES
# ============================================================================

SNOWFLAKE_HOST    <- paste0(Sys.getenv("SNOWFLAKE_ACCOUNT"), ".snowflakecomputing.com")
SNOWFLAKE_ACCOUNT <- toupper(Sys.getenv("SNOWFLAKE_ACCOUNT"))   # formato ORG-ACCOUNT requerido por JWT
SNOWFLAKE_USER    <- "SVC_ANALYTICS"
SNOWFLAKE_PEM     <- Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")
CORTEX_AGENT_NAME <- "DB_ANALYTICS.SCH_CORE.MAESTRO_INMUEBLES"

# ----------------------------------------------------------------------------
# Genera el token JWT para la REST API (distinto al JDBC — este es estándar)
# ----------------------------------------------------------------------------
generar_jwt_rest <- function() {
  tryCatch({
    # Usar llave privada Java cacheada globalmente
    private_key <- SNOWFLAKE_PRIVATE_KEY
    if (is.null(private_key)) {
      private_key <- convertir_pem_a_privatekey(file.path(getwd(), SNOWFLAKE_PEM))
    }
    
    # ── 2. Extraer clave pública desde la privada (RSAPrivateCrtKey → pública)
    rsa_priv <- .jcast(private_key, "java/security/interfaces/RSAPrivateCrtKey")
    modulus  <- .jcall(rsa_priv, "Ljava/math/BigInteger;", "getModulus")
    pub_exp  <- .jcall(rsa_priv, "Ljava/math/BigInteger;", "getPublicExponent")
    
    pub_spec <- .jnew("java/security/spec/RSAPublicKeySpec", modulus, pub_exp)
    kf       <- .jcall("java/security/KeyFactory",
                       "Ljava/security/KeyFactory;", "getInstance", "RSA")
    pub_key  <- .jcall(kf, "Ljava/security/PublicKey;", "generatePublic",
                       .jcast(pub_spec, "java/security/spec/KeySpec"))
    
    # ── 3. DER de la clave pública (SubjectPublicKeyInfo — lo que pide Snowflake)
    pub_der_bytes <- .jcall(pub_key, "[B", "getEncoded")
    
    # ── 4. SHA256 en Java
    md <- .jcall("java/security/MessageDigest",
                 "Ljava/security/MessageDigest;", "getInstance", "SHA-256")
    .jcall(md, "V", "update", pub_der_bytes)
    sha256_bytes <- .jcall(md, "[B", "digest")
    
    # ── 5. Base64 en Java → fingerprint listo
    encoder     <- .jcall("java/util/Base64", "Ljava/util/Base64$Encoder;", "getEncoder")
    fingerprint <- .jcall(encoder, "Ljava/lang/String;", "encodeToString", sha256_bytes)
    
    # ── 6. Firmar el JWT con openssl (RS256) — usar llave R cacheada
    llave_r <- SNOWFLAKE_R_KEY
    if (is.null(llave_r)) {
      llave_r <- openssl::read_key(file.path(getwd(), SNOWFLAKE_PEM))
    }
    ahora   <- as.integer(Sys.time())
    
    # Construir JWT manualmente (jose::jwt_claim rechaza el ':' en iss)
    header  <- jsonlite::toJSON(list(alg = "RS256", typ = "JWT"), auto_unbox = TRUE)
    payload <- jsonlite::toJSON(list(
      iss = paste0(SNOWFLAKE_ACCOUNT, ".", SNOWFLAKE_USER, ".SHA256:", fingerprint),
      sub = paste0(SNOWFLAKE_ACCOUNT, ".", SNOWFLAKE_USER),
      iat = ahora,
      exp = ahora + 3600L
    ), auto_unbox = TRUE)
    
    base64url_encode <- function(x) {
      b64 <- openssl::base64_encode(x)
      b64 <- gsub("=+$", "", b64)
      b64 <- gsub("\\+", "-", b64)
      b64 <- gsub("/", "_", b64)
      b64
    }
    
    header_b64  <- base64url_encode(charToRaw(as.character(header)))
    payload_b64 <- base64url_encode(charToRaw(as.character(payload)))
    
    signing_input <- paste0(header_b64, ".", payload_b64)
    signature <- openssl::signature_create(charToRaw(signing_input), hash = sha256, key = llave_r)
    sig_b64   <- base64url_encode(signature)
    
    paste0(signing_input, ".", sig_b64)
    
  }, error = function(e) {
    stop(paste("Error JWT:", e$message))
  })
}

# ----------------------------------------------------------------------------
# Parsea la respuesta SSE del Cortex Agent y extrae el texto final
# ----------------------------------------------------------------------------
parsear_sse_cortex <- function(texto_sse) {
  lineas       <- strsplit(texto_sse, "\n")[[1]]
  texto_final  <- ""
  
  # Imprimir raw para debug (quitar después)
  cat("── SSE RAW (primeros 2000 chars) ──\n")
  cat(substr(texto_sse, 1, 2000), "\n")
  cat("── FIN SSE RAW ──\n")
  
  for (linea in lineas) {
    linea <- trimws(linea)
    if (nchar(linea) == 0) next
    if (grepl("\\[DONE\\]", linea)) next
    
    # Remover prefijo "data:" o "data: "
    if (!startsWith(linea, "data:") && !startsWith(linea, "data: ")) next
    json_str <- trimws(sub("^data:\\s*", "", linea))
    if (nchar(json_str) == 0) next
    
    tryCatch({
      ev <- jsonlite::fromJSON(json_str, simplifyVector = FALSE)
      
      # Formato nuevo: response.text.delta → {"content_index":0, "text":"..."}
      if (!is.null(ev$text) && is.null(ev$role)) {
        texto_final <- paste0(texto_final, ev$text)
      }
      
      # Formato nuevo: evento "response" final → {"role":"assistant","content":[{"type":"text","text":"..."}]}
      if (!is.null(ev$role) && identical(ev$role, "assistant") && !is.null(ev$content)) {
        for (bloque in ev$content) {
          if (identical(bloque$type, "text") && !is.null(bloque$text))
            texto_final <- paste0(texto_final, bloque$text)
        }
      }
      
      # Formato legacy A: delta.content[].text
      if (!is.null(ev$delta$content)) {
        for (bloque in ev$delta$content) {
          if (identical(bloque$type, "text") && !is.null(bloque$text))
            texto_final <- paste0(texto_final, bloque$text)
        }
      }
      # Formato legacy B: delta.text
      if (!is.null(ev$delta$text))
        texto_final <- paste0(texto_final, ev$delta$text)
      
      # Formato legacy C: choices[].delta.content
      if (!is.null(ev$choices)) {
        for (ch in ev$choices) {
          if (!is.null(ch$delta$content))
            texto_final <- paste0(texto_final, ch$delta$content)
        }
      }
    }, error = function(e) NULL)
  }
  
  if (nchar(trimws(texto_final)) == 0)
    texto_final <- "⚠️ El agente respondió pero no se pudo extraer el texto. Revisa la consola para ver el SSE raw."
  
  texto_final
}

# ----------------------------------------------------------------------------
# Llama al Cortex Agent y devuelve lista(exito, texto, status)
# ----------------------------------------------------------------------------
llamar_cortex_agent <- function(pregunta, historial_api = list(), jwt) {
  # Construir mensaje con formato correcto: content es array de objetos
  msg_usuario <- list(
    role    = "user",
    content = list(list(type = "text", text = pregunta))
  )
  mensajes <- c(historial_api, list(msg_usuario))
  
  body_json <- jsonlite::toJSON(
    list(messages = mensajes),
    auto_unbox = TRUE
  )
  
  # Endpoint con agent object: /api/v2/databases/{db}/schemas/{schema}/agents/{name}:run
  agent_url <- paste0(
    "https://", SNOWFLAKE_HOST,
    "/api/v2/databases/DB_ANALYTICS/schemas/SCH_CORE/agents/MAESTRO_INMUEBLES:run"
  )
  
  resp <- tryCatch(
    httr::POST(
      url  = agent_url,
      httr::add_headers(
        `Authorization`                          = paste("Bearer", jwt),
        `Content-Type`                           = "application/json",
        `Accept`                                 = "text/event-stream",
        `X-Snowflake-Authorization-Token-Type`   = "KEYPAIR_JWT",
        `X-Snowflake-Warehouse`                  = "WH_ANALYTICS"
      ),
      body    = body_json,
      encode  = "raw",
      httr::timeout(90)
    ),
    error = function(e) NULL
  )
  
  if (is.null(resp))
    return(list(exito = FALSE, texto = "❌ No se pudo conectar con Snowflake.", status = 0))
  
  status <- httr::status_code(resp)
  raw    <- httr::content(resp, "text", encoding = "UTF-8")
  
  if (status != 200) {
    cat("⚠️ Cortex Agent error", status, ":\n", raw, "\n")
    return(list(exito = FALSE,
                texto  = paste0("❌ Error ", status, " al llamar al agente. Revisa la consola."),
                status = status))
  }
  
  list(exito = TRUE, texto = parsear_sse_cortex(raw), status = status)
}

# ============================================================================
# UI INTEGRADA 
# ============================================================================
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$script(HTML("
      // Handler para actualizar progreso del Excel
      Shiny.addCustomMessageHandler('updateProgressExcel', function(data) {
        var progressBar = document.getElementById('progress_bar_excel');
        var progressMsg = document.getElementById('progress_message_excel');
        
        if (progressBar && progressMsg) {
          progressBar.style.width = data.progress + '%';
          progressBar.textContent = data.progress + '%';
          progressMsg.textContent = data.message;
        }
      });
    ")),
    
    tags$style(HTML("
      /* ============================================
         PANTALLA DE LOGIN
         ============================================ */
      .login-container {
        display: flex;
        justify-content: center;
        align-items: center;
        min-height: 100vh;
        background: linear-gradient(135deg, #FFF4E6 0%, #FFE9CC 100%);
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: 9999;
      }
      
      .login-box {
        background: white;
        padding: 40px;
        border-radius: 15px;
        box-shadow: 0 10px 40px rgba(230, 126, 34, 0.3);
        border: 3px solid #ff5b49;
        width: 100%;
        max-width: 400px;
      }
      
      .login-header {
        text-align: center;
        margin-bottom: 30px;
      }
      
      .login-title-main {
        color: #ff5b49;
        font-size: 28px;
        font-weight: bold;
        margin: 0 0 10px 0;
      }
      
      .login-subtitle-main {
        color: #6B3A00;
        font-size: 14px;
        margin: 0;
      }
      
      .login-input-group {
        margin-bottom: 20px;
      }
      
      .login-label-main {
        color: #4A2E05;
        font-weight: 600;
        margin-bottom: 8px;
        display: block;
        font-size: 14px;
      }
      
      .login-btn-main {
        width: 100%;
        background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%) !important;
        color: white !important;
        border: none !important;
        padding: 14px;
        border-radius: 8px;
        font-weight: bold;
        font-size: 16px;
        cursor: pointer;
        transition: all 0.3s ease;
        box-shadow: 0 4px 12px rgba(230, 126, 34, 0.3);
      }
      
      .login-btn-main:hover {
        background: linear-gradient(135deg, #ff5b49 0%, #C0510D 100%) !important;
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(230, 126, 34, 0.4);
      }
      
      .login-error-main {
        background: linear-gradient(135deg, #FFEEE0 0%, #FFDDB3 100%);
        border: 1px solid #ff5b49;
        color: #6B3A00;
        padding: 12px;
        border-radius: 8px;
        margin-top: 15px;
        text-align: center;
        font-size: 13px;
        font-weight: 500;
      }
      
      .app-content {
        display: none;
      }
      
      .app-content.authenticated {
        display: block;
      }
        
        /* Espacio entre menú superior y contenido */
        .tab-content {
          padding-top: 25px !important;
        }
        
        /* ==============================================
           ESTILOS PARA VISIBILIDAD
           ============================================== */
        
        body, html {
          height: 100% !important;
          margin: 0;
          padding: 0;
          overflow-x: hidden;
          background: #ffffff !important;
        }
        
        /* ==============================================
           ESTILOS PARA EL SISTEMA DE LOGIN
           ============================================== */
        
        @keyframes slideInUp {
          from {
            opacity: 0;
            transform: translateY(50px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        .login-title {
         color: #333;
         font-size: 2.5rem;
         font-weight: bold;
         margin-bottom: 15px;
         background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
        }
        
        .login-subtitle {
          color: #666;
          font-size: 1.1rem;
          margin-bottom: 40px;
        }
        
        .login-form {
          text-align: left;
        }
        
        .login-form .form-group {
          margin-bottom: 25px;
          position: relative;
        }
        
        .login-form label {
          font-weight: bold;
          color: #333;
          margin-bottom: 8px;
          display: block;
        }
        
        .login-form .form-control {
          border: 2px solid #e1e5e9;
          border-radius: 10px;
          padding: 15px 20px;
          font-size: 1rem;
          transition: all 0.3s ease;
          background: #f8f9fa;
        }
        
        .login-form .form-control:focus {
          border-color: #ff5b49;
          box-shadow: 0 0 15px rgba(220, 53, 69, 0.3);
          background: white;
          outline: none;
        }
        
        .btn-login:hover {
         background: linear-gradient(135deg, #ff5b49 0%,#C0510D 100%);
         transform: translateY(-2px);
         box-shadow: 0 8px 25px rgba(220, 53, 69, 0.4);
            color: white !important;
        } 
        
        .btn-logout:hover {
          background: linear-gradient(135deg, #ff5b49 0%,#C0510D 100%);
          transform: translateY(-1px);
          box-shadow: 0 4px 15px rgba(220, 53, 69, 0.4);
          color: white;
        }
        
        .alert-login {
          margin-top: 20px;
          padding: 15px;
          border-radius: 8px;
          font-weight: 500;
        }
        
        .alert-success {
          background: linear-gradient(135deg, #FFF4E6 0%, #FFE9CC 100%);
          border: 1px solid #802e25;
          color:#4A2E05;
        }
        
        .alert-danger {
          background: linear-gradient(135deg, #FFEEE0 0%, #FFDDB3 100%);
          border: 1px solid #ff5b49;
          color: #6B3A00;
        }
        
        .alert-warning {
          background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);
          border: 1px solid #ffc107;
          color: #856404;
        }
        
        .user-info {
          background: linear-gradient(135deg, #e8f5e8 0%, #FFF4E6 100%);
          padding: 8px 15px;
          border-radius: 6px;
          border-left: 4px solid #802e25;
          margin-right: 15px;
          font-weight: bold;
          color:#4A2E05;
          display: inline-block;
        }
        
        /* ==============================================
           ESTILOS PARA SELECTINPUT EN PANEL DE FILTROS
           ============================================== */
        
        #panel_filtros .control-label {
          color: white !important;
          font-weight : bold;
        }
        
        #filtro_empresa, #filtro_estado, #filtro_municipio, #filtro_inmueble {
          background-color: white !important;
          border: 2px solid rgba(255,255,255,0.5) !important;
          border-radius: 5px !important;
        }
        
        /* Scroll personalizado para la tabla */
        .dataTables_scrollBody::-webkit-scrollbar {
          width: 8px;
        }
        
        .dataTables_scrollBody::-webkit-scrollbar-track {
          background: #f1f1f1;
          border-radius: 10px;
        }
        
        .dataTables_scrollBody::-webkit-scrollbar-thumb {
          background: #ff5b49;
          border-radius: 10px;
        }
        
        .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
          background: #ff5b49;
        }
        
        /* ============================================== 
           ESTILOS PARA TAB BAR (NAVBAR) - VERSIÓN MEJORADA
           ============================================== */
        
        /* Navbar principal - Mayor especificidad */
        .navbar.navbar-default,
        nav.navbar.navbar-default {
          background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%) !important;
          border: none !important;
          box-shadow: 0 4px 15px rgba(230, 126, 34, 0.3) !important;
          min-height: 65px !important;
          margin-bottom: 15px !important; 
        }
        
        /* Título de la navbar */
        .navbar-default .navbar-brand,
        .navbar.navbar-default .navbar-brand {
          color: white !important;
          font-weight: bold !important;
          font-size: 24px !important;
          padding: 20px 15px !important;
          text-shadow: 2px 2px 4px rgba(0,0,0,0.2) !important;
        }
        
        .navbar-default .navbar-brand:hover,
        .navbar-default .navbar-brand:focus {
          color: white !important;
        }
        
        /* Tabs - Enlaces del navbar */
        .navbar-default .navbar-nav > li > a,
        .navbar.navbar-default .navbar-nav > li > a {
          color: white !important;
          font-size: 16px !important;
          font-weight: 500 !important;
          padding: 22px 20px !important;
          transition: all 0.3s ease !important;
          border-bottom: 3px solid transparent !important;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.2) !important;
        }
        
        /* Tab hover */
        .navbar-default .navbar-nav > li > a:hover,
        .navbar.navbar-default .navbar-nav > li > a:hover {
          background: rgba(255, 255, 255, 0.15) !important;
          border-bottom: 3px solid white !important;
          color: white !important;
        }
        
        /* Tab activo */
        .navbar-default .navbar-nav > .active > a,
        .navbar-default .navbar-nav > .active > a:hover,
        .navbar-default .navbar-nav > .active > a:focus,
        .navbar.navbar-default .navbar-nav > .active > a {
          background: rgba(255, 255, 255, 0.25) !important;
          color: white !important;
          border-bottom: 3px solid white !important;
          font-weight: bold !important;
        }
        
        /* Asegurar que el navbar-header también tenga fondo */
        .navbar-default .navbar-header {
          background: transparent !important;
        }
        
        /* Toggle button (menú hamburguesa en móvil) */
        .navbar-default .navbar-toggle {
          border-color: white !important;
          background: rgba(255, 255, 255, 0.2) !important;
        }
        
        .navbar-default .navbar-toggle:hover,
        .navbar-default .navbar-toggle:focus {
          background-color: rgba(255, 255, 255, 0.3) !important;
        }
        
        .navbar-default .navbar-toggle .icon-bar {
          background-color: white !important;
        }
        
        /* Forzar visibilidad del navbar-collapse */
        .navbar-collapse {
          background: transparent !important;
        } 
        
        /* ==============================================
           ESTILOS ORIGINALES DE ACTUALIZACION
           ============================================== */
        
        .required-field {
          color: red;
          font-weight: bold;
        }
        
        .input-error {
          border: 2px solid #ff5b49 !important;
          background-color: #fff5f5 !important;
        }
        
        .validation-message {
          color: #ff5b49;
          font-size: 12px;
          margin-top: 5px;
          font-weight: bold;
        }
        
        .import-container {
          background: linear-gradient(135deg, #FFEEE0 0%, #FFDDB3 100%);
          border-radius: 15px;
          padding: 25px;
          margin-bottom: 20px;
          border: 2px solid #ff5b49;
        }
        
        .import-title {
          color: #ff5b49;
          font-size: 1.5rem;
          font-weight: bold;
          margin-bottom: 20px;
          text-align: center;
        }
        
        .file-upload-area {
          border: 3px dashed #ff5b49;
          border-radius: 10px;
          padding: 30px;
          text-align: center;
          background: white;
          transition: all 0.3s ease;
          margin-bottom: 20px;
        }
        
        .file-upload-area:hover {
          border-color: #ff5b49;
          background: #f8f9fa;
        }
        
        .btn-import {
          background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%);
          color: white !important;
          border: none;
          padding: 12px 30px;
          border-radius: 8px;
          font-size: 1.1rem;
          font-weight: bold;
          transition: all 0.3s ease;
          margin: 10px 5px;
        }
        
        .btn-import:hover {
          background: linear-gradient(135deg, #ff5b49 0%,#C0510D 100%) !important;
          transform: translateY(-2px);
          box-shadow: 0 5px 15px rgba(220, 53, 69, 0.4);
          color: white !important;
        }
        
        .preview-table {
          background: white;
          border-radius: 10px;
          padding: 20px;
          margin-top: 20px;
          border: 2px solid #802e25;
          box-shadow: 0 4px 15px rgba(0,0,0,0.1);
        }
        
        .preview-title {
          color: #802e25;
          font-size: 1.3rem;
          font-weight: bold;
          margin-bottom: 15px;
        }
        
        .status-success {
          background: linear-gradient(135deg, #FFF4E6 0%, #FFE9CC 100%);
          border: 2px solid #802e25;
          color:#4A2E05;
          padding: 15px;
          border-radius: 8px;
          margin: 10px 0;
        }
        
        .status-error {
          background: linear-gradient(135deg, #FFEEE0 0%, #FFDDB3 100%);
          border: 2px solid #ff5b49;
          color: #6B3A00;
          padding: 15px;
          border-radius: 8px;
          margin: 10px 0;
        }
        
        .status-warning {
          background: linear-gradient(135deg, #fff3cd 0%, #ffeaa7 100%);
          border: 2px solid #ffc107;
          color: #856404;
          padding: 15px;
          border-radius: 8px;
          margin: 10px 0;
        }
        
        .fade-in {
          animation: fadeIn 0.5s ease-in;
        }
        
        @keyframes fadeIn {
          from { opacity: 0; transform: translateY(20px); }
          to { opacity: 1; transform: translateY(0); }
        }
        
        /* Estilos específicos para pestaña AGENDA Snowflake */
        .agenda-info-box {
          background: linear-gradient(135deg, #ffeaea 0%, #FFEEE0 100%);
          border: 2px solid #ff5b49;
          border-radius: 8px;
          padding: 15px;
          margin-bottom: 15px;
          border-left: 4px solid #ff5b49;
        }
        
        .agenda-info-title {
          color: #ff5b49;
          margin: 0 0 10px 0;
          font-weight: bold;
        }
        
        .agenda-info-text {
          color: #6B3A00;
          margin: 5px 0;
        }
        
        .agenda-status-box {
          background-color: #ffeaea;
          border: 2px solid #ff5b49;
          padding: 15px;
          border-radius: 8px;
          border-left: 4px solid #802e25;
          color: #6B3A00;
        }
        
        /* ==============================================
           ESTILOS PARA SLIDER DE TERRENO M²
           ============================================== */
        
        /* Barra del slider */
        .irs--shiny .irs-bar {
          background: linear-gradient(135deg, #802e25 0%, #ff5b49 100%) !important;
          border: none !important;
        }
        
        /* Línea completa del slider */
        .irs--shiny .irs-line {
          background: rgba(255, 255, 255, 0.3) !important;
          border: 1px solid rgba(255, 255, 255, 0.5) !important;
        }
        
        /* Círculos/handles del slider */
        .irs--shiny .irs-handle {
          background: white !important;
          border: 3px solid #802e25 !important;
          box-shadow: 0 2px 8px rgba(243, 156, 18, 0.4) !important;
        }
        
        .irs--shiny .irs-handle:hover {
          background: #FFF4E6 !important;
          border-color: #ff5b49 !important;
        }
        
        /* Tooltips con valores */
        .irs--shiny .irs-from,
        .irs--shiny .irs-to,
        .irs--shiny .irs-single {
          background: #802e25 !important;
          color: white !important;
          font-weight: bold !important;
          border: none !important;
        }
        
        .irs--shiny .irs-from:before,
        .irs--shiny .irs-to:before,
        .irs--shiny .irs-single:before {
          border-top-color: #802e25 !important;
        }
        
        /* Valores min/max debajo del slider */
        .irs--shiny .irs-min,
        .irs--shiny .irs-max {
          background: rgba(255, 255, 255, 0.2) !important;
          color: white !important;
          font-weight: bold !important;
        }
        
        /* ==============================================
           ESTILOS PARA BARRA DE PROGRESO
           ============================================== */
        
        .shiny-notification {
          background-color: #ffffff;
          opacity: 1;
          position: fixed;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          width: 500px;
          border-radius: 15px;
          box-shadow: 0 10px 40px rgba(0,0,0,0.3);
        }
        
        .shiny-notification .progress {
          height: 35px;
          margin-top: 15px;
          border-radius: 10px;
          background: linear-gradient(135deg, #f8f9fa 0%, #e9ecef 100%);
          overflow: hidden;
        }
        
        .shiny-notification .progress-bar {
          background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%);
          box-shadow: 0 2px 8px rgba(220, 53, 69, 0.5);
          animation: progress-animation 1.5s ease-in-out infinite;
          font-size: 14px;
          font-weight: bold;
          line-height: 30px;
        }
        
        @keyframes progress-animation {
          0% { background-position: 0% 50%; }
          50% { background-position: 100% 50%; }
          100% { background-position: 0% 50%; }
        }
        
        .shiny-notification-message {
          font-size: 16px;
          font-weight: bold;
          color: #ff5b49;
          text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
          margin-bottom: 10px;
        }
        
        .shiny-notification-content-text {
          font-size: 16px;
          font-weight: 600;
          color: #333333;
          margin-top: 10px;
        }
        
        /* ============================================== 
           ESTILOS PARA MENÚ HAMBURGUESA Y PANEL LATERAL
           ============================================== */
        
        /* Botón hamburguesa interno (dentro del panel de filtros) */
        .hamburger-btn-interno:hover {
        background: rgba(255, 255, 255, 0.3) !important;
        transform: scale(1.05);
        box-shadow: 0 4px 12px rgba(255, 255, 255, 0.3);
        }

        .hamburger-btn-interno:active {
        transform: scale(0.95);
        }
        
        /* Botón flotante para mostrar filtros */
        #boton_mostrar_filtros {
        animation: fadeIn 0.4s ease-in-out;
        }
      
        @keyframes fadeIn {
          from {
            opacity: 0;
            transform: scale(0.8);
          }
          to {
            opacity: 1;
            transform: scale(1);
          }
        }
        
        #mostrar_filtros_btn:hover {
          background: linear-gradient(135deg, #ff5b49 0%, #C0510D 100%) !important;
          transform: scale(1.1) rotate(5deg);
          box-shadow: 0 6px 20px rgba(230, 126, 34, 0.6);
        }
        
        #mostrar_filtros_btn:active {
          transform: scale(0.95);
        }
                
        .filtros-panel {
          transition: all 0.4s ease-in-out;
          margin-left: 0;
        }
          
        .filtros-panel.oculto {
          margin-left: -100%;
          opacity: 0;
          visibility: hidden;
          width: 0;  
          overflow: hidden;
        }
      
        .col-sm-0 {
        width: 0 !important;
        padding: 0 !important;
        margin: 0 !important;
        display: none !important;
        }
        
        .contenido-principal {
          transition: all 0.4s ease-in-out;
        }
        
        .contenido-principal.expandido {
          width: 100% !important;
          max-width: 100% !important;
        }
        
        #tabla_catalogo_completo .dataTables_scrollBody::-webkit-scrollbar {
  height: 8px;
}
#tabla_catalogo_completo .dataTables_scrollBody::-webkit-scrollbar-track {
  background: #f1f1f1;
  border-radius: 10px;
}
#tabla_catalogo_completo .dataTables_scrollBody::-webkit-scrollbar-thumb {
  background: #ff5b49;
  border-radius: 10px;
}
#tabla_catalogo_completo .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
  background: #802e25;
}
        
        /* Ajuste para el mapa cuando se expande */
        .leaflet-container {
          transition: all 0.4s ease-in-out;
        }
        
                /* Estilo para módulos deshabilitados y títulos de sección */
        .modulo-inactivo {
          color: #a0a0a0 !important;
          cursor: not-allowed !important;
          opacity: 0.6;
          pointer-events: none; /* Evita que el clic dispare el link */
          text-decoration: none !important;
        }
        
        .seccion-titulo {
          color: #4A2E05;
          font-weight: bold;
          border-bottom: 2px solid #ff5b49;
          margin-top: 20px;
          margin-bottom: 10px;
          padding-bottom: 5px;
        }
        
        /* Asegura que el menú desplegable esté por encima de los controles del mapa */
        .navbar-nav .dropdown-menu {
          z-index: 10001 !important;
        }
        
        /* Opcional: Si el mapa sigue molestando, bajamos el nivel de los controles del mapa */
          .leaflet-top, .leaflet-bottom {
            z-index: 999 !important;
          }
        
/* Overlay de procesando sobre el panel de filtros */
  .filtros-bloqueados {
    pointer-events: none;
    opacity: 0.6;
    transition: opacity 0.2s ease;
  }
  .filtros-bloqueados::after {
    content: 'Aplicando filtro...';
    position: absolute;
    top: 50%;
    left: 50%;
    transform: translate(-50%, -50%);
    background: rgba(0,0,0,0.7);
    color: white;
    padding: 8px 16px;
    border-radius: 8px;
    font-size: 13px;
    z-index: 9999;
  }
        ")),
    
    tags$script(HTML("
  // ============================================================================
  // VARIABLE GLOBAL PARA ESTADO DEL PANEL
  // ============================================================================
  var panelVisible = true;
  
  // ============================================================================
  // EVENTO: SHINY CONECTADO
  // ============================================================================
  $(document).on('shiny:connected', function() {
      // Handler para triggers de descarga
      Shiny.addCustomMessageHandler('triggerDownload', function(id) {
          var element = document.getElementById(id);
          if (element) {
              element.click();
          }
      });
  });
  
  // =================================================================
  // EVENTO: TOGGLE PANEL DE FILTROS (MENÚ HAMBURGUESA)
  // =================================================================
  $(document).on('click', '#toggle_filtros, #mostrar_filtros_btn', function() {
      console.log('Toggle filtros clicked, panelVisible:', panelVisible);
      
      var panel = $('#panel_filtros');
      var contenido = $('#contenido_principal');
      var colFiltros = panel.closest('.col-sm-3, .col-sm-0'); 
      var colContenido = contenido.closest('.col-sm-9, .col-sm-12');
      var botonFlotante = $('#boton_mostrar_filtros');
      
      if (panelVisible) {
          // OCULTAR PANEL
          panel.addClass('oculto');
          contenido.addClass('expandido');
          
          // Ajustar columnas de Bootstrap
          colFiltros.removeClass('col-sm-3').addClass('col-sm-0');
          colContenido.removeClass('col-sm-9').addClass('col-sm-12');
          
          // MOSTRAR botón flotante con animación
          botonFlotante.css('display', 'block').hide().fadeIn(400);
          
          panelVisible = false;
          
          // Forzar redimensionamiento del mapa
          setTimeout(function() {
              window.dispatchEvent(new Event('resize'));
              
              // Invalidar tamaño del mapa de Leaflet
              if (typeof window.leafletMap !== 'undefined') {
                  window.leafletMap.invalidateSize();
              }
          }, 450);
          
      } else {
          // MOSTRAR PANEL
          panel.removeClass('oculto');
          contenido.removeClass('expandido');
          
          // Restaurar columnas de Bootstrap
          colFiltros.removeClass('col-sm-0').addClass('col-sm-3');
          colContenido.removeClass('col-sm-12').addClass('col-sm-9');
          
          // OCULTAR botón flotante con animación
          botonFlotante.fadeOut(400);
          
          panelVisible = true;
          
          // Forzar redimensionamiento del mapa
          setTimeout(function() {
              window.dispatchEvent(new Event('resize'));
              
              // Invalidar tamaño del mapa de Leaflet
              if (typeof window.leafletMap !== 'undefined') {
                  window.leafletMap.invalidateSize();
              }
          }, 450);
        }
       });
     "))
  ),
  
  # ============================================================================
  # PANTALLA DE LOGIN
  # ============================================================================
  div(id = "login_screen", class = "login-container",
      div(class = "login-box",
          div(class = "login-header",
              h1(class = "login-title-main",
                 icon("lock"), " M.I. ALMENA"),
              p(class = "login-subtitle-main",
                "Maestro de Inmuebles")
          ),
          
          div(class = "login-input-group",
              tags$label(class = "login-label-main", "👤 Usuario"),
              textInput("login_usuario",
                        label = NULL,
                        placeholder = "Ingrese su usuario",
                        width = "100%")
          ),
          
          div(class = "login-input-group",
              tags$label(class = "login-label-main", " Contraseña"),
              passwordInput("login_password",
                            label = NULL,
                            placeholder = "Ingrese su contraseña",
                            width = "100%")
          ),
          
          actionButton("btn_login",
                       " Iniciar Sesión",
                       class = "login-btn-main",
                       width = "100%"),
          
          uiOutput("login_error_msg")
      )
  ),
  
  # ============================================================================
  # CONTENIDO PRINCIPAL (SOLO VISIBLE DESPUÉS DE LOGIN)
  # ============================================================================
  div(id = "app_content", class = "app-content",
      
      navbarPage(
        title = div(
          icon("building"),
          "M.I. ALMENA",
          style = "font-weight: bold; font-size: 20px;"
        ),
        id = "main_navbar",
        windowTitle = "Maestro de Inmuebles ALMENA",
        collapsible = TRUE,
        inverse = TRUE,
        
        # PESTAÑA 1: MAESTRO DE INMUEBLES (CON FILTROS LATERALES COMPLETOS)
        tabPanel(
          title = div(icon("database"), "Maestro de Inmuebles"),
          value = "maestro",
          
          # ESTRUCTURA: FILTROS LATERALES + CONTENIDO DERECHO
          fluidRow(
            # ============================================================================
            # COLUMNA IZQUIERDA: PANEL DE FILTROS (CON ID PARA CONTROLAR VISIBILIDAD)
            # ============================================================================
            column(3,
                   div(id = "panel_filtros",
                       class = "filtros-panel",
                       style = "background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%); 
         padding: 25px; 
         border-radius: 0;
         min-height: calc(79vh + 200px);
         box-shadow: 4px 0 15px rgba(230, 126, 34, 0.3);
         margin-left: -15px;
         margin-right: 0;",
                       
                       # TÍTULO DEL PANEL CON BOTÓN HAMBURGUESA
                       div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 10px;",
                           h3(style = "color: white; margin: 0; font-weight: bold;",
                              " FILTROS "),
                           actionButton("toggle_filtros", 
                                        icon("bars"),
                                        class = "hamburger-btn-interno",
                                        title = "Ocultar/Mostrar Filtros",
                                        style = "background: rgba(255, 255, 255, 0.2);
                              color: white;
                              border: 2px solid white;
                              border-radius: 6px;
                              padding: 8px 12px;
                              cursor: pointer;
                              transition: all 0.3s ease;
                              font-size: 18px;")
                       ),
                       
                       hr(style = "border-color: rgba(255,255,255,0.3);"),
                       
                       # FILTRO POR EMPRESA
                       div(style = "margin-bottom: 20px;",
                           tags$label(style = "color: white; font-weight: bold; margin-bottom: 8px; display: block;",
                                      "Empresa:"),
                           selectInput("filtro_empresa",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE)
                       ),
                       
                       # FILTRO POR ESTADO
                       div(style = "margin-bottom: 20px;",
                           tags$label(style = "color: white; font-weight: bold; margin-bottom: 8px; display: block;",
                                      "Estado:"),
                           selectInput("filtro_estado",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE)
                       ),
                       
                       # FILTRO POR MUNICIPIO
                       div(style = "margin-bottom: 20px;",
                           tags$label(style = "color: white; font-weight: bold; margin-bottom: 8px; display: block;",
                                      "Municipio:"),
                           selectInput("filtro_municipio",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE)
                       ),
                       
                       # FILTRO POR INMUEBLE
                       div(style = "margin-bottom: 20px;",
                           tags$label(style = "color: white; font-weight: bold; margin-bottom: 8px; display: block;",
                                      "Inmueble:"),
                           selectInput("filtro_inmueble",
                                       label = NULL,
                                       choices = NULL,
                                       multiple = TRUE)
                       ),
                       
                       # FILTRO POR TERRENO M²
                       div(style = "margin-bottom: 20px;",
                           tags$label(style = "color: white; font-weight: bold; margin-bottom: 8px; display: block;",
                                      " Terreno M²:"),
                           sliderInput("filtro_terreno_m2",
                                       label = NULL,
                                       min = 0,
                                       max = 50000,
                                       value = c(0, 50000),
                                       step = 50, 
                                       sep = ",",
                                       post = " m²",
                                       ticks = TRUE,  
                                       animate = FALSE),
                           
                           div(style = "margin-top: 12px; margin-left: 5px;",
                               checkboxInput("incluir_grandes",
                                             label = div(style = "color: white; font-size: 13px;",
                                                         "Incluir inmuebles grandes (>50,000 m²)"),
                                             value = TRUE)
                           )
                       ),
                       
                       hr(style = "border-color: rgba(255,255,255,0.3);"),
                       
                       # BOTÓN LIMPIAR FILTROS
                       actionButton("limpiar_filtros",
                                    " Limpiar Filtros",
                                    class = "btn btn-block",
                                    style = "background-color: white; 
                                         color: #ff5b49; 
                                         border: none; 
                                         font-weight: bold;
                                         margin-bottom: 15px;
                                         padding: 12px;"),
                       
                       actionButton("recargar_mapa",
                                    tagList(icon("sync-alt"), "Recargar Mapa"),
                                    class = "btn btn-block",
                                    style = "background-color: #802e25;
                      color: white; 
                      border: none; 
                      font-weight: bold;
                      margin-top: 15px;
                      padding: 12px;"),
                       
                       actionButton("descargar_excel_mi",
                                    tagList(icon("download"), "Descargar Excel"),
                                    class = "btn btn-block",
                                    style = "background-color: #802e25; 
                        color: white; 
                        border: none;
                        font-weight: bold;
                        padding: 12px;"),
                   )
            ),
            
            # ============================================================================
            # COLUMNA DERECHA: FICHAS + MAPA + TABLA (CON ID PARA EXPANDIR)
            # ============================================================================
            column(9,
                   div(id = "contenido_principal",
                       class = "contenido-principal",
                       
                       # BOTÓN FLOTANTE
                       div(id = "boton_mostrar_filtros",
                           style = "display: none; position: absolute; top: 20px; left: 15px; z-index: 1000;",
                           actionButton("mostrar_filtros_btn",
                                        "",
                                        icon = icon("bars"),
                                        class = "btn",
                                        style = "background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%);
                                     color: white;
                                     border: none;
                                     border-radius: 50%;
                                     width: 50px;
                                     height: 50px;
                                     font-size: 20px;
                                     box-shadow: 0 4px 15px rgba(230, 126, 34, 0.4);
                                     cursor: pointer;
                                     transition: all 0.3s ease;")
                       ),
                       
                       # FICHAS INFORMATIVAS SUPERIORES
                       # FICHAS INFORMATIVAS SUPERIORES
                       fluidRow(
                         # FICHA 1: INMUEBLES TOTALES
                         column(3,
                                div(style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                                         padding: 20px; 
                                         border-radius: 10px; 
                                         color: white; 
                                         text-align: center;
                                         box-shadow: 0 4px 15px rgba(251, 140, 0, 0.3);
                                         margin-bottom: 20px;",
                                    h2(style = "margin: 0; font-size: 2.5rem; font-weight: bold;",
                                       textOutput("total_inmuebles")),
                                    p(style = "margin: 5px 0 0 0; font-size: 1.1rem;", "Inmuebles Totales")
                                )
                         ),
                         
                         # FICHA 2: TERRENO M² TOTAL
                         column(3,
                                div(style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                                         padding: 20px; 
                                         border-radius: 10px; 
                                         color: white; 
                                         text-align: center;
                                         box-shadow: 0 4px 15px rgba(245, 124, 0, 0.3);
                                         margin-bottom: 20px;",
                                    h2(style = "margin: 0; font-size: 2.5rem; font-weight: bold;",
                                       textOutput("total_terreno_m2")),
                                    p(style = "margin: 5px 0 0 0; font-size: 1.1rem;", "Terreno M² Total")
                                )
                         ),
                         
                         # FICHA 3: EMPRESAS
                         column(3,
                                div(style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                                         padding: 20px; 
                                         border-radius: 10px; 
                                         color: white; 
                                         text-align: center;
                                         box-shadow: 0 4px 15px rgba(244, 81, 30, 0.3);
                                         margin-bottom: 20px;",
                                    h2(style = "margin: 0; font-size: 2.5rem; font-weight: bold;",
                                       textOutput("total_empresas")),
                                    p(style = "margin: 5px 0 0 0; font-size: 1.1rem;", "Empresas")
                                )
                         ),
                         
                         # FICHA 4: VALOR TOTAL PORTAFOLIO
                         column(3,
                                div(style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                                         padding: 20px; 
                                         border-radius: 10px; 
                                         color: white; 
                                         text-align: center;
                                         box-shadow: 0 4px 15px rgba(244, 81, 30, 0.3);
                                         margin-bottom: 20px;",
                                    h2(style = "margin: 0; font-size: 2.5rem; font-weight: bold;",
                                       textOutput("total_valor_portafolio")),
                                    p(style = "margin: 5px 0 0 0; font-size: 1.1rem;", "Valor Total. Precio de Adquisición")
                                )
                         )
                       ),
                       
                       # FILA CON MAPA + TABLA
                       fluidRow(
                         # MAPA CON RECUADRO
                         column(6,
                                div(style = "background: white; 
                                         padding: 15px; 
                                         border-radius: 10px; 
                                         border: 3px solid #ff5b49;
                                         box-shadow: 0 4px 15px rgba(230, 126, 34, 0.2);",
                                    div(style = "background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%);
                                             padding: 10px;
                                             border-radius: 5px;
                                             margin-bottom: 10px;",
                                        h4(style = "color: white; margin: 0; font-weight: bold; text-align: center;",
                                           "️ Ubicación de Inmuebles")
                                    ),
                                    leafletOutput("mapa_inmuebles", height = "79vh")
                                )
                         ),
                         
                         # TABLA CON RECUADRO
                         column(6,
                                div(style = "background: white; 
                                         padding: 15px; 
                                         border-radius: 10px; 
                                         border: 3px solid #802e25;
                                         box-shadow: 0 4px 15px rgba(243, 156, 18, 0.2);",
                                    div(style = "background: linear-gradient(135deg, #802e25 0%, #ff5b49 100%);
                                             padding: 10px;
                                             border-radius: 5px;
                                             margin-bottom: 10px;",
                                        h4(style = "color: white; margin: 0; font-weight: bold; text-align: center;",
                                           " Información Detallada")
                                    ),
                                    DT::dataTableOutput("tabla_inmuebles_resumida"),
                                    tags$style(HTML("
                                  div.dataTables_scrollBody {
                                    max-height: 65vh !important;
                                    height: 65vh !important;
                                  }
                                "))
                                )
                         )
                       )
                   )
            )
          )
        ),
        
        # ====================================================================
        # PESTAÑA 1.5: ASISTENTE IA
        # ====================================================================
        tabPanel(
          title = div(icon("robot"), " Asistente IA"),
          value = "asistente_ia",
          
          div(style = "padding: 25px;",
              
              # CSS del chat (solo aplica dentro de esta pestaña)
              tags$style(HTML("
              .chat-burbuja-usuario {
                display: flex; justify-content: flex-end; margin-bottom: 12px;
              }
              .chat-burbuja-usuario > div {
                background: linear-gradient(135deg, #802e25, #401712);
                color: white; padding: 10px 16px;
                border-radius: 18px 18px 4px 18px;
                max-width: 72%; font-size: 14px; line-height: 1.5;
              }
              .chat-burbuja-asistente {
                display: flex; justify-content: flex-start; margin-bottom: 12px;
              }
              .chat-burbuja-asistente > div {
                background: white; border: 1.5px solid #e0d4c3;
                color: #4A2E05; padding: 10px 16px;
                border-radius: 18px 18px 18px 4px;
                max-width: 72%; font-size: 14px; line-height: 1.5;
              }
              .chat-cargando > div {
                background: #f0ebe4 !important; color: #999 !important;
                font-style: italic;
              }
              #chat_scroll_box {
                height: 430px; overflow-y: auto; padding: 18px;
                background: #faf7f4; border-radius: 10px;
                border: 2px solid #e0d4c3; margin-bottom: 14px;
              }
            ")),
              
              fluidRow(
                column(12,
                       div(style = "background: white; padding: 22px; border-radius: 12px;
                             border: 3px solid #802e25;
                             box-shadow: 0 4px 15px rgba(230,126,34,0.15);",
                           
                           # Encabezado
                           div(style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                               padding: 12px 18px; border-radius: 8px; margin-bottom: 18px;
                               display: flex; align-items: center; justify-content: space-between;",
                               h4(style = "color: white; margin: 0; font-weight: bold;",
                                 " Asistente IA · Maestro de Inmuebles ALMENA"),
                               actionButton("chat_limpiar", "🗑️ Nueva conversación",
                                            style = "background: rgba(255,255,255,0.2); color: white;
                                          border: 1px solid white; border-radius: 6px;
                                          font-size: 12px; padding: 4px 10px;")
                           ),
                           
                           # Historial de chat
                           div(id = "chat_scroll_box",
                               uiOutput("chat_mensajes_ui")
                           ),
                           
                           # Input + botón
                           fluidRow(
                             column(9,
                                    textInput("chat_input", label = NULL, value = "",
                                              placeholder = "Ej: ¿Cuántos inmuebles activos hay en Querétaro?",
                                              width = "100%")
                             ),
                             column(3,
                                    div(style = "display: flex; gap: 6px;",
                                        actionButton("chat_enviar",
                                                     tagList(icon("paper-plane"), " Enviar"),
                                                     class  = "btn btn-primary",
                                                     style  = "width: 100%; height: 34px; font-weight: bold;"),
                                        disabled(
                                          actionButton("chat_enviar_disabled", "⏳",
                                                       style = "display:none; width:100%; height:34px;")
                                        )
                                    )
                             )
                           ),
                           
                           # Preguntas de ejemplo
                           div(style = "margin-top: 12px; padding-top: 12px;
                               border-top: 1px solid #e0d4c3;",
                               p(style = "color: #802e25; font-weight: bold; font-size: 12px;
                               margin-bottom: 8px;",
                                 "💡 Preguntas de ejemplo:"),
                               div(style = "display: flex; flex-wrap: wrap; gap: 6px;",
                                   actionButton("ej1", "¿Cuántos inmuebles activos hay?",
                                                class = "btn", style = "font-size: 11px; padding: 3px 10px;
                                   background: #f0ebe4; color: #802e25;
                                   border: 1px solid #c0a898; border-radius: 20px;"),
                                   actionButton("ej2", "¿Qué inmuebles están en Querétaro?",
                                                class = "btn", style = "font-size: 11px; padding: 3px 10px;
                                   background: #f0ebe4; color: #802e25;
                                   border: 1px solid #c0a898; border-radius: 20px;"),
                                   actionButton("ej3", "¿Cuál es el inmueble con mayor terreno?",
                                                class = "btn", style = "font-size: 11px; padding: 3px 10px;
                                   background: #f0ebe4; color: #802e25;
                                   border: 1px solid #c0a898; border-radius: 20px;"),
                                   actionButton("ej4", "¿Cuántos m² de construcción tenemos en total?",
                                                class = "btn", style = "font-size: 11px; padding: 3px 10px;
                                   background: #f0ebe4; color: #802e25;
                                   border: 1px solid #c0a898; border-radius: 20px;")
                               )
                           )
                       )
                )
              )
          )
        ),
        
        # PESTAÑA 2: CATÁLOGO DE INMUEBLES
        tabPanel(
          title = div(icon("table"), "Catálogo de Inmuebles"),
          value = "catalogo",
          fluidRow(
            column(12,
                   div(style = "background: white; padding: 20px; border-radius: 10px; border: 3px solid #ff5b49; margin-top: 10px;",
                       
                       # ENCABEZADO
                       div(style = "background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%); padding: 10px; border-radius: 5px; margin-bottom: 15px;",
                           h4(style = "color: white; margin: 0; font-weight: bold; text-align: center;",
                              icon("table"), " Catálogo Completo de Inmuebles")
                       ),
                       
                       # FILA DE FILTROS
                       fluidRow(
                         column(3,
                                div(style = "margin-bottom: 15px;",
                                    tags$label(style = "font-weight: bold; color: #802e25;", "Status:"),
                                    selectInput("filtro_catalogo_status",
                                                label = NULL,
                                                choices = c("Todos" = "todos",
                                                            "Activos" = "ACTIVO",
                                                            "Inactivos" = "INACTIVO"),
                                                selected = "todos",
                                                width = "100%")
                                )
                         ),
                         column(3,
                                div(style = "margin-bottom: 15px;",
                                    tags$label(style = "font-weight: bold; color: #802e25;", "Año:"),
                                    selectInput("filtro_catalogo_anio",
                                                label = NULL,
                                                choices = c("Todos" = "todos",
                                                            as.character(seq(as.integer(format(Sys.Date(), "%Y")), 2015, by = -1))),
                                                selected = "todos",
                                                width = "100%")
                                )
                         ),
                         column(4,
                                div(style = "margin-bottom: 15px;",
                                    tags$label(style = "font-weight: bold; color: #802e25;", "Rango de fechas:"),
                                    dateRangeInput("filtro_catalogo_fechas",
                                                   label = NULL,
                                                   start = paste0(format(Sys.Date(), "%Y"), "-01-01"),
                                                   end   = Sys.Date(),
                                                   format = "dd/mm/yyyy",
                                                   language = "es",
                                                   separator = " a ",
                                                   width = "100%")
                                )
                         ),
                         column(2,
                                div(style = "margin-bottom: 15px; padding-top: 23px; display: flex; gap: 8px;",
                                    actionButton("limpiar_filtros_catalogo",
                                                 tagList(icon("times"), " Limpiar"),
                                                 class = "btn",
                                                 style = "background-color: #802e25; color: white; border: none; font-weight: bold; padding: 7px; flex: 1;"),
                                    downloadButton("descargar_excel_catalogo",
                                                   tagList(icon("file-excel"), " Excel"),
                                                   class = "btn",
                                                   style = "background-color: #27ae60; color: white; border: none; font-weight: bold; padding: 7px; flex: 1;")
                                )
                         )
                       ),
                       
                       # RESUMEN RÁPIDO
                       uiOutput("resumen_catalogo"),
                       
                       DT::dataTableOutput("tabla_catalogo_completo")
                   )
            )
          )
        ),
        
        # PESTAÑA 3: CAPTURA DE DATOS 
        navbarMenu(
          title = div(icon("edit"), "Captura de Datos"),
          tabPanel("Administración", value = "cap_admin"),
          tabPanel("Jurídico", value = "cap_juridico"),
          tabPanel("Expansión", value = "cap_expansion"),
          tabPanel("Operaciones", value = "cap_operaciones"),
          tabPanel("Proyectos", value = "cap_proyectos"),
          tabPanel("Mercadotecnia", value = "cap_mkt"),
          tabPanel("Vivienda", value = "cap_vivienda"),
          tabPanel("Industrial", value = "cap_industrial"),
          tabPanel("Finanzas", value = "cap_finanzas"),
          tabPanel("Comercial", value = "cap_comercial"),
          tabPanel("Talento", value = "cap_talento"),
          tabPanel("Dirección", value = "cap_direccion")
        )
      )
  )
)

# ============================================================================
# FUNCIÓN: Guardar o Actualizar Inmueble Directamente en Snowflake
# ============================================================================
guardar_inmueble_snowflake <- function(datos_inmueble, poligono_kmz = NULL) {
  tryCatch({
    # Crear conexión JWT
    conn <- crear_conexion_snowflake()
    
    # Activar warehouse
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    # Escapar comillas simples en campos de texto
    id_limpio <- gsub("'", "''", datos_inmueble$ID)
    inmueble_limpio <- gsub("'", "''", datos_inmueble$INMUEBLE)
    empresa_limpia <- gsub("'", "''", datos_inmueble$EMPRESA)
    alias_limpio <- gsub("'", "''", datos_inmueble$ALIAS)
    direccion_limpia <- gsub("'", "''", datos_inmueble$DIRECCION)
    municipio_limpio <- gsub("'", "''", datos_inmueble$MUNICIPIO)
    estado_limpio <- gsub("'", "''", datos_inmueble$ESTADO)
    pais_limpio <- gsub("'", "''", datos_inmueble$PAIS)
    
    # Formatear fecha
    fecha_escritura_formateada <- paste0("'", datos_inmueble$FECHA_ESCRITURA, "'")
    
    # Verificar si ya existe el registro
    existe_sql <- paste0("SELECT COUNT(*) as count FROM INMUEBLES WHERE ID = '", id_limpio, "'")
    resultado_existe <- dbGetQuery(conn, existe_sql)
    ya_existe <- resultado_existe$COUNT > 0
    
    if(ya_existe) {
      # ============================================================================
      # ACTUALIZAR REGISTRO EXISTENTE
      # ============================================================================
      update_sql <- paste0(
        "UPDATE INMUEBLES SET ",
        "INMUEBLE = '", inmueble_limpio, "', ",
        "EMPRESA = '", empresa_limpia, "', ",
        "ALIAS = '", alias_limpio, "', ",
        "DIRECCION = '", direccion_limpia, "', ",
        "MUNICIPIO = '", municipio_limpio, "', ",
        "ESTADO = '", estado_limpio, "', ",
        "PAIS = '", pais_limpio, "', ",
        "FECHA_ESCRITURA = ", fecha_escritura_formateada, ", ",
        "PRECIO_ADQUISICION = ", datos_inmueble$PRECIO_ADQUISICION, ", ",
        "TERRENO_M2 = ", datos_inmueble$TERRENO_M2, ", ",
        "PRECIO_TERRENO_M2 = ", datos_inmueble$PRECIO_TERRENO_M2, ", ",
        "CONSTRUCCION_M2 = ", datos_inmueble$CONSTRUCCION_M2, ", ",
        "PRECIO_CONSTRUCCION_M2 = ", datos_inmueble$PRECIO_CONSTRUCCION_M2, ", ",
        "FECHA_ACTUALIZACION = CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ ",
        "WHERE ID = '", id_limpio, "'"
      )
      
      dbSendUpdate(conn, update_sql)
      cat(" Registro ACTUALIZADO en Snowflake:", datos_inmueble$ID, "\n")
      tipo_operacion <- "actualizado"
      
    } else {
      # ============================================================================
      # INSERTAR NUEVO REGISTRO
      # ============================================================================
      insert_sql <- paste0(
        "INSERT INTO INMUEBLES (ID, INMUEBLE, EMPRESA, ALIAS, DIRECCION, MUNICIPIO, ESTADO, PAIS, ",
        "FECHA_ESCRITURA, PRECIO_ADQUISICION, TERRENO_M2, PRECIO_TERRENO_M2, CONSTRUCCION_M2, PRECIO_CONSTRUCCION_M2, FECHA_ACTUALIZACION) VALUES (",  
        "'", id_limpio, "', ",
        "'", inmueble_limpio, "', ",
        "'", empresa_limpia, "', ",
        "'", alias_limpio, "', ",
        "'", direccion_limpia, "', ",
        "'", municipio_limpio, "', ",
        "'", estado_limpio, "', ",
        "'", pais_limpio, "', ",
        fecha_escritura_formateada, ", ",
        datos_inmueble$PRECIO_ADQUISICION, ", ",
        datos_inmueble$TERRENO_M2, ", ",
        datos_inmueble$PRECIO_TERRENO_M2, ", ",
        datos_inmueble$CONSTRUCCION_M2, ", ",
        datos_inmueble$PRECIO_CONSTRUCCION_M2, ", ",
        "CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ",
        ")"
      )
      
      dbSendUpdate(conn, insert_sql)
      cat("➕ Nuevo registro INSERTADO en Snowflake:", datos_inmueble$ID, "\n")
      tipo_operacion <- "guardado"
    }
    
    # ============================================================================
    # PROCESAR KMZ SI EXISTE
    # ============================================================================
    if (!is.null(poligono_kmz)) {
      # Añadir datos del inmueble al polígono
      poligono_kmz$ID_INMUEBLE <- datos_inmueble$ID
      poligono_kmz$INMUEBLE <- datos_inmueble$INMUEBLE
      poligono_kmz$EMPRESA <- datos_inmueble$EMPRESA
      poligono_kmz$ALIAS <- datos_inmueble$ALIAS
      poligono_kmz$ESTADO <- datos_inmueble$ESTADO
      poligono_kmz$MUNICIPIO <- datos_inmueble$MUNICIPIO
      poligono_kmz$PRECIO_ADQUISICION <- datos_inmueble$PRECIO_ADQUISICION
      poligono_kmz$TERRENO_M2 <- datos_inmueble$TERRENO_M2
      poligono_kmz$CONSTRUCCION_M2 <- datos_inmueble$CONSTRUCCION_M2
      
      cat(" KMZ procesado para el inmueble\n")
    }
    
    # Cerrar conexión
    dbDisconnect(conn)
    
    return(list(exito = TRUE, tipo = tipo_operacion, poligono = poligono_kmz))
    
  }, error = function(e) {
    cat("❌ Error al guardar Inmueble:", e$message, "\n")
    return(list(exito = FALSE, mensaje = e$message))
  })
}

# ============================================================================
# CACHE GLOBAL INPC — se lee desde Snowflake
# ============================================================================
cache_inpc <- NULL

cargar_cache_inpc <- function() {
  tryCatch({
    conn <- crear_conexion_snowflake_global()
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    df_inpc <- dbGetQuery(conn, "
      SELECT FECHA, VALOR
      FROM DB_ANALYTICS.SCH_CORE.INPC_HISTORICO
      ORDER BY FECHA ASC
    ")
    
    dbDisconnect(conn)
    
    if (nrow(df_inpc) == 0) {
      cat("⚠️ Tabla INPC_HISTORICO vacía\n")
      return(FALSE)
    }
    
    df_inpc$FECHA <- as.Date(df_inpc$FECHA)
    df_inpc$VALOR <- as.numeric(df_inpc$VALOR)
    names(df_inpc) <- c("fecha", "valor")
    
    cache_inpc <<- df_inpc
    cat("✅ INPC cargado desde Snowflake:", nrow(df_inpc), 
        "observaciones. Último:", format(tail(df_inpc$fecha, 1), "%m/%Y"), "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("⚠️ No se pudo cargar INPC desde Snowflake:", e$message, "\n")
    return(FALSE)
  })
}

# Cargar INPC al inicio (scope global, solo una vez por proceso R)
cargar_cache_inpc()

obtener_valor_inflacionado <- function(precio_original, fecha_adquisicion) {
  tryCatch({
    if (is.null(precio_original) || is.na(precio_original) || precio_original == 0) return(NULL)
    if (is.null(fecha_adquisicion) || is.na(fecha_adquisicion)) return(NULL)
    
    fecha_str <- substr(trimws(as.character(fecha_adquisicion)), 1, 10)
    if (fecha_str %in% c("", "-", "NA")) return(NULL)
    
    fecha <- suppressWarnings(as.Date(fecha_str, format = "%Y-%m-%d"))
    if (is.na(fecha)) return(NULL)
    
    # Si no hay cache todavía, retornar NULL silenciosamente
    if (is.null(cache_inpc) || nrow(cache_inpc) == 0) return(NULL)
    
    df_inpc <- cache_inpc
    
    fecha_compra_mes <- as.Date(paste0(format(fecha, "%Y-%m"), "-01"))
    inpc_compra_row  <- df_inpc[df_inpc$fecha >= fecha_compra_mes, ]
    if (nrow(inpc_compra_row) == 0) return(NULL)
    inpc_compra <- inpc_compra_row$valor[1]
    inpc_actual <- tail(df_inpc$valor, 1)
    
    if (is.na(inpc_compra) || is.na(inpc_actual) || inpc_compra == 0) return(NULL)
    
    valor_actualizado <- precio_original * (inpc_actual / inpc_compra)
    
    return(list(
      valor         = round(valor_actualizado, 0),
      fecha_inpc    = format(tail(df_inpc$fecha, 1), "%m/%Y"),
      variacion_pct = round((inpc_actual / inpc_compra - 1) * 100, 1)
    ))
  }, error = function(e) {
    return(NULL)
  })
}

server <- function(input, output, session) {
  
  # INPC ya se cargó a nivel global (fuera del server)
  # ==========================================================================
  # SISTEMA DE AUTENTICACIÓN
  # ==========================================================================
  
  
  # Variable reactiva para controlar autenticación
  usuario_autenticado <- reactiveVal(FALSE)
  nombre_usuario <- reactiveVal("")
  
  # Observar intento de login
  observeEvent(input$btn_login, {
    
    usuario <- trimws(input$login_usuario)
    password <- trimws(input$login_password)
    
    # Validar campos vacíos
    if (usuario == "" || password == "") {
      output$login_error_msg <- renderUI({
        div(class = "login-error-main",
            "⚠️ Por favor ingrese usuario y contraseña")
      })
      return()
    }
    
    # Validar credenciales
    if (verificar_credenciales(usuario, password, usuarios_validos)) {
      
      # Login exitoso
      usuario_autenticado(TRUE)
      nombre_usuario(usuario)
      
      # Ocultar pantalla de login y mostrar app
      shinyjs::hide("login_screen")
      shinyjs::addClass("app_content", "authenticated")
      
      showNotification(" Cargando datos...", type = "message", duration = 2)
      
      shinyjs::delay(300, {
        # Cargar polígonos e inmuebles en una sola conexión
        cat("️ Iniciando carga consolidada...\n")
        cargar_datos_inicio()
        
        # ESPERAR A QUE EL MAPA SEA VISIBLE Y DIBUJAR POLÍGONOS
        shinyjs::delay(200, {
          poligonos_sf <- poligonos_inmuebles()
          if (!is.null(poligonos_sf) && nrow(poligonos_sf) > 0) {
            cat("Poligonos disponibles:", nrow(poligonos_sf), "\n")
            
            sf::sf_use_s2(FALSE)
            poligonos_sf <- st_make_valid(poligonos_sf)
            centroides <- st_centroid(poligonos_sf)
            coords <- st_coordinates(centroides)
            lng_vals <- coords[, 1]
            lat_vals <- coords[, 2]
            
            # Aplicar fitBounds
            leafletProxy("mapa_inmuebles") %>%
              fitBounds(
                lng1 = min(lng_vals) - 0.5,
                lat1 = min(lat_vals) - 0.5,
                lng2 = max(lng_vals) + 0.5,
                lat2 = max(lat_vals) + 0.5
              )
            
            showNotification(" Mapa cargado con polígonos", type = "message", duration = 1)
          } else {
            cat("❌ No hay polígonos disponibles\n")
            showNotification("⚠️ No se encontraron polígonos", type = "warning", duration = 3)
          }
          
          sf::sf_use_s2(TRUE)
          
        })
      })
      
      # Limpiar campos
      updateTextInput(session, "login_usuario", value = "")
      updateTextInput(session, "login_password", value = "")
      output$login_error_msg <- renderUI({NULL})
      
      showNotification(
        paste0("✅ Bienvenido ", usuario, "!"),
        type = "message",
        duration = 3
      )
      
    } else {
      # Login fallido
      output$login_error_msg <- renderUI({
        div(class = "login-error-main",
            "❌ Usuario o contraseña incorrectos")
      })
      
      updateTextInput(session, "login_password", value = "")
    }
  })
  
  # Permitir login con Enter
  observe({
    shinyjs::runjs('
      $(document).on("keypress", "#login_password", function(e) {
        if(e.which == 13) {
          $("#btn_login").click();
        }
      });
    ')
  })
  
  # Variable reactiva para almacenar el archivo Excel generado
  archivo_excel_generado <- reactiveVal(NULL)
  
  # ============================================================================
  # FUNCIÓN: Conectar a Snowflake con JWT (usa driver y llave cacheados)
  # ============================================================================
  crear_conexion_snowflake <- function(usuario = "SVC_ANALYTICS", ruta_pem = Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")) {
    tryCatch({
      # Usar driver cacheado globalmente
      driver <- SNOWFLAKE_JDBC_DRIVER
      if (is.null(driver)) {
        ruta_archivo_jar <- file.path(getwd(), "snowflake-jdbc-3.25.1.jar")
        driver <- RJDBC::JDBC("net.snowflake.client.jdbc.SnowflakeDriver", 
                              classPath = ruta_archivo_jar, 
                              identifier.quote = "`")
      }
      
      # Usar llave privada cacheada globalmente
      private_key <- SNOWFLAKE_PRIVATE_KEY
      if (is.null(private_key)) {
        ruta_pem_absoluta <- file.path(getwd(), ruta_pem)
        private_key <- convertir_pem_a_privatekey(ruta_pem_absoluta)
      }
      
      url_conexion <- paste0("jdbc:snowflake://", Sys.getenv("SNOWFLAKE_ACCOUNT"), ".snowflakecomputing.com/")
      
      propiedades <- .jnew("java.util.Properties")
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "user", usuario)
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "warehouse", "WH_ANALYTICS")
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "db", "DB_ANALYTICS")
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "schema", "SCH_CORE")
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "role", "ROLE_ANALYTICS")
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "authenticator", "SNOWFLAKE_JWT")
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "tracing", "OFF")
      .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "JDBC_QUERY_RESULT_FORMAT", "JSON")
      
      .jcall(propiedades, "Ljava/lang/Object;", "put", 
             .jcast(.jnew("java/lang/String", "privateKey"), "java/lang/Object"),
             .jcast(private_key, "java/lang/Object"))
      
      jconn <- .jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url_conexion, propiedades)
      
      if (is.jnull(jconn)) {
        stop("La conexión retornó null")
      }
      
      conexion <- new("JDBCConnection", jc = jconn, identifier.quote = driver@identifier.quote)
      
      if (!DBI::dbIsValid(conexion)) {
        stop("La conexión no es válida")
      }
      
      cat("✅ Conexión exitosa a Snowflake (JWT)\n")
      return(conexion)
      
    }, error = function(e) {
      stop(paste("❌ Error:", e$message))
    })
  }
  
  # ============================================================================
  # FUNCIÓN: Carga consolidada de inicio (polígonos + inmuebles en 1 conexión)
  # ============================================================================
  cargar_datos_inicio <- function() {
    tryCatch({
      cat("Carga consolidada: polígonos + inmuebles en una sola conexión...\n")
      
      conn <- crear_conexion_snowflake()
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
      dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
      dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
      
      # --- QUERY 1: Polígonos activos ---
      query_poligonos <- "
        SELECT
          g.ID        AS ID_INMUEBLE,
          i.INMUEBLE  AS Name,
          i.INMUEBLE  AS INMUEBLE,
          i.EMPRESA,
          i.DIRECCION,
          i.MUNICIPIO,
          i.ESTADO,
          g.GEOMETRIA_WKT,
          g.LATITUD_CENTROIDE  AS LATITUD,
          g.LONGITUD_CENTROIDE AS LONGITUD,
          'SNOWFLAKE_TABLE' AS FUENTE,
          CAST(NULL AS VARCHAR) AS NOMBRE_ARCHIVO_ORIGINAL
        FROM DB_ANALYTICS.SCH_CORE.INMUEBLES_GEOMETRIA g
        JOIN DB_ANALYTICS.SCH_CORE.INMUEBLES i
          ON g.ID = i.ID
        WHERE g.ID IN (
          SELECT ID
          FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
          WHERE UPPER(TRIM(STATUS_ACTIVIDAD)) = 'ACTIVO'
        )
      "
      df_geometria <- dbGetQuery(conn, query_poligonos)
      cat("Polígonos obtenidos:", nrow(df_geometria), "\n")
      
      # --- QUERY 2: Inmuebles activos ---
      query_inmuebles <- "
      WITH ULTIMA_ACTIVIDAD AS (
        SELECT 
          ID, 
          STATUS_ACTIVIDAD, 
          FECHA_ASIGNACION, 
          USUARIO AS USUARIO_ACTIVIDAD
        FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
        QUALIFY ROW_NUMBER() OVER (PARTITION BY ID ORDER BY FECHA_ASIGNACION DESC) = 1
      )
      SELECT 
        i.*,
        a.STATUS_ACTIVIDAD,
        a.FECHA_ASIGNACION,
        a.USUARIO_ACTIVIDAD
      FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
      INNER JOIN ULTIMA_ACTIVIDAD a ON i.ID = a.ID
      WHERE UPPER(TRIM(a.STATUS_ACTIVIDAD)) = 'ACTIVO'
      "
      inmuebles_activos <- dbGetQuery(conn, query_inmuebles)
      cat("Inmuebles activos obtenidos:", nrow(inmuebles_activos), "\n")
      
      # Cerrar la única conexión
      dbDisconnect(conn)
      
      # --- PROCESAR POLÍGONOS ---
      if (nrow(df_geometria) > 0) {
        df_geometria$LATITUD  <- as.numeric(df_geometria$LATITUD)
        df_geometria$LONGITUD <- as.numeric(df_geometria$LONGITUD)
        
        poligonos_sf <- st_as_sf(df_geometria, wkt = "GEOMETRIA_WKT", crs = 4326)
        poligonos_sf <- st_make_valid(poligonos_sf)
        
        # Normalizar geometrias
        poligonos_sf <- tryCatch({
          st_collection_extract(poligonos_sf, "POLYGON")
        }, error = function(e) {
          geoms_limpias <- lapply(seq_len(nrow(poligonos_sf)), function(i) {
            tryCatch({
              g <- st_geometry(poligonos_sf[i, ])
              tipo <- as.character(st_geometry_type(g))
              if (grepl("COLLECTION", tipo)) {
                st_collection_extract(poligonos_sf[i, ], "POLYGON")
              } else {
                poligonos_sf[i, ]
              }
            }, error = function(e2) NULL)
          })
          geoms_limpias <- geoms_limpias[!sapply(geoms_limpias, is.null)]
          do.call(rbind, geoms_limpias)
        })
        
        # Unificar a MULTIPOLYGON
        geom_col <- attr(poligonos_sf, "sf_column")
        poligonos_sf[[geom_col]] <- lapply(seq_len(nrow(poligonos_sf)), function(i) {
          tryCatch({
            g <- st_geometry(poligonos_sf[i, ])[[1]]
            tipo <- as.character(st_geometry_type(g))
            if (tipo == "POLYGON") {
              st_cast(g, "MULTIPOLYGON")
            } else if (tipo == "MULTIPOLYGON") {
              g
            } else {
              st_cast(st_cast(g, "POLYGON"), "MULTIPOLYGON")
            }
          }, error = function(e) st_geometry(poligonos_sf[i, ])[[1]])
        })
        poligonos_sf[[geom_col]] <- st_sfc(poligonos_sf[[geom_col]], crs = 4326)
        poligonos_sf <- st_sf(st_drop_geometry(poligonos_sf),
                              geometry = poligonos_sf[[geom_col]])
        
        cat(nrow(poligonos_sf), "polígonos procesados.\n")
        poligonos_inmuebles(poligonos_sf)
      } else {
        cat("No hay polígonos activos.\n")
        poligonos_inmuebles(NULL)
      }
      
      # --- PROCESAR INMUEBLES ---
      if (nrow(inmuebles_activos) > 0) {
        showNotification(" Inmuebles cargados", type = "message", duration = 1)
      } else {
        showNotification("⚠️ No se encontraron inmuebles ACTIVOS", type = "warning")
      }
      inmuebles_snowflake_df(inmuebles_activos)
      
      cat("✅ Carga consolidada completada.\n")
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error en carga consolidada:", e$message, "\n")
      return(FALSE)
    })
  }
  
  # ============================================================================
  # FUNCIÓN: Cargar polígonos desde SNOWFLAKE STAGE
  # ============================================================================
  cargar_poligonos_inmuebles <- function() {
    tryCatch({
      cat("Cargando poligonos optimizados desde Snowflake...\n")
      
      conn <- crear_conexion_snowflake()
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
      dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
      dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
      
      query <- "
        SELECT
          g.ID        AS ID_INMUEBLE,
          i.INMUEBLE  AS Name,
          i.INMUEBLE  AS INMUEBLE,
          i.EMPRESA,
          i.DIRECCION,
          i.MUNICIPIO,
          i.ESTADO,
          g.GEOMETRIA_WKT,
          g.LATITUD_CENTROIDE  AS LATITUD,
          g.LONGITUD_CENTROIDE AS LONGITUD,
          'SNOWFLAKE_TABLE' AS FUENTE,
          CAST(NULL AS VARCHAR) AS NOMBRE_ARCHIVO_ORIGINAL
        FROM DB_ANALYTICS.SCH_CORE.INMUEBLES_GEOMETRIA g
        JOIN DB_ANALYTICS.SCH_CORE.INMUEBLES i
          ON g.ID = i.ID
        WHERE g.ID IN (
          SELECT ID
          FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
          WHERE UPPER(TRIM(STATUS_ACTIVIDAD)) = 'ACTIVO'
        )
      "
      
      df_geometria <- dbGetQuery(conn, query)
      dbDisconnect(conn)
      
      if (nrow(df_geometria) == 0) {
        cat("No hay poligonos activos en INMUEBLES_GEOMETRIA\n")
        return(NULL)
      }
      
      df_geometria$LATITUD  <- as.numeric(df_geometria$LATITUD)
      df_geometria$LONGITUD <- as.numeric(df_geometria$LONGITUD)
      
      poligonos_sf <- st_as_sf(df_geometria, wkt = "GEOMETRIA_WKT", crs = 4326)
      poligonos_sf <- st_make_valid(poligonos_sf)
      
      # Normalizar geometrias: extraer poligonos de colecciones mixtas
      poligonos_sf <- tryCatch({
        st_collection_extract(poligonos_sf, "POLYGON")
      }, error = function(e) {
        # Si falla en bloque, procesar fila por fila
        geoms_limpias <- lapply(seq_len(nrow(poligonos_sf)), function(i) {
          tryCatch({
            g <- st_geometry(poligonos_sf[i, ])
            tipo <- as.character(st_geometry_type(g))
            if (grepl("COLLECTION", tipo)) {
              st_collection_extract(poligonos_sf[i, ], "POLYGON")
            } else {
              poligonos_sf[i, ]
            }
          }, error = function(e2) NULL)
        })
        geoms_limpias <- geoms_limpias[!sapply(geoms_limpias, is.null)]
        do.call(rbind, geoms_limpias)
      })
      
      # Unificar todo a MULTIPOLYGON fila por fila para evitar errores de cast
      geom_col <- attr(poligonos_sf, "sf_column")
      poligonos_sf[[geom_col]] <- lapply(seq_len(nrow(poligonos_sf)), function(i) {
        tryCatch({
          g <- st_geometry(poligonos_sf[i, ])[[1]]
          tipo <- as.character(st_geometry_type(g))
          if (tipo == "POLYGON") {
            st_cast(g, "MULTIPOLYGON")
          } else if (tipo == "MULTIPOLYGON") {
            g
          } else {
            st_cast(st_cast(g, "POLYGON"), "MULTIPOLYGON")
          }
        }, error = function(e) st_geometry(poligonos_sf[i, ])[[1]])
      })
      poligonos_sf[[geom_col]] <- st_sfc(poligonos_sf[[geom_col]], crs = 4326)
      poligonos_sf <- st_sf(st_drop_geometry(poligonos_sf),
                            geometry = poligonos_sf[[geom_col]])
      
      cat(nrow(poligonos_sf), "poligonos cargados desde tabla.\n")
      return(poligonos_sf)
      
    }, error = function(e) {
      cat("Error en carga optimizada:", e$message, "\n")
      return(NULL)
    })
  }
  
  # ============================================== 
  # VARIABLES REACTIVAS ORIGINALES DE ACTUALIZACION
  # ==============================================
  datos_clientes <- reactiveVal(crear_estructura_vacia())
  agenda_df <- reactiveVal(NULL)
  inmuebles_snowflake_df <- reactiveVal(NULL)
  kmz_temporal <- reactiveVal(NULL)
  inmueble_seleccionado_mapa <- reactiveVal(NULL)
  
  # ============================================================================
  # LÓGICA DE CAPTURA DE DATOS 
  # ============================================================================
  observeEvent(input$main_navbar, {
    req(input$main_navbar)
    
    # Función para generar los modales con descripciones en activos
    crear_lista_subtitulos <- function(titulo, subs_activos = list(), subs_inactivos = list()) {
      showModal(modalDialog(
        title = div(style = "color: #ff5b49; font-weight: bold; font-size: 22px; text-align: center;", titulo),
        div(style = "padding: 20px;",
            
            # SECCIÓN DE MÓDULOS ACTIVOS (CON DESCRIPCIÓN)
            if(length(subs_activos) > 0) {
              lapply(subs_activos, function(s) {
                tags$a(href = s$url, target = "_blank", style = "text-decoration: none;",
                       div(style = "background: white; padding: 15px; border-radius: 10px; border: 2px solid #ff5b49; margin-bottom: 15px; display: flex; align-items: center; gap: 15px;",
                           div(style = "background: #ff5b49; padding: 10px; border-radius: 50%; color: white;", icon("external-link-alt")),
                           div(
                             h4(style = "margin: 0; color: #2c3e50; font-weight: bold;", s$nombre),
                             p(style = "margin: 5px 0 0 0; color: #666; font-size: 12px;", s$descripcion) # <-- AQUÍ LA DESCRIPCIÓN
                           )
                       )
                )
              })
            },
            
            # SECCIÓN DE MÓDULOS INACTIVOS (GRISES)
            if(length(subs_inactivos) > 0) {
              lapply(subs_inactivos, function(s) {
                div(class = "modulo-inactivo", 
                    style = "background: #f8f9fa; padding: 15px; border-radius: 10px; border: 1px solid #ddd; margin-bottom: 10px; display: flex; align-items: center; gap: 15px;",
                    div(style = "color: #a0a0a0;", icon("lock")),
                    div(h4(style = "margin: 0; color: #a0a0a0;", s))
                )
              })
            }
        ),
        footer = modalButton("Volver"),
        size = "m", easyClose = TRUE
      ))
    }
    
    # Ruteo con las descripciones originales
    if(input$main_navbar == "cap_admin") {
      crear_lista_subtitulos("Administración", 
                             subs_inactivos = list("Contabilidad", "Facturación", "Cobranza", "Tesorería"))
      
    } else if(input$main_navbar == "cap_juridico") {
      crear_lista_subtitulos("Jurídico", 
                             subs_activos = list(
                               list(nombre = "Edición de Inmuebles", 
                                    descripcion = "Modifica los atributos y datos maestros de las propiedades existentes.",
                                    url = "https://demo-account.shinyapps.io/EdicionInmueble/"),
                               list(nombre = "Registro de Fichas Predios", 
                                    descripcion = "Captura nueva información técnica y legal de predios para regulación.",
                                    url = "https://demo-account.shinyapps.io/RegulacionFichasprediales/")
                             ),
                             subs_inactivos = list("Contratos", "Tramites", "PLD", "Compraventa", "Contratos internos", "Prediales"))
      
    } else if(input$main_navbar == "cap_expansion") {
      crear_lista_subtitulos("Expansión", 
                             subs_activos = list(
                               list(nombre = "Registro de Estatus Productividad", 
                                    descripcion = "Monitorea y actualiza el avance de actividades de la dirección de expansión.",
                                    url = "https://demo-account.shinyapps.io/Estatus_Actividad/")
                             ))
      
    } else if(input$main_navbar == "cap_operaciones") {
      crear_lista_subtitulos("Operaciones", 
                             subs_inactivos = list("Adecuaciones", "Servicios", "Sistemas"))
      
    } else if(input$main_navbar == "cap_proyectos") {
      crear_lista_subtitulos("Proyectos", 
                             subs_inactivos = list("Presupuesto de obra", "Proyectos", "Desarrollos"))
      
    } else if(input$main_navbar %in% c("cap_mkt", "cap_vivienda", "cap_industrial", "cap_finanzas", "cap_comercial", "cap_talento", "cap_direccion")) {
      crear_lista_subtitulos("Módulo en Desarrollo", 
                             subs_inactivos = list("Esta sección estará disponible próximamente."))
    }
  })
  
  # AGREGAR ESTE OBSERVE PARA CARGAR AGENDA:
  observe({
    if(is.null(agenda_df())) {
      tryCatch({
        # Usar datos de inmuebles como agenda por defecto
        datos_actuales <- datos_clientes()
        if(nrow(datos_actuales) > 0) {
          # Crear agenda con formato correcto PARA INMUEBLES
          agenda_datos <- data.frame(
            ID = datos_actuales$ID,                    
            EMPRESA = datos_actuales$EMPRESA,          
            INMUEBLE = datos_actuales$INMUEBLE,        
            stringsAsFactors = FALSE
          )
          agenda_df(agenda_datos)
          cat("✅ Agenda cargada desde datos de inmuebles:", nrow(agenda_datos), "registros\n")
        } else {
          # Crear agenda vacía PARA INMUEBLES
          agenda_datos <- data.frame(
            ID = character(0),
            EMPRESA = character(0),
            INMUEBLE = character(0),
            stringsAsFactors = FALSE
          )
          agenda_df(agenda_datos)
          cat("✅ Agenda vacía creada\n")
        }
      }, error = function(e) {
        cat("❌ Error al cargar agenda:", e$message, "\n")
        agenda_df(data.frame(
          ID = character(0),
          EMPRESA = character(0),
          INMUEBLE = character(0),
          stringsAsFactors = FALSE
        ))
      })
    }
  })
  
  # Variable reactiva para almacenar polígonos
  poligonos_inmuebles <- reactiveVal(NULL)
  
  datos_mapa_inmuebles <- reactive({
    cat("Construyendo datos para el mapa...\n")
    
    poligonos <- poligonos_inmuebles()
    
    if (is.null(poligonos) || nrow(poligonos) == 0) {
      cat("No hay poligonos disponibles\n")
      return(NULL)
    }
    
    cat("Poligonos listos:", nrow(poligonos), "registros\n")
    
    tryCatch({
      
      # Usar datos ya cargados en memoria en lugar de abrir nueva conexión
      inmuebles_full <- inmuebles_snowflake_df()
      if (is.null(inmuebles_full) || nrow(inmuebles_full) == 0) {
        cat("Datos de inmuebles aún no disponibles en memoria\n")
        return(NULL)
      }
      
      # Seleccionar solo las columnas necesarias para el enriquecimiento
      cols_necesarias <- c("ID", "ALIAS", "PRECIO_ADQUISICION", "TERRENO_M2", "CONSTRUCCION_M2", "PAIS", "FECHA_ESCRITURA")
      cols_disponibles <- cols_necesarias[cols_necesarias %in% names(inmuebles_full)]
      inmuebles <- inmuebles_full[, cols_disponibles, drop = FALSE]
      
      poligonos_data <- st_drop_geometry(poligonos)
      
      datos_enriquecidos <- merge(
        poligonos_data,
        inmuebles,
        by.x = "ID_INMUEBLE",
        by.y = "ID",
        all.x = TRUE
      )
      
      # Calcular areas desde la geometria
      areas_kmz_m2 <- as.numeric(st_area(poligonos))
      nombre_orden <- match(datos_enriquecidos$ID_INMUEBLE, poligonos$ID_INMUEBLE)
      datos_enriquecidos$TERRENO_M2_KMZ <- areas_kmz_m2[nombre_orden]
      
      terreno_sf <- as.numeric(datos_enriquecidos$TERRENO_M2)
      datos_enriquecidos$TERRENO_M2_ORIGINAL <- terreno_sf
      datos_enriquecidos$TERRENO_M2 <- ifelse(
        !is.na(terreno_sf) & terreno_sf > 0 & terreno_sf < 500000,
        terreno_sf,
        round(datos_enriquecidos$TERRENO_M2_KMZ, 2)
      )
      
      # Reconstruir sf con geometria
      geometria_ordenada <- poligonos[
        match(datos_enriquecidos$ID_INMUEBLE, poligonos$ID_INMUEBLE),
        attr(poligonos, "sf_column")
      ]
      
      poligonos_enriquecidos <- st_sf(
        datos_enriquecidos,
        geometry = geometria_ordenada[[attr(poligonos, "sf_column")]]
      )
      
      cat("Poligonos finales en el mapa:", nrow(poligonos_enriquecidos), "\n")
      
      return(poligonos_enriquecidos)
      
    }, error = function(e) {
      cat("Error en datos_mapa_inmuebles:", e$message, "\n")
      return(NULL)
    })
  })
  
  observe({
    # Solo cargar si ya está autenticado y los datos no se cargaron aún
    if(isTRUE(usuario_autenticado()) && is.null(inmuebles_snowflake_df())) {
      cat(" Cargando inmuebles al iniciar app...\n")
      cargar_inmuebles_snowflake()
    }
  })
  
  # ============================================== 
  # LÓGICA DEL SISTEMA DE LOGIN
  # =============================================
  output$login_message_area <- renderUI({
    mensaje <- mensaje_login()
    if (!is.null(mensaje)) {
      div(class = paste("alert-login", mensaje$tipo), mensaje$texto)
    }
  })
  
  # Manejar el botón de login
  observeEvent(input$login_btn, {
    # Verificar que los campos no estén vacíos
    if(is.null(input$login_usuario) || input$login_usuario == "" || 
       is.null(input$login_password) || input$login_password == "") {
      mensaje_login(list(
        tipo = "alert-warning",
        texto = "⚠️ Por favor, complete todos los campos."
      ))
      return()
    }
    
    # Verificar credenciales
    usuario_valido <- validar_conexion_snowflake(input$login_usuario, input$login_password)
    
    if(usuario_valido) {
      
      # Mostrar notificación de bienvenida
      showNotification(
        paste(" ¡Bienvenido,", input$login_usuario, "! Acceso autorizado."), 
        type = "message", 
        duration = 1
      )
      
    } else {
      # Login fallido
      mensaje_login(list(
        tipo = "alert-danger",
        texto = "❌ Usuario o contraseña incorrectos. Intente nuevamente."
      ))
    }
  })
  
  # Manejar el botón de logout
  observeEvent(input$logout_btn, {
    usuario_actual(NULL)
    mensaje_login(NULL)
    
    # Limpiar campos de login
    updateTextInput(session, "login_usuario", value = "")
    updateTextInput(session, "login_password", value = "")
    
    showNotification(" Sesión cerrada exitosamente", type = "warning", duration = 1)
  })
  
  # ============================================================================
  # FUNCIÓN: CARGAR INMUEBLES DESDE SNOWFLAKE 
  # ============================================================================
  cargar_inmuebles_snowflake <- function(){
    tryCatch({
      conn <- crear_conexion_snowflake()
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
      
      # SQL unificado: Trae inmuebles y su ÚLTIMO estatus de actividad
      query_completo <- "
      WITH ULTIMA_ACTIVIDAD AS (
        SELECT 
          ID, 
          STATUS_ACTIVIDAD, 
          FECHA_ASIGNACION, 
          USUARIO AS USUARIO_ACTIVIDAD
        FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
        QUALIFY ROW_NUMBER() OVER (PARTITION BY ID ORDER BY FECHA_ASIGNACION DESC) = 1
      )
      SELECT 
        i.*,
        a.STATUS_ACTIVIDAD,
        a.FECHA_ASIGNACION,
        a.USUARIO_ACTIVIDAD
      FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
      INNER JOIN ULTIMA_ACTIVIDAD a ON i.ID = a.ID
      WHERE UPPER(TRIM(a.STATUS_ACTIVIDAD)) = 'ACTIVO'
    "
      
      inmuebles_activos <- dbGetQuery(conn, query_completo)
      
      dbDisconnect(conn)
      
      if(nrow(inmuebles_activos) == 0) {
        showNotification("⚠️ No se encontraron inmuebles ACTIVOS", type = "warning")
      } else {
        showNotification(" Inmuebles cargados ", type = "message")
      }
      
      # Actualizar variables reactivas
      inmuebles_snowflake_df(inmuebles_activos)
      
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error en carga Snowflake:", e$message, "\n")
      if(exists("conn")) dbDisconnect(conn)
      return(FALSE)
    })
  }
  
  # FUNCIÓN CORREGIDA PARA ACTUALIZAR AGENDA EN SNOWFLAKE 
  actualizar_agenda_snowflake <- function() {
    tryCatch({
      # Obtener datos actuales de inmuebles
      datos_inmuebles_actual <- datos_clientes()
      
      # FILTRAR SOLO REGISTROS EDITADOS
      datos_editados <- datos_inmuebles_actual[datos_inmuebles_actual$EDITADO == TRUE, ]
      
      if(nrow(datos_editados) == 0) {
        showNotification("ℹ️ No hay registros editados para actualizar en INMUEBLES", type = "warning", duration = 3)
        return(FALSE)
      }
      
      datos_inmuebles_actual <- datos_editados
      
      # NUEVA LÍNEA: Crear conexión usando JWT
      conn <- crear_conexion_snowflake()
      
      # ACTIVAR WAREHOUSE
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
      
      # Leer tabla existente
      inmuebles_existentes <- tryCatch({
        dbReadTable(conn, "INMUEBLES")
      }, error = function(e) {
        data.frame(ID = character(0), stringsAsFactors = FALSE)
      })
      
      # CORREGIR: Usar ID en lugar de IDENTIFICADOR
      ids_existentes <- if(nrow(inmuebles_existentes) > 0 && "ID" %in% names(inmuebles_existentes)) {
        inmuebles_existentes$ID
      } else {
        character(0)
      }
      
      cat(" IDs existentes en Snowflake:", length(ids_existentes), "\n")
      
      # Filtrar solo los registros NUEVOS
      ids_locales <- datos_inmuebles_actual$ID
      registros_nuevos <- datos_inmuebles_actual[!ids_locales %in% ids_existentes, ]
      
      num_registros_a_insertar <- nrow(registros_nuevos)
      
      cat(" Registros locales:", nrow(datos_inmuebles_actual), "\n")
      cat(" Registros nuevos a insertar:", num_registros_a_insertar, "\n")
      
      if(num_registros_a_insertar == 0) {
        dbDisconnect(conn)
        showNotification("ℹ️ Todos los registros ya existen en Snowflake.", 
                         type = "message", duration = 4)
        inmuebles_snowflake_df(inmuebles_existentes)
        return(TRUE)
      }
      
      # PREPARAR DATOS NUEVOS CON TODAS LAS 14 COLUMNAS
      datos_nuevos_inmuebles <- data.frame(
        ID = registros_nuevos$ID,
        INMUEBLE = ifelse(registros_nuevos$INMUEBLE == "" | is.na(registros_nuevos$INMUEBLE), 
                          "-", registros_nuevos$INMUEBLE),
        EMPRESA = ifelse(registros_nuevos$EMPRESA == "" | is.na(registros_nuevos$EMPRESA), 
                         "-", registros_nuevos$EMPRESA),
        ALIAS = ifelse(registros_nuevos$ALIAS == "" | is.na(registros_nuevos$ALIAS), 
                       "-", registros_nuevos$ALIAS),
        DIRECCION = ifelse(registros_nuevos$DIRECCION == "" | is.na(registros_nuevos$DIRECCION), 
                           "-", registros_nuevos$DIRECCION),
        MUNICIPIO = ifelse(registros_nuevos$MUNICIPIO == "" | is.na(registros_nuevos$MUNICIPIO), 
                           "-", registros_nuevos$MUNICIPIO),
        ESTADO = ifelse(registros_nuevos$ESTADO == "" | is.na(registros_nuevos$ESTADO), 
                        "-", registros_nuevos$ESTADO),
        PAIS = ifelse(registros_nuevos$PAIS == "" | is.na(registros_nuevos$PAIS), 
                      "México", registros_nuevos$PAIS),
        FECHA_ESCRITURA = ifelse(is.na(registros_nuevos$FECHA_ESCRITURA), 
                                 "NULL", 
                                 paste0("'", registros_nuevos$FECHA_ESCRITURA, "'")),
        PRECIO_ADQUISICION = ifelse(is.na(registros_nuevos$PRECIO_ADQUISICION) | registros_nuevos$PRECIO_ADQUISICION == 0,
                                    0, registros_nuevos$PRECIO_ADQUISICION),
        TERRENO_M2 = ifelse(is.na(registros_nuevos$TERRENO_M2) | registros_nuevos$TERRENO_M2 == 0,
                            0, registros_nuevos$TERRENO_M2),
        PRECIO_TERRENO_M2 = ifelse(is.na(registros_nuevos$PRECIO_TERRENO_M2) | registros_nuevos$PRECIO_TERRENO_M2 == 0,
                                   0, registros_nuevos$PRECIO_TERRENO_M2),
        CONSTRUCCION_M2 = ifelse(is.na(registros_nuevos$CONSTRUCCION_M2) | registros_nuevos$CONSTRUCCION_M2 == 0,
                                 0, registros_nuevos$CONSTRUCCION_M2),
        PRECIO_CONSTRUCCION_M2 = ifelse(is.na(registros_nuevos$PRECIO_CONSTRUCCION_M2) | registros_nuevos$PRECIO_CONSTRUCCION_M2 == 0,
                                        0, registros_nuevos$PRECIO_CONSTRUCCION_M2),
        stringsAsFactors = FALSE
      )
      
      cat(" Columnas preparadas para insertar:", ncol(datos_nuevos_inmuebles), "\n")
      
      # Insertar TODOS los registros nuevos
      registros_insertados_exitosamente <- 0
      
      if(nrow(datos_nuevos_inmuebles) > 0) {
        tryCatch({
          # CONSTRUIR INSERT CON TODAS LAS 14 COLUMNAS
          for(i in 1:nrow(datos_nuevos_inmuebles)) {
            fila <- datos_nuevos_inmuebles[i, ]
            
            # Escapar comillas simples en campos de texto
            id_limpio <- gsub("'", "''", fila$ID)
            inmueble_limpio <- gsub("'", "''", fila$INMUEBLE)
            empresa_limpia <- gsub("'", "''", fila$EMPRESA)
            alias_limpio <- gsub("'", "''", fila$ALIAS)
            direccion_limpia <- gsub("'", "''", fila$DIRECCION)
            municipio_limpio <- gsub("'", "''", fila$MUNICIPIO)
            estado_limpio <- gsub("'", "''", fila$ESTADO)
            pais_limpio <- gsub("'", "''", fila$PAIS)
            
            insert_sql <- paste0(
              "INSERT INTO INMUEBLES (ID, INMUEBLE, EMPRESA, ALIAS, DIRECCION, MUNICIPIO, ESTADO, PAIS, ",
              "FECHA_ESCRITURA, PRECIO_ADQUISICION, TERRENO_M2, PRECIO_TERRENO_M2, CONSTRUCCION_M2, PRECIO_CONSTRUCCION_M2) VALUES (",
              "'", id_limpio, "', ",
              "'", inmueble_limpio, "', ",
              "'", empresa_limpia, "', ",
              "'", alias_limpio, "', ",
              "'", direccion_limpia, "', ",
              "'", municipio_limpio, "', ",
              "'", estado_limpio, "', ",
              "'", pais_limpio, "', ",
              fila$FECHA_ESCRITURA, ", ",  # Ya viene formateado o NULL
              fila$PRECIO_ADQUISICION, ", ",
              fila$TERRENO_M2, ", ",
              fila$PRECIO_TERRENO_M2, ", ",
              fila$CONSTRUCCION_M2, ", ",
              fila$PRECIO_CONSTRUCCION_M2,
              ")"
            )
            
            dbSendUpdate(conn, insert_sql)
            registros_insertados_exitosamente <- registros_insertados_exitosamente + 1
          }
          
        }, error = function(e) {
          cat("❌ Error durante la inserción:", e$message, "\n")
        })
      }
      
      cat(" Total de registros insertados:", registros_insertados_exitosamente, "\n")
      
      # Obtener datos completos actualizados
      inmuebles_completos_actualizados <- dbReadTable(conn, "INMUEBLES")
      
      # Cerrar conexión
      dbDisconnect(conn)
      
      # Actualizar visualización local
      inmuebles_snowflake_df(inmuebles_completos_actualizados)
      
      # ELIMINAR REGISTROS LOCALES QUE FUERON CARGADOS EXITOSAMENTE
      if(registros_insertados_exitosamente > 0) {
        ids_insertados <- datos_nuevos_inmuebles$ID[1:registros_insertados_exitosamente]
        
        datos_locales_actuales <- datos_clientes()
        datos_locales_filtrados <- datos_locales_actuales[!datos_locales_actuales$ID %in% ids_insertados, ]
        
        datos_clientes(datos_locales_filtrados)
        
        tryCatch({
          saveRDS(datos_locales_filtrados, "inmuebles_data.rds")
          cat("🗑️ Eliminados", length(ids_insertados), "registros locales después de cargar en Snowflake\n")
        }, error = function(e) {
          cat("⚠️ Error al eliminar registros locales:", e$message, "\n")
        })
        
        # Actualizar agenda local también
        if(nrow(datos_locales_filtrados) > 0) {
          agenda_local_actualizada <- data.frame(
            ID = datos_locales_filtrados$ID,
            EMPRESA = datos_locales_filtrados$EMPRESA,
            INMUEBLE = datos_locales_filtrados$INMUEBLE,
            stringsAsFactors = FALSE
          )
          agenda_df(agenda_local_actualizada)
        } else {
          agenda_df(data.frame(
            ID = character(0),
            EMPRESA = character(0),
            INMUEBLE = character(0),
            stringsAsFactors = FALSE
          ))
        }
        
        # Notificación de éxito
        total_final <- nrow(inmuebles_completos_actualizados)
        mensaje_exito <- paste("🎉 ¡INMUEBLES actualizados exitosamente!\n",
                               " Total en Snowflake:", total_final, "registros")
        
        showNotification(mensaje_exito, type = "message", duration = 3)
      }
      
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error al actualizar inmuebles en Snowflake:", e$message, "\n")
      showNotification(paste("Error al actualizar INMUEBLES:", e$message), type = "error", duration = 5)
      return(FALSE)
    })
  }
  
  # ============================================================================
  # OUTPUTS PARA FICHAS INFORMATIVAS
  # ============================================================================
  # Ficha 1: Total de Inmuebles ACTIVOS (Corregida para reaccionar a M2)
  output$total_inmuebles <- renderText({
    datos <- inmuebles_snowflake_df()
    if (is.null(datos) || nrow(datos) == 0) return("0")
    
    # Filtros categóricos
    if (!is.null(input$filtro_empresa) && length(input$filtro_empresa) > 0) {
      datos <- datos[datos$EMPRESA %in% input$filtro_empresa, ]
    }
    if (!is.null(input$filtro_estado) && length(input$filtro_estado) > 0) {
      datos <- datos[datos$ESTADO %in% input$filtro_estado, ]
    }
    if (!is.null(input$filtro_municipio) && length(input$filtro_municipio) > 0) {
      datos <- datos[datos$MUNICIPIO %in% input$filtro_municipio, ]
    }
    
    # Filtro de Terreno (Tolerante a NA)
    if (!is.null(input$filtro_terreno_m2)) {
      terreno_min <- input$filtro_terreno_m2[1]
      terreno_max <- input$filtro_terreno_m2[2]
      val_num <- suppressWarnings(as.numeric(datos$TERRENO_M2))
      
      # Esto evita que los inmuebles de CDMX sin metros se borren de la cuenta
      datos <- datos[is.na(val_num) | val_num == 0 | 
                       (val_num >= terreno_min & val_num <= terreno_max) |
                       (input$incluir_grandes & val_num > terreno_max), ]
    }
    
    format(nrow(datos), big.mark = ",")
  })
  
  # Ficha 2: Terreno M² Total (desde Snowflake - con o sin KMZ)
  output$total_terreno_m2 <- renderText({
    datos <- inmuebles_snowflake_df()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return("0 m²")
    }
    
    # Aplicar filtros de la UI
    datos_filtrados <- datos
    
    # FILTRO DE EMPRESA
    if (!is.null(input$filtro_empresa) && length(input$filtro_empresa) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$EMPRESA %in% input$filtro_empresa, ]
    }
    
    # FILTRO DE ESTADO
    if (!is.null(input$filtro_estado) && length(input$filtro_estado) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$ESTADO %in% input$filtro_estado, ]
    }
    
    # FILTRO DE MUNICIPIO
    if (!is.null(input$filtro_municipio) && length(input$filtro_municipio) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$MUNICIPIO %in% input$filtro_municipio, ]
    }
    
    # FILTRO DE TERRENO M² 
    if (!is.null(input$filtro_terreno_m2)) {
      tryCatch({
        terreno_min <- input$filtro_terreno_m2[1]
        terreno_max <- input$filtro_terreno_m2[2]
        
        datos_filtrados$TERRENO_M2_NUM <- suppressWarnings(as.numeric(datos_filtrados$TERRENO_M2))
        
        terrenos_validos <- datos_filtrados$TERRENO_M2_NUM[
          !is.na(datos_filtrados$TERRENO_M2_NUM) & 
            datos_filtrados$TERRENO_M2_NUM > 0 & 
            datos_filtrados$TERRENO_M2_NUM <= 500000
        ]
        
        if (length(terrenos_validos) > 0) {
          min_real <- 0
          max_real_p90 <- quantile(terrenos_validos, 0.90, na.rm = TRUE)
          max_real <- min(ceiling(max_real_p90 / 1000) * 1000, 50000)
          
          filtro_movido <- (terreno_min > min_real || terreno_max < max_real)
          
          if (filtro_movido) {
            if (!is.null(input$incluir_grandes) && input$incluir_grandes) {
              datos_filtrados <- datos_filtrados[
                !is.na(datos_filtrados$TERRENO_M2_NUM) & (
                  (datos_filtrados$TERRENO_M2_NUM >= terreno_min & datos_filtrados$TERRENO_M2_NUM <= terreno_max) |
                    datos_filtrados$TERRENO_M2_NUM > terreno_max
                ), 
              ]
            } else {
              datos_filtrados <- datos_filtrados[
                !is.na(datos_filtrados$TERRENO_M2_NUM) & 
                  datos_filtrados$TERRENO_M2_NUM >= terreno_min & 
                  datos_filtrados$TERRENO_M2_NUM <= terreno_max, 
              ]
            }
          }
        }
        
      }, error = function(e) {
        cat("⚠️ Error en filtro de terreno (Ficha 2):", e$message, "\n")
      })
    }
    
    if ("TERRENO_M2_NUM" %in% names(datos_filtrados)) {
      terreno_total <- sum(datos_filtrados$TERRENO_M2_NUM, na.rm = TRUE)
    } else {
      terreno_total <- sum(as.numeric(datos_filtrados$TERRENO_M2), na.rm = TRUE)
    }
    
    paste(format(round(terreno_total, 0), big.mark = ","), "m²")
  })
  
  # Ficha 3: Total de Empresas (desde Snowflake - con o sin KMZ)
  output$total_empresas <- renderText({
    datos <- inmuebles_snowflake_df()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return("0")
    }
    
    # Aplicar filtros de la UI
    datos_filtrados <- datos
    
    # FILTRO DE EMPRESA
    if (!is.null(input$filtro_empresa) && length(input$filtro_empresa) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$EMPRESA %in% input$filtro_empresa, ]
    }
    
    # FILTRO DE ESTADO
    if (!is.null(input$filtro_estado) && length(input$filtro_estado) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$ESTADO %in% input$filtro_estado, ]
    }
    
    # FILTRO DE MUNICIPIO
    if (!is.null(input$filtro_municipio) && length(input$filtro_municipio) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$MUNICIPIO %in% input$filtro_municipio, ]
    }
    
    # FILTRO DE TERRENO M² 
    if (!is.null(input$filtro_terreno_m2)) {
      tryCatch({
        terreno_min <- input$filtro_terreno_m2[1]
        terreno_max <- input$filtro_terreno_m2[2]
        
        datos_filtrados$TERRENO_M2_NUM <- suppressWarnings(as.numeric(datos_filtrados$TERRENO_M2))
        
        terrenos_validos <- datos_filtrados$TERRENO_M2_NUM[
          !is.na(datos_filtrados$TERRENO_M2_NUM) & 
            datos_filtrados$TERRENO_M2_NUM > 0 & 
            datos_filtrados$TERRENO_M2_NUM <= 500000
        ]
        
        if (length(terrenos_validos) > 0) {
          min_real <- 0
          max_real_p90 <- quantile(terrenos_validos, 0.90, na.rm = TRUE)
          max_real <- min(ceiling(max_real_p90 / 1000) * 1000, 50000)
          
          filtro_movido <- (terreno_min > min_real || terreno_max < max_real)
          
          if (filtro_movido) {
            if (!is.null(input$incluir_grandes) && input$incluir_grandes) {
              datos_filtrados <- datos_filtrados[
                !is.na(datos_filtrados$TERRENO_M2_NUM) & (
                  (datos_filtrados$TERRENO_M2_NUM >= terreno_min & datos_filtrados$TERRENO_M2_NUM <= terreno_max) |
                    datos_filtrados$TERRENO_M2_NUM > terreno_max
                ), 
              ]
            } else {
              datos_filtrados <- datos_filtrados[
                !is.na(datos_filtrados$TERRENO_M2_NUM) & 
                  datos_filtrados$TERRENO_M2_NUM >= terreno_min & 
                  datos_filtrados$TERRENO_M2_NUM <= terreno_max, 
              ]
            }
          }
        }
        
        datos_filtrados$TERRENO_M2_NUM <- NULL
        
      }, error = function(e) {
        cat("⚠️ Error en filtro de terreno (Ficha 3):", e$message, "\n")
      })
    }
    
    empresas_unicas <- length(unique(datos_filtrados$EMPRESA[!is.na(datos_filtrados$EMPRESA) & 
                                                               datos_filtrados$EMPRESA != "" & 
                                                               datos_filtrados$EMPRESA != "-"]))
    format(empresas_unicas, big.mark = ",")
  })
  
  # FICHA 4: VALOR TOTAL PORTAFOLIO (reactivo a filtros)
  output$total_valor_portafolio <- renderText({
    datos <- inmuebles_snowflake_df()
    if (is.null(datos) || nrow(datos) == 0) return("$0")
    
    # Aplicar los mismos filtros que las otras fichas
    if (!is.null(input$filtro_empresa) && length(input$filtro_empresa) > 0) {
      datos <- datos[datos$EMPRESA %in% input$filtro_empresa, ]
    }
    if (!is.null(input$filtro_estado) && length(input$filtro_estado) > 0) {
      datos <- datos[datos$ESTADO %in% input$filtro_estado, ]
    }
    if (!is.null(input$filtro_municipio) && length(input$filtro_municipio) > 0) {
      datos <- datos[datos$MUNICIPIO %in% input$filtro_municipio, ]
    }
    if (!is.null(input$filtro_terreno_m2)) {
      terreno_min <- input$filtro_terreno_m2[1]
      terreno_max <- input$filtro_terreno_m2[2]
      val_num <- suppressWarnings(as.numeric(datos$TERRENO_M2))
      datos <- datos[is.na(val_num) | val_num == 0 |
                       (val_num >= terreno_min & val_num <= terreno_max) |
                       (input$incluir_grandes & val_num > terreno_max), ]
    }
    
    tryCatch({
      total <- sum(suppressWarnings(as.numeric(datos$PRECIO_ADQUISICION)), na.rm = TRUE)
      
      if (total >= 1e9) {
        paste0("$", format(round(total / 1e9, 2), big.mark = ","), "B")
      } else if (total >= 1e6) {
        paste0("$", format(round(total / 1e6, 2), big.mark = ","), "M")
      } else {
        paste0("$", format(round(total, 0), big.mark = ","))
      }
    }, error = function(e) "$0")
  })
  
  # ============================================================================
  # DATOS FILTRADOS PARA MAPA
  # ============================================================================
  
  # Variable reactiva con datos filtrados
  datos_mapa_inmuebles_filtrados <- reactive({
    datos <- datos_mapa_inmuebles()
    
    if (is.null(datos) || nrow(datos) == 0) return(NULL)
    
    # Verificar si es sf
    es_sf <- inherits(datos, "sf")
    
    # APLICAR FILTROS DIRECTAMENTE EN EL OBJETO SF (SIN QUITAR GEOMETRÍA)
    datos_filtrados <- datos
    
    # FILTRO DE EMPRESA
    if (!is.null(input$filtro_empresa) && length(input$filtro_empresa) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$EMPRESA %in% input$filtro_empresa, ]
    }
    
    # FILTRO DE ESTADO
    if (!is.null(input$filtro_estado) && length(input$filtro_estado) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$ESTADO %in% input$filtro_estado, ]
    }
    
    # FILTRO DE MUNICIPIO
    if (!is.null(input$filtro_municipio) && length(input$filtro_municipio) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$MUNICIPIO %in% input$filtro_municipio, ]
    }
    
    # FILTRO DE INMUEBLE
    if (!is.null(input$filtro_inmueble) && length(input$filtro_inmueble) > 0) {
      datos_filtrados <- datos_filtrados[datos_filtrados$INMUEBLE %in% input$filtro_inmueble, ]
    }
    
    # FILTRO 4: TERRENO M² - SOLO SI SE MOVIÓ DEL RANGO COMPLETO
    if (!is.null(input$filtro_terreno_m2)) {
      terreno_min <- input$filtro_terreno_m2[1]
      terreno_max <- input$filtro_terreno_m2[2]
      terrenos_numericos <- suppressWarnings(as.numeric(datos_filtrados$TERRENO_M2))
      
      # Obtener el rango completo de los datos
      terrenos_validos <- datos_filtrados$TERRENO_M2[
        !is.na(datos_filtrados$TERRENO_M2) & 
          datos_filtrados$TERRENO_M2 > 0 & 
          datos_filtrados$TERRENO_M2 <= 500000
      ]
      
      if (length(terrenos_validos) > 0) {
        min_real <- 0
        max_real_p90 <- quantile(terrenos_validos, 0.90, na.rm = TRUE)
        max_real <- min(ceiling(max_real_p90 / 1000) * 1000, 50000)
        
        # SOLO APLICAR FILTRO SI EL USUARIO MOVIÓ EL SLIDER
        filtro_movido <- (terreno_min > min_real || terreno_max < max_real)
        
        if (filtro_movido) {
          datos_filtrados$TERRENO_M2_NUM <- suppressWarnings(as.numeric(datos_filtrados$TERRENO_M2))
          antes <- nrow(datos_filtrados)
          
          if (!is.null(input$incluir_grandes) && input$incluir_grandes) {
            # Incluir inmuebles grandes (> terreno_max)
            datos_filtrados <- datos_filtrados[
              !is.na(datos_filtrados$TERRENO_M2_NUM) & (
                (datos_filtrados$TERRENO_M2_NUM >= terreno_min & datos_filtrados$TERRENO_M2_NUM <= terreno_max) |
                  datos_filtrados$TERRENO_M2_NUM > terreno_max
              ), 
            ]
            cat(" Filtro TERRENO (+ grandes) aplicado:", antes, "→", nrow(datos_filtrados), "\n")
          } else {
            # Solo rango estricto
            datos_filtrados <- datos_filtrados[
              !is.na(datos_filtrados$TERRENO_M2_NUM) & 
                datos_filtrados$TERRENO_M2_NUM >= terreno_min & 
                datos_filtrados$TERRENO_M2_NUM <= terreno_max, 
            ]
            cat(" Filtro TERRENO (estricto) aplicado:", antes, "→", nrow(datos_filtrados), "\n")
          }
          
          datos_filtrados$TERRENO_M2_NUM <- NULL
        } else {
          cat("ℹ️ Slider en rango completo - NO se aplica filtro de terreno\n")
        }
      }
    }
    
    return(datos_filtrados)
  })
  
  # ============================================================================
  # DATOS FILTRADOS PARA FICHAS INFORMATIVAS (SNOWFLAKE)
  # ============================================================================
  
  datos_snowflake_filtrados <- reactive({
    datos <- inmuebles_snowflake_df()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(NULL)
    }
    
    # FILTRO DE EMPRESA
    if (!is.null(input$filtro_empresa) && length(input$filtro_empresa) > 0) {
      datos <- datos[datos$EMPRESA %in% input$filtro_empresa, ]
    }
    
    # FILTRO DE ESTADO
    if (!is.null(input$filtro_estado) && length(input$filtro_estado) > 0) {
      datos <- datos[datos$ESTADO %in% input$filtro_estado, ]
    }
    
    # FILTRO DE MUNICIPIO
    if (!is.null(input$filtro_municipio) && length(input$filtro_municipio) > 0) {
      datos <- datos[datos$MUNICIPIO %in% input$filtro_municipio, ]
    }
    
    return(datos)
  })
  
  # ============================================================================
  # DATOS FILTRADOS PARA CSV - INDEPENDIENTE DEL MAPA
  # ============================================================================
  datos_excel_filtrados <- reactive({
    datos <- inmuebles_snowflake_df()
    
    if (is.null(datos) || nrow(datos) == 0) {
      cat("⚠️ No hay datos en Snowflake para CSV\n")
      return(NULL)
    }
    
    cat(" Datos iniciales para CSV:", nrow(datos), "inmuebles\n")
    
    # Iniciar con todos los datos
    datos_filtrados <- datos
    
    # ============================================================================
    # APLICAR FILTROS DE LA UI
    # ============================================================================
    
    # FILTRO 1: EMPRESA
    if (!is.null(input$filtro_empresa) && length(input$filtro_empresa) > 0) {
      antes <- nrow(datos_filtrados)
      datos_filtrados <- datos_filtrados[datos_filtrados$EMPRESA %in% input$filtro_empresa, ]
      cat(" Filtro EMPRESA aplicado:", antes, "→", nrow(datos_filtrados), "\n")
    }
    
    # FILTRO 2: ESTADO
    if (!is.null(input$filtro_estado) && length(input$filtro_estado) > 0) {
      antes <- nrow(datos_filtrados)
      datos_filtrados <- datos_filtrados[datos_filtrados$ESTADO %in% input$filtro_estado, ]
      cat("️ Filtro ESTADO aplicado:", antes, "→", nrow(datos_filtrados), "\n")
    }
    
    # FILTRO 3: MUNICIPIO
    if (!is.null(input$filtro_municipio) && length(input$filtro_municipio) > 0) {
      antes <- nrow(datos_filtrados)
      datos_filtrados <- datos_filtrados[datos_filtrados$MUNICIPIO %in% input$filtro_municipio, ]
      cat(" Filtro MUNICIPIO aplicado:", antes, "→", nrow(datos_filtrados), "\n")
    }
    
    # FILTRO 4: INMUEBLE
    if (!is.null(input$filtro_inmueble) && length(input$filtro_inmueble) > 0) {
      antes <- nrow(datos_filtrados)
      datos_filtrados <- datos_filtrados[datos_filtrados$INMUEBLE %in% input$filtro_inmueble, ]
      cat(" Filtro INMUEBLE aplicado:", antes, "→", nrow(datos_filtrados), "\n")
    }
    
    cat("✅ Total FINAL para CSV:", nrow(datos_filtrados), "inmuebles\n")
    
    return(datos_filtrados)
  })
  
  # ============================================================================
  # ACTUALIZAR OPCIONES DE FILTROS DINÁMICAMENTE
  # ============================================================================
  
  # POBLAR FILTROS AL CARGAR (solo cuando llegan los datos por primera vez)
  observeEvent(datos_mapa_inmuebles(), {
    datos <- datos_mapa_inmuebles()
    if (is.null(datos) || nrow(datos) == 0) return()
    if (inherits(datos, "sf")) datos <- st_drop_geometry(datos)
    
    empresas <- sort(unique(datos$EMPRESA[!is.na(datos$EMPRESA) & datos$EMPRESA != "" & datos$EMPRESA != "-"]))
    updateSelectInput(session, "filtro_empresa", choices = c("Todas" = "", empresas), selected = "")
    
    estados <- sort(unique(datos$ESTADO[!is.na(datos$ESTADO) & datos$ESTADO != "" & datos$ESTADO != "-"]))
    updateSelectInput(session, "filtro_estado", choices = c("Todos" = "", estados), selected = "")
    
    municipios <- sort(unique(datos$MUNICIPIO[!is.na(datos$MUNICIPIO) & datos$MUNICIPIO != "" & datos$MUNICIPIO != "-"]))
    updateSelectInput(session, "filtro_municipio", choices = c("Todos" = "", municipios), selected = "")
    
    inmuebles_disp <- sort(unique(datos$INMUEBLE[!is.na(datos$INMUEBLE) & datos$INMUEBLE != "" & datos$INMUEBLE != "-"]))
    updateSelectInput(session, "filtro_inmueble", choices = c("Todos" = "", inmuebles_disp), selected = "")
    
    terrenos_validos <- datos$TERRENO_M2[!is.na(datos$TERRENO_M2) & datos$TERRENO_M2 > 0 & datos$TERRENO_M2 <= 500000]
    if (length(terrenos_validos) > 0) {
      max_terreno_p90 <- quantile(terrenos_validos, 0.90, na.rm = TRUE)
      max_terreno <- min(ceiling(max_terreno_p90 / 1000) * 1000, 50000)
      updateSliderInput(session, "filtro_terreno_m2", min = 0, max = max_terreno, value = c(0, max_terreno), step = 50)
    }
  }, ignoreNULL = TRUE)
  
  # CASCADA CON BLOQUEO DE UI
  observeEvent(
    list(input$filtro_empresa, input$filtro_estado, input$filtro_municipio),
    {
      # 1. BLOQUEAR panel inmediatamente
      shinyjs::addClass("panel_filtros", "filtros-bloqueados")
      shinyjs::disable("filtro_empresa")
      shinyjs::disable("filtro_estado")
      shinyjs::disable("filtro_municipio")
      shinyjs::disable("filtro_inmueble")
      
      # 2. Asegura que el bloqueo se renderice ANTES de procesar
      shinyjs::delay(50, {
        
        datos <- isolate(datos_mapa_inmuebles())
        
        if (!is.null(datos) && nrow(datos) > 0) {
          if (inherits(datos, "sf")) datos <- st_drop_geometry(datos)
          
          emp_actual  <- isolate(input$filtro_empresa)
          est_actual  <- isolate(input$filtro_estado)
          muni_actual <- isolate(input$filtro_municipio)
          inm_actual  <- isolate(input$filtro_inmueble)
          
          # ESTADO
          datos_x_empresa <- datos
          if (!is.null(emp_actual) && length(emp_actual) > 0) {
            datos_x_empresa <- datos_x_empresa[datos_x_empresa$EMPRESA %in% emp_actual, ]
          }
          estados <- sort(unique(datos_x_empresa$ESTADO[!is.na(datos_x_empresa$ESTADO) & datos_x_empresa$ESTADO != "" & datos_x_empresa$ESTADO != "-"]))
          est_valido <- est_actual[est_actual %in% estados]
          updateSelectInput(session, "filtro_estado", choices = c("Todos" = "", estados), selected = est_valido)
          
          # MUNICIPIO
          datos_x_estado <- datos_x_empresa
          if (!is.null(est_actual) && length(est_actual) > 0) {
            datos_x_estado <- datos_x_estado[datos_x_estado$ESTADO %in% est_actual, ]
          }
          municipios <- sort(unique(datos_x_estado$MUNICIPIO[!is.na(datos_x_estado$MUNICIPIO) & datos_x_estado$MUNICIPIO != "" & datos_x_estado$MUNICIPIO != "-"]))
          muni_valido <- muni_actual[muni_actual %in% municipios]
          updateSelectInput(session, "filtro_municipio", choices = c("Todos" = "", municipios), selected = muni_valido)
          
          # INMUEBLE
          datos_x_municipio <- datos_x_estado
          if (!is.null(muni_actual) && length(muni_actual) > 0) {
            datos_x_municipio <- datos_x_municipio[datos_x_municipio$MUNICIPIO %in% muni_actual, ]
          }
          inmuebles_disp <- sort(unique(datos_x_municipio$INMUEBLE[!is.na(datos_x_municipio$INMUEBLE) & datos_x_municipio$INMUEBLE != "" & datos_x_municipio$INMUEBLE != "-"]))
          inm_valido <- inm_actual[inm_actual %in% inmuebles_disp]
          updateSelectInput(session, "filtro_inmueble", choices = c("Todos" = "", inmuebles_disp), selected = inm_valido)
        }
        
        # 3. DESBLOQUEAR cuando termina
        shinyjs::removeClass("panel_filtros", "filtros-bloqueados")
        shinyjs::enable("filtro_empresa")
        shinyjs::enable("filtro_estado")
        shinyjs::enable("filtro_municipio")
        shinyjs::enable("filtro_inmueble")
      })
    },
    ignoreInit = TRUE
  )
   
  # ============================================================================
  # ACTUALIZAR CATÁLOGOS DE ID Y EMPRESA EN PESTAÑA REGISTRAR INMUEBLE
  # ============================================================================
  
  observe({
    datos_sf <- inmuebles_snowflake_df()
    
    if (!is.null(datos_sf) && nrow(datos_sf) > 0) {
      ids_unicos <- sort(unique(datos_sf$ID[!is.na(datos_sf$ID) & 
                                              datos_sf$ID != "" & 
                                              datos_sf$ID != "-"]))
      
      updateSelectizeInput(session, "id_inmueble",
                           choices = c("", ids_unicos),
                           selected = "",
                           server = TRUE)
      
      empresas_unicas <- sort(unique(datos_sf$EMPRESA[!is.na(datos_sf$EMPRESA) & 
                                                        datos_sf$EMPRESA != "" & 
                                                        datos_sf$EMPRESA != "-"]))
      
      updateSelectizeInput(session, "empresa",
                           choices = c("", empresas_unicas),
                           selected = "",
                           server = TRUE)
      
      inmuebles_unicos <- sort(unique(datos_sf$INMUEBLE[!is.na(datos_sf$INMUEBLE) &
                                                          datos_sf$INMUEBLE != "" &
                                                          datos_sf$INMUEBLE != "-"]))
      
      inmuebles_unicos <- sort(unique(datos_sf$INMUEBLE[!is.na(datos_sf$INMUEBLE) & 
                                                          datos_sf$INMUEBLE != "" & 
                                                          datos_sf$INMUEBLE != "-"]))
      
      updateSelectizeInput(session, "nombre_inmueble",
                           choices = c("", inmuebles_unicos),
                           selected = "",
                           server = TRUE)
      
      cat(" Catálogos actualizados:", length(ids_unicos), "IDs,", 
          length(empresas_unicas), "empresas y",
          length(inmuebles_unicos), "inmuebles\n")
    }
  })
  
  # Botón para limpiar filtros
  observeEvent(input$limpiar_filtros, {
    
    # LIMPIAR SELECCIÓN
    inmueble_seleccionado_mapa(NULL)
    
    # RESETEAR FILTROS
    updateSelectInput(session, "filtro_empresa", selected = "")
    updateSelectInput(session, "filtro_estado", selected = "")
    updateSelectInput(session, "filtro_municipio", selected = "")
    updateSelectInput(session, "filtro_inmueble", selected = "")
    
    # RESETEAR CHECKBOX
    updateCheckboxInput(session, "incluir_grandes", value = TRUE)
    
    # RESETEAR SLIDER
    datos <- datos_mapa_inmuebles()
    if (!is.null(datos) && nrow(datos) > 0) {
      if (inherits(datos, "sf")) {
        datos <- st_drop_geometry(datos)
      }
      
      terrenos_validos <- datos$TERRENO_M2[
        !is.na(datos$TERRENO_M2) & 
          datos$TERRENO_M2 > 0 & 
          datos$TERRENO_M2 <= 500000
      ]
      
      if(length(terrenos_validos) > 0) {
        min_terreno <- 0  
        max_terreno <- ceiling(max(terrenos_validos) / 1000) * 1000
        
        updateSliderInput(session, "filtro_terreno_m2",
                          value = c(min_terreno, max_terreno))
      }
    }
    
    shinyjs::runjs("
    setTimeout(function() {
      var mapContainer = document.getElementById('mapa_inmuebles');
      if (mapContainer) {
        var clickEvent = new MouseEvent('click', {
          bubbles: true,
          cancelable: true,
          view: window
        });
        mapContainer.dispatchEvent(clickEvent);
      }
    }, 100);
  ")
    
    # Y también el método normal
    leafletProxy("mapa_inmuebles") %>%
      clearPopups() %>%
      setView(lng = -100.3, lat = 25.7, zoom = 6)
    
    showNotification(" Filtros limpiados", type = "message", duration = 1)
  })
  
  # Confirmar limpieza total
  observeEvent(input$confirmar_limpiar_todos, {
    datos_vacios <- crear_estructura_vacia()
    datos_clientes(datos_vacios)
    
    # Limpiar agenda también
    agenda_df(data.frame(
      ID = character(0),
      EMPRESA = character(0),
      INMUEBLE = character(0),
      stringsAsFactors = FALSE
    ))
    
    tryCatch({
      saveRDS(datos_vacios, "inmuebles_data.rds")
      showNotification("Todos los registros han sido eliminados", type = "error", duration = 5)
    }, error = function(e) {
      showNotification(paste("Error al limpiar registros:", e$message), type = "error", duration = 5)
    })
    
    removeModal()
  })
  
  output$tabla_inmuebles_snowflake <- DT::renderDataTable({
    datos <- datos_mapa_inmuebles() 
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay datos disponibles"))
    }
    
    datos_mostrar <- datos
    
    # Limpiar columnas
    for(col in names(datos_mostrar)) {
      datos_mostrar[[col]] <- as.character(datos_mostrar[[col]])
      datos_mostrar[[col]][is.na(datos_mostrar[[col]]) | datos_mostrar[[col]] == ""] <- "-"
    }
    
    datos_mostrar$`Google Maps` <- sprintf(
      '<a href="https://www.google.com/maps?q=%s,%s" target="_blank" class="btn btn-primary btn-sm">Ver</a>',
      datos_mostrar$LATITUD,
      datos_mostrar$LONGITUD
    )
    
    DT::datatable(
      datos_mostrar,
      escape = FALSE,
      options = list(
        paging = FALSE,           
        scrollX = TRUE,
        scrollY = "65vh",       
        searching = TRUE,
        ordering = TRUE,
        
        language = list(
          zeroRecords = "No se encontraron inmuebles",
          info = "Mostrando _TOTAL_ inmuebles", 
          search = "Buscar:"
        )
      ),
      
      rownames = FALSE,
      class = 'table table-striped'
    )
  })
  
  # Tabla principal de registros - SOLO SI ESTÁ AUTENTICADO
  output$tabla_registro_tiempo_real <- DT::renderDataTable({
    
    datos <- datos_clientes()
    
    if (nrow(datos) == 0) {
      return(data.frame(Mensaje = "Aún no hay inmuebles registrados"))
    }
    
    datos_mostrar <- datos[, c(
      "ID", "INMUEBLE", "EMPRESA", "MUNICIPIO", "ESTADO", "PRECIO_ADQUISICION"
    )]
    
    datos_mostrar$EMPRESA[datos_mostrar$EMPRESA == ""] <- "-"
    datos_mostrar$MUNICIPIO[datos_mostrar$MUNICIPIO == ""] <- "-"
    datos_mostrar$ESTADO[datos_mostrar$ESTADO == ""] <- "-"
    
    datos_mostrar$PRECIO_ADQUISICION <- ifelse(
      datos_mostrar$PRECIO_ADQUISICION == 0,
      "-",
      paste0(
        "$",
        format(
          datos_mostrar$PRECIO_ADQUISICION,
          big.mark = ",",
          scientific = FALSE
        )
      )
    )
    
    datos_mostrar$Estado <- ifelse(
      datos$EDITADO,
      '<span style="color:#802e25; font-weight: bold; font-size: 13px;">✅ Listo</span>',
      '<span style="color:#6c757d; font-size: 13px;">⏳ Sin editar</span>'
    )
    
    datos_mostrar$Acciones <- paste0(
      '<div style="display:flex; gap:3px; justify-content:center;">',
      
      ifelse(
        !datos$EDITADO,
        paste0(
          '<button class="btn btn-info btn-xs" onclick="Shiny.setInputValue(\'marcar_editado\', \'',
          datos$ID,
          '\', {priority:\'event\'});"><i class="fa fa-check"></i> Listo</button>'
        ),
        '<span style="color:#802e25; font-size:11px; font-weight:bold;">✅ Listo</span>'
      ),
      
      paste0(
        '<button class="btn btn-primary btn-xs" onclick="Shiny.setInputValue(\'editar_registro\', \'',
        datos$ID,
        '\', {priority:\'event\'});"><i class="fa fa-edit"></i> Editar</button>'
      ),
      
      paste0(
        '<button class="btn btn-success btn-xs" onclick="Shiny.setInputValue(\'actualizar_agenda_individual\', \'',
        datos$ID,
        '\', {priority:\'event\'});"><i class="fa fa-sync"></i> Actualizar</button>'
      ),
      
      paste0(
        '<button class="btn btn-danger btn-xs" onclick="Shiny.setInputValue(\'eliminar_registro\', \'',
        datos$ID,
        '\', {priority:\'event\'});"><i class="fa fa-trash"></i> Eliminar</button>'
      ),
      
      '</div>'
    )
    
    DT::datatable(
      datos_mostrar,
      options = list(
        pageLength = 15,
        lengthMenu = list(
          c(10, 15, 25, 50, 100, -1),
          c("10", "15", "25", "50", "100", "Todos")
        ),
        dom = 'lfrtip',
        scrollX = TRUE,
        responsive = TRUE,
        searching = TRUE,
        ordering = TRUE,
        order = list(list(0, "asc")),
        
        columnDefs = list(
          list(targets = 0, width = "100px", className = "text-center font-weight-bold"),
          list(targets = 1, width = "150px"),
          list(targets = 2, width = "120px"),
          list(targets = 3, width = "100px"),
          list(targets = 4, width = "200px"),
          list(targets = 5, width = "120px"),
          list(targets = 6, width = "100px"),
          list(targets = 7, width = "80px"),
          list(targets = 8, width = "120px", className = "text-center"),
          list(targets = c(9, 10, 11, 12, 13), className = "text-right"),
          list(targets = 14, width = "220px", className = "text-center", orderable = FALSE),
          list(targets = 15, width = "150px", className = "text-center"),
          list(targets = 16, width = "100px", className = "text-center")
        ),
        
        language = list(
          lengthMenu = "Mostrar _MENU_ registros por pagina",
          zeroRecords = "No se encontraron registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "No hay registros disponibles",
          infoFiltered = "(filtrado de _MAX_ registros totales)",
          search = "Buscar:",
          paginate = list(
            first = "Primero",
            last = "Ultimo",
            `next` = "Siguiente",
            previous = "Anterior"
          )
        )
      ),
      
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      class = "table table-striped table-hover"
    )
  })
  
  # Contador - SOLO SI ESTÁ AUTENTICADO
  output$contador_hoy <- renderText({
    
    datos <- datos_clientes()
    total <- nrow(datos) 
    
    if(total == 0) {
      "No hay inmuebles registrados aún"
    } else if (total == 1 ) {
      "1 inmueble registrado"
    } else {
      paste(total, "inmuebles registrados")
    }
  })
  
  # ============================================================================
  # MODAL DE CONFIRMACIÓN PARA DESCARGA DE EXCEL
  # ============================================================================
  observeEvent(input$descargar_excel_mi, {
    showModal(modalDialog(
      title = div(
        style = "color: #ff5b49; font-weight: bold; font-size: 20px;",
        icon("exclamation-triangle"),
        " Confirmar Generación de Excel"
      ),
      
      div(
        style = "padding: 15px; text-align: center;",
        p(
          style = "color: #555; font-size: 15px; line-height: 1.6;",
          " ¿Desea confirmar la generación del archivo?.",
        )
      ),
      
      footer = tagList(
        tags$button(
          type = "button",
          class = "btn",
          `data-dismiss` = "modal",
          style = "background: #95a5a6; color: white; border: none; 
            padding: 10px 25px; border-radius: 6px; font-size: 14px;",
          "Cancelar"
        ),
        actionButton("confirmar_generacion_excel", 
                     icon("check"), " Sí, Generar Excel",
                     class = "btn",
                     style = "background: linear-gradient(135deg, #27ae60 0%, #229954 100%);
                            color: white; border: none; font-weight: bold; 
                            padding: 10px 25px; border-radius: 6px;
                            box-shadow: 0 4px 12px rgba(39, 174, 96, 0.4);
                            font-size: 14px;")
      ),
      
      easyClose = FALSE,
      size = "m"
    ))
  })
  
  # ============================================================================
  # GENERAR EL ARCHIVO EXCEL
  # ============================================================================
  observeEvent(input$confirmar_generacion_excel, {
    removeModal()
    
    showModal(modalDialog(
      title = div(
        style = "color: #ff5b49; font-weight: bold; font-size: 18px;",
        icon("file-excel"),
        " Generando Excel de Inmuebles"
      ),
      div(
        style = "padding: 20px;",
        
        div(
          id = "progress_container_excel",
          style = "margin-bottom: 20px;",
          tags$div(
            class = "progress",
            style = "height: 30px; background: #f0f0f0;",
            tags$div(
              id = "progress_bar_excel",
              class = "progress-bar progress-bar-striped active",
              role = "progressbar",
              style = "width: 0%; background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%); 
                 transition: width 0.3s ease;",
              "0%"
            )
          )
        ),
        
        div(
          id = "progress_message_excel",
          style = "text-align: center; color: #666; font-size: 14px; margin-top: 10px;",
          "Iniciando proceso..."
        ),
      ),
      
      footer = NULL,
      easyClose = FALSE,
      size = "m"
    ))
    
    actualizar_progreso <- function(valor, detalle) {
      porcentaje <- round(valor * 100)
      session$sendCustomMessage("updateProgressExcel", list(
        progress = porcentaje,
        message = detalle
      ))
    }
    
    tryCatch({
      # Obtener IDs filtrados del mapa
      datos_filtrados_excel <- datos_excel_filtrados()
      ids_filtrados <- if (!is.null(datos_filtrados_excel) && nrow(datos_filtrados_excel) > 0) {
        datos_filtrados_excel$ID
      } else {
        NULL
      }
      
      archivo_binario <- crear_excel_mi(progress_callback = actualizar_progreso, 
                                        ids_filtrar = ids_filtrados)
      
      if (is.null(archivo_binario)) {
        removeModal()
        showNotification("❌ No se pudo generar el archivo Excel", type = "error", duration = 4)
        return()
      }
      
      archivo_excel_generado(archivo_binario)
      removeModal()
      
      showModal(modalDialog(
        title = div(
          style = "color: #27ae60; font-weight: bold; font-size: 18px;",
          icon("check-circle"),
          " Excel Generado Exitosamente"
        ),
        
        div(
          style = "padding: 20px; text-align: center;",
          p(
            style = "color: #555; font-size: 15px; margin-bottom: 25px;",
            "El archivo Excel se ha generado correctamente.",
            br(),
            "Haga clic en el botón para descargarlo."
          ),
          
          downloadButton("ejecutar_descarga_excel",
                         label = "Descargar Excel",  
                         class = "btn",
                         style = "background: linear-gradient(135deg, #ff5b49 0%, #ff5b49 100%);
                        color: white; border: none; font-weight: bold; 
                        padding: 12px 30px; border-radius: 6px;
                        box-shadow: 0 4px 12px rgba(230, 126, 34, 0.4);
                        font-size: 15px;")
        ),
        
        footer = tags$button(
          type = "button",
          class = "btn",
          `data-dismiss` = "modal",
          style = "background: #95a5a6; color: white; border: none; 
                  padding: 10px 25px; border-radius: 6px;",
          "Cerrar"
        ),
        easyClose = TRUE,
        size = "m"
      ))
      
    }, error = function(e) {
      removeModal()
      cat("❌ Error al generar Excel:", e$message, "\n")
      showNotification(paste("❌ Error:", e$message), type = "error", duration = 5)
    })
  })
  
  # ============================================================================
  # DESCARGAR EL ARCHIVO YA GENERADO
  # ============================================================================
  output$ejecutar_descarga_excel <- downloadHandler(
    filename = function() {
      fecha_actual <- format(Sys.Date(), "%Y%m%d")
      paste0("ALMENA_Sistema_Inmuebles_", fecha_actual, ".xlsx")
    },
    
    content = function(file) {
      archivo <- archivo_excel_generado()
      
      if (is.null(archivo)) {
        showNotification("❌ Error: No hay archivo disponible", type = "error", duration = 4)
        return()
      }
      
      writeBin(archivo, file)
      removeModal()
      showNotification("✅ Excel descargado exitosamente", type = "message", duration = 3)
    },
    
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # ============================================================================
  # OUTPUTS PARA MAPA DE INMUEBLES
  # ============================================================================
  
  # Contador de inmuebles para el mapa
  output$contador_inmuebles_mapa <- renderText({
    datos <- datos_mapa_inmuebles()
    
    if (is.null(datos) || nrow(datos) == 0) {
      "📍 No hay inmuebles con ubicación geográfica disponible"
    } else {
      total <- nrow(datos)
      paste("📍", total, "inmuebles con ubicación registrada")
    }
  })
  
  # Renderizar mapa con polígonos
  output$mapa_inmuebles <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -100.3, lat = 25.7, zoom = 6)  # Centro de México
  })
  
  observeEvent(datos_mapa_inmuebles_filtrados(), {
    datos <- datos_mapa_inmuebles_filtrados()
    
    if (is.null(datos) || nrow(datos) == 0) {
      leafletProxy("mapa_inmuebles") %>%
        clearShapes() %>%
        clearMarkers()
      return()
    }
    
    cat("🗺️ Actualizando mapa con", nrow(datos), "polígonos\n")
    
    # Verificar que tiene geometría válida
    if (!inherits(datos, "sf")) {
      cat("⚠️ Los datos NO son SF, no se pueden dibujar polígonos\n")
      return()
    }
    
    # Función auxiliar para valor seguro
    safe_val <- function(x, default = "-") {
      ifelse(is.na(x) | x == "", default, x)
    }
    
    safe_num <- function(x) {
      val <- suppressWarnings(as.numeric(x))
      ifelse(is.na(val) | val == 0, "-", format(round(val, 0), big.mark = ","))
    }
    
    # Crear popups informativos
    popup_html <- sapply(1:nrow(datos), function(i) {
      
      # Calcular precio actualizado por INPC
      precio_raw   <- suppressWarnings(as.numeric(datos$PRECIO_ADQUISICION[i]))
      fecha_raw    <- if("FECHA_ESCRITURA" %in% names(datos)) datos$FECHA_ESCRITURA[i] else NA
      # Cache global para los datos del INPC (se descarga una sola vez por sesión)
      inpc_result  <- obtener_valor_inflacionado(precio_raw, fecha_raw)
      
      bloque_precio <- if ("PRECIO_ADQUISICION" %in% names(datos)) {
        if (!is.null(inpc_result)) {
          paste0(
            "<div style='background:#f9f9f9; border-left:3px solid #ff5b49; ",
            "padding:6px 8px; margin:6px 0; border-radius:3px;'>",
            "<b> Precio Adquisición (INPC ", inpc_result$fecha_inpc, "):</b><br>",
            "<span style='font-size:13px; color:#802e25; font-weight:bold;'>",
            "$", format(inpc_result$valor, big.mark = ","), " MXN</span>",
            " <span style='font-size:10px; color:#888;'>(+", inpc_result$variacion_pct, "% inflación)</span>",
            "<br><span style='font-size:10px; color:#aaa;'>Original: $", 
            safe_num(datos$PRECIO_ADQUISICION[i]), " MXN</span>",
            "</div>"
          )
        } else {
          paste0("<b> Precio Adquisición:</b> $", safe_num(datos$PRECIO_ADQUISICION[i]), "<br>")
        }
      } else ""
      
      sprintf(
        "<div style='font-family:Arial; font-size:12px; min-width:260px;'>
          <h4 style='color:#ff5b49; margin:0 0 10px 0;'>%s</h4>
          %s
          %s
          %s
          %s
          <b>Terreno:</b> %s m²<br>
          <b>Construcción:</b> %s m²<br>
          %s
        </div>",
        safe_val(if("INMUEBLE" %in% names(datos)) datos$INMUEBLE[i] else datos$ID_INMUEBLE[i]),
        if("ID_INMUEBLE" %in% names(datos)) paste0("<b> ID:</b> ", safe_val(datos$ID_INMUEBLE[i]), "<br>") else "",
        if("EMPRESA" %in% names(datos)) paste0("<b> Empresa:</b> ", safe_val(datos$EMPRESA[i]), "<br>") else "",
        if("ESTADO" %in% names(datos)) paste0("<b> Estado:</b> ", safe_val(datos$ESTADO[i]), "<br>") else "",
        if("MUNICIPIO" %in% names(datos)) paste0("<b>️ Municipio:</b> ", safe_val(datos$MUNICIPIO[i]), "<br>") else "",
        safe_num(datos$TERRENO_M2[i]),
        safe_num(datos$CONSTRUCCION_M2[i]),
        bloque_precio
      )
    })
    
    # Calcular bounds válidos
    lng_vals <- datos$LONGITUD[!is.na(datos$LONGITUD)]
    lat_vals <- datos$LATITUD[!is.na(datos$LATITUD)]
    
    # Actualizar mapa
    map_proxy <- leafletProxy("mapa_inmuebles", data = datos) %>%
      clearShapes() %>%
      clearMarkers() %>%
      addPolygons(
        fillColor = "#ff5b49",
        fillOpacity = 0.5,
        color = "#ff5b49",
        weight = 2,
        popup = popup_html,
        label = ~INMUEBLE,
        layerId = ~ID_INMUEBLE,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#802e25",
          fillOpacity = 0.7,
          bringToFront = TRUE
        )
      )
    
    # Agregar marcadores solo si hay coordenadas válidas
    if (length(lng_vals) > 0 && length(lat_vals) > 0) {
      map_proxy <- map_proxy %>%
        addCircleMarkers(
          lng = ~LONGITUD,
          lat = ~LATITUD,
          radius = 6,
          fillColor = "#3498DB",
          fillOpacity = 0.8,
          color = "#2C3E50",
          weight = 2,
          popup = popup_html,
          label = ~INMUEBLE,
          layerId = ~ID_INMUEBLE 
        ) %>%
        fitBounds(
          lng1 = min(lng_vals),
          lat1 = min(lat_vals),
          lng2 = max(lng_vals),
          lat2 = max(lat_vals)
        )
    }
    
    map_proxy
  })
  
  # ============================================================================
  # OBSERVAR CLICKS EN POLÍGONOS DEL MAPA
  # ============================================================================
  
  observeEvent(input$mapa_inmuebles_shape_click, {
    click <- input$mapa_inmuebles_shape_click
    if (!is.null(click) && !is.null(click$id)) {
      nombre_poligono <- click$id
      inmueble_seleccionado_mapa(nombre_poligono)
    }
  })
  
  # ============================================================================
  # OBSERVAR CLICKS EN MARCADORES DEL MAPA (PUNTOS AZULES)
  # ============================================================================
  
  observeEvent(input$mapa_inmuebles_marker_click, {
    click <- input$mapa_inmuebles_marker_click
    if (!is.null(click) && !is.null(click$id)) {
      nombre_poligono <- click$id
      inmueble_seleccionado_mapa(nombre_poligono)
    }
  })
  
  # Tabla resumida (5 columnas más importantes)
  output$tabla_inmuebles_resumida <- DT::renderDataTable({
    datos <- datos_mapa_inmuebles_filtrados()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay datos disponibles"))
    }
    
    if (inherits(datos, "sf")) {
      datos <- st_drop_geometry(datos)
    }
    
    tryCatch({
      # Los polígonos ya están filtrados por activos desde cargar_poligonos_inmuebles()
      # Filtro adicional en memoria usando inmuebles_snowflake_df (solo activos)
      inmuebles_activos <- inmuebles_snowflake_df()
      if (!is.null(inmuebles_activos) && nrow(inmuebles_activos) > 0 && "ID_INMUEBLE" %in% names(datos)) {
        datos <- datos[datos$ID_INMUEBLE %in% inmuebles_activos$ID, ]
      }
    }, error = function(e) {
      cat("⚠️ Error filtrando inactivos en tabla:", e$message, "\n")
    })
    
    if ("ID_INMUEBLE" %in% names(datos)) {
      datos <- datos[!is.na(datos$ID_INMUEBLE) & datos$ID_INMUEBLE != "" & datos$ID_INMUEBLE != "-", ]
    }
    
    if (nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay inmuebles con ID válido"))
    }
    
    nombre_seleccionado <- inmueble_seleccionado_mapa()
    
    tabla_resumida <- datos %>%
      mutate(
        ID_SHOW = if("ID_INMUEBLE" %in% names(datos)) ID_INMUEBLE else if("ID" %in% names(datos)) ID else "-",
        ALIAS_SHOW = if("ALIAS" %in% names(datos)) ifelse(is.na(ALIAS) | ALIAS == "", "-", ALIAS) else "-",
        EMPRESA_SHOW = if("EMPRESA" %in% names(datos)) ifelse(is.na(EMPRESA) | EMPRESA == "", "-", EMPRESA) else "-",
        ESTADO_SHOW = if("ESTADO" %in% names(datos)) ifelse(is.na(ESTADO) | ESTADO == "", "-", ESTADO) else "-",
        TERRENO_SHOW = if("TERRENO_M2" %in% names(datos)) {
          terreno_numeric <- suppressWarnings(as.numeric(TERRENO_M2))
          ifelse(is.na(terreno_numeric) | terreno_numeric == 0, "-", 
                 paste(format(round(terreno_numeric, 0), big.mark = ","), "m²"))
        } else "-",
        MAPS_SHOW = sprintf(
          '<a href="https://www.google.com/maps?q=%s,%s" target="_blank" class="btn btn-primary btn-sm">Ver</a>',
          LATITUD, LONGITUD
        )
      ) %>%
      select(ID_SHOW, ALIAS_SHOW, EMPRESA_SHOW, ESTADO_SHOW, TERRENO_SHOW, MAPS_SHOW) %>%
      rename(
        ID = ID_SHOW,
        Alias = ALIAS_SHOW,
        Empresa = EMPRESA_SHOW,
        Estado = ESTADO_SHOW,
        Terreno = TERRENO_SHOW,
        Maps = MAPS_SHOW
      )
    
    DT::datatable(
      tabla_resumida,
      escape = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "65vh",
        searching = TRUE,
        ordering = TRUE,
        rowCallback = JS(
          "function(row, data, index) {",
          # ✅ RESALTAR basándose en Name (que está en datos$Name)
          sprintf("  var nombreSeleccionado = '%s';", 
                  if(!is.null(nombre_seleccionado)) nombre_seleccionado else ""),
          sprintf("  var namesArray = %s;", 
                  jsonlite::toJSON(datos$ID_INMUEBLE)),
          "  var nombreActual = namesArray[index];",
          "  if (nombreSeleccionado !== '' && nombreActual === nombreSeleccionado) {",
          "    $(row).css('background-color', '#FFE9CC');",
          "    $(row).css('font-weight', 'bold');",
          "    $(row).css('border-left', '4px solid #ff5b49');",
          "  }",
          "}"
        ),
        language = list(
          zeroRecords = "No se encontraron inmuebles",
          info = "Mostrando _TOTAL_ inmuebles",
          search = "Buscar:"
        )
      ),
      rownames = FALSE,
      selection = 'single',
      class = 'table table-striped table-hover'
    )
  })
  
  # Sincronizar selección de tabla con mapa
  observeEvent(input$tabla_inmuebles_resumida_rows_selected, {
    sel <- input$tabla_inmuebles_resumida_rows_selected
    
    if (length(sel) == 0) return()
    
    datos <- datos_mapa_inmuebles_filtrados()
    if (is.null(datos)) return()
    
    if (inherits(datos, "sf")) {
      datos <- st_drop_geometry(datos)
    }
    
    inmueble_seleccionado <- datos[sel, ]
    
    if (!is.na(inmueble_seleccionado$LATITUD) && !is.na(inmueble_seleccionado$LONGITUD)) {
      leafletProxy("mapa_inmuebles") %>%
        setView(
          lng = inmueble_seleccionado$LONGITUD,
          lat = inmueble_seleccionado$LATITUD,
          zoom = 18
        )
    }
  })
  
  # ============================================================================
  # OBSERVEEVENT: RECARGAR POLÍGONOS DESDE SNOWFLAKE
  # ============================================================================
  observeEvent(input$recargar_mapa, {
    showNotification(" Recargando polígonos desde Snowflake...", 
                     type = "message", duration = 2)
    
    # Recargar polígonos
    poligonos_sf <- cargar_poligonos_inmuebles()
    poligonos_inmuebles(poligonos_sf)
    
    if (!is.null(poligonos_sf) && nrow(poligonos_sf) > 0) {
      showNotification(paste(" Mapa actualizado "), 
                       type = "message", duration = 2)
    } else {
      showNotification("⚠️ No se encontraron polígonos", 
                       type = "warning", duration = 3)
    }
  })
  
  # ============================================================================
  # OUTPUT: CATÁLOGO COMPLETO DE INMUEBLES (ACTIVOS E INACTIVOS)
  # ============================================================================
  # Limpiar filtros del catálogo
  observeEvent(input$limpiar_filtros_catalogo, {
    updateSelectInput(session, "filtro_catalogo_status", selected = "todos")
    updateSelectInput(session, "filtro_catalogo_anio",   selected = "todos")
    updateDateRangeInput(session, "filtro_catalogo_fechas",
                         start = paste0(format(Sys.Date(), "%Y"), "-01-01"),
                         end   = Sys.Date())
  })
  
  # Descargar catálogo filtrado como Excel
  output$descargar_excel_catalogo <- downloadHandler(
    filename = function() {
      status_sel <- if (!is.null(input$filtro_catalogo_status) && 
                        input$filtro_catalogo_status != "todos") {
        paste0("_", input$filtro_catalogo_status)
      } else ""
      paste0("ALMENA_Catalogo", status_sel, "_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    
    content = function(file) {
      datos <- datos_catalogo_reactivo()
      
      if (is.null(datos) || nrow(datos) == 0) {
        showNotification("⚠️ No hay datos para exportar", type = "warning", duration = 3)
        return()
      }
      
      # ── PREPARAR DATOS ─────────────────────────────────────────
      # Formato fechas
      datos$FECHA_ESCRITURA <- tryCatch({
        format(as.Date(substr(as.character(datos$FECHA_ESCRITURA), 1, 10)), "%d/%m/%Y")
      }, error = function(e) as.character(datos$FECHA_ESCRITURA))
      datos$FECHA_ESCRITURA[is.na(datos$FECHA_ESCRITURA) | datos$FECHA_ESCRITURA == "NA"] <- "-"
      
      datos$FECHA_ACTUALIZACION <- tryCatch({
        format(as.POSIXct(datos$FECHA_ACTUALIZACION), "%d/%m/%Y %H:%M")
      }, error = function(e) as.character(datos$FECHA_ACTUALIZACION))
      datos$FECHA_ACTUALIZACION[is.na(datos$FECHA_ACTUALIZACION) | datos$FECHA_ACTUALIZACION == "NA"] <- "-"
      
      datos$FECHA_DE_ASIGNACION <- tryCatch({
        format(as.POSIXct(datos$FECHA_DE_ASIGNACION), "%d/%m/%Y %H:%M")
      }, error = function(e) as.character(datos$FECHA_DE_ASIGNACION))
      datos$FECHA_DE_ASIGNACION[is.na(datos$FECHA_DE_ASIGNACION) | datos$FECHA_DE_ASIGNACION == "NA"] <- "-"
      
      # Limpiar NAs
      datos[] <- lapply(datos, function(col) {
        x <- as.character(col)
        x[is.na(x) | x == "NA"] <- "-"
        x
      })
      
      # Quitar guiones bajos en títulos
      names(datos) <- gsub("_", " ", names(datos))
      
      # ── CREAR WORKBOOK ─────────────────────────────────────────
      wb <- createWorkbook()
      addWorksheet(wb, "Catálogo Inmuebles")
      
      # Estilos
      estilo_header <- createStyle(
        fontSize = 11, fontColour = "#FFFFFF", halign = "center", valign = "center",
        textDecoration = "bold", fgFill = "#DC3545",
        border = "TopBottomLeftRight", borderColour = "#000000"
      )
      estilo_activo <- createStyle(
        fontSize = 10, fontColour = "#27ae60", halign = "center",
        textDecoration = "bold", fgFill = "#e8f5e9"
      )
      estilo_inactivo <- createStyle(
        fontSize = 10, fontColour = "#e74c3c", halign = "center",
        textDecoration = "bold", fgFill = "#fdecea"
      )
      estilo_normal <- createStyle(fontSize = 10, valign = "center")
      estilo_titulo <- createStyle(
        fontSize = 13, fontColour = "#DC3545", halign = "left",
        textDecoration = "bold"
      )
      estilo_subtitulo <- createStyle(
        fontSize = 10, fontColour = "#888888", halign = "left",
        textDecoration = "italic"
      )
      
      # Título y subtítulo
      writeData(wb, "Catálogo Inmuebles",
                x = "ALMENA — Catálogo de Inmuebles",
                startRow = 1, startCol = 1)
      addStyle(wb, "Catálogo Inmuebles", estilo_titulo, rows = 1, cols = 1)
      
      filtro_desc <- paste0(
        "Filtros aplicados: ",
        "Status = ", ifelse(input$filtro_catalogo_status == "todos", "Todos", input$filtro_catalogo_status),
        " | Año = ", ifelse(input$filtro_catalogo_anio == "todos", "Todos", input$filtro_catalogo_anio),
        " | Generado: ", format(Sys.time(), "%d/%m/%Y %H:%M")
      )
      writeData(wb, "Catálogo Inmuebles", x = filtro_desc, startRow = 2, startCol = 1)
      addStyle(wb, "Catálogo Inmuebles", estilo_subtitulo, rows = 2, cols = 1)
      
      # Datos desde fila 4
      writeData(wb, "Catálogo Inmuebles", x = datos, startRow = 4, startCol = 1)
      
      # Header rojo
      addStyle(wb, "Catálogo Inmuebles", estilo_header,
               rows = 4, cols = 1:ncol(datos), gridExpand = TRUE)
      
      # Colorear columna STATUS si existe
      col_status <- which(names(datos) == "STATUS")
      if (length(col_status) > 0 && nrow(datos) > 0) {
        for (fila in seq_len(nrow(datos))) {
          val <- datos[fila, col_status]
          if (!is.na(val) && toupper(trimws(val)) == "ACTIVO") {
            addStyle(wb, "Catálogo Inmuebles", estilo_activo,
                     rows = fila + 4, cols = col_status, stack = FALSE)
          } else if (!is.na(val) && toupper(trimws(val)) == "INACTIVO") {
            addStyle(wb, "Catálogo Inmuebles", estilo_inactivo,
                     rows = fila + 4, cols = col_status, stack = FALSE)
          }
        }
      }
      
      # Estilo normal al resto
      if (nrow(datos) > 0) {
        addStyle(wb, "Catálogo Inmuebles", estilo_normal,
                 rows = 5:(nrow(datos) + 4), cols = 1:ncol(datos), gridExpand = TRUE, stack = TRUE)
      }
      
      # Anchos de columna
      setColWidths(wb, "Catálogo Inmuebles", cols = 1:ncol(datos), widths = "auto")
      setRowHeights(wb, "Catálogo Inmuebles", rows = 1, heights = 20)
      setRowHeights(wb, "Catálogo Inmuebles", rows = 2, heights = 15)
      setRowHeights(wb, "Catálogo Inmuebles", rows = 4, heights = 22)
      
      saveWorkbook(wb, file, overwrite = TRUE)
      
      showNotification(
        paste0("✅ Excel descargado: ", nrow(datos), " inmuebles"),
        type = "message", duration = 3
      )
    },
    
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Resumen rápido encima de la tabla
  output$resumen_catalogo <- renderUI({
    datos <- datos_catalogo_reactivo()
    if (is.null(datos) || nrow(datos) == 0) return(NULL)
    
    n_activos   <- sum(toupper(trimws(datos$STATUS)) == "ACTIVO",  na.rm = TRUE)
    n_inactivos <- sum(toupper(trimws(datos$STATUS)) == "INACTIVO", na.rm = TRUE)
    n_sin       <- sum(is.na(datos$STATUS) | datos$STATUS == "-")
    
    div(style = "display: flex; gap: 15px; margin-bottom: 15px;",
        div(style = "background: #e8f5e9; border-left: 4px solid #27ae60; padding: 10px 18px; border-radius: 6px;",
            tags$b(style = "color: #27ae60; font-size: 1.4rem;", n_activos),
            tags$span(style = "color: #555; font-size: 0.9rem; display: block;", "Activos")
        ),
        div(style = "background: #fdecea; border-left: 4px solid #e74c3c; padding: 10px 18px; border-radius: 6px;",
            tags$b(style = "color: #e74c3c; font-size: 1.4rem;", n_inactivos),
            tags$span(style = "color: #555; font-size: 0.9rem; display: block;", "Inactivos")
        ),
        div(style = "background: #f5f5f5; border-left: 4px solid #aaa; padding: 10px 18px; border-radius: 6px;",
            tags$b(style = "color: #777; font-size: 1.4rem;", nrow(datos)),
            tags$span(style = "color: #555; font-size: 0.9rem; display: block;", "Total mostrados")
        )
    )
  })
  
  # Reactive que trae y filtra los datos del catálogo
  datos_catalogo_reactivo <- reactive({
    tryCatch({
      conn <- crear_conexion_snowflake()
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
      
      datos <- dbGetQuery(conn, "
  SELECT 
    i.ID, i.INMUEBLE, i.EMPRESA, i.ALIAS, i.DIRECCION, i.MUNICIPIO,
    i.ESTADO, i.PAIS, i.FECHA_ESCRITURA, i.PRECIO_ADQUISICION,
    i.TERRENO_M2, i.PRECIO_TERRENO_M2, i.CONSTRUCCION_M2,
    i.PRECIO_CONSTRUCCION_M2, i.FECHA_ACTUALIZACION,
    s.STATUS_ACTIVIDAD  AS STATUS,
    s.STATUS_INMUEBLE   AS STATUS_INMUEBLE,
    s.FECHA_ASIGNACION  AS FECHA_DE_ASIGNACION,
    s.USUARIO           AS USUARIO_STATUS
  FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
  LEFT JOIN (
    SELECT ID, STATUS_ACTIVIDAD, STATUS_INMUEBLE, FECHA_ASIGNACION, USUARIO,
           ROW_NUMBER() OVER (PARTITION BY ID ORDER BY FECHA_ASIGNACION DESC) AS rn
    FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
  ) s ON i.ID = s.ID AND s.rn = 1
  ORDER BY i.EMPRESA, i.ID
")
      dbDisconnect(conn)
      
      if (nrow(datos) == 0) return(data.frame())
      
      # ── FILTRO STATUS ──────────────────────────────────────────
      if (!is.null(input$filtro_catalogo_status) && 
          input$filtro_catalogo_status != "todos") {
        datos <- datos[!is.na(datos$STATUS) & 
                         toupper(trimws(datos$STATUS)) == input$filtro_catalogo_status, ]
      }
      
      # ── FILTRO AÑO (tiene prioridad sobre rango libre) ─────────
      if (!is.null(input$filtro_catalogo_anio) && 
          input$filtro_catalogo_anio != "todos") {
        anio_sel <- as.integer(input$filtro_catalogo_anio)
        fechas_num <- suppressWarnings(
          as.integer(format(as.Date(substr(as.character(datos$FECHA_DE_ASIGNACION), 1, 10)), "%Y"))
        )
        datos <- datos[!is.na(fechas_num) & fechas_num == anio_sel, ]
        
      } else if (!is.null(input$filtro_catalogo_fechas)) {
        # ── FILTRO RANGO LIBRE ──────────────────────────────────
        fecha_ini <- as.Date(input$filtro_catalogo_fechas[1])
        fecha_fin <- as.Date(input$filtro_catalogo_fechas[2])
        
        fechas_asig <- suppressWarnings(
          as.Date(substr(as.character(datos$FECHA_DE_ASIGNACION), 1, 10))
        )
        datos <- datos[!is.na(fechas_asig) & 
                         fechas_asig >= fecha_ini & 
                         fechas_asig <= fecha_fin, ]
      }
      
      return(datos)
      
    }, error = function(e) {
      cat("Error en catálogo reactivo:", e$message, "\n")
      return(NULL)
    })
  })
  
  output$tabla_catalogo_completo <- DT::renderDataTable({
    datos <- datos_catalogo_reactivo()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay inmuebles con los filtros aplicados"))
    }
    
    # Formato fechas
    datos$FECHA_ESCRITURA <- tryCatch({
      format(as.Date(substr(as.character(datos$FECHA_ESCRITURA), 1, 10)), "%d/%m/%Y")
    }, error = function(e) as.character(datos$FECHA_ESCRITURA))
    datos$FECHA_ESCRITURA[is.na(datos$FECHA_ESCRITURA) | datos$FECHA_ESCRITURA == "NA"] <- "-"
    
    datos$FECHA_ACTUALIZACION <- tryCatch({
      format(as.POSIXct(datos$FECHA_ACTUALIZACION), "%d/%m/%Y %H:%M")
    }, error = function(e) as.character(datos$FECHA_ACTUALIZACION))
    datos$FECHA_ACTUALIZACION[is.na(datos$FECHA_ACTUALIZACION) | datos$FECHA_ACTUALIZACION == "NA"] <- "-"
    
    datos$FECHA_DE_ASIGNACION <- tryCatch({
      format(as.POSIXct(datos$FECHA_DE_ASIGNACION), "%d/%m/%Y %H:%M")
    }, error = function(e) as.character(datos$FECHA_DE_ASIGNACION))
    datos$FECHA_DE_ASIGNACION[is.na(datos$FECHA_DE_ASIGNACION) | datos$FECHA_DE_ASIGNACION == "NA"] <- "-"
    
    # Formato moneda
    formatear_moneda <- function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      ifelse(is.na(x_num) | x_num == 0, "-",
             paste0("$", format(round(x_num, 0), big.mark = ",", scientific = FALSE)))
    }
    datos$PRECIO_ADQUISICION     <- formatear_moneda(datos$PRECIO_ADQUISICION)
    datos$PRECIO_TERRENO_M2      <- formatear_moneda(datos$PRECIO_TERRENO_M2)
    datos$PRECIO_CONSTRUCCION_M2 <- formatear_moneda(datos$PRECIO_CONSTRUCCION_M2)
    
    # Colorear STATUS con HTML
    datos$STATUS <- ifelse(
      is.na(datos$STATUS) | datos$STATUS == "",
      "<span style='color:#aaa;'>—</span>",
      ifelse(
        toupper(trimws(datos$STATUS)) == "ACTIVO",
        "<span style='background:#e8f5e9; color:#27ae60; font-weight:bold; padding:2px 8px; border-radius:10px;'>✅ ACTIVO</span>",
        "<span style='background:#fdecea; color:#e74c3c; font-weight:bold; padding:2px 8px; border-radius:10px;'>🔴 INACTIVO</span>"
      )
    )
    
    # Colorear STATUS_INMUEBLE con HTML
    datos$STATUS_INMUEBLE <- ifelse(
      is.na(datos$STATUS_INMUEBLE) | datos$STATUS_INMUEBLE == "",
      "<span style='color:#aaa;'>—</span>",
      ifelse(
        toupper(trimws(datos$STATUS_INMUEBLE)) == "VENDIDO",
        "<span style='background:#fff3cd; color:#856404; font-weight:bold; padding:2px 8px; border-radius:10px;'>️ Vendido</span>",
        "<span style='background:#f8d7da; color:#721c24; font-weight:bold; padding:2px 8px; border-radius:10px;'> Dado de Baja</span>"
      )
    )
    
    # Forzar todo a character
    datos[] <- lapply(datos, function(col) {
      x <- as.character(col)
      x[is.na(x) | x == "NA"] <- "-"
      x
    })
    
    # Quitar guiones bajos en títulos
    names(datos) <- gsub("_", " ", names(datos))
    
    DT::datatable(
      datos,
      escape = FALSE,
      options = list(
        paging    = FALSE,
        scrollX   = TRUE,
        scrollY   = "65vh",
        searching = TRUE,
        ordering  = TRUE,
        dom       = 'frtip',
        autoWidth = FALSE,
        columnDefs = list(list(width = "120px", targets = "_all")),
        initComplete = JS("
          function(settings, json) {
            var wrapper = $(this.api().table().node()).closest('.dataTables_wrapper');
            var scrollHead = wrapper.find('.dataTables_scrollHead');
            var scrollBody = wrapper.find('.dataTables_scrollBody');
            scrollBody.on('scroll', function() {
              scrollHead.scrollLeft(scrollBody.scrollLeft());
            });
          }
        "),
        language = list(
          zeroRecords = "No se encontraron inmuebles",
          info        = "Total: _TOTAL_ inmuebles",
          search      = "Buscar:"
        )
      ),
      rownames = FALSE,
      class    = "table table-striped table-hover"
    )
  })
  
  # ==========================================================================
  # ASISTENTE IA — CORTEX AGENT
  # ==========================================================================
  
  chat_display  <- reactiveVal(list())   # mensajes para mostrar en pantalla
  chat_api_hist <- reactiveVal(list())   # historial para context multi-turno
  chat_ocupado  <- reactiveVal(FALSE)    # semáforo para evitar doble envío
  
  # Render del historial de chat
  output$chat_mensajes_ui <- renderUI({
    msgs <- chat_display()
    
    if (length(msgs) == 0) {
      return(div(style = "text-align: center; padding: 60px 20px; color: #bbb;",
                 icon("robot", style = "font-size: 52px; color: #e0d4c3; display: block; margin-bottom: 12px;"),
                 p(style = "font-size: 15px; margin: 0; color: #999;",
                   "¡Hola! Soy tu Asistente IA de Inmuebles ALMENA."),
                 p(style = "font-size: 13px; color: #bbb; margin-top: 6px;",
                   "Puedo consultar el maestro de inmuebles, geometrías, actividad y más.")
      ))
    }
    
    tagList(lapply(seq_along(msgs), function(i) {
      msg   <- msgs[[i]]
      es_cargando <- isTRUE(msg$cargando)
      
      if (msg$role == "user") {
        div(class = "chat-burbuja-usuario",
            div(HTML(gsub("\n", "<br>", htmltools::htmlEscape(msg$content))))
        )
      } else {
        div(class = paste("chat-burbuja-asistente", if (es_cargando) "chat-cargando"),
            div(
              if (!es_cargando) icon("robot", style = "color: #802e25; margin-right: 6px; font-size: 12px;"),
              HTML(gsub("\n", "<br>", htmltools::htmlEscape(msg$content)))
            )
        )
      }
    }))
  })
  
  # Auto-scroll al último mensaje
  observe({
    chat_display()
    shinyjs::runjs("
      var box = document.getElementById('chat_scroll_box');
      if (box) box.scrollTop = box.scrollHeight;
    ")
  })
  
  # Función central: procesar pregunta
  procesar_pregunta_ia <- function(pregunta) {
    pregunta <- trimws(pregunta)
    if (nchar(pregunta) == 0 || chat_ocupado()) return()
    
    chat_ocupado(TRUE)
    shinyjs::disable("chat_enviar")
    updateTextInput(session, "chat_input", value = "")
    
    # Agregar mensaje usuario
    hist <- chat_display()
    hist <- c(hist, list(list(role = "user", content = pregunta)))
    # Placeholder de carga
    hist <- c(hist, list(list(role = "assistant", content = "⏳ Consultando al agente...", cargando = TRUE)))
    chat_display(hist)
    
    # Llamada al agente (en segundo plano con future/promise si quieres async;
    # aquí síncrono — la app se "pausa" ~2-5 seg mientras espera)
    jwt <- tryCatch(generar_jwt_rest(),
                    error = function(e) { cat("JWT error:", e$message, "\n"); NULL })
    
    if (is.null(jwt)) {
      respuesta_txt <- "❌ Error al generar token JWT. Revisa que el archivo PEM esté en el directorio de trabajo."
    } else {
      resultado <- llamar_cortex_agent(pregunta, chat_api_hist(), jwt)
      respuesta_txt <- resultado$texto
      
      if (resultado$exito) {
        # Actualizar historial para contexto multi-turno
        api_h <- chat_api_hist()
        api_h <- c(api_h,
                   list(list(role = "user",      content = pregunta)),
                   list(list(role = "assistant",  content = respuesta_txt)))
        chat_api_hist(api_h)
      }
    }
    
    # Reemplazar placeholder con respuesta real
    hist <- chat_display()
    hist[[length(hist)]] <- list(role = "assistant", content = respuesta_txt)
    chat_display(hist)
    
    chat_ocupado(FALSE)
    shinyjs::enable("chat_enviar")
  }
  
  # Eventos de envío
  observeEvent(input$chat_enviar,  { procesar_pregunta_ia(input$chat_input) })
  observeEvent(input$ej1, { procesar_pregunta_ia("¿Cuántos inmuebles activos hay?") })
  observeEvent(input$ej2, { procesar_pregunta_ia("¿Qué inmuebles están en Querétaro?") })
  observeEvent(input$ej3, { procesar_pregunta_ia("¿Cuál es el inmueble con mayor terreno?") })
  observeEvent(input$ej4, { procesar_pregunta_ia("¿Cuántos m² de construcción tenemos en total?") })
  
  # Limpiar conversación
  observeEvent(input$chat_limpiar, {
    chat_display(list())
    chat_api_hist(list())
    showNotification("🗑️ Conversación reiniciada", type = "message", duration = 2)
  })
  
}

# EJECUTAR APP INTEGRADA CON LOGIN
shinyApp(ui = ui, server = server)

# -- Consultas el valor en: https://www.inegi.org.mx/temas/inpc/
#   -- y ejecutas una línea:
#   CALL DB_ANALYTICS.SCH_CORE.SP_INSERTAR_INPC('2025-05-01', 150.XX); <- para consultas del INEGI mensuales