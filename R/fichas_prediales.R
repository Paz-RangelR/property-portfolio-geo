library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(DBI)
library(RJDBC)
library(rJava)
library(openssl)
library(tools)
library(later)
library(magick)
library(pdftools) 

# Users come from APP_USERS (salted hashes, never passwords); see R/auth.R.
source(file.path(if (dir.exists("R")) "R" else ".", "auth.R"))
usuarios_validos <- cargar_usuarios()

# ============================================================================
# FUNCIÓN: Convertir PEM a PrivateKey para JWT
# ============================================================================
convertir_pem_a_privatekey <- function(ruta_pem) {
  tryCatch({
    lineas <- readLines(ruta_pem, warn = FALSE)
    lineas <- lineas[!grepl("^-----", lineas)]
    clave_base64 <- paste(lineas, collapse = "")
    
    decoder <- .jcall("java/util/Base64", "Ljava/util/Base64$Decoder;", "getDecoder")
    bytes_clave <- .jcall(decoder, "[B", "decode", clave_base64)
    
    key_spec <- .jnew("java/security/spec/PKCS8EncodedKeySpec", bytes_clave)
    key_factory <- .jcall("java/security/KeyFactory", "Ljava/security/KeyFactory;", 
                          "getInstance", "RSA")
    
    private_key <- .jcall(key_factory, "Ljava/security/PrivateKey;", 
                          "generatePrivate", .jcast(key_spec, "java/security/spec/KeySpec"))
    
    return(private_key)
  }, error = function(e) {
    stop(paste("Error al convertir la clave privada:", e$message))
  })
}

# ============================================================================
# FUNCIÓN: Crear Conexión a Snowflake con JWT
# ============================================================================
crear_conexion_snowflake <- function(usuario = "SVC_ANALYTICS", 
                                     ruta_pem = Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")) {
  tryCatch({
    # Buscamos los archivos en la carpeta donde vive la App
    ruta_jar <- normalizePath("snowflake-jdbc-3.25.1.jar", mustWork = TRUE)
    ruta_pem_abs <- normalizePath(ruta_pem, mustWork = TRUE)
    
    cat("✅ Localizado JAR en:", ruta_jar, "\n")
    
    # Creamos el driver usando la ruta absoluta detectada
    driver <- RJDBC::JDBC(
      driverClass = "net.snowflake.client.jdbc.SnowflakeDriver", 
      classPath = ruta_jar, 
      identifier.quote = "`"
    )
    
    # El resto de tu lógica de JWT se mantiene igual...
    private_key <- convertir_pem_a_privatekey(ruta_pem_abs)
    
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
# FUNCIÓN: Cargar Datos de Inmuebles con PDFs - MODIFICADA PARA CONTEO
# ============================================================================
cargar_inmuebles_con_pdfs <- function() {
  tryCatch({
    conn <- crear_conexion_snowflake()
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    query <- "
      WITH ULTIMA_ACTIVIDAD AS (
        SELECT ID, STATUS_ACTIVIDAD
        FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
        QUALIFY ROW_NUMBER() OVER (PARTITION BY ID ORDER BY FECHA_ASIGNACION DESC) = 1
      )
      SELECT 
        i.ID,
        i.INMUEBLE,
        i.EMPRESA,
        i.ALIAS,
        i.MUNICIPIO,
        i.ESTADO,
        i.PAIS,
        COUNT(p.ID_ARCHIVO) as TOTAL_PDFS,
        MAX(p.FECHA_CARGA) as ULTIMA_CARGA
      FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
      LEFT JOIN DB_ANALYTICS.SCH_CORE.FICHAS_PREDIALES p
        ON i.ID = p.ID
      INNER JOIN ULTIMA_ACTIVIDAD a
        ON i.ID = a.ID
      WHERE UPPER(TRIM(a.STATUS_ACTIVIDAD)) = 'ACTIVO'
      GROUP BY i.ID, i.INMUEBLE, i.EMPRESA, i.ALIAS, i.MUNICIPIO, i.ESTADO, i.PAIS"
    
    datos <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    
    if (nrow(datos) == 0) {
      return(data.frame(
        ID = character(0),
        INMUEBLE = character(0),
        EMPRESA = character(0),
        ALIAS = character(0),
        MUNICIPIO = character(0),
        ESTADO = character(0),
        PAIS = character(0),
        TOTAL_PDFS = numeric(0),
        ULTIMA_CARGA = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    return(datos)
    
  }, error = function(e) {
    cat("❌ Error al cargar datos de PDFs:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN NUEVA: Obtener lista de PDFs de un inmueble específico
# ============================================================================
obtener_pdfs_inmueble <- function(id_inmueble) {
  tryCatch({
    conn <- crear_conexion_snowflake()
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    query <- sprintf("
      SELECT 
        ID_ARCHIVO,
        NOMBRE_ARCHIVO,
        RUTA_STAGE,
        FECHA_CARGA,
        USUARIO
      FROM DB_ANALYTICS.SCH_CORE.FICHAS_PREDIALES
      WHERE ID = '%s'
      ORDER BY FECHA_CARGA DESC
    ", gsub("'", "''", trimws(id_inmueble)))
    
    datos <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    
    return(datos)
    
  }, error = function(e) {
    cat("❌ Error al obtener PDFs:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN: Subir PDF a Snowflake Stage - MODIFICADA PARA MÚLTIPLES ARCHIVOS
# ============================================================================
subir_pdf_a_snowflake <- function(ruta_archivo_local, id_inmueble, nombre_original) {
  tryCatch({
    conn <- crear_conexion_snowflake()
    
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    
    # Generar ID único para este archivo
    id_archivo <- paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", 
                         as.integer(runif(1, 1000, 9999)))
    
    temp_dir <- tempdir()
    # Renombrar archivo con ID único para evitar colisiones en el stage
    nombre_con_id <- paste0(id_archivo, "_", nombre_original)
    ruta_con_nombre_real <- file.path(temp_dir, nombre_con_id)
    file.copy(ruta_archivo_local, ruta_con_nombre_real, overwrite = TRUE)
    
    cat(" Archivo preparado:", nombre_con_id, "\n")
    
    objeto_conexion_JDBC <- conn@jc
    ruta_normalizada <- gsub("\\\\", "/", normalizePath(ruta_con_nombre_real))
    ruta_para_snowflake <- paste0("file://", ruta_normalizada)
    
    id_limpio <- trimws(id_inmueble)
    ruta_stage <- paste0("@FICHAS_PREDIALES_PDF/", id_limpio)  
    
    # Query PUT - OVERWRITE = FALSE para no sobreescribir archivos existentes
    query_put <- sprintf(
      "PUT '%s' '%s/' OVERWRITE = FALSE AUTO_COMPRESS = FALSE",
      ruta_para_snowflake,
      ruta_stage
    )
    
    cat(" PUT:", query_put, "\n")
    
    # Ejecutar PUT
    statement <- .jcall(objeto_conexion_JDBC, "Ljava/sql/Statement;", "createStatement")
    resultado <- .jcall(statement, "Ljava/sql/ResultSet;", "executeQuery", query_put)
    .jcall(statement, "V", "close")
    
    cat(" PDF subido al stage FICHAS_PREDIALES_PDF/", id_limpio, "/\n")
    
    ruta_pdf_stage <- paste0("@FICHAS_PREDIALES_PDF/", id_limpio, "/", nombre_con_id)
    
    # SIEMPRE INSERTAR (no actualizar) - esto permite múltiples PDFs por inmueble
    insert_sql <- sprintf(
      "INSERT INTO FICHAS_PREDIALES (ID, ID_ARCHIVO, RUTA_STAGE, NOMBRE_ARCHIVO, FECHA_CARGA, USUARIO)
       VALUES ('%s', '%s', '%s', '%s', 
               CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ, 
               'SISTEMA')",
      gsub("'", "''", id_limpio),
      id_archivo,
      ruta_pdf_stage,
      gsub("'", "''", nombre_original)
    )
    dbSendUpdate(conn, insert_sql)
    cat(" Nuevo PDF registrado en tabla con ID:", id_archivo, "\n")
    
    # ==========================================================================
    # 2. NUEVO: Registrar la actividad en el historial (DENTRO DEL TRY)
    # ==========================================================================
    insert_historial_doc <- sprintf(
      "INSERT INTO DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD 
       (ID, STATUS_ACTIVIDAD, FECHA_ASIGNACION, USUARIO)
       VALUES ('%s', 'Activo', CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ, 'SISTEMA')",
      gsub("'", "''", id_limpio)
    )
    dbSendUpdate(conn, insert_historial_doc)
    cat("✅ Actividad de carga registrada en historial.\n")
    
    # Limpiar temporal
    if (file.exists(ruta_con_nombre_real)) {
      file.remove(ruta_con_nombre_real)
    }
    
    dbDisconnect(conn)
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error al subir PDF:", e$message, "\n")
    return(FALSE)
  })
  
}

# ============================================================================
# FUNCIÓN: Descargar PDF desde Snowflake Stage
# ============================================================================
descargar_pdf_desde_snowflake <- function(id_inmueble, nombre_archivo) {
  tryCatch({
    conn <- crear_conexion_snowflake()
    
    folder_id <- as.numeric(Sys.time())
    temp_dir <- file.path(tempdir(), paste0("snowflake_", folder_id))
    if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)
    
    # Ruta en el Stage
    ruta_stage <- sprintf("@FICHAS_PREDIALES_PDF/%s/%s", trimws(id_inmueble), nombre_archivo)
    ruta_destino_normalizada <- gsub("\\\\", "/", normalizePath(temp_dir, mustWork = TRUE))
    
    query_get <- sprintf(
      "GET '%s' 'file://%s' PARALLEL = 16",
      ruta_stage,
      ruta_destino_normalizada
    )
    
    statement <- .jcall(conn@jc, "Ljava/sql/Statement;", "createStatement")
    .jcall(statement, "Ljava/sql/ResultSet;", "executeQuery", query_get)
    .jcall(statement, "V", "close")
    dbDisconnect(conn)
    Sys.sleep(1)
    
    archivos_encontrados <- list.files(temp_dir, full.names = TRUE)
    
    if (length(archivos_encontrados) > 0) {
      archivo_descargado <- archivos_encontrados[1]
      
      # Forzar la extensión .pdf si Snowflake le agregó .gz
      ruta_final <- file.path(temp_dir, nombre_archivo)
      if (!grepl("\\.pdf$", ruta_final, ignore.case = TRUE)) {
        ruta_final <- paste0(ruta_final, ".pdf")
      }
      
      file.rename(archivo_descargado, ruta_final)
      return(ruta_final)
    } else {
      return(NULL)
    }
    
  }, error = function(e) {
    cat("❌ Error en descarga:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# UI - INTERFAZ DE USUARIO
# ============================================================================
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
    /* ============================================
       PANTALLA DE LOGIN
       ============================================ */
    .login-container {
      display: flex;
      justify-content: center;
      align-items: center;
      min-height: 100vh;
      background: linear-gradient(135deg, #f5efe6 0%, #FFE9CC 100%);
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
    
    .login-title {
      color: #ff5b49;
      font-size: 28px;
      font-weight: bold;
      margin: 0 0 10px 0;
    }
    
    .login-subtitle {
      color: #401712;
      font-size: 14px;
      margin: 0;
    }
    
    .login-input-group {
      margin-bottom: 20px;
    }
    
    .login-label {
      color: #401712;
      font-weight: 600;
      margin-bottom: 8px;
      display: block;
      font-size: 14px;
    }
    
    .login-input {
      width: 100%;
      padding: 12px;
      border: 2px solid #ff9d92;
      border-radius: 8px;
      font-size: 14px;
      transition: all 0.3s ease;
    }
    
    .login-input:focus {
      border-color: #ff5b49;
      box-shadow: 0 0 10px rgba(230, 126, 34, 0.3);
      outline: none;
    }
    
    .login-btn {
      width: 100%;
      background: linear-gradient(135deg, #ff5b49 0%, #802e25 100%) !important;
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
    
    .login-btn:hover {
      background: linear-gradient(135deg, #802e25 0%, #802e25 100%) !important;
      transform: translateY(-2px);
      box-shadow: 0 6px 20px rgba(230, 126, 34, 0.4);
    }
    
    .login-error {
      background: linear-gradient(135deg, #FFEEE0 0%, #FFDDB3 100%);
      border: 1px solid #ff5b49;
      color: #401712;
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
    
    /* Colores cálidos del tema original */
    body {
      background-color: #e0d4c3;
      font-family: Arial, sans-serif;
    }
      
      .main-header {
        background: linear-gradient(135deg, #ff5b49 0%, #802e25 100%);
        padding: 25px;
        border-radius: 10px;
        margin-bottom: 20px;
        box-shadow: 0 4px 15px rgba(230, 126, 34, 0.3);
        color: white;
        text-align: center;
      }
      
      .main-header h2 {
        margin: 0;
        font-weight: bold;
      }
      
      .main-header p {
        margin: 10px 0 0 0;
        font-size: 14px;
      }
      
      .table-container {
        background: white;
        padding: 20px;
        border-radius: 10px;
        border: 3px solid #ff5b49;
        box-shadow: 0 4px 15px rgba(230, 126, 34, 0.2);
      }
      
      .table-header {
        background: linear-gradient(135deg, #ff9d92 0%, #ff5b49 100%);
        padding: 15px;
        border-radius: 8px;
        margin-bottom: 15px;
        display: flex;
        justify-content: space-between;
        align-items: center;
      }
      
      .table-header h4 {
        color: white;
        margin: 0;
        font-weight: bold;
      }
      
      .btn-actualizar {
        background: white;
        color: #ff5b49;
        font-weight: bold;
        border: none;
        padding: 8px 20px;
        border-radius: 6px;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .btn-actualizar:hover {
        background: #f5efe6;
        transform: translateY(-2px);
        box-shadow: 0 4px 12px rgba(230, 126, 34, 0.3);
      }
      
      /* Ajuste para que la tabla no se salga del contenedor */
      #tabla_documentos_pdf {
        width: 100% !important;
        margin: 0 auto;
        table-layout: fixed; /* Fuerza a respetar el ancho del contenedor */
        word-wrap: break-word;
      }
      
      .table-container {
        background: white;
        padding: 20px;
        border-radius: 10px;
        border: 3px solid #ff5b49;
        box-shadow: 0 4px 15px rgba(230, 126, 34, 0.2);
        overflow-x: hidden; /* Evita scroll horizontal en el contenedor padre */
      }
      
      #tabla_documentos_pdf .dataTables_wrapper {
        max-height: 70vh;
      }
      
      #tabla_documentos_pdf .dataTables_scrollBody {
        max-height: 65vh !important;
        overflow-y: auto !important;
      }
      
      #tabla_documentos_pdf .dataTables_scrollBody::-webkit-scrollbar {
        width: 10px;
      }
      
      #tabla_documentos_pdf .dataTables_scrollBody::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 10px;
      }
      
      #tabla_documentos_pdf .dataTables_scrollBody::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #ff5b49 0%, #802e25 100%);
        border-radius: 10px;
      }
      
      #tabla_documentos_pdf thead th {
        background: linear-gradient(135deg, #ff9d92 0%, #ff5b49 100%) !important;
        color: white !important;
        font-weight: bold !important;
        text-align: center !important;
        padding: 12px !important;
      }
      
      #tabla_documentos_pdf tbody tr {
        background-color: white !important;
      }
      
      #tabla_documentos_pdf tbody tr:nth-child(even) {
        background-color: #FFF5F0 !important;
      }
      
      #tabla_documentos_pdf tbody tr:hover {
        background-color: #FFE9CC !important;
      }
      
      /* Botones de acción */
      .btn-primary {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        border: none;
        color: white;
        font-weight: bold;
      }
      
      .btn-warning {
        background: linear-gradient(135deg, #ff9d92 0%, #ff5b49 100%);
        border: none;
        color: white;
        font-weight: bold;
      }
      
      /* Modal */
      .modal-content {
        border-radius: 10px;
        border: 3px solid #ff5b49;
      }
      
      .modal-header {
        background: linear-gradient(135deg, #ff5b49 0%, #802e25 100%);
        color: white;
        border-radius: 7px 7px 0 0;
      }
      
      .info-box {
        background: linear-gradient(135deg, #f5efe6 0%, #FFE9CC 100%);
        padding: 15px;
        border-radius: 8px;
        border-left: 4px solid #ff9d92;
        margin-bottom: 20px;
      }
      
      .info-box h5 {
        color: #401712;
        margin: 0 0 10px 0;
      }
      
      .info-box p {
        margin: 0;
        color: #401712;
        font-weight: bold;
      }
      
      .alert-info {
        background: #d1ecf1;
        padding: 10px;
        border-radius: 5px;
        border-left: 4px solid #0c5460;
        margin-top: 10px;
      }
      
      .alert-info p {
        margin: 0;
        color: #0c5460;
        font-size: 12px;
      }
      
      /* ESTILO NUEVO PARA EL HIPERVÍNCULO */
      .link-pdf-converter {
        color: white !important;
        font-weight: bold;
        text-decoration: none;
        background: rgba(255, 255, 255, 0.2);
        padding: 8px 15px;
        border-radius: 5px;
        transition: all 0.3s ease;
        border: 1px solid white;
        display: inline-block;
      }
      
      .link-pdf-converter:hover {
        background: white;
        color: #ff5b49 !important;
        text-decoration: none;
      }

      .archivo {
        background-color: #ffe8cc;
        padding: 8px;
        margin-bottom: 6px;
        border: 1px solid #ffa94d;
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-radius: 5px;
      }

      .convertidor-container {
        padding: 15px;
        background: #e0d4c3;
        border-radius: 8px;
      }

      .btn-converter {
        background-color: #f76707 !important;
        border-color: #f76707 !important;
        color: white !important;
        font-weight: bold;
        margin: 5px;
      }

      .btn-converter:hover {
        background-color: #e8590c !important;
      }
      
      /* Estilos para la lista de PDFs */
      .pdf-item {
        background: #f8f9fa;
        padding: 12px;
        margin-bottom: 10px;
        border-radius: 5px;
        border-left: 4px solid #ff5b49;
        display: flex;
        justify-content: space-between;
        align-items: center;
        transition: all 0.3s ease;
      }
      
      .pdf-item:hover {
        background: #e9ecef;
        transform: translateX(5px);
      }
      
      .pdf-item-info {
        flex-grow: 1;
      }
      
      .pdf-item-name {
        color: #2C2C2C;
        font-weight: bold;
        margin-bottom: 5px;
      }
      
      .pdf-item-meta {
        color: #666;
        font-size: 12px;
      }
      
    "))
  ),
  
  # ============================================================================
  # PANTALLA DE LOGIN
  # ============================================================================
  div(id = "login_screen", class = "login-container",
      div(class = "login-box",
          div(class = "login-header",
              h1(class = "login-title",
                 icon("lock"), " M.I. ALMENA"),
              p(class = "login-subtitle",
                "Sistema de Gestión de Documentos PDF")
          ),
          
          div(class = "login-input-group",
              tags$label(class = "login-label", "👤 Usuario"),
              textInput("login_usuario",
                        label = NULL,
                        placeholder = "Ingrese su usuario",
                        width = "100%")
          ),
          
          div(class = "login-input-group",
              tags$label(class = "login-label", " Contraseña"),
              passwordInput("login_password",
                            label = NULL,
                            placeholder = "Ingrese su contraseña",
                            width = "100%")
          ),
          
          actionButton("btn_login",
                       " Iniciar Sesión",
                       class = "login-btn",
                       width = "100%"),
          
          uiOutput("login_error_msg")
      )
  ),
  
  # ============================================================================
  # CONTENIDO PRINCIPAL (SOLO VISIBLE DESPUÉS DE LOGIN)
  # ============================================================================
  div(id = "app_content", class = "app-content",
      
      # Encabezado principal
      div(class = "main-header",
          h2(icon("file-pdf"), " GESTIÓN DE DOCUMENTOS PDF"),
          p("Carga y administra documentos PDF de cada inmueble")
      ),
      
      # Contenedor de la tabla
      div(class = "table-container",
          # Encabezado de la tabla
          div(class = "table-header",
              h4(" DOCUMENTOS POR INMUEBLE "),
              div(style = "display: flex; align-items: center; gap: 10px;",
                  uiOutput("badge_sin_pdf"),
                  actionButton(
                    "filtro_sin_pdf",
                    " Ver sin PDF",
                    class = "btn",
                    style = "background: white; color: #ff5b49; font-weight: bold;
                 border: 2px solid white; padding: 7px 15px; border-radius: 6px;"
                  ),
                  actionButton(
                    "abrir_convertidor_pdf",
                    " Convierte tus imágenes a formato PDF aquí",
                    class = "link-pdf-converter",
                    style = "border: none; cursor: pointer;"
                  )
              )
          ),
          
          # Tabla
          DT::dataTableOutput("tabla_documentos_pdf")
      )
  )
)

# ============================================================================
# SERVER - LÓGICA DEL SERVIDOR
# ============================================================================
server <- function(input, output, session) {
  
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
        div(class = "login-error",
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
      
      # Limpiar campos de login
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
        div(class = "login-error",
            "❌ Usuario o contraseña incorrectos")
      })
      
      # Limpiar contraseña
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
  
  # Variable reactiva para almacenar datos
  inmuebles_pdf_df <- reactiveVal(NULL)
  pdf_a_descargar <- reactiveVal(NULL)
  mostrar_solo_sin_pdf <- reactiveVal(FALSE)
  
  # Variables reactivas para el convertidor
  archivos_convertidor <- reactiveVal(data.frame(
    id = character(), 
    name = character(),
    path = character(),
    stringsAsFactors = FALSE
  ))
  
  estado_convertidor <- reactiveVal("")
  contador_archivos <- reactiveVal(0) 
  
  # ============================================================================
  # DOWNLOAD HANDLER PERMANENTE PARA PDFs
  # ============================================================================
  output$download_pdf_final <- downloadHandler(
    filename = function() {
      pdf_a_descargar()$nombre
    },
    content = function(file) {
      info <- pdf_a_descargar()
      if(!is.null(info) && file.exists(info$ruta)) {
        file.copy(info$ruta, file)
      }
    },
    contentType = NA
  )
  
  # Cargar datos al iniciar
  observe({
    if(is.null(inmuebles_pdf_df())) {
      cat(" Cargando datos de PDFs al iniciar app...\n")
      datos_pdf <- cargar_inmuebles_con_pdfs()
      inmuebles_pdf_df(datos_pdf)
    }
  })
  
  # ============================================================================
  # OUTPUT: TABLA DOCUMENTOS PDF - MODIFICADA PARA MOSTRAR CONTEO
  # ============================================================================
  output$tabla_documentos_pdf <- DT::renderDataTable({
    datos <- isolate(inmuebles_pdf_df())
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay datos disponibles"))
    }
    
    if (isolate(mostrar_solo_sin_pdf())) {
      datos <- datos[is.na(datos$TOTAL_PDFS) | datos$TOTAL_PDFS == 0, ]
      if (nrow(datos) == 0) {
        return(data.frame(Mensaje = " ¡Todos los inmuebles tienen al menos un PDF cargado!"))
      }
    }
    
    # Formatear fecha
    if ("ULTIMA_CARGA" %in% names(datos)) {
      datos$ULTIMA_CARGA <- sapply(datos$ULTIMA_CARGA, function(fecha) {
        tryCatch({
          if (is.na(fecha) || length(fecha) == 0 || as.character(fecha) == "") {
            return("-")
          }
          
          # Convertir a character primero
          fecha_str <- as.character(fecha)
          
          # Si ya tiene formato legible, dejarlo así
          if (grepl("^\\d{4}-\\d{2}-\\d{2}", fecha_str)) {
            return(substr(fecha_str, 1, 19))  # Primeros 19 caracteres (YYYY-MM-DD HH:MM:SS)
          }
          
          return(fecha_str)
          
        }, error = function(e) {
          return("-")
        })
      })
    }
    
    # Columna de acción para cargar/ver PDFs
    datos$CARGAR_PDF <- sapply(1:nrow(datos), function(i) {
      id_inmueble <- datos$ID[i]
      total_pdfs <- ifelse(is.na(datos$TOTAL_PDFS[i]), 0, datos$TOTAL_PDFS[i])
      timestamp <- as.numeric(Sys.time()) + i
      
      if (total_pdfs > 0) {
        paste0(
          '<div style="text-align: center;">',
          '<span style="color: #27ae60; font-size: 14px; font-weight: bold; display: block; margin-bottom: 5px;">',
          '✅ ', total_pdfs, ' PDF(s) almacenado(s)</span>',
          '<small style="color: #666; display: block; margin-bottom: 8px;">',
          'Última carga: ', datos$ULTIMA_CARGA[i], '</small>',
          
          # Botón VER PDFs
          '<button class="btn btn-success btn-xs" ',
          'onclick="Shiny.setInputValue(\'ver_pdfs_inmueble\', \'', id_inmueble, '_', timestamp, '\', {priority: \'event\'});" ',
          'style="font-size: 11px; padding: 4px 8px; margin-right: 5px;">',
          '<i class="fa fa-list"></i> Ver PDFs (', total_pdfs, ')</button>',
          
          # Botón CARGAR MÁS
          '<button class="btn btn-warning btn-xs" ',
          'onclick="Shiny.setInputValue(\'cargar_pdf_inmueble\', \'', id_inmueble, '_', timestamp, '\', {priority: \'event\'});" ',
          'style="font-size: 11px; padding: 4px 8px;">',
          '<i class="fa fa-upload"></i> Cargar Más</button>',
          '</div>'
        )
      } else {
        paste0(
          '<button class="btn btn-sm" ',
          'onclick="Shiny.setInputValue(\'cargar_pdf_inmueble\', \'', id_inmueble, '_', timestamp, '\', {priority: \'event\'});" ',
          'style="font-size: 12px; padding: 6px 12px; font-weight: bold; background-color: #ff5b49; color: white; border: none; border-radius: 4px;">',
          '<i class="fa fa-upload"></i> Cargar PDF</button>'
        )
      }
    })
    
    # Seleccionar columnas a mostrar
    datos_mostrar <- datos[, c("ID", "INMUEBLE", "EMPRESA", "ALIAS", 
                               "MUNICIPIO", "ESTADO", "PAIS", 
                               "TOTAL_PDFS", "ULTIMA_CARGA", "CARGAR_PDF")]
    
    # Renombrar columnas
    names(datos_mostrar) <- c("ID", "Inmueble", "Empresa", "Alias",
                              "Municipio", "Estado", "País",
                              "Total PDFs", "Última Carga", "Acción")
    
    # Reemplazar NAs
    datos_mostrar[is.na(datos_mostrar)] <- "-"
    
    DT::datatable(
      datos_mostrar,
      escape = FALSE,
      options = list(
        paging = FALSE,
        scrollX = TRUE, 
        scrollY = "60vh",
        searching = TRUE,
        ordering = TRUE,
        autoWidth = FALSE, 
        columnDefs = list(
          # Ajustamos anchos fijos para columnas con texto largo
          list(targets = 0, width = "80px"),  # ID
          list(targets = 1, width = "200px"), # Inmueble
          list(targets = 2, width = "150px"), # Empresa
          list(targets = 3, width = "120px"), # Alias
          list(targets = 9, width = "180px", className = "text-center", orderable = FALSE) # Acción
        ),
        language = list(
          zeroRecords = "No se encontraron registros",
          info = "Mostrando _TOTAL_ inmuebles",
          search = "Buscar:"
        )
      ),
      rownames = FALSE,
      class = 'table table-striped table-hover'
    )
  })
  
  # ============================================================================
  # OBSERVEEVENT: Cargar PDF para inmueble
  # ============================================================================
  observeEvent(input$cargar_pdf_inmueble, {
    
    # Extraer ID (remover timestamp aleatorio)
    id_inmueble <- input$cargar_pdf_inmueble
    
    if (!is.null(id_inmueble) && id_inmueble != "") {
      # Limpiar el ID (remover el timestamp)
      id_limpio <- gsub("_[0-9\\.]+$", "", id_inmueble)
      
      showModal(modalDialog(
        title = div(style = "color: #f5efe6; font-weight: bold; font-size: 18px;",
                    " Cargar Documento PDF"),
        
        div(style = "padding: 15px;",
            div(class = "info-box",
                h5(" Inmueble seleccionado:"),
                p(paste("ID:", id_limpio))
            ),
            
            fileInput("pdf_upload_modal", 
                      label = "Seleccionar archivo PDF:",
                      accept = c(".pdf", "application/pdf"),
                      placeholder = "Ningún archivo seleccionado",
                      buttonLabel = "📂 Adjuntar",
                      width = "100%"),
            
            uiOutput("pdf_modal_status"),
            
            div(class = "alert-info",
                p("ℹ️ Solo se permiten archivos PDF. El archivo se guardará en Snowflake y se agregará al historial del inmueble.")
            )
        ),
        
        footer = tagList(
          modalButton("✖ Cancelar"),
          actionButton("confirmar_pdf_upload", 
                       "✅ Guardar PDF", 
                       class = "btn btn-warning",
                       style = "padding: 10px 25px; border-radius: 6px;")
        ),
        
        easyClose = FALSE,
        size = "m"
      ))
      
      # Guardar ID limpio para usar en confirmación
      session$userData$id_inmueble_pdf <- id_limpio
    }
  })
  
  # ============================================================================
  # OBSERVEEVENT: Confirmar carga de PDF
  # ============================================================================
  observeEvent(input$confirmar_pdf_upload, {
    
    id_inmueble <- session$userData$id_inmueble_pdf
    
    if (is.null(input$pdf_upload_modal) || is.null(input$pdf_upload_modal$datapath)) {
      showNotification("⚠️ Debe seleccionar un archivo PDF", type = "warning", duration = 3)
      return()
    }
    
    # Validar que sea PDF
    if (!grepl("\\.pdf$", tolower(input$pdf_upload_modal$name))) {
      showNotification("❌ Solo se permiten archivos PDF", type = "error", duration = 3)
      return()
    }
    
    removeModal()
    
    showNotification(" Subiendo PDF a Snowflake...", type = "message", duration = 2)
    
    resultado <- subir_pdf_a_snowflake(
      ruta_archivo_local = input$pdf_upload_modal$datapath,
      id_inmueble = id_inmueble,
      nombre_original = input$pdf_upload_modal$name  
    )
    
    if (resultado) {
      # Actualizar el conteo visualmente en la celda via JS (sin re-render de tabla)
      shinyjs::runjs(sprintf(
        "var btns = document.querySelectorAll('[onclick]');
         btns.forEach(function(b) {
           var oc = b.getAttribute('onclick') || '';
           if (oc.indexOf(\"'%s_\") !== -1 || oc.indexOf(\"'%s'\") !== -1) {
             var celda = b.closest('td');
             if (celda) {
               var span = celda.querySelector('span');
               if (span && span.innerText.indexOf('PDF') !== -1) {
                 var match = span.innerText.match(/([0-9]+) PDF/);
                 var n = match ? parseInt(match[1]) + 1 : 1;
                 span.innerText = '✅ ' + n + ' PDF(s) almacenado(s)';
               } else if (!span) {
                 celda.innerHTML = '<div style=\"text-align:center\">' +
                   '<span style=\"color:#27ae60;font-size:14px;font-weight:bold;display:block;margin-bottom:5px;\">✅ 1 PDF(s) almacenado(s)</span>' +
                   '</div>';
               }
             }
           }
         });",
        id_inmueble, id_inmueble
      ))
      
      # Actualizar datos en memoria sin disparar re-render
      datos_actualizados <- cargar_inmuebles_con_pdfs()
      isolate(inmuebles_pdf_df(datos_actualizados))
      
      showNotification(
        paste0("✅ PDF '", input$pdf_upload_modal$name, "' guardado exitosamente"),
        type = "message", 
        duration = 2
      )
    } else {
      showNotification("❌ Error al guardar el PDF", type = "error", duration = 3)
    }
    
    session$userData$id_inmueble_pdf <- NULL
  })
  
  # ============================================================================
  # NUEVO OBSERVEEVENT: Ver lista de PDFs de un inmueble
  # ============================================================================
  observeEvent(input$ver_pdfs_inmueble, {
    id_completo <- input$ver_pdfs_inmueble
    id_limpio <- gsub("_[0-9\\.]+$", "", id_completo)
    
    # Obtener lista de PDFs
    pdfs_lista <- obtener_pdfs_inmueble(id_limpio)
    
    if (is.null(pdfs_lista) || nrow(pdfs_lista) == 0) {
      showNotification("⚠️ No se encontraron PDFs para este inmueble", type = "warning", duration = 3)
      return()
    }
    
    # Obtener nombre del inmueble
    datos <- inmuebles_pdf_df()
    fila_inmueble <- datos[datos$ID == id_limpio, ]
    nombre_inmueble <- if(nrow(fila_inmueble) > 0) fila_inmueble$INMUEBLE[1] else id_limpio
    
    showModal(modalDialog(
      title = div(style = "color: #f5efe6; font-weight: bold; font-size: 18px;",
                  " Documentos PDF del Inmueble"),
      
      div(style = "padding: 15px;",
          div(class = "info-box",
              h5(" Inmueble seleccionado:"),
              p(strong(nombre_inmueble)),
              p(paste("ID:", id_limpio)),
              p(paste("Total de documentos:", nrow(pdfs_lista))),
              div(style = "margin-top: 10px;",
                  actionButton(
                    "descargar_todos_pdfs",
                    "Descargar todos los PDFs (.zip)",
                    class = "btn btn-warning btn-sm",
                    onclick = sprintf("Shiny.setInputValue('descargar_todos_id', '%s', {priority: 'event'});", id_limpio),
                    style = "border: none; font-weight: bold;"
                  )
              )
          ),
          
          h5(style = "color: #ff5b49; margin-top: 20px; margin-bottom: 15px;", 
             " Lista de documentos (más recientes primero):"),
          
          # Lista de PDFs con scroll
          div(style = "max-height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; border-radius: 5px;",
              lapply(1:nrow(pdfs_lista), function(i) {
                pdf_info <- pdfs_lista[i, ]
                timestamp_btn <- as.numeric(Sys.time()) * 1000 + i
                
                # Formatear fecha
                fecha_formateada <- tryCatch({
                  format(as.POSIXct(pdf_info$FECHA_CARGA), "%Y-%m-%d %H:%M:%S")
                }, error = function(e) {
                  as.character(pdf_info$FECHA_CARGA)
                })
                
                div(class = "pdf-item",
                    div(class = "pdf-item-info",
                        div(class = "pdf-item-name",
                            icon("file-pdf", style = "color: #ff5b49;"),
                            " ",
                            strong(pdf_info$NOMBRE_ARCHIVO)
                        ),
                        div(class = "pdf-item-meta",
                            fecha_formateada,
                            pdf_info$USUARIO
                        )
                    ),
                    actionButton(
                      paste0("descargar_pdf_lista_", i),
                      " Descargar",
                      class = "btn btn-success btn-sm",
                      onclick = sprintf(
                        "Shiny.setInputValue('descargar_pdf_especifico', '%s|%s|%s|%s_%s', {priority: 'event'});",
                        id_limpio,
                        pdf_info$ID_ARCHIVO,
                        pdf_info$NOMBRE_ARCHIVO,
                        gsub("\\.pdf$", "", pdf_info$NOMBRE_ARCHIVO, ignore.case = TRUE),
                        timestamp_btn
                      ),
                      style = "min-width: 100px;"
                    )
                )
              })
          )
      ),
      
      footer = tagList(
        modalButton("✖ Cerrar")
      ),
      easyClose = TRUE,
      size = "l"
    ))
    
    # Guardar ID para referencia
    session$userData$id_inmueble_actual <- id_limpio
  })
  
  # ============================================================================
  # NUEVO OBSERVEEVENT: Descargar PDF específico de la lista
  # ============================================================================
  observeEvent(input$descargar_pdf_especifico, {
    datos_descarga <- strsplit(input$descargar_pdf_especifico, "\\|")[[1]]
    
    if (length(datos_descarga) < 4) {
      showNotification("❌ Error en los datos de descarga", type = "error", duration = 3)
      return()
    }
    
    id_inmueble <- datos_descarga[1]
    id_archivo <- datos_descarga[2]
    nombre_original <- datos_descarga[3]
    timestamp_data <- datos_descarga[4]
    
    showNotification(" Extrayendo PDF de Snowflake...", type = "message", id = "loading_pdf", duration = NULL)
    
    # Construir nombre completo del archivo en stage (ID_ARCHIVO_NOMBREORIGINAL)
    nombre_completo_stage <- paste0(id_archivo, "_", nombre_original)
    
    ruta_lista <- descargar_pdf_desde_snowflake(id_inmueble, nombre_completo_stage)
    
    removeNotification("loading_pdf")
    
    if (!is.null(ruta_lista) && file.exists(ruta_lista)) {
      pdf_a_descargar(list(
        ruta = ruta_lista,
        nombre = nombre_original  
      ))
      
      showModal(modalDialog(
        title = div(style = "color: #2C2C2C; font-weight: bold;", " Documento listo"),
        div(style = "text-align: center; padding: 20px;",
            icon("file-pdf", class = "fa-4x", style = "color: #ff5b49; margin-bottom: 15px;"),
            h4("El documento está listo para descargar"),
            p(strong(nombre_original)),
            br(),
            downloadButton("download_pdf_final", " Guardar en mi equipo", class = "btn-warning")
        ),
        footer = modalButton("Cerrar"),
        easyClose = TRUE
      ))
    } else {
      showNotification("❌ Error: No se pudo descargar el archivo desde Snowflake", type = "error", duration = 4)
    }
  })
  
  # ============================================================================
  # OBSERVEEVENT: Descargar todos los PDFs de un inmueble como ZIP
  # ============================================================================
  observeEvent(input$descargar_todos_id, {
    id_inmueble <- input$descargar_todos_id
    
    showNotification("Preparando paquete de archivos...", type = "message", id = "zip_loading", duration = NULL)
    
    pdfs_lista <- obtener_pdfs_inmueble(id_inmueble)
    
    if (is.null(pdfs_lista) || nrow(pdfs_lista) == 0) {
      removeNotification("zip_loading")
      showNotification("No hay archivos para descargar", type = "warning")
      return()
    }
    
    temp_zip_dir <- file.path(tempdir(), paste0("zip_", as.numeric(Sys.time())))
    dir.create(temp_zip_dir)
    
    archivos_descargados <- c()
    
    for (i in 1:nrow(pdfs_lista)) {
      nombre_stage <- paste0(pdfs_lista$ID_ARCHIVO[i], "_", pdfs_lista$NOMBRE_ARCHIVO[i])
      ruta_tmp <- descargar_pdf_desde_snowflake(id_inmueble, nombre_stage)
      
      if (!is.null(ruta_tmp) && file.exists(ruta_tmp)) {
        ruta_destino <- file.path(temp_zip_dir, pdfs_lista$NOMBRE_ARCHIVO[i])
        file.copy(ruta_tmp, ruta_destino)
        archivos_descargados <- c(archivos_descargados, ruta_destino)
      }
    }
    
    removeNotification("zip_loading")
    
    if (length(archivos_descargados) > 0) {
      zip_file <- file.path(tempdir(), paste0("PDFs_", id_inmueble, ".zip"))
      utils::zip(zip_file, files = archivos_descargados, flags = "-j")
      
      pdf_a_descargar(list(
        ruta = zip_file,
        nombre = paste0("Documentos_", id_inmueble, ".zip")
      ))
      
      showModal(modalDialog(
        title = div(style = "color: #2C2C2C; font-weight: bold;", "Paquete listo"),
        div(style = "text-align: center; padding: 20px;",
            icon("file-archive", class = "fa-4x", style = "color: #ff5b49; margin-bottom: 15px;"),
            h4("Se han comprimido todos los archivos"),
            p(paste("Total:", length(archivos_descargados), "archivos")),
            br(),
            downloadButton("download_pdf_final", "Descargar archivo .ZIP", class = "btn-warning")
        ),
        footer = modalButton("Cerrar"),
        easyClose = TRUE
      ))
    } else {
      showNotification("No se pudieron descargar los archivos", type = "error")
    }
  })
  
  observeEvent(input$filtro_sin_pdf, {
    nuevo_estado <- !mostrar_solo_sin_pdf()
    mostrar_solo_sin_pdf(nuevo_estado)
    
    label <- if (nuevo_estado) " Ver todos" else " Ver sin PDF"
    shinyjs::runjs(sprintf(
      "$('#filtro_sin_pdf').text('%s');", label
    ))
  })
  
  output$badge_sin_pdf <- renderUI({
    datos <- inmuebles_pdf_df()
    if (is.null(datos)) return(NULL)
    n <- sum(is.na(datos$TOTAL_PDFS) | datos$TOTAL_PDFS == 0, na.rm = TRUE)
    if (n == 0) return(NULL)
    div(style = "background: white; color: #ff5b49; font-weight: bold;
               padding: 5px 12px; border-radius: 12px; font-size: 13px;",
        paste0( n, " sin PDF"))
  })
  
  # ============================================================================
  # BOTÓN ACTUALIZAR TABLA PDF
  # ============================================================================
  observeEvent(input$actualizar_tabla_pdf, {
    showNotification("🔄 Actualizando datos...", type = "message", duration = 2)
    
    datos_actualizados <- cargar_inmuebles_con_pdfs()
    inmuebles_pdf_df(datos_actualizados)
    
    if (!is.null(datos_actualizados) && nrow(datos_actualizados) > 0) {
      showNotification(" Datos actualizados", type = "message", duration = 2)
    } else {
      showNotification("️ No se encontraron datos", type = "warning", duration = 2)
    }
  })
  
  # ============================================================================
  # MODAL: Convertidor de Imágenes a PDF
  # ============================================================================
  observeEvent(input$abrir_convertidor_pdf, {
    archivos_convertidor(data.frame(
      id = character(),
      name = character(),
      path = character(),
      stringsAsFactors = FALSE
    ))
    contador_archivos(0)  
    estado_convertidor("")
    
    showModal(modalDialog(
      title = div(style = "color: #f5efe6; font-weight: bold; font-size: 18px;",
                  "️ Transformador de Imágenes a PDF"),
      
      div(class = "convertidor-container",
          
          # Input de archivos
          fileInput(
            "files_convertidor",
            "📂 Agregar imágenes o PDFs",
            multiple = TRUE,
            accept = c("image/*", "application/pdf"),
            buttonLabel = "Seleccionar archivos",
            placeholder = "Ningún archivo seleccionado"
          ),
          
          # Lista de archivos
          div(style = "margin: 20px 0;",
              h5(style = "color: #ff5b49; font-weight: bold;", " Archivos seleccionados:"),
              uiOutput("lista_archivos_convertidor")
          ),
          
          # Estado
          div(style = "margin-top: 15px; padding: 10px; background: #e3f2fd; border-radius: 5px;",
              textOutput("estado_convertidor")
          ),
          
          # Botones de acción
          div(style = "text-align: center; margin-top: 20px;",
              downloadButton(
                "convertir_descargar_pdf",
                " Convertir y Descargar PDF",
                class = "btn-converter"
              ),
              actionButton(
                "reset_convertidor",
                "🔄 Limpiar todo",
                class = "btn btn-warning",
                style = "margin-left: 10px;"
              )
          )
      ),
      
      footer = modalButton("✖ Cerrar"),
      easyClose = TRUE,
      size = "l"
    ))
  })
  
  # ============================================================================
  # CONVERTIDOR: Agregar archivos
  # ============================================================================
  observeEvent(input$files_convertidor, {
    req(input$files_convertidor)
    
    # Generar IDs únicos para cada archivo nuevo
    n_nuevos <- nrow(input$files_convertidor)
    base_id <- contador_archivos()
    
    nuevos <- data.frame(
      id = paste0("file_", (base_id + 1):(base_id + n_nuevos)),  
      name = input$files_convertidor$name,
      path = input$files_convertidor$datapath,
      stringsAsFactors = FALSE
    )
    
    # Actualizar contador
    contador_archivos(base_id + n_nuevos)
    
    archivos_convertidor(rbind(archivos_convertidor(), nuevos))
    estado_convertidor(paste("✅", n_nuevos, "archivo(s) agregado(s)"))
  })
  
  # ============================================================================
  # CONVERTIDOR: Mostrar lista de archivos
  # ============================================================================
  output$lista_archivos_convertidor <- renderUI({
    datos <- archivos_convertidor()
    
    if (nrow(datos) == 0) {
      return(div(style = "text-align: center; color: #999; padding: 20px;",
                 "No hay archivos seleccionados"))
    }
    
    lapply(1:nrow(datos), function(i) {
      file_id <- datos$id[i]
      
      div(
        class = "archivo",
        span(style = "flex-grow: 1;", 
             icon("file-image"), " ", datos$name[i]),
        actionButton(
          paste0("remove_convertidor_", file_id),  
          "✕",
          class = "btn btn-danger btn-sm",
          style = "padding: 2px 8px;"
        )
      )
    })
  })
  
  # ============================================================================
  # CONVERTIDOR: Eliminar archivos individuales 
  # ============================================================================
  observe({
    datos <- archivos_convertidor()
    req(nrow(datos) > 0)
    
    # Iterar sobre cada archivo usando su ID único
    lapply(1:nrow(datos), function(i) {
      file_id <- datos$id[i]
      button_id <- paste0("remove_convertidor_", file_id)
      
      observeEvent(input[[button_id]], {
        # Eliminar el archivo con ese ID específico
        tmp <- archivos_convertidor()
        tmp_filtrado <- tmp[tmp$id != file_id, ]  
        archivos_convertidor(tmp_filtrado)
        
        n_restantes <- nrow(tmp_filtrado)
        estado_convertidor(paste("🗑️ Archivo eliminado. Quedan", n_restantes, "archivo(s)"))
      }, ignoreInit = TRUE)
    })
  })
  
  # ============================================================================
  # CONVERTIDOR: Reset
  # ============================================================================
  observeEvent(input$reset_convertidor, {
    archivos_convertidor(data.frame(
      id = character(),
      name = character(),
      path = character(),
      stringsAsFactors = FALSE
    ))
    contador_archivos(0)  
    estado_convertidor("🔄 Proceso reiniciado")
  })
  
  # ============================================================================
  # CONVERTIDOR: Generar y descargar PDF
  # ============================================================================
  output$convertir_descargar_pdf <- downloadHandler(
    filename = function() {
      paste0("imagenes_unidas_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      datos <- archivos_convertidor()
      req(nrow(datos) > 0)
      
      tryCatch({
        estado_convertidor("🔄 Generando PDF...")
        
        tmp_dir <- tempdir()
        pdfs <- c()
        
        for (i in seq_len(nrow(datos))) {
          ruta <- datos$path[i]
          ext <- tolower(tools::file_ext(datos$name[i]))
          
          if (ext == "pdf") {
            pdfs <- c(pdfs, ruta)
          } else {
            img <- image_read(ruta)
            pdf_img <- file.path(tmp_dir, paste0("img_", i, ".pdf"))
            image_write(img, pdf_img, format = "pdf")
            pdfs <- c(pdfs, pdf_img)
          }
        }
        
        pdf_combine(pdfs, file)
        
        estado_convertidor("✅ PDF generado ")
        
        showNotification(
          "✅ PDF descargado correctamente",
          type = "message",
          duration = 4
        )
        
      }, error = function(e) {
        estado_convertidor(paste("❌ Error:", e$message))
        showNotification(
          paste("❌ Error al generar PDF:", e$message),
          type = "error",
          duration = 5
        )
      })
    }
  )
  
  # ============================================================================
  # CONVERTIDOR: Mostrar estado
  # ============================================================================
  output$estado_convertidor <- renderText({
    estado_convertidor()
  })
}

# ============================================================================
# EJECUTAR APLICACIÓN
# ============================================================================
shinyApp(ui = ui, server = server)

