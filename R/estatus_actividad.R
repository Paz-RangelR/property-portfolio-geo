library(shiny)
library(shinyjs)
library(DT)
library(dplyr)
library(DBI)
library(RJDBC)
library(rJava)
library(openssl)
library(sf)          
library(stringdist)  

# ==============================================================================
# FUNCIONES DE CONEXIÓN A SNOWFLAKE
# ==============================================================================

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

crear_conexion_snowflake <- function(usuario = "SVC_ANALYTICS", 
                                     ruta_pem = Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")) {
  tryCatch({
    ruta_archivo_jar <- file.path(getwd(), "snowflake-jdbc-3.25.1.jar")
    ruta_pem_absoluta <- file.path(getwd(), ruta_pem)
    
    driver <- RJDBC::JDBC("net.snowflake.client.jdbc.SnowflakeDriver", 
                          classPath = ruta_archivo_jar, 
                          identifier.quote = "`")
    
    private_key <- convertir_pem_a_privatekey(ruta_pem_absoluta)
    
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


# ==============================================================================
# Insertar siempre para generar historial
# ==============================================================================

actualizar_status_snowflake <- function(id_inmueble, nuevo_status, usuario = "SISTEMA") {
  tryCatch({
    conn <- crear_conexion_snowflake()
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    id_limpio <- gsub("'", "''", id_inmueble)
    status_limpio <- gsub("'", "''", nuevo_status)
    usuario_limpio <- gsub("'", "''", usuario)
    
    insert_query <- sprintf(
      "INSERT INTO DB_ANALYTICS.SCH_CORE.INMUEBLES_STATUS (ID, STATUS, FECHA_DE_ASIGNACION, USUARIO)
       VALUES ('%s', '%s', CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ, '%s')",
      id_limpio, status_limpio, usuario_limpio
    )
    
    dbSendUpdate(conn, insert_query)
    cat("✅ Historial de status registrado para:", id_inmueble, "->", nuevo_status, "\n")
    
    dbDisconnect(conn)
    return(TRUE)
    
  }, error = function(e) {
    cat("❌ Error al actualizar status:", e$message, "\n")
    if(exists("conn")) dbDisconnect(conn)
    return(FALSE)
  })
}

# ==============================================================================
# FUNCIÓN: CARGAR INMUEBLES DESDE SNOWFLAKE
# ==============================================================================

cargar_inmuebles_snowflake <- function(){
  tryCatch({
    conn <- crear_conexion_snowflake()
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    query_join <- "
  SELECT 
    i.ID,
    i.INMUEBLE,
    i.EMPRESA,
    i.ALIAS,
    i.DIRECCION,
    i.MUNICIPIO,
    i.ESTADO,
    i.PAIS,
    i.FECHA_ESCRITURA,
    i.PRECIO_ADQUISICION,
    i.TERRENO_M2,
    i.PRECIO_TERRENO_M2,
    i.CONSTRUCCION_M2,
    i.PRECIO_CONSTRUCCION_M2,
    i.FECHA_ACTUALIZACION,
    s.STATUS,
    s.FECHA_DE_ASIGNACION,
    s.USUARIO
  FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
  LEFT JOIN (
    SELECT ID, STATUS, FECHA_DE_ASIGNACION, USUARIO
    FROM DB_ANALYTICS.SCH_CORE.INMUEBLES_STATUS
    QUALIFY ROW_NUMBER() OVER (
      PARTITION BY ID
      ORDER BY FECHA_DE_ASIGNACION DESC
    ) = 1
  ) s ON i.ID = s.ID
"
    
    inmuebles <- dbGetQuery(conn, query_join)
    dbDisconnect(conn)
    
    if(nrow(inmuebles) == 0) {
      cat("⚠️ No hay inmuebles en Snowflake\n")
      return(data.frame(
        ID = character(0),
        INMUEBLE = character(0),
        EMPRESA = character(0),
        ALIAS = character(0),
        DIRECCION = character(0),
        MUNICIPIO = character(0),
        ESTADO = character(0),
        PAIS = character(0),
        FECHA_ESCRITURA = character(0),
        PRECIO_ADQUISICION = numeric(0),
        TERRENO_M2 = numeric(0),
        PRECIO_TERRENO_M2 = numeric(0),
        CONSTRUCCION_M2 = numeric(0),
        PRECIO_CONSTRUCCION_M2 = numeric(0),
        FECHA_ACTUALIZACION = character(0),
        STATUS = character(0),
        FECHA_DE_ASIGNACION = character(0),
        USUARIO = character(0),
        stringsAsFactors = FALSE
      ))
    }
    
    # Obtener STATUS_ACTIVIDAD
    tryCatch({
      conn2 <- crear_conexion_snowflake()
      dbSendUpdate(conn2, "USE WAREHOUSE WH_ANALYTICS;")
      
      query_actividad <- "
        SELECT 
          ID,
          STATUS_ACTIVIDAD,
          FECHA_ASIGNACION,
          USUARIO as USUARIO_ACTIVIDAD
        FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
        QUALIFY ROW_NUMBER() OVER (
          PARTITION BY ID 
          ORDER BY FECHA_ASIGNACION DESC
        ) = 1"
      
      actividad_data <- dbGetQuery(conn2, query_actividad)
      dbDisconnect(conn2)
      
      if(nrow(actividad_data) > 0) {
        inmuebles <- merge(inmuebles, actividad_data, by = "ID", all.x = TRUE)
        cat("✅ Datos de actividad obtenidos:", nrow(actividad_data), "registros\n")
      } else {
        inmuebles$STATUS_ACTIVIDAD <- "ACTIVO"
        inmuebles$FECHA_ASIGNACION <- as.POSIXct(NA)
        inmuebles$USUARIO_ACTIVIDAD <- NA
        cat("ℹ️ Tabla INMUEBLE_ACTIVIDAD vacía - todos los inmuebles se marcan como ACTIVOS\n")
      }
      
    }, error = function(e) {
      cat("⚠️ Tabla INMUEBLE_ACTIVIDAD no existe - todos los inmuebles se marcan como ACTIVOS\n")
      inmuebles$STATUS_ACTIVIDAD <- "ACTIVO"
      inmuebles$FECHA_ASIGNACION <- as.POSIXct(NA)
      inmuebles$USUARIO_ACTIVIDAD <- NA
    })
    
    # FILTRAR: Solo mostrar inmuebles ACTIVOS
    inmuebles$STATUS_ACTIVIDAD[is.na(inmuebles$STATUS_ACTIVIDAD) | 
                                 inmuebles$STATUS_ACTIVIDAD == ""] <- "Activo"
    
    inmuebles$STATUS_ACTIVIDAD_UPPER <- toupper(trimws(inmuebles$STATUS_ACTIVIDAD))
    inmuebles_activos <- inmuebles[inmuebles$STATUS_ACTIVIDAD_UPPER == "ACTIVO", ]
    inmuebles_activos$STATUS_ACTIVIDAD_UPPER <- NULL
    
    cat("📋 Total de inmuebles en Snowflake:", nrow(inmuebles), "\n")
    cat("✅ Inmuebles ACTIVOS:", nrow(inmuebles_activos), "\n")
    cat("🔴 Inmuebles INACTIVOS:", nrow(inmuebles) - nrow(inmuebles_activos), "\n")
    
    return(inmuebles_activos)
    
  }, error = function(e) {
    cat("❌ Error al cargar INMUEBLES desde Snowflake:", e$message, "\n")
    return(data.frame(
      ID = character(0),
      INMUEBLE = character(0),
      EMPRESA = character(0),
      ALIAS = character(0),
      DIRECCION = character(0),
      MUNICIPIO = character(0),
      ESTADO = character(0),
      PAIS = character(0),
      FECHA_ESCRITURA = character(0),
      PRECIO_ADQUISICION = numeric(0),
      TERRENO_M2 = numeric(0),
      PRECIO_TERRENO_M2 = numeric(0),
      CONSTRUCCION_M2 = numeric(0),
      PRECIO_CONSTRUCCION_M2 = numeric(0),
      FECHA_ACTUALIZACION = character(0),
      STATUS = character(0),
      FECHA_DE_ASIGNACION = character(0),
      USUARIO = character(0),
      stringsAsFactors = FALSE
    ))
  })
}

# ==============================================================================
# INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- fluidPage(
  useShinyjs(),
  title = "STATUS de Inmuebles ALMENA",
  
  tags$head(
    tags$style(HTML("
    /* Estilos generales */
    body {
      background: #e0d4c3;
      font-family: Arial, sans-serif;
    }
    
    /* ============================================
       PANTALLA DE LOGIN
       ============================================ */
    .login-container {
      display: flex;
      justify-content: center;
      align-items: center;
      min-height: 100vh;
      background: linear-gradient(135deg, #f5efe6 0%, #e0d4c3 100%);
    }
    
    .login-box {
      background: white;
      padding: 40px;
      border-radius: 15px;
      box-shadow: 0 10px 40px rgba(230, 126, 34, 0.3);
      border: 3px solid #802e25;
      width: 100%;
      max-width: 400px;
    }
    
    .login-header {
      text-align: center;
      margin-bottom: 30px;
    }
    
    .login-title {
      color: #802e25;
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
      color: #4A2E05;
      font-weight: 600;
      margin-bottom: 8px;
      display: block;
      font-size: 14px;
    }
    
    .login-input {
      width: 100%;
      padding: 12px;
      border: 2px solid #E67E22;
      border-radius: 8px;
      font-size: 14px;
      transition: all 0.3s ease;
    }
    
    .login-input:focus {
      border-color: #802e25;
      box-shadow: 0 0 10px rgba(230, 126, 34, 0.3);
      outline: none;
    }
    
    .login-btn {
      width: 100%;
      background: linear-gradient(135deg, #802e25 0%, #D35400 100%) !important;
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
      background: linear-gradient(135deg, #D35400 0%, #C0510D 100%) !important;
      transform: translateY(-2px);
      box-shadow: 0 6px 20px rgba(230, 126, 34, 0.4);
    }
    
    .login-error {
      background: linear-gradient(135deg, #FFEEE0 0%, #FFDDB3 100%);
      border: 1px solid #802e25;
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
      
      /* Scroll personalizado */
      .dataTables_scrollBody::-webkit-scrollbar {
        width: 10px;
      }
      
      .dataTables_scrollBody::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 10px;
      }
      
      .dataTables_scrollBody::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #802e25 0%, #D35400 100%);
        border-radius: 10px;
      }
      
      .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #D35400 0%, #C0392B 100%);
      }
      
      /* Encabezados de tabla */
      #tabla_status_inmuebles thead th {
        background: linear-gradient(135deg, #E67E22 0%, #802e25 100%) !important;
        color: white !important;
        font-weight: bold !important;
        text-align: center !important;
        padding: 12px !important;
        border: none !important;
      }
      
      /* Filas de tabla */
      #tabla_status_inmuebles tbody tr {
        background-color: white !important;
      }
      
      #tabla_status_inmuebles tbody tr:nth-child(even) {
        background-color: #FFF5F0 !important;
      }
      
      #tabla_status_inmuebles tbody tr:hover {
        background-color: #ff9d92 !important;
      }
      
      #tabla_status_inmuebles tbody td {
        background-color: transparent !important;
        border-bottom: 1px solid #e0e0e0 !important;
      }
      
      /* Buscador */
      #tabla_status_inmuebles_filter input {
        border: 2px solid #802e25 !important;
        border-radius: 6px !important;
        padding: 8px 12px !important;
        font-size: 14px !important;
      }
      
      #tabla_status_inmuebles_filter input:focus {
        border-color: #D35400 !important;
        box-shadow: 0 0 8px rgba(230, 126, 34, 0.4) !important;
        outline: none !important;
      }
      
      #tabla_status_inmuebles_filter label {
        font-weight: bold !important;
        color: #802e25 !important;
        font-size: 15px !important;
      }
      
      /* Botón actualizar */
.btn-actualizar {
  background: linear-gradient(135deg, #E67E22 0%, #D35400 100%) !important;
  color: white !important;
  border: none !important;
  padding: 10px 25px !important;
  border-radius: 6px !important;
  font-weight: bold !important;
  transition: all 0.3s ease !important;
}

.btn-actualizar:hover {
  background: linear-gradient(135deg, #D35400 0%, #C0510D 100%) !important;
  transform: translateY(-2px) !important;
  box-shadow: 0 4px 15px rgba(230, 126, 34, 0.4) !important;
}
      
      /* Botón Actualizar (azul -> temático naranja/terracota) */
.btn-primary {
  background: linear-gradient(135deg, #E67E22 0%, #D35400 100%) !important;
  color: white !important;
  border: none !important;
  transition: all 0.3s ease !important;
  font-weight: bold !important;
}

.btn-primary:hover {
  background: linear-gradient(135deg, #D35400 0%, #C0510D 100%) !important;
  transform: translateY(-2px) !important;
  box-shadow: 0 4px 15px rgba(230, 126, 34, 0.4) !important;
}

/* Botón Cargar KMZ (naranja -> temático) */
.btn-warning {
  background: linear-gradient(135deg, #ff5b49 0%, #E67E22 100%) !important;
  color: white !important;
  border: none !important;
  transition: all 0.3s ease !important;
  font-weight: bold !important;
}

.btn-warning:hover {
  background: linear-gradient(135deg, #E67E22 0%, #D35400 100%) !important;
  transform: translateY(-2px) !important;
  box-shadow: 0 4px 15px rgba(255, 91, 73, 0.4) !important;
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
                "Sistema de STATUS de Inmuebles")
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
      
      # ENCABEZADO
      div(style = "background: linear-gradient(135deg, #802e25 0%, #D35400 100%);
                 padding: 25px;
                 border-radius: 10px;
                 margin-bottom: 20px;
                 box-shadow: 0 4px 15px rgba(230, 126, 34, 0.3);",
          
          h2(style = "color: white; margin: 0; font-weight: bold; text-align: center;",
             " MAESTRO DE INMUEBLES - ESTATUS PREDIO"),
      ),
      
      # CONTENEDOR DE LA TABLA
      div(style = "background: white;
                 padding: 20px;
                 border-radius: 10px;
                 border: 3px solid #802e25;
                 box-shadow: 0 4px 15px rgba(230, 126, 34, 0.2);",
          
          # ENCABEZADO DE LA TABLA
          div(style = "background: linear-gradient(135deg, #E67E22 0%, #802e25 100%);
           padding: 15px;
           border-radius: 8px;
           margin-bottom: 15px;
           display: flex;
           justify-content: space-between;
           align-items: center;",
              
              h4(style = "color: white; margin: 0; font-weight: bold;",
                 " ESTATUS DE INMUEBLES"),
              
              div(style = "display: flex; align-items: center; gap: 10px;",
                  uiOutput("badge_sin_status"),
                  actionButton(
                    "filtro_sin_status",
                    " Ver sin estatus",
                    style = "background: white; color: #802e25; font-weight: bold;
                   border: 2px solid white; padding: 7px 15px; border-radius: 6px;"
                  )
              )
          ),
          
          # TABLA CON SCROLL
          DT::dataTableOutput("tabla_status_inmuebles")
      )
  )
)

# ============================================================================
# FUNCIÓN: Cargar polígonos desde KMZ
# ============================================================================
cargar_poligonos_inmuebles <- function() {
  tryCatch({
    cat("📄 Cargando polígonos desde KMZ...\n")
    
    archivos_kmz <- list.files(pattern = "\\.kmz$", ignore.case = TRUE)
    if (length(archivos_kmz) == 0) return(NULL)
    
    ruta_kmz <- archivos_kmz[1]
    cat("📂 Usando:", ruta_kmz, "\n")
    
    temp_dir <- tempdir()
    unzip(ruta_kmz, exdir = temp_dir, overwrite = TRUE)
    
    kml_files <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE)
    if (length(kml_files) == 0) return(NULL)
    
    capas <- st_layers(kml_files[1])
    poligonos_list <- list()
    
    for (capa in capas$name) {
      capa_sf <- tryCatch(st_read(kml_files[1], layer = capa, quiet = TRUE), error = function(e) NULL)
      if (!is.null(capa_sf) && nrow(capa_sf) > 0) {
        poligonos_list[[length(poligonos_list) + 1]] <- capa_sf
      }
    }
    
    if (length(poligonos_list) == 0) return(NULL)
    
    poligonos <- do.call(rbind, poligonos_list)
    poligonos <- st_transform(poligonos, 4326)
    poligonos <- st_make_valid(poligonos)
    poligonos <- st_zm(poligonos, drop = TRUE, what = "ZM")
    
    centroides <- st_point_on_surface(poligonos)
    coords <- st_coordinates(centroides)
    
    poligonos$LONGITUD <- coords[, 1]
    poligonos$LATITUD <- coords[, 2]
    
    cat("✅ Cargados:", nrow(poligonos), "polígonos\n")
    return(poligonos)
    
  }, error = function(e) {
    cat("❌ Error:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN: Procesar KMZ individual
# ============================================================================
procesar_kmz_individual <- function(ruta_kmz) {
  id_proceso  <- basename(tempfile(pattern = "proc_"))
  dir_proceso <- file.path(tempdir(), id_proceso)
  dir.create(dir_proceso)
  
  tryCatch({
    unzip(ruta_kmz, exdir = dir_proceso, overwrite = TRUE)
    
    kml_files <- list.files(dir_proceso, pattern = "\\.kml$",
                            full.names = TRUE, recursive = TRUE)
    if (length(kml_files) == 0) {
      unlink(dir_proceso, recursive = TRUE)
      return(NULL)
    }
    
    sf::sf_use_s2(FALSE)
    
    poligono_sf <- st_read(kml_files[1], quiet = TRUE)
    poligono_sf <- st_transform(poligono_sf, 4326)
    poligono_sf <- st_make_valid(poligono_sf)
    poligono_sf <- st_zm(poligono_sf, drop = TRUE, what = "ZM")
    
    centroide <- st_point_on_surface(poligono_sf)
    coords    <- st_coordinates(centroide)
    
    poligono_sf$LONGITUD <- coords[, 1]
    poligono_sf$LATITUD  <- coords[, 2]
    
    sf::sf_use_s2(TRUE)
    unlink(dir_proceso, recursive = TRUE)
    return(poligono_sf)
    
  }, error = function(e) {
    sf::sf_use_s2(TRUE)
    if (dir.exists(dir_proceso)) unlink(dir_proceso, recursive = TRUE)
    cat("Error procesando KMZ:", e$message, "\n")
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN: Subir KMZ a Snowflake
# ============================================================================
subir_kmz_a_snowflake <- function(ruta_archivo_local, id_inmueble, nombre_archivo_original = NULL) {
  tryCatch({
    conn <- crear_conexion_snowflake()
    
    dbSendUpdate(conn, "USE DATABASE DB_ANALYTICS")
    dbSendUpdate(conn, "USE SCHEMA SCH_CORE")
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS")
    
    objeto_conexion_JDBC <- conn@jc
    
    if (!is.null(nombre_archivo_original) && nombre_archivo_original != "") {
      nombre_archivo <- nombre_archivo_original
    } else {
      id_limpio <- gsub("[^A-Za-z0-9_-]", "_", trimws(id_inmueble))
      nombre_archivo <- paste0(id_limpio, ".kmz")
    }
    
    cat("Nombre del archivo a subir:", nombre_archivo, "\n")
    
    temp_dir <- tempdir()
    ruta_temp_con_nombre <- file.path(temp_dir, nombre_archivo)
    file.copy(ruta_archivo_local, ruta_temp_con_nombre, overwrite = TRUE)
    
    id_limpio <- gsub("[^A-Za-z0-9_-]", "_", trimws(id_inmueble))
    
    ruta_normalizada <- gsub("\\\\", "/", normalizePath(ruta_temp_con_nombre))
    ruta_para_snowflake <- paste0("file://", ruta_normalizada)
    ruta_stage <- paste0("@STAGE_KMZ_INMUEBLES/", id_limpio, "/")
    
    query_put <- paste0(
      "PUT '", ruta_para_snowflake, 
      "' ", ruta_stage,
      " OVERWRITE = TRUE AUTO_COMPRESS = FALSE"
    )
    
    cat("PUT:", query_put, "\n")
    
    statement <- .jcall(objeto_conexion_JDBC, "Ljava/sql/Statement;", "createStatement")
    resultado <- .jcall(statement, "Ljava/sql/ResultSet;", "executeQuery", query_put)
    .jcall(statement, "V", "close")
    
    cat("Archivo subido con nombre:", nombre_archivo, "\n")
    
    ruta_kmz <- paste0("@STAGE_KMZ_INMUEBLES/", id_limpio, "/", nombre_archivo)
    
    # Verificar si ya existe
    existe_sql <- sprintf(
      "SELECT COUNT(*) as COUNT FROM DB_ANALYTICS.SCH_CORE.KMZ_ARCHIVOS WHERE ID = '%s'",
      gsub("'", "''", trimws(id_inmueble))
    )
    resultado_existe <- dbGetQuery(conn, existe_sql)
    
    if (resultado_existe$COUNT > 0) {
      update_sql <- sprintf(
        "UPDATE DB_ANALYTICS.SCH_CORE.KMZ_ARCHIVOS 
         SET RUTA_KMZ = '%s', 
             NOMBRE_ARCHIVO = '%s',
             FECHA_CARGA = CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ
         WHERE ID = '%s'",
        ruta_kmz,
        gsub("'", "''", nombre_archivo),
        gsub("'", "''", trimws(id_inmueble))
      )
      dbSendUpdate(conn, update_sql)
      cat("Registro actualizado en KMZ_ARCHIVOS\n")
    } else {
      insert_sql <- sprintf(
        "INSERT INTO DB_ANALYTICS.SCH_CORE.KMZ_ARCHIVOS (ID, RUTA_KMZ, NOMBRE_ARCHIVO, FECHA_CARGA)
         VALUES ('%s', '%s', '%s', CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ)",
        gsub("'", "''", trimws(id_inmueble)),
        ruta_kmz,
        gsub("'", "''", nombre_archivo)
      )
      dbSendUpdate(conn, insert_sql)
      cat("Registro insertado en KMZ_ARCHIVOS\n")
    }
    
    unlink(ruta_temp_con_nombre)
    dbDisconnect(conn)
    return(TRUE)
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================================
# FUNCION: Guardar geometria en INMUEBLES_GEOMETRIA
# ============================================================================
guardar_geometria_snowflake <- function(id_inmueble, sf_objeto) {
  tryCatch({
    conn <- crear_conexion_snowflake()
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    wkt_text <- st_as_text(st_geometry(st_make_valid(sf_objeto))[[1]])
    wkt_text <- gsub("'", "''", wkt_text)
    lat <- sf_objeto$LATITUD[1]
    lng <- sf_objeto$LONGITUD[1]
    
    sql <- sprintf("
      MERGE INTO DB_ANALYTICS.SCH_CORE.INMUEBLES_GEOMETRIA t
      USING (SELECT '%s' as ID_VAL, '%s' as WKT, %f as LAT, %f as LNG) s
      ON (t.ID = s.ID_VAL)
      WHEN MATCHED THEN
        UPDATE SET GEOMETRIA_WKT = s.WKT,
                   LATITUD_CENTROIDE = s.LAT,
                   LONGITUD_CENTROIDE = s.LNG,
                   FECHA_ACTUALIZACION = CURRENT_TIMESTAMP()
      WHEN NOT MATCHED THEN
        INSERT (ID, GEOMETRIA_WKT, LATITUD_CENTROIDE, LONGITUD_CENTROIDE)
        VALUES (s.ID_VAL, s.WKT, s.LAT, s.LNG);",
                   gsub("'", "''", trimws(id_inmueble)), wkt_text, lat, lng
    )
    
    dbSendUpdate(conn, sql)
    dbDisconnect(conn)
    cat("Geometria guardada en INMUEBLES_GEOMETRIA para:", id_inmueble, "\n")
    return(TRUE)
  }, error = function(e) {
    cat("Error al guardar geometria:", e$message, "\n")
    return(FALSE)
  })
}

# ============================================================================
# FUNCIÓN: Datos del mapa con polígonos enriquecidos
# ============================================================================
datos_mapa_inmuebles <- function() {
  tryCatch({
    conn <- crear_conexion_snowflake()
    dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    
    query_individuales <- "
      SELECT
        k.ID,
        k.NOMBRE_ARCHIVO,
        'KMZ_INDIVIDUAL' as FUENTE,
        k.FECHA_CARGA,
        k.RUTA_KMZ,
        i.INMUEBLE,
        i.EMPRESA,
        i.DIRECCION,
        i.MUNICIPIO
      FROM DB_ANALYTICS.SCH_CORE.KMZ_ARCHIVOS k
      LEFT JOIN DB_ANALYTICS.SCH_CORE.INMUEBLES i
        ON k.ID = i.ID
      ORDER BY k.FECHA_CARGA DESC
    "
    
    resultado <- dbGetQuery(conn, query_individuales)
    dbDisconnect(conn)
    
    if (nrow(resultado) == 0) {
      cat("No hay KMZ registrados en KMZ_ARCHIVOS\n")
      return(NULL)
    }
    
    cat("KMZ registrados:", nrow(resultado), "\n")
    return(resultado)
    
  }, error = function(e) {
    cat("Error al cargar datos del mapa:", e$message, "\n")
    return(NULL)
  })
}

# ==============================================================================
# SERVIDOR (SERVER)
# ==============================================================================

server <- function(input, output, session) {
  
  # ==========================================================================
  # SISTEMA DE AUTENTICACIÓN
  # ==========================================================================
  
  # Usuarios y contraseñas hardcodeados
  usuarios_validos <- list(
    "sofia.ibarra@almena.mx" = "A7F3K9M2Q8",
    "paula.serna@almena.mx" = "B9K2M7Q4A8",
    "miguel.rios@almena.mx" = "F3A8Q6K9M2",
    "1" = "1"
  )
  
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
    if (usuario %in% names(usuarios_validos) && 
        usuarios_validos[[usuario]] == password) {
      
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
  
  # Variable reactiva para polígonos
  poligonos_inmuebles <- reactiveVal(NULL)
  
  # Cargar polígonos al iniciar
  observe({
    if (is.null(poligonos_inmuebles())) {
      poligonos_sf <- cargar_poligonos_inmuebles()
      poligonos_inmuebles(poligonos_sf)
    }
  })
  
  # Datos enriquecidos del mapa
  datos_mapa_inmuebles_reactivo <- reactive({
    trigger_recarga_kmz()
    datos_mapa_inmuebles()
  })
  
  # Variable reactiva para almacenar inmuebles
  inmuebles_snowflake_df <- reactiveVal(NULL)
  trigger_recarga_kmz <- reactiveVal(0)
  mostrar_solo_sin_status <- reactiveVal(FALSE)
  
  # Cargar datos al iniciar
  observe({
    if(is.null(inmuebles_snowflake_df())) {
      cat("📄 Cargando inmuebles al iniciar app...\n")
      datos <- cargar_inmuebles_snowflake()
      inmuebles_snowflake_df(datos)
      
      if(!is.null(datos) && nrow(datos) > 0) {
        showNotification("✅ INMUEBLES ACTIVOS cargados correctamente", 
                         type = "message", duration = 2)
      }
    }
  })
  
  # OUTPUT: TABLA STATUS INMUEBLES
  output$tabla_status_inmuebles <- DT::renderDataTable({
    datos <- isolate(inmuebles_snowflake_df())
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay datos disponibles en Snowflake"))
    }
    
    if (mostrar_solo_sin_status()) {
      datos <- datos[is.na(datos$STATUS) | trimws(datos$STATUS) == "", ]
      if (nrow(datos) == 0) {
        return(data.frame(Mensaje = "¡Todos los inmuebles tienen estatus asignado!"))
      }
    }
    
    datos_tabla <- datos
    
    # FORMATEAR FECHAS
    if ("FECHA_DE_ASIGNACION" %in% names(datos_tabla)) {
      datos_tabla$FECHA_DE_ASIGNACION <- sapply(datos_tabla$FECHA_DE_ASIGNACION, function(fecha) {
        if (is.na(fecha) || length(fecha) == 0) {
          return("-")
        }
        
        fecha_char <- as.character(fecha)
        if (fecha_char == "" || fecha_char == "-") {
          return("-")
        }
        
        tryCatch({
          if (is.numeric(fecha) || grepl("^[0-9]+$", fecha_char)) {
            fecha_num <- as.numeric(fecha_char)
            
            if (fecha_num > 1e12) {
              fecha_utc <- as.POSIXct(fecha_num / 1000, origin = "1970-01-01", tz = "UTC")
            } else {
              fecha_utc <- as.POSIXct(fecha_num, origin = "1970-01-01", tz = "UTC")
            }
            
            fecha_mty <- format(fecha_utc, "%Y-%m-%d %H:%M:%S", tz = "America/Mexico_City")
            return(fecha_mty)
          }
          
          fecha_limpia <- gsub("T", " ", fecha_char)
          fecha_limpia <- gsub("Z", "", fecha_limpia)
          fecha_limpia <- gsub("\\..*", "", fecha_limpia)
          
          fecha_utc <- as.POSIXct(fecha_limpia, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
          fecha_mty <- format(fecha_utc, "%Y-%m-%d %H:%M:%S", tz = "America/Mexico_City")
          
          return(fecha_mty)
          
        }, error = function(e) {
          return(fecha_char)
        })
      })
    }
    
    # CREAR DROPDOWN DE STATUS
    datos_tabla$STATUS_DROPDOWN <- sapply(1:nrow(datos_tabla), function(i) {
      status_actual <- datos_tabla$STATUS[i]
      if (is.na(status_actual) || status_actual == "") {
        status_actual <- ""
      }
      
      id_inmueble <- datos_tabla$ID[i]
      paste0(
        '<select class="form-control status-selector" ',
        'data-id="', id_inmueble, '" ',
        'onchange="Shiny.setInputValue(\'status_changed\', {id: \'', id_inmueble, '\', status: this.value, random: Math.random()}, {priority: \'event\'});" ',
        'style="width: 100%; font-size: 11px; font-weight: bold; padding: 5px; min-width: 200px; white-space: normal;">',
        '<option value="" ', ifelse(status_actual == "", 'selected', ''), ' style="color: #95a5a6;">-- Seleccionar --</option>',
        '<option value="SIN ASIGNAR" ', ifelse(status_actual == "SIN ASIGNAR", 'selected', ''), ' style="color: #7f8c8d;">SIN ASIGNAR</option>',
        '<option value="IMPRODUCTIVO" ', ifelse(status_actual == "IMPRODUCTIVO", 'selected', ''), ' style="color: #e74c3c;">IMPRODUCTIVO</option>',
        '<option value="IMPRODUCTIVO CON POTENCIAL" ', ifelse(status_actual == "IMPRODUCTIVO CON POTENCIAL", 'selected', ''), ' style="color: #E67E22;">IMPRODUCTIVO CON POTENCIAL</option>',
        '<option value="POR DESINCORPORAR" ', ifelse(status_actual == "POR DESINCORPORAR", 'selected', ''), ' style="color: #802e25;">POR DESINCORPORAR</option>',
        '<option value="PRODUCTIVO" ', ifelse(status_actual == "PRODUCTIVO", 'selected', ''), ' style="color: #27ae60;">PRODUCTIVO</option>',
        '<option value="PROPUESTA INVERSION" ', ifelse(status_actual == "PROPUESTA INVERSION", 'selected', ''), ' style="color: #3498db;">PROPUESTA INVERSION</option>',
        '<option value="PROPUESTA POR DESINCORPORAR" ', ifelse(status_actual == "PROPUESTA POR DESINCORPORAR", 'selected', ''), ' style="color: #9b59b6;">PROPUESTA POR DESINCORPORAR</option>',
        '</select>'
      )
    })
    
    # NO usar isolate() - queremos que sea reactivo
    kmz_registrados <- isolate(datos_mapa_inmuebles_reactivo())
    
    # Crear la columna KMZ con NOMBRE DEL ARCHIVO
    datos_tabla$KMZ_COLUMN <- sapply(1:nrow(datos_tabla), function(i) {
      id_inmueble <- datos_tabla$ID[i]
      
      tiene_kmz <- FALSE
      nombre_archivo <- ""
      
      # Verificar si existe KMZ EN LA TABLA DE SNOWFLAKE
      if (!is.null(kmz_registrados) && nrow(kmz_registrados) > 0) {
        idx <- which(kmz_registrados$ID == id_inmueble)
        if (length(idx) > 0) {
          tiene_kmz <- TRUE
          nombre_archivo <- kmz_registrados$NOMBRE_ARCHIVO[idx[1]]
        }
      }
      
      if (tiene_kmz) {
        paste0(
          '<div style="text-align: center; display: flex; flex-direction: column; align-items: center; gap: 5px;">',
          '<span style="color: #27ae60; font-size: 18px; font-weight: bold;">✅</span>',
          '<small style="color: #27ae60; font-weight: bold; margin-bottom: 3px;">Cargado</small>',
          '<small style="color: #555; font-size: 10px; margin-bottom: 3px; max-width: 150px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap;" title="', nombre_archivo, '">📄 ', nombre_archivo, '</small>',
          '<button class="btn btn-primary btn-xs" ',
          'onclick="Shiny.setInputValue(\'cargar_kmz_inmueble\', \'', id_inmueble, '_', runif(1), '\', {priority: \'event\'});" ',
          'style="font-size: 10px; padding: 3px 6px; white-space: nowrap; background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); border: none;">',
          '<i class="fa fa-edit"></i> Actualizar</button>',
          '</div>'
        )
      } else {
        # Mostrar botón "Cargar KMZ"
        paste0(
          '<button class="btn btn-warning btn-xs" ',
          'onclick="Shiny.setInputValue(\'cargar_kmz_inmueble\', \'', id_inmueble, '_', runif(1), '\', {priority: \'event\'});" ',
          'style="font-size: 11px; padding: 4px 8px; white-space: nowrap;">',
          '<i class="fa fa-upload"></i> Cargar KMZ</button>'
        )
      }
    })
    
    # Eliminar columna STATUS original
    datos_tabla$STATUS <- NULL
    columnas_orden <- c("ID", "INMUEBLE", "EMPRESA", "ALIAS", "DIRECCION", 
                        "MUNICIPIO", "ESTADO", "PAIS", "FECHA_ESCRITURA",
                        "PRECIO_ADQUISICION", "TERRENO_M2", "PRECIO_TERRENO_M2",
                        "CONSTRUCCION_M2", "PRECIO_CONSTRUCCION_M2",
                        "STATUS_DROPDOWN", "KMZ_COLUMN")
    
    datos_tabla <- datos_tabla[, columnas_orden[columnas_orden %in% names(datos_tabla)]]
    
    # Renombrar columnas para visualización
    names(datos_tabla) <- c(
      "ID", "Inmueble", "Empresa", "Alias", "Dirección",
      "Municipio", "Estado", "País", "Fecha Escritura",
      "Precio Adquisición", "Terreno (m²)", "Precio Terreno/m²",
      "Construcción (m²)", "Precio Construcción/m²",
      "Status", "KMZ"  
    )
    
    # Renderizar DataTable
    DT::datatable(
      datos_tabla,
      escape = FALSE,  
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "60vh",
        searching = TRUE,
        ordering = TRUE,
        
        columnDefs = list(
          list(targets = 0, width = "90px"),
          list(targets = 14, width = "220px", orderable = FALSE),  # Status - ancho fijo visible
          list(targets = 15, width = "120px", orderable = FALSE, className = "text-center"),  
          list(targets = c(8, 9, 11, 13), visible = FALSE)
        ),
        
        language = list(
          lengthMenu = "Mostrar _MENU_ registros",
          zeroRecords = "No se encontraron inmuebles",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          search = "Buscar:"
        )
      ),
      rownames = FALSE,
      class = 'table table-striped table-hover'
    )
  })
  
  # OBSERVEEVENT: CAMBIO DE STATUS DESDE DROPDOWN
  observeEvent(input$status_changed, {
    cambio <- input$status_changed
    if (!is.null(cambio) && !is.null(cambio$id) && !is.null(cambio$status)) {
      id_inmueble <- cambio$id
      nuevo_status <- cambio$status
      
      if (nuevo_status == "") {
        showNotification("⚠️ Debe seleccionar un status válido", type = "warning", duration = 3)
        return()
      }
      
      showNotification(
        paste0("🔄 Actualizando status a: ", nuevo_status, "..."),
        type = "message",
        duration = 2
      )
      
      resultado <- actualizar_status_snowflake(
        id_inmueble = id_inmueble,
        nuevo_status = nuevo_status,
        usuario = "SISTEMA"
      )
      
      if (resultado) {
        # Actualizar el valor del dropdown en el DOM via JS (sin re-render de tabla)
        shinyjs::runjs(sprintf(
          "var selects = document.querySelectorAll('.status-selector');
           selects.forEach(function(s) {
             if (s.getAttribute('data-id') === '%s') {
               s.value = '%s';
             }
           });",
          gsub("'", "\\'", id_inmueble),
          gsub("'", "\\'", nuevo_status)
        ))
        
        # Actualizar datos en memoria en segundo plano (sin disparar re-render)
        datos_actualizados <- cargar_inmuebles_snowflake()
        isolate(inmuebles_snowflake_df(datos_actualizados))
        
        showNotification(
          paste0("✅ Status actualizado a: ", nuevo_status),
          type = "message",
          duration = 3
        )
        cat("✅ Status actualizado para inmueble:", id_inmueble, "->", nuevo_status, "\n")
      } else {
        showNotification("❌ Error al actualizar el status", type = "error", duration = 4)
      }
    }
  })
  
  observeEvent(input$filtro_sin_status, {
    nuevo_estado <- !mostrar_solo_sin_status()
    mostrar_solo_sin_status(nuevo_estado)
    
    label <- if (nuevo_estado) " Ver todos" else " Ver sin estatus"
    shinyjs::runjs(sprintf(
      "$('#filtro_sin_status').text('%s');", label
    ))
  })
  
  output$badge_sin_status <- renderUI({
    datos <- inmuebles_snowflake_df()
    if (is.null(datos)) return(NULL)
    n <- sum(is.na(datos$STATUS) | trimws(datos$STATUS) == "", na.rm = TRUE)
    if (n == 0) return(NULL)
    div(style = "background: white; color: #802e25; font-weight: bold;
               padding: 5px 12px; border-radius: 12px; font-size: 13px;",
        paste0( n, " sin estatus"))
  })
  
  # BOTÓN ACTUALIZAR DATOS
  observeEvent(input$actualizar_status, {
    showNotification("🔄 Actualizando datos desde Snowflake...", 
                     type = "message", duration = 2)
    
    datos <- cargar_inmuebles_snowflake()
    inmuebles_snowflake_df(datos)
    
    if(!is.null(datos) && nrow(datos) > 0) {
      showNotification("✅ Datos actualizados exitosamente", 
                       type = "message", duration = 3)
    } else {
      showNotification("⚠️ Error al actualizar los datos", 
                       type = "warning", duration = 3)
    }
  })
  
  # ============================================================================
  # OBSERVEEVENT: Cargar KMZ para inmueble
  # ============================================================================
  observeEvent(input$cargar_kmz_inmueble, {
    # Extraer ID (remover timestamp)
    id_inmueble <- gsub("_[0-9\\.]+$", "", input$cargar_kmz_inmueble)
    
    if (!is.null(id_inmueble) && id_inmueble != "") {
      
      poligonos_actuales <- poligonos_inmuebles()
      tiene_kmz_previo <- FALSE
      
      if (!is.null(poligonos_actuales) && nrow(poligonos_actuales) > 0) {
        if ("ID" %in% names(poligonos_actuales)) {
          tiene_kmz_previo <- id_inmueble %in% poligonos_actuales$ID
        }
      }
      
      # Título dinámico según si ya tiene KMZ o no
      titulo_modal <- if (tiene_kmz_previo) {
        div(style = "color: #3498db; font-weight: bold; font-size: 18px;",
            "🔄 Actualizar KMZ del Inmueble")
      } else {
        div(style = "color: #E67E22; font-weight: bold; font-size: 18px;",
            " Cargar KMZ para Inmueble")
      }
      
      showModal(modalDialog(
        title = titulo_modal,
        
        div(style = "padding: 15px;",
            div(style = "background: linear-gradient(135deg, #f5efe6 0%, #e0d4c3 100%);
                       padding: 15px;
                       border-radius: 8px;
                       border-left: 4px solid #E67E22;
                       margin-bottom: 20px;",
                h5(style = "color: #4A2E05; margin: 0 0 10px 0;", " Inmueble seleccionado:"),
                p(style = "margin: 0; color: #401712; font-weight: bold;", 
                  paste("ID:", id_inmueble))
            ),
            
            fileInput("kmz_upload_modal", 
                      label = "Seleccionar archivo KMZ:",
                      accept = c(".kmz"),
                      placeholder = "Ningún archivo seleccionado",
                      buttonLabel = " Adjuntar",
                      width = "100%"),
            
            uiOutput("kmz_modal_status"),
            
            div(style = "background: #d1ecf1; 
                       padding: 10px; 
                       border-radius: 5px; 
                       border-left: 4px solid #0c5460;
                       margin-top: 10px;",
                p(style = "margin: 0; color: #0c5460; font-size: 12px;",
                  "ℹ️ El archivo KMZ debe contener el polígono del inmueble")
            )
        ),
        
        footer = tagList(
          modalButton("❌ Cancelar"),
          actionButton("confirmar_kmz_upload", 
                       "✅ Guardar KMZ", 
                       class = "btn",
                       style = "background: linear-gradient(135deg, #E67E22 0%, #802e25 100%);
                              color: white; 
                              border: none;
                              font-weight: bold;
                              padding: 10px 25px;")
        ),
        
        easyClose = FALSE,
        size = "m"
      ))
      
      # Guardar ID para usar en confirmar
      session$userData$id_inmueble_kmz <- id_inmueble
    }
  })
  
  # ============================================================================
  # OBSERVEEVENT: Confirmar carga de KMZ
  # ============================================================================
  observeEvent(input$confirmar_kmz_upload, {
    
    id_inmueble <- session$userData$id_inmueble_kmz
    
    if (is.null(input$kmz_upload_modal) || is.null(input$kmz_upload_modal$datapath)) {
      showNotification("⚠️ Debe seleccionar un archivo KMZ", type = "warning", duration = 3)
      return()
    }
    
    # Cerrar modal ANTES de procesar
    removeModal()
    
    showNotification(" Subiendo KMZ a Snowflake...", type = "message", duration = 2)
    
    nombre_original <- input$kmz_upload_modal$name
    cat(" Nombre original del archivo:", nombre_original, "\n")
    
    # SUBIR A SNOWFLAKE
    resultado_subida <- subir_kmz_a_snowflake(
      ruta_archivo_local = input$kmz_upload_modal$datapath,
      id_inmueble = id_inmueble,
      nombre_archivo_original = nombre_original  
    )
    
    if (resultado_subida) {
      
      # Extraer geometria y guardar en INMUEBLES_GEOMETRIA
      sf_temp <- procesar_kmz_individual(input$kmz_upload_modal$datapath)
      if (!is.null(sf_temp)) {
        guardar_geometria_snowflake(id_inmueble, sf_temp)
        cat("Geometria actualizada en INMUEBLES_GEOMETRIA para:", id_inmueble, "\n")
      } else {
        cat("No se pudo extraer geometria del KMZ para:", id_inmueble, "\n")
      }
      
      # 1. Incrementar trigger PRIMERO
      trigger_recarga_kmz(trigger_recarga_kmz() + 1)
      
      # 2. Recargar datos de inmuebles
      datos_actualizados <- cargar_inmuebles_snowflake()
      inmuebles_snowflake_df(datos_actualizados)
      
      cat("Tabla actualizada despues de subir KMZ\n")
      cat("Trigger de recarga activado\n")
      
      showNotification("KMZ guardado en Snowflake exitosamente", 
                       type = "message", duration = 3)
      
    } else {
      showNotification("Error al subir KMZ", type = "error", duration = 3)
    }
    
    session$userData$id_inmueble_kmz <- NULL
  })
}

# ==============================================================================
# EJECUTAR APLICACIÓN
# ==============================================================================
shinyApp(ui = ui, server = server)

