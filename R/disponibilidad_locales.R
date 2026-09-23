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
# FUNCIÓN: Crear Conexión a Snowflake con JWT (Rutas Dinámicas)
# ============================================================================
crear_conexion_snowflake <- function(usuario = "SVC_ANALYTICS", 
                                     nombre_pem = "SVC_ANALYTICS_key_pk8.pem") {
  tryCatch({
    directorio_base <- tryCatch({
      dirname(rstudioapi::getSourceEditorContext()$path)
    }, error = function(e) {
      getwd()
    })
    
    if (is.null(directorio_base) || directorio_base == "") {
      directorio_base <- getwd()
    }
    
    ruta_jar <- file.path(directorio_base, "snowflake-jdbc-3.25.1.jar")
    ruta_pem_abs <- file.path(directorio_base, nombre_pem)
    
    cat("Directorio base:", directorio_base, "\n")
    cat("Buscando JAR en:", ruta_jar, "\n")
    cat("Buscando PEM en:", ruta_pem_abs, "\n")
    
    if(!file.exists(ruta_jar)) {
      archivos_disponibles <- list.files(directorio_base)
      stop(paste("No se encontró el JAR. Archivos disponibles:", paste(archivos_disponibles, collapse = ", ")))
    }
    
    if(!file.exists(ruta_pem_abs)) {
      stop("No se encontró el archivo PEM")
    }
    
    driver <- RJDBC::JDBC(
      driverClass = "net.snowflake.client.jdbc.SnowflakeDriver", 
      classPath = ruta_jar, 
      identifier.quote = "`"
    )
    
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
    
    .jcall(propiedades, "Ljava/lang/Object;", "put", 
           .jcast(.jnew("java/lang/String", "privateKey"), "java/lang/Object"),
           .jcast(private_key, "java/lang/Object"))
    
    jconn <- .jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url_conexion, propiedades)
    
    if (is.jnull(jconn)) stop("La conexión retornó null")
    
    conexion <- new("JDBCConnection", jc = jconn, identifier.quote = driver@identifier.quote)
    
    cat("Conexión exitosa a Snowflake (JWT)\n")
    return(conexion)
    
  }, error = function(e) {
    message(paste("Error de Conexión:", e$message))
    return(NULL)
  })
}

# ============================================================================
# FUNCIÓN: Validar Disponibilidad de Metros en Locales
# ============================================================================
validar_disponibilidad <- function(conexion, id_local, metros_solicitados) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    query <- sprintf("
      SELECT 
        M2_TOTALES,
        M2_DISPONIBLES
      FROM DB_ANALYTICS.SCH_CORE.VW_DISPONIBILIDAD
      WHERE ID_LOCAL = '%s'", id_local)
    
    cat("Query validación:", query, "\n")
    resultado <- dbGetQuery(conexion, query)
    
    if (nrow(resultado) == 0) {
      return(list(valido = FALSE, mensaje = paste0("El local ", id_local, " no existe"), disponibles = 0))
    }
    
    metros_disponibles <- as.numeric(resultado$M2_DISPONIBLES[1])
    metros_totales <- as.numeric(resultado$M2_TOTALES[1])
    
    if (metros_solicitados > metros_disponibles) {
      return(list(valido = FALSE, mensaje = paste0("Solo hay ", sprintf("%.2f", metros_disponibles), " m² disponibles"), disponibles = metros_disponibles))
    }
    
    return(list(valido = TRUE, mensaje = "Disponibilidad confirmada", disponibles = metros_disponibles))
    
  }, error = function(e) {
    cat("Error en validación:", e$message, "\n")
    return(list(valido = FALSE, mensaje = paste0("Error al validar: ", e$message), disponibles = 0))
  })
}

# ============================================================================
# FUNCIÓN: Generar ID_COMERCIAL dinámico (ÚNICO)
# ============================================================================
generar_id_comercial <- function(locales_vector, conexion) {
  locales_ordenados <- sort(locales_vector)
  locales_str <- paste(locales_ordenados, collapse = "_")
  
  max_intentos <- 100
  for (i in 1:max_intentos) {
    random_num <- sample(1:10000, 1)
    id_propuesto <- paste0(locales_str, "_", random_num)
    
    query <- sprintf(
      "SELECT COUNT(*) AS existe FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES WHERE ID_COMERCIAL = '%s'",
      id_propuesto
    )
    resultado <- dbGetQuery(conexion, query)
    
    if (resultado$EXISTE[1] == 0) {
      return(id_propuesto)
    }
  }
  
  return(paste0(locales_str, "_", as.integer(Sys.time())))
}

# ============================================================================
# FUNCIÓN: Insertar Registro (SIN MODIFICACIONES)
# ============================================================================
insertar_registro <- function(conexion, id_comercial, id_local, cliente, metros, nivel = "PB", plaza = "TEC") {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    query <- sprintf(
      "INSERT INTO DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES 
       (ID_COMERCIAL, ID_LOCAL, M2_OCUPADOS, CLIENTE, NIVEL, FECHA_ACTUALIZACION, PLAZA) 
       VALUES ('%s', '%s', %.2f, '%s', '%s', CURRENT_DATE, '%s')",
      id_comercial, id_local, metros, cliente, nivel, plaza
    )
    dbSendUpdate(conexion, query)
    return(list(exito = TRUE, mensaje = "Registro insertado correctamente"))
  }, error = function(e) {
    return(list(exito = FALSE, mensaje = paste0("Error al insertar: ", e$message)))
  })
}

# ============================================================================
# FUNCIÓN: Liberar Local (SIN MODIFICACIONES)
# ============================================================================
liberar_local <- function(conexion, id_comercial_a_liberar, id_local_especifico, metros_a_liberar, plaza = "TEC") {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    query_info <- sprintf("
      SELECT MAX(CLIENTE) as CLIENTE, MAX(NIVEL) as NIVEL
      FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
      WHERE ID_COMERCIAL = '%s' AND ID_LOCAL = '%s'
      GROUP BY ID_LOCAL", id_comercial_a_liberar, id_local_especifico)
    
    info_registro <- dbGetQuery(conexion, query_info)
    
    if(nrow(info_registro) == 0) return(list(exito = FALSE, mensaje = "No se encontró el registro"))
    
    query_insert <- sprintf(
      "INSERT INTO DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES 
       (ID_COMERCIAL, ID_LOCAL, M2_OCUPADOS, CLIENTE, NIVEL, FECHA_ACTUALIZACION, PLAZA) 
       VALUES ('%s', '%s', %.2f, '%s', '%s', CURRENT_DATE, '%s')",
      id_comercial_a_liberar, 
      id_local_especifico, 
      -abs(metros_a_liberar),
      info_registro$CLIENTE[1], 
      info_registro$NIVEL[1],
      plaza
    )
    
    dbSendUpdate(conexion, query_insert) 
    
    return(list(exito = TRUE, mensaje = "Liberación registrada con éxito"))
    
  }, error = function(e) {
    return(list(exito = FALSE, mensaje = paste0("Error técnico: ", e$message)))
  })
}

# ============================================================================
# FUNCIÓN: Obtener Lista de Plazas (desde tabla LOCALES)
# ============================================================================
obtener_plazas <- function(conexion) {
  tryCatch({
    if (is.null(conexion)) return(c("TEC"))
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    query <- "
      SELECT DISTINCT PLAZA
      FROM DB_ANALYTICS.SCH_CORE.LOCALES
      WHERE PLAZA IS NOT NULL AND PLAZA != ''
      ORDER BY PLAZA"
    resultado <- dbGetQuery(conexion, query)
    if (nrow(resultado) == 0) return(c("TEC"))
    return(resultado$PLAZA)
  }, error = function(e) {
    cat("Error en obtener_plazas:", e$message, "\n")
    return(c("TEC"))
  })
}

# ============================================================================
# FUNCIÓN: Obtener Lista de Locales
# ============================================================================
obtener_locales_disponibles <- function(conexion, plaza = NULL) {
  tryCatch({
    if (is.null(conexion)) return(c("Error: Sin conexión"))
    
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    filtro_plaza <- if (!is.null(plaza) && plaza != "") {
      sprintf("AND L.PLAZA = '%s'", plaza)
    } else ""
    
    query <- sprintf("
      SELECT V.ID_LOCAL 
      FROM DB_ANALYTICS.SCH_CORE.VW_DISPONIBILIDAD V
      INNER JOIN DB_ANALYTICS.SCH_CORE.LOCALES L ON V.ID_LOCAL = L.ID_LOCAL
      WHERE V.M2_DISPONIBLES > 0.05 %s
      ORDER BY V.ID_LOCAL", filtro_plaza)
    
    resultado <- dbGetQuery(conexion, query)
    
    if (nrow(resultado) == 0) return(c("No hay locales con espacio"))
    
    return(resultado$ID_LOCAL)
    
  }, error = function(e) {
    cat("Error en obtener_locales_disponibles:", e$message, "\n")
    return(c("Error al cargar locales"))
  })
}

# ============================================================================
# FUNCIÓN: Obtener Datos Disponibilidad (Desde Vista)
# ============================================================================
obtener_disponibilidad <- function(conexion, plaza = NULL) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    filtro_plaza <- if (!is.null(plaza) && plaza != "") {
      sprintf("INNER JOIN DB_ANALYTICS.SCH_CORE.LOCALES L ON V.ID_LOCAL = L.ID_LOCAL AND L.PLAZA = '%s'", plaza)
    } else {
      "LEFT JOIN DB_ANALYTICS.SCH_CORE.LOCALES L ON V.ID_LOCAL = L.ID_LOCAL"
    }
    
    query <- sprintf("
      SELECT 
        V.ID_LOCAL,
        V.M2_TOTALES,
        V.M2_USADOS,
        V.M2_DISPONIBLES,
        V.CLIENTE_ACTUAL
      FROM DB_ANALYTICS.SCH_CORE.VW_DISPONIBILIDAD V
      %s
      ORDER BY V.ID_LOCAL
    ", filtro_plaza)
    resultado <- dbGetQuery(conexion, query)
    
    if (nrow(resultado) > 0) {
      resultado$M2_TOTALES <- as.numeric(resultado$M2_TOTALES)
      resultado$M2_USADOS <- as.numeric(resultado$M2_USADOS)
      resultado$M2_DISPONIBLES <- as.numeric(resultado$M2_DISPONIBLES)
    }
    
    return(resultado)
  }, error = function(e) {
    cat("Error al obtener disponibilidad:", e$message, "\n")
    return(data.frame(
      ID_LOCAL = character(0),
      M2_TOTALES = numeric(0),
      M2_USADOS = numeric(0),
      M2_DISPONIBLES = numeric(0),
      CLIENTE_ACTUAL = character(0)
    ))
  })
}

# ============================================================================
# FUNCIÓN: Obtener IDs Comerciales Activos
# ============================================================================
obtener_ids_activos <- function(conexion, plaza = NULL) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    filtro_plaza <- if (!is.null(plaza) && plaza != "") {
      sprintf("AND PLAZA = '%s'", plaza)
    } else ""
    
    query <- sprintf("
      SELECT DISTINCT ID_COMERCIAL 
      FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
      WHERE ID_COMERCIAL IN (
        SELECT ID_COMERCIAL 
        FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
        WHERE 1=1 %s
        GROUP BY ID_COMERCIAL
        HAVING SUM(M2_OCUPADOS) > 0
      )
      ORDER BY ID_COMERCIAL DESC
    ", filtro_plaza)
    resultado <- dbGetQuery(conexion, query)
    return(resultado$ID_COMERCIAL)
  }, error = function(e) {
    cat("Error al obtener IDs activos:", e$message, "\n")
    return(character(0))
  })
}

# ============================================================================
# FUNCIÓN NUEVA: Obtener IDs por Cliente (para detección automática)
# ============================================================================
obtener_ids_por_cliente <- function(conexion, nombre_cliente, plaza = NULL) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    filtro_plaza <- if (!is.null(plaza) && plaza != "") {
      sprintf("AND PLAZA = '%s'", plaza)
    } else ""
    
    query <- sprintf("
      SELECT DISTINCT ID_COMERCIAL 
      FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
      WHERE UPPER(CLIENTE) = UPPER('%s')
        AND ID_COMERCIAL IN (
          SELECT ID_COMERCIAL 
          FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
          WHERE 1=1 %s
          GROUP BY ID_COMERCIAL
          HAVING SUM(M2_OCUPADOS) > 0
        )
      ORDER BY ID_COMERCIAL DESC
    ", nombre_cliente, filtro_plaza)
    
    resultado <- dbGetQuery(conexion, query)
    return(resultado$ID_COMERCIAL)
  }, error = function(e) {
    cat("Error al obtener IDs por cliente:", e$message, "\n")
    return(character(0))
  })
}

obtener_todos_clientes <- function(conexion, plaza = NULL) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    filtro_plaza <- if (!is.null(plaza) && plaza != "") {
      sprintf("AND PLAZA = '%s'", plaza)
    } else ""
    
    query <- sprintf("
      SELECT DISTINCT CLIENTE
      FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
      WHERE CLIENTE IS NOT NULL AND CLIENTE != '' %s
      ORDER BY CLIENTE
    ", filtro_plaza)
    resultado <- dbGetQuery(conexion, query)
    return(resultado$CLIENTE)
  }, error = function(e) {
    cat("Error al obtener clientes:", e$message, "\n")
    return(character(0))
  })
}

obtener_nivel_locales <- function(conexion, plaza = NULL) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    filtro_plaza <- if (!is.null(plaza) && plaza != "") {
      sprintf("WHERE PLAZA = '%s'", plaza)
    } else ""
    
    query <- sprintf("
      SELECT ID_LOCAL, NIVEL
      FROM DB_ANALYTICS.SCH_CORE.LOCALES
      %s
      ORDER BY ID_LOCAL
    ", filtro_plaza)
    resultado <- dbGetQuery(conexion, query)
    return(resultado)
  }, error = function(e) {
    cat("Error al obtener niveles:", e$message, "\n")
    return(data.frame(ID_LOCAL = character(0), NIVEL = character(0)))
  })
}

# ============================================================================
# FUNCIÓN: Obtener Detalles de un ID_COMERCIAL
# ============================================================================
obtener_detalles_id_comercial <- function(conexion, id_comercial) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    query <- sprintf("
      SELECT 
        ID_LOCAL,
        SUM(M2_OCUPADOS) AS M2_OCUPADOS_ACTUALES,
        MAX(CLIENTE) AS CLIENTE
      FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
      WHERE ID_COMERCIAL = '%s'
      GROUP BY ID_LOCAL
      HAVING SUM(M2_OCUPADOS) > 0
      ORDER BY ID_LOCAL
    ", id_comercial)
    resultado <- dbGetQuery(conexion, query)
    
    if (nrow(resultado) > 0) {
      resultado$M2_OCUPADOS_ACTUALES <- as.numeric(resultado$M2_OCUPADOS_ACTUALES)
    }
    
    return(resultado)
  }, error = function(e) {
    cat("Error al obtener detalles:", e$message, "\n")
    return(data.frame())
  })
}

# ============================================================================
# FUNCIÓN: Obtener Registros Completos (Historial)
# ============================================================================
obtener_registros_completos <- function(conexion, plaza = NULL) {
  tryCatch({
    dbSendUpdate(conexion, "USE WAREHOUSE WH_ANALYTICS;")
    
    filtro_plaza <- if (!is.null(plaza) && plaza != "") {
      sprintf("WHERE PLAZA = '%s'", plaza)
    } else ""
    
    query <- sprintf("
      SELECT 
        ID_TRANSACCION,
        ID_COMERCIAL,
        ID_LOCAL,
        M2_OCUPADOS,
        CLIENTE,
        NIVEL,
        FECHA_ACTUALIZACION,
        FECHA_SISTEMA,
        PLAZA
      FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES
      %s
      ORDER BY FECHA_SISTEMA DESC
    ", filtro_plaza)
    resultado <- dbGetQuery(conexion, query)
    return(resultado)
  }, error = function(e) {
    cat("Error al obtener registros:", e$message, "\n")
    return(data.frame())
  })
}

# ============================================================================
# UI
# ============================================================================
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      /* === PALETA TEMÁTICA DE ROJOS === */
      :root {
        --rojo-primario: #ff5b49;
        --rojo-oscuro: #802e25;
        --rojo-profundo: #401712;
        --rojo-claro: #ff8d92;
        --beige: #e0d4c3;
        --gris-claro: #f5f5f5;
        --texto-oscuro: #393835;
      }
      
      /* === FONDO Y TEXTO BASE === */
      body {
        background-color: var(--beige);
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
        color: var(--texto-oscuro);
      }
      
      /* === BOTONES PRIMARIOS === */
      .btn-primary {
        background-color: var(--rojo-primario);
        border: 2px solid var(--rojo-oscuro);
        color: #ffffff;
        font-weight: bold;
        border-radius: 8px;
        transition: all 0.3s ease;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.2);
      }
      
      .btn-primary:hover {
        background-color: var(--rojo-oscuro);
        border-color: var(--rojo-profundo);
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(64, 23, 18, 0.4);
      }
      
      /* === BOTONES SECUNDARIOS === */
      .btn-danger {
        background-color: var(--rojo-profundo);
        border: 2px solid var(--rojo-oscuro);
        color: #ffffff;
        font-weight: bold;
        border-radius: 8px;
        transition: all 0.3s ease;
      }
      
      .btn-danger:hover {
        background-color: var(--rojo-oscuro);
        border-color: var(--rojo-primario);
        transform: translateY(-2px);
        box-shadow: 0 4px 8px rgba(255, 91, 73, 0.4);
      }
      
      /* === SIDEBAR === */
      .sidebar {
        background: linear-gradient(135deg, var(--gris-claro) 0%, #ffffff 100%);
        padding: 20px;
        border-radius: 12px;
        border: 3px solid var(--rojo-oscuro);
        box-shadow: 0 4px 12px rgba(128, 46, 37, 0.15);
      }
      
      /* === TÍTULOS === */
      h4, h5 {
        color: var(--rojo-profundo);
        font-weight: bold;
        text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
      }
      
      .well h4 {
        border-bottom: 3px solid var(--rojo-primario);
        padding-bottom: 10px;
        margin-bottom: 15px;
      }
      
      /* === INPUTS === */
      .form-control {
        border: 2px solid var(--beige);
        border-radius: 6px;
        transition: all 0.3s ease;
      }
      
      .form-control:focus {
        border-color: var(--rojo-primario);
        box-shadow: 0 0 8px rgba(255, 91, 73, 0.4);
        outline: none;
      }
      
      /* === SELECTIZE === */
      .selectize-input {
        border: 2px solid var(--beige);
        border-radius: 6px;
        transition: all 0.3s ease;
      }
      
      .selectize-input.focus {
        border-color: var(--rojo-primario);
        box-shadow: 0 0 8px rgba(255, 91, 73, 0.4);
      }
      
      .selectize-dropdown {
        border: 2px solid var(--rojo-claro);
        border-radius: 6px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
      }
      
      .selectize-dropdown .active {
        background-color: var(--rojo-primario);
        color: #ffffff;
      }
      
      .selectize-dropdown .option:hover {
        background-color: var(--rojo-claro);
        color: #ffffff;
      }
      
      /* === TABS PRINCIPALES (Panel derecho) === */
      .nav-tabs {
        border-bottom: 3px solid var(--rojo-oscuro);
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:hover,
      .nav-tabs > li.active > a:focus {
        background-color: var(--rojo-primario) !important;
        color: #ffffff !important;
        border: 2px solid var(--rojo-oscuro) !important;
        border-bottom-color: transparent !important;
        font-weight: bold;
      }
      
      .nav-tabs > li > a {
        color: var(--rojo-profundo);
        font-weight: bold;
        border: 2px solid transparent;
        transition: all 0.3s ease;
      }
      
      .nav-tabs > li > a:hover {
        background-color: var(--rojo-claro) !important;
        color: #ffffff !important;
        border-color: var(--rojo-oscuro) !important;
      }
      
      /* === TABS SIDEBAR (Pestañas Rentar/Liberar) === */
        #sidebar_tabs > li {
          width: 50%;
          text-align: center;
          margin-bottom: 0px;
        }
        
        .nav-tabs > li {
          margin-bottom: 0px;
        }
        
        .sidebar-tabs-container .nav-tabs {
          display: flex;
          flex-wrap: nowrap;
          justify-content: space-between;
          border-bottom: 2px solid var(--rojo-oscuro);
        }
        
        .sidebar-tabs-container .nav-tabs > li.active > a,
        .sidebar-tabs-container .nav-tabs > li.active > a:hover,
        .sidebar-tabs-container .nav-tabs > li.active > a:focus {
          background-color: var(--rojo-oscuro) !important;
          color: #ffffff !important;
          border: 2px solid var(--rojo-profundo) !important;
          border-bottom-color: transparent !important;
          font-weight: bold;
          border-radius: 6px 6px 0 0;
          padding: 10px 5px;
          font-size: 13px;
        }
        
        .sidebar-tabs-container .nav-tabs > li > a {
          color: var(--rojo-profundo);
          font-weight: bold;
          border: 2px solid transparent;
          transition: all 0.3s ease;
          border-radius: 6px 6px 0 0;
          padding: 10px 5px;
          font-size: 13px;
        }
        
        .sidebar-tabs-container .nav-tabs > li > a:hover {
          background-color: var(--rojo-claro) !important;
          color: #ffffff !important;
          border-color: var(--rojo-oscuro) !important;
        }
      
      /* === RADIO BUTTONS === */
      .radio label {
        color: var(--rojo-profundo);
        font-weight: 500;
        padding: 8px 12px;
        border-radius: 6px;
        transition: all 0.3s ease;
      }
      
      .radio input[type='radio']:checked + span {
        color: var(--rojo-primario);
        font-weight: bold;
      }
      
      /* === TABLAS DATATABLE === */
      .dataTables_wrapper .dataTables_length,
      .dataTables_wrapper .dataTables_filter,
      .dataTables_wrapper .dataTables_info,
      .dataTables_wrapper .dataTables_processing,
      .dataTables_wrapper .dataTables_paginate {
        color: var(--rojo-profundo);
      }
      
      table.dataTable thead th {
        background-color: var(--rojo-oscuro);
        color: #ffffff;
        font-weight: bold;
        border-bottom: 3px solid var(--rojo-profundo);
      }
      
      table.dataTable tbody tr:hover {
        background-color: rgba(255, 91, 73, 0.1);
      }
      
      table.dataTable tbody tr.odd {
        background-color: rgba(224, 212, 195, 0.3);
      }
      
      /* === HR === */
      hr {
        border-top: 2px solid var(--rojo-claro);
        margin: 20px 0;
      }
      
      /* === RESUMEN PRE === */
      pre {
        background-color: rgba(255, 141, 146, 0.1);
        border: 2px solid var(--rojo-claro);
        border-radius: 6px;
        padding: 10px;
        color: var(--rojo-profundo);
        font-weight: 500;
      }
      
      /* === MODAL === */
      .modal-header {
        background-color: var(--rojo-primario);
        color: #ffffff;
        border-bottom: 3px solid var(--rojo-oscuro);
      }
      
      .modal-title {
        font-weight: bold;
      }
      
      /* === ALERTAS === */
      .alert {
        border-width: 2px;
        border-radius: 8px;
      }
      
      .alert-success {
        background-color: rgba(76, 175, 80, 0.1);
        border-color: #4CAF50;
        color: #2e7d32;
      }
      
      .alert-danger {
        background-color: rgba(244, 67, 54, 0.1);
        border-color: var(--rojo-primario);
        color: var(--rojo-profundo);
      }
      
      .alert-warning {
        background-color: rgba(255, 152, 0, 0.1);
        border-color: #FF9800;
        color: #e65100;
      }
      
      /* === SELECTOR DE PLAZA === */
      .plaza-selector-wrapper {
        background: linear-gradient(135deg, var(--rojo-profundo) 0%, var(--rojo-oscuro) 100%);
        border-radius: 10px;
        padding: 12px 15px;
        margin-bottom: 15px;
        box-shadow: 0 3px 8px rgba(64, 23, 18, 0.3);
      }
      
      .plaza-selector-wrapper label {
        color: #ffffff !important;
        font-weight: bold;
        font-size: 13px;
        margin-bottom: 6px;
        display: block;
        text-transform: uppercase;
        letter-spacing: 0.5px;
      }
      
      .plaza-badge {
        display: inline-block;
        background-color: var(--rojo-primario);
        color: #ffffff;
        font-weight: bold;
        font-size: 11px;
        padding: 2px 8px;
        border-radius: 12px;
        margin-left: 6px;
        vertical-align: middle;
      }
      

      .container-fluid > h2 {
        color: var(--rojo-profundo);
        font-weight: bold;
        text-align: center;
        padding: 15px;
        background: linear-gradient(135deg, var(--rojo-claro) 0%, var(--rojo-primario) 100%);
        color: #ffffff;
        border-radius: 8px;
        margin-bottom: 20px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
        text-shadow: 2px 2px 4px rgba(0,0,0,0.3);
      }
      
      /* === PANEL DE INFORMACIÓN === */
      .info-panel {
        background-color: rgba(255, 243, 205, 0.5);
        border-left: 5px solid var(--rojo-primario);
        padding: 12px;
        border-radius: 6px;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* === PANEL DE SUGERENCIA === */
      .suggestion-panel {
        background-color: rgba(173, 216, 230, 0.3);
        border-left: 5px solid #4CAF50;
        padding: 12px;
        border-radius: 6px;
        margin-bottom: 15px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      /* === TAB CONTENT === */
      .tab-content {
        background-color: #ffffff;
        padding: 20px;
        border-radius: 0 6px 6px 6px;
        border: 2px solid var(--beige);
      }
    "))
  ),
  
  titlePanel(
    tags$span(
      "Gestión de Locales Comerciales — ",
      tags$span(id = "titulo_plaza", "Plaza Bernal")
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "sidebar",
      
      # ── SELECTOR DE PLAZA ──────────────────────────────────────────────────
      tags$div(
        class = "plaza-selector-wrapper",
        tags$label(
          "Plaza Comercial:"
        ),
        selectInput(
          "plaza_activa",
          label    = NULL,
          choices  = c("Cargando..." = ""),
          selected = NULL,
          width    = "100%"
        )
      ),
      
      hr(),
      
      # PESTAÑAS PARA SIDEBAR: RENTAR Y LIBERAR
      tags$div(
        class = "sidebar-tabs-container",
        tabsetPanel(
          id = "sidebar_tabs",
          type = "tabs",
          
          # PESTAÑA 1: RENTAR LOCALES 
          tabPanel(
            "Rentar Locales",
            br(),
            
            # Radio buttons para tipo de operación
            radioButtons("tipo_renta", 
                         "Tipo de Operación:", 
                         choices = c("Nuevo Contrato" = "nuevo", 
                                     "Expansión de Contrato" = "expansion"),
                         selected = "nuevo",
                         inline = FALSE),
            
            # Input de nombre de cliente
            selectizeInput("cliente", 
                           "Nombre del Cliente:",
                           choices = NULL,
                           options = list(
                             create = TRUE,
                             placeholder = "Ej: Banco del Istmo, Telecom Sierra, Cafe Bonanza",
                             maxOptions = 15
                           )),
            
            # Panel condicional para expansión
            conditionalPanel(
              condition = "input.tipo_renta == 'expansion'",
              uiOutput("sugerencia_id_expansion"),
              uiOutput("selector_id_expansion")
            ),
            
            hr(),
            
            uiOutput("selector_locales_multiple"),
            hr(),
            uiOutput("inputs_metros_dinamicos"),
            hr(),
            
            tags$div(
              tags$label("Nivel/Piso:"),
              tags$div(
                style = "background-color: #f5f5f5; border: 2px solid #e0d4c3; border-radius: 6px; padding: 6px 12px; color: #802e25; font-weight: bold;",
                textOutput("nivel_automatico", inline = TRUE)
              ),
              tags$input(type = "hidden", id = "nivel")
            ),
            
            hr(),
            
            # SELECTOR DE MÉTODO DE ID
            radioButtons("metodo_id", 
                         "Método de ID Comercial:",
                         choices = c("Automático" = "auto", "Personalizado" = "manual"),
                         selected = "auto", 
                         inline = TRUE),
            
            # CAMPO DE ID EDITABLE
            textInput("id_comercial_input", 
                      "ID Comercial:", 
                      value = "",
                      placeholder = "Se generará automáticamente..."),
            
            helpText(style = "font-size: 11px; color: #802e25;", 
                     "Nota: En modo Expansión se usa el ID seleccionado. En modo Personalizado puedes escribir tu propio ID."),
            
            hr(),
            
            actionButton("asignar", "Registrar Renta", class = "btn-primary btn-lg", width = "100%"),
            hr(),
            h5("Resumen de Selección:"),
            verbatimTextOutput("resumen_seleccion")
          ),
          
          # PESTAÑA 2: LIBERAR LOCALES
          tabPanel(
            "Liberar Locales",
            br(),
            uiOutput("selector_id_comercial"),
            uiOutput("info_id_comercial"),
            uiOutput("inputs_metros_liberar_dinamicos"),
            actionButton("liberar", "Devolver Local", class = "btn-danger btn-lg", width = "100%")
          )
        )
      ),
      
      hr(),
      
      # BOTÓN DE REFRESCAR (común para ambas pestañas)
      actionButton("refrescar", "Refrescar Datos", class = "btn-primary btn-lg", width = "100%")
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        # TAB 1: ESTADO ACTUAL
        tabPanel(
          "Estado Actual",
          br(),
          fluidRow(
            column(12, 
                   h4("Estado de Disponibilidad"), 
                   p("Disponible | Ocupado | Parcial", style = "color: #802e25; font-weight: bold;"))
          ),
          fluidRow(
            column(12, DTOutput("tabla_disponibilidad"))
          ),
          hr(),
          fluidRow(
            column(12, h4("Mapa de Ocupación Visual"), plotOutput("mapa_ocupacion", height = "500px"))
          )
        ),
        
        # TAB 2: RESUMEN DE CLIENTES
        tabPanel(
          "Resumen de Clientes",
          br(),
          fluidRow(
            column(12, 
                   h4("Resumen de Clientes Activos"),
                   p("Listado de clientes con locales ocupados actualmente", 
                     style = "color: #802e25; font-weight: bold;"))
          ),
          fluidRow(
            column(12, DTOutput("tabla_clientes"))
          )
        ),
        
        # TAB 3: HISTORIAL COMPLETO
        tabPanel(
          "Historial de Movimientos",
          br(),
          fluidRow(
            column(12, 
                   h4("Registro Completo de Transacciones"),
                   p("Renta = Ocupación | Devolución = Liberación", 
                     style = "color: #802e25; font-weight: bold;")
            )
          ),
          fluidRow(
            column(12, DTOutput("tabla_historial"))
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVIDOR
# ============================================================================
server <- function(input, output, session) {
  
  con_pura <- NULL
  error_conexion <- NULL
  
  tryCatch({
    con_pura <- crear_conexion_snowflake()
  }, error = function(e) {
    error_conexion <<- e$message
  })
  
  estado <- reactiveValues(
    conexion = con_pura, 
    conexion_valida = !is.null(con_pura), 
    error_mensaje = error_conexion
  )
  trigger_refresh <- reactiveVal(0)
  
  # =========================================================================
  # INICIALIZAR SELECTOR DE PLAZAS
  # =========================================================================
  observe({
    req(estado$conexion_valida)
    plazas <- obtener_plazas(estado$conexion)
    updateSelectInput(session, "plaza_activa",
                      choices  = plazas,
                      selected = plazas[1])
  })
  
  niveles_locales <- reactive({
    req(estado$conexion_valida)
    obtener_nivel_locales(estado$conexion, input$plaza_activa)
  })
  
  output$nivel_automatico <- renderText({
    req(input$locales_seleccionados)
    niveles <- niveles_locales()
    
    if (nrow(niveles) == 0 || length(input$locales_seleccionados) == 0) {
      return("Sin asignar")
    }
    
    niveles_seleccionados <- niveles %>%
      filter(ID_LOCAL %in% input$locales_seleccionados) %>%
      pull(NIVEL) %>%
      unique()
    
    if (length(niveles_seleccionados) == 0) return("Sin asignar")
    
    paste(niveles_seleccionados, collapse = " / ")
  })
  
  observe({
    trigger_refresh()
    req(estado$conexion_valida)
    clientes <- obtener_todos_clientes(estado$conexion, input$plaza_activa)
    updateSelectizeInput(session, "cliente", 
                         choices = clientes, 
                         server = TRUE)
  })
  
  # =========================================================================
  # NUEVO: GENERACIÓN Y CONTROL DEL ID COMERCIAL
  # =========================================================================
  
  # ID sugerido automáticamente basado en locales seleccionados
  id_sugerido <- reactive({
    req(input$locales_seleccionados)
    if (length(input$locales_seleccionados) == 0) return("")
    
    locales_str <- paste(sort(input$locales_seleccionados), collapse = "_")
    random_num <- sample(1:10000, 1)
    paste0(locales_str, "_", random_num)
  })
  
  # Actualizar el campo de ID según el contexto
  observe({
    # CASO 1: Modo Expansión - usar ID existente
    if (!is.null(input$tipo_renta) && input$tipo_renta == "expansion") {
      if (!is.null(input$id_para_expandir) && input$id_para_expandir != "") {
        updateTextInput(session, "id_comercial_input", value = input$id_para_expandir)
        shinyjs::disable("id_comercial_input")
        shinyjs::disable("metodo_id")
      }
    } 
    # CASO 2: Modo Nuevo Contrato
    else {
      shinyjs::enable("metodo_id")
      
      if (!is.null(input$metodo_id)) {
        if (input$metodo_id == "auto") {
          # Modo automático: mostrar sugerencia y bloquear edición
          if (length(input$locales_seleccionados) > 0) {
            updateTextInput(session, "id_comercial_input", value = id_sugerido())
          }
          shinyjs::disable("id_comercial_input")
        } else {
          # Modo manual: habilitar campo para que usuario escriba
          shinyjs::enable("id_comercial_input")
        }
      }
    }
  })
  
  # =========================================================================
  # NUEVO: DETECCIÓN AUTOMÁTICA DE IDs POR CLIENTE
  # =========================================================================
  ids_sugeridos <- reactive({
    req(input$tipo_renta == "expansion", input$cliente != "")
    obtener_ids_por_cliente(estado$conexion, input$cliente, input$plaza_activa)
  })
  
  # =========================================================================
  # NUEVO: PANEL DE SUGERENCIA
  # =========================================================================
  output$sugerencia_id_expansion <- renderUI({
    ids <- ids_sugeridos()
    
    if (length(ids) == 0) {
      tags$div(
        class = "alert alert-warning",
        style = "font-size: 12px; padding: 8px;",
        strong("Atención: "),
        sprintf("No se encontraron contratos activos para '%s'. ", input$cliente),
        "Si es correcto, cambia a 'Nuevo Contrato'."
      )
    } else if (length(ids) == 1) {
      tags$div(
        class = "suggestion-panel",
        style = "font-size: 12px;",
        strong("Sugerencia: "),
        sprintf("Se detectó 1 contrato activo para '%s': %s", input$cliente, ids[1])
      )
    } else {
      tags$div(
        class = "suggestion-panel",
        style = "font-size: 12px;",
        strong("Sugerencia: "),
        sprintf("Se detectaron %d contratos activos para '%s'. Selecciona uno a continuación.", 
                length(ids), input$cliente)
      )
    }
  })
  
  # =========================================================================
  # NUEVO: SELECTOR DE ID PARA EXPANSIÓN
  # =========================================================================
  output$selector_id_expansion <- renderUI({
    req(input$tipo_renta == "expansion")
    ids <- ids_sugeridos()
    
    if (length(ids) == 0) {
      return(NULL)
    }
    
    selectInput("id_para_expandir", 
                "Seleccionar ID Comercial a Expandir:", 
                choices = ids,
                selected = ids[1])
  })
  
  # =========================================================================
  # SELECTOR DE LOCALES PARA RENTAR
  # =========================================================================
  output$selector_locales_multiple <- renderUI({
    trigger_refresh() 
    req(estado$conexion_valida)
    
    locales_aptos <- obtener_locales_disponibles(estado$conexion, input$plaza_activa)
    
    selectInput("locales_seleccionados", 
                "Seleccionar Local(es) a Rentar:", 
                choices = locales_aptos, 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  # =========================================================================
  # SELECTOR DE ID_COMERCIAL PARA LIBERAR
  # =========================================================================
  output$selector_id_comercial <- renderUI({
    trigger_refresh()
    req(estado$conexion_valida)
    ids_activos <- obtener_ids_activos(estado$conexion, input$plaza_activa)
    selectInput("id_comercial_liberar", 
                "ID Comercial a Liberar:", 
                choices = ids_activos,
                selected = NULL)
  })
  
  # =========================================================================
  # INFO DEL ID_COMERCIAL SELECCIONADO
  # =========================================================================
  detalles_id_comercial <- reactive({
    req(input$id_comercial_liberar)
    obtener_detalles_id_comercial(estado$conexion, input$id_comercial_liberar)
  })
  
  output$info_id_comercial <- renderUI({
    req(input$id_comercial_liberar)
    detalles <- detalles_id_comercial()
    
    if (nrow(detalles) == 0) {
      return(p("No hay información disponible", style = "color: red;"))
    }
    
    info_texto <- paste(
      sprintf("Cliente: %s", detalles$CLIENTE[1]),
      sprintf("Locales ocupados: %d", nrow(detalles)),
      collapse = "\n"
    )
    
    tags$div(
      class = "info-panel",
      tags$pre(style = "margin: 0; font-size: 12px; background: transparent; border: none;", info_texto)
    )
  })
  
  output$inputs_metros_liberar_dinamicos <- renderUI({
    req(input$id_comercial_liberar)
    detalles <- detalles_id_comercial()
    if (nrow(detalles) == 0) return(NULL)
    
    inputs_lista <- lapply(seq_len(nrow(detalles)), function(i) {
      local <- detalles$ID_LOCAL[i]
      metros_ocupados <- detalles$M2_OCUPADOS_ACTUALES[i]
      div(
        style = "margin-bottom: 10px;",
        numericInput(
          inputId = paste0("metros_liberar_", local),
          label = sprintf("%s (max: %.2f m² ocupados):", local, metros_ocupados),
          value = 0,
          min = 0,
          max = metros_ocupados,
          step = 0.5
        )
      )
    })
    
    tagList(h5("Metros a Liberar por Local:"), inputs_lista)
  })
  
  # =========================================================================
  # INPUTS DINÁMICOS DE METROS
  # =========================================================================
  output$inputs_metros_dinamicos <- renderUI({
    req(input$locales_seleccionados)
    datos <- datos_disponibilidad()
    inputs_lista <- lapply(input$locales_seleccionados, function(local) {
      metros_disp <- 0
      if (nrow(datos) > 0) {
        info_local <- datos %>% filter(ID_LOCAL == local)
        if (nrow(info_local) > 0) metros_disp <- info_local$M2_DISPONIBLES[1]
      }
      div(
        style = "margin-bottom: 10px;", 
        numericInput(
          inputId = paste0("metros_", local), 
          label = sprintf("%s (Disponibles: %.2f m²)", local, metros_disp), 
          value = 0, 
          min = 0, 
          max = metros_disp, 
          step = 0.5
        )
      )
    })
    tagList(h5("Metros a Ocupar por Local:"), inputs_lista)
  })
  
  # =========================================================================
  # DATOS REACTIVOS
  # =========================================================================
  datos_disponibilidad <- reactive({
    trigger_refresh()
    req(estado$conexion_valida)
    obtener_disponibilidad(estado$conexion, input$plaza_activa)
  })
  
  observeEvent(estado$conexion_valida, { 
    if (estado$conexion_valida) trigger_refresh(1) 
  }, once = TRUE)
  
  # =========================================================================
  # RESUMEN DE SELECCIÓN
  # =========================================================================
  output$resumen_seleccion <- renderText({
    if (is.null(input$locales_seleccionados) || length(input$locales_seleccionados) == 0) {
      return("No hay locales seleccionados")
    }
    
    total_metros <- 0
    detalles <- c()
    for (local in input$locales_seleccionados) {
      metros_input <- input[[paste0("metros_", local)]]
      if (!is.null(metros_input) && !is.na(metros_input) && metros_input > 0) {
        total_metros <- total_metros + metros_input
        detalles <- c(detalles, sprintf("%s: %.2f m²", local, metros_input))
      }
    }
    
    nivel_texto <- if (!is.null(input$nivel)) input$nivel else "PB"
    tipo_operacion <- if (!is.null(input$tipo_renta) && input$tipo_renta == "expansion") "Expansión" else "Nuevo Contrato"
    
    resumen_base <- sprintf("Tipo: %s\nLocales seleccionados: %d\nTotal metros: %.2f m²\nNivel: %s\n\nDetalle:\n%s", 
                            tipo_operacion,
                            length(input$locales_seleccionados), 
                            total_metros,
                            nivel_texto,
                            if (length(detalles) > 0) paste(detalles, collapse = "\n") else "Sin metros asignados")
    
    # Agregar información del ID
    if (!is.null(input$id_comercial_input) && input$id_comercial_input != "") {
      metodo_texto <- if (!is.null(input$metodo_id) && input$metodo_id == "manual") "Personalizado" else "Automático"
      
      if (!is.null(input$tipo_renta) && input$tipo_renta == "expansion") {
        resumen_base <- paste0(resumen_base, sprintf("\n\nID (Expansión): %s", input$id_comercial_input))
      } else {
        resumen_base <- paste0(resumen_base, sprintf("\n\nID (%s): %s", metodo_texto, input$id_comercial_input))
      }
    }
    
    return(resumen_base)
  })
  
  # =========================================================================
  # EVENTO: ASIGNAR/RENTAR LOCAL (MODIFICADO)
  # =========================================================================
  observeEvent(input$asignar, {
    req(input$locales_seleccionados, input$cliente != "", estado$conexion_valida)
    
    # Validar que en modo expansión haya un ID seleccionado
    if (input$tipo_renta == "expansion") {
      if (is.null(input$id_para_expandir) || input$id_para_expandir == "") {
        showModal(modalDialog(
          title = "Error de Validación",
          "Debes seleccionar un ID Comercial para expandir o cambiar a 'Nuevo Contrato'.",
          easyClose = TRUE
        ))
        return()
      }
    }
    
    # Recopilar locales con metros
    locales_con_metros <- list()
    for (local in input$locales_seleccionados) {
      metros <- input[[paste0("metros_", local)]]
      if (!is.null(metros) && metros > 0) {
        validacion <- validar_disponibilidad(estado$conexion, local, metros)
        if (!validacion$valido) {
          showModal(modalDialog(
            title = "Error de Disponibilidad",
            paste0("Local ", local, ": ", validacion$mensaje),
            easyClose = TRUE
          ))
          return()
        }
        locales_con_metros[[local]] <- metros
      }
    }
    
    if (length(locales_con_metros) == 0) {
      showModal(modalDialog(
        title = "Advertencia",
        "Debe asignar al menos un local con metros > 0",
        easyClose = TRUE
      ))
      return()
    }
    
    # ========================================
    # LÓGICA PRINCIPAL: DETERMINAR EL ID A USAR
    # ========================================
    
    # Obtener el ID del campo de texto
    id_comercial <- trimws(input$id_comercial_input)
    
    # Validar que no esté vacío
    if (id_comercial == "") {
      showModal(modalDialog(
        title = "Error de Validación",
        "El ID Comercial no puede estar vacío. Selecciona locales o escribe un ID personalizado.",
        easyClose = TRUE
      ))
      return()
    }
    
    # VALIDACIÓN DE UNICIDAD (Solo para contratos nuevos)
    if (input$tipo_renta == "nuevo") {
      query_check <- sprintf(
        "SELECT COUNT(*) AS existe FROM DB_ANALYTICS.SCH_CORE.REGISTRO_LOCALES WHERE ID_COMERCIAL = '%s'",
        id_comercial
      )
      
      cat("Validando unicidad del ID:", id_comercial, "\n")
      res_check <- dbGetQuery(estado$conexion, query_check)
      
      if (res_check$EXISTE[1] > 0) {
        showModal(modalDialog(
          title = "ID Comercial en Existencia",
          HTML(sprintf("El ID <code>%s</code> ya está registrado en la base de datos.<br><br>
                   Por favor, intente con otro nombre o use el modo automático.", id_comercial)),
          easyClose = TRUE,
          footer = tagList(
            modalButton("Cerrar")
          )
        ))
        return() # Detiene la ejecución
      }
      
      tipo_mensaje <- "Nuevo Contrato"
    } else {
      tipo_mensaje <- "Expansión de Contrato"
    }
    
    # Insertar registros
    for (local in names(locales_con_metros)) {
      niveles <- niveles_locales()
      nivel_local <- niveles %>% 
        filter(ID_LOCAL == local) %>% 
        pull(NIVEL)
      nivel_usar <- if (length(nivel_local) > 0) nivel_local[1] else "PB"
      
      resultado <- insertar_registro(
        estado$conexion, 
        id_comercial, 
        local, 
        input$cliente, 
        locales_con_metros[[local]],
        nivel_usar,
        input$plaza_activa
      )
      
      if (!resultado$exito) {
        showModal(modalDialog(
          title = "Error al Insertar",
          resultado$mensaje,
          easyClose = TRUE
        ))
        return()
      }
    }
    
    # Limpiar formulario
    updateSelectInput(session, "locales_seleccionados", selected = character(0))
    updateTextInput(session, "cliente", value = "")
    updateSelectInput(session, "nivel", selected = "PB")
    updateRadioButtons(session, "tipo_renta", selected = "nuevo")
    trigger_refresh(trigger_refresh() + 1)
    
    # Mensaje de éxito diferenciado
    showModal(modalDialog(
      title = "Registro Exitoso",
      HTML(sprintf("<strong>%s registrada correctamente</strong><br><br>
                   ID Comercial: <code>%s</code><br>
                   Cliente: %s<br>
                   Locales: %s<br>
                   Total m²: %.2f", 
                   tipo_mensaje,
                   id_comercial,
                   input$cliente,
                   paste(names(locales_con_metros), collapse = ", "),
                   sum(unlist(locales_con_metros)))),
      easyClose = TRUE
    ))
  })
  
  # =========================================================================
  # EVENTO: LIBERAR LOCAL (SIN CAMBIOS)
  # =========================================================================
  observeEvent(input$liberar, {
    req(input$id_comercial_liberar, estado$conexion_valida)
    
    detalles <- detalles_id_comercial()
    if (nrow(detalles) == 0) return()
    
    locales_a_liberar <- list()
    for (i in seq_len(nrow(detalles))) {
      local <- detalles$ID_LOCAL[i]
      metros <- input[[paste0("metros_liberar_", local)]]
      metros_max <- detalles$M2_OCUPADOS_ACTUALES[i]
      
      if (!is.null(metros) && metros > 0) {
        if (metros > metros_max) {
          showModal(modalDialog(
            title = "Error de Validacion",
            paste0("El local ", local, " solo tiene ", sprintf("%.2f", metros_max), " m² ocupados."),
            easyClose = TRUE
          ))
          return()
        }
        locales_a_liberar[[local]] <- metros
      }
    }
    
    if (length(locales_a_liberar) == 0) {
      showModal(modalDialog(
        title = "Advertencia",
        "Debes ingresar al menos un local con metros > 0 para liberar.",
        easyClose = TRUE
      ))
      return()
    }
    
    errores <- c()
    for (local in names(locales_a_liberar)) {
      resultado <- liberar_local(
        estado$conexion,
        input$id_comercial_liberar,
        local,
        locales_a_liberar[[local]],
        input$plaza_activa
      )
      if (!resultado$exito) errores <- c(errores, paste0(local, ": ", resultado$mensaje))
    }
    
    trigger_refresh(trigger_refresh() + 1)
    
    if (length(errores) > 0) {
      showModal(modalDialog(
        title = "Errores al Liberar",
        HTML(paste(errores, collapse = "<br>")),
        easyClose = TRUE
      ))
    } else {
      showModal(modalDialog(
        title = "Liberacion Exitosa",
        HTML(sprintf("Se liberaron <strong>%d local(es)</strong> del ID Comercial <code>%s</code>:<br><br>%s",
                     length(locales_a_liberar),
                     input$id_comercial_liberar,
                     paste(sprintf("%s: %.2f m²", names(locales_a_liberar), unlist(locales_a_liberar)), collapse = "<br>"))),
        easyClose = TRUE
      ))
    }
  })
  
  # =========================================================================
  # EVENTO: REFRESCAR
  # =========================================================================
  observeEvent(input$refrescar, { 
    trigger_refresh(trigger_refresh() + 1) 
  })
  
  # =========================================================================
  # TABLA: DISPONIBILIDAD
  # =========================================================================
  output$tabla_disponibilidad <- renderDT({
    datos <- datos_disponibilidad()
    if (nrow(datos) == 0) return(datatable(data.frame(Mensaje = "No hay registros")))
    
    tabla_final <- datos %>%
      mutate(
        `% Ocupación` = (M2_USADOS / M2_TOTALES) * 100,
        Estado = case_when(
          M2_USADOS <= 0.05 ~ "Disponible", # Tolerancia para decimales muy pequeños
          `% Ocupación` >= 99 ~ "Ocupado",
          `% Ocupación` > 0 ~ "Parcial",
          TRUE ~ "Disponible"
        ),
        `% Ocupación` = sprintf("%.1f%%", `% Ocupación`)
      ) %>%
      select(ID_LOCAL, Estado, M2_TOTALES, M2_USADOS, M2_DISPONIBLES, `% Ocupación`, CLIENTE_ACTUAL)
    
    datatable(tabla_final,
              colnames = c('Local', 'Estado', 'm² Totales', 'm² Usados', 'm² Disponibles', '% Ocupación', 'Cliente Actual'),
              options = list(
                pageLength = 15, 
                dom = 'ftp', 
                scrollX = TRUE, 
                scrollY = "400px", 
                scrollCollapse = TRUE,
                order = list(list(0, 'asc'))
              ),
              rownames = FALSE, 
              selection = 'none') %>%
      formatStyle(
        'Estado',
        backgroundColor = styleEqual(
          c('Disponible', 'Ocupado', 'Parcial'),
          c('#d4edda', '#f8d7da', '#fff3cd')
        ),
        color = styleEqual(
          c('Disponible', 'Ocupado', 'Parcial'),
          c('#155724', '#721c24', '#856404')
        ),
        fontWeight = 'bold'
      )
  })
  
  # =========================================================================
  # TABLA: HISTORIAL COMPLETO
  # =========================================================================
  output$tabla_historial <- renderDT({
    trigger_refresh()
    req(estado$conexion_valida)
    datos <- obtener_registros_completos(estado$conexion, input$plaza_activa)
    
    if (nrow(datos) == 0) return(datatable(data.frame(Mensaje = "No hay registros")))
    
    tabla_final <- datos %>%
      mutate(
        Tipo = ifelse(M2_OCUPADOS > 0, "Renta", "Devolución"),
        M2_OCUPADOS = sprintf("%.2f", M2_OCUPADOS)
      ) %>%
      select(ID_TRANSACCION, Tipo, ID_COMERCIAL, ID_LOCAL, M2_OCUPADOS, CLIENTE, NIVEL, FECHA_ACTUALIZACION, FECHA_SISTEMA, PLAZA)
    
    datatable(tabla_final,
              colnames = c('ID Trans.', 'Tipo', 'ID Comercial', 'Local', 'm² Ocupados', 'Cliente', 'Nivel', 'Fecha Act.', 'Fecha Sistema', 'Plaza'),
              options = list(
                pageLength = 20, 
                dom = 'ftp', 
                scrollX = TRUE, 
                scrollY = "500px", 
                scrollCollapse = TRUE,
                order = list(list(8, 'desc'))
              ),
              rownames = FALSE, 
              selection = 'none') %>%
      formatStyle(
        'Tipo',
        backgroundColor = styleEqual(
          c('Renta', 'Devolución'),
          c('#d4edda', '#f8d7da')
        ),
        color = styleEqual(
          c('Renta', 'Devolución'),
          c('#155724', '#721c24')
        ),
        fontWeight = 'bold'
      )
  })
  
  # =========================================================================
  # MAPA DE OCUPACIÓN (MODIFICADO - COLOR ÚNICO)
  # =========================================================================
  output$mapa_ocupacion <- renderPlot({
    datos <- datos_disponibilidad()
    req(nrow(datos) > 0)
    n_locales <- nrow(datos)
    
    par(mar = c(5, 2, 3, 2), bg = "#e0d4c3")
    plot(1:n_locales, rep(1, n_locales), type = "n", 
         xlim = c(0.5, n_locales + 0.5), ylim = c(0, 1.8), 
         axes = FALSE, 
         main = paste0("Mapa de Ocupación — Plaza ", input$plaza_activa), 
         col.main = "#401712", 
         cex.main = 1.5, 
         font.main = 2)
    
    # Color único para todas las barras ocupadas
    color_ocupacion <- "#ff5b49"  # Rojo primario
    color_base <- "#ffffff"       # Blanco para espacio disponible
    
    for (i in 1:nrow(datos)) {
      # Rectángulo base (blanco - espacio total)
      rect(i - 0.4, 0.3, i + 0.4, 1.3, col = color_base, border = "#401712", lwd = 2.5)
      
      # Relleno de ocupación (rojo - espacio ocupado)
      if (datos$M2_USADOS[i] > 0) {
        div_total <- ifelse(datos$M2_TOTALES[i] > 0, datos$M2_TOTALES[i], 1)
        altura_ocupada <- (datos$M2_USADOS[i] / div_total)
        rect(i - 0.4, 0.3, i + 0.4, 0.3 + altura_ocupada, 
             col = color_ocupacion, border = NA)
      }
      
      # Etiqueta del local
      text(i, 1.5, datos$ID_LOCAL[i], cex = 0.8, srt = 90, col = "#401712", font = 2)
    }
    
    # Leyenda simplificada (sin lista de clientes)
    legend("topright", 
           legend = c("Ocupado", "Disponible"), 
           fill = c(color_ocupacion, color_base), 
           title = "Estado",
           cex = 0.9,
           bg = "#ffffff",
           box.col = "#401712",
           box.lwd = 2,
           border = c("#401712", "#401712"))
  })
  
  # =========================================================================
  # TABLA: RESUMEN CLIENTES
  # =========================================================================
  output$tabla_clientes <- renderDT({
    datos <- datos_disponibilidad()
    req(nrow(datos) > 0)
    resumen <- datos %>% 
      filter(!is.na(CLIENTE_ACTUAL) & CLIENTE_ACTUAL != "") %>% 
      group_by(Cliente = CLIENTE_ACTUAL) %>% 
      summarise(
        `Total m²` = sum(M2_USADOS), 
        `Locales` = paste(ID_LOCAL, collapse = ", "),
        `Num. Locales` = n(),
        .groups = 'drop'
      ) %>%
      arrange(desc(`Total m²`))
    
    if (nrow(resumen) == 0) {
      return(datatable(data.frame(Mensaje = "No hay clientes activos")))
    }
    
    datatable(resumen, 
              colnames = c('Cliente', 'Total m²', 'Locales Ocupados', 'Cantidad Locales'),
              options = list(
                pageLength = 15, 
                dom = 'ftp',
                scrollX = TRUE,
                scrollY = "600px",
                scrollCollapse = TRUE,
                order = list(list(1, 'desc'))
              ), 
              rownames = FALSE,
              selection = 'none') %>%
      formatStyle(
        'Total m²',
        background = styleColorBar(resumen$`Total m²`, '#ff5b49'),
        backgroundSize = '98% 88%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # =========================================================================
  # DESCONEXIÓN
  # =========================================================================
  session$onSessionEnded(function() { 
    if (!is.null(con_pura)) dbDisconnect(con_pura) 
  })
}

# ============================================================================
# EJECUTAR APP
# ============================================================================
  shinyApp(ui = ui, server = server)
