library(openxlsx)
library(RJDBC)
library(DBI)
library(dplyr)
library(stringr)

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
# FUNCIÓN: Conectar a Snowflake con JWT (SIN USUARIO/PASSWORD)
# ============================================================================
crear_conexion_snowflake <- function(usuario = "SVC_ANALYTICS", ruta_pem = Sys.getenv("SNOWFLAKE_PRIVATE_KEY_PATH")) {
  tryCatch({
    # Usar driver cacheado globalmente si está disponible
    if (exists("SNOWFLAKE_JDBC_DRIVER") && !is.null(SNOWFLAKE_JDBC_DRIVER)) {
      driver <- SNOWFLAKE_JDBC_DRIVER
    } else {
      ruta_archivo_jar <- file.path(getwd(), "snowflake-jdbc-3.25.1.jar")
      driver <- RJDBC::JDBC("net.snowflake.client.jdbc.SnowflakeDriver", 
                            classPath = ruta_archivo_jar, 
                            identifier.quote = "`")
    }
    
    # Usar llave privada cacheada globalmente si está disponible
    if (exists("SNOWFLAKE_PRIVATE_KEY") && !is.null(SNOWFLAKE_PRIVATE_KEY)) {
      private_key <- SNOWFLAKE_PRIVATE_KEY
    } else {
      ruta_pem_absoluta <- file.path(getwd(), ruta_pem)
      private_key <- convertir_pem_a_privatekey(ruta_pem_absoluta)
    }
    
    # URL de conexión
    url_conexion <- paste0("jdbc:snowflake://", Sys.getenv("SNOWFLAKE_ACCOUNT"), ".snowflakecomputing.com/")
    
    # Crear Properties usando rJava
    propiedades <- .jnew("java.util.Properties")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "user", usuario)
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "warehouse", "WH_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "db", "DB_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "schema", "SCH_CORE")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "role", "ROLE_ANALYTICS")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "authenticator", "SNOWFLAKE_JWT")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "tracing", "OFF")
    .jcall(propiedades, "Ljava/lang/Object;", "setProperty", "JDBC_QUERY_RESULT_FORMAT", "JSON")
    
    # Agregar la clave privada a las propiedades
    .jcall(propiedades, "Ljava/lang/Object;", "put", 
           .jcast(.jnew("java/lang/String", "privateKey"), "java/lang/Object"),
           .jcast(private_key, "java/lang/Object"))
    
    # Obtener el conector usando el driver
    jconn <- .jcall(driver@jdrv, "Ljava/sql/Connection;", "connect", url_conexion, propiedades)
    
    # Verificar que la conexión no sea nula
    if (is.jnull(jconn)) {
      stop("La conexión retornó null. Verifica las credenciales y permisos.")
    }
    
    # Convertir a objeto RJDBC
    conexion <- new("JDBCConnection", jc = jconn, identifier.quote = driver@identifier.quote)
    
    # Verificar que la conexión sea válida
    if (!DBI::dbIsValid(conexion)) {
      stop("La conexión no es válida después de establecerla")
    }
    
    cat("✅ Conexión exitosa a Snowflake (JWT)\n")
    
    # Devolver el objeto de conexión
    return(conexion)
    
  }, error = function(e) {
    stop(paste("❌ Error conectando a Snowflake:", e$message))
  })
}

crear_excel_mi <- function(progress_callback = NULL, ids_filtrar = NULL) {
  tryCatch({
    
    # =========================
    # 1) CONEXIÓN + QUERY (sin cambios)
    # =========================
    if(!is.null(progress_callback)) {
      progress_callback(0.05, " Conectando a Snowflake...")
    }
    
    conn <- tryCatch({
      crear_conexion_snowflake()
    }, error = function(e) {
      cat("❌ Error de conexión:", e$message, "\n")
      stop("No se pudo conectar a Snowflake.")
    })
    
    tryCatch({
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
    }, error = function(e) {
      dbDisconnect(conn)
      stop("No se pudo activar el warehouse.")
    })
    
    cat(" Consultando tabla INMUEBLES...\n")
    
    query_columnas <- "SELECT * FROM INMUEBLES LIMIT 1"
    df_test <- dbGetQuery(conn, query_columnas)
    columnas_disponibles <- names(df_test)
    
    columnas_base <- c("PAIS", "MUNICIPIO", "ESTADO", "EMPRESA", "INMUEBLE", "ID", "ALIAS", "DIRECCION")
    columnas_adicionales <- c("FECHA_ESCRITURA", "PRECIO_ADQUISICION", "TERRENO_M2",
                              "PRECIO_TERRENO_M2", "CONSTRUCCION_M2", "PRECIO_CONSTRUCCION_M2")
    
    columnas_a_consultar <- columnas_base[columnas_base %in% columnas_disponibles]
    columnas_extra_disponibles <- columnas_adicionales[columnas_adicionales %in% columnas_disponibles]
    
    if(length(columnas_extra_disponibles) > 0) {
      columnas_a_consultar <- c(columnas_a_consultar, columnas_extra_disponibles)
    }
    
    query <- paste0("SELECT ", paste(columnas_a_consultar, collapse=", "), " FROM INMUEBLES")
    df_inmuebles <- dbGetQuery(conn, query)
    dbDisconnect(conn)
    
    # APLICAR FILTRO POR IDs (provienen de los filtros activos en la UI)
    # Si ids_filtrar tiene valores = hay filtros activos → solo esos IDs
    # Si ids_filtrar es NULL = sin filtros → todos los registros del query
    if (!is.null(ids_filtrar) && length(ids_filtrar) > 0) {
      df_inmuebles <- df_inmuebles[df_inmuebles$ID %in% ids_filtrar, ]
      cat("🔍 Filtro aplicado al Excel:", nrow(df_inmuebles), "inmuebles\n")
    } else {
      cat("📋 Sin filtros activos: Excel completo con", nrow(df_inmuebles), "inmuebles\n")
    }
    
    if(!is.null(progress_callback)) {
      progress_callback(0.15, paste("✅ Datos obtenidos:", nrow(df_inmuebles), "registros"))
    }
    
    if (nrow(df_inmuebles) == 0) return(NULL)
    
    # =========================
    # 2) LIMPIEZA + PRECONVERSIÓN
    # =========================
    if(!is.null(progress_callback)) {
      progress_callback(0.20, " Preparando datos...")
    }
    
    for(col in names(df_inmuebles)) {
      df_inmuebles[[col]] <- as.character(df_inmuebles[[col]])
    }
    
    if("FECHA_ESCRITURA" %in% names(df_inmuebles)) {
      df_inmuebles$FECHA_ESCRITURA <- ifelse(
        is.na(df_inmuebles$FECHA_ESCRITURA) | df_inmuebles$FECHA_ESCRITURA %in% c("", "-"),
        "",
        as.character(df_inmuebles$FECHA_ESCRITURA)
      )
    }
    
    # Orden países
    df_inmuebles$ORDEN_PAIS <- 3  # Default: sin país
    df_inmuebles$ORDEN_PAIS[!is.na(df_inmuebles$PAIS) & 
                              toupper(df_inmuebles$PAIS) %in% c("MÉXICO", "MEXICO", "MEX", "MX")] <- 1
    df_inmuebles$ORDEN_PAIS[!is.na(df_inmuebles$PAIS) & df_inmuebles$ORDEN_PAIS == 3] <- 2
    
    df_inmuebles <- df_inmuebles[order(df_inmuebles$ORDEN_PAIS,
                                       df_inmuebles$PAIS,
                                       df_inmuebles$MUNICIPIO,
                                       df_inmuebles$EMPRESA,
                                       df_inmuebles$INMUEBLE), ]
    
    # Preconversión numérica (CRÍTICO PARA VELOCIDAD)
    to_num <- function(x) {
      x <- ifelse(is.na(x) | x %in% c("", "-"), NA, x)
      suppressWarnings(as.numeric(gsub(",", "", x)))
    }
    
    df_inmuebles$PRECIO_ADQ_NUM <- to_num(df_inmuebles$PRECIO_ADQUISICION)
    df_inmuebles$TERRENO_NUM <- to_num(df_inmuebles$TERRENO_M2)
    df_inmuebles$PRECIO_TERRENO_NUM <- to_num(df_inmuebles$PRECIO_TERRENO_M2)
    df_inmuebles$CONSTRUCCION_NUM <- to_num(df_inmuebles$CONSTRUCCION_M2)
    df_inmuebles$PRECIO_CONST_NUM <- to_num(df_inmuebles$PRECIO_CONSTRUCCION_M2)
    
    # =========================
    # 3) CONSTRUIR MATRIZ COMPLETA EN MEMORIA 
    # =========================
    if(!is.null(progress_callback)) {
      progress_callback(0.30, "️ Construyendo estructura jerárquica...")
    }
    
    # LISTAS SEPARADAS: texto (cols 1-7) y números (cols 8-12)
    filas_texto <- list()
    filas_numeros <- list()
    filas_tipos <- character()
    filas_heights <- numeric()
    merges_acumulados <- list()
    
    fila_idx <- 1
    total_inmuebles <- nrow(df_inmuebles)
    inmuebles_procesados <- 0
    
    paises_unicos <- unique(df_inmuebles$PAIS[order(df_inmuebles$ORDEN_PAIS, df_inmuebles$PAIS)])
    
    for (pais in paises_unicos) {
      
      # FILA PAÍS
      filas_texto[[fila_idx]] <- c(toupper(pais), rep("", 6))
      filas_numeros[[fila_idx]] <- rep(NA, 5)
      filas_tipos[fila_idx] <- "pais"
      filas_heights[fila_idx] <- 30
      merges_acumulados[[length(merges_acumulados) + 1]] <- list(row = fila_idx, cols = 1:12)
      fila_idx <- fila_idx + 1
      
      df_pais <- df_inmuebles[df_inmuebles$PAIS == pais, ]
      municipios_unicos <- unique(df_pais$MUNICIPIO)
      
      for (municipio in municipios_unicos) {
        
        # FILA MUNICIPIO
        filas_texto[[fila_idx]] <- c(paste0("   ", toupper(municipio)), rep("", 6))
        filas_numeros[[fila_idx]] <- rep(NA, 5)
        filas_tipos[fila_idx] <- "municipio"
        filas_heights[fila_idx] <- 25
        merges_acumulados[[length(merges_acumulados) + 1]] <- list(row = fila_idx, cols = 1:12)
        fila_idx <- fila_idx + 1
        
        df_municipio <- df_pais[df_pais$MUNICIPIO == municipio, ]
        empresas_unicas <- unique(df_municipio$EMPRESA)
        
        suma_muni_precio <- 0
        suma_muni_terreno <- 0
        suma_muni_const <- 0
        
        for (empresa in empresas_unicas) {
          
          df_empresa <- df_municipio[df_municipio$EMPRESA == empresa, ]
          
          suma_emp_precio <- 0
          suma_emp_terreno <- 0
          suma_emp_const <- 0
          
          # INMUEBLES
          for (i in seq_len(nrow(df_empresa))) {
            
            inmuebles_procesados <- inmuebles_procesados + 1
            
            if (inmuebles_procesados %% 100 == 0) {
              porcentaje <- (inmuebles_procesados / total_inmuebles) * 100
              mensaje <- sprintf("⏳ Procesando... %d/%d inmuebles (%.1f%%)",
                                 inmuebles_procesados, total_inmuebles, porcentaje)
              
              if(!is.null(progress_callback)) {
                progreso <- 0.30 + (porcentaje / 100) * 0.50
                progress_callback(progreso, mensaje)
              }
            }
            
            # Extraer datos de texto (columnas 1-7)
            inmueble_texto <- paste0("         ", df_empresa$INMUEBLE[i], " (", df_empresa$ID[i], ")")
            alias <- ifelse(is.na(df_empresa$ALIAS[i]) | df_empresa$ALIAS[i] %in% c("", "-"), "", df_empresa$ALIAS[i])
            direccion <- ifelse(is.na(df_empresa$DIRECCION[i]) | df_empresa$DIRECCION[i] %in% c("", "-"), "", df_empresa$DIRECCION[i])
            muni <- ifelse(is.na(df_empresa$MUNICIPIO[i]) | df_empresa$MUNICIPIO[i] %in% c("", "-"), "", df_empresa$MUNICIPIO[i])
            estado <- ifelse(is.na(df_empresa$ESTADO[i]) | df_empresa$ESTADO[i] %in% c("", "-"), "", df_empresa$ESTADO[i])
            pais_val <- ifelse(is.na(df_empresa$PAIS[i]) | df_empresa$PAIS[i] %in% c("", "-"), "", df_empresa$PAIS[i])
            fecha <- ifelse(is.na(df_empresa$FECHA_ESCRITURA[i]) | df_empresa$FECHA_ESCRITURA[i] %in% c("", "-"), "", df_empresa$FECHA_ESCRITURA[i])
            
            # Extraer datos numéricos (columnas 8-12) - MANTENER COMO NUMERIC
            precio <- df_empresa$PRECIO_ADQ_NUM[i]
            terreno <- df_empresa$TERRENO_NUM[i]
            precio_t <- df_empresa$PRECIO_TERRENO_NUM[i]
            const <- df_empresa$CONSTRUCCION_NUM[i]
            precio_c <- df_empresa$PRECIO_CONST_NUM[i]
            
            # Limpiar valores 0 o NA
            precio <- if(!is.na(precio) && precio != 0) precio else NA
            terreno <- if(!is.na(terreno) && terreno != 0) terreno else NA
            precio_t <- if(!is.na(precio_t) && precio_t != 0) precio_t else NA
            const <- if(!is.na(const) && const != 0) const else NA
            precio_c <- if(!is.na(precio_c) && precio_c != 0) precio_c else NA
            
            # Guardar en listas separadas
            filas_texto[[fila_idx]] <- c(inmueble_texto, alias, direccion, muni, estado, pais_val, fecha)
            filas_numeros[[fila_idx]] <- c(precio, terreno, precio_t, const, precio_c)
            
            filas_tipos[fila_idx] <- "inmueble"
            filas_heights[fila_idx] <- 20
            fila_idx <- fila_idx + 1
            
            # Acumular sumas
            if(!is.na(precio)) suma_emp_precio <- suma_emp_precio + precio
            if(!is.na(terreno)) suma_emp_terreno <- suma_emp_terreno + terreno
            if(!is.na(const)) suma_emp_const <- suma_emp_const + const
          }
          
          # FILA EMPRESA + SUBTOTAL
          empresa_texto <- ifelse(is.na(empresa) | empresa %in% c("", "-"), "Sin empresa registrada", empresa)
          
          filas_texto[[fila_idx]] <- c(paste0("      ", empresa_texto), "", "", "", "", "", "Subtotal:")
          filas_numeros[[fila_idx]] <- c(
            if(suma_emp_precio > 0) suma_emp_precio else NA,
            if(suma_emp_terreno > 0) suma_emp_terreno else NA,
            NA,
            if(suma_emp_const > 0) suma_emp_const else NA,
            NA
          )
          
          filas_tipos[fila_idx] <- "empresa"
          filas_heights[fila_idx] <- 22
          merges_acumulados[[length(merges_acumulados) + 1]] <- list(row = fila_idx, cols = 1:6)
          fila_idx <- fila_idx + 1
          
          suma_muni_precio <- suma_muni_precio + suma_emp_precio
          suma_muni_terreno <- suma_muni_terreno + suma_emp_terreno
          suma_muni_const <- suma_muni_const + suma_emp_const
        }
        
        # FILA TOTAL MUNICIPIO
        filas_texto[[fila_idx]] <- rep("", 7)
        filas_numeros[[fila_idx]] <- c(
          if(suma_muni_precio > 0) suma_muni_precio else NA,
          if(suma_muni_terreno > 0) suma_muni_terreno else NA,
          NA,
          if(suma_muni_const > 0) suma_muni_const else NA,
          NA
        )
        
        filas_tipos[fila_idx] <- "total_muni"
        filas_heights[fila_idx] <- 24
        merges_acumulados[[length(merges_acumulados) + 1]] <- list(row = fila_idx, cols = 1:6)
        fila_idx <- fila_idx + 1
        
        # # Línea en blanco
        # filas_texto[[fila_idx]] <- rep("", 7)
        # filas_numeros[[fila_idx]] <- rep(NA, 5)
        # filas_tipos[fila_idx] <- "blanco"
        # filas_heights[fila_idx] <- 15
        # fila_idx <- fila_idx + 1
      }
    }
    
    # =========================
    # 4) CREAR WORKBOOK Y ESTILOS
    # =========================
    archivo_temp <- tempfile(fileext = ".xlsx")
    wb <- createWorkbook()
    addWorksheet(wb, "Inmuebles ALMENA")
    
    # Estilos
    estilo_titulo <- createStyle(fontSize = 14, fontColour = "#DC3545", halign = "center", valign = "center", textDecoration = "bold", wrapText = TRUE)
    estilo_pais <- createStyle(fontSize = 13, fontColour = "#000000", halign = "left", valign = "center", textDecoration = "bold")
    estilo_municipio <- createStyle(fontSize = 12, fontColour = "#DC3545", halign = "left", valign = "center", textDecoration = "bold")
    estilo_empresa <- createStyle(fontSize = 11, fontColour = "#000000", halign = "left", valign = "center", textDecoration = "bold", fgFill = "#E8E8E8")
    estilo_empresa_con_borde <- createStyle(fontSize = 11, fontColour = "#000000", halign = "left", valign = "center", textDecoration = "bold", fgFill = "#E8E8E8", border = "Top", borderColour = "#000000", borderStyle = "thin")
    estilo_inmueble <- createStyle(fontSize = 10, fontColour = "#000000", halign = "left", valign = "center")
    estilo_texto <- createStyle(fontSize = 10, fontColour = "#000000", halign = "left", valign = "center", numFmt = "@")
    estilo_texto_bold <- createStyle(fontSize = 10, fontColour = "#000000", halign = "left", valign = "center", textDecoration = "bold", numFmt = "@")
    estilo_moneda <- createStyle(fontSize = 10, fontColour = "#000000", halign = "right", valign = "center", numFmt = "$#,##0.00", textDecoration = "bold")
    estilo_numerico <- createStyle(fontSize = 10, fontColour = "#000000", halign = "right", valign = "center", textDecoration = "bold", numFmt = "#,##0.00")
    estilo_moneda_normal <- createStyle(fontSize = 10, fontColour = "#000000", halign = "right", valign = "center", numFmt = "$#,##0.00")
    estilo_numerico_normal <- createStyle(fontSize = 10, fontColour = "#000000", halign = "right", valign = "center", numFmt = "#,##0.00")
    estilo_total_municipio <- createStyle(fontSize = 11, fontColour = "#000000", halign = "left", valign = "center", textDecoration = "bold")
    estilo_header <- createStyle(fontSize = 11, fontColour = "#FFFFFF", halign = "center", valign = "center", textDecoration = "bold", fgFill = "#DC3545", border = "TopBottomLeftRight", borderColour = "#000000")
    
    # =========================
    # 5) TÍTULO Y HEADERS
    # =========================
    locale_original <- Sys.getlocale("LC_TIME")
    tryCatch({ Sys.setlocale("LC_TIME", "es_ES.UTF-8") }, error = function(e) {
      tryCatch({ Sys.setlocale("LC_TIME", "Spanish") }, error = function(e2) {})
    })
    
    fecha_hora <- tolower(
      format(
        as.POSIXct(Sys.time(), tz = "America/Mexico_City"),
        "%d/%m/%y    %H:%M:%S"))
    Sys.setlocale("LC_TIME", locale_original)
    
    contenido_a1 <- paste0("ALMENA - MAESTRO DE INMUEBLES\n Generado el: ", fecha_hora)
    writeData(wb, "Inmuebles ALMENA", x = contenido_a1, startRow = 1, startCol = 1)
    addStyle(wb, "Inmuebles ALMENA", estilo_titulo, rows = 1, cols = 1)
    setRowHeights(wb, "Inmuebles ALMENA", rows = 1, heights = 45)
    
    headers <- c("ALIAS", "DIRECCIÓN", "MUNICIPIO", "ESTADO", "PAÍS", "FECHA ESCRITURA",
                 "PRECIO ADQUISICION", "TERRENO M2", "PRECIO TERRENO M2", "CONSTRUCCION M2", "PRECIO CONSTRUCCION M2")
    writeData(wb, "Inmuebles ALMENA", x = t(headers), startRow = 1, startCol = 2, colNames = FALSE)
    addStyle(wb, "Inmuebles ALMENA", estilo_header, rows = 1, cols = 2:12, gridExpand = TRUE)
    
    # =========================
    # 6) ESCRIBIR DATOS SEPARADOS (TEXTO + NUMÉRICOS)
    # =========================
    if(!is.null(progress_callback)) {
      progress_callback(0.90, " Escribiendo datos al Excel...")
    }
    
    # Convertir listas a matrices
    matriz_texto <- do.call(rbind, filas_texto)
    matriz_numeros <- do.call(rbind, filas_numeros)
    
    # Asegurarse que matriz_numeros sea realmente numérica
    matriz_numeros <- apply(matriz_numeros, 2, as.numeric)
    
    # Escribir texto (columnas A-G = 1-7)
    writeData(wb, "Inmuebles ALMENA", x = matriz_texto, 
              startRow = 3, startCol = 1, colNames = FALSE)
    
    # Escribir números (columnas H-L = 8-12) - COMO NUMÉRICOS
    writeData(wb, "Inmuebles ALMENA", x = matriz_numeros, 
              startRow = 3, startCol = 8, colNames = FALSE)
    
    # =========================
    # 7) APLICAR ESTILOS POR LOTES (MUCHO MÁS RÁPIDO)
    # =========================
    if(!is.null(progress_callback)) {
      progress_callback(0.93, " Aplicando estilos...")
    }
    
    # Identificar filas por tipo
    filas_pais <- which(filas_tipos == "pais") + 2
    filas_muni <- which(filas_tipos == "municipio") + 2
    filas_empresa <- which(filas_tipos == "empresa") + 2
    filas_inmueble <- which(filas_tipos == "inmueble") + 2
    filas_total_muni <- which(filas_tipos == "total_muni") + 2
    
    # Aplicar estilos por bloques
    if(length(filas_pais) > 0) {
      addStyle(wb, "Inmuebles ALMENA", estilo_pais, rows = filas_pais, cols = 1:12, gridExpand = TRUE, stack = FALSE)
    }
    
    if(length(filas_muni) > 0) {
      addStyle(wb, "Inmuebles ALMENA", estilo_municipio, rows = filas_muni, cols = 1:12, gridExpand = TRUE, stack = FALSE)
    }
    
    if(length(filas_empresa) > 0) {
      # Primero aplicar el fondo gris a TODA la fila (columnas 1-12)
      addStyle(wb, "Inmuebles ALMENA", estilo_empresa_con_borde, rows = filas_empresa, cols = 1:12, gridExpand = TRUE, stack = FALSE)
      
      # Luego aplicar formatos específicos ENCIMA (con stack = TRUE)
      addStyle(wb, "Inmuebles ALMENA", estilo_texto_bold, rows = filas_empresa, cols = 7, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, "Inmuebles ALMENA", estilo_moneda, rows = filas_empresa, cols = 8, gridExpand = TRUE, stack = TRUE)
      addStyle(wb, "Inmuebles ALMENA", estilo_numerico, rows = filas_empresa, cols = c(9, 11), gridExpand = TRUE, stack = TRUE)
    }
    
    if(length(filas_total_muni) > 0) {
      addStyle(wb, "Inmuebles ALMENA", estilo_total_municipio, rows = filas_total_muni, cols = 1:6, gridExpand = TRUE, stack = FALSE)
      addStyle(wb, "Inmuebles ALMENA", estilo_moneda, rows = filas_total_muni, cols = 8, gridExpand = TRUE, stack = FALSE)
      addStyle(wb, "Inmuebles ALMENA", estilo_numerico, rows = filas_total_muni, cols = c(9, 11), gridExpand = TRUE, stack = FALSE)
    }
    
    if(length(filas_inmueble) > 0) {
      addStyle(wb, "Inmuebles ALMENA", estilo_inmueble, rows = filas_inmueble, cols = 1:6, gridExpand = TRUE, stack = FALSE)
      addStyle(wb, "Inmuebles ALMENA", estilo_texto, rows = filas_inmueble, cols = 7, gridExpand = TRUE, stack = FALSE)
      addStyle(wb, "Inmuebles ALMENA", estilo_moneda_normal, rows = filas_inmueble, cols = c(8, 10, 12), gridExpand = TRUE, stack = FALSE)
      addStyle(wb, "Inmuebles ALMENA", estilo_numerico_normal, rows = filas_inmueble, cols = c(9, 11), gridExpand = TRUE, stack = FALSE)
    }
    
    # =========================
    # 8) MERGES Y HEIGHTS POR LOTES
    # =========================
    if(!is.null(progress_callback)) {
      progress_callback(0.96, " Aplicando formato final...")
    }
    
    # Aplicar todos los merges
    for(merge_info in merges_acumulados) {
      mergeCells(wb, "Inmuebles ALMENA", cols = merge_info$cols, rows = merge_info$row + 2)
    }
    
    # Aplicar todas las alturas de una vez
    todas_las_filas <- seq_along(filas_heights) + 2
    setRowHeights(wb, "Inmuebles ALMENA", rows = todas_las_filas, heights = filas_heights)
    
    # =========================
    # 9) ANCHOS DE COLUMNA
    # =========================
    setColWidths(wb, "Inmuebles ALMENA", cols = 1:12, 
                 widths = c(60, 25, 60, 20, 15, 15, 20, 22, 18, 22, 20, 25))
    
    # =========================
    # 9.5) TIPO DE CAMBIO EN TIEMPO REAL
    # =========================
    fila_tc <- length(filas_texto) + 4  # 2 de offset + 1 línea en blanco de separación
    
    usd_mxn <- NA
    eur_mxn <- NA
    fuente_fecha <- format(Sys.time(), "%d/%m/%Y %H:%M")
    
    tryCatch({
      resp_usd <- readLines(url("https://api.frankfurter.app/latest?from=USD&to=MXN"), warn = FALSE)
      datos_usd <- jsonlite::fromJSON(paste(resp_usd, collapse = ""))
      usd_mxn <- datos_usd$rates$MXN
      
      resp_eur <- readLines(url("https://api.frankfurter.app/latest?from=EUR&to=MXN"), warn = FALSE)
      datos_eur <- jsonlite::fromJSON(paste(resp_eur, collapse = ""))
      eur_mxn <- datos_eur$rates$MXN
      
      fuente_fecha <- format(Sys.time(), "%d/%m/%Y %H:%M")
      cat("✅ Tipo de cambio obtenido: USD=", usd_mxn, " EUR=", eur_mxn, "\n")
    }, error = function(e) {
      cat("⚠️ No se pudo obtener tipo de cambio:", e$message, "\n")
    })
    
    # Estilos para sección tipo de cambio
    estilo_tc_header <- createStyle(
      fontSize = 11, fontColour = "#FFFFFF", halign = "left", valign = "center",
      textDecoration = "bold", fgFill = "#401712"
    )
    estilo_tc_label <- createStyle(
      fontSize = 10, fontColour = "#401712", halign = "left", valign = "center",
      textDecoration = "bold"
    )
    estilo_tc_valor <- createStyle(
      fontSize = 10, fontColour = "#000000", halign = "right", valign = "center",
      textDecoration = "bold", numFmt = "$#,##0.0000"
    )
    estilo_tc_fuente <- createStyle(
      fontSize = 9, fontColour = "#888888", halign = "left", valign = "center",
      textDecoration = "italic"
    )
    
    # Encabezado
    writeData(wb, "Inmuebles ALMENA", x = " Tipo de Cambio ",
              startRow = fila_tc, startCol = 1)
    addStyle(wb, "Inmuebles ALMENA", estilo_tc_header,
             rows = fila_tc, cols = 1:4, gridExpand = TRUE)
    mergeCells(wb, "Inmuebles ALMENA", cols = 1:4, rows = fila_tc)
    setRowHeights(wb, "Inmuebles ALMENA", rows = fila_tc, heights = 22)
    
    # Fila USD
    writeData(wb, "Inmuebles ALMENA", x = "Dólar (USD)  →  Peso Mexicano (MXN)",
              startRow = fila_tc + 1, startCol = 1)
    writeData(wb, "Inmuebles ALMENA", x = if(!is.na(usd_mxn)) usd_mxn else "No disponible",
              startRow = fila_tc + 1, startCol = 2)
    addStyle(wb, "Inmuebles ALMENA", estilo_tc_label, rows = fila_tc + 1, cols = 1)
    if(!is.na(usd_mxn)) {
      addStyle(wb, "Inmuebles ALMENA", estilo_tc_valor, rows = fila_tc + 1, cols = 2)
    }
    setRowHeights(wb, "Inmuebles ALMENA", rows = fila_tc + 1, heights = 20)
    
    # Fila EUR
    writeData(wb, "Inmuebles ALMENA", x = "Euro (EUR)  →  Peso Mexicano (MXN)",
              startRow = fila_tc + 2, startCol = 1)
    writeData(wb, "Inmuebles ALMENA", x = if(!is.na(eur_mxn)) eur_mxn else "No disponible",
              startRow = fila_tc + 2, startCol = 2)
    addStyle(wb, "Inmuebles ALMENA", estilo_tc_label, rows = fila_tc + 2, cols = 1)
    if(!is.na(eur_mxn)) {
      addStyle(wb, "Inmuebles ALMENA", estilo_tc_valor, rows = fila_tc + 2, cols = 2)
    }
    setRowHeights(wb, "Inmuebles ALMENA", rows = fila_tc + 2, heights = 20)
    
    # Fila fuente
    writeData(wb, "Inmuebles ALMENA",
              x = paste0("Fuente: frankfurter.app  |  Actualizado: ", fuente_fecha),
              startRow = fila_tc + 3, startCol = 1)
    addStyle(wb, "Inmuebles ALMENA", estilo_tc_fuente, rows = fila_tc + 3, cols = 1:4, gridExpand = TRUE)
    mergeCells(wb, "Inmuebles ALMENA", cols = 1:4, rows = fila_tc + 3)
    setRowHeights(wb, "Inmuebles ALMENA", rows = fila_tc + 3, heights = 18)
    
    # =========================
    # 10) GUARDAR
    # =========================
    if(!is.null(progress_callback)) {
      progress_callback(0.98, " Guardando archivo...")
    }
    
    saveWorkbook(wb, archivo_temp, overwrite = TRUE)
    archivo_binario <- readBin(archivo_temp, "raw", file.info(archivo_temp)$size)
    unlink(archivo_temp)
    
    cat("✅ Excel jerárquico generado exitosamente\n")
    cat("📊 Total de filas generadas:", length(filas_texto) + 2, "\n")
    cat("🎉 ¡Descarga lista!\n")
    
    return(archivo_binario)
    
  }, error = function(e) {
    cat("❌ ERROR DETALLADO al crear Excel jerárquico:\n")
    cat("   Mensaje:", e$message, "\n")
    cat("   Llamada:", deparse(e$call), "\n")
    return(NULL)
  })
}