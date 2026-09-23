library(dplyr)
library(dbplyr)
library(stringr)
library(openxlsx)
library(readxl)
library(DBI)
library(RJDBC)
library(rJava)
library(openssl)
library(tools)
library(shiny)
library(shinyjs)
library(DT)
library(rsconnect)

# ============================================================================
# CONFIGURACIÓN INICIAL
# ============================================================================
if (!exists("script_dir")) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
    setwd(script_dir)
    cat("📁 Directorio establecido desde RStudio:", script_dir, "\n")
  } else {
    script_dir <- getwd()
    cat("📁 Directorio de trabajo:", script_dir, "\n")
  }
}

# ============================================================================
# FUNCIONES AUXILIARES
# ============================================================================
es_numerico <- function(texto) {
  if (is.null(texto) || texto == "") return(TRUE)
  return(grepl("^[0-9]+$", texto))
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

validar_longitud_identificador <- function(identificador) {
  if (is.null(identificador) || identificador == "") return(TRUE)
  return(nchar(identificador) <= 18)
}

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
# UI SIMPLIFICADA
# ============================================================================
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$style(HTML("
      /* ============================================
         ESTILOS GENERALES
         ============================================ */
      body, html {
        height: 100% !important;
        margin: 0;
        padding: 0;
        overflow-x: hidden;
        background: #e0d4c3 !important;
      }
      
      /* ============================================
         PANTALLA DE LOGIN
         ============================================ */
      .login-container {
        display: flex;
        justify-content: center;
        align-items: center;
        min-height: 100vh;
        background: linear-gradient(135deg, #e0d4c3 0%, #e0d4c3 100%);
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
        color: #4A2E05;
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
        border: 2px solid #802e25;
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
        background: linear-gradient(135deg, #802e25 0%, #401712 100%) !important;
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
        background: linear-gradient(135deg, #401712 0%, #C0510D 100%) !important;
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(230, 126, 34, 0.4);
      }
      
      .login-error {
        background: linear-gradient(135deg, #e0d4c3 0%, #ff9d92 100%);
        border: 1px solid #802e25;
        color: #4A2E05;
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
      
      /* ============================================
         HEADER / TÍTULO
         ============================================ */
      .app-header {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        padding: 20px;
        text-align: center;
        box-shadow: 0 4px 15px rgba(230, 126, 34, 0.3);
        margin-bottom: 30px;
      }
      
      .app-title {
        color: white;
        font-size: 2rem;
        font-weight: bold;
        margin: 0;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.2);
      }
      
      /* ============================================
         CAJAS DE CONTENIDO
         ============================================ */
      .content-box {
        background: white;
        padding: 20px;
        border-radius: 10px;
        border: 3px solid #802e25;
        box-shadow: 0 4px 15px rgba(230, 126, 34, 0.2);
        margin-bottom: 20px;
      }
      
      .box-header {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        padding: 10px;
        border-radius: 8px;
        margin-bottom: 15px;
      }
      
      /* ============================================
         CAMPOS DE FORMULARIO
         ============================================ */
      .required-field {
        color: #802e25;
        font-weight: bold;
      }
      
      .input-error {
        border: 2px solid #802e25 !important;
        background-color: #FFF5F0 !important;
      }
      
      .validation-message {
        color: #802e25;
        font-size: 12px;
        margin-top: 5px;
        font-weight: bold;
      }
      
      .form-control {
        border: 2px solid #e0d4c3;
        border-radius: 6px;
        padding: 10px;
        transition: all 0.3s ease;
      }
      
      .form-control:focus {
        border-color: #802e25;
        box-shadow: 0 0 10px rgba(230, 126, 34, 0.3);
        outline: none;
      }
      
      /* ============================================
         BOTONES
         ============================================ */
      .btn-primary {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%) !important;
        color: white !important;
        border: none !important;
        padding: 10px 25px;
        border-radius: 8px;
        font-weight: bold;
        transition: all 0.3s ease;
        box-shadow: 0 4px 12px rgba(230, 126, 34, 0.3);
      }
      
      .btn-primary:hover {
        background: linear-gradient(135deg, #401712 0%, #C0510D 100%) !important;
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(230, 126, 34, 0.4);
      }
      
      .btn-success {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%) !important;
        color: white !important;
        border: none !important;
      }
      
      .btn-danger {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%) !important;
        color: white !important;
        border: none !important;
      }
      
      /* ============================================
         ALERTAS Y NOTIFICACIONES
         ============================================ */
      .alert-success {
        background: linear-gradient(135deg, #e0d4c3 0%, #e0d4c3 100%);
        border: 1px solid #ff5b49;
        color: #4A2E05;
      }
      
      .alert-danger {
        background: linear-gradient(135deg, #e0d4c3 0%, #ff9d92 100%);
        border: 1px solid #802e25;
        color: #4A2E05;
      }
      
      .alert-warning {
        background: linear-gradient(135deg, #e0d4c3 0%, #e0d4c3 100%);
        border: 1px solid #ff5b49;
        color: #6B3A00;
      }
      
      /* ============================================
         TABLA DATATABLE
         ============================================ */
      .dataTables_wrapper {
        padding: 15px;
      }
      
      .dataTables_scrollBody::-webkit-scrollbar {
        width: 10px;
        height: 10px;
      }
      
      .dataTables_scrollBody::-webkit-scrollbar-track {
        background: #e0d4c3;
        border-radius: 10px;
      }
      
      .dataTables_scrollBody::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        border-radius: 10px;
      }
      
      .dataTables_scrollBody::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #401712 0%, #C0510D 100%);
      }
      
      table.dataTable thead th {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%) !important;
        color: white !important;
        font-weight: bold !important;
        text-align: center !important;
        padding: 12px !important;
      }
      
      table.dataTable tbody tr:nth-child(even) {
        background-color: #FFF5F0 !important;
      }
      
      table.dataTable tbody tr:hover {
        background-color: #e0d4c3 !important;
      }
      
      /* ============================================
         UPLOAD DE ARCHIVOS
         ============================================ */
      .file-upload-area {
        border: 3px dashed #802e25;
        border-radius: 10px;
        padding: 20px;
        text-align: center;
        background: white;
        transition: all 0.3s ease;
      }
      
      .file-upload-area:hover {
        border-color: #401712;
        background: #e0d4c3;
      }
      
      /* ============================================
         MODAL
         ============================================ */
      .modal-header {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        color: white;
      }
      
      .modal-content {
        border-radius: 15px;
        border: none;
        box-shadow: 0 10px 40px rgba(0,0,0,0.3);
      }
      
      /* ============================================
         BARRA DE PROGRESO
         ============================================ */
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
        background: linear-gradient(135deg, #e0d4c3 0%, #e0d4c3 100%);
        overflow: hidden;
      }
      
      .shiny-notification .progress-bar {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%);
        box-shadow: 0 2px 8px rgba(230, 126, 34, 0.5);
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
      
      /* ============================================
         ANIMACIONES
         ============================================ */
      .fade-in {
        animation: fadeIn 0.5s ease-in;
      }
      
      /* Sin scroll de página en la pestaña de tabla */
body, html {
  overflow-x: hidden;
}

/* Pestaña de tabla sin scroll de página */
#tab_tabla {
  overflow: hidden;
}

#tab_tabla > div {
  overflow: hidden;
}

/* La tabla tiene su propio scroll interno */
#tabla_maestro_inmuebles_registro {
  font-size: 13px;
}

#tabla_maestro_inmuebles_registro .dataTables_scrollBody {
  overflow-y: auto !important;
}
      
      @keyframes fadeIn {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
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
                "Sistema de Registro de Inmuebles")
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
      
      # ============================================================================
      # HEADER
      # ============================================================================
      div(class = "app-header",
          h1(class = "app-title",
             icon("building"), " M.I. ALMENA - Registro de Inmuebles")
      ),
      
      # Estilos para las pestañas
      tags$style(HTML("
      .nav-tabs {
        border-bottom: 3px solid #802e25 !important;
        margin-bottom: 0 !important;
      }
      .nav-tabs > li > a {
        color: #802e25 !important;
        font-weight: bold !important;
        font-size: 15px !important;
        border-radius: 8px 8px 0 0 !important;
      }
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        background: linear-gradient(135deg, #802e25 0%, #401712 100%) !important;
        color: white !important;
        border-color: #802e25 !important;
      }
      .tab-content {
        background: #e0d4c3;
        padding: 20px 0 0 0;
      }
    ")),
      
      tabsetPanel(
        id = "pestanas_principales",
        
        # ========================================================================
        # PESTAÑA 1: REGISTRO DE INMUEBLES
        # ========================================================================
        tabPanel(
          title = tagList(icon("plus-circle"), " Registrar Inmueble"),
          value = "tab_registro",
          
          div(style = "padding-top: 20px;",
              
              # FILA 1: INFORMACIÓN BÁSICA (3 COLUMNAS)
              fluidRow(
                column(4,
                       div(class = "content-box",
                           div(class = "box-header",
                               h3(style = "color: white; font-weight: bold; font-size: 18px; margin: 5px;",
                                  "INFORMACIÓN BÁSICA")
                           ),
                           
                           textInput("id_inmueble",
                                     HTML("ID (Auto-generado - Editable): <span class='required-field'>*</span>"),
                                     value = "",
                                     placeholder = "Generando ID automático..."),
                           div(id = "id_validation", class = "validation-message",
                               style = "color: #28a745; font-weight: bold; font-size: 11px;"),
                           
                           selectizeInput("nombre_inmueble",
                                          HTML("INMUEBLE: <span class='required-field'>*</span>"),
                                          choices = NULL,
                                          options = list(
                                            create = TRUE,
                                            placeholder = "Seleccione o escriba el nombre del inmueble",
                                            maxItems = 1
                                          )),
                           div(id = "inmueble_validation", class = "validation-message"),
                           
                           selectizeInput("empresa",
                                          HTML("EMPRESA: <span class='required-field'>*</span>"),
                                          choices = NULL,
                                          options = list(
                                            create = TRUE,
                                            placeholder = "Seleccione o escriba el nombre de la empresa",
                                            maxItems = 1
                                          )),
                           
                           textInput("alias", HTML("ALIAS: <span class='required-field'>*</span>"),
                                     placeholder = "Alias del inmueble")
                       )
                ),
                
                column(4,
                       div(class = "content-box",
                           div(class = "box-header",
                               h3(style = "color: white; font-weight: bold; font-size: 18px; margin: 5px;",
                                  "UBICACIÓN")
                           ),
                           
                           textAreaInput("direccion", HTML("DIRECCIÓN: <span class='required-field'>*</span>"),
                                         placeholder = "Dirección completa", rows = 3),
                           
                           textInput("municipio", HTML("MUNICIPIO: <span class='required-field'>*</span>"),
                                     placeholder = "Municipio"),
                           
                           selectizeInput("estado",
                                          HTML("ESTADO: <span class='required-field'>*</span>"),
                                          choices = c(
                                            # --- MÉXICO ---
                                            "Aguascalientes" = "AGS", "Baja California" = "BC", "Baja California Sur" = "BCS",
                                            "Campeche" = "CAMP", "Chiapas" = "CHIS", "Chihuahua" = "CHIH", "Ciudad de México" = "CDMX",
                                            "Coahuila" = "COAH", "Colima" = "COL", "Durango" = "DGO", "Guanajuato" = "GTO",
                                            "Guerrero" = "GRO", "Hidalgo" = "HGO", "Jalisco" = "JAL", "Estado de México" = "MEX",
                                            "Michoacán" = "MICH", "Morelos" = "MOR", "Nayarit" = "NAY", "Querétaro" = "NL",
                                            "Oaxaca" = "OAX", "Puebla" = "PUE", "Querétaro" = "QRO", "Quintana Roo" = "QROO",
                                            "San Luis Potosí" = "SLP", "Sinaloa" = "SIN", "Sonora" = "SON", "Tabasco" = "TAB",
                                            "Tamaulipas" = "TAMPS", "Tlaxcala" = "TLAX", "Veracruz" = "VER", "Yucatán" = "YUC",
                                            "Zacatecas" = "ZAC",
                                            # --- USA ---
                                            "Texas" = "TEX", "California" = "CA", "Arizona" = "AZ", "New Mexico" = "NM",
                                            "Florida" = "FL", "Louisiana" = "LA", "Alabama" = "AL", "Mississippi" = "MS",
                                            "Georgia" = "GA", "Nevada" = "NV", "Utah" = "UT", "Colorado" = "CO",
                                            "Oklahoma" = "OK", "Arkansas" = "AR", "Tennessee" = "TN", "North Carolina" = "NC",
                                            "South Carolina" = "SC", "Virginia" = "VA", "Maryland" = "MD", "Kansas" = "KS",
                                            "Missouri" = "MO", "Illinois" = "IL", "New York" = "NY", "Oregon" = "OR", "Washington" = "WA",
                                            # --- ESPAÑA ---
                                            "España" = "ESP"
                                          ),
                                          selected = NULL,
                                          options = list(
                                            create = FALSE,
                                            placeholder = "Seleccione un estado",
                                            maxItems = 1
                                          )),
                           
                           selectizeInput("pais",
                                          HTML("PAÍS: <span class='required-field'>*</span>"),
                                          choices = c("México" = "MEX", "España" = "ESP", "Estados Unidos" = "EU"),
                                          selected = "MEX",
                                          options = list(
                                            create = TRUE,
                                            placeholder = "Seleccione o escriba un país",
                                            maxItems = 1
                                          ))
                       )
                ),
                
                column(4,
                       div(class = "content-box",
                           div(class = "box-header",
                               h3(style = "color: white; font-weight: bold; font-size: 18px; margin: 5px;",
                                  "MEDIDAS DEL INMUEBLE")
                           ),
                           
                           fluidRow(
                             column(6,
                                    numericInput("terreno_m2", "TERRENO M²:",
                                                 value = NA, min = 0, step = 0.01)
                             ),
                             column(6,
                                    numericInput("precio_terreno_m2", "PRECIO/M² TERRENO ($):",
                                                 value = NA, min = 0, step = 0.01)
                             )
                           ),
                           
                           fluidRow(
                             column(6,
                                    numericInput("construccion_m2", "CONSTRUCCIÓN M²:",
                                                 value = NA, min = 0, step = 0.01)
                             ),
                             column(6,
                                    numericInput("precio_construccion_m2", "PRECIO/M² CONST. ($):",
                                                 value = NA, min = 0, step = 0.01)
                             )
                           )
                       )
                )
              ),
              
              # FILA 2: INFORMACIÓN FINANCIERA Y ARCHIVO KMZ
              fluidRow(
                column(6,
                       div(class = "content-box",
                           div(class = "box-header",
                               h3(style = "color: white; font-weight: bold; font-size: 18px; margin: 5px;",
                                  "INFORMACIÓN FINANCIERA")
                           ),
                           
                           fluidRow(
                             column(6,
                                    dateInput("fecha_escritura", HTML("FECHA ESCRITURA: <span class='required-field'>*</span>"),
                                              format = "yyyy-mm-dd")
                             ),
                             column(6,
                                    numericInput("precio_adquisicion", HTML("PRECIO ($): <span class='required-field'>*</span>"),
                                                 value = NA, min = 0, step = 0.01)
                             )
                           )
                       )
                ),
                
                column(6,
                       div(class = "content-box",
                           div(class = "box-header",
                               h3(style = "color: white; font-weight: bold; font-size: 18px; margin: 5px;",
                                  "ARCHIVO KMZ")
                           ),
                           
                           fileInput("kmz_upload",
                                     label = "Seleccionar KMZ:",
                                     accept = c(".kmz"),
                                     placeholder = "Ningún archivo",
                                     buttonLabel = "📂 Adjuntar",
                                     width = "100%"),
                           
                           uiOutput("kmz_status"),
                           
                           div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #e0d4c3 100%);
                                      padding: 8px;
                                      border-radius: 6px;
                                      margin-top: 8px;
                                      border-left: 3px solid #ff5b49;",
                               p(style = "margin: 0; color: #4A2E05; font-size: 11px;",
                                 "ℹ️ Adjuntar el polígono del inmueble")
                           )
                       )
                )
              ),
              
              # FILA 3: BOTONES DE ACCIÓN
              fluidRow(
                column(12,
                       div(style = "text-align: center; margin: 30px 0;",
                           actionButton("guardar", " Guardar Inmueble ",
                                        class = "btn btn-primary btn-lg",
                                        style = "padding: 15px 50px; font-size: 18px; margin-right: 15px;"),
                           
                           actionButton("limpiar", "🗑️ Limpiar Campos",
                                        class = "btn btn-lg",
                                        style = "background: linear-gradient(135deg, #ff5b49 0%, #802e25 100%);
                                               color: white;
                                               border: none;
                                               padding: 15px 50px;
                                               font-size: 18px;
                                               font-weight: bold;
                                               border-radius: 8px;
                                               box-shadow: 0 4px 15px rgba(243, 156, 18, 0.3);")
                       )
                )
              )
          )
        ),
        
        # ========================================================================
        # PESTAÑA 2: BASE DE DATOS
        # ========================================================================
        tabPanel(
          title = tagList(icon("table"), " Base de Datos"),
          value = "tab_tabla",
          
          div(style = "padding-top: 15px;",
              div(style = "background: white;
                         padding: 20px;
                         border-radius: 10px;
                         border: 3px solid #802e25;
                         box-shadow: 0 4px 15px rgba(230, 126, 34, 0.2);",
                  
                  div(style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                             padding: 15px;
                             border-radius: 8px;
                             margin-bottom: 15px;
                             display: flex;
                             justify-content: space-between;
                             align-items: center;",
                      h4(style = "color: white; margin: 0; font-weight: bold;",
                         " REGISTROS ACTUALES")
                  ),
                  
                  DT::dataTableOutput("tabla_maestro_inmuebles_registro")
              )
          )
        )
      )
  )
)

# ============================================================================
# SERVER
# ============================================================================
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
  
  # ==========================================================================
  # FUNCIÓN: Conectar a Snowflake con JWT
  # ==========================================================================
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
  
  # ==========================================================================
  # FUNCIÓN: Procesar KMZ individual
  # ==========================================================================
  procesar_kmz_individual <- function(ruta_kmz) {
    tryCatch({
      temp_dir <- tempdir()
      unzip(ruta_kmz, exdir = temp_dir, overwrite = TRUE)
      
      kml_files <- list.files(temp_dir, pattern = "\\.kml$", full.names = TRUE)
      if (length(kml_files) == 0) return(NULL)
      
      # Retornar solo la ruta del KMZ procesado
      return(ruta_kmz)
      
    }, error = function(e) {
      cat("❌ Error procesando KMZ:", e$message, "\n")
      return(NULL)
    })
  }
  
  # ==========================================================================
  # FUNCIÓN: Guardar inmueble en Snowflake (ACTUALIZADA PARA 3 TABLAS)
  # ==========================================================================
  guardar_inmueble_snowflake <- function(datos_inmueble, poligono_kmz = NULL) {
    tryCatch({
      conn <- crear_conexion_snowflake()
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
      
      # Escapar comillas para seguridad
      id_limpio <- gsub("'", "''", datos_inmueble$ID)
      inmueble_limpio <- gsub("'", "''", datos_inmueble$INMUEBLE)
      empresa_limpia <- gsub("'", "''", datos_inmueble$EMPRESA)
      alias_limpio <- gsub("'", "''", datos_inmueble$ALIAS)
      direccion_limpia <- gsub("'", "''", datos_inmueble$DIRECCION)
      municipio_limpio <- gsub("'", "''", datos_inmueble$MUNICIPIO)
      estado_limpio <- gsub("'", "''", datos_inmueble$ESTADO)
      pais_limpio <- gsub("'", "''", datos_inmueble$PAIS)
      
      v_terr   <- if(is.na(datos_inmueble$TERRENO_M2)) "NULL" else datos_inmueble$TERRENO_M2
      v_pterr  <- if(is.na(datos_inmueble$PRECIO_TERRENO_M2)) "NULL" else datos_inmueble$PRECIO_TERRENO_M2
      v_const  <- if(is.na(datos_inmueble$CONSTRUCCION_M2)) "NULL" else datos_inmueble$CONSTRUCCION_M2
      v_pconst <- if(is.na(datos_inmueble$PRECIO_CONSTRUCCION_M2)) "NULL" else datos_inmueble$PRECIO_CONSTRUCCION_M2
      
      fecha_escritura_formateada <- paste0("'", datos_inmueble$FECHA_ESCRITURA, "'")
      
      # 1. Verificar si existe en la tabla principal
      existe_sql <- sprintf("SELECT COUNT(*) as count FROM INMUEBLES WHERE ID = '%s'", id_limpio)
      resultado_existe <- dbGetQuery(conn, existe_sql)
      ya_existe <- resultado_existe$COUNT > 0
      
      if(ya_existe) {
        # --- LÓGICA DE ACTUALIZACIÓN ---
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
          "TERRENO_M2 = ", v_terr, ", ", 
          "PRECIO_TERRENO_M2 = ", v_pterr, ", ", 
          "CONSTRUCCION_M2 = ", v_const, ", ", 
          "PRECIO_CONSTRUCCION_M2 = ", v_pconst, ", ", 
          "FECHA_ACTUALIZACION = CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ ",
          "WHERE ID = '", id_limpio, "'"
        )
        dbSendUpdate(conn, update_sql)
        tipo_operacion <- "actualizado"
        
      } else {
        # --- LÓGICA DE NUEVO REGISTRO ---
        insert_principal <- sprintf(
          "INSERT INTO INMUEBLES (ID, INMUEBLE, EMPRESA, ALIAS, DIRECCION, MUNICIPIO, ESTADO, PAIS, FECHA_ESCRITURA, PRECIO_ADQUISICION, TERRENO_M2, PRECIO_TERRENO_M2, CONSTRUCCION_M2, PRECIO_CONSTRUCCION_M2, FECHA_ACTUALIZACION) 
         VALUES ('%s', '%s', '%s', '%s', '%s', '%s', '%s', '%s', %s, %f, %s, %s, %s, %s, CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ)",
          id_limpio, inmueble_limpio, empresa_limpia, alias_limpio, direccion_limpia, municipio_limpio, estado_limpio, pais_limpio, 
          fecha_escritura_formateada, datos_inmueble$PRECIO_ADQUISICION, v_terr, v_pterr, v_const, v_pconst
        )
        dbSendUpdate(conn, insert_principal)
        
        insert_status <- sprintf(
          "INSERT INTO INMUEBLES_STATUS (ID, STATUS, FECHA_DE_ASIGNACION, USUARIO) 
         VALUES ('%s', NULL, NULL, NULL)",
          id_limpio
        )
        dbSendUpdate(conn, insert_status)
        
        insert_actividad <- sprintf(
          "INSERT INTO INMUEBLE_ACTIVIDAD (ID, STATUS_ACTIVIDAD, FECHA_ASIGNACION, USUARIO) 
         VALUES ('%s', 'Activo', CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ, 'SISTEMA')",
          id_limpio
        )
        dbSendUpdate(conn, insert_actividad)
        
        cat("✨ Nuevo registro vinculado en las 3 tablas para el ID:", id_limpio, "\n")
        tipo_operacion <- "guardado"
      }
      
      dbDisconnect(conn)
      return(list(exito = TRUE, tipo = tipo_operacion, poligono = poligono_kmz))
      
    }, error = function(e) {
      cat("❌ Error en operación de tres tablas:", e$message, "\n")
      if(exists("conn")) dbDisconnect(conn)
      return(list(exito = FALSE, mensaje = e$message))
    })
  }
  
  # ==========================================================================
  # FUNCIÓN: Cargar inmuebles desde Snowflake
  # ==========================================================================
  cargar_inmuebles_snowflake <- function() {
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
          i.FECHA_ACTUALIZACION
        FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
      "
      
      inmuebles <- dbGetQuery(conn, query_join)
      dbDisconnect(conn)
      
      if(nrow(inmuebles) == 0) {
        showNotification("⚠️ No hay inmuebles en Snowflake",
                         type = "warning", duration = 3)
        inmuebles_snowflake_df(data.frame())
        return(FALSE)
      }
      
      # Intentar obtener el STATUS más reciente del historial
      tryCatch({
        conn2 <- crear_conexion_snowflake()
        dbSendUpdate(conn2, "USE WAREHOUSE WH_ANALYTICS;")
        
        # Usamos QUALIFY y ROW_NUMBER para tomar solo el registro más nuevo por cada ID
        # Consulta para obtener solo la última "foto" de cada inmueble en el historial
        query_actividad <- "
          SELECT 
            ID,
            STATUS_ACTIVIDAD,
            FECHA_ASIGNACION,
            USUARIO
          FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
          QUALIFY ROW_NUMBER() OVER (
            PARTITION BY ID 
            ORDER BY FECHA_ASIGNACION DESC
          ) = 1
        "
        
        actividad_data <- dbGetQuery(conn2, query_actividad)
        dbDisconnect(conn2)
        
        if(nrow(actividad_data) > 0) {
          inmuebles <- merge(inmuebles, actividad_data, by = "ID", all.x = TRUE)
        } else {
          inmuebles$STATUS_ACTIVIDAD <- "ACTIVO"
          inmuebles$FECHA_ASIGNACION <- as.POSIXct(NA)
          inmuebles$USUARIO_ACTIVIDAD <- NA
        }
        
      }, error = function(e) {
        inmuebles$STATUS_ACTIVIDAD <- "ACTIVO"
        inmuebles$FECHA_ASIGNACION <- as.POSIXct(NA)
        inmuebles$USUARIO_ACTIVIDAD <- NA
      })
      
      # Filtrar solo ACTIVOS
      inmuebles$STATUS_ACTIVIDAD[is.na(inmuebles$STATUS_ACTIVIDAD) |
                                   inmuebles$STATUS_ACTIVIDAD == ""] <- "Activo"
      
      inmuebles$STATUS_ACTIVIDAD_UPPER <- toupper(trimws(inmuebles$STATUS_ACTIVIDAD))
      inmuebles_activos <- inmuebles[inmuebles$STATUS_ACTIVIDAD_UPPER == "ACTIVO", ]
      inmuebles_activos$STATUS_ACTIVIDAD_UPPER <- NULL
      
      cat(" Total de inmuebles en Snowflake:", nrow(inmuebles), "\n")
      cat("✅ Inmuebles ACTIVOS:", nrow(inmuebles_activos), "\n")
      
      if(nrow(inmuebles_activos) == 0) {
        showNotification("⚠️ No hay inmuebles ACTIVOS en Snowflake",
                         type = "warning", duration = 3)
      } else {
        showNotification("✅ INMUEBLES ACTIVOS cargados correctamente",
                         type = "message", duration = 2)
      }
      
      inmuebles_snowflake_df(inmuebles_activos)
      return(TRUE)
      
    }, error = function(e) {
      cat("❌ Error al cargar INMUEBLES desde Snowflake:", e$message, "\n")
      showNotification(paste("Error al cargar INMUEBLES:", e$message),
                       type = "error", duration = 5)
      inmuebles_snowflake_df(data.frame())
      return(FALSE)
    })
  }
  
  # ==========================================================================
  # FUNCIÓN: Cambiar status de actividad
  # ==========================================================================
  cambiar_status_actividad <- function(id_inmueble, nuevo_status = "Inactivo", status_inmueble = NULL, usuario = "SISTEMA") {
    tryCatch({
      conn <- crear_conexion_snowflake()
      dbSendUpdate(conn, "USE WAREHOUSE WH_ANALYTICS;")
      
      id_limpio      <- gsub("'", "''", id_inmueble)
      status_limpio  <- gsub("'", "''", nuevo_status)
      usuario_limpio <- gsub("'", "''", usuario)
      status_inm_val <- if (!is.null(status_inmueble) && status_inmueble != "") {
        paste0("'", gsub("'", "''", status_inmueble), "'")
      } else "NULL"
      
      insert_sql <- sprintf(
        "INSERT INTO DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD 
       (ID, STATUS_ACTIVIDAD, FECHA_ASIGNACION, USUARIO, STATUS_INMUEBLE)
       VALUES ('%s', '%s', CONVERT_TIMEZONE('America/Mexico_City', CURRENT_TIMESTAMP())::TIMESTAMP_NTZ, '%s', %s)",
        id_limpio, status_limpio, usuario_limpio, status_inm_val
      )
      
      dbSendUpdate(conn, insert_sql)
      dbDisconnect(conn)
      return(TRUE)
      
    }, error = function(e) {
      if(exists("conn")) dbDisconnect(conn)
      return(FALSE)
    })
  }
  
  # ==========================================================================
  # VARIABLES REACTIVAS
  # ==========================================================================
  inmuebles_snowflake_df <- reactiveVal(NULL)
  kmz_temporal <- reactiveVal(NULL)
  
  # ==========================================================================
  # FUNCIÓN: Generar ID Automático
  # ==========================================================================
  generar_id_automatico <- function() {
    tryCatch({
      # 1. Obtener fecha actual en formato DDMMYYYY
      fecha_hoy <- format(Sys.Date(), "%d%m%Y")
      
      # 2. Obtener el número de inmuebles actuales
      datos <- inmuebles_snowflake_df()
      conteo_activos <- if (is.null(datos) || nrow(datos) == 0) 0 else nrow(datos)
      
      # 3. Fórmula: Fecha + (Conteo + 1)
      nuevo_id <- paste0(fecha_hoy, (conteo_activos + 1))
      
      cat("🆔 ID Generado:", nuevo_id, 
          "| Fecha:", fecha_hoy, 
          "| Activos:", conteo_activos, "\n")
      
      return(nuevo_id)
      
    }, error = function(e) {
      cat("❌ Error generando ID:", e$message, "\n")
      return(paste0(format(Sys.Date(), "%d%m%Y"), "001"))
    })
  }
  
  debe_generar_nuevo_id <- reactiveVal(TRUE)
  
  # ==========================================================================
  # OBSERVADOR: Generar ID SOLO cuando sea necesario
  # ==========================================================================
  observe({
    req(usuario_autenticado()) # Solo cuando esté autenticado
    
    # Solo generar si la bandera está activada
    if (debe_generar_nuevo_id()) {
      nuevo_id <- generar_id_automatico()
      updateTextInput(session, "id_inmueble", value = nuevo_id)
      shinyjs::html("id_validation", 
                    paste0(" ID generado: ", nuevo_id, " (editable)"))
      
      # Desactivar bandera para evitar regeneración constante
      debe_generar_nuevo_id(FALSE)
    }
  })
  
  # ==========================================================================
  # CARGAR DATOS AL INICIAR
  # ==========================================================================
  observe({
    if(is.null(inmuebles_snowflake_df())) {
      cat("🔄 Cargando inmuebles al iniciar app...\n")
      cargar_inmuebles_snowflake()
    }
  })
  
  # ==========================================================================
  # FUNCIÓN AUXILIAR: Normalizar texto para deduplicación
  # ==========================================================================
  normalizar_texto <- function(x) {
    x <- toupper(trimws(x))
    x <- gsub("[áàäâã]", "A", x, ignore.case = TRUE)
    x <- gsub("[éèëê]",  "E", x, ignore.case = TRUE)
    x <- gsub("[íìïî]",  "I", x, ignore.case = TRUE)
    x <- gsub("[óòöôõ]", "O", x, ignore.case = TRUE)
    x <- gsub("[úùüû]",  "U", x, ignore.case = TRUE)
    x <- gsub("[^A-Z0-9 ]", "", x)
    x <- gsub("\\s+", " ", x)
    trimws(x)
  }
  
  # ==========================================================================
  # ACTUALIZAR CATÁLOGOS
  # ==========================================================================
  observe({
    datos_sf <- inmuebles_snowflake_df()
    
    if (!is.null(datos_sf) && nrow(datos_sf) > 0) {
      
      empresas_raw <- datos_sf$EMPRESA[!is.na(datos_sf$EMPRESA) &
                                         datos_sf$EMPRESA != "" &
                                         datos_sf$EMPRESA != "-"]
      if (length(empresas_raw) > 0) {
        df_emp <- data.frame(
          original = empresas_raw,
          clave    = normalizar_texto(empresas_raw),
          n_mayusc = nchar(gsub("[^A-ZÁÉÍÓÚÜÑ]", "", empresas_raw)),
          stringsAsFactors = FALSE
        )
        df_emp        <- df_emp[order(df_emp$clave, -df_emp$n_mayusc), ]
        df_emp        <- df_emp[!duplicated(df_emp$clave), ]
        empresas_unicas <- sort(df_emp$original)
      } else {
        empresas_unicas <- character(0)
      }
      
      updateSelectizeInput(session, "empresa",
                           choices = c("", empresas_unicas),
                           selected = "",
                           server = TRUE)
      
      inmuebles_unicos <- sort(unique(datos_sf$INMUEBLE[!is.na(datos_sf$INMUEBLE) &
                                                          datos_sf$INMUEBLE != "" &
                                                          datos_sf$INMUEBLE != "-"]))
      
      updateSelectizeInput(session, "nombre_inmueble",
                           choices = c("", inmuebles_unicos),
                           selected = "",
                           server = TRUE)
    }
  })
  
  # ==========================================================================
  # VALIDACIÓN: Campo ID
  # ==========================================================================
  observeEvent(input$id_inmueble, {
    id_inmueble <- input$id_inmueble
    shinyjs::html("id_validation", "")
    shinyjs::removeClass("id_inmueble", "input-error")
    
    if (!is.null(id_inmueble) && id_inmueble != "") {
      if (nchar(id_inmueble) > 50) {
        shinyjs::html("id_validation", "⚠️ ID muy largo, máximo 50 caracteres")
        shinyjs::addClass("id_inmueble", "input-error")
      } else {
        shinyjs::html("id_validation", 
                      paste0("✓ ID personalizado: ", id_inmueble))
      }
    }
  })
  
  observeEvent(input$nombre_inmueble, {
    nombre <- input$nombre_inmueble
    shinyjs::html("inmueble_validation", "")
    shinyjs::removeClass("nombre_inmueble", "input-error")
    
    if (!is.null(nombre) && nombre != "") {
      if (nchar(nombre) > 100) {
        shinyjs::html("inmueble_validation", "⚠️ Nombre muy largo, máximo 100 caracteres")
        shinyjs::addClass("nombre_inmueble", "input-error")
      }
      
      # --- AUTO-RELLENO: si el inmueble ya existe en BD, cargar sus datos ---
      datos_sf <- inmuebles_snowflake_df()
      if (!is.null(datos_sf) && nrow(datos_sf) > 0) {
        fila <- datos_sf[trimws(datos_sf$INMUEBLE) == trimws(nombre), ]
        if (nrow(fila) > 0) {
          fila <- fila[1, ]
          
          updateTextInput(session,      "id_inmueble", value = fila$ID)
          updateSelectizeInput(session, "empresa",     selected = fila$EMPRESA)
          updateTextInput(session,      "alias",       value = if(is.na(fila$ALIAS) || fila$ALIAS == "-") "" else fila$ALIAS)
          updateTextAreaInput(session,  "direccion",   value = if(is.na(fila$DIRECCION) || fila$DIRECCION == "-") "" else fila$DIRECCION)
          updateTextInput(session,      "municipio",   value = if(is.na(fila$MUNICIPIO) || fila$MUNICIPIO == "-") "" else fila$MUNICIPIO)
          updateSelectizeInput(session, "estado",      selected = fila$ESTADO)
          updateSelectizeInput(session, "pais",        selected = fila$PAIS)
          
          tryCatch({
            fecha_val <- fila$FECHA_ESCRITURA
            if (!is.na(fecha_val) && as.character(fecha_val) != "" && as.character(fecha_val) != "-")
              updateDateInput(session, "fecha_escritura", value = as.Date(fecha_val))
          }, error = function(e) {})
          
          precio_num <- suppressWarnings(as.numeric(fila$PRECIO_ADQUISICION))
          if (!is.na(precio_num) && precio_num > 0)
            updateNumericInput(session, "precio_adquisicion", value = precio_num)
          
          terreno_num <- suppressWarnings(as.numeric(fila$TERRENO_M2))
          if (!is.na(terreno_num) && terreno_num > 0)
            updateNumericInput(session, "terreno_m2", value = terreno_num)
          
          pterreno_num <- suppressWarnings(as.numeric(fila$PRECIO_TERRENO_M2))
          if (!is.na(pterreno_num) && pterreno_num > 0)
            updateNumericInput(session, "precio_terreno_m2", value = pterreno_num)
          
          const_num <- suppressWarnings(as.numeric(fila$CONSTRUCCION_M2))
          if (!is.na(const_num) && const_num > 0)
            updateNumericInput(session, "construccion_m2", value = const_num)
          
          pconst_num <- suppressWarnings(as.numeric(fila$PRECIO_CONSTRUCCION_M2))
          if (!is.na(pconst_num) && pconst_num > 0)
            updateNumericInput(session, "precio_construccion_m2", value = pconst_num)
          
          showNotification(paste0("ℹ️ Datos cargados desde: ", nombre),
                           type = "message", duration = 3)
        }
      }
    }
  }, ignoreInit = TRUE)
  
  # ==========================================================================
  # OBSERVAR CARGA DE KMZ
  # ==========================================================================
  observeEvent(input$kmz_upload, {
    req(input$kmz_upload)
    
    showNotification(" Procesando archivo KMZ...", type = "message", duration = 2)
    
    poligono <- procesar_kmz_individual(input$kmz_upload$datapath)
    
    if (!is.null(poligono)) {
      kmz_temporal(poligono)
      
      output$kmz_status <- renderUI({
        div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #e0d4c3 100%);
                     padding: 12px;
                     border-radius: 8px;
                     border-left: 4px solid #ff5b49;
                     margin-bottom: 10px;",
            p(style = "margin: 0; color: #4A2E05; font-weight: bold;",
              "✅ KMZ cargado correctamente"),
            p(style = "margin: 5px 0 0 0; color: #6B3A00; font-size: 12px;",
              paste("📎 Archivo:", input$kmz_upload$name))
        )
      })
      
      showNotification("✅ KMZ procesado exitosamente", type = "message", duration = 3)
      
    } else {
      output$kmz_status <- renderUI({
        div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #ff9d92 100%);
                     padding: 12px;
                     border-radius: 8px;
                     border-left: 4px solid #802e25;
                     margin-bottom: 10px;",
            p(style = "margin: 0; color: #6B3A00; font-weight: bold;",
              "❌ Error al procesar el archivo KMZ"),
            p(style = "margin: 5px 0 0 0; color: #8B4513; font-size: 12px;",
              "Verifica que el archivo sea válido y contenga polígonos.")
        )
      })
      
      showNotification("❌ Error al procesar el KMZ", type = "error", duration = 3)
    }
  })
  
  # ==========================================================================
  # BOTÓN LIMPIAR
  # ==========================================================================
  observeEvent(input$limpiar, {
    showModal(modalDialog(
      title = div(style = "color: #FFFFFF; font-weight: bold; font-size: 20px;",
                  "⚠️ Confirmar Limpieza de Campos"),
      
      div(style = "padding: 15px;",
          div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #e0d4c3 100%);
                       padding: 20px;
                       border-radius: 10px;
                       border-left: 5px solid #802e25;
                       margin-bottom: 15px;",
              h4(style = "color: #4A2E05; margin: 0 0 15px 0; font-weight: bold;",
                 "¿Está seguro de limpiar todos los campos del formulario?"),
              p(style = "color: #6B3A00; margin: 0; font-size: 14px;",
                "Esta acción borrará toda la información ingresada y no se puede deshacer.")
          ),
          
          div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #ff9d92 100%);
                       padding: 12px;
                       border-radius: 8px;
                       border-left: 4px solid #ff5b49;",
              p(style = "margin: 0; color: #8B4513; font-size: 13px; font-weight: 500;",
                "ℹ️ Se limpiarán también los archivos KMZ cargados.")
          )
      ),
      
      footer = tagList(
        modalButton("❌ Cancelar"),
        actionButton("confirmar_limpiar",
                     " Sí, Limpiar Todo ",
                     class = "btn",
                     style = "background: linear-gradient(135deg, #ff5b49 0%, #802e25 100%);
                              color: white; border: none; font-weight: bold;
                              padding: 10px 25px; border-radius: 6px;")
      ),
      easyClose = FALSE
    ))
  })
  
  observeEvent(input$confirmar_limpiar, {
    removeModal()
    
    
    updateTextInput(session, "nombre_inmueble", value = "")
    updateTextInput(session, "empresa", value = "")
    updateTextInput(session, "alias", value = "")
    updateTextAreaInput(session, "direccion", value = "")
    updateTextInput(session, "municipio", value = "")
    updateSelectizeInput(session, "estado", selected = character(0))
    updateSelectizeInput(session, "pais", selected = "MEX")
    updateDateInput(session, "fecha_escritura", value = NULL)
    updateNumericInput(session, "precio_adquisicion", value = NA)
    updateNumericInput(session, "terreno_m2", value = NA)
    updateNumericInput(session, "precio_terreno_m2", value = NA)
    updateNumericInput(session, "construccion_m2", value = NA)
    updateNumericInput(session, "precio_construccion_m2", value = NA)
    
    tryCatch({
      shinyjs::reset("kmz_upload")
      output$kmz_status <- renderUI({NULL})
      kmz_temporal(NULL)
    }, error = function(e) {})
    
    showNotification(" Campos limpios", type = "message", duration = 2)
  })
  
  # ==========================================================================
  # BOTÓN GUARDAR
  # ==========================================================================
  observeEvent(input$guardar, {
    
    # Validaciones
    if (is.null(input$id_inmueble) || trimws(input$id_inmueble) == "") {
      showNotification("El campo ID es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$nombre_inmueble) || trimws(input$nombre_inmueble) == "") {
      showNotification("El campo INMUEBLE es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$empresa) || trimws(input$empresa) == "") {
      showNotification("El campo EMPRESA es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$alias) || trimws(input$alias) == "") {
      showNotification("El campo ALIAS es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$direccion) || trimws(input$direccion) == "") {
      showNotification("El campo DIRECCIÓN es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$municipio) || trimws(input$municipio) == "") {
      showNotification("El campo MUNICIPIO es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$estado) || length(input$estado) == 0 || trimws(input$estado) == "") {
      showNotification("El campo ESTADO es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$pais) || length(input$pais) == 0 || trimws(input$pais) == "") {
      showNotification("El campo PAÍS es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$fecha_escritura) || is.na(input$fecha_escritura)) {
      showNotification("El campo FECHA ESCRITURA es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$precio_adquisicion) || is.na(input$precio_adquisicion) || input$precio_adquisicion <= 0) {
      showNotification("El campo PRECIO ADQUISICIÓN es obligatorio y debe ser mayor a 0", type = "error", duration = 3)
      return()
    }
    # 
    # if (is.null(input$terreno_m2) || is.na(input$terreno_m2) || input$terreno_m2 <= 0) {
    #   showNotification("El campo TERRENO M² es obligatorio y debe ser mayor a 0", type = "error", duration = 3)
    #   return()
    # }
    # 
    # if (is.null(input$precio_terreno_m2) || is.na(input$precio_terreno_m2) || input$precio_terreno_m2 <= 0) {
    #   showNotification("El campo PRECIO TERRENO M² es obligatorio y debe ser mayor a 0", type = "error", duration = 3)
    #   return()
    # }
    # 
    # if (is.null(input$construccion_m2) || is.na(input$construccion_m2) || input$construccion_m2 <= 0) {
    #   showNotification("El campo CONSTRUCCIÓN M² es obligatorio y debe ser mayor a 0", type = "error", duration = 3)
    #   return()
    # }
    # 
    # if (is.null(input$precio_construccion_m2) || is.na(input$precio_construccion_m2) || input$precio_construccion_m2 <= 0) {
    #   showNotification("El campo PRECIO CONSTRUCCIÓN M² es obligatorio y debe ser mayor a 0", type = "error", duration = 3)
    #   return()
    # }
    
    # Crear objeto
    nuevo_inmueble <- list(
      ID = trimws(input$id_inmueble),
      INMUEBLE = trimws(input$nombre_inmueble),
      EMPRESA = trimws(input$empresa),
      ALIAS = trimws(input$alias),
      DIRECCION = trimws(input$direccion),
      MUNICIPIO = trimws(input$municipio),
      ESTADO = trimws(input$estado),
      PAIS = trimws(input$pais),
      FECHA_ESCRITURA = as.character(input$fecha_escritura),
      PRECIO_ADQUISICION = input$precio_adquisicion,
      TERRENO_M2 = input$terreno_m2,
      PRECIO_TERRENO_M2 = input$precio_terreno_m2,
      CONSTRUCCION_M2 = input$construccion_m2,
      PRECIO_CONSTRUCCION_M2 = input$precio_construccion_m2
    )
    
    session$userData$inmueble_temporal <- nuevo_inmueble
    
    # Modal de confirmación
    info_item <- function(label, value) {
      div(style = "margin-bottom: 6px;",
          span(style = "font-size: 13px; color: #6c757d;", paste0(label, ": ")),
          span(style = "font-size: 14px; font-weight: 500; color: #212529;", value))
    }
    
    showModal(modalDialog(
      title = div(style = "font-size: 20px; font-weight: 600; color: #802e25;",
                  "Confirmación de Registro de Inmueble"),
      
      div(style = "padding: 20px;",
          
          div(style = "background-color: #e0d4c3; padding: 15px; border-radius: 8px;
                       border-left: 5px solid #802e25; margin-bottom: 25px;",
              h4(style = "margin: 0 0 8px 0; font-size: 17px; font-weight: 600;",
                 "¿Desea cargar este inmueble en la Base de Datos?"),
              p(style = "margin: 0; font-size: 14px; color: #555;",
                "Revise cuidadosamente la información antes de confirmar.")
          ),
          
          # INFORMACIÓN BÁSICA
          div(style = "margin-bottom: 25px;",
              h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                          border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                 "INFORMACIÓN BÁSICA"),
              fluidRow(
                column(6, info_item("ID", nuevo_inmueble$ID)),
                column(6, info_item("Inmueble", nuevo_inmueble$INMUEBLE))
              ),
              fluidRow(
                column(6, info_item("Empresa", nuevo_inmueble$EMPRESA)),
                column(6, info_item("Alias", nuevo_inmueble$ALIAS))
              )
          ),
          
          # UBICACIÓN
          div(style = "margin-bottom: 25px;",
              h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                          border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                 "UBICACIÓN"),
              info_item("Dirección", nuevo_inmueble$DIRECCION),
              fluidRow(
                column(4, info_item("Municipio", nuevo_inmueble$MUNICIPIO)),
                column(4, info_item("Estado", nuevo_inmueble$ESTADO)),
                column(4, info_item("País", nuevo_inmueble$PAIS))
              )
          ),
          
          # INFORMACIÓN FINANCIERA
          div(style = "margin-bottom: 25px;",
              h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                          border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                 "INFORMACIÓN FINANCIERA"),
              fluidRow(
                column(6, info_item("Fecha de Escritura", nuevo_inmueble$FECHA_ESCRITURA)),
                column(6, info_item("Precio de Adquisición",
                                    paste0("$", format(nuevo_inmueble$PRECIO_ADQUISICION, big.mark = ","))))
              )
          ),
          
          # MEDIDAS
          div(style = "margin-bottom: 25px;",
              h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                          border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                 "MEDIDAS"),
              fluidRow(
                column(6,
                       info_item("Terreno (m²)", format(nuevo_inmueble$TERRENO_M2, big.mark = ",")),
                       info_item("Precio por m² Terreno",
                                 paste0("$", format(nuevo_inmueble$PRECIO_TERRENO_M2, big.mark = ",")))
                ),
                column(6,
                       info_item("Construcción (m²)", format(nuevo_inmueble$CONSTRUCCION_M2, big.mark = ",")),
                       info_item("Precio por m² Construcción",
                                 paste0("$", format(nuevo_inmueble$PRECIO_CONSTRUCCION_M2, big.mark = ",")))
                )
              )
          ),
          
          # ESTADO KMZ
          div(style = "background-color: #F8F9FA; padding: 12px; border-radius: 6px;
                       border-left: 4px solid #6C757D; font-size: 14px;",
              if (!is.null(kmz_temporal())) {
                strong("✅ Archivo KMZ incluido")
              } else {
                strong("ℹ️ No se adjuntó archivo KMZ")
              }
          )
      ),
      
      footer = tagList(
        modalButton("❌ Cancelar"),
        actionButton("confirmar_guardar_snowflake",
                     " Guardar Inmueble ",
                     class = "btn btn-success")
      ),
      size = "l",
      easyClose = FALSE
    ))
  })
  
  # ==========================================================================
  # CONFIRMAR GUARDADO
  # ==========================================================================
  observeEvent(input$confirmar_guardar_snowflake, {
    
    removeModal()
    
    withProgress(message = ' Guardando en Snowflake...', value = 0, {
      
      inmueble <- session$userData$inmueble_temporal
      
      if (is.null(inmueble)) {
        showNotification("❌ Error: No hay datos del inmueble", type = "error", duration = 3)
        return()
      }
      
      setProgress(0.3, detail = "Procesando datos...")
      
      poligono_kmz <- kmz_temporal()
      
      setProgress(0.5, detail = "Conectando a Snowflake...")
      
      resultado <- guardar_inmueble_snowflake(inmueble, poligono_kmz)
      
      setProgress(0.8, detail = "Finalizando...")
      
      if (resultado$exito) {
        
        setProgress(0.9, detail = "Actualizando vista...")
        
        cargar_inmuebles_snowflake()
        
        setProgress(1, detail = "¡Completado!")
        
        debe_generar_nuevo_id(TRUE)
        
        # Limpiar formulario
        
        updateTextInput(session, "nombre_inmueble", value = "")
        updateTextInput(session, "empresa", value = "")
        updateTextInput(session, "alias", value = "")
        updateTextAreaInput(session, "direccion", value = "")
        updateTextInput(session, "municipio", value = "")
        updateSelectizeInput(session, "estado", selected = character(0))
        updateSelectizeInput(session, "pais", selected = "MEX")
        updateDateInput(session, "fecha_escritura", value = NULL)
        updateNumericInput(session, "precio_adquisicion", value = NA)
        updateNumericInput(session, "terreno_m2", value = NA)
        updateNumericInput(session, "precio_terreno_m2", value = NA)
        updateNumericInput(session, "construccion_m2", value = NA)
        updateNumericInput(session, "precio_construccion_m2", value = NA)
        
        tryCatch({
          shinyjs::reset("kmz_upload")
          output$kmz_status <- renderUI({NULL})
          kmz_temporal(NULL)
        }, error = function(e) {})
        
        session$userData$inmueble_temporal <- NULL
        
        if (resultado$tipo == "actualizado") {
          showNotification("✅ ¡Inmueble actualizado exitosamente en Snowflake!",
                           type = "message", duration = 2)
        } else {
          showNotification("✅ ¡Inmueble guardado exitosamente en Snowflake!",
                           type = "message", duration = 2)
        }
        
      } else {
        showNotification(paste("❌ Error al guardar:", resultado$mensaje),
                         type = "error", duration = 3)
      }
    })
  })
  
  # ==========================================================================
  # TABLA MAESTRO
  # ==========================================================================
  output$tabla_maestro_inmuebles_registro <- DT::renderDataTable({
    datos <- inmuebles_snowflake_df()
    
    if (is.null(datos) || nrow(datos) == 0) {
      return(data.frame(Mensaje = "No hay datos disponibles en Snowflake"))
    }
    
    columnas_mostrar <- c("ID", "INMUEBLE", "EMPRESA", "ALIAS", "DIRECCION",
                          "MUNICIPIO", "ESTADO", "PAIS", "FECHA_ESCRITURA",
                          "PRECIO_ADQUISICION", "TERRENO_M2", "PRECIO_TERRENO_M2",
                          "CONSTRUCCION_M2", "PRECIO_CONSTRUCCION_M2",
                          "FECHA_ACTUALIZACION", "STATUS_ACTIVIDAD")
    
    datos_tabla <- datos[, columnas_mostrar]
    
    # Conversión segura
    convertir_numerico_seguro <- function(x) {
      x_num <- suppressWarnings(as.numeric(x))
      x_num[is.na(x_num)] <- 0
      return(x_num)
    }
    
    # Formatear valores vacíos
    for(col in names(datos_tabla)) {
      if(is.character(datos_tabla[[col]])) {
        datos_tabla[[col]][is.na(datos_tabla[[col]]) | datos_tabla[[col]] == ""] <- "-"
      }
    }
    
    # Formatear precios
    if("PRECIO_ADQUISICION" %in% names(datos_tabla)) {
      precio_num <- convertir_numerico_seguro(datos_tabla$PRECIO_ADQUISICION)
      datos_tabla$PRECIO_ADQUISICION <- ifelse(
        precio_num == 0,
        "-",
        paste0("$", format(round(precio_num, 0), big.mark = ","))
      )
    }
    
    if("TERRENO_M2" %in% names(datos_tabla)) {
      terreno_num <- convertir_numerico_seguro(datos_tabla$TERRENO_M2)
      datos_tabla$TERRENO_M2 <- ifelse(
        terreno_num == 0,
        "-",
        paste0(format(round(terreno_num, 2), big.mark = ","), " m²")
      )
    }
    
    if("PRECIO_TERRENO_M2" %in% names(datos_tabla)) {
      precio_terreno_num <- convertir_numerico_seguro(datos_tabla$PRECIO_TERRENO_M2)
      datos_tabla$PRECIO_TERRENO_M2 <- ifelse(
        precio_terreno_num == 0,
        "-",
        paste0("$", format(round(precio_terreno_num, 0), big.mark = ","))
      )
    }
    
    if("CONSTRUCCION_M2" %in% names(datos_tabla)) {
      construccion_num <- convertir_numerico_seguro(datos_tabla$CONSTRUCCION_M2)
      datos_tabla$CONSTRUCCION_M2 <- ifelse(
        construccion_num == 0,
        "-",
        paste0(format(round(construccion_num, 2), big.mark = ","), " m²")
      )
    }
    
    if("PRECIO_CONSTRUCCION_M2" %in% names(datos_tabla)) {
      precio_const_num <- convertir_numerico_seguro(datos_tabla$PRECIO_CONSTRUCCION_M2)
      datos_tabla$PRECIO_CONSTRUCCION_M2 <- ifelse(
        precio_const_num == 0,
        "-",
        paste0("$", format(round(precio_const_num, 0), big.mark = ","))
      )
    }
    
    # Formatear fechas
    if("FECHA_ESCRITURA" %in% names(datos_tabla)) {
      datos_tabla$FECHA_ESCRITURA <- as.character(datos_tabla$FECHA_ESCRITURA)
      datos_tabla$FECHA_ESCRITURA[is.na(datos_tabla$FECHA_ESCRITURA) |
                                    datos_tabla$FECHA_ESCRITURA == ""] <- "-"
    }
    
    if("FECHA_ACTUALIZACION" %in% names(datos_tabla)) {
      datos_tabla$FECHA_ACTUALIZACION <- sapply(datos_tabla$FECHA_ACTUALIZACION, function(fecha) {
        if (is.na(fecha) || length(fecha) == 0) return("-")
        
        fecha_char <- as.character(fecha)
        if (fecha_char == "" || fecha_char == "-") return("-")
        
        tryCatch({
          if (is.numeric(fecha) || grepl("^[0-9]+$", fecha_char)) {
            fecha_num <- as.numeric(fecha_char)
            if (fecha_num > 1e12) {
              fecha_utc <- as.POSIXct(fecha_num / 1000, origin = "1970-01-01", tz = "UTC")
            } else {
              fecha_utc <- as.POSIXct(fecha_num, origin = "1970-01-01", tz = "UTC")
            }
            return(format(fecha_utc, "%Y-%m-%d %H:%M:%S"))
          }
          
          fecha_limpia <- gsub("T", " ", fecha_char)
          fecha_limpia <- gsub("Z", "", fecha_limpia)
          fecha_limpia <- gsub("\\..*", "", fecha_limpia)
          
          fecha_obj <- as.POSIXct(fecha_limpia, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
          
          if (is.na(fecha_obj)) {
            fecha_obj <- as.POSIXct(fecha_limpia, format = "%Y-%m-%d", tz = "UTC")
          }
          
          if (is.na(fecha_obj)) return(fecha_char)
          
          return(format(fecha_obj, "%Y-%m-%d %H:%M:%S"))
          
        }, error = function(e) {
          return(fecha_char)
        })
      })
    }
    
    # Columna de actividad
    datos_tabla$Actividad <- sapply(1:nrow(datos_tabla), function(i) {
      id_actual <- datos$ID[i]
      
      if("STATUS_ACTIVIDAD" %in% names(datos_tabla)) {
        status_act <- datos_tabla$STATUS_ACTIVIDAD[i]
      } else {
        status_act <- NA
      }
      
      if(is.na(status_act) || status_act == "" || toupper(trimws(status_act)) == "ACTIVO") {
        paste0(
          '<button class="btn btn-success btn-sm" ',
          'onclick="Shiny.setInputValue(\'cambiar_a_inactivo\', \'',
          id_actual, '_', runif(1), '\', {priority: \'event\'});" ',
          'style="background: linear-gradient(135deg, #802e25 0%, #401712 100%); ',
          'border: none; color: white; padding: 5px 12px; border-radius: 5px; ',
          'font-weight: bold; font-size: 12px;">',
          '🟢 Activo</button>'
        )
      } else {
        '<span style="color: #802e25; font-weight: bold;">🔴 Inactivo</span>'
      }
    })
    
    # Columna de acciones
    datos_tabla$Acciones <- sapply(1:nrow(datos_tabla), function(i) {
      id_actual <- datos$ID[i]
      paste0(
        '<button class="btn btn-primary btn-sm" ',
        'onclick="Shiny.setInputValue(\'editar_inmueble_snowflake\', \'',
        id_actual, '_', runif(1), '\', {priority: \'event\'});" ',
        'style="background: linear-gradient(135deg, #3498db 0%, #2980b9 100%); ',
        'border: none; color: white; padding: 5px 12px; border-radius: 5px; ',
        'font-weight: bold; font-size: 12px;">',
        '<i class="fa fa-edit"></i> Editar</button>'
      )
    })
    
    # Renombrar
    names(datos_tabla) <- c(
      "ID", "Inmueble", "Empresa", "Alias", "Dirección",
      "Municipio", "Estado", "País", "Fecha Escritura",
      "Precio Adquisición", "Terreno (m²)", "Precio/m² Terreno",
      "Construcción (m²)", "Precio/m² Construcción", "Fecha Actualización",
      "STATUS_ACTIVIDAD_ORIGINAL",
      "Actividad",
      "Acciones"
    )
    
    datos_tabla$STATUS_ACTIVIDAD_ORIGINAL <- NULL
    
    DT::datatable(
      datos_tabla,
      escape = FALSE,
      options = list(
        paging      = TRUE,
        pageLength  = 50,
        lengthMenu  = list(c(50, 100, 150, -1), c("50", "100", "150", "Todos")),
        scrollX     = TRUE,
        scrollY     = "55vh",
        scrollCollapse = TRUE,
        searching   = TRUE,
        ordering    = TRUE,
        dom = '<"top"lf>rt<"bottom"ip>',
        
        columnDefs = list(
          list(targets = 0, width = "100px"),
          list(targets = c(1, 2, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13), visible = TRUE),
          list(targets = 3, visible = FALSE),
          list(targets = 14, visible = FALSE),
          list(targets = 15, visible = FALSE),
          list(targets = 16, width = "120px", className = "text-center", orderable = FALSE)
        ),
        
        language = list(
          lengthMenu  = "Mostrar _MENU_ registros por página",
          zeroRecords = "No se encontraron registros",
          info        = "Mostrando _START_ a _END_ de _TOTAL_ registros activos",
          search      = "Buscar:"
        )
      ),
      rownames = FALSE,
      class = 'table table-striped table-hover'
    )
  })
  
  # ==========================================================================
  # BOTÓN ACTUALIZAR TABLA
  # ==========================================================================
  observeEvent(input$actualizar_tabla_registro, {
    showNotification("🔄 Actualizando datos desde Snowflake...",
                     type = "message", duration = 2)
    
    resultado <- cargar_inmuebles_snowflake()
    
    if(resultado) {
      showNotification("✅ Datos actualizados exitosamente",
                       type = "message", duration = 3)
    } else {
      showNotification("⚠️ Error al actualizar los datos",
                       type = "warning", duration = 3)
    }
  })
  
  # ==========================================================================
  # EDITAR INMUEBLE
  # ==========================================================================
  observeEvent(input$editar_inmueble_snowflake, {
    
    id_editar <- gsub("_[0-9\\.]+$", "", input$editar_inmueble_snowflake)
    
    if (!is.null(id_editar) && id_editar != "") {
      
      datos_sf <- inmuebles_snowflake_df()
      inmueble <- datos_sf[datos_sf$ID == id_editar, ]
      
      if (nrow(inmueble) > 0) {
        
        showModal(modalDialog(
          title = div(style = "font-size: 20px; font-weight: 600;  color: #FFFFFF;",
                      "Editar Inmueble"),
          size = "l",
          
          div(style = "padding: 20px;",
              
              div(style = "background-color: #e0d4c3; padding: 15px; border-radius: 8px;
                           border-left: 5px solid #802e25; margin-bottom: 25px;",
                  h4(style = "margin: 0 0 8px 0; font-size: 17px; font-weight: 600;",
                     "¿Desea modificar este inmueble en la Base de Datos?"),
                  p(style = "margin: 0; font-size: 14px; color: #555;",
                    "Revise cuidadosamente la información antes de confirmar.")
              ),
              
              # INFORMACIÓN BÁSICA
              div(style = "margin-bottom: 25px;",
                  h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                              border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                     "INFORMACIÓN BÁSICA"),
                  
                  fluidRow(
                    column(6, textInput("edit_sf_id",
                                        HTML("ID: <span class='required-field'>*</span>"),
                                        value = inmueble$ID)),
                    column(6, textInput("edit_sf_inmueble",
                                        HTML("Inmueble: <span class='required-field'>*</span>"),
                                        value = if(inmueble$INMUEBLE == "-") "" else inmueble$INMUEBLE))
                  ),
                  fluidRow(
                    column(6, selectizeInput("edit_sf_empresa",
                                             HTML("Empresa: <span class='required-field'>*</span>"),
                                             choices = NULL,
                                             selected = if(inmueble$EMPRESA == "-") "" else inmueble$EMPRESA,
                                             options = list(create = TRUE, maxItems = 1))),
                    column(6, textInput("edit_sf_alias",
                                        HTML("Alias: <span class='required-field'>*</span>"),
                                        value = if(inmueble$ALIAS == "-") "" else inmueble$ALIAS))
                  )
              ),
              
              # UBICACIÓN
              div(style = "margin-bottom: 25px;",
                  h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                              border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                     "UBICACIÓN"),
                  
                  textAreaInput("edit_sf_direccion",
                                HTML("Dirección: <span class='required-field'>*</span>"),
                                value = if(inmueble$DIRECCION == "-") "" else inmueble$DIRECCION,
                                rows = 2),
                  fluidRow(
                    column(4, textInput("edit_sf_municipio",
                                        HTML("Municipio: <span class='required-field'>*</span>"),
                                        value = if(inmueble$MUNICIPIO == "-") "" else inmueble$MUNICIPIO)),
                    column(4, selectizeInput("edit_sf_estado",
                                             HTML("Estado: <span class='required-field'>*</span>"),
                                             choices = c(
                                               # --- MÉXICO ---
                                               "Aguascalientes" = "AGS", "Baja California" = "BC", "Baja California Sur" = "BCS",
                                               "Campeche" = "CAMP", "Chiapas" = "CHIS", "Chihuahua" = "CHIH", "Ciudad de México" = "CDMX",
                                               "Coahuila" = "COAH", "Colima" = "COL", "Durango" = "DGO", "Guanajuato" = "GTO",
                                               "Guerrero" = "GRO", "Hidalgo" = "HGO", "Jalisco" = "JAL", "Estado de México" = "MEX",
                                               "Michoacán" = "MICH", "Morelos" = "MOR", "Nayarit" = "NAY", "Querétaro" = "NL",
                                               "Oaxaca" = "OAX", "Puebla" = "PUE", "Querétaro" = "QRO", "Quintana Roo" = "QROO",
                                               "San Luis Potosí" = "SLP", "Sinaloa" = "SIN", "Sonora" = "SON", "Tabasco" = "TAB",
                                               "Tamaulipas" = "TAMPS", "Tlaxcala" = "TLAX", "Veracruz" = "VER", "Yucatán" = "YUC",
                                               "Zacatecas" = "ZAC",
                                               # --- USA ---
                                               "Texas" = "TEX", "California" = "CA", "Arizona" = "AZ", "New Mexico" = "NM", 
                                               "Florida" = "FL", "Louisiana" = "LA", "Alabama" = "AL", "Mississippi" = "MS",
                                               "Georgia" = "GA", "Nevada" = "NV", "Utah" = "UT", "Colorado" = "CO", 
                                               "Oklahoma" = "OK", "Arkansas" = "AR", "Tennessee" = "TN", "North Carolina" = "NC",
                                               "South Carolina" = "SC", "Virginia" = "VA", "Maryland" = "MD", "Kansas" = "KS",
                                               "Missouri" = "MO", "Illinois" = "IL", "New York" = "NY", "Oregon" = "OR", "Washington" = "WA",
                                               # --- ESPAÑA ---
                                               "España" = "ESP"
                                             ),
                                             selected = if(inmueble$ESTADO == "-") NULL else inmueble$ESTADO,
                                             options = list(
                                               create = FALSE, 
                                               maxItems = 1
                                             ))
                           ),
                    column(4, selectizeInput("edit_sf_pais",
                                             HTML("País: <span class='required-field'>*</span>"),
                                             choices = c("México" = "MEX", "España" = "ESP", "Estados Unidos" = "EU"),
                                             selected = if(inmueble$PAIS == "-") "MEX" else inmueble$PAIS,
                                             options = list(create = TRUE, maxItems = 1)))
                  )
              ),
              
              # INFORMACIÓN FINANCIERA
              div(style = "margin-bottom: 25px;",
                  h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                              border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                     "INFORMACIÓN FINANCIERA"),
                  
                  fluidRow(
                    column(6, dateInput("edit_sf_fecha_escritura",
                                        HTML("Fecha de Escritura: <span class='required-field'>*</span>"),
                                        value = if(is.na(inmueble$FECHA_ESCRITURA)) NULL else as.Date(inmueble$FECHA_ESCRITURA),
                                        format = "yyyy-mm-dd")),
                    column(6, numericInput("edit_sf_precio_adquisicion",
                                           HTML("Precio de Adquisición ($): <span class='required-field'>*</span>"),
                                           value = if(is.na(inmueble$PRECIO_ADQUISICION) || inmueble$PRECIO_ADQUISICION == 0) NA else as.numeric(inmueble$PRECIO_ADQUISICION),
                                           min = 0, step = 0.01))
                  )
              ),
              
              # MEDIDAS
              div(style = "margin-bottom: 25px;",
                  h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                              border-bottom: 2px solid #ff5b49; padding-bottom: 6px;",
                     "MEDIDAS"),
                  
                  fluidRow(
                    column(6,
                           numericInput("edit_sf_terreno_m2",
                                        "Terreno (m²):",
                                        value = if(is.na(inmueble$TERRENO_M2) || inmueble$TERRENO_M2 == 0) NA else as.numeric(inmueble$TERRENO_M2),
                                        min = 0, step = 0.01),
                           numericInput("edit_sf_precio_terreno_m2",
                                        "Precio por m² Terreno ($):",
                                        value = if(is.na(inmueble$PRECIO_TERRENO_M2) || inmueble$PRECIO_TERRENO_M2 == 0) NA else as.numeric(inmueble$PRECIO_TERRENO_M2),
                                        min = 0, step = 0.01)
                    ),
                    column(6,
                           numericInput("edit_sf_construccion_m2",
                                        "Construcción (m²):",
                                        value = if(is.na(inmueble$CONSTRUCCION_M2) || inmueble$CONSTRUCCION_M2 == 0) NA else as.numeric(inmueble$CONSTRUCCION_M2),
                                        min = 0, step = 0.01),
                           numericInput("edit_sf_precio_construccion_m2",
                                        "Precio por m² Construcción ($):",
                                        value = if(is.na(inmueble$PRECIO_CONSTRUCCION_M2) || inmueble$PRECIO_CONSTRUCCION_M2 == 0) NA else as.numeric(inmueble$PRECIO_CONSTRUCCION_M2),
                                        min = 0, step = 0.01)
                    )
                  )
              ),
              
              # STATUS DE ACTIVIDAD
              div(style = "margin-bottom: 25px;",
                  h5(style = "font-size: 16px; font-weight: 600; color: #802e25;
                              border-bottom: 2px solid #802e25; padding-bottom: 6px;",
                     "⚠️ STATUS DE ACTIVIDAD"),
                  
                  div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #ff9d92 100%);
                               padding: 20px; border-radius: 8px; border-left: 5px solid #802e25;
                               margin-top: 15px;",
                      
                      radioButtons("edit_sf_motivo_inactivo",
                                   label = div(style = "font-size: 15px; font-weight: bold; color: #6B3A00;",
                                               "🔴 Cambiar status del inmueble:"),
                                   choices = list(
                                     "Mantener Activo"  = "activo",
                                     "Vendido"          = "Vendido",
                                     "Dado de Baja"     = "Dado de Baja"
                                   ),
                                   selected = "activo"),
                      
                      div(style = "background: #e0d4c3; padding: 12px; border-radius: 6px;
             border-left: 4px solid #726f6b; margin-top: 10px;",
                          p(style = "margin: 0; color: #856404; font-size: 13px;",
                            "⚠️ Si selecciona Vendido o Dado de Baja, el inmueble NO aparecerá más en el sistema.")
                      )
                  )
              )
          ),
          
          footer = tagList(
            tags$button(type = "button", class = "btn", `data-dismiss` = "modal",
                        style = "background-color: #802e25; color: white; font-weight: 600;
                                 padding: 10px 25px; border-radius: 6px; border: none;",
                        "✕ Cancelar"),
            actionButton("confirmar_editar_sf",
                         "✔ Guardar Cambios",
                         class = "btn",
                         style = "background-color: #802e25; color: white; font-weight: 600;
                                  padding: 10px 25px; border-radius: 6px; border: none;")
          ),
          
          easyClose = FALSE
        ))
        
        session$userData$id_sf_original <- id_editar
        
        # Actualizar opciones de empresa
        shinyjs::delay(100, {
          datos_all <- inmuebles_snowflake_df()
          if (!is.null(datos_all) && nrow(datos_all) > 0) {
            empresas_unicas <- sort(unique(datos_all$EMPRESA[!is.na(datos_all$EMPRESA) &
                                                               datos_all$EMPRESA != "" &
                                                               datos_all$EMPRESA != "-"]))
            updateSelectizeInput(session, "edit_sf_empresa",
                                 choices = c("", empresas_unicas),
                                 selected = if(inmueble$EMPRESA == "-") "" else inmueble$EMPRESA,
                                 server = TRUE)
          }
        })
      }
    }
  })
  
  # ==========================================================================
  # CONFIRMAR EDICIÓN
  # ==========================================================================
  observeEvent(input$confirmar_editar_sf, {
    
    id_original <- session$userData$id_sf_original
    
    # VALIDACIONES
    if (is.null(input$edit_sf_id) || trimws(input$edit_sf_id) == "") {
      showNotification("El campo ID es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_inmueble) || trimws(input$edit_sf_inmueble) == "") {
      showNotification("El campo INMUEBLE es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_empresa) || trimws(input$edit_sf_empresa) == "") {
      showNotification("El campo EMPRESA es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_alias) || trimws(input$edit_sf_alias) == "") {
      showNotification("El campo ALIAS es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_direccion) || trimws(input$edit_sf_direccion) == "") {
      showNotification("El campo DIRECCIÓN es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_municipio) || trimws(input$edit_sf_municipio) == "") {
      showNotification("El campo MUNICIPIO es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_estado) || length(input$edit_sf_estado) == 0) {
      showNotification("El campo ESTADO es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_pais) || length(input$edit_sf_pais) == 0) {
      showNotification("El campo PAÍS es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_fecha_escritura) || is.na(input$edit_sf_fecha_escritura)) {
      showNotification("El campo FECHA ESCRITURA es obligatorio", type = "error", duration = 3)
      return()
    }
    
    if (is.null(input$edit_sf_precio_adquisicion) || is.na(input$edit_sf_precio_adquisicion) || input$edit_sf_precio_adquisicion <= 0) {
      showNotification("El campo PRECIO ADQUISICIÓN es obligatorio y debe ser mayor a 0", type = "error", duration = 3)
      return()
    }
    
    # ============================================================================
    # 🆕 VERIFICAR SI SE MARCÓ COMO INACTIVO
    # ============================================================================
    motivo_seleccionado <- input$edit_sf_motivo_inactivo
    marcar_inactivo <- !is.null(motivo_seleccionado) && motivo_seleccionado != "activo"
    
    if (marcar_inactivo) {
      removeModal()
      
      showModal(modalDialog(
        title = div(style = "color: #FFFFFF; font-weight: bold; font-size: 20px;",
                    "⚠️ CONFIRMAR CAMBIO DE STATUS"),
        
        div(style = "padding: 20px;",
            div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #ff9d92 100%);
                   padding: 20px; border-radius: 10px; border-left: 5px solid #802e25;
                   margin-bottom: 20px;",
                h4(style = "color: #6B3A00; margin: 0 0 15px 0; font-weight: bold;",
                   paste0("¿Está seguro de marcar este inmueble como ", toupper(motivo_seleccionado), "?")),
                
                p(style = "margin: 0 0 10px 0; color: #8B4513; font-size: 14px;",
                  paste0(" ID del Inmueble: ", input$edit_sf_id)),
                
                p(style = "margin: 0; color: #8B4513; font-size: 14px;",
                  paste0(" Nombre: ", input$edit_sf_inmueble))
            ),
            
            div(style = "background: #e0d4c3; padding: 15px; border-radius: 8px;
                   border-left: 4px solid #726f6b; margin-bottom: 15px;",
                p(style = "margin: 0; color: #856404; font-size: 14px; font-weight: 500;",
                  "⚠️ Al confirmar, este inmueble NO aparecerá más en el sistema")
            ),
            
            div(style = "background: #d1ecf1; padding: 12px; border-radius: 6px;
                   border-left: 4px solid #0c5460;",
                p(style = "margin: 0; color: #0c5460; font-size: 13px;",
                  paste0("ℹ️ El inmueble quedará INACTIVO con motivo: ", motivo_seleccionado))
            )
        ),
        
        footer = tagList(
          actionButton("cancelar_inactivo_edicion",
                       " Cancelar y Volver",
                       class = "btn",
                       style = "background: #95a5a6; color: white; border: none;
                          padding: 10px 25px; border-radius: 6px; font-weight: bold;"),
          actionButton("confirmar_guardar_inactivo",
                       paste0(" Sí, Guardar como ", motivo_seleccionado),
                       class = "btn",
                       style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                          color: white; border: none; font-weight: bold;
                          padding: 10px 25px; border-radius: 6px;")
        ),
        easyClose = FALSE,
        size = "m"
      ))
      
      session$userData$datos_edicion_temporal <- list(
        ID = trimws(input$edit_sf_id),
        INMUEBLE = trimws(input$edit_sf_inmueble),
        EMPRESA = trimws(input$edit_sf_empresa),
        ALIAS = trimws(input$edit_sf_alias),
        DIRECCION = trimws(input$edit_sf_direccion),
        MUNICIPIO = trimws(input$edit_sf_municipio),
        ESTADO = trimws(input$edit_sf_estado),
        PAIS = trimws(input$edit_sf_pais),
        FECHA_ESCRITURA = as.character(input$edit_sf_fecha_escritura),
        PRECIO_ADQUISICION = input$edit_sf_precio_adquisicion,
        TERRENO_M2 = input$edit_sf_terreno_m2,
        PRECIO_TERRENO_M2 = input$edit_sf_precio_terreno_m2,
        CONSTRUCCION_M2 = input$edit_sf_construccion_m2,
        PRECIO_CONSTRUCCION_M2 = input$edit_sf_precio_construccion_m2,
        MARCAR_INACTIVO = TRUE,
        STATUS_INMUEBLE = motivo_seleccionado
      )
      
    } else {
      # SI NO SE MARCÓ INACTIVO: Guardar normalmente
      removeModal()
      
      withProgress(message = ' Actualizando en Snowflake...', value = 0, {
        
        setProgress(0.3, detail = "Preparando datos...")
        
        datos_actualizados <- list(
          ID = trimws(input$edit_sf_id),
          INMUEBLE = trimws(input$edit_sf_inmueble),
          EMPRESA = trimws(input$edit_sf_empresa),
          ALIAS = trimws(input$edit_sf_alias),
          DIRECCION = trimws(input$edit_sf_direccion),
          MUNICIPIO = trimws(input$edit_sf_municipio),
          ESTADO = trimws(input$edit_sf_estado),
          PAIS = trimws(input$edit_sf_pais),
          FECHA_ESCRITURA = as.character(input$edit_sf_fecha_escritura),
          PRECIO_ADQUISICION = input$edit_sf_precio_adquisicion,
          TERRENO_M2 = input$edit_sf_terreno_m2,
          PRECIO_TERRENO_M2 = input$edit_sf_precio_terreno_m2,
          CONSTRUCCION_M2 = input$edit_sf_construccion_m2,
          PRECIO_CONSTRUCCION_M2 = input$edit_sf_precio_construccion_m2
        )
        
        setProgress(0.5, detail = "Conectando a Snowflake...")
        
        resultado <- guardar_inmueble_snowflake(datos_actualizados, poligono_kmz = NULL)
        
        setProgress(0.8, detail = "Finalizando...")
        
        if (resultado$exito) {
          setProgress(0.9, detail = "Recargando datos...")
          cargar_inmuebles_snowflake()
          setProgress(1, detail = "¡Completado!")
          
          showNotification("✅ ¡Inmueble actualizado exitosamente en Snowflake!", 
                           type = "message", duration = 4)
          session$userData$id_sf_original <- NULL
        } else {
          showNotification(paste("❌ Error al actualizar:", resultado$mensaje), 
                           type = "error", duration = 5)
        }
      })
    }
  })
  
  # ==========================================================================
  # CANCELAR CAMBIO A INACTIVO
  # ==========================================================================
  observeEvent(input$cancelar_inactivo_edicion, {
    removeModal()
    showNotification("ℹ️ Operación cancelada.", type = "warning", duration = 2)
    session$userData$datos_edicion_temporal <- NULL
  })
  
  # ==========================================================================
  # CONFIRMAR GUARDAR INACTIVO
  # ==========================================================================
  observeEvent(input$confirmar_guardar_inactivo, {
    
    datos_temp <- session$userData$datos_edicion_temporal
    id_original <- session$userData$id_sf_original
    
    if (is.null(datos_temp)) {
      showNotification("❌ Error: No hay datos para guardar", type = "error", duration = 3)
      removeModal()
      return()
    }
    
    removeModal()
    
    withProgress(message = ' Guardando cambios y marcando como INACTIVO...', value = 0, {
      
      setProgress(0.2, detail = "Preparando datos...")
      
      datos_actualizados <- list(
        ID = datos_temp$ID,
        INMUEBLE = datos_temp$INMUEBLE,
        EMPRESA = datos_temp$EMPRESA,
        ALIAS = datos_temp$ALIAS,
        DIRECCION = datos_temp$DIRECCION,
        MUNICIPIO = datos_temp$MUNICIPIO,
        ESTADO = datos_temp$ESTADO,
        PAIS = datos_temp$PAIS,
        FECHA_ESCRITURA = datos_temp$FECHA_ESCRITURA,
        PRECIO_ADQUISICION = datos_temp$PRECIO_ADQUISICION,
        TERRENO_M2 = datos_temp$TERRENO_M2,
        PRECIO_TERRENO_M2 = datos_temp$PRECIO_TERRENO_M2,
        CONSTRUCCION_M2 = datos_temp$CONSTRUCCION_M2,
        PRECIO_CONSTRUCCION_M2 = datos_temp$PRECIO_CONSTRUCCION_M2
      )
      
      setProgress(0.4, detail = "Actualizando datos del inmueble...")
      
      resultado <- guardar_inmueble_snowflake(datos_actualizados, poligono_kmz = NULL)
      
      if (!resultado$exito) {
        showNotification(paste("❌ Error al actualizar inmueble:", resultado$mensaje),
                         type = "error", duration = 5)
        return()
      }
      
      setProgress(0.6, detail = "Cambiando status a INACTIVO...")
      
      resultado_status <- cambiar_status_actividad(
        id_inmueble    = datos_temp$ID,
        nuevo_status   = "Inactivo",
        status_inmueble = datos_temp$STATUS_INMUEBLE,
        usuario        = "SISTEMA"
      )
      
      if (!resultado_status) {
        showNotification("⚠️ Datos actualizados pero error al cambiar status",
                         type = "warning", duration = 3)
        return()
      }
      
      setProgress(0.9, detail = "Recargando datos...")
      
      cargar_inmuebles_snowflake()
      
      setProgress(1, detail = "¡Completado!")
      
      showNotification(
        paste0("✅ Inmueble actualizado y marcado como INACTIVO. Ya no aparecerá en el sistema."),
        type = "message",
        duration = 5
      )
      
      session$userData$datos_edicion_temporal <- NULL
      session$userData$id_sf_original <- NULL
    })
  })
  
  # ==========================================================================
  # CAMBIAR A INACTIVO
  # ==========================================================================
  observeEvent(input$cambiar_a_inactivo, {
    id_inmueble <- gsub("_[0-9\\.]+$", "", input$cambiar_a_inactivo)
    
    if (!is.null(id_inmueble) && id_inmueble != "") {
      
      showModal(modalDialog(
        title = div(style = "color: #802e25; font-weight: bold; font-size: 18px;",
                    "⚠️ Cambiar Status de Inmueble"),
        
        div(style = "padding: 20px;",
            div(style = "background: linear-gradient(135deg, #e0d4c3 0%, #ff9d92 100%);
                         padding: 20px; border-radius: 10px; border-left: 5px solid #802e25;
                         margin-bottom: 20px;",
                h4(style = "color: #6B3A00; margin: 0 0 15px 0; font-weight: bold;",
                   "¿Quiere cambiar el status del inmueble a INACTIVO?"),
                p(style = "margin: 0; color: #8B4513; font-size: 14px;",
                  paste0(" ID del Inmueble: ", id_inmueble))
            ),
            
            div(style = "background: #e0d4c3; padding: 15px; border-radius: 8px;
                         border-left: 4px solid #726f6b; margin-bottom: 15px;",
                p(style = "margin: 0; color: #856404; font-size: 13px; font-weight: 500;",
                  "⚠️ Al hacerlo este inmueble NO aparecerá más en el maestro de inmuebles")
            ),
            
            div(style = "background: #d1ecf1; padding: 12px; border-radius: 6px;
                         border-left: 4px solid #0c5460;",
                p(style = "margin: 0; color: #0c5460; font-size: 12px;",
                  "ℹ️ El inmueble NO será eliminado de Snowflake, solo cambiará su status de actividad")
            )
        ),
        
        footer = tagList(
          tags$button(type = "button", class = "btn", `data-dismiss` = "modal",
                      style = "background: #95a5a6; color: white; border: none;
                               padding: 10px 25px; border-radius: 6px; font-weight: bold;",
                      "❌ Cancelar"),
          actionButton("confirmar_cambiar_inactivo",
                       "✅ Sí, cambiar status del inmueble",
                       class = "btn",
                       style = "background: linear-gradient(135deg, #802e25 0%, #401712 100%);
                                color: white; border: none; font-weight: bold;
                                padding: 10px 25px; border-radius: 6px;")
        ),
        
        easyClose = FALSE,
        size = "m"
      ))
      
      session$userData$id_cambiar_inactivo <- id_inmueble
    }
  })
  
  # ==========================================================================
  # CONFIRMAR CAMBIO INACTIVO
  # ==========================================================================
  observeEvent(input$confirmar_cambiar_inactivo, {
    id_inmueble <- session$userData$id_cambiar_inactivo
    
    if (!is.null(id_inmueble)) {
      removeModal()
      
      showNotification("🔄 Cambiando status a INACTIVO...",
                       type = "message", duration = 2)
      
      resultado <- cambiar_status_actividad(
        id_inmueble = id_inmueble,
        nuevo_status = "Inactivo",
        usuario = "SISTEMA"
      )
      
      if (resultado) {
        cargar_inmuebles_snowflake()
        
        showNotification(
          paste0("✅ Inmueble ", id_inmueble, " marcado como INACTIVO y removido del maestro"),
          type = "message",
          duration = 4
        )
      } else {
        showNotification("❌ Error al cambiar status", type = "error", duration = 4)
      }
      
      session$userData$id_cambiar_inactivo <- NULL
    }
  })
}

# ============================================================================
# EJECUTAR APP
# ============================================================================
shinyApp(ui = ui, server = server)
