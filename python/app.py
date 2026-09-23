# -*- coding: utf-8 -*-
"""
============================================================================
 MAESTRO DE INMUEBLES  ·  ALMENA
 Réplica en Python (Dash + dash-leaflet) de la PRIMERA PESTAÑA (Mapa)
 del aplicativo original "Maestro de Inmuebles.R".

 Incluye únicamente la pestaña del mapa:
   - Panel lateral de filtros (Empresa / Estado / Municipio / Inmueble / Terreno)
   - 4 fichas KPI reactivas a los filtros
   - Mapa Leaflet con polígonos + marcadores y popups informativos (con INPC)
   - Tabla detallada sincronizada con el mapa
   - Descarga a Excel

 Conexión a Snowflake vía autenticación JWT keypair (mismo usuario/llave/.pem
 que la app en R): SVC_ANALYTICS + SVC_ANALYTICS_key_pk8.pem
============================================================================
"""

import os
import sys
import warnings

# Windows consoles default to cp1252 and choke on the emoji in the log lines.
for _s in (sys.stdout, sys.stderr):
    try:
        _s.reconfigure(encoding="utf-8", errors="replace")
    except Exception:
        pass

import pandas as pd
from cryptography.hazmat.primitives import serialization
from shapely import wkt
from pyproj import Geod

import dash
from dash import Dash, dcc, html, Input, Output, State, ALL, ctx, no_update
from dash.dash_table import DataTable
import dash_leaflet as dl

warnings.filterwarnings("ignore")

# ============================================================================
# CONFIGURACIÓN DE CONEXIÓN A SNOWFLAKE  (idéntica a la app en R)
# ============================================================================
BASE_DIR   = os.path.dirname(os.path.abspath(__file__))
PEM_PATH   = os.environ.get("SNOWFLAKE_PRIVATE_KEY_PATH", "")

SNOW_CFG = dict(
    account   = os.environ.get("SNOWFLAKE_ACCOUNT", ""),       # host: ${SNOWFLAKE_ACCOUNT}.snowflakecomputing.com
    user      = "SVC_ANALYTICS",
    role      = "ROLE_ANALYTICS",
    warehouse = "WH_ANALYTICS",
    database  = "DB_ANALYTICS",
    schema    = "SCH_CORE",
)

# --- Queries (copiadas 1:1 de la app en R) ---------------------------------
QUERY_POLIGONOS = """
    SELECT
      g.ID        AS ID_INMUEBLE,
      i.INMUEBLE  AS INMUEBLE,
      i.EMPRESA,
      i.DIRECCION,
      i.MUNICIPIO,
      i.ESTADO,
      g.GEOMETRIA_WKT,
      g.LATITUD_CENTROIDE  AS LATITUD,
      g.LONGITUD_CENTROIDE AS LONGITUD
    FROM DB_ANALYTICS.SCH_CORE.INMUEBLES_GEOMETRIA g
    JOIN DB_ANALYTICS.SCH_CORE.INMUEBLES i
      ON g.ID = i.ID
    WHERE g.ID IN (
      SELECT ID
      FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
      WHERE UPPER(TRIM(STATUS_ACTIVIDAD)) = 'ACTIVO'
    )
"""

QUERY_INMUEBLES = """
    WITH ULTIMA_ACTIVIDAD AS (
      SELECT ID, STATUS_ACTIVIDAD, FECHA_ASIGNACION, USUARIO AS USUARIO_ACTIVIDAD
      FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
      QUALIFY ROW_NUMBER() OVER (PARTITION BY ID ORDER BY FECHA_ASIGNACION DESC) = 1
    )
    SELECT i.*, a.STATUS_ACTIVIDAD, a.FECHA_ASIGNACION, a.USUARIO_ACTIVIDAD
    FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
    INNER JOIN ULTIMA_ACTIVIDAD a ON i.ID = a.ID
    WHERE UPPER(TRIM(a.STATUS_ACTIVIDAD)) = 'ACTIVO'
"""

QUERY_INPC = """
    SELECT FECHA, VALOR
    FROM DB_ANALYTICS.SCH_CORE.INPC_HISTORICO
    ORDER BY FECHA ASC
"""

# Calificaciones de Google Maps (tabla que llena enriquecer_gmaps.py).
# Si la tabla aún no existe, la app funciona igual (sin estrellas).
QUERY_GMAPS = """
    SELECT ID AS ID_INMUEBLE, RATING, RESENAS
    FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_GMAPS
"""

_GEOD = Geod(ellps="WGS84")


# ============================================================================
# CONEXIÓN
# ============================================================================
def _private_key_der():
    """Carga la llave privada PKCS8 (.pem sin passphrase) y la devuelve en DER."""
    with open(PEM_PATH, "rb") as f:
        p_key = serialization.load_pem_private_key(f.read(), password=None)
    return p_key.private_bytes(
        encoding=serialization.Encoding.DER,
        format=serialization.PrivateFormat.PKCS8,
        encryption_algorithm=serialization.NoEncryption(),
    )


def crear_conexion():
    import snowflake.connector
    return snowflake.connector.connect(private_key=_private_key_der(), **SNOW_CFG)


def _fetch(cur, query):
    cur.execute(query)
    return cur.fetch_pandas_all()


# ============================================================================
# LÓGICA INPC (réplica de obtener_valor_inflacionado)
# ============================================================================
def obtener_valor_inflacionado(precio_original, fecha_adquisicion, inpc):
    """Actualiza un precio por inflación usando la tabla INPC_HISTORICO."""
    try:
        if precio_original is None or pd.isna(precio_original) or float(precio_original) == 0:
            return None
        if fecha_adquisicion is None or pd.isna(fecha_adquisicion):
            return None
        if inpc is None or len(inpc) == 0:
            return None

        fecha = pd.to_datetime(str(fecha_adquisicion)[:10], errors="coerce")
        if pd.isna(fecha):
            return None

        fecha_compra_mes = fecha.to_period("M").to_timestamp()
        posteriores = inpc[inpc["fecha"] >= fecha_compra_mes]
        if len(posteriores) == 0:
            return None

        inpc_compra = float(posteriores["valor"].iloc[0])
        inpc_actual = float(inpc["valor"].iloc[-1])
        if inpc_compra == 0:
            return None

        valor = float(precio_original) * (inpc_actual / inpc_compra)
        return dict(
            valor         = round(valor),
            fecha_inpc    = inpc["fecha"].iloc[-1].strftime("%m/%Y"),
            variacion_pct = round((inpc_actual / inpc_compra - 1) * 100, 1),
            original      = round(float(precio_original)),
        )
    except Exception:
        return None


# ============================================================================
# CARGA + PROCESAMIENTO DE DATOS (réplica de datos_mapa_inmuebles)
# ============================================================================
def _area_m2(geom):
    try:
        a, _ = _GEOD.geometry_area_perimeter(geom)
        return abs(a)
    except Exception:
        return None


def _geom_to_positions(geom):
    """Convierte una geometría shapely a positions [[lat,lon],...] para Leaflet."""
    try:
        if geom.geom_type == "Polygon":
            polys = [geom]
        elif geom.geom_type == "MultiPolygon":
            polys = list(geom.geoms)
        else:
            return None
        parts = []
        for p in polys:
            parts.append([[lat, lon] for lon, lat in p.exterior.coords])
        return parts if parts else None
    except Exception:
        return None


def _datos_demo():
    """Los cuatro DataFrames que devolverian las queries, desde los fixtures.

    Se usa cuando DEMO_MODE=1: misma forma, sin credenciales ni Snowflake.
    """
    import sys as _sys
    _sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    from demo import fixtures as _fx

    inmuebles = _fx.generar_inmuebles()
    activos = [r for r in inmuebles if r["STATUS_ACTIVIDAD"] == "Activo"]

    df_geo = pd.DataFrame([{
        "ID_INMUEBLE": r["ID"], "INMUEBLE": r["INMUEBLE"], "EMPRESA": r["EMPRESA"],
        "DIRECCION": r["DIRECCION"], "MUNICIPIO": r["MUNICIPIO"],
        "ESTADO": r["ESTADO"], "GEOMETRIA_WKT": r["GEOMETRIA_WKT"],
        "LATITUD": r["LATITUD"], "LONGITUD": r["LONGITUD"],
    } for r in activos])

    df_inm = pd.DataFrame([{
        k: r[k] for k in ("ID", "ALIAS", "PRECIO_ADQUISICION", "TERRENO_M2",
                          "CONSTRUCCION_M2", "PAIS", "FECHA_ESCRITURA",
                          "EMPRESA", "MUNICIPIO", "ESTADO", "STATUS_ACTIVIDAD")
    } for r in activos])

    df_inpc = pd.DataFrame(_fx.generar_inpc())
    df_rate = pd.DataFrame([{"ID_INMUEBLE": r["ID"], "RATING": r["RATING"],
                             "RESENAS": r["RESENAS"]}
                            for r in activos if r["RATING"] is not None])
    return df_geo, df_inm, df_inpc, df_rate


def cargar_datos():
    """Devuelve (df_mapa, df_inm) enriquecidos y listos para la vista."""
    if os.environ.get("DEMO_MODE") == "1":
        df_geo, df_inm, df_inpc, df_rate = _datos_demo()
        return _preparar(df_geo, df_inm, df_inpc, df_rate)
    conn = crear_conexion()
    try:
        cur = conn.cursor()
        df_geo  = _fetch(cur, QUERY_POLIGONOS)
        df_inm  = _fetch(cur, QUERY_INMUEBLES)
        df_inpc = _fetch(cur, QUERY_INPC)
        try:
            df_rate = _fetch(cur, QUERY_GMAPS)
        except Exception as e:
            print("ℹ️ Sin calificaciones de Google Maps (tabla INMUEBLE_GMAPS aún no existe):", e)
            df_rate = pd.DataFrame(columns=["ID_INMUEBLE", "RATING", "RESENAS"])
    finally:
        conn.close()

    return _preparar(df_geo, df_inm, df_inpc, df_rate)


def _preparar(df_geo, df_inm, df_inpc, df_rate):
    """Todo lo que pasa despues de traer los datos: geometria, areas, INPC.

    Identico para Snowflake y para el modo demo.
    """
    # --- INPC ---
    if len(df_inpc):
        df_inpc = df_inpc.rename(columns={"FECHA": "fecha", "VALOR": "valor"})
        df_inpc["fecha"] = pd.to_datetime(df_inpc["fecha"])
        df_inpc["valor"] = pd.to_numeric(df_inpc["valor"], errors="coerce")
        df_inpc = df_inpc.sort_values("fecha").reset_index(drop=True)

    # --- Geometría ---
    df_geo["LATITUD"]  = pd.to_numeric(df_geo["LATITUD"], errors="coerce")
    df_geo["LONGITUD"] = pd.to_numeric(df_geo["LONGITUD"], errors="coerce")
    df_geo["geom"]      = df_geo["GEOMETRIA_WKT"].apply(
        lambda w: wkt.loads(w) if isinstance(w, str) and w.strip() else None)
    df_geo = df_geo[df_geo["geom"].notna()].copy()
    df_geo["positions"] = df_geo["geom"].apply(_geom_to_positions)
    df_geo["AREA_KMZ_M2"] = df_geo["geom"].apply(_area_m2)

    # --- Enriquecer con columnas de INMUEBLES ---
    cols = ["ID", "ALIAS", "PRECIO_ADQUISICION", "TERRENO_M2",
            "CONSTRUCCION_M2", "PAIS", "FECHA_ESCRITURA"]
    cols = [c for c in cols if c in df_inm.columns]
    df_mapa = df_geo.merge(df_inm[cols], left_on="ID_INMUEBLE", right_on="ID", how="left")

    # TERRENO_M2 = valor de tabla si es válido (>0 y <500000); si no, área del polígono
    terreno_tabla = pd.to_numeric(df_mapa["TERRENO_M2"], errors="coerce")
    df_mapa["TERRENO_M2"] = terreno_tabla.where(
        (terreno_tabla > 0) & (terreno_tabla < 500000),
        df_mapa["AREA_KMZ_M2"].round(2),
    )

    # --- Precalcular INPC por inmueble (para los popups) ---
    def _inpc_row(r):
        return obtener_valor_inflacionado(r.get("PRECIO_ADQUISICION"),
                                          r.get("FECHA_ESCRITURA"), df_inpc)
    inpc_calc = df_mapa.apply(_inpc_row, axis=1)
    df_mapa["INPC_VALOR"] = inpc_calc.apply(lambda x: x["valor"] if x else None)
    df_mapa["INPC_FECHA"] = inpc_calc.apply(lambda x: x["fecha_inpc"] if x else None)
    df_mapa["INPC_VAR"]   = inpc_calc.apply(lambda x: x["variacion_pct"] if x else None)

    # --- Calificaciones de Google Maps ---
    if df_rate is not None and len(df_rate):
        df_rate["RATING"]  = pd.to_numeric(df_rate["RATING"], errors="coerce")
        df_rate["RESENAS"] = pd.to_numeric(df_rate["RESENAS"], errors="coerce")
        df_mapa = df_mapa.merge(df_rate, on="ID_INMUEBLE", how="left")
    else:
        df_mapa["RATING"] = None
        df_mapa["RESENAS"] = None

    return df_mapa, df_inm


# ============================================================================
# ESTADO GLOBAL (se recarga con el botón "Recargar Mapa")
# ============================================================================
STATE = {"df_mapa": pd.DataFrame(), "df_inm": pd.DataFrame(),
         "coords": {}, "terreno_max": 50000, "error": None}


def recargar_estado():
    try:
        df_mapa, df_inm = cargar_datos()
        STATE["df_mapa"] = df_mapa
        STATE["df_inm"]  = df_inm
        STATE["coords"]  = {
            r["ID_INMUEBLE"]: (r["LATITUD"], r["LONGITUD"])
            for _, r in df_mapa.iterrows()
            if pd.notna(r["LATITUD"]) and pd.notna(r["LONGITUD"])
        }
        STATE["terreno_max"] = _calc_terreno_max(df_mapa)
        STATE["error"] = None
        print(f"✅ Datos cargados: {len(df_mapa)} polígonos, {len(df_inm)} inmuebles activos")
    except Exception as e:
        STATE["error"] = str(e)
        print("❌ Error al cargar datos de Snowflake:", e)


def _calc_terreno_max(df):
    t = pd.to_numeric(df["TERRENO_M2"], errors="coerce")
    t = t[(t > 0) & (t <= 500000)]
    if len(t) == 0:
        return 50000
    p90 = t.quantile(0.90)
    return int(min((-(-p90 // 1000)) * 1000, 50000))   # ceil(p90/1000)*1000, tope 50000


# ============================================================================
# FILTRADO (réplica de datos_mapa_inmuebles_filtrados)
# ============================================================================
def _norm(v):
    """Normaliza el valor de un dropdown múltiple a lista."""
    if v is None:
        return []
    if isinstance(v, list):
        return [x for x in v if x not in (None, "")]
    return [v] if v != "" else []


def filtrar(df, empresa, estado, municipio, inmueble, terreno, incluir_grandes,
            terreno_max_ref):
    if df is None or len(df) == 0:
        return df
    d = df
    if empresa:   d = d[d["EMPRESA"].isin(empresa)]
    if estado:    d = d[d["ESTADO"].isin(estado)]
    if municipio: d = d[d["MUNICIPIO"].isin(municipio)]
    if inmueble and "INMUEBLE" in d.columns:
        d = d[d["INMUEBLE"].isin(inmueble)]

    # Filtro de terreno: solo si el usuario movió el slider respecto al rango completo
    if terreno and "TERRENO_M2" in d.columns:
        tmin, tmax = terreno[0], terreno[1]
        movido = (tmin > 0) or (tmax < terreno_max_ref)
        if movido:
            val = pd.to_numeric(d["TERRENO_M2"], errors="coerce")
            if incluir_grandes:
                d = d[val.notna() & (((val >= tmin) & (val <= tmax)) | (val > tmax))]
            else:
                d = d[val.notna() & (val >= tmin) & (val <= tmax)]
    return d


def filtrar_kpi(df, empresa, estado, municipio, terreno, incluir_grandes):
    """Filtro para las fichas KPI sobre la tabla completa de inmuebles activos."""
    if df is None or len(df) == 0:
        return df
    d = df
    if empresa:   d = d[d["EMPRESA"].isin(empresa)]
    if estado:    d = d[d["ESTADO"].isin(estado)]
    if municipio: d = d[d["MUNICIPIO"].isin(municipio)]
    if terreno and "TERRENO_M2" in d.columns:
        tmin, tmax = terreno[0], terreno[1]
        val = pd.to_numeric(d["TERRENO_M2"], errors="coerce")
        d = d[val.isna() | (val == 0) |
              ((val >= tmin) & (val <= tmax)) |
              (incluir_grandes & (val > tmax))]
    return d


# ============================================================================
# HELPERS DE FORMATO (réplica de safe_val / safe_num)
# ============================================================================
def safe_val(x, default="-"):
    if x is None or (isinstance(x, float) and pd.isna(x)) or str(x).strip() in ("", "nan"):
        return default
    return str(x)


def safe_num(x):
    try:
        v = float(x)
        if pd.isna(v) or v == 0:
            return "-"
        return f"{int(round(v)):,}"
    except (TypeError, ValueError):
        return "-"


def estrellas_txt(rating, count=None):
    """Devuelve '★★★★☆ 4.3 (128)' a partir de una calificación 0-5."""
    if rating is None or pd.isna(rating):
        return ""
    r = float(rating)
    llenas = int(round(r))
    llenas = max(0, min(5, llenas))
    s = "★" * llenas + "☆" * (5 - llenas)
    txt = f"{s} {r:.1f}"
    if count is not None and pd.notna(count):
        txt += f" ({int(count)})"
    return txt


# ============================================================================
# POPUP INFORMATIVO (réplica del sprintf HTML del mapa en R)
# ============================================================================
def build_popup(row):
    children = [
        html.H4(safe_val(row.get("INMUEBLE") or row.get("ID_INMUEBLE")),
                style={"color": "#ff5b49", "margin": "0 0 6px 0"}),
    ]

    # Calificación de Google Maps (si el inmueble tiene ficha en Google)
    if pd.notna(row.get("RATING")):
        children.append(html.Div(
            estrellas_txt(row.get("RATING"), row.get("RESENAS")),
            style={"color": "#f5b301", "fontWeight": "bold", "margin": "0 0 8px 0",
                   "fontSize": "13px"}))

    children += [
        html.Div([html.B("ID: "), safe_val(row.get("ID_INMUEBLE"))]),
        html.Div([html.B("Empresa: "), safe_val(row.get("EMPRESA"))]),
        html.Div([html.B("Estado: "), safe_val(row.get("ESTADO"))]),
        html.Div([html.B("Municipio: "), safe_val(row.get("MUNICIPIO"))]),
        html.Div([html.B("Terreno: "), f"{safe_num(row.get('TERRENO_M2'))} m²"]),
        html.Div([html.B("Construcción: "), f"{safe_num(row.get('CONSTRUCCION_M2'))} m²"]),
    ]

    # Bloque de Precio de Adquisición actualizado por INPC
    if pd.notna(row.get("INPC_VALOR")):
        children.append(html.Div([
            html.B(f"Precio Adquisición (INPC {row.get('INPC_FECHA')}):"),
            html.Br(),
            html.Span(f"${int(row['INPC_VALOR']):,} MXN",
                      style={"fontSize": "13px", "color": "#802e25", "fontWeight": "bold"}),
            html.Span(f"  (+{row.get('INPC_VAR')}% inflación)",
                      style={"fontSize": "10px", "color": "#888"}),
            html.Br(),
            html.Span(f"Original: ${safe_num(row.get('PRECIO_ADQUISICION'))} MXN",
                      style={"fontSize": "10px", "color": "#aaa"}),
        ], style={"background": "#f9f9f9", "borderLeft": "3px solid #ff5b49",
                  "padding": "6px 8px", "margin": "6px 0", "borderRadius": "3px"}))
    elif pd.notna(row.get("PRECIO_ADQUISICION")):
        children.append(html.Div([html.B("Precio Adquisición: "),
                                  f"${safe_num(row.get('PRECIO_ADQUISICION'))}"]))

    return html.Div(children, style={"fontFamily": "Arial", "fontSize": "12px",
                                     "minWidth": "260px"})


# ============================================================================
# CONSTRUCCIÓN DE CAPAS DEL MAPA
# ============================================================================
def construir_capas(df):
    poligonos, marcadores = [], []
    if df is None or len(df) == 0:
        return poligonos, marcadores

    for _, r in df.iterrows():
        idv = r["ID_INMUEBLE"]

        if r.get("positions"):
            poligonos.append(dl.Polygon(
                positions=r["positions"],
                color="#ff5b49", weight=2, fillColor="#ff5b49", fillOpacity=0.5,
                children=[dl.Tooltip(safe_val(r.get("INMUEBLE"))),
                          dl.Popup(build_popup(r), maxWidth=320)],
                id={"type": "poligono", "index": str(idv)},
            ))

        if pd.notna(r.get("LATITUD")) and pd.notna(r.get("LONGITUD")):
            marcadores.append(dl.CircleMarker(
                center=[r["LATITUD"], r["LONGITUD"]],
                radius=6, color="#2C3E50", weight=2,
                fillColor="#3498DB", fillOpacity=0.8,
                children=[dl.Tooltip(safe_val(r.get("INMUEBLE"))),
                          dl.Popup(build_popup(r), maxWidth=320)],
                id={"type": "marcador", "index": str(idv)},
            ))
    return poligonos, marcadores


def bounds_de(df):
    if df is None or len(df) == 0:
        return no_update
    lat = pd.to_numeric(df["LATITUD"], errors="coerce").dropna()
    lon = pd.to_numeric(df["LONGITUD"], errors="coerce").dropna()
    if len(lat) == 0 or len(lon) == 0:
        return no_update
    return [[float(lat.min()), float(lon.min())],
            [float(lat.max()), float(lon.max())]]


def construir_tabla(df):
    if df is None or len(df) == 0:
        return []
    filas = []
    for _, r in df.iterrows():
        idv = r["ID_INMUEBLE"]
        if idv is None or str(idv).strip() in ("", "-", "nan"):
            continue
        terreno = safe_num(r.get("TERRENO_M2"))
        terreno = f"{terreno} m²" if terreno != "-" else "-"
        lat, lon = r.get("LATITUD"), r.get("LONGITUD")
        maps = (f"[Ver](https://www.google.com/maps?q={lat},{lon})"
                if pd.notna(lat) and pd.notna(lon) else "-")
        cal = estrellas_txt(r.get("RATING"), r.get("RESENAS"))
        filas.append({
            "ID": safe_val(idv), "Alias": safe_val(r.get("ALIAS")),
            "Empresa": safe_val(r.get("EMPRESA")), "Estado": safe_val(r.get("ESTADO")),
            "Terreno": terreno, "Calificacion": cal if cal else "-", "Maps": maps,
        })
    return filas


# ============================================================================
# CARGA INICIAL
# ============================================================================
recargar_estado()
_df0 = STATE["df_mapa"]


def _opts(df, col):
    if df is None or len(df) == 0 or col not in df.columns:
        return []
    vals = sorted(v for v in df[col].dropna().unique()
                  if str(v).strip() not in ("", "-"))
    return [{"label": v, "value": v} for v in vals]


# ============================================================================
# APLICACIÓN DASH  ·  LAYOUT
# ============================================================================
app = Dash(__name__, title="Maestro de Inmuebles ALMENA",
           suppress_callback_exceptions=True)
server = app.server

TMAX = STATE["terreno_max"]


def panel_filtros():
    return html.Div(id="panel_filtros", className="filtros-panel", children=[
        html.Div(className="filtros-header", children=[
            html.H3("FILTROS"),
            html.Button("☰", id="toggle_filtros", className="hamburger-btn",
                        title="Ocultar/Mostrar Filtros", n_clicks=0),
        ]),
        html.Hr(),

        html.Div(className="filtro-grupo", children=[
            html.Label("Empresa:", className="filtro-label"),
            dcc.Dropdown(id="filtro_empresa", options=_opts(_df0, "EMPRESA"),
                         multi=True, placeholder="Todas"),
        ]),
        html.Div(className="filtro-grupo", children=[
            html.Label("Estado:", className="filtro-label"),
            dcc.Dropdown(id="filtro_estado", options=_opts(_df0, "ESTADO"),
                         multi=True, placeholder="Todos"),
        ]),
        html.Div(className="filtro-grupo", children=[
            html.Label("Municipio:", className="filtro-label"),
            dcc.Dropdown(id="filtro_municipio", options=_opts(_df0, "MUNICIPIO"),
                         multi=True, placeholder="Todos"),
        ]),
        html.Div(className="filtro-grupo", children=[
            html.Label("Inmueble:", className="filtro-label"),
            dcc.Dropdown(id="filtro_inmueble", options=_opts(_df0, "INMUEBLE"),
                         multi=True, placeholder="Todos"),
        ]),
        html.Div(className="filtro-grupo", children=[
            html.Label("Terreno M²:", className="filtro-label"),
            dcc.RangeSlider(id="filtro_terreno_m2", min=0, max=TMAX,
                            value=[0, TMAX], step=50,
                            marks={0: "0", TMAX: f"{TMAX:,}"},
                            tooltip={"placement": "bottom", "always_visible": False}),
            dcc.Checklist(id="incluir_grandes",
                          options=[{"label": " Incluir inmuebles grandes (>50,000 m²)",
                                    "value": "si"}],
                          value=["si"], className="filtro-check"),
        ]),
        html.Hr(),

        html.Button("Limpiar Filtros", id="limpiar_filtros",
                    className="btn-mi btn-limpiar", n_clicks=0),
        html.Button("↻ Recargar Mapa", id="recargar_mapa",
                    className="btn-mi btn-vino", n_clicks=0),
        html.Button("⭳ Descargar Excel", id="descargar_excel_mi",
                    className="btn-mi btn-vino", n_clicks=0),
        dcc.Download(id="download_excel"),
    ])


def contenido_principal():
    return html.Div(id="contenido_principal", className="contenido-principal", children=[
        html.Button("☰", id="mostrar_filtros_btn", className="boton-flotante",
                    n_clicks=0, style={"display": "none"}),

        # Fichas KPI
        html.Div(className="kpi-row", children=[
            _kpi("total_inmuebles", "Inmuebles Totales"),
            _kpi("total_terreno_m2", "Terreno M² Total"),
            _kpi("total_empresas", "Empresas"),
            _kpi("total_valor_portafolio", "Valor Total · Precio de Adquisición"),
        ]),

        # Mapa + Tabla
        html.Div(className="mapa-tabla-row", children=[
            html.Div(className="caja-mapa", children=[
                html.Div("Ubicación de Inmuebles", className="caja-titulo titulo-mapa"),
                dl.Map(id="mapa_inmuebles", center=[23.6, -102.5], zoom=5,
                       boundsOptions={"padding": [25, 25]},
                       children=[
                           dl.TileLayer(),
                           dl.LayerGroup(id="capa_poligonos"),
                           dl.LayerGroup(id="capa_marcadores"),
                       ]),
            ]),
            html.Div(className="caja-tabla", children=[
                html.Div("Información Detallada", className="caja-titulo titulo-tabla"),
                DataTable(
                    id="tabla_inmuebles",
                    columns=[
                        {"name": "ID", "id": "ID"},
                        {"name": "Alias", "id": "Alias"},
                        {"name": "Empresa", "id": "Empresa"},
                        {"name": "Estado", "id": "Estado"},
                        {"name": "Terreno", "id": "Terreno"},
                        {"name": "Calificación", "id": "Calificacion"},
                        {"name": "Maps", "id": "Maps", "presentation": "markdown"},
                    ],
                    data=[], page_action="none",
                    sort_action="native", filter_action="native",
                    style_table={"height": "62vh", "overflowY": "auto",
                                 "overflowX": "auto"},
                    style_cell={"fontFamily": "Segoe UI, Arial", "fontSize": "13px",
                                "textAlign": "left", "padding": "6px"},
                    style_header={"backgroundColor": "#802e25", "color": "white",
                                  "fontWeight": "bold"},
                    style_data_conditional=[
                        {"if": {"row_index": "odd"}, "backgroundColor": "#faf5f4"}],
                    css=[{"selector": ".dash-cell-value a",
                          "rule": "background:#ff5b49;color:white;padding:2px 10px;"
                                  "border-radius:4px;text-decoration:none;"}],
                ),
            ]),
        ]),
    ])


def _kpi(id_, label):
    return html.Div(className="kpi-card", children=[
        html.H2("0", id=id_, className="kpi-valor"),
        html.P(label, className="kpi-label"),
    ])


app.layout = html.Div([
    dcc.Store(id="seleccionado"),
    dcc.Store(id="data_version", data=0),
    dcc.Store(id="panel_visible", data=True),
    html.Div(id="toast_area"),

    html.Div(className="mi-navbar", children=[
        html.Span("🏢 M.I. ALMENA", className="mi-title"),
        html.Span("🗄️ Maestro de Inmuebles", className="mi-tab"),
    ]),

    html.Div(className="mi-wrapper", children=[
        panel_filtros(),
        contenido_principal(),
    ]),
])


# ============================================================================
# CALLBACKS
# ============================================================================

# --- Filtros en cascada (opciones dependientes) -----------------------------
@app.callback(
    Output("filtro_empresa", "options"),
    Output("filtro_estado", "options"),
    Output("filtro_municipio", "options"),
    Output("filtro_inmueble", "options"),
    Input("filtro_empresa", "value"),
    Input("filtro_estado", "value"),
    Input("filtro_municipio", "value"),
    Input("data_version", "data"),
)
def cascada_opciones(f_emp, f_est, f_mun, _ver):
    df = STATE["df_mapa"]
    emp, est, mun = _norm(f_emp), _norm(f_est), _norm(f_mun)

    base = df
    if emp: base = base[base["EMPRESA"].isin(emp)]
    d_est = base
    if est: d_est = d_est[d_est["ESTADO"].isin(est)]
    d_mun = d_est
    if mun: d_mun = d_mun[d_mun["MUNICIPIO"].isin(mun)]

    return (_opts(df, "EMPRESA"), _opts(base, "ESTADO"),
            _opts(d_est, "MUNICIPIO"), _opts(d_mun, "INMUEBLE"))


# --- Vista principal: KPIs + mapa + tabla -----------------------------------
@app.callback(
    Output("capa_poligonos", "children"),
    Output("capa_marcadores", "children"),
    Output("mapa_inmuebles", "bounds"),
    Output("total_inmuebles", "children"),
    Output("total_terreno_m2", "children"),
    Output("total_empresas", "children"),
    Output("total_valor_portafolio", "children"),
    Output("tabla_inmuebles", "data"),
    Input("filtro_empresa", "value"),
    Input("filtro_estado", "value"),
    Input("filtro_municipio", "value"),
    Input("filtro_inmueble", "value"),
    Input("filtro_terreno_m2", "value"),
    Input("incluir_grandes", "value"),
    Input("data_version", "data"),
)
def actualizar_vista(f_emp, f_est, f_mun, f_inm, terreno, grandes, _ver):
    emp, est, mun, inm = _norm(f_emp), _norm(f_est), _norm(f_mun), _norm(f_inm)
    incluir = bool(grandes)
    tmax_ref = STATE["terreno_max"]

    # Mapa + tabla (solo inmuebles con geometría)
    dfm = filtrar(STATE["df_mapa"], emp, est, mun, inm, terreno, incluir, tmax_ref)
    poligonos, marcadores = construir_capas(dfm)
    bounds = bounds_de(dfm)
    tabla = construir_tabla(dfm)

    # KPIs (sobre todos los inmuebles activos)
    dfk = filtrar_kpi(STATE["df_inm"], emp, est, mun, terreno, incluir)
    total_inm = f"{len(dfk):,}" if dfk is not None else "0"

    terr = pd.to_numeric(dfk["TERRENO_M2"], errors="coerce").sum() if len(dfk) else 0
    total_terr = f"{int(round(terr)):,} m²"

    empresas = dfk["EMPRESA"].dropna()
    empresas = empresas[~empresas.isin(["", "-"])].nunique() if len(dfk) else 0
    total_emp = f"{empresas:,}"

    valor = pd.to_numeric(dfk["PRECIO_ADQUISICION"], errors="coerce").sum() if len(dfk) else 0
    if valor >= 1e9:
        total_val = f"${valor/1e9:,.2f}B"
    elif valor >= 1e6:
        total_val = f"${valor/1e6:,.2f}M"
    else:
        total_val = f"${int(round(valor)):,}"

    return (poligonos, marcadores, bounds,
            total_inm, total_terr, total_emp, total_val, tabla)


# --- Selección: click en mapa o en tabla ------------------------------------
@app.callback(
    Output("seleccionado", "data"),
    Input({"type": "poligono", "index": ALL}, "n_clicks"),
    Input({"type": "marcador", "index": ALL}, "n_clicks"),
    Input("tabla_inmuebles", "active_cell"),
    State("tabla_inmuebles", "data"),
    prevent_initial_call=True,
)
def registrar_seleccion(_pc, _mc, active_cell, tabla_data):
    if not ctx.triggered:
        return no_update
    trig = ctx.triggered_id
    trig_val = ctx.triggered[0].get("value")
    if isinstance(trig, dict) and "index" in trig:
        # Ignorar disparos espurios al montarse los marcadores (n_clicks None/0)
        if not trig_val:
            return no_update
        return trig["index"]
    if trig == "tabla_inmuebles" and active_cell and tabla_data:
        fila = tabla_data[active_cell["row"]]
        return fila.get("ID")
    return no_update


# --- Resaltar fila en tabla + centrar mapa ----------------------------------
@app.callback(
    Output("tabla_inmuebles", "style_data_conditional"),
    Output("mapa_inmuebles", "center"),
    Output("mapa_inmuebles", "zoom"),
    Input("seleccionado", "data"),
    prevent_initial_call=True,
)
def resaltar_y_centrar(sel):
    base = [{"if": {"row_index": "odd"}, "backgroundColor": "#faf5f4"}]
    if not sel:
        return base, no_update, no_update

    base.append({
        "if": {"filter_query": f'{{ID}} = "{sel}"'},
        "backgroundColor": "#FFE9CC", "fontWeight": "bold",
        "borderLeft": "4px solid #ff5b49",
    })

    coord = STATE["coords"].get(sel)
    if coord:
        return base, [float(coord[0]), float(coord[1])], 18
    return base, no_update, no_update


# --- Limpiar filtros --------------------------------------------------------
@app.callback(
    Output("filtro_empresa", "value"),
    Output("filtro_estado", "value"),
    Output("filtro_municipio", "value"),
    Output("filtro_inmueble", "value"),
    Output("filtro_terreno_m2", "value"),
    Output("incluir_grandes", "value"),
    Input("limpiar_filtros", "n_clicks"),
    prevent_initial_call=True,
)
def limpiar_filtros(_n):
    return [], [], [], [], [0, STATE["terreno_max"]], ["si"]


# --- Recargar datos desde Snowflake -----------------------------------------
@app.callback(
    Output("data_version", "data"),
    Output("toast_area", "children"),
    Input("recargar_mapa", "n_clicks"),
    State("data_version", "data"),
    prevent_initial_call=True,
)
def recargar(_n, ver):
    recargar_estado()
    if STATE["error"]:
        msg = html.Div(f"⚠️ Error: {STATE['error']}", className="mi-toast",
                       style={"background": "#c0392b"})
    else:
        msg = html.Div(f"✔ Mapa actualizado · {len(STATE['df_mapa'])} inmuebles",
                       className="mi-toast")
    return (ver or 0) + 1, msg


# --- Descargar Excel --------------------------------------------------------
@app.callback(
    Output("download_excel", "data"),
    Input("descargar_excel_mi", "n_clicks"),
    State("filtro_empresa", "value"),
    State("filtro_estado", "value"),
    State("filtro_municipio", "value"),
    State("filtro_inmueble", "value"),
    prevent_initial_call=True,
)
def descargar_excel(_n, f_emp, f_est, f_mun, f_inm):
    emp, est, mun, inm = _norm(f_emp), _norm(f_est), _norm(f_mun), _norm(f_inm)
    d = STATE["df_inm"]
    if emp: d = d[d["EMPRESA"].isin(emp)]
    if est: d = d[d["ESTADO"].isin(est)]
    if mun: d = d[d["MUNICIPIO"].isin(mun)]
    if inm and "INMUEBLE" in d.columns:
        d = d[d["INMUEBLE"].isin(inm)]
    d = d.drop(columns=[c for c in d.columns if d[c].dtype == "object"
                        and c.upper().startswith("GEOMETRIA")], errors="ignore")
    return dcc.send_data_frame(d.to_excel, "ALMENA_Maestro_Inmuebles.xlsx", index=False)


# --- Mostrar / ocultar panel de filtros -------------------------------------
@app.callback(
    Output("panel_filtros", "className"),
    Output("contenido_principal", "style"),
    Output("mostrar_filtros_btn", "style"),
    Output("panel_visible", "data"),
    Input("toggle_filtros", "n_clicks"),
    Input("mostrar_filtros_btn", "n_clicks"),
    State("panel_visible", "data"),
    prevent_initial_call=True,
)
def toggle_filtros(_t, _m, visible):
    visible = not visible
    if visible:
        return "filtros-panel", {}, {"display": "none"}, True
    return "filtros-panel oculto", {}, {"display": "block"}, False


# ============================================================================
if __name__ == "__main__":
    app.run(debug=True, port=8050)
