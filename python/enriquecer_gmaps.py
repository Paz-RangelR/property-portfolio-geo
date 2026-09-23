# -*- coding: utf-8 -*-
"""
============================================================================
 ENRIQUECIMIENTO GOOGLE MAPS  ·  Maestro de Inmuebles ALMENA
============================================================================
 Recorre los inmuebles ACTIVOS de Snowflake, consulta la calificación (estrellas)
 y el número de reseñas en Google Maps (Places API New) y guarda el resultado
 en la tabla:  DB_ANALYTICS.SCH_CORE.INMUEBLE_GMAPS

 La app (app.py) lee esa tabla para mostrar las estrellas en el popup y la tabla.
 Solo los inmuebles que existen como ficha en Google traerán calificación.

 USO:
   1. Crea una API Key en Google Cloud con "Places API (New)" + facturación.
   2. Ponla en una variable de entorno:
        PowerShell:  $env:GOOGLE_API_KEY = "TU_API_KEY"
   3. Ejecuta:
        py enriquecer_gmaps.py           # procesa TODOS los inmuebles activos
        py enriquecer_gmaps.py --limit 20   # prueba barata con 20 (recomendado 1ª vez)

 COSTO: cada inmueble = 1 llamada a Text Search (con rating ≈ SKU "Pro").
        Google da crédito mensual gratis; revisa tu consola de facturación.
============================================================================
"""

import os
import sys
import time
import argparse

import pandas as pd
import requests
from cryptography.hazmat.primitives import serialization
import snowflake.connector
from snowflake.connector.pandas_tools import write_pandas

# --- Conexión a Snowflake (idéntica a app.py) ------------------------------
BASE_DIR = os.path.dirname(os.path.abspath(__file__))
PEM_PATH   = os.environ.get("SNOWFLAKE_PRIVATE_KEY_PATH", "")

SNOW_CFG = dict(
    account   = os.environ.get("SNOWFLAKE_ACCOUNT", ""),
    user="SVC_ANALYTICS",
    role="ROLE_ANALYTICS",
    warehouse="WH_ANALYTICS",
    database="DB_ANALYTICS",
    schema="SCH_CORE",
)

TABLA_DESTINO = "INMUEBLE_GMAPS"

QUERY_INMUEBLES = """
    SELECT i.ID, i.INMUEBLE, i.DIRECCION, i.MUNICIPIO, i.ESTADO,
           g.LATITUD_CENTROIDE  AS LAT,
           g.LONGITUD_CENTROIDE AS LON
    FROM DB_ANALYTICS.SCH_CORE.INMUEBLES i
    LEFT JOIN DB_ANALYTICS.SCH_CORE.INMUEBLES_GEOMETRIA g
      ON i.ID = g.ID
    WHERE i.ID IN (
      SELECT ID FROM DB_ANALYTICS.SCH_CORE.INMUEBLE_ACTIVIDAD
      WHERE UPPER(TRIM(STATUS_ACTIVIDAD)) = 'ACTIVO'
    )
"""

PLACES_URL = "https://places.googleapis.com/v1/places:searchText"


def _private_key_der():
    with open(PEM_PATH, "rb") as f:
        p_key = serialization.load_pem_private_key(f.read(), password=None)
    return p_key.private_bytes(
        encoding=serialization.Encoding.DER,
        format=serialization.PrivateFormat.PKCS8,
        encryption_algorithm=serialization.NoEncryption(),
    )


def crear_conexion():
    return snowflake.connector.connect(private_key=_private_key_der(), **SNOW_CFG)


def buscar_rating(nombre, direccion, lat, lon, api_key):
    """Consulta Google Places (New) y devuelve rating + reseñas del mejor match."""
    headers = {
        "Content-Type": "application/json",
        "X-Goog-Api-Key": api_key,
        "X-Goog-FieldMask": ("places.id,places.displayName,places.formattedAddress,"
                             "places.rating,places.userRatingCount,places.location"),
    }
    partes = [str(x) for x in (nombre, direccion) if x and str(x).strip() not in ("", "-", "nan")]
    query = " ".join(partes).strip()
    if not query:
        return None

    body = {"textQuery": query, "languageCode": "es", "regionCode": "MX"}
    if pd.notna(lat) and pd.notna(lon):
        body["locationBias"] = {"circle": {
            "center": {"latitude": float(lat), "longitude": float(lon)},
            "radius": 2000.0,
        }}

    try:
        resp = requests.post(PLACES_URL, headers=headers, json=body, timeout=25)
        if resp.status_code != 200:
            print(f"   ⚠️ HTTP {resp.status_code}: {resp.text[:160]}")
            return None
        places = resp.json().get("places", [])
        if not places:
            return None
        p = places[0]
        return {
            "PLACE_ID": p.get("id"),
            "NOMBRE_GMAPS": (p.get("displayName") or {}).get("text"),
            "DIRECCION_GMAPS": p.get("formattedAddress"),
            "RATING": p.get("rating"),
            "RESENAS": p.get("userRatingCount"),
        }
    except Exception as e:
        print(f"   ⚠️ Error consultando Google: {e}")
        return None


def main():
    parser = argparse.ArgumentParser(description="Enriquecer inmuebles con Google Maps")
    parser.add_argument("--limit", type=int, default=0,
                        help="Procesar solo N inmuebles (para pruebas). 0 = todos.")
    parser.add_argument("--pausa", type=float, default=0.15,
                        help="Segundos de pausa entre llamadas (evita rate-limit).")
    args = parser.parse_args()

    api_key = os.environ.get("GOOGLE_API_KEY", "").strip()
    if not api_key:
        print("❌ Falta la API Key. Ejecuta primero:")
        print('   $env:GOOGLE_API_KEY = "TU_API_KEY"')
        sys.exit(1)

    print("Conectando a Snowflake...")
    conn = crear_conexion()
    df = conn.cursor().execute(QUERY_INMUEBLES).fetch_pandas_all()
    print(f"Inmuebles activos a procesar: {len(df)}")
    if args.limit > 0:
        df = df.head(args.limit)
        print(f"(Modo prueba: solo los primeros {len(df)})")

    filas = []
    con_rating = 0
    for i, r in df.reset_index(drop=True).iterrows():
        nombre = r.get("INMUEBLE")
        res = buscar_rating(nombre, r.get("DIRECCION"), r.get("LAT"), r.get("LON"), api_key)
        rating = res.get("RATING") if res else None
        resenas = res.get("RESENAS") if res else None
        if rating is not None:
            con_rating += 1
        filas.append({
            "ID": r["ID"],
            "RATING": rating,
            "RESENAS": resenas,
            "NOMBRE_GMAPS": res.get("NOMBRE_GMAPS") if res else None,
            "DIRECCION_GMAPS": res.get("DIRECCION_GMAPS") if res else None,
            "PLACE_ID": res.get("PLACE_ID") if res else None,
        })
        estrellas = f"⭐ {rating} ({resenas})" if rating is not None else "sin ficha"
        print(f"  [{i+1}/{len(df)}] {str(nombre)[:45]:45s} -> {estrellas}")
        time.sleep(args.pausa)

    df_out = pd.DataFrame(filas)
    df_out["FECHA_ACTUALIZACION"] = pd.Timestamp.now()

    print(f"\nGuardando {len(df_out)} filas en {TABLA_DESTINO} ({con_rating} con calificación)...")
    write_pandas(conn, df_out, TABLA_DESTINO,
                 database="DB_ANALYTICS", schema="SCH_CORE",
                 auto_create_table=True, overwrite=True, quote_identifiers=False)
    conn.close()
    print("✅ Listo. Reinicia app.py (o pulsa 'Recargar Mapa') para ver las estrellas.")


if __name__ == "__main__":
    main()
