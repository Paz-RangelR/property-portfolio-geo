# -*- coding: utf-8 -*-
"""
Synthetic property portfolio with real geometry.

The polygons are invented, but they are built the way real parcel footprints
are: an irregular ring of vertices around a centroid, in WGS84, sized in metres
and converted to degrees at that latitude. So `shapely` parses them, `pyproj`
measures them, and the areas that come out are plausible — which is what the
app's surface-area logic needs in order to be exercised at all.

Nothing here corresponds to a real property, company, address or price.
Centroids are scattered around six Mexican cities that are NOT the ones in the
original portfolio.
"""
from __future__ import annotations

import math
import random
from datetime import date, timedelta

SEED = 1701

# (city, state, lat, lon) — real cities, invented properties.
CIUDADES = [
    ("Queretaro", "Queretaro", 20.5888, -100.3899),
    ("Aguascalientes", "Aguascalientes", 21.8853, -102.2916),
    ("Leon", "Guanajuato", 21.1219, -101.6833),
    ("Merida", "Yucatan", 20.9674, -89.5926),
    ("Puebla", "Puebla", 19.0414, -98.2063),
    ("Culiacan", "Sinaloa", 24.8091, -107.3940),
]

EMPRESAS = [
    "ALMENA ADMINISTRACION INMOBILIARIA SA DE CV",
    "ALMENA DESARROLLOS Y PROYECTOS SA DE CV",
    "INMOBILIARIA LOMA PRIETA SA DE CV",
    "EDIFICIOS DE LA PRADERA SA DE CV",
    "BIENES SAN MARCOS SA DE CV",
]

TIPOS = [
    ("Paseo", "Centro comercial", 18_000, 140_000),
    ("Plaza", "Centro comercial", 9_000, 60_000),
    ("Punto", "Strip center", 3_500, 18_000),
    ("Torre", "Oficinas", 1_200, 7_000),
    ("Nave", "Industrial", 12_000, 90_000),
    ("Predio", "Terreno", 5_000, 260_000),
]

SUFIJOS = [
    "Altamira", "Bernal", "San Isidro", "La Rioja", "Los Alamos", "Alameda",
    "El Mirador", "Santa Rosa", "Las Huertas", "Del Roble", "La Cantera",
    "Valle Verde", "San Marcos", "Portal Norte", "Las Palmas", "Rio Hondo",
    "El Encino", "Puerta Sur", "Los Fresnos", "Bella Vista", "Cerro Azul",
    "La Herradura", "Camino Real", "Nuevo Centro", "Pinar", "Solares",
]

CALLES = [
    "Av. Constituyentes", "Blvd. Independencia", "Av. Reforma", "Calle 60 Norte",
    "Av. Juarez", "Blvd. Solidaridad", "Av. Universidad", "Calzada del Valle",
]

STATUS = ["Activo", "Activo", "Activo", "Activo", "Inactivo", "En proceso"]

# Land value per square metre, by use. Invented, but ordered the way a real
# portfolio is: offices per m2 are worth many times industrial land.
PRECIO_M2 = {
    "Centro comercial": 5_200,
    "Strip center": 4_100,
    "Oficinas": 14_000,
    "Industrial": 1_700,
    "Terreno": 1_100,
}


def _anillo(lat: float, lon: float, lado_m: float, rng: random.Random,
            vertices: int = 7) -> list[tuple[float, float]]:
    """An irregular closed ring around (lat, lon), roughly `lado_m` across."""
    # Metres per degree at this latitude.
    m_por_grado_lat = 111_320.0
    m_por_grado_lon = 111_320.0 * math.cos(math.radians(lat))
    radio = lado_m / 2.0

    puntos = []
    for i in range(vertices):
        ang = 2 * math.pi * i / vertices
        r = radio * rng.uniform(0.72, 1.28)          # irregular, not a circle
        dx = r * math.cos(ang)
        dy = r * math.sin(ang)
        puntos.append((lon + dx / m_por_grado_lon, lat + dy / m_por_grado_lat))
    puntos.append(puntos[0])                          # close the ring
    return puntos


def _wkt(puntos: list[tuple[float, float]]) -> str:
    cuerpo = ", ".join(f"{x:.6f} {y:.6f}" for x, y in puntos)
    return f"POLYGON (({cuerpo}))"


def generar_inmuebles(n: int = 42, semilla: int = SEED) -> list[dict]:
    rng = random.Random(semilla)
    inmuebles = []
    for i in range(n):
        ciudad, estado, lat0, lon0 = CIUDADES[i % len(CIUDADES)]
        prefijo, uso, area_min, area_max = TIPOS[i % len(TIPOS)]
        nombre = f"{prefijo} {SUFIJOS[i % len(SUFIJOS)]}"

        # Scatter within ~12 km of the city centre.
        lat = lat0 + rng.uniform(-0.11, 0.11)
        lon = lon0 + rng.uniform(-0.11, 0.11)

        terreno = round(rng.uniform(area_min, area_max), 2)
        lado = math.sqrt(terreno) * rng.uniform(1.0, 1.25)
        anillo = _anillo(lat, lon, lado, rng)

        construccion = round(terreno * rng.uniform(0.25, 0.85), 2)
        escritura = date(2008, 1, 1) + timedelta(days=rng.randint(0, 6000))

        inmuebles.append({
            "ID": 1000 + i,
            "ALIAS": nombre,
            "INMUEBLE": nombre,
            "EMPRESA": EMPRESAS[i % len(EMPRESAS)],
            "USO": uso,
            "DIRECCION": f"{rng.choice(CALLES)} {rng.randint(100, 4999)}",
            "MUNICIPIO": ciudad,
            "ESTADO": estado,
            "PAIS": "Mexico",
            "LATITUD": round(lat, 6),
            "LONGITUD": round(lon, 6),
            "GEOMETRIA_WKT": _wkt(anillo),
            "TERRENO_M2": terreno,
            "CONSTRUCCION_M2": construccion,
            "PRECIO_ADQUISICION": round(terreno * PRECIO_M2[uso] * rng.uniform(0.8, 1.25), 2),
            "FECHA_ESCRITURA": escritura.isoformat(),
            "STATUS_ACTIVIDAD": rng.choice(STATUS),
            "RATING": round(rng.uniform(3.4, 4.9), 1) if rng.random() > 0.25 else None,
            "RESENAS": rng.randint(12, 2400) if rng.random() > 0.25 else None,
        })
    return inmuebles


def generar_inpc(semilla: int = SEED) -> list[dict]:
    """Monthly index, 2008 to 2026. Shaped like INEGI's INPC, values invented."""
    rng = random.Random(semilla + 3)
    serie, valor = [], 62.0
    for anio in range(2008, 2027):
        for mes in range(1, 13):
            if anio == 2026 and mes > 9:
                break
            valor *= 1 + rng.uniform(0.0008, 0.0062)
            serie.append({"FECHA": date(anio, mes, 1).isoformat(),
                          "VALOR": round(valor, 4)})
    return serie


def valor_inflacionado(precio: float, fecha_escritura: str,
                       inpc: list[dict]) -> float | None:
    """Same rule the app uses: index at today / index at the deed date."""
    if not precio or not fecha_escritura or not inpc:
        return None
    base = next((r["VALOR"] for r in inpc if r["FECHA"][:7] == fecha_escritura[:7]),
                None)
    if not base:
        anteriores = [r for r in inpc if r["FECHA"] <= fecha_escritura]
        if not anteriores:
            return None
        base = anteriores[-1]["VALOR"]
    return round(precio * (inpc[-1]["VALOR"] / base), 2)


def generar_locales(inmuebles: list[dict], semilla: int = SEED) -> list[dict]:
    """Leasable units inside the shopping-centre properties."""
    rng = random.Random(semilla + 11)
    arrendatarios = [
        "Cafe Bonanza", "Farmacia Lucero", "Banco del Istmo", "Gimnasio Vertice",
        "Telecom Sierra", "Burger Nogal", "Optica Clara", "Tiendas Almendro",
        "Sushi Kaiso", "Papeleria Nova", "Academia Lincoln", "Viajes Orion",
    ]
    locales = []
    for inm in inmuebles:
        if inm["USO"] not in ("Centro comercial", "Strip center"):
            continue
        for k in range(rng.randint(8, 34)):
            ocupado = rng.random() > 0.22
            locales.append({
                "ID_INMUEBLE": inm["ID"],
                "INMUEBLE": inm["INMUEBLE"],
                "LOCAL": f"L-{k + 1:03d}",
                "NIVEL": rng.choice(["PB", "PB", "N1", "N2"]),
                "SUPERFICIE_M2": round(rng.uniform(18, 480), 2),
                "ESTATUS": "Ocupado" if ocupado else "Disponible",
                "ARRENDATARIO": rng.choice(arrendatarios) if ocupado else "",
                "RENTA_MENSUAL": (round(rng.uniform(12_000, 320_000), 2)
                                  if ocupado else None),
            })
    return locales
