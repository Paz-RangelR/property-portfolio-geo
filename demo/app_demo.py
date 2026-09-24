# -*- coding: utf-8 -*-
"""
Simplified public demo of the property portfolio dashboard.

The production versions are in `R/` (Shiny + Leaflet + sf) and `python/`
(Dash + dash-leaflet + shapely + pyproj), both reading Snowflake. This is a
smaller Streamlit build over synthetic data, so the map can be opened by anyone
with no credentials.

    streamlit run demo/app_demo.py
"""
from __future__ import annotations

import sys
from pathlib import Path

import pandas as pd
import pydeck as pdk
import streamlit as st

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))
from demo import fixtures as fx      # noqa: E402

st.set_page_config(page_title="Property Portfolio — demo",
                   page_icon="🗺️", layout="wide")


# --------------------------------------------------------------------------- #
def anillo_de_wkt(wkt_texto: str) -> list[list[float]]:
    """POLYGON ((lon lat, ...)) -> [[lon, lat], ...] for pydeck."""
    cuerpo = wkt_texto[wkt_texto.index("((") + 2: wkt_texto.rindex("))")]
    return [[float(x), float(y)]
            for x, y in (par.split() for par in cuerpo.split(","))]


@st.cache_data
def cargar():
    inmuebles = fx.generar_inmuebles()
    inpc = fx.generar_inpc()
    locales = fx.generar_locales(inmuebles)
    for r in inmuebles:
        r["POLIGONO"] = anillo_de_wkt(r["GEOMETRIA_WKT"])
        r["VALOR_ACTUALIZADO"] = fx.valor_inflacionado(
            r["PRECIO_ADQUISICION"], r["FECHA_ESCRITURA"], inpc)
    return pd.DataFrame(inmuebles), pd.DataFrame(locales), inpc


df, locales, inpc = cargar()

COLORES = {
    "Centro comercial": [214, 69, 65],
    "Strip center": [232, 140, 74],
    "Oficinas": [64, 120, 192],
    "Industrial": [110, 116, 124],
    "Terreno": [96, 158, 106],
}

# --------------------------------------------------------------------------- #
st.title("Property Portfolio")
st.caption(
    "Every property in the group on one map, with surface area measured from "
    "its own polygon and acquisition value restated for inflation. "
    "**Simplified demo on synthetic data** — production runs in R/Shiny and "
    "Python/Dash against Snowflake."
)

# --------------------------------------------------------------------------- #
with st.sidebar:
    st.header("Filters")
    empresas = st.multiselect("Company", sorted(df["EMPRESA"].unique()))
    estados = st.multiselect("State", sorted(df["ESTADO"].unique()))
    usos = st.multiselect("Use", sorted(df["USO"].unique()))
    solo_activos = st.checkbox("Active only", value=True)
    st.divider()
    st.caption(
        "Synthetic data from `demo/fixtures.py`, fixed seed. Polygons are "
        "invented but built in WGS84 and measured with `pyproj`, so the areas "
        "are real measurements of fake parcels."
    )

d = df.copy()
if empresas:
    d = d[d["EMPRESA"].isin(empresas)]
if estados:
    d = d[d["ESTADO"].isin(estados)]
if usos:
    d = d[d["USO"].isin(usos)]
if solo_activos:
    d = d[d["STATUS_ACTIVIDAD"] == "Activo"]

# --------------------------------------------------------------------------- #
k1, k2, k3, k4 = st.columns(4)
k1.metric("Properties", len(d))
k2.metric("Land area", f"{d['TERRENO_M2'].sum() / 10_000:,.1f} ha")
k3.metric("Acquisition value", f"${d['PRECIO_ADQUISICION'].sum() / 1e6:,.0f} M")
k4.metric("Restated for inflation",
          f"${d['VALOR_ACTUALIZADO'].sum() / 1e6:,.0f} M",
          delta=f"{(d['VALOR_ACTUALIZADO'].sum() / max(d['PRECIO_ADQUISICION'].sum(), 1) - 1) * 100:,.0f}%")

if d.empty:
    st.warning("No properties match those filters.")
    st.stop()

# --------------------------------------------------------------------------- #
mapa = d.assign(
    color=d["USO"].map(COLORES),
    terreno_txt=d["TERRENO_M2"].map(lambda v: f"{v:,.0f} m²"),
    precio_txt=d["PRECIO_ADQUISICION"].map(lambda v: f"${v:,.0f}"),
    valor_txt=d["VALOR_ACTUALIZADO"].map(
        lambda v: f"${v:,.0f}" if pd.notna(v) else "—"),
)

st.pydeck_chart(pdk.Deck(
    map_style=None,
    initial_view_state=pdk.ViewState(
        latitude=float(d["LATITUD"].mean()),
        longitude=float(d["LONGITUD"].mean()),
        zoom=4.6, pitch=35),
    layers=[
        # El `id` explicito no es cosmetico. Sin el, pydeck le pone a cada capa
        # un UUID nuevo en cada rerun, y como Streamlit re-ejecuta el script
        # completo cada vez que alguien mueve un filtro, deck.gl cree que las
        # capas anteriores desaparecieron: las destruye mientras todavia esta
        # dibujando y truena con "Cannot read properties of null". Con un id
        # estable reconoce que es la misma capa con datos nuevos y solo la
        # actualiza.
        pdk.Layer("PolygonLayer", id="parcelas", data=mapa,
                  get_polygon="POLIGONO", get_fill_color="color",
                  get_line_color=[255, 255, 255], line_width_min_pixels=1,
                  opacity=0.55, pickable=True, stroked=True, filled=True),
        pdk.Layer("ScatterplotLayer", id="centroides", data=mapa,
                  get_position="[LONGITUD, LATITUD]", get_fill_color="color",
                  get_radius=1600, radius_min_pixels=4, pickable=True),
    ],
    tooltip={"html":
             "<b>{ALIAS}</b><br/>{USO}<br/>{MUNICIPIO}, {ESTADO}<br/>"
             "Land: {terreno_txt}<br/>Paid: {precio_txt}<br/>"
             "Today: {valor_txt}"},
), height=520)

leyenda = "  ".join(
    f"<span style='color:rgb({r},{g},{b})'>●</span> {uso}"
    for uso, (r, g, b) in COLORES.items())
st.markdown(f"<div style='font-size:0.85rem'>{leyenda}</div>",
            unsafe_allow_html=True)

st.divider()

# --------------------------------------------------------------------------- #
izq, der = st.columns([3, 2])

with izq:
    st.subheader("Properties")
    st.dataframe(
        d[["ALIAS", "USO", "MUNICIPIO", "ESTADO", "TERRENO_M2",
           "CONSTRUCCION_M2", "PRECIO_ADQUISICION", "VALOR_ACTUALIZADO",
           "FECHA_ESCRITURA", "RATING"]]
        .rename(columns={
            "ALIAS": "Property", "USO": "Use", "MUNICIPIO": "City",
            "ESTADO": "State", "TERRENO_M2": "Land m²",
            "CONSTRUCCION_M2": "Built m²", "PRECIO_ADQUISICION": "Paid",
            "VALOR_ACTUALIZADO": "Restated", "FECHA_ESCRITURA": "Deed date",
            "RATING": "Rating"}),
        hide_index=True, width="stretch", height=380)

with der:
    st.subheader("Occupancy")
    ocupables = locales[locales["ID_INMUEBLE"].isin(d["ID"])]
    if ocupables.empty:
        st.info("No leasable units in the current selection.")
    else:
        ocupados = int((ocupables["ESTATUS"] == "Ocupado").sum())
        st.metric("Units leased",
                  f"{ocupados} / {len(ocupables)}",
                  f"{100 * ocupados / len(ocupables):.1f}%")
        resumen = (ocupables.groupby(["INMUEBLE", "ESTATUS"])
                   .size().unstack(fill_value=0))
        st.bar_chart(resumen, height=300)

st.divider()
st.caption(
    "No real property, company, address, price or tenant appears in this "
    "repository. Generated by `demo/fixtures.py`."
)
