"""Build the ECON 474 RPS state-year panel (1990-2024).

Usage: python build_panel.py [raw_data_dir]   (default: ./raw)
raw_data_dir must contain the raw files listed in SOURCES below.
Outputs rps_panel_1990_2024.csv and rps_panel_1990_2024.xlsx next to this script.
"""
import sys
from pathlib import Path

import numpy as np
import pandas as pd

OUT = Path(__file__).resolve().parent
RAW = Path(sys.argv[1]) if len(sys.argv) > 1 else OUT / "raw"
YEARS = range(1990, 2025)

STATES = ["AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN",
          "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH",
          "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
          "VT", "VA", "WA", "WV", "WI", "WY"]

# ---------------------------------------------------------------- RPS timing
# Enactment year: Berkeley Lab, "U.S. State Electricity Resource Standards: 2026 Data Update",
# slide 9 ("RPS Enactment" timeline). Repeal years: same LBNL deck / targets workbook notes.
RPS_ENACT = {"IA": 1983, "MN": 1994, "AZ": 1996, "NV": 1997, "ME": 1997, "MA": 1997,
             "WI": 1998, "PA": 1998, "CT": 1998, "TX": 1999, "NJ": 1999, "NM": 2000,
             "CA": 2002, "RI": 2004, "HI": 2004, "CO": 2004, "NY": 2004, "MD": 2004,
             "MT": 2005, "DE": 2005, "DC": 2005, "WA": 2006, "OR": 2007, "NC": 2007,
             "NH": 2007, "IL": 2007, "OH": 2008, "MO": 2008, "MI": 2008, "KS": 2009,
             "VT": 2015, "VA": 2020}
# Last year the mandatory RPS was in force (repealed / made voluntary afterwards).
RPS_LAST = {"KS": 2014, "MT": 2020}  # KS made voluntary 2015 (SB 91); MT repealed 2021 (HB 576)

# ---------------------------------------------------------------- RGGI
# RGGI compliance began 1 Jan 2009. NJ withdrew end-2011, rejoined 2020. VA joined 2021,
# exited end-2023. PA never entered compliance (court injunction), so coded 0.
RGGI = {s: (2009, 2024) for s in ["CT", "DE", "ME", "MD", "MA", "NH", "NY", "RI", "VT"]}
RGGI_SPELLS = {s: [v] for s, v in RGGI.items()}
RGGI_SPELLS["NJ"] = [(2009, 2011), (2020, 2024)]
RGGI_SPELLS["VA"] = [(2021, 2023)]

# ---------------------------------------------------------------- Retail-choice restructuring
# Year retail-choice law/commission order adopted -> last year before suspension/repeal.
# Compiled from EIA "Status of State Electricity Industry Restructuring Activity" (2003, 2010)
# and state statutes; see sources sheet. Treat as a confounder control, verify before citing.
RESTRUCT = {"CA": (1996, 2001), "NH": (1996, 2024), "PA": (1996, 2024), "RI": (1996, 2024),
            "NY": (1996, 2024), "MA": (1997, 2024), "ME": (1997, 2024), "IL": (1997, 2024),
            "MT": (1997, 2006), "NV": (1997, 2000), "CT": (1998, 2024), "AZ": (1998, 2004),
            "VA": (1999, 2006), "AR": (1999, 2002), "NM": (1999, 2002), "DE": (1999, 2024),
            "MD": (1999, 2024), "NJ": (1999, 2024), "OH": (1999, 2024), "TX": (1999, 2024),
            "OR": (1999, 2024), "DC": (1999, 2024), "MI": (2000, 2024)}

# NOAA nClimDiv state codes
NOAA = {"AL": 1, "AZ": 2, "AR": 3, "CA": 4, "CO": 5, "CT": 6, "DE": 7, "FL": 8, "GA": 9, "ID": 10,
        "IL": 11, "IN": 12, "IA": 13, "KS": 14, "KY": 15, "LA": 16, "ME": 17, "MD": 18, "MA": 19,
        "MI": 20, "MN": 21, "MS": 22, "MO": 23, "MT": 24, "NE": 25, "NV": 26, "NH": 27, "NJ": 28,
        "NM": 29, "NY": 30, "NC": 31, "ND": 32, "OH": 33, "OK": 34, "OR": 35, "PA": 36, "RI": 37,
        "SC": 38, "SD": 39, "TN": 40, "TX": 41, "UT": 42, "VT": 43, "VA": 44, "WA": 45, "WV": 46,
        "WI": 47, "WY": 48, "HI": 49, "AK": 50}


def panel():
    return pd.MultiIndex.from_product([STATES, YEARS], names=["state", "year"]).to_frame(index=False)


def eia_emissions():
    e = pd.read_excel(RAW / "emission_annual.xlsx", sheet_name=0)
    e = e[(e["Producer Type"] == "Total Electric Power Industry") & (e["Energy Source"] == "All Sources")]
    e = e.rename(columns={"Year": "year", "State": "state"})
    co2col = [c for c in e.columns if c.startswith("CO2")][0]
    return e[["state", "year", co2col]].rename(columns={co2col: "co2_tons"})


def eia_generation():
    g = pd.read_excel(RAW / "annual_generation_state.xls", sheet_name=0, header=1)
    g = g[g["TYPE OF PRODUCER"] == "Total Electric Power Industry"]
    g = g.rename(columns={"YEAR": "year", "STATE": "state", "ENERGY SOURCE": "src",
                          "GENERATION (Megawatthours)": "mwh"})
    g["state"] = g["state"].str.strip().str.upper()
    w = g.pivot_table(index=["state", "year"], columns="src", values="mwh", aggfunc="sum").fillna(0)
    out = pd.DataFrame(index=w.index)
    out["gen_total_mwh"] = w["Total"]
    tot = w["Total"].where(w["Total"] > 0)
    out["coal_share"] = 100 * w["Coal"] / tot
    out["gas_share"] = 100 * w["Natural Gas"] / tot
    out["petroleum_share"] = 100 * w["Petroleum"] / tot
    out["nuclear_share"] = 100 * w["Nuclear"] / tot
    out["hydro_share"] = 100 * w["Hydroelectric Conventional"] / tot
    wind, solar = w["Wind"], w["Solar Thermal and Photovoltaic"]
    bio = w["Wood and Wood Derived Fuels"] + w["Other Biomass"]
    out["wind_solar_share"] = 100 * (wind + solar) / tot
    out["nonhydro_renew_share"] = 100 * (wind + solar + bio + w["Geothermal"]) / tot
    out["renew_share_incl_hydro"] = out["nonhydro_renew_share"] + out["hydro_share"]
    return out.reset_index()


def degree_days():
    rows = []
    for elem, fname in [("hdd", "climdiv-hddcst-v1.0.0-20261105"), ("cdd", "climdiv-cddcst-v1.0.0-20261105"),
                        ("precip_in", "climdiv-pcpnst-v1.0.0-20261105")]:
        for line in open(RAW / fname):
            code, year = int(line[0:3]), int(line[6:10])
            vals = [float(line[10 + 7 * i: 17 + 7 * i]) for i in range(12)]
            if any(v <= -9.99 for v in vals):  # missing codes: -9999 (DD), -9.99 (precip)
                continue
            rows.append((code, year, elem, sum(vals)))
    d = pd.DataFrame(rows, columns=["code", "year", "elem", "val"])
    d = d.pivot_table(index=["code", "year"], columns="elem", values="val").reset_index()
    inv = {v: k for k, v in NOAA.items()}
    d["state"] = d["code"].map(inv)
    d = d.dropna(subset=["state"])
    md = d[d.state == "MD"].copy()
    md["state"] = "DC"  # DC has no NOAA series; Maryland used as proxy (flagged)
    d = pd.concat([d, md])
    return d[["state", "year", "hdd", "cdd", "precip_in"]]


def rps_targets():
    v = pd.read_excel(RAW / "rps_targets.xlsx", sheet_name="RPS & CES Targets (%)", header=24)
    v["State"] = v["State"].ffill()
    yrs = [c for c in v.columns if isinstance(c, int)]
    tier = v["RPS Tier or Carve Out"].astype(str)
    tot = v[tier.str.startswith("Total RPS")]
    long = tot.melt(id_vars=["State"], value_vars=yrs, var_name="year", value_name="rps_target_pct")
    long["rps_target_pct"] = 100 * long["rps_target_pct"].astype(float)
    long = long.rename(columns={"State": "state"})
    # first compliance year = first year any RPS tier (not CES) has a positive requirement
    # rows belong to the block opened by the last "Total RPS"/"Total CES" header row
    block, blocks = None, []
    for t in tier:
        if t.startswith("Total RPS"):
            block = "RPS"
        elif t.startswith("Total CES"):
            block = "CES"
        blocks.append(block if t != "nan" else None)
    rps_rows = v[pd.Series(blocks, index=v.index).eq("RPS") & v["State"].isin(list(RPS_ENACT))]
    first = {}
    for st, grp in rps_rows.groupby("State"):
        pos = [y for y in yrs if (grp[y].astype(float) > 0).any()]
        if pos:
            first[st] = pos[0]
    return long, first


def main():
    p = panel()
    p = p.merge(eia_emissions(), on=["state", "year"], how="left")
    p = p.merge(eia_generation(), on=["state", "year"], how="left")
    p["co2_per_mwh"] = p["co2_tons"] / p["gen_total_mwh"].where(p["gen_total_mwh"] > 0)
    p["ln_co2_per_mwh"] = np.log(p["co2_per_mwh"].where(p["co2_per_mwh"] > 0))

    tg, first = rps_targets()
    p["rps_enact_year"] = p.state.map(RPS_ENACT)
    p["rps_first_compliance_year"] = p.state.map(first)
    p["rps_repeal_year"] = p.state.map({k: v + 1 for k, v in RPS_LAST.items()})
    last = p.state.map(RPS_LAST).fillna(9999)
    p["rps_enacted"] = ((p.year >= p.rps_enact_year) & (p.year <= last)).astype(int)
    p["rps_binding"] = ((p.year >= p.rps_first_compliance_year) & (p.year <= last)).astype(int)
    p["rps_ever"] = p.rps_enact_year.notna().astype(int)
    p["cohort_g"] = p.rps_enact_year.where(p.rps_enact_year >= 1991, other=np.nan)
    p.loc[p.rps_ever == 0, "cohort_g"] = 0  # CS-DiD convention: 0 = never treated
    p["event_time"] = p.year - p.rps_enact_year

    p = p.merge(tg, on=["state", "year"], how="left")
    # LBNL targets cover 2000+. Never-RPS states and pre-enactment years are 0; RPS states in
    # 1990-1999 after enactment are left missing (LBNL does not report pre-2000 targets).
    p.loc[p.rps_ever == 0, "rps_target_pct"] = 0.0
    p.loc[(p.rps_ever == 1) & (p.year < p.rps_enact_year), "rps_target_pct"] = 0.0
    p.loc[(p.year >= 2000) & p.rps_target_pct.isna(), "rps_target_pct"] = 0.0
    # No state other than Iowa had a positive requirement before 2000 (all first compliance
    # years are >= 2000), so 1990-1999 targets are 0. Iowa's legacy MW mandate stays missing.
    p.loc[(p.year < 2000) & (p.state != "IA"), "rps_target_pct"] = 0.0

    # Cohort defined by first compliance year (robustness timing); 0 = never treated, blank = IA
    p["cohort_binding"] = p.rps_first_compliance_year.where(p.rps_ever == 1, 0)
    p.loc[p.state == "IA", "cohort_binding"] = np.nan

    p["rggi"] = [int(any(a <= y <= b for a, b in RGGI_SPELLS.get(s, []))) for s, y in zip(p.state, p.year)]
    p["restructured"] = [int(s in RESTRUCT and RESTRUCT[s][0] <= y <= RESTRUCT[s][1])
                         for s, y in zip(p.state, p.year)]

    p = p.merge(degree_days(), on=["state", "year"], how="left")
    p["dd_proxy_flag"] = (p.state == "DC").astype(int)

    # Pre-treatment baseline generation mix (fixed at 1990, so it cannot be affected by any RPS)
    base = p.loc[p.year == 1990, ["state", "coal_share", "gas_share"]].rename(
        columns={"coal_share": "coal_share_1990", "gas_share": "gas_share_1990"})
    p = p.merge(base, on="state", how="left")

    p["state_id"] = p.state.map({s: i + 1 for i, s in enumerate(sorted(STATES))})
    p["flag_outlier"] = p.state.isin(["VT", "DC"]).astype(int)
    p["flag_reversal"] = p.state.isin(["KS", "MT"]).astype(int)
    p["sample_main"] = 1
    p.loc[p.state == "IA", "sample_main"] = 0  # always treated over 1990-2024
    p["sample_preferred"] = ((p.sample_main == 1) & (p.flag_outlier == 0) & (p.flag_reversal == 0)).astype(int)
    p["note"] = ""
    p.loc[p.state == "IA", "note"] = "RPS enacted 1983: no pre-period; drop from main sample"
    p.loc[p.state == "KS", "note"] = "RPS made voluntary 2015 (treatment switches off)"
    p.loc[p.state == "MT", "note"] = "RPS repealed 2021 (treatment switches off)"
    p.loc[p.state == "DC", "note"] = "Weather vars use Maryland as proxy; tiny generation base (outlier)"
    p.loc[p.state.isin(["AK", "HI"]), "note"] = "No NOAA degree-day series (HDD/CDD missing)"
    p.loc[p.state == "VT", "note"] = "Almost no fossil generation: CO2/MWh near zero every year, so ln values are extreme outliers"

    cols = ["state", "state_id", "year", "ln_co2_per_mwh", "co2_per_mwh", "co2_tons", "gen_total_mwh",
            "rps_enacted", "rps_binding", "rps_ever", "rps_enact_year", "rps_first_compliance_year",
            "rps_repeal_year", "cohort_g", "cohort_binding", "event_time", "rps_target_pct", "rggi",
            "restructured", "hdd", "cdd", "precip_in", "dd_proxy_flag", "coal_share", "gas_share",
            "petroleum_share", "nuclear_share", "hydro_share", "wind_solar_share",
            "nonhydro_renew_share", "renew_share_incl_hydro", "coal_share_1990", "gas_share_1990",
            "flag_outlier", "flag_reversal", "sample_main", "sample_preferred", "note"]
    p = p[cols].sort_values(["state", "year"])
    num = p.select_dtypes("float").columns
    p[num] = p[num].round(5)
    p.to_csv(OUT / "rps_panel_1990_2024.csv", index=False)
    return p


CODEBOOK = [
    ("state", "ID", "Two-letter postal code (50 states + DC)", ""),
    ("state_id", "ID", "Numeric state identifier (1-51, alphabetical by postal code) for DiD packages", ""),
    ("year", "ID", "Calendar year, 1990-2024", ""),
    ("ln_co2_per_mwh", "Outcome", "ln(co2_per_mwh)", "log metric tons/MWh"),
    ("co2_per_mwh", "Outcome (levels)", "CO2 from all electric-power-industry generators / total net generation", "metric tons CO2/MWh"),
    ("co2_tons", "Building block", "CO2 emissions, Total Electric Power Industry, all sources (EIA)", "metric tons"),
    ("gen_total_mwh", "Building block", "Net generation, Total Electric Power Industry, all sources (EIA)", "MWh"),
    ("rps_enacted", "Treatment (main)", "1 if year >= RPS enactment year and RPS not repealed", "0/1"),
    ("rps_binding", "Treatment (alt.)", "1 if year >= first year with a positive LBNL compliance target and not repealed", "0/1"),
    ("rps_ever", "Group", "1 if state ever enacted a mandatory RPS", "0/1"),
    ("rps_enact_year", "Timing", "Year RPS was first enacted (LBNL timeline)", "year"),
    ("rps_first_compliance_year", "Timing", "First year LBNL lists a positive RPS requirement (any tier); LBNL series starts in 2000", "year"),
    ("rps_repeal_year", "Timing", "Year RPS was repealed / made voluntary (KS 2015, MT 2021)", "year"),
    ("cohort_g", "DiD", "Adoption cohort for staggered DiD (= rps_enact_year; 0 = never treated; blank = IA, always treated)", "year"),
    ("cohort_binding", "DiD (robustness)", "Adoption cohort by first compliance year (= rps_first_compliance_year; 0 = never treated; blank = IA)", "year"),
    ("event_time", "DiD", "year - rps_enact_year", "years"),
    ("rps_target_pct", "Policy intensity", "Total nominal RPS requirement, % of RPS-applicable retail sales (LBNL, 2000+). 0 before 2000 (no state but IA had a positive requirement), before enactment, and for never-RPS states; blank for IA 1990-1999", "%"),
    ("rggi", "Confounder", "1 if state is in RGGI compliance that year", "0/1"),
    ("restructured", "Confounder", "1 if retail-choice restructuring law/order in force (enactment through suspension)", "0/1"),
    ("hdd", "Confounder", "Annual population-weighted heating degree days (sum of monthly NOAA nClimDiv)", "degree days (F)"),
    ("cdd", "Confounder", "Annual population-weighted cooling degree days (sum of monthly NOAA nClimDiv)", "degree days (F)"),
    ("precip_in", "Confounder (added)", "Annual statewide precipitation (sum of monthly NOAA nClimDiv); proxy for hydro water years", "inches"),
    ("dd_proxy_flag", "Flag", "1 if weather variables are a proxy (DC uses Maryland)", "0/1"),
    ("coal_share", "Mechanism / covariate", "Coal net generation / total net generation", "%"),
    ("gas_share", "Mechanism / covariate", "Natural gas net generation / total", "%"),
    ("petroleum_share", "Covariate", "Petroleum net generation / total", "%"),
    ("nuclear_share", "Covariate (added)", "Nuclear net generation / total", "%"),
    ("hydro_share", "Covariate (added)", "Conventional hydro net generation / total", "%"),
    ("wind_solar_share", "Mechanism (added)", "Wind + solar net generation / total", "%"),
    ("nonhydro_renew_share", "Mechanism", "Wind + solar + wood + other biomass + geothermal / total", "%"),
    ("renew_share_incl_hydro", "Mechanism", "nonhydro_renew_share + hydro_share", "%"),
    ("coal_share_1990", "Baseline covariate", "Coal share of generation in 1990 (pre-treatment, time-invariant; safe for conditional parallel trends)", "%"),
    ("gas_share_1990", "Baseline covariate", "Natural gas share of generation in 1990 (pre-treatment, time-invariant)", "%"),
    ("flag_outlier", "Flag", "1 for VT (near-zero fossil generation) and DC (tiny generation base)", "0/1"),
    ("flag_reversal", "Flag", "1 for KS and MT (RPS repealed/made voluntary; treatment switches off)", "0/1"),
    ("sample_main", "Sample", "1 if in main DiD sample (drops IA)", "0/1"),
    ("sample_preferred", "Sample", "sample_main excluding VT, DC, KS, MT (recommended main sample)", "0/1"),
    ("note", "Flag", "State-specific caveats", ""),
]

SOURCES = [
    ("CO2 emissions", "U.S. EIA, State Electricity Profiles: Emissions by state, producer type and energy source, 1990-2024 (emission_annual.xlsx)", "https://www.eia.gov/electricity/data/state/emission_annual.xlsx"),
    ("Net generation & fuel shares", "U.S. EIA, Net Generation by State by Type of Producer by Energy Source, 1990-2024 (annual_generation_state.xls, EIA-906/920/923)", "https://www.eia.gov/electricity/data/state/annual_generation_state.xls"),
    ("RPS targets", "Berkeley Lab (Barbose), RPS & CES Targets and Demand workbook, June 2026", "https://emp.lbl.gov/sites/default/files/2026-06/RPS%20%26%20CES%20Targets%20and%20Demand_June%202026.xlsx"),
    ("RPS enactment years", "Berkeley Lab, U.S. State Electricity Resource Standards: 2026 Data Update, slide 9 (RPS enactment timeline)", "https://emp.lbl.gov/sites/default/files/2026-06/State%20Electricity%20Resource%20Standards-2026%20Data%20Update.pdf"),
    ("RPS overview", "Berkeley Lab, State Electricity Resource Standards project page", "https://emp.lbl.gov/projects/renewables-portfolio"),
    ("Degree days", "NOAA NCEI nClimDiv statewide monthly heating/cooling degree days (climdiv-hddcst, climdiv-cddcst; population weights from 2010 Census)", "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/"),
    ("Precipitation", "NOAA NCEI nClimDiv statewide monthly precipitation (climdiv-pcpnst)", "https://www.ncei.noaa.gov/pub/data/cirs/climdiv/"),
    ("RGGI participation", "Regional Greenhouse Gas Initiative, program history (compliance start 2009; NJ 2009-2011 & 2020-; VA 2021-2023)", "https://www.rggi.org/program-overview-and-design/elements"),
    ("Restructuring", "U.S. EIA, Status of State Electricity Industry Restructuring Activity (archived), plus state statutes; Utah Legislature staff comparison table", "https://webarchive.library.unt.edu/eot2008/20090102194315/http:/www.eia.doe.gov/cneaf/electricity/page/restructuring/elect_restruct3bs_Pnw_d.html"),
    ("Restructuring (cross-check)", "Utah Legislature, comparison of state restructuring legislation (2017)", "https://le.utah.gov/interim/2017/pdf/00002992.pdf"),
]


def write_xlsx(df):
    cohorts = (df[df.rps_ever == 1].groupby("state")
               .agg(rps_enact_year=("rps_enact_year", "first"),
                    rps_first_compliance_year=("rps_first_compliance_year", "first"),
                    rps_repeal_year=("rps_repeal_year", "first"),
                    in_main_sample=("sample_main", "first"),
                    target_2024_pct=("rps_target_pct", "last"))
               .reset_index().sort_values(["rps_enact_year", "state"]))
    controls = sorted(df.loc[df.rps_ever == 0, "state"].unique())
    with pd.ExcelWriter(OUT / "rps_panel_1990_2024.xlsx") as xw:
        df.to_excel(xw, sheet_name="panel", index=False)
        pd.DataFrame(CODEBOOK, columns=["variable", "role", "definition", "unit"]).to_excel(xw, sheet_name="codebook", index=False)
        cohorts.to_excel(xw, sheet_name="rps_states", index=False)
        pd.DataFrame({"never_treated_controls": controls}).to_excel(xw, sheet_name="controls", index=False)
        pd.DataFrame(SOURCES, columns=["data", "source", "url"]).to_excel(xw, sheet_name="sources", index=False)


if __name__ == "__main__":
    df = main()
    write_xlsx(df)
    print(df.shape)
    print(df.isna().sum()[lambda s: s > 0])
