# Cajas verdes to the ICES VMS & Logbook Data Call

Migration of Andalusian small-scale fleet data — **cajas verdes** (green-box
tracking devices) and **first-sale notes** — into the formats of the ICES VMS
and Logbook Data Call, as a test of a (hypothetical) data call for small-scale
fisheries (SSF).

The small-scale fleet has no EU logbook and no VMS. Its activity is recorded by
the cajas verdes (one position every ~3 minutes) and its landings by the sales
notes (kg and EUR by species, vessel and sale date). This repository adds a
**primary conversion script** that transforms and adapts both sources to the
TACSAT and EFLALO exchange formats, and then runs them through the official ICES
workflow to produce the two submission tables (Annex 1, VMS / VE, and Annex 2,
logbook / LE).

First run: fleets **FPO** (pots and traps) and **HMD** (hydraulic dredges),
years **2023–2024**, ICES division **27.9.a** (Gulf of Cádiz).

## How it works

```
cajas verdes (.rds) ─┐
                     ├─► 0b_cajasverdes_notas_formato.R ─► TACSAT + EFLALO ─► ICES workflow (0 → 1 → 2 → 3) ─► Annex 1 (VE) + Annex 2 (LE)
sales notes (.xlsx) ─┘        primary conversion code
```

### 1. Primary conversion: `0b_cajasverdes_notas_formato.R`

Reads the two sources separately, links them and writes `Data/tacsat_YYYY.RData`
and `Data/eflalo_YYYY.RData` in the format the ICES workflow expects. For each
step it takes the best source available and records which one it used (its
*basis*), following the logic of *One flow for effort, pressure and catch at
any data resolution* (E. Hjörleifsson, 2026):

| Step | Source, best first → fallback |
|---|---|
| Trip | vessel + calendar day of the tracking data |
| Gear | gear in the sales notes of the trip → vessel's main gear in its notes of the month → of the year |
| Catch | sales notes of the same day → of the next day → none (trip kept with effort only) |
| Metier level 6 | main species group in the notes → default of the gear |
| Vessel length, kW, GT | EU Fleet Register, by CFR |

It also converts local time to UTC, assigns ICES rectangle and division,
excludes Mediterranean trips (east of 5°36'W) and keeps the catch **by FAO
species**. The basis of every trip is written to `Data/cv_nv_viajes_YYYY.csv`.

### 2. ICES workflow (adapted)

The official scripts of
[ices-eg/ICES-VMS-and-Logbook-Data-Call](https://github.com/ices-eg/ICES-VMS-and-Logbook-Data-Call)
(v2.1.0, commit `a844911`), with a few minimal changes, each marked in the code
with `[cajas verdes: ...]` and the original line kept as a comment:

| Script | Main changes |
|---|---|
| `0_global.R` | `intThres` 5 → 1 min (pings every ~3 min); years 2023–2024; local fallback for the BENTHIS gear-width API; longer download timeout |
| `1_eflalo_tacsat_preprocessing.R` | `dplyr::select()` namespace |
| `2_eflalo_tacsat_analysis.R` | speed ranges per gear for gears outside the automatic detection (FPO 0.1–3 kn); **bug fix**: the speed rule for these gears never ran in v2.1.0; `drop = FALSE` for single-species EFLALO |
| `3_data_submission.R` | country code `ESP` → `ES` before the DATSU check; NA summary fix |

HMD keeps the standard automatic detection of fishing activity.

## Run order

From the project root:

```r
source("0b_cajasverdes_notas_formato.R")   # cajas verdes + sales notes -> TACSAT / EFLALO (needs ~10-12 GB RAM)
source("0_global.R")                       # settings, packages, habitat and depth layers
source("1_eflalo_tacsat_preprocessing.R")  # cleaning
source("2_eflalo_tacsat_analysis.R")       # trips, fishing activity, effort, landings per ping
source("3_data_submission.R")              # Annex 1 (VE) and Annex 2 (LE) in Results/
```

`0_cajasverdes_formato.R` is version 1 (pre-merged source file); do not run it
before the workflow, as it overwrites the TACSAT and EFLALO files.

## Documentation

- `d_CajasVerdes_report.Rmd` — full report: source data, source hierarchy,
  format comparison, assumptions, issues found, every change to the ICES code,
  results of each step and a worked example of one ping through the workflow.
- `a_Data preparation and submission.md`, `b_EFLALO & TACSAT Formats.md` —
  ICES documentation of the data call formats.

## Data

**No data are included.** `Data/` and `Results/` are excluded by `.gitignore`:
the source files contain vessel identifiers and commercial sales and are not
public. To run the code, place in `Data/`:

- `CV_2017_2024_ZE_fmt.rds` — cajas verdes positions
- `notas de venta/… NV YYYY.xlsx` — sales notes by species
- `EU_fleet_register_ESP.csv` — EU Fleet Register export for Spain

## Credits and licence

ICES workflow: ICES and WGSFD (GPL-3, see `LICENSE`; original copy described in
`ORIGEN.txt`). Conversion code, adaptations and report: Mauricio Mardones
(IEO-CSIC, Centro Oceanográfico de Cádiz).
