#'------------------------------------------------------------------------------
#
# Cajas verdes (CV_2017_2024_ZE_fmt.rds) + notas de venta por especie
#   -> TACSAT / EFLALO para el workflow ICES VMS & Logbook Data Call (v2.1.0)
#
# Version 2 de 0_cajasverdes_formato.R. Las dos fuentes se cruzan aqui (antes
# venian ya fusionadas en CV_NV_2018_2024_ZE.fst) y cada viaje guarda de que
# fuente salio cada variable (columnas *_basis), siguiendo la logica de
# "One flow for effort, pressure and catch at any data resolution"
# (E. Hjorleifsson, 2026): cada paso toma la mejor fuente disponible y, si
# falta, pasa a la siguiente; lo que no tiene fuente queda NA con basis "none".
#
# Decisiones (2026-10-08):
#   viaje    = buque + dia calendario (FT_REF del .rds)                [vessel_day]
#   arte     = nota de venta del viaje > arte dominante (kg) del buque en
#              sus notas del mes > del anio > none
#   captura  = nota del mismo dia > nota del dia siguiente (si ese dia no hay
#              viaje con caja verde) > NA (viaje solo con esfuerzo)
#   metier6  = grupo de especies dominante en la nota > por defecto del arte
#
# Seleccion: FPO (nasas + trampas) y HMD (dragas hidraulicas), 2023-2024,
#            division 27.9.a (se excluye lo que esta al E de 5 36' W)
#
# Correr desde la raiz del proyecto ANTES de 0_global.R / 1_... :
#   source("0b_cajasverdes_notas_formato.R")
# Lee el .rds completo (70 M filas): necesita ~10-12 GB de RAM.
#
# Salida en Data/:
#   tacsat_YYYY.RData, eflalo_YYYY.RData  (mismo nombre que la v1; la primera
#       vez los de la v1 se copian a Data/respaldo_v1_fst/)
#   cv_nv_viajes_YYYY.csv        un registro por viaje seleccionado con sus bases
#   cv_nv_notas_sin_track_YYYY.csv  notas FPO/HMD de buques con caja verde
#                                   que no encuentran viaje
#   cv_nv_notas_viajes_YYYY.csv  notas de venta de los buques seleccionados (para el reporte)
#   cv_nv_resumen.csv            resumen por anio
#'------------------------------------------------------------------------------
library(data.table)
library(readxl)

dataPath  <- "Data/"
cvFile    <- file.path(dataPath, "CV_2017_2024_ZE_fmt.rds")
nvDir     <- file.path(dataPath, "notas de venta")
fleetFile <- file.path(dataPath, "EU_fleet_register_ESP.csv")
years     <- 2023:2024
gears     <- c("FPO", "HMD")
lonMed    <- -5.6     # limite FAO 27 / 37 (Estrecho de Gibraltar)
minShare  <- 0.001    # especies con < 0,1 % del kg anual de FPO+HMD se agrupan en MZZ
catchNA   <- TRUE     # viajes sin nota: LE_KG_* / LE_EURO_* = NA. Si el paso 2
                      # del workflow falla por los NA, poner FALSE (quedan en 0;
                      # la base "none" sigue en cv_nv_viajes_YYYY.csv)

#'------------------------------------------------------------------------------
# Funciones
#'------------------------------------------------------------------------------
ices_rect <- function(lat, lon) {
  lonL <- c("A","B","C","D","E","F","G","H","J","K","L","M")
  paste0(sprintf("%02d", floor((lat - 36) * 2) + 1),
         lonL[floor((lon + 50) / 10) + 1],
         floor(lon + 50) %% 10)
}

mode1 <- function(x) { x <- x[!is.na(x)]; if (!length(x)) NA_character_ else names(which.max(table(x))) }

# Arte de la nota de venta -> codigo de arte (metier nivel 4).
# Patrones sin tildes para no depender de la codificacion.
arte2gear <- function(a) {
  a <- toupper(a)
  fcase(grepl("NASA|TRAMPA", a),        "FPO",
        grepl("DRAGA", a),              "HMD",
        grepl("RASTRA", a),             "DRB",
        grepl("GEMELAS", a),            "OTT",
        grepl("ARRASTRE PEL", a),       "OTM",
        grepl("ARRASTRE", a),           "OTB",
        grepl("COMBINADA", a),          "GTN",
        grepl("TRASMALLO", a),          "GTR",
        grepl("ENMALLE DE FONDO", a),   "GNS",
        grepl("ENMALLE", a),            "GN",
        grepl("CERCO", a),              "PS",
        grepl("PALANGRES CALADOS", a),  "LLS",
        grepl("DERIVA", a),             "LLD",
        grepl("PALANGRE", a),           "LL",
        grepl("MECANIZADAS", a),        "LHM",
        grepl("MANUALES", a),           "LHP",
        grepl("ANZUELO", a),            "LX",
        grepl("CURRICAN", a),           "LTL",
        default = "MIS")
}

# Especie FAO -> grupo de especies objetivo del metier (DCF)
asm <- function(fao) fcase(
  fao %in% c("OCC","OCT","OCM","EOI","OMZ","CTC","CTL","CTR","SQM","SQZ",
             "SQR","SQC","OFJ","EDT","IAX","SQI","OUL"), "CEP",
  fao %in% c("SVE","DXL","CLV","VEV","VNR","VSC","CTG","MUS","MSM","OYF",
             "OYG","SCE","QSC","CLB","RAZ","EJE","GKL","SLC","MUX","GAS",
             "BIV","MOL","CRA","RPW","DON","CTS","KLK","HKC","PIC","CLJ"), "MOL",
  fao %in% c("TGS","DPS","ARA","ARS","NEP","CGE","LBE","SLO","PEN","CRB",
             "CRE","CRS","CRU","PRA","CSH","SCR","KCS","SPC","ERS","SWM"), "CRU",
  default = "DEF")

met6 <- function(gear, a) fcase(
  gear == "HMD", "HMD_MOL_>0_0_0",
  gear == "FPO" & a %in% c("CEP","MOL","CRU","DEF"), paste0("FPO_", a, "_>0_0_0"),
  gear == "FPO", "FPO_CEP_>0_0_0",
  default = NA_character_)

read_fleet <- function(f, year) {
  fl <- fread(f, sep = "auto", encoding = "UTF-8", colClasses = "character")
  setnames(fl, tolower(gsub("[^A-Za-z]", "", names(fl))))
  if (all(c("veref", "velen") %in% names(fl))) {
    out <- fl[, .(VE_REF = veref, VE_LEN = velen, VE_KW = vekw, VE_TON = veton)]
  } else {
    num <- function(x) as.numeric(gsub(",", ".", x))
    dte <- function(x) as.IDate(substr(x, 1, 10), tryFormats = c("%Y-%m-%d", "%d/%m/%Y", "%Y%m%d"))
    fl[, `:=`(s = dte(eventstartdate), e = dte(eventenddate))]
    fl[is.na(e), e := as.IDate("2100-01-01")]
    vig <- fl[s <= as.IDate(paste0(year, "-12-31")) & e >= as.IDate(paste0(year, "-01-01"))]
    vig <- rbind(vig, fl[!cfr %in% vig$cfr])
    setorder(vig, cfr, -s)
    vig <- vig[, .SD[1], by = cfr]
    out <- vig[, .(VE_REF = cfr, VE_LEN = num(loa), VE_KW = num(powermain), VE_TON = num(tongt))]
  }
  out[, `:=`(VE_LEN = as.numeric(VE_LEN), VE_KW = as.numeric(VE_KW), VE_TON = as.numeric(VE_TON))]
  unique(out, by = "VE_REF")
}

read_nv <- function(yr) {
  f <- list.files(nvDir, pattern = paste0("NV ", yr, "\\.xlsx$"), full.names = TRUE)
  if (!length(f)) stop("No encuentro la nota de venta de ", yr, " en ", nvDir)
  nv <- setDT(read_excel(f[length(f)]))
  nv <- nv[, .(VE_REF = as.character(ID_UDPROD), FECHA = as.IDate(FECHAVENTA),
               FAO = as.character(FAO), ESPECIE = as.character(NOMBRE_ESPECIE),
               ARTE = as.character(ARTE),
               KG = as.numeric(TOTAL_KILOS), EUR = as.numeric(TOTAL_EUROS))]
  nv[, GEAR := arte2gear(ARTE)]
  nv[!is.na(FECHA) & !is.na(KG)]
}

#'------------------------------------------------------------------------------
# 0. Respaldo de la v1 y lectura de cajas verdes
#'------------------------------------------------------------------------------
bk <- file.path(dataPath, "respaldo_v1_fst")
if (!dir.exists(bk)) {
  dir.create(bk)
  old <- file.path(dataPath, c(paste0(rep(c("tacsat_", "eflalo_"), each = length(years)), years, ".RData"),
                               "cajasverdes_resumen_formato.csv"))
  file.copy(old[file.exists(old)], bk)
  message("Archivos de la v1 copiados a ", bk)
}

message("Leyendo ", cvFile, " ...")
cv <- readRDS(cvFile)
setDT(cv)
cv <- cv[ANYO %in% years,
         .(VE_REF, FT_REF, ANYO, SI_TIMESTAMP, SI_LATI, SI_LONG, SI_SP, SI_HE, SI_TDIFF, EN_PUERTO)]
gc()
message("Pings ", paste(years, collapse = "-"), ": ", format(nrow(cv), big.mark = "."))

# Control de zona horaria: FT_REF debe ser el dia local (Europe/Madrid) del ping
chk <- cv[sample(.N, min(.N, 2e5)),
          mean(format(SI_TIMESTAMP, "%Y_%m_%d", tz = "Europe/Madrid") == sub("^[^_]+_", "", FT_REF))]
message(sprintf("Pings cuyo dia local (Madrid) coincide con el dia de FT_REF: %.1f%%", 100 * chk))
if (chk < 0.95) warning("FT_REF no coincide con el dia local de SI_TIMESTAMP: revisar zona horaria del .rds")

# El instante es correcto; TACSAT/EFLALO van en UTC, resolucion de minutos
cv[, SI_UTC := as.POSIXct(floor(as.numeric(SI_TIMESTAMP) / 60) * 60, origin = "1970-01-01", tz = "UTC")]
setorder(cv, VE_REF, SI_UTC)
cv <- unique(cv, by = c("VE_REF", "SI_UTC"))                  # 1 ping por buque-minuto

#'------------------------------------------------------------------------------
# 1. Por anio
#'------------------------------------------------------------------------------
resumen <- list()

for (yr in years) {
  message("\n==== ", yr, " ====")
  y  <- cv[ANYO == yr]
  nv <- read_nv(yr)

  # ---- 1.1 Viajes: buque + dia (FT_REF) --------------------------------------
  tr <- y[, {
    mar <- EN_PUERTO == 0
    ok  <- mar & SI_LONG <= lonMed
    .(TDATE   = as.IDate(sub("^[^_]+_", "", FT_REF[1]), format = "%Y_%m_%d"),
      d = min(SI_UTC), l = max(SI_UTC),
      n_pings = .N, n_mar = sum(mar),
      pMed    = if (any(mar)) mean(SI_LONG[mar] > lonMed) else NA_real_,
      LE_RECT = if (any(ok)) mode1(ices_rect(SI_LATI[ok], SI_LONG[ok])) else NA_character_)
  }, by = .(FT_REF, VE_REF)]
  tr[, trip_basis := fifelse(n_mar > 0, "vessel_day", "in_port_only")]
  tr[, LE_DIV := fifelse(pMed > 0.5, "37.1.1", "27.9.a")]
  ts <- tr[n_mar > 0, .(VE_REF, FT_REF, TDATE)]

  # ---- 1.2 Notas -> viaje: mismo dia > dia siguiente --------------------------
  nd <- nv[, .(KG = sum(KG), EUR = sum(EUR)), by = .(VE_REF, FECHA)]   # buque-dia de venta
  nd[ts, on = .(VE_REF, FECHA = TDATE), FT_REF0 := i.FT_REF]
  nd[, FECHA1 := FECHA - 1L]
  nd[ts, on = .(VE_REF, FECHA1 = TDATE), FT_REF1 := i.FT_REF]
  nd[, `:=`(FT_REF = fcoalesce(FT_REF0, FT_REF1),
            lag    = fifelse(!is.na(FT_REF0), 0L, fifelse(!is.na(FT_REF1), 1L, NA_integer_)))]
  nvT <- nd[, .(VE_REF, FECHA, FT_REF, lag)][nv, on = .(VE_REF, FECHA)]
  nvT[, ASM := asm(FAO)]

  # por viaje: base de captura, arte y grupo de especies de la nota
  cb <- nvT[!is.na(FT_REF), .(catch_basis = fcase(all(lag == 0L), "landing_same_day",
                                                  all(lag == 1L), "landing_next_day",
                                                  default = "landing_same_and_next_day"),
                              KG_nota = sum(KG), EUR_nota = sum(EUR)), by = FT_REF]
  gn <- nvT[!is.na(FT_REF), .(k = sum(KG)), by = .(FT_REF, GEAR)][order(FT_REF, -k)][
            , .(gear_note = GEAR[1], n_gear_note = .N), by = FT_REF]
  an <- nvT[!is.na(FT_REF), .(k = sum(KG)), by = .(FT_REF, ASM)][order(FT_REF, -k)][
            , .(asm_note = ASM[1]), by = FT_REF]

  # arte dominante del buque (todas sus notas) en el mes y en el anio
  gm <- nv[, .(k = sum(KG)), by = .(VE_REF, M = month(FECHA), GEAR)][order(VE_REF, M, -k)][
           , .(gear_month = GEAR[1]), by = .(VE_REF, M)]
  gy <- nv[, .(k = sum(KG)), by = .(VE_REF, GEAR)][order(VE_REF, -k)][
           , .(gear_year = GEAR[1]), by = VE_REF]

  tr[, M := month(TDATE)]
  tr <- merge(tr, cb, by = "FT_REF", all.x = TRUE)
  tr <- merge(tr, gn, by = "FT_REF", all.x = TRUE)
  tr <- merge(tr, an, by = "FT_REF", all.x = TRUE)
  tr <- merge(tr, gm, by = c("VE_REF", "M"), all.x = TRUE)
  tr <- merge(tr, gy, by = "VE_REF", all.x = TRUE)

  # ---- 1.3 Arte, captura y metier con su base ---------------------------------
  tr[, LE_GEAR := fcoalesce(gear_note, gear_month, gear_year)]
  tr[, gear_basis := fcase(!is.na(gear_note),  "landing",
                           !is.na(gear_month), "dominant_month",
                           !is.na(gear_year),  "dominant_year",
                           default = "none")]
  tr[is.na(catch_basis), catch_basis := "none"]
  tr[, LE_MET := met6(LE_GEAR, asm_note)]
  tr[, metier_basis := fifelse(!is.na(asm_note) & LE_GEAR %in% gears, "landing_species", "gear_default")]

  nTrips <- tr[trip_basis == "vessel_day", .N]
  sel <- tr[trip_basis == "vessel_day" & LE_GEAR %in% gears]
  nMed <- sel[LE_DIV != "27.9.a", .N]
  sel <- sel[LE_DIV == "27.9.a"]

  # ---- 1.4 Captura por especie ------------------------------------------------
  spT  <- nvT[FT_REF %in% sel$FT_REF]
  shr  <- spT[, .(k = sum(KG)), by = FAO][, s := k / sum(k)]
  keep <- shr[s >= minShare, FAO]
  spT[, FAO2 := fifelse(FAO %in% keep, FAO, "MZZ")]
  sp <- spT[, .(KG = sum(KG), EUR = sum(EUR)), by = .(FT_REF, FAO2)]
  wk <- dcast(sp, FT_REF ~ FAO2, value.var = c("KG", "EUR"), fill = 0)
  setnames(wk, sub("^KG_", "LE_KG_", sub("^EUR_", "LE_EURO_", names(wk))))
  spCols <- setdiff(names(wk), "FT_REF")

  # ---- 1.5 EFLALO: un registro por viaje --------------------------------------
  ef <- merge(sel, wk, by = "FT_REF", all.x = TRUE)
  if (!catchNA) for (cc in spCols) set(ef, which(is.na(ef[[cc]])), cc, 0)
  ef[, `:=`(VE_FLT = LE_GEAR, VE_COU = "ESP",
            FT_DCOU = "ESP", FT_DHAR = NA_character_,
            FT_DDAT = format(d, "%d/%m/%Y"), FT_DTIME = format(d, "%H:%M"),
            FT_LCOU = "ESP", FT_LHAR = NA_character_,
            FT_LDAT = format(l, "%d/%m/%Y"), FT_LTIME = format(l, "%H:%M"),
            LE_ID   = paste0(FT_REF, "_1"),
            LE_CDAT = format(TDATE, "%d/%m/%Y"),   # dia de pesca (v1: fecha de venta)
            LE_MSZ  = NA_real_)]

  if (file.exists(fleetFile)) {
    ef <- merge(ef, read_fleet(fleetFile, yr), by = "VE_REF", all.x = TRUE)
  } else {
    ef[, `:=`(VE_LEN = NA_real_, VE_KW = NA_real_, VE_TON = NA_real_)]
    warning("Sin ", fleetFile, ": VE_LEN/VE_KW/VE_TON quedan NA")
  }
  sinFlota <- ef[is.na(VE_LEN) | is.na(VE_KW) | is.na(VE_TON), uniqueN(VE_REF)]

  # tabla de viajes con todas las bases (no entra al workflow ICES)
  fwrite(ef[, .(FT_REF, VE_REF, TDATE, LE_GEAR, LE_MET, LE_RECT, n_pings, n_mar,
                trip_basis, gear_basis, catch_basis, metier_basis,
                gear_note, n_gear_note, gear_month, gear_year, asm_note,
                KG_nota, EUR_nota)],
         file.path(dataPath, paste0("cv_nv_viajes_", yr, ".csv")))

  ef <- ef[, c("VE_REF", "VE_FLT", "VE_COU", "VE_LEN", "VE_KW", "VE_TON",
               "FT_REF", "FT_DCOU", "FT_DHAR", "FT_DDAT", "FT_DTIME",
               "FT_LCOU", "FT_LHAR", "FT_LDAT", "FT_LTIME",
               "LE_ID", "LE_CDAT", "LE_GEAR", "LE_MSZ", "LE_RECT", "LE_DIV", "LE_MET",
               spCols), with = FALSE]

  # ---- 1.6 TACSAT -------------------------------------------------------------
  ta <- y[FT_REF %in% ef$FT_REF,
          .(VE_COU = "ESP", VE_REF,
            SI_LATI = as.numeric(SI_LATI), SI_LONG = as.numeric(SI_LONG),
            SI_DATE = format(SI_UTC, "%d/%m/%Y"), SI_TIME = format(SI_UTC, "%H:%M"),
            SI_SP = as.numeric(SI_SP), SI_HE = as.numeric(SI_HE))]
  tdiff <- y[FT_REF %in% ef$FT_REF, SI_TDIFF]

  assign(paste0("eflalo_", yr), as.data.frame(ef))
  assign(paste0("tacsat_", yr), as.data.frame(ta))
  save(list = paste0("eflalo_", yr), file = file.path(dataPath, paste0("eflalo_", yr, ".RData")))
  save(list = paste0("tacsat_", yr), file = file.path(dataPath, paste0("tacsat_", yr, ".RData")))

  # notas de los buques seleccionados (archivo pequeno para el ejemplo del reporte)
  fwrite(nv[VE_REF %in% ef$VE_REF, .(ID_UDPROD = VE_REF, FECHAVENTA = FECHA, FAO,
                                     NOMBRE_ESPECIE = ESPECIE, ARTE, TOTAL_KILOS = KG, TOTAL_EUROS = EUR)],
         file.path(dataPath, paste0("cv_nv_notas_viajes_", yr, ".csv")))

  # ---- 1.7 Notas FPO/HMD de buques con caja verde sin viaje -------------------
  cvV <- unique(y$VE_REF)
  sinT <- nvT[is.na(FT_REF) & VE_REF %in% cvV & GEAR %in% gears,
              .(notas_dia = uniqueN(FECHA), KG = sum(KG), EUR = sum(EUR)), by = .(VE_REF, GEAR, M = month(FECHA))]
  fwrite(sinT, file.path(dataPath, paste0("cv_nv_notas_sin_track_", yr, ".csv")))
  kgNV <- nvT[VE_REF %in% cvV & GEAR %in% gears, sum(KG)]

  b <- function(v, k) ef_b[get(v) == k, .N]
  ef_b <- sel
  resumen[[as.character(yr)]] <- data.table(
    anio = yr,
    pings_anio = nrow(y), buques_cv = uniqueN(y$VE_REF),
    viajes_buque_dia = nTrips, viajes_solo_puerto = tr[trip_basis == "in_port_only", .N],
    viajes_sel = nrow(ef), viajes_FPO = ef[LE_GEAR == "FPO", .N], viajes_HMD = ef[LE_GEAR == "HMD", .N],
    viajes_mediterraneo_excluidos = nMed,
    arte_landing = b("gear_basis", "landing"), arte_dominant_month = b("gear_basis", "dominant_month"),
    arte_dominant_year = b("gear_basis", "dominant_year"),
    captura_mismo_dia = b("catch_basis", "landing_same_day"),
    captura_dia_siguiente = b("catch_basis", "landing_next_day"),
    captura_ambos = b("catch_basis", "landing_same_and_next_day"),
    captura_none = b("catch_basis", "none"),
    metier_por_especie = b("metier_basis", "landing_species"),
    buques = uniqueN(ef$VE_REF), buques_sin_datos_flota = sinFlota,
    especies_columnas = length(spCols) / 2,
    kg_asignado = sum(sel$KG_nota, na.rm = TRUE),
    kg_notas_FPO_HMD_buques_cv = kgNV,
    kg_notas_FPO_HMD_sin_viaje = sum(sinT$KG),
    pings_tacsat = nrow(ta),
    intervalo_mediana_s = median(tdiff, na.rm = TRUE),
    pct_intervalo_menor_300s = round(100 * mean(tdiff < 300, na.rm = TRUE), 1))
  print(t(resumen[[as.character(yr)]]))
  rm(y, nv, nd, nvT, tr, ts, sel, ef, ta, sp, spT, wk); gc()
}

resumen <- rbindlist(resumen)
fwrite(resumen, file.path(dataPath, "cv_nv_resumen.csv"))
print(t(resumen))
rm(cv); gc()
