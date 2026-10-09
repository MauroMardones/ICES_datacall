#'------------------------------------------------------------------------------
#
# Cajas verdes + notas de venta (CV_NV_2018_2024_ZE.fst) -> TACSAT / EFLALO
# para el workflow ICES VMS & Logbook Data Call (v2.1.0, Anexo 3)
#
# Seleccion: flotas FPO (nasas + trampas) y HMD (dragas hidraulicas), 2023-2024
#
# Correr desde la raiz del proyecto ANTES de 0_global.R / 1_... :
#   source("0_cajasverdes_formato.R")
#
# Salida en Data/: tacsat_2023.RData, eflalo_2023.RData, tacsat_2024.RData,
#                  eflalo_2024.RData  (objetos tacsat_YYYY / eflalo_YYYY)
#                  + Data/cajasverdes_resumen_formato.csv
#
# Ajustes necesarios en 0_global.R (seccion 0.2):
#   yearsToSubmit <- 2023:2024
#   intThres      <- 1   # cajas verdes emiten cada ~3 min; con 5 se borra ~98% de pings
#'------------------------------------------------------------------------------
install.packages("sqldf")
library(icesVMS)        # ahora debería cargar sin error
exists("get_benthis_parameters") 
library(fst)
library(data.table)

dataPath <- "Data/"
fstFile  <- file.path(dataPath, "CV_NV_2018_2024_ZE.fst")
years    <- 2023:2024
gears    <- c("FPO", "HMD")

# Metier L6 por arte (LE_MET6 viene vacio en el .fst)
met6 <- c(FPO = "FPO_CEP_>0_0_0",   # nasas/trampas a pulpo
          HMD = "HMD_MOL_>0_0_0")   # dragas hidraulicas a moluscos

# Especie: notas de venta solo con total por viaje -> codigo generico FAO
spCode <- "MZZ"

# Limite FAO 27 / 37 (Estrecho de Gibraltar): 5 36' W
lonMed <- -5.6

# Caracteristicas de buque. VE_REF = CFR -> exportacion del Registro de Flota UE
#   https://webgate.ec.europa.eu/fleet-europa -> Country ESP -> exportar CSV
#   guardar como Data/EU_fleet_register_ESP.csv
# Alternativa: Data/censo_flota_cajasverdes.csv con VE_REF, VE_LEN, VE_KW, VE_TON
fleetFile <- file.path(dataPath, "EU_fleet_register_ESP.csv")
if (!file.exists(fleetFile)) fleetFile <- file.path(dataPath, "censo_flota_cajasverdes.csv")

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

#'------------------------------------------------------------------------------
# 0. Leer solo columnas necesarias y filtrar
#'------------------------------------------------------------------------------
cols <- c("FT_REF", "VE_REF", "ANYO", "SI_TIMESTAMP", "SI_LATI", "SI_LONG",
          "SI_SP", "SI_HE", "EN_PUERTO", "LE_MET4", "FECHAVENTA",
          "TOTAL_KILOS", "TOTAL_EUROS")
cv <- read_fst(fstFile, columns = cols, as.data.table = TRUE)
cv <- cv[ANYO %in% years & LE_MET4 %in% gears]
message("Pings seleccionados: ", nrow(cv))

# Cajas verdes viene en hora local (Europe/Madrid); TACSAT/EFLALO van en UTC
cv[, SI_UTC := as.POSIXct(format(SI_TIMESTAMP, tz = "UTC"), tz = "UTC")]
cv[, SI_UTC := as.POSIXct(trunc(SI_UTC, "mins"))]            # resolucion HH:MM
setorder(cv, VE_REF, SI_UTC)
cv <- unique(cv, by = c("VE_REF", "SI_UTC"))                  # 1 ping por buque-minuto

#'------------------------------------------------------------------------------
# 1. Zona de pesca por viaje: ICES 27.9.a vs Mediterraneo (FAO 37)
#'------------------------------------------------------------------------------
zona <- cv[, {
  mar <- EN_PUERTO == 0
  if (!any(mar)) mar <- rep(TRUE, .N)
  .(pMed = mean(SI_LONG[mar] > lonMed),
    LE_RECT = mode1(ices_rect(SI_LATI[mar & SI_LONG <= lonMed], SI_LONG[mar & SI_LONG <= lonMed])))
}, by = FT_REF]
zona[, LE_DIV := fifelse(pMed > 0.5, "37.1.1", "27.9.a")]

#'------------------------------------------------------------------------------
# 2. Construir por anio
#'------------------------------------------------------------------------------
fleet <- NULL
resumen <- list()

for (yr in years) {
  y <- cv[ANYO == yr]

  # ---- EFLALO: un registro por viaje (FT_REF) ----
  ef <- y[, .(VE_FLT = LE_MET4[1], LE_GEAR = LE_MET4[1],
              d = min(SI_UTC), l = max(SI_UTC),
              FECHAVENTA = FECHAVENTA[1],
              KG = TOTAL_KILOS[1], EUR = TOTAL_EUROS[1]),
          by = .(FT_REF, VE_REF)]
  ef <- merge(ef, zona, by = "FT_REF", all.x = TRUE)

  nTot <- nrow(ef)
  nMed <- ef[LE_DIV != "27.9.a", .N]
  ef <- ef[LE_DIV == "27.9.a"]                 # fuera del area ICES -> no va al data call

  ef[, `:=`(VE_COU = "ESP",
            FT_DCOU = "ESP", FT_DHAR = NA_character_,
            FT_DDAT = format(d, "%d/%m/%Y"), FT_DTIME = format(d, "%H:%M"),
            FT_LCOU = "ESP", FT_LHAR = NA_character_,
            FT_LDAT = format(l, "%d/%m/%Y"), FT_LTIME = format(l, "%H:%M"),
            LE_ID   = paste0(FT_REF, "_1"),
            LE_CDAT = format(FECHAVENTA, "%d/%m/%Y"),
            LE_MSZ  = NA_real_,
            LE_MET  = unname(met6[LE_GEAR]))]
  ef[is.na(LE_CDAT), LE_CDAT := FT_LDAT]
  ef[, paste0("LE_KG_", spCode)   := KG]
  ef[, paste0("LE_EURO_", spCode) := EUR]

  # Caracteristicas de buque
  if (file.exists(fleetFile)) {
    fleet <- read_fleet(fleetFile, yr)
    ef <- merge(ef, fleet, by = "VE_REF", all.x = TRUE)
  } else {
    ef[, `:=`(VE_LEN = NA_real_, VE_KW = NA_real_, VE_TON = NA_real_)]
  }
  sinFlota <- ef[is.na(VE_LEN) | is.na(VE_KW) | is.na(VE_TON), uniqueN(VE_REF)]

  ef <- ef[, c("VE_REF", "VE_FLT", "VE_COU", "VE_LEN", "VE_KW", "VE_TON",
               "FT_REF", "FT_DCOU", "FT_DHAR", "FT_DDAT", "FT_DTIME",
               "FT_LCOU", "FT_LHAR", "FT_LDAT", "FT_LTIME",
               "LE_ID", "LE_CDAT", "LE_GEAR", "LE_MSZ", "LE_RECT", "LE_DIV", "LE_MET",
               paste0("LE_KG_", spCode), paste0("LE_EURO_", spCode)), with = FALSE]

  # ---- TACSAT: pings de los buques/viajes que quedan en 27.9.a ----
  ta <- y[FT_REF %in% ef$FT_REF,
          .(VE_COU = "ESP", VE_REF,
            SI_LATI = as.numeric(SI_LATI), SI_LONG = as.numeric(SI_LONG),
            SI_DATE = format(SI_UTC, "%d/%m/%Y"), SI_TIME = format(SI_UTC, "%H:%M"),
            SI_SP = as.numeric(SI_SP), SI_HE = as.numeric(SI_HE))]

  assign(paste0("eflalo_", yr), as.data.frame(ef))
  assign(paste0("tacsat_", yr), as.data.frame(ta))
  save(list = paste0("eflalo_", yr), file = file.path(dataPath, paste0("eflalo_", yr, ".RData")))
  save(list = paste0("tacsat_", yr), file = file.path(dataPath, paste0("tacsat_", yr, ".RData")))

  resumen[[as.character(yr)]] <- data.table(
    anio = yr,
    viajes_total = nTot, viajes_mediterraneo_excluidos = nMed, viajes_eflalo = nrow(ef),
    viajes_FPO = ef[LE_GEAR == "FPO", .N], viajes_HMD = ef[LE_GEAR == "HMD", .N],
    buques = uniqueN(ef$VE_REF), buques_sin_datos_flota = sinFlota,
    viajes_sin_LE_RECT = ef[is.na(LE_RECT), .N],
    kg_total = sum(ef[[paste0("LE_KG_", spCode)]], na.rm = TRUE),
    pings_tacsat = nrow(ta))
}

resumen <- rbindlist(resumen)
fwrite(resumen, file.path(dataPath, "cajasverdes_resumen_formato.csv"))
print(resumen)
if (!file.exists(fleetFile))
  warning("Sin archivo de flota: VE_LEN/VE_KW/VE_TON quedan NA y el paso 2 del workflow se detendra.\n",
          "Descarga el Registro de Flota UE (ESP) como Data/EU_fleet_register_ESP.csv y vuelve a correr.")
