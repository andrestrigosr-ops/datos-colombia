# datos-colombia: copia propia de las series del Banco de la República
#
# Qué hace, en orden:
#   1. Baja el catálogo completo de SUAMECA (unas 850 series, con la fecha de
#      su último y su próximo cargue) y lo guarda en catalogo/catalogo.csv.
#   2. Para cada serie de series.csv, si el Banco la cargó de nuevo desde la
#      última vez, baja la serie completa y la guarda en datos/banrep/<id>.csv.
#   3. Arma datos/banrep/monitor.csv (todas las series juntas, con su clave)
#      y escribe NOVEDADES.md con las series mensuales, trimestrales o anuales
#      que trajeron un periodo nuevo.
#
# Si una serie falla o llega rara, se deja la versión anterior y se anota en
# estado/errores.csv. El script solo se detiene si falla el catálogo.
#
# Se corre solo cada noche en GitHub (ver .github/workflows/actualizar.yml).
# También se puede correr a mano en RStudio con el repositorio como carpeta
# de trabajo.

for (p in c("httr", "jsonlite")) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org")
}

BASE     <- "https://suameca.banrep.gov.co"
URL_CAT  <- paste0(BASE, "/buscador-de-series/rest/buscadorSeriesRestService/listarSeriesXCategoria")
URL_SER  <- paste0(BASE, "/estadisticas-economicas-back/rest/estadisticaEconomicaRestService/consultaInformacionSerie?idSerie=")
REFERER  <- paste0(BASE, "/descarga-multiple-de-datos/")
UA       <- "Mozilla/5.0 (datos-colombia; uso analitico)"
TZ       <- "America/Bogota"
HOY      <- format(Sys.time(), "%Y-%m-%d %H:%M", tz = TZ)

# Para probar sin internet: Sys.setenv(DATOS_FIXTURES = "carpeta") con
# catalogo.json y <id>.json adentro.
FIXTURES <- Sys.getenv("DATOS_FIXTURES")

dir.create("catalogo", showWarnings = FALSE)
dir.create("datos/banrep", recursive = TRUE, showWarnings = FALSE)
dir.create("estado", showWarnings = FALSE)

# --- Funciones -------------------------------------------------------

pedir <- function(url, intentos = 3) {
  for (k in seq_len(intentos)) {
    r <- tryCatch(httr::GET(url, httr::user_agent(UA), httr::timeout(180),
                            httr::add_headers(Referer = REFERER, Accept = "application/json, text/plain, */*")),
                  error = function(e) e)
    if (!inherits(r, "error") && httr::status_code(r) == 200) {
      txt <- httr::content(r, as = "text", encoding = "UTF-8")
      if (nchar(txt) > 2) return(txt)
    }
    Sys.sleep(5 * k)
  }
  stop("No respondió: ", url)
}

leer_json <- function(url, fixture) {
  if (nzchar(FIXTURES)) return(jsonlite::fromJSON(file.path(FIXTURES, fixture), simplifyVector = FALSE))
  jsonlite::fromJSON(pedir(url), simplifyVector = FALSE)
}

# Las fechas del catálogo vienen como {year, month (0 = enero), dayOfMonth}
fecha_cat <- function(x) {
  if (is.null(x) || is.null(x$year)) return(NA_character_)
  format(as.Date(sprintf("%04d-%02d-%02d", x$year, x$month + 1, x$dayOfMonth)))
}
txt <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])

leer_csv <- function(f) if (file.exists(f)) utils::read.csv(f, colClasses = "character", encoding = "UTF-8") else NULL
escribir <- function(d, f) utils::write.csv(d, f, row.names = FALSE, fileEncoding = "UTF-8", na = "")

# --- 1. Catálogo -----------------------------------------------------

message("Bajando el catálogo de SUAMECA...")
cat_json <- leer_json(URL_CAT, "catalogo.json")
filas <- list()
for (n1 in names(cat_json)) for (n2 in names(cat_json[[n1]])) for (s in cat_json[[n1]][[n2]]) {
  filas[[length(filas) + 1]] <- data.frame(
    id = as.character(s$id), nombre = txt(s$nombre),
    categoria1 = n1, categoria2 = n2, categoria3 = txt(s$categoriaNivel3),
    periodicidad = txt(s$descripcionPeriodicidad), unidad = txt(s$unidad),
    fuente = txt(s$fuente), producida_banrep = txt(s$datoProducidoBanrep),
    fecha_inicial = fecha_cat(s$fechaInicial), fecha_final = fecha_cat(s$fechaFinal),
    ultimo_cargue = fecha_cat(s$fechaUltimoCargue), proximo_cargue = fecha_cat(s$fechaProximoCargue),
    stringsAsFactors = FALSE)
}
catalogo <- do.call(rbind, filas)
catalogo <- catalogo[!duplicated(catalogo$id), ]
catalogo <- catalogo[order(as.numeric(catalogo$id)), ]
if (nrow(catalogo) < 500) stop("El catálogo llegó con solo ", nrow(catalogo), " series. No se cambia nada.")
escribir(catalogo, "catalogo/catalogo.csv")
message("  ", nrow(catalogo), " series en el catálogo.")

# --- 2. Series pedidas -----------------------------------------------

pedidas <- utils::read.csv("series.csv", colClasses = "character", encoding = "UTF-8")
pedidas[is.na(pedidas)] <- ""
lista <- list()
for (i in seq_len(nrow(pedidas))) {
  p <- pedidas[i, ]
  if (nzchar(p$id)) {
    lista[[length(lista) + 1]] <- data.frame(clave = p$clave, id = p$id, tablero = p$tablero, stringsAsFactors = FALSE)
  } else if (nzchar(p$patron)) {
    m <- catalogo[grepl(p$patron, catalogo$nombre, perl = TRUE), ]
    if (nrow(m) == 0) warning("El patrón de ", p$clave, " no encontró series en el catálogo.")
    suf <- tolower(gsub("[^A-Za-z0-9]+", "_", iconv(sub(p$patron, "", m$nombre, perl = TRUE), "UTF-8", "ASCII//TRANSLIT")))
    suf <- gsub("^_|_$", "", sub("_trimestral$|_mensual$|_anual$", "", suf))
    if (nrow(m)) lista[[length(lista) + 1]] <- data.frame(clave = paste0(p$clave, ":", suf), id = m$id, tablero = p$tablero, stringsAsFactors = FALSE)
  }
}
lista <- do.call(rbind, lista)
lista <- lista[!duplicated(lista$id), ]

estado_ant <- leer_csv("estado/estado.csv")
if (is.null(estado_ant)) estado_ant <- data.frame(id = character(), ultimo_cargue = character(), ultima_fecha = character(),
                                                  observaciones = character(), revisado = character())

estado <- list(); errores <- list(); novedades <- list()
for (i in seq_len(nrow(lista))) {
  id <- lista$id[i]; clave <- lista$clave[i]
  info <- catalogo[catalogo$id == id, ]
  ant  <- estado_ant[estado_ant$id == id, ]
  archivo <- file.path("datos/banrep", paste0(id, ".csv"))

  if (nrow(info) == 0) {
    errores[[length(errores) + 1]] <- data.frame(id = id, clave = clave, error = "No está en el catálogo (¿la descontinuaron?)")
    if (nrow(ant)) estado[[length(estado) + 1]] <- ant
    next
  }
  sin_cambio <- nrow(ant) == 1 && identical(ant$ultimo_cargue, info$ultimo_cargue) && file.exists(archivo)
  if (sin_cambio) { estado[[length(estado) + 1]] <- ant; next }

  res <- tryCatch({
    s <- leer_json(paste0(URL_SER, id), paste0(id, ".json"))[[1]]
    obs <- s$data
    if (length(obs) == 0) stop("Llegó sin datos")
    ms  <- vapply(obs, function(o) as.numeric(o[[1]]), numeric(1))
    val <- vapply(obs, function(o) if (is.null(o[[2]])) NA_real_ else as.numeric(o[[2]]), numeric(1))
    fecha <- format(as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC"), "%Y-%m-%d", tz = TZ)
    d <- data.frame(fecha = fecha, valor = val, stringsAsFactors = FALSE)
    d <- d[!duplicated(d$fecha, fromLast = TRUE), ]
    d <- d[order(d$fecha), ]
    n_ant <- if (nrow(ant)) as.numeric(ant$observaciones) else 0
    if (nrow(d) < 0.9 * n_ant) stop("Llegaron ", nrow(d), " datos y antes había ", n_ant)
    d
  }, error = function(e) e)

  if (inherits(res, "error")) {
    errores[[length(errores) + 1]] <- data.frame(id = id, clave = clave, error = gsub("\\s+", " ", conditionMessage(res)))
    if (nrow(ant)) estado[[length(estado) + 1]] <- ant
    next
  }
  escribir(res, archivo)
  ultima <- max(res$fecha)
  estado[[length(estado) + 1]] <- data.frame(id = id, ultimo_cargue = info$ultimo_cargue, ultima_fecha = ultima,
                                             observaciones = as.character(nrow(res)), revisado = HOY)
  nuevo_periodo <- nrow(ant) == 1 && !is.na(ant$ultima_fecha) && ultima > ant$ultima_fecha
  if (nuevo_periodo && !info$periodicidad %in% c("Diaria", "Semanal")) {
    novedades[[length(novedades) + 1]] <- data.frame(
      clave = clave, id = id, tablero = lista$tablero[i], nombre = info$nombre, periodicidad = info$periodicidad,
      periodo_anterior = ant$ultima_fecha, periodo_nuevo = ultima, valor = res$valor[res$fecha == ultima])
  }
  Sys.sleep(0.5)
}

estado <- do.call(rbind, estado)
escribir(estado, "estado/estado.csv")
errores <- if (length(errores)) do.call(rbind, errores) else data.frame(id = character(), clave = character(), error = character())
escribir(errores, "estado/errores.csv")

# --- 3. Archivo único para el Monitor y novedades ---------------------

junto <- do.call(rbind, lapply(seq_len(nrow(lista)), function(i) {
  f <- file.path("datos/banrep", paste0(lista$id[i], ".csv"))
  if (!file.exists(f)) return(NULL)
  d <- leer_csv(f)
  data.frame(clave = lista$clave[i], id = lista$id[i], tablero = lista$tablero[i], fecha = d$fecha, valor = d$valor)
}))
escribir(junto, "datos/banrep/monitor.csv")

if (length(novedades)) {
  nov <- do.call(rbind, novedades)
  nov <- nov[order(nov$tablero, nov$clave), ]
  # Las remesas por país van en una sola línea
  rp <- grepl("^remesas_pais:", nov$clave)
  lineas <- with(nov[!rp, ], sprintf("- **%s** (%s): nuevo dato de %s, valor %s. Antes llegaba hasta %s.",
                                     nombre, tablero, periodo_nuevo, formatC(valor, format = "f", digits = 2, big.mark = ".", decimal.mark = ","), periodo_anterior))
  if (any(rp)) lineas <- c(lineas, sprintf("- **Remesas por país de origen** (externo): %d países con dato de %s.", sum(rp), max(nov$periodo_nuevo[rp])))
  tableros <- sort(unique(nov$tablero))
  md <- c(paste0("# Datos nuevos del Banco de la República (", HOY, ")"), "",
          lineas, "",
          paste0("Tableros para actualizar: ", paste(tableros, collapse = ", "), "."))
  escribir(nov, "estado/novedades.csv")
} else {
  md <- c(paste0("# Sin datos nuevos (", HOY, ")"), "",
          "Ninguna serie mensual, trimestral o anual trajo un periodo nuevo. Las diarias se actualizan sin aviso.")
  escribir(data.frame(clave = character()), "estado/novedades.csv")
}
if (nrow(errores)) md <- c(md, "", "## Series con problemas", "",
                           sprintf("- %s (%s): %s", errores$clave, errores$id, errores$error))
writeLines(md, "NOVEDADES.md", useBytes = TRUE)

# Para GitHub Actions: avisa si hay novedades
salida <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(salida)) cat(sprintf("hay_novedades=%s\nhay_errores=%s\n", tolower(length(novedades) > 0), tolower(nrow(errores) > 0)),
                        file = salida, append = TRUE)

message("Listo. ", length(novedades), " series con dato nuevo, ", nrow(errores), " con problemas.")
