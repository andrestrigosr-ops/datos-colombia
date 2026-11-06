# datos-colombia

Copia propia de las series del Banco de la República que usa el [Monitor Colombia](https://monitorcolombia.margeninsights.com), de margen insights. Se actualiza sola cada noche.

## De dónde salen

De SUAMECA, el sistema de estadísticas del Banco de la República. Primero baja el catálogo completo (unas 850 series, con su último y su próximo cargue) y después cada serie que haya cambiado. Son los mismos datos que se ven en la página del Banco, sin intermediarios.

## Qué hay en cada carpeta

- `series.csv` es la lista de series que se copian. Para agregar una, se añade una fila con su clave, su `id` (el número de la serie en el catálogo) y el tablero donde se usa. Con `patron` en vez de `id` se toman todas las series cuyo nombre empiece así, como las remesas por país.
- `catalogo/catalogo.csv` es el catálogo completo del Banco, para buscar el `id` de cualquier serie.
- `datos/banrep/<id>.csv` tiene una serie, con fecha y valor.
- `datos/banrep/monitor.csv` tiene todas las series juntas, con su clave. Es el que lee el Monitor.
- `NOVEDADES.md` dice qué series trajeron un dato nuevo en la última corrida y cuáles fallaron.
- `estado/` guarda el control interno (último cargue de cada serie, errores y novedades).

## Cuándo avisa

Cada noche, a las 11:37 p. m. de Bogotá, corre `R/actualizar.R`. Si una serie mensual, trimestral o anual trae un periodo nuevo, abre un *issue* con la lista y llega el aviso de GitHub al correo y a la app. Las series diarias, como la TRM, se actualizan sin aviso.

Si una serie falla o llega con menos datos de los que tenía, se conserva la versión anterior y queda anotada en `NOVEDADES.md`.

## Correrlo a mano

En GitHub, en *Actions*, entra a *Actualizar datos* y presiona *Run workflow*. También se puede correr `R/actualizar.R` en RStudio con esta carpeta como directorio de trabajo.

## Leer los datos desde R

```r
url <- "https://raw.githubusercontent.com/andrestrigosr-ops/datos-colombia/main/datos/banrep/monitor.csv"
d <- read.csv(url)
trm <- subset(d, clave == "trm")
```
