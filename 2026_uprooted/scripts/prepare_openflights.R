# prepare_openflights.R
#
# Build a country-pair table of direct flight routes from the OpenFlights
# database. Used as an additional bilateral control in the Section 5 horse
# race ("frequency of flights" referee request, round 3).
#
# OpenFlights is a community-maintained dataset of airports, airlines and
# routes. The routes file lists ~67,000 direct route segments served by
# scheduled airlines, captured around 2017. We aggregate to undirected
# country pairs by counting the number of distinct origin-destination
# airport pairs, then symmetrising with max(i->j, j->i).
#
# Source URLs:
#   https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat
#   https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat
#
# The script caches downloaded files inside generated/openflights_cache/ so
# repeated runs do not need to re-download.
#
# Output: Submission/scripts/generated/openflights_routes.csv with columns
#   iso3_i, iso3_j (alphabetically ordered ISO3 codes),
#   n_direct_routes (count of distinct airport-pair routes),
#   log_routes = log(1 + n_direct_routes)

suppressMessages({
  library(tibble)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(countrycode)
})

# Path setup ------------------------------------------------------------------
script_dir <- tryCatch({
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    dirname(normalizePath(sub("^--file=", "", file_arg[1])))
  } else {
    getwd()
  }
}, error = function(e) getwd())

generated_dir <- file.path(script_dir, "generated")
cache_dir     <- file.path(generated_dir, "openflights_cache")
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

airports_path <- file.path(cache_dir, "airports.dat")
routes_path   <- file.path(cache_dir, "routes.dat")

airports_url <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat"
routes_url   <- "https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat"

download_if_missing <- function(url, dest) {
  if (!file.exists(dest) || file.info(dest)$size < 1000) {
    cat(sprintf("Downloading %s\n  -> %s\n", url, dest))
    utils::download.file(url, dest, quiet = TRUE, mode = "wb")
  } else {
    cat(sprintf("Cached: %s\n", dest))
  }
}

download_if_missing(airports_url, airports_path)
download_if_missing(routes_url,   routes_path)

# Read airports ---------------------------------------------------------------
# Columns: id, name, city, country, IATA, ICAO, lat, lon, alt, tz, dst, tz_db, type, source
airports_col_names <- c("airport_id", "name", "city", "country",
                        "iata", "icao", "lat", "lon", "alt",
                        "tz_offset", "dst", "tz_db", "type", "source")

airports <- read_csv(airports_path,
                     col_names = airports_col_names,
                     na = c("\\N", "", "NA"),
                     show_col_types = FALSE,
                     progress = FALSE)

cat(sprintf("Loaded %d airports across %d distinct country labels.\n",
            nrow(airports),
            length(unique(airports$country))))

# Map country names to ISO3 ---------------------------------------------------
# OpenFlights uses common English country names. countrycode handles most.
# Hardcode a few that the package warns about for stability across versions.
manual_iso <- c(
  "Burma"                    = "MMR",
  "Congo (Brazzaville)"      = "COG",
  "Congo (Kinshasa)"         = "COD",
  "Cote d'Ivoire"            = "CIV",
  "East Timor"               = "TLS",
  "Falkland Islands"         = "FLK",
  "Iran"                     = "IRN",
  "Korea"                    = "KOR",
  "Laos"                     = "LAO",
  "Macedonia"                = "MKD",
  "Micronesia"               = "FSM",
  "Moldova"                  = "MDA",
  "Russia"                   = "RUS",
  "South Korea"              = "KOR",
  "Syria"                    = "SYR",
  "Taiwan"                   = "TWN",
  "Tanzania"                 = "TZA",
  "United States"            = "USA",
  "United Kingdom"           = "GBR",
  "Vatican City"             = "VAT",
  "Venezuela"                = "VEN",
  "Vietnam"                  = "VNM",
  "Virgin Islands"           = "VGB"
)

iso3_lookup <- function(name) {
  out <- unname(manual_iso[name])
  needs <- is.na(out)
  if (any(needs)) {
    out[needs] <- suppressWarnings(
      countrycode::countrycode(name[needs], origin = "country.name",
                               destination = "iso3c")
    )
  }
  out
}

airports$iso3 <- iso3_lookup(airports$country)

unmatched <- airports %>%
  filter(is.na(iso3)) %>%
  count(country, sort = TRUE)

if (nrow(unmatched) > 0) {
  cat(sprintf("Warning: %d airports could not be mapped to ISO3:\n",
              sum(unmatched$n)))
  print(head(unmatched, 10))
}

airport_iso <- airports %>%
  select(airport_id, iso3) %>%
  filter(!is.na(iso3)) %>%
  mutate(airport_id = as.character(airport_id))

# Read routes -----------------------------------------------------------------
# Columns: airline, airline_id, src_iata, src_id, dst_iata, dst_id,
#          codeshare, stops, equipment
routes_col_names <- c("airline", "airline_id",
                      "src_iata", "src_id",
                      "dst_iata", "dst_id",
                      "codeshare", "stops", "equipment")

routes <- read_csv(routes_path,
                   col_names = routes_col_names,
                   na = c("\\N", "", "NA"),
                   show_col_types = FALSE,
                   progress = FALSE)

cat(sprintf("Loaded %d route segments.\n", nrow(routes)))

# Keep only direct routes (stops == 0) and rows with both endpoints resolved
routes_clean <- routes %>%
  filter(stops == 0L | is.na(stops)) %>%
  mutate(src_id = as.character(src_id),
         dst_id = as.character(dst_id)) %>%
  filter(!is.na(src_id), !is.na(dst_id)) %>%
  inner_join(airport_iso, by = c("src_id" = "airport_id")) %>%
  rename(iso3_src = iso3) %>%
  inner_join(airport_iso, by = c("dst_id" = "airport_id")) %>%
  rename(iso3_dst = iso3)

cat(sprintf("Routes with both endpoints mapped to ISO3: %d\n",
            nrow(routes_clean)))

# Build undirected country-pair counts ----------------------------------------
# Count distinct (src_airport, dst_airport, iso3_i, iso3_j) tuples per pair.
# Then symmetrise i->j and j->i so the data are undirected.
ordered_pair <- function(a, b) {
  data.frame(iso3_i = pmin(a, b), iso3_j = pmax(a, b),
             stringsAsFactors = FALSE)
}

route_pairs <- routes_clean %>%
  filter(iso3_src != iso3_dst) %>%
  distinct(src_id, dst_id, iso3_src, iso3_dst)

pair_idx <- ordered_pair(route_pairs$iso3_src, route_pairs$iso3_dst)
route_pairs$iso3_i <- pair_idx$iso3_i
route_pairs$iso3_j <- pair_idx$iso3_j

country_pairs <- route_pairs %>%
  count(iso3_i, iso3_j, name = "n_direct_routes") %>%
  mutate(log_routes = log1p(n_direct_routes))

cat(sprintf("Country pairs with at least one direct route: %d\n",
            nrow(country_pairs)))

# Write -----------------------------------------------------------------------
out_path <- file.path(generated_dir, "openflights_routes.csv")
write_csv(country_pairs, out_path)
cat(sprintf("Wrote %s (%d rows)\n", out_path, nrow(country_pairs)))

cat("\nTop 10 country pairs by direct-route count:\n")
print(head(country_pairs[order(-country_pairs$n_direct_routes), ], 10))
