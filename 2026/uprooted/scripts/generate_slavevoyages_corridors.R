library(data.table)

root_dir <- "C:/Users/johanf/Dropbox/0Claude0/1Research/Fourie_Meta"
data_dir <- file.path(root_dir, "Data")
wp_dir <- file.path(root_dir, "WorkingPaperNew")
script_dir <- file.path(wp_dir, "scripts")
gen_dir <- file.path(script_dir, "generated")

dir.create(gen_dir, recursive = TRUE, showWarnings = FALSE)

fmt_num <- function(x) format(round(x, 1), big.mark = ",", trim = TRUE, scientific = FALSE)
fmt_pct <- function(x) paste0(sprintf("%.1f", 100 * x), "%")
normalize_text <- function(x) {
  x <- trimws(as.character(x))
  x[x %in% c("", "0", "nan", "NA")] <- NA_character_
  bad <- !is.na(x) & grepl("[ÃâÂ]", x)
  if (any(bad)) {
    repaired_latin1 <- iconv(x[bad], from = "latin1", to = "UTF-8")
    repaired_cp1252 <- iconv(x[bad], from = "windows-1252", to = "UTF-8")
    keep_cp1252 <- !is.na(repaired_cp1252) & (!is.na(repaired_latin1) & nchar(repaired_cp1252) >= nchar(repaired_latin1) | is.na(repaired_latin1))
    repaired <- repaired_latin1
    repaired[keep_cp1252] <- repaired_cp1252[keep_cp1252]
    x[bad] <- repaired
  }
  x
}

read_slavevoyages <- function(path, dataset) {
  dt <- fread(path, encoding = "UTF-8")
  names(dt) <- make.unique(names(dt))
  setDT(dt)

  dt[, dataset := dataset]
  dt[, year_arrived := suppressWarnings(as.numeric(`Year arrived with captives`))]
  dt[, disembarked_imp := suppressWarnings(as.numeric(`Total disembarked (IMP)`))]
  dt[, embarked_imp := suppressWarnings(as.numeric(`Total embarked (IMP)`))]

  dt[, origin_place := normalize_text(voyage_itinerary__imp_principal_place_of_slave_purchase__name)]
  dt[, dest_port := normalize_text(voyage_itinerary__imp_principal_port_slave_dis__name)]
  dt[, origin_region := normalize_text(voyage_itinerary__imp_principal_region_of_slave_purchase__name)]
  dt[, dest_region := normalize_text(voyage_itinerary__imp_principal_region_slave_dis__name)]
  dt[, origin_broad := normalize_text(`Broad region of captive purchase (IMP)`)]
  dt[, dest_broad := normalize_text(`Broad region of captive disembarkation (IMP)`)]

  dt[!is.finite(disembarked_imp), disembarked_imp := NA_real_]
  dt[!is.finite(embarked_imp), embarked_imp := NA_real_]

  dt[]
}

map_dict <- rbindlist(list(
  data.table(
    source_column = c(
      "origin_region", "origin_region", "origin_region", "origin_region",
      "origin_region", "origin_region", "origin_region", "origin_region"
    ),
    label = c(
      "West Central Africa and St Helena",
      "Bight of Benin",
      "Bight of Biafra and Gulf of Guinea islands",
      "East Africa and Indian Ocean islands",
      "Gold Coast",
      "Senegambia and offshore Atlantic",
      "Sierra Leone",
      "Windward Coast"
    ),
    iso3 = c("AGO", "BEN", "NGA", "MOZ", "GHA", "SEN", "SLE", "LBR"),
    modern_name = c("Angola", "Benin", "Nigeria", "Mozambique", "Ghana", "Senegal", "Sierra Leone", "Liberia"),
    map_quality = rep("region_approx", 8),
    notes = c(
      "Broad historical embarkation region mapped conservatively to Angola.",
      "Historical embarkation region mapped conservatively to Benin.",
      "Historical embarkation region mapped conservatively to Nigeria.",
      "Historical embarkation region mapped conservatively to Mozambique.",
      "Historical embarkation region mapped to Ghana.",
      "Historical embarkation region mapped conservatively to Senegal.",
      "Historical embarkation region mapped to Sierra Leone.",
      "Historical embarkation region mapped conservatively to Liberia."
    )
  ),
  data.table(
    source_column = c(
      "origin_place", "origin_place", "origin_place", "origin_place", "origin_place",
      "origin_place", "origin_place", "origin_place", "origin_place", "origin_place",
      "origin_place", "origin_place", "origin_place", "origin_place", "origin_place",
      "origin_place", "origin_place", "origin_place", "origin_place", "origin_place",
      "origin_place", "origin_place", "origin_place", "origin_place", "origin_place",
      "origin_place", "origin_place", "origin_place", "origin_place", "origin_place",
      "origin_place", "origin_place"
    ),
    label = c(
      "Luanda", "Benguela", "Cabinda", "Malembo", "Bonny",
      "Whydah, Ouidah", "Calabar", "Anomabu", "Cape Coast Castle", "Mozambique",
      "Quilimane", "Congo River", "Loango", "Gambia", "Cape Verde Islands",
      "Elmina", "Lagos, Onim", "São Tomé", "Axim", "Gorée",
      "Bissau", "Cacheu", "Sierra Leone estuary", "Inhambane", "Lourenço Marques",
      "Accra", "Badagry/Apa", "Grand Popo", "Ardra", "Little Popo",
      "Bance/Bunce Island", "Gallinhas"
    ),
    iso3 = c(
      "AGO", "AGO", "AGO", "AGO", "NGA",
      "BEN", "NGA", "GHA", "GHA", "MOZ",
      "MOZ", "COG", "COG", "GMB", "CPV",
      "GHA", "NGA", "STP", "GHA", "SEN",
      "GNB", "GNB", "SLE", "MOZ", "MOZ",
      "GHA", "NGA", "BEN", "BEN", "TGO",
      "SLE", "LBR"
    ),
    modern_name = c(
      "Angola", "Angola", "Angola", "Angola", "Nigeria",
      "Benin", "Nigeria", "Ghana", "Ghana", "Mozambique",
      "Mozambique", "Republic of the Congo", "Republic of the Congo", "Gambia", "Cabo Verde",
      "Ghana", "Nigeria", "Sao Tome and Principe", "Ghana", "Senegal",
      "Guinea-Bissau", "Guinea-Bissau", "Sierra Leone", "Mozambique", "Mozambique",
      "Ghana", "Nigeria", "Benin", "Benin", "Togo",
      "Sierra Leone", "Liberia"
    ),
    map_quality = rep("exact_place", 32),
    notes = "Specific embarkation place mapped to modern country."
  ),
  data.table(
    source_column = c(
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region"
    ),
    label = c(
      "Southeast Brazil", "Bahia", "Jamaica", "Cuba", "Saint-Domingue",
      "Pernambuco", "Barbados", "South Carolina", "Amazonia", "Grenada",
      "St Kitts", "Antigua", "Dominica", "Danish West Indies", "British Guiana",
      "Guadeloupe", "St Vincent", "Virginia", "Martinique", "Puerto Rico",
      "Trinidad", "Bahamas", "Florida", "Georgia", "Maryland",
      "Massachusetts", "New York", "North Carolina", "Pennsylvania", "Rhode Island",
      "Connecticut", "New Jersey", "New Hampshire", "British Honduras"
    ),
    iso3 = c(
      "BRA", "BRA", "JAM", "CUB", "HTI",
      "BRA", "BRB", "USA", "BRA", "GRD",
      "KNA", "ATG", "DMA", "VIR", "GUY",
      "GLP", "VCT", "USA", "MTQ", "PRI",
      "TTO", "BHS", "USA", "USA", "USA",
      "USA", "USA", "USA", "USA", "USA",
      "USA", "USA", "USA", "BLZ"
    ),
    modern_name = c(
      "Brazil", "Brazil", "Jamaica", "Cuba", "Haiti",
      "Brazil", "Barbados", "United States", "Brazil", "Grenada",
      "Saint Kitts and Nevis", "Antigua and Barbuda", "Dominica", "U.S. Virgin Islands", "Guyana",
      "Guadeloupe", "Saint Vincent and the Grenadines", "United States", "Martinique", "Puerto Rico",
      "Trinidad and Tobago", "Bahamas", "United States", "United States", "United States",
      "United States", "United States", "United States", "United States", "United States",
      "United States", "United States", "United States", "Belize"
    ),
    map_quality = rep("region_exact_or_admin", 34),
    notes = "Disembarkation region mapped to modern country or territory."
  ),
  data.table(
    source_column = c(
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port"
    ),
    label = c(
      "Bahia, place unspecified", "Rio de Janeiro", "Kingston", "Jamaica, place unspecified", "Pernambuco, place unspecified",
      "Barbados, place unspecified", "Cap Français", "Havana", "Suriname, place unspecified", "Cuba, port unspecified",
      "Martinique, place unspecified", "Cartagena", "Charleston", "Grenada, place unspecified", "St Kitts, port unspecified",
      "Saint John (Antigua)", "Curaçao", "Port-au-Prince", "Dominica, place unspecified", "Freetown",
      "Maranhão", "Léogane", "Demerara", "Santiago de Cuba", "Belém",
      "Guadeloupe, place unspecified", "Trinidad, place unspecified", "San Juan", "Montevideo", "Buenos Aires",
      "Savannah", "Annapolis", "Beaufort", "Boston", "New York",
      "Virginia, place unspecified", "Maryland, port unspecified", "Basse-Terre", "Fort-Royale", "Tobago, place unspecified"
    ),
    iso3 = c(
      "BRA", "BRA", "JAM", "JAM", "BRA",
      "BRB", "HTI", "CUB", "SUR", "CUB",
      "MTQ", "COL", "USA", "GRD", "KNA",
      "ATG", "CUW", "HTI", "DMA", "SLE",
      "BRA", "HTI", "GUY", "CUB", "BRA",
      "GLP", "TTO", "PRI", "URY", "ARG",
      "USA", "USA", "USA", "USA", "USA",
      "USA", "USA", "GLP", "MTQ", "TTO"
    ),
    modern_name = c(
      "Brazil", "Brazil", "Jamaica", "Jamaica", "Brazil",
      "Barbados", "Haiti", "Cuba", "Suriname", "Cuba",
      "Martinique", "Colombia", "United States", "Grenada", "Saint Kitts and Nevis",
      "Antigua and Barbuda", "Curacao", "Haiti", "Dominica", "Sierra Leone",
      "Brazil", "Haiti", "Guyana", "Cuba", "Brazil",
      "Guadeloupe", "Trinidad and Tobago", "Puerto Rico", "Uruguay", "Argentina",
      "United States", "United States", "United States", "United States", "United States",
      "United States", "United States", "Guadeloupe", "Martinique", "Trinidad and Tobago"
    ),
    map_quality = rep("exact_port", 40),
    notes = "Specific disembarkation port mapped to modern country or territory."
  ),
  data.table(
    source_column = rep("origin_place", 24),
    label = c(
      "Jamaica, place unspecified", "Kingston", "Portos do Norte", "Curaçao", "Bahia, place unspecified",
      "Dominica, place unspecified", "Norfolk", "Pernambuco, place unspecified", "New Orleans", "Charleston",
      "Baltimore", "St Thomas", "Brazil, place unspecified", "Santos", "Campos",
      "Richmond (VA)", "Barbados, place unspecified", "Mobile", "Suriname, place unspecified", "Martinique, place unspecified",
      "Guadeloupe, place unspecified", "Trinidad, port unspecified", "New Providence", "St Vincent, port unspecified"
    ),
    iso3 = c(
      "JAM", "JAM", "BRA", "CUW", "BRA",
      "DMA", "USA", "BRA", "USA", "USA",
      "USA", "VIR", "BRA", "BRA", "BRA",
      "USA", "BRB", "USA", "SUR", "MTQ",
      "GLP", "TTO", "BHS", "VCT"
    ),
    modern_name = c(
      "Jamaica", "Jamaica", "Brazil", "Curacao", "Brazil",
      "Dominica", "United States", "Brazil", "United States", "United States",
      "United States", "U.S. Virgin Islands", "Brazil", "Brazil", "Brazil",
      "United States", "Barbados", "United States", "Suriname", "Martinique",
      "Guadeloupe", "Trinidad and Tobago", "Bahamas", "Saint Vincent and the Grenadines"
    ),
    map_quality = rep("exact_place", 24),
    notes = "Specific intra-American purchase place mapped to modern country or territory."
  ),
  data.table(
    source_column = c(
      "origin_region", "origin_region", "origin_region", "origin_region", "origin_region",
      "origin_region", "origin_region", "origin_region", "origin_region", "origin_region",
      "origin_region", "origin_region", "origin_region", "origin_region", "origin_region",
      "origin_region", "origin_region", "origin_region", "origin_region", "origin_region",
      "origin_region", "origin_region", "origin_region"
    ),
    label = c(
      "Jamaica", "Southeast Brazil", "Amazonia", "Bahia", "Barbados",
      "Virginia", "Dominica", "Pernambuco", "Danish West Indies", "South Carolina",
      "Maryland", "Antigua", "Puerto Rico", "St Kitts", "Florida",
      "Grenada", "Cuba", "British Guiana", "British Honduras", "Saint-Domingue",
      "Texas", "Georgia", "Rio de la Plata"
    ),
    iso3 = c(
      "JAM", "BRA", "BRA", "BRA", "BRB",
      "USA", "DMA", "BRA", "VIR", "USA",
      "USA", "ATG", "PRI", "KNA", "USA",
      "GRD", "CUB", "GUY", "BLZ", "HTI",
      "USA", "USA", "ARG"
    ),
    modern_name = c(
      "Jamaica", "Brazil", "Brazil", "Brazil", "Barbados",
      "United States", "Dominica", "Brazil", "U.S. Virgin Islands", "United States",
      "United States", "Antigua and Barbuda", "Puerto Rico", "Saint Kitts and Nevis", "United States",
      "Grenada", "Cuba", "Guyana", "Belize", "Haiti",
      "United States", "United States", "Argentina"
    ),
    map_quality = c(rep("region_exact_or_admin", 22), "region_approx"),
    notes = "Intra-American purchase region mapped to modern country or territory."
  ),
  data.table(
    source_column = c(
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port", "dest_port", "dest_port",
      "dest_port", "dest_port", "dest_port"
    ),
    label = c(
      "Rio de Janeiro", "Havana", "New Orleans", "Portobelo", "Cartagena",
      "Buenos Aires", "Montevideo", "Charleston", "La Guaira", "Galveston",
      "Santiago de Cuba", "São Luis", "Veracruz", "San Juan", "Curaçao",
      "New York", "Savannah", "Barbados, place unspecified", "Jamaica, place unspecified", "Guadeloupe, place unspecified",
      "Puerto Rico, port unspecified", "Saint-Domingue, then Haiti, port unspecified", "Antigua, place unspecified"
    ),
    iso3 = c(
      "BRA", "CUB", "USA", "PAN", "COL",
      "ARG", "URY", "USA", "VEN", "USA",
      "CUB", "BRA", "MEX", "PRI", "CUW",
      "USA", "USA", "BRB", "JAM", "GLP",
      "PRI", "HTI", "ATG"
    ),
    modern_name = c(
      "Brazil", "Cuba", "United States", "Panama", "Colombia",
      "Argentina", "Uruguay", "United States", "Venezuela", "United States",
      "Cuba", "Brazil", "Mexico", "Puerto Rico", "Curacao",
      "United States", "United States", "Barbados", "Jamaica", "Guadeloupe",
      "Puerto Rico", "Haiti", "Antigua and Barbuda"
    ),
    map_quality = rep("exact_port", 23),
    notes = "Specific intra-American destination port mapped to modern country or territory."
  ),
  data.table(
    source_column = c(
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region",
      "dest_region", "dest_region", "dest_region", "dest_region", "dest_region"
    ),
    label = c(
      "Southeast Brazil", "Cuba", "Gulf coast", "Rio de la Plata", "South Carolina",
      "Texas", "Puerto Rico", "Saint-Domingue", "Amazonia", "Virginia",
      "Spanish Americas", "Georgia", "Jamaica", "Antigua", "Barbados",
      "British Guiana", "Dominica", "St Vincent", "Bahamas", "Guadeloupe"
    ),
    iso3 = c(
      "BRA", "CUB", "USA", "ARG", "USA",
      "USA", "PRI", "HTI", "BRA", "USA",
      NA_character_, "USA", "JAM", "ATG", "BRB",
      "GUY", "DMA", "VCT", "BHS", "GLP"
    ),
    modern_name = c(
      "Brazil", "Cuba", "United States", "Argentina", "United States",
      "United States", "Puerto Rico", "Haiti", "Brazil", "United States",
      NA_character_, "United States", "Jamaica", "Antigua and Barbuda", "Barbados",
      "Guyana", "Dominica", "Saint Vincent and the Grenadines", "Bahamas", "Guadeloupe"
    ),
    map_quality = c(
      rep("region_exact_or_admin", 10),
      "unmapped",
      rep("region_exact_or_admin", 9)
    ),
    notes = c(
      rep("Intra-American destination region mapped to modern country or territory.", 10),
      "Spanish Americas retained as unmapped because it is too broad for a conservative country assignment.",
      rep("Intra-American destination region mapped to modern country or territory.", 9)
    )
  )
), use.names = TRUE, fill = TRUE)

map_dict <- unique(map_dict, by = c("source_column", "label"))

extract_observed_labels <- function(dt, dataset) {
  rbindlist(list(
    data.table(dataset = dataset, direction = "origin", source_column = "origin_place", label = dt$origin_place, disembarked_imp = dt$disembarked_imp),
    data.table(dataset = dataset, direction = "origin", source_column = "origin_region", label = dt$origin_region, disembarked_imp = dt$disembarked_imp),
    data.table(dataset = dataset, direction = "origin", source_column = "origin_broad", label = dt$origin_broad, disembarked_imp = dt$disembarked_imp),
    data.table(dataset = dataset, direction = "destination", source_column = "dest_port", label = dt$dest_port, disembarked_imp = dt$disembarked_imp),
    data.table(dataset = dataset, direction = "destination", source_column = "dest_region", label = dt$dest_region, disembarked_imp = dt$disembarked_imp),
    data.table(dataset = dataset, direction = "destination", source_column = "dest_broad", label = dt$dest_broad, disembarked_imp = dt$disembarked_imp)
  ), use.names = TRUE, fill = TRUE)[!is.na(label), .(disembarked_imp = sum(disembarked_imp, na.rm = TRUE)), by = .(dataset, direction, source_column, label)]
}

apply_mapping <- function(dt) {
  place_map <- map_dict[source_column == "origin_place", .(label, iso3, modern_name, map_quality, notes)]
  region_map <- map_dict[source_column == "origin_region", .(label, iso3, modern_name, map_quality, notes)]
  broad_map <- map_dict[source_column == "origin_broad", .(label, iso3, modern_name, map_quality, notes)]
  port_map <- map_dict[source_column == "dest_port", .(label, iso3, modern_name, map_quality, notes)]
  dest_region_map <- map_dict[source_column == "dest_region", .(label, iso3, modern_name, map_quality, notes)]
  dest_broad_map <- map_dict[source_column == "dest_broad", .(label, iso3, modern_name, map_quality, notes)]

  dt[place_map, on = .(origin_place = label), `:=`(
    origin_iso3_place = i.iso3,
    origin_name_place = i.modern_name,
    origin_quality_place = i.map_quality
  )]
  dt[region_map, on = .(origin_region = label), `:=`(
    origin_iso3_region = i.iso3,
    origin_name_region = i.modern_name,
    origin_quality_region = i.map_quality
  )]
  dt[broad_map, on = .(origin_broad = label), `:=`(
    origin_iso3_broad = i.iso3,
    origin_name_broad = i.modern_name,
    origin_quality_broad = i.map_quality
  )]
  dt[port_map, on = .(dest_port = label), `:=`(
    dest_iso3_port = i.iso3,
    dest_name_port = i.modern_name,
    dest_quality_port = i.map_quality
  )]
  dt[dest_region_map, on = .(dest_region = label), `:=`(
    dest_iso3_region = i.iso3,
    dest_name_region = i.modern_name,
    dest_quality_region = i.map_quality
  )]
  dt[dest_broad_map, on = .(dest_broad = label), `:=`(
    dest_iso3_broad = i.iso3,
    dest_name_broad = i.modern_name,
    dest_quality_broad = i.map_quality
  )]

  dt[, origin_iso3 := fcoalesce(origin_iso3_place, origin_iso3_region, origin_iso3_broad)]
  dt[, origin_name := fcoalesce(origin_name_place, origin_name_region, origin_name_broad)]
  dt[, origin_map_quality := fcoalesce(origin_quality_place, origin_quality_region, origin_quality_broad)]
  dt[, origin_map_source := fifelse(!is.na(origin_iso3_place), "origin_place",
                             fifelse(!is.na(origin_iso3_region), "origin_region",
                             fifelse(!is.na(origin_iso3_broad), "origin_broad", NA_character_)))]

  dt[, dest_iso3 := fcoalesce(dest_iso3_port, dest_iso3_region, dest_iso3_broad)]
  dt[, dest_name := fcoalesce(dest_name_port, dest_name_region, dest_name_broad)]
  dt[, dest_map_quality := fcoalesce(dest_quality_port, dest_quality_region, dest_quality_broad)]
  dt[, dest_map_source := fifelse(!is.na(dest_iso3_port), "dest_port",
                           fifelse(!is.na(dest_iso3_region), "dest_region",
                           fifelse(!is.na(dest_iso3_broad), "dest_broad", NA_character_)))]

  dt[]
}

build_corridors <- function(dt) {
  out <- dt[!is.na(origin_iso3) & !is.na(dest_iso3) & !is.na(disembarked_imp) & disembarked_imp > 0,
            .(
              slave_flow_disembarked = sum(disembarked_imp, na.rm = TRUE),
              slave_flow_embarked = sum(embarked_imp, na.rm = TRUE),
              n_voyages = .N,
              exact_origin_share = mean(origin_map_quality == "exact_place", na.rm = TRUE),
              exact_dest_share = mean(dest_map_quality %in% c("exact_port", "exact_place"), na.rm = TRUE),
              first_year = suppressWarnings(min(year_arrived, na.rm = TRUE)),
              last_year = suppressWarnings(max(year_arrived, na.rm = TRUE))
            ),
            by = .(
              dataset, origin_iso3, origin_name, dest_iso3, dest_name
            )]
  out[, crossborder := origin_iso3 != dest_iso3]
  out[order(-crossborder, -slave_flow_disembarked)]
}

coverage_stats <- function(dt) {
  total_dis <- dt[, sum(disembarked_imp, na.rm = TRUE)]
  data.table(
    dataset = unique(dt$dataset),
    total_disembarked = total_dis,
    mapped_origin_disembarked = dt[!is.na(origin_iso3), sum(disembarked_imp, na.rm = TRUE)],
    mapped_dest_disembarked = dt[!is.na(dest_iso3), sum(disembarked_imp, na.rm = TRUE)],
    mapped_both_disembarked = dt[!is.na(origin_iso3) & !is.na(dest_iso3), sum(disembarked_imp, na.rm = TRUE)],
    mapped_origin_share = dt[!is.na(origin_iso3), sum(disembarked_imp, na.rm = TRUE)] / total_dis,
    mapped_dest_share = dt[!is.na(dest_iso3), sum(disembarked_imp, na.rm = TRUE)] / total_dis,
    mapped_both_share = dt[!is.na(origin_iso3) & !is.na(dest_iso3), sum(disembarked_imp, na.rm = TRUE)] / total_dis
  )
}

quality_stats <- function(dt) {
  dt[!is.na(origin_iso3) & !is.na(dest_iso3) & !is.na(disembarked_imp) & disembarked_imp > 0,
     .(disembarked_imp = sum(disembarked_imp, na.rm = TRUE)),
     by = .(dataset, origin_map_source, origin_map_quality, dest_map_source, dest_map_quality)][
       order(-disembarked_imp)
     ]
}

top_unmapped <- function(dt, source_column_name, top_n = 15) {
  dt_local <- copy(dt)
  target_col <- source_column_name
  mapped_col <- if (source_column_name %in% c("origin_place", "origin_region", "origin_broad")) "origin_iso3" else "dest_iso3"
  direction <- if (source_column_name %in% c("origin_place", "origin_region", "origin_broad")) "origin" else "destination"

  out <- dt_local[is.na(get(mapped_col)) & !is.na(get(target_col)),
                  .(disembarked_imp = sum(disembarked_imp, na.rm = TRUE), voyages = .N),
                  by = .(dataset, label = get(target_col))][order(-disembarked_imp)][1:top_n]
  out[, `:=`(direction = direction, source_column = source_column_name)]
  setcolorder(out, c("dataset", "direction", "source_column", "label", "disembarked_imp", "voyages"))
  out[!is.na(dataset) & !is.na(label)][]
}

ta <- read_slavevoyages(file.path(data_dir, "trans-atlantic3.csv"), "trans_atlantic3")
ia <- read_slavevoyages(file.path(data_dir, "intra-american.csv"), "intra_american")

ta <- apply_mapping(ta)
ia <- apply_mapping(ia)

observed_crosswalk <- rbindlist(list(
  extract_observed_labels(ta, "trans_atlantic3"),
  extract_observed_labels(ia, "intra_american")
), use.names = TRUE, fill = TRUE)

crosswalk_full <- merge(
  observed_crosswalk,
  map_dict,
  by = c("source_column", "label"),
  all.x = TRUE
)

crosswalk_full[is.na(iso3), `:=`(
  modern_name = NA_character_,
  map_quality = "unmapped",
  notes = "No conservative country assignment yet."
)]

setorder(crosswalk_full, dataset, source_column, -disembarked_imp, label)

ta_corridors <- build_corridors(ta)
ia_corridors <- build_corridors(ia)

fwrite(crosswalk_full, file.path(script_dir, "slavevoyages_country_crosswalk.csv"))
fwrite(ta_corridors, file.path(gen_dir, "slave_corridors_transatlantic_country.csv"))
fwrite(ia_corridors, file.path(gen_dir, "slave_corridors_intraamerican_country.csv"))
fwrite(ta_corridors[crossborder == TRUE], file.path(gen_dir, "slave_corridors_transatlantic_country_crossborder.csv"))
fwrite(ia_corridors[crossborder == TRUE], file.path(gen_dir, "slave_corridors_intraamerican_country_crossborder.csv"))
fwrite(rbindlist(list(quality_stats(ta), quality_stats(ia)), use.names = TRUE, fill = TRUE),
       file.path(gen_dir, "slavevoyages_mapping_quality.csv"))
fwrite(ta_corridors[1:20], file.path(gen_dir, "slave_corridors_transatlantic_top20.csv"))
fwrite(ia_corridors[crossborder == TRUE][1:20], file.path(gen_dir, "slave_corridors_intraamerican_top20_crossborder.csv"))

coverage <- rbindlist(list(coverage_stats(ta), coverage_stats(ia)), use.names = TRUE, fill = TRUE)
quality_tbl <- rbindlist(list(quality_stats(ta), quality_stats(ia)), use.names = TRUE, fill = TRUE)

unmapped_labels <- rbindlist(list(
  top_unmapped(ta, "origin_place"),
  top_unmapped(ta, "origin_region"),
  top_unmapped(ta, "dest_port"),
  top_unmapped(ta, "dest_region"),
  top_unmapped(ia, "origin_place"),
  top_unmapped(ia, "origin_region"),
  top_unmapped(ia, "dest_port"),
  top_unmapped(ia, "dest_region")
), use.names = TRUE, fill = TRUE)

md <- c(
  "# Slave Voyages Diagnostics",
  "",
  "This file was generated by `generate_slavevoyages_corridors.R`.",
  "",
  "## Coverage",
  ""
)

for (i in seq_len(nrow(coverage))) {
  row <- coverage[i]
  md <- c(
    md,
    paste0("### ", row$dataset),
    "",
    paste0("- Total disembarked volume: ", fmt_num(row$total_disembarked)),
    paste0("- Origin mapped volume: ", fmt_num(row$mapped_origin_disembarked), " (", fmt_pct(row$mapped_origin_share), ")"),
    paste0("- Destination mapped volume: ", fmt_num(row$mapped_dest_disembarked), " (", fmt_pct(row$mapped_dest_share), ")"),
    paste0("- Origin and destination both mapped: ", fmt_num(row$mapped_both_disembarked), " (", fmt_pct(row$mapped_both_share), ")"),
    ""
  )
}

md <- c(md, "## Top mapped cross-border corridors", "")

top_ta <- ta_corridors[crossborder == TRUE][1:15]
top_ia <- ia_corridors[crossborder == TRUE][1:15]

md <- c(md, "### trans_atlantic3", "", "| origin | destination | disembarked | voyages |", "|---|---|---:|---:|")
for (i in seq_len(nrow(top_ta))) {
  row <- top_ta[i]
  md <- c(md, paste0("| ", row$origin_iso3, " | ", row$dest_iso3, " | ", fmt_num(row$slave_flow_disembarked), " | ", row$n_voyages, " |"))
}

md <- c(md, "", "### intra_american", "", "| origin | destination | disembarked | voyages |", "|---|---|---:|---:|")
for (i in seq_len(nrow(top_ia))) {
  row <- top_ia[i]
  md <- c(md, paste0("| ", row$origin_iso3, " | ", row$dest_iso3, " | ", fmt_num(row$slave_flow_disembarked), " | ", row$n_voyages, " |"))
}

md <- c(md, "", "## Largest unmapped labels", "")
for (dataset_name in na.omit(unique(unmapped_labels$dataset))) {
  sub <- unmapped_labels[dataset == dataset_name][1:20]
  md <- c(md, paste0("### ", dataset_name), "", "| direction | source column | label | disembarked | voyages |", "|---|---|---|---:|---:|")
  for (i in seq_len(nrow(sub))) {
    row <- sub[i]
    md <- c(md, paste0("| ", row$direction, " | ", row$source_column, " | ", row$label, " | ", fmt_num(row$disembarked_imp), " | ", row$voyages, " |"))
  }
  md <- c(md, "")
}

md <- c(md, "## Mapping quality", "")
for (dataset_name in unique(quality_tbl$dataset)) {
  sub <- quality_tbl[dataset == dataset_name][1:12]
  md <- c(md, paste0("### ", dataset_name), "", "| origin source | origin quality | destination source | destination quality | disembarked |", "|---|---|---|---|---:|")
  for (i in seq_len(nrow(sub))) {
    row <- sub[i]
    md <- c(md, paste0("| ", row$origin_map_source, " | ", row$origin_map_quality, " | ", row$dest_map_source, " | ", row$dest_map_quality, " | ", fmt_num(row$disembarked_imp), " |"))
  }
  md <- c(md, "")
}

writeLines(md, file.path(gen_dir, "slavevoyages_diagnostics.md"))

cat("Generated files:\n")
cat(file.path(script_dir, "slavevoyages_country_crosswalk.csv"), "\n")
cat(file.path(gen_dir, "slave_corridors_transatlantic_country.csv"), "\n")
cat(file.path(gen_dir, "slave_corridors_intraamerican_country.csv"), "\n")
cat(file.path(gen_dir, "slavevoyages_diagnostics.md"), "\n")
