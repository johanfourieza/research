"""
Classify Cape Colony farm names by language origin (Dutch, English, French, German)
and assign each farm polygon to a GADM2 district.
Output: data/farms_by_district.csv
"""
import fiona
from shapely.geometry import shape, Point
import re, csv, os
from collections import defaultdict, Counter

data_dir = "C:/Users/johanf/Dropbox/0Claude0/1Research/Fourie_Meta/Data"

# ============================================================
# 1. Language classification rules
# ============================================================

# Dutch/Afrikaans morphemes (very common in Cape farm names)
DUTCH_SUFFIXES = [
    "fontein", "kloof", "berg", "berge", "kop", "koppen", "kraal",
    "rivier", "poort", "vlei", "vlakte", "hoek", "nek", "drift",
    "bos", "bosch", "dal", "veld", "pan", "mond", "spruit",
    "plaat", "plaats", "plats", "gat", "krans", "rand",
    "heuwel", "heuvel", "hoogte", "laagte", "ruggens", "rug",
    "kuil", "kuyl", "dam", "klip", "klippen", "rif",
    "stroom", "wagt", "wacht", "drif", "dorp",
    "bult", "bulte", "kolk", "put", "puts", "draai",
    "vlak", "grond", "baai", "eiland",
]

DUTCH_PREFIXES = [
    "groot", "klein", "lang", "kort", "breede", "breed", "oud", "oude",
    "nieuwe", "zwart", "wit", "rood", "groen", "blouw", "blauw",
    "booven", "boven", "onder", "agter", "achter", "voor",
    "ooster", "wester", "noorder", "zuider",
    "harte", "hart", "buffels", "elands", "leeuwen", "leeuw",
    "oliphants", "olifants", "rhenoster", "rhenosters",
    "doorn", "palmiet", "boontjies", "aalwyn", "alewys",
]

DUTCH_WORDS = {
    "de", "het", "van", "aan", "op", "in", "bij", "bij",
    "welgelegen", "welgemoed", "welgevonden", "weltevreden",
    "geluk", "genoeg", "vergenoegd", "vergenoeg",
    "uitkyk", "uitkijk", "uitzigt", "uitzicht",
    "allesverloren", "alleengelaten", "alleengelaaten",
    "nooitgedacht", "nooitgedagt", "altydgedacht", "altijdgedacht",
    "goedehoop", "goede", "hoop", "moed", "rust",
    "vrede", "vreed", "vreede", "eenzaamheid",
    "afgunst", "aanstoot", "onverwacht", "onverwagt",
    "eikeboomen", "eikenboom", "doornboom",
    "zandvliet", "rietvlei", "rietvalley", "rietvallei",
    "ongelegen", "verlegen", "vogelvalley",
    "toekenningsgebied", "bosreservaat",
    "klipheuvel", "moddergat", "dassenklip",
    "groenland", "bergland", "zoutpan",
    "fortuin", "fortuyn", "landskroon", "lekkerwyn",
    "seesig", "waterpoel", "roggeland", "vlakkeland",
    "wagenmakersvallei", "halfmanshof",
    "morgenzon", "dorpsmeent", "morgenster",
}

# English morphemes
ENGLISH_SUFFIXES = [
    "field", "ford", "wood", "woods", "dale", "vale",
    "hill", "hills", "bury", "grove", "park", "side",
    "stead", "shire", "wick", "ham", "ton",
    "bridge", "well", "spring", "springs", "mount",
    "meadow", "meadows", "view", "glen", "hollow",
    "place", "hall", "house", "lodge", "farm",
    "castle", "tower", "point", "retreat", "prospect",
    "villa",
]

ENGLISH_PREFIXES = [
    "glen", "upper", "lower", "new", "old", "east", "west",
    "north", "south", "great", "little", "mount",
    "pine", "oak", "elm", "ash",
]

ENGLISH_WORDS = {
    "the", "fountain", "valley", "river", "flat", "flats",
    "retreat", "prospect", "paradise", "pleasant",
    "happy", "content", "rest", "peace",
    "victoria", "albert", "alfred", "edward", "george",
    "wellington", "somerset", "bathurst", "grahamstown",
    "highland", "highlands", "lowland",
    "wilderness", "haven", "harbour",
    "sunshine", "morning", "evening",
    "garden", "orchard", "grove",
    "ben", "etive", "loch", "glen",  # Scottish
    "table", "round", "square",
    "almonds", "almond",
}

# French morphemes (Huguenot heritage)
FRENCH_SUFFIXES = [
    "ville", "mont", "bois", "champs", "lieu",
    "maison", "chateau",
]

# NOTE: "la"/"le" only matched as standalone words (see classify function)
FRENCH_PREFIXES = [
    "les", "des", "du",
    "beau", "belle", "bon", "bonne",
    "grand", "petit", "vieux",
    "saint", "sainte",
    "val",
]

FRENCH_WORDS = {
    "languedoc", "champagne", "normandie", "provence",
    "cabriere", "rhone", "loire",
    "plaisir", "joie", "repos",
    "sans", "avec", "entre",
    "chateau", "maison", "terre",
    "bien", "donne", "fait",
    "deux", "trois",
    "rouge", "blanc", "noir", "vert",
    "fontaine",
}

# German morphemes
# NOTE: "-burg" and "-hof" removed — shared with Dutch and nearly always
# Dutch in the Cape Colony context. Genuine German names are in KNOWN_GERMAN.
GERMAN_SUFFIXES = [
    "stein", "wald", "heim", "dorf", "thal", "tal",
    "eck", "fels",
]

GERMAN_WORDS = {
    "alt", "neu", "ober", "unter", "mittel",
    "schwarz", "weiss", "gross",
    "altona", "hamburg", "berlin", "dresden",
    "frieden", "freude", "hoffnung",
    "wittenberg", "heidelberg",
    "lubeck", "wandsbeck", "whupperthal", "lichtenstein",
}

# Known classifications for ambiguous/proper names
KNOWN_ENGLISH = {
    "ben etive", "glen avon", "glen lynden", "glen craig",
    "fort beaufort", "fort brown", "fort willshire",
    "somerset", "bathurst", "grahamstown", "salem",
    "port elizabeth", "port alfred", "port beaufort",
    "martindale", "springfield", "richmond",
    "willowmore", "cradock", "aberdeen",
    "colesberg", "albert",
    "table farm", "round hill", "sidbury",
    "fairfield", "thornfield", "brookfield",
    "highland", "belmont", "claremont",
    "rosedale", "oakdale", "ferndale",
    "woodlands", "woodstock", "newlands",
    "greenfield", "greenwood", "oakfield",
    "pinewood", "ashburn", "thornhill",
    "sunnydale", "sunnyside",
}

KNOWN_FRENCH = {
    "la motte", "la provence", "la bri", "la cotte",
    "la couronne", "la gratitude", "la plaisante",
    "la concorde", "la chasseur", "la paix",
    "le chasseur", "le valeur", "le plaisir",
    "l'arc", "bien donne", "champagne",
    "languedoc", "cabriere", "bourgogne",
    "normandie", "rhone", "mont rochelle",
    "belle ombre",
}

KNOWN_GERMAN = {
    "altona", "hamburg", "klein hamburg", "berlin",
    "wittenberg", "heidelberg", "saxenburg",
}


def classify_farm_name(raw_name):
    """Classify a farm name as Dutch, English, French, or German."""
    name = raw_name.lower().strip()
    # Remove numbers and common suffixes
    name = re.sub(r'\d+', '', name).strip()
    name = re.sub(r'\s*ag\s*$', '', name).strip()
    name = re.sub(r'\s*\(.*?\)', '', name).strip()

    if not name:
        return "Dutch"  # default for unnamed

    # Check known lists first
    for known in KNOWN_FRENCH:
        if known in name:
            return "French"
    for known in KNOWN_ENGLISH:
        if known in name:
            return "English"
    for known in KNOWN_GERMAN:
        if known in name:
            return "German"

    # Score-based classification
    scores = {"Dutch": 0, "English": 0, "French": 0, "German": 0}

    words = re.split(r'[\s\-]+', name)

    # Check suffixes — only match as standalone word endings (not embedded in compound)
    name_lower = name.lower()
    for s in DUTCH_SUFFIXES:
        if name_lower.endswith(s) or any(w.endswith(s) for w in words):
            scores["Dutch"] += 2
    for s in ENGLISH_SUFFIXES:
        # Only match if it's a standalone word OR the word before is also English-ish
        if any(w == s or (w.endswith(s) and len(w) > len(s) + 2) for w in words):
            scores["English"] += 2
    for s in FRENCH_SUFFIXES:
        if name_lower.endswith(s) or any(w.endswith(s) for w in words):
            scores["French"] += 2
    for s in GERMAN_SUFFIXES:
        if name_lower.endswith(s) or any(w.endswith(s) for w in words):
            scores["German"] += 2

    # Check prefixes — only match standalone words, not compound word starts
    for p in DUTCH_PREFIXES:
        if any(w == p or (w.startswith(p) and len(w) > len(p)) for w in words):
            scores["Dutch"] += 1.5
    for p in ENGLISH_PREFIXES:
        if any(w == p for w in words):  # standalone only
            scores["English"] += 1.5
    for p in FRENCH_PREFIXES:
        if any(w == p for w in words):  # standalone only
            scores["French"] += 1.5
    # Special handling: "la"/"le" as French only when standalone first word
    if len(words) >= 2 and words[0] in ("la", "le"):
        scores["French"] += 2

    # Check word matches
    for w in words:
        if w in DUTCH_WORDS:
            scores["Dutch"] += 2
        if w in ENGLISH_WORDS:
            scores["English"] += 2
        if w in FRENCH_WORDS:
            scores["French"] += 2
        if w in GERMAN_WORDS:
            scores["German"] += 2

    # Dutch-specific orthographic markers
    dutch_markers = ["ij", "oe", "aa", "ee", "oo", "uu", "ei", "ui",
                     "sch", "ght", "kw"]
    for m in dutch_markers:
        if m in name_lower:
            scores["Dutch"] += 0.3

    # English-specific markers
    if any(c in name_lower for c in ["th", "sh", "tion", "ght"]):
        scores["English"] += 0.3

    # If a word starts with "z" followed by vowel — very Dutch
    if any(w.startswith("z") for w in words):
        scores["Dutch"] += 1

    # "v" at start of word common in Dutch
    if any(w.startswith("v") and len(w) > 2 for w in words):
        scores["Dutch"] += 0.5

    # Default: Dutch (overwhelming majority of Cape farm names are Dutch)
    # Only override if another language scores significantly higher
    max_score = max(scores.values())
    if max_score == 0:
        return "Dutch"

    # Need a meaningful advantage to override the Dutch default
    if scores["Dutch"] >= max_score:
        return "Dutch"

    best = max(scores, key=scores.get)
    # Require non-Dutch to score at least 2 AND beat Dutch by at least 1
    if best != "Dutch" and scores[best] >= 2.0 and scores[best] > scores["Dutch"] + 1.0:
        return best

    return "Dutch"


# ============================================================
# 2. Load farm polygons and compute centroids
# ============================================================

print("Loading farm polygons...")
shp_path = os.path.join(data_dir, "Bewaarders", "Polygons_1850.shp")

farms = []
with fiona.open(shp_path) as src:
    for feat in src:
        name = feat["properties"]["Name"]
        geom = shape(feat["geometry"])
        centroid = geom.centroid

        # Extract farm name (strip "District-Wyk - " prefix)
        parts = name.split(" - ", 1)
        farm_name = parts[1].strip() if len(parts) == 2 else name.strip()
        # Clean: remove trailing numbers and AG
        farm_clean = re.sub(r'\s*\(.*?\)', '', farm_name)
        farm_clean = re.sub(r'\s*AG\s*$', '', farm_clean, flags=re.IGNORECASE)
        farm_clean = re.sub(r'\s*\d[\d,\s]*$', '', farm_clean).strip().rstrip(',').strip()

        # Extract district from folder path
        folder = feat["properties"]["FolderPath"]

        lang = classify_farm_name(farm_clean)

        farms.append({
            "name": name,
            "farm_name": farm_clean,
            "folder": folder,
            "language": lang,
            "lon": centroid.x,
            "lat": centroid.y,
            "area": geom.area,
        })

print(f"Loaded {len(farms)} farm polygons")

# Language distribution
lang_counts = Counter(f["language"] for f in farms)
print(f"\nLanguage classification:")
for lang, n in lang_counts.most_common():
    print(f"  {lang}: {n} ({n/len(farms)*100:.1f}%)")

# Print some examples of each non-Dutch classification
for lang in ["English", "French", "German"]:
    examples = [f["farm_name"] for f in farms if f["language"] == lang][:15]
    print(f"\n{lang} examples: {', '.join(examples)}")


# ============================================================
# 3. Spatial join to GADM2 districts
# ============================================================

print("\nLoading GADM2 district boundaries...")
gadm2_path = os.path.join(data_dir, "za_gadm2.gpkg")
districts = []
with fiona.open(gadm2_path) as src:
    for feat in src:
        districts.append({
            "GID_2": feat["properties"]["GID_2"],
            "NAME_2": feat["properties"]["NAME_2"],
            "geom": shape(feat["geometry"]).buffer(0),
        })

print(f"Loaded {len(districts)} districts")

# Assign each farm to a district
print("Assigning farms to districts...")
matched = 0
unmatched = 0
for farm in farms:
    pt = Point(farm["lon"], farm["lat"])
    farm["GID_2"] = None
    for d in districts:
        if d["geom"].contains(pt):
            farm["GID_2"] = d["GID_2"]
            matched += 1
            break
    if farm["GID_2"] is None:
        # Nearest district within tolerance
        min_dist = float("inf")
        nearest = None
        for d in districts:
            dist = d["geom"].distance(pt)
            if dist < min_dist:
                min_dist = dist
                nearest = d
        if nearest and min_dist < 0.05:
            farm["GID_2"] = nearest["GID_2"]
            matched += 1
        else:
            unmatched += 1

print(f"Matched: {matched}, Unmatched: {unmatched}")

# Which districts have farms?
districts_with_farms = Counter(f["GID_2"] for f in farms if f["GID_2"])
print(f"\nDistricts with farms: {len(districts_with_farms)}")
for gid, n in districts_with_farms.most_common():
    dname = next(d["NAME_2"] for d in districts if d["GID_2"] == gid)
    print(f"  {gid} ({dname}): {n} farms")


# ============================================================
# 4. Aggregate by district
# ============================================================

print("\nAggregating by district...")
district_lang = defaultdict(lambda: {"total": 0, "Dutch": 0, "English": 0, "French": 0, "German": 0})

for farm in farms:
    gid = farm["GID_2"]
    if gid is None:
        continue
    lang = farm["language"]
    district_lang[gid]["total"] += 1
    district_lang[gid][lang] += 1

# Save to CSV
out_path = os.path.join(data_dir, "farm_language_by_district.csv")
with open(out_path, "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["GID_2", "n_farms", "n_dutch", "n_english", "n_french", "n_german",
                "pct_dutch", "pct_english", "pct_french", "pct_german"])
    for gid in sorted(district_lang.keys()):
        d = district_lang[gid]
        total = d["total"]
        w.writerow([
            gid, total, d["Dutch"], d["English"], d["French"], d["German"],
            round(d["Dutch"] / total, 4) if total > 0 else 0,
            round(d["English"] / total, 4) if total > 0 else 0,
            round(d["French"] / total, 4) if total > 0 else 0,
            round(d["German"] / total, 4) if total > 0 else 0,
        ])

print(f"Saved to {out_path}")

# Also save farm-level data for the map
farm_out = os.path.join(data_dir, "farm_classifications.csv")
with open(farm_out, "w", newline="", encoding="utf-8") as f:
    w = csv.writer(f)
    w.writerow(["name", "farm_name", "language", "lon", "lat", "GID_2"])
    for farm in farms:
        w.writerow([farm["name"], farm["farm_name"], farm["language"],
                     round(farm["lon"], 6), round(farm["lat"], 6), farm["GID_2"]])
print(f"Saved farm-level data to {farm_out}")

print("\nDone!")
