import pandas as pd
from reverse_geocoder import search as rg_search
import pycountry
from tqdm import tqdm
import multiprocessing
import math

EU_SCHENGEN_EFTA_COUNTRIES = {
    'AT', 'BE', 'BG', 'HR', 'CY', 'CZ', 'DK', 'EE', 'FI', 'FR',
    'DE', 'GR', 'HU', 'IE', 'IT', 'LV', 'LT', 'LU', 'MT', 'NL',
    'PL', 'PT', 'RO', 'SK', 'SI', 'ES', 'SE',  # EU
    'IS', 'LI', 'NO', 'CH','GB'                    # EFTA / Schengen / UK
}

def cc_to_name(cc):
    country = pycountry.countries.get(alpha_2=cc)
    return country.name if country else cc

DISTANCE_THRESHOLD_METERS = 100.0

def haversine(lat1, lon1, lat2, lon2):
    R = 6371000
    phi1, phi2 = math.radians(lat1), math.radians(lat2)
    delta_phi = math.radians(lat2 - lat1)
    delta_lambda = math.radians(lon2 - lon1)

    a = math.sin(delta_phi / 2.0) ** 2 + \
        math.cos(phi1) * math.cos(phi2) * math.sin(delta_lambda / 2.0) ** 2
    c = 2 * math.atan2(math.sqrt(a), math.sqrt(1 - a))
    return R * c

def main():
    INPUT_FILE = 'flickr_species_observations_eu_combined_latin_normtag_2004-now.csv'
    OUTPUT_FILE = 'deduplicated_geocoded_flickr_2004-now.csv'

    df = pd.read_csv(INPUT_FILE)
    df = df.dropna(subset=['latitude', 'longitude'])

    # Preprocess
    df['tags'] = df['tags'].fillna('')
    df['tag_list'] = df['tags'].str.lower().str.split()
    df['tag_set'] = df['tag_list'].apply(lambda tags: set(t for t in tags if not t.startswith('ngid')))
    df['timestamp'] = pd.to_datetime(df['date_taken'], errors='coerce')
    df['date_only'] = df['timestamp'].dt.date.astype(str)

    keep_indices = []

    total_rows = len(df)
    with tqdm(total=total_rows, desc="Deduplicating rows") as pbar:
        for date, group in df.groupby('date_only'):
            seen_local = set()
            idxs = group.index.tolist()
            tag_sets = group['tag_set'].tolist()
            latitudes = group['latitude'].tolist()
            longitudes = group['longitude'].tolist()
            timestamps = group['timestamp'].tolist()

            for pos, i in enumerate(idxs):
                pbar.update(1)  # Update progress bar per row processed
                if i in seen_local:
                    continue
                keep_indices.append(i)
                tags_i = tag_sets[pos]
                lat_i = latitudes[pos]
                lon_i = longitudes[pos]
                time_i = timestamps[pos]

                for j_pos in range(pos + 1, len(idxs)):
                    j = idxs[j_pos]
                    if j in seen_local:
                        continue
                    tags_j = tag_sets[j_pos]
                    lat_j = latitudes[j_pos]
                    lon_j = longitudes[j_pos]
                    time_j = timestamps[j_pos]

                    union = tags_i | tags_j
                    if not union:
                        continue
                    similarity = len(tags_i & tags_j) / len(union)
                    distance = haversine(lat_i, lon_i, lat_j, lon_j)
                    time_diff = abs((time_i - time_j).total_seconds()) / 3600.0

                    if similarity >= 0.7 and distance < DISTANCE_THRESHOLD_METERS and time_diff <= 1.0:
                        seen_local.add(j)

    df_filtered = (
        df.loc[keep_indices]
          .drop(columns=['tag_list', 'tag_set', 'date_only'])
          .reset_index(drop=True)
    )

    coords = tuple(zip(df_filtered['latitude'], df_filtered['longitude']))
    rg_results = rg_search(coords, mode=1, verbose=False)

    df_filtered['detected_cc'] = [r['cc'] for r in rg_results]
    df_filtered['detected_country'] = df_filtered['detected_cc'].apply(cc_to_name)

    df_europe = df_filtered[df_filtered['detected_cc'].isin(EU_SCHENGEN_EFTA_COUNTRIES)].reset_index(drop=True)

    df_europe.to_csv(OUTPUT_FILE, index=False)
    print(f"Deduplicated & geocoded European data saved to: {OUTPUT_FILE}")
    print(f"Number of rows in final CSV: {len(df_europe)}")

if __name__ == "__main__":
    import multiprocessing
    multiprocessing.freeze_support()
    main()