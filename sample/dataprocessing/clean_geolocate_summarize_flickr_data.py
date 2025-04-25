import pandas as pd
from reverse_geocoder import search as rg_search
import pycountry
from tqdm import tqdm
import multiprocessing

# Define tags that indicate the photo should be excluded
EXCLUDE_TAGS = {
    'ibis hotel', 'hotel', 'antwerpenzoo', 'zoo', 'historisch', 'kölnzoo','foodporn',
    'building', 'architecture', 'statue', 'sculpture', 'historical', 'rugby',
    'church', 'antique', 'cosplay', 'captive', 'aquarium', 'museum', 'captivity',
    'caged', 'captivebred', 'pet', 'artificial', 'poster', 'drawing','berlinaquarium',
}

def cc_to_name(cc):
    country = pycountry.countries.get(alpha_2=cc)
    return country.name if country else cc

def main():
    # ─── Load & clean ─────────────────────────────────────────────────────────
    input_file = 'flickr_species_observations_eu_combined_latin.csv'
    df = pd.read_csv(input_file)
    df['tags']     = df['tags'].fillna('')
    df['tag_list'] = df['tags'].str.lower().str.split()
    df = df[~df['tag_list'].apply(lambda tags: any(tag in EXCLUDE_TAGS for tag in tags))]
    df['date_only'] = pd.to_datetime(df['date_taken']).dt.date.astype(str)
    df['tag_set']   = df['tag_list'].apply(lambda tags: set(t for t in tags if not t.startswith('ngid')))
    df = df.sort_values(by='date_taken')

    # ─── De-duplication (fast, grouped) ───────────────────────────────────────
    keep_indices = []
    for date, group in tqdm(df.groupby('date_only'),
                            total=df['date_only'].nunique(),
                            desc="Filtering duplicates by date"):
        seen_local = set()
        idxs      = group.index.tolist()
        tag_sets  = group['tag_set'].tolist()

        for pos, i in enumerate(idxs):
            if i in seen_local:
                continue
            keep_indices.append(i)
            tags_i = tag_sets[pos]
            for j_pos in range(pos + 1, len(idxs)):
                j = idxs[j_pos]
                if j in seen_local:
                    continue
                tags_j = tag_sets[j_pos]
                union  = tags_i | tags_j
                if union and len(tags_i & tags_j) / len(union) >= 0.7:
                    seen_local.add(j)

    df_filtered = (
        df.loc[keep_indices]
          .drop(columns=['date_only', 'tag_set', 'tag_list'])
          .reset_index(drop=True)
    )

    # ─── super-fast batch reverse-geocoding (one shot) ────────────────────────

    # build a single tuple-of-tuples of all your coords
    coords = tuple(zip(df_filtered['latitude'], df_filtered['longitude']))

    # batch-search once in single-threaded mode (no multiprocessing)
    rg_results = rg_search(coords, mode=1, verbose=False)

    # unpack the ISO codes / country names
    df_filtered['detected_cc']      = [r['cc'] for r in rg_results]
    df_filtered['detected_country'] = df_filtered['detected_cc'].apply(cc_to_name)


    # ─── Save ────────────────────────────────────────────────────────────────
    output_file = 'filtered_clean_photos_flickr_latin.csv'
    df_filtered.to_csv(output_file, index=False)
    print(f"Filtered data saved to: {output_file}")

if __name__ == "__main__":
    multiprocessing.freeze_support()
    main()
