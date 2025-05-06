#!/usr/bin/env python3
import os
import re
import time
import base64
import requests
import pandas as pd
from urllib.parse import urlencode
from dotenv import load_dotenv
from datetime import datetime, timezone

# ─── Config & Constants ───────────────────────────────────────────────────
OUT_CSV       = "invasive_live_items_ebay_all_eu.csv"
PAGE_SZ       = 50
BROWSE_API    = "https://api.ebay.com/buy/browse/v1/item_summary/search"
TOKEN_URL     = "https://api.ebay.com/identity/v1/oauth2/token"
CLIENT_SCOPE  = "https://api.ebay.com/oauth/api_scope"

# ─── European marketplace IDs ──────────────────────────────────────────────
MARKETPLACES = [
    "EBAY_DE", "EBAY_FR", "EBAY_GB", "EBAY_IT", "EBAY_ES",
    "EBAY_NL", "EBAY_BEFR", "EBAY_BENL", "EBAY_AT", "EBAY_PL",
    "EBAY_IE", "EBAY_CZ", "EBAY_SE", "EBAY_PT", "EBAY_GR",
    "EBAY_DK", "EBAY_FI", "EBAY_HU", "EBAY_RO", "EBAY_SK"
]

LOCATION_FILTER = "itemLocationRegion:CONTINENTAL_EUROPE"

def write_rows_incrementally(rows):
    df = pd.DataFrame(rows)
    write_header = not os.path.isfile(OUT_CSV)
    df.to_csv(OUT_CSV, mode="a", header=write_header, index=False)

def get_app_token():
    client_id     = os.getenv("EBAY_CLIENT_ID")
    client_secret = os.getenv("EBAY_CLIENT_SECRET")
    if not client_id or not client_secret:
        raise RuntimeError("Set EBAY_CLIENT_ID & EBAY_CLIENT_SECRET in .env")

    creds = f"{client_id}:{client_secret}".encode()
    b64   = base64.b64encode(creds).decode()
    headers = {
        "Content-Type": "application/x-www-form-urlencoded",
        "Authorization": f"Basic {b64}"
    }
    data = {"grant_type":"client_credentials","scope":CLIENT_SCOPE}
    r = requests.post(TOKEN_URL, headers=headers, data=urlencode(data))
    r.raise_for_status()
    return r.json()["access_token"]

def fetch_european_listings(token, species, global_seen):
    patt = re.compile(rf"\b{re.escape(species)}\b", re.IGNORECASE)
    all_rows = []

    for mkt in MARKETPLACES:
        headers = {
            "Authorization": f"Bearer {token}",
            "X-EBAY-C-MARKETPLACE-ID": mkt
        }
        offset = 0

        while True:
            params = {
                "q":       species,
                "filter":  LOCATION_FILTER,
                "limit":   PAGE_SZ,
                "offset":  offset,
                "sort":    "RELEVANCE"
            }
            resp = requests.get(BROWSE_API, headers=headers, params=params)

            # 400 → past last page, 409 → filter unsupported → stop this market
            if resp.status_code in (400, 409):
                print(f" ⚠️ {mkt} offset/filter unsupported (status {resp.status_code}), skipping.")
                break

            resp.raise_for_status()
            items = resp.json().get("itemSummaries", [])
            if not items:
                break

            new_count = 0
            for it in items:
                if not patt.search(it.get("title","")):
                    continue
                iid = it["itemId"]
                if iid not in global_seen:
                    global_seen.add(iid)
                    all_rows.append({
                        "marketplace":    mkt,
                        "species":        species,
                        "itemId":         iid,
                        "title":          it.get("title"),
                        "startDate":      it.get("itemCreationDate"),
                        "price_value":    it.get("price", {}).get("value"),
                        "price_currency": it.get("price", {}).get("currency"),
                        "listingCountry": it.get("itemLocation", {}).get("country"),
                        "condition":      it.get("condition")
                    })
                    new_count += 1

            if new_count == 0 or len(items) < PAGE_SZ:
                break
            offset += PAGE_SZ
            time.sleep(0.1)

    return all_rows

def main():
    load_dotenv()
    if os.path.isfile(OUT_CSV):
        os.remove(OUT_CSV)

    df = pd.read_csv("list_of_union_concern.csv")
    species_list = df["Scientific Name"].dropna().str.strip().tolist()

    token = get_app_token()
    print(f"[{datetime.now(timezone.utc):%Y-%m-%d %H:%M UTC}] Token obtained")

    global_seen = set()
    for sp in species_list:
        print(f"▶️ {sp} …", end="", flush=True)
        rows = fetch_european_listings(token, sp, global_seen)
        write_rows_incrementally(rows)
        print(f" {len(rows)} saved")

    print(f"\n✅ Done — see {OUT_CSV}")

if __name__ == "__main__":
    main()
