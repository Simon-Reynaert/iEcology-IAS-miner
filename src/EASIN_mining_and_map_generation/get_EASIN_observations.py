# =====================================================================================================
# Script Name: fetch_easin_occurrences.py
# Description: Modular script to fetch invasive species occurrence data from the EASIN API.
#              Includes optional start/end date filtering for the API call.
# Author: Simon Reynaert
# =====================================================================================================

import os
import sys
import time
import json
import requests
import pandas as pd
from dotenv import load_dotenv
from requests.exceptions import RequestException
from tqdm import tqdm
from typing import List, Dict, Tuple, Set, Union, Optional

# =====================================================================================================
# 1. CONFIGURATION & CONSTANTS
# =====================================================================================================

# EASIN API endpoint for occurrence data retrieval
EASIN_OCCURRENCES_URL = "https://easin.jrc.ec.europa.eu/apixg2/geo/getoccurrences"

# Maximum number of records to retrieve per API request (pagination limit)
TAKE_LIMIT = 10000

# Maximum number of retry attempts for failed API requests
MAX_RETRIES = 5

# Default output file path for occurrence data
default_ouput_file = "easin_occurrences_EU.csv"

# Default input file containing species list with EASIN IDs
default_species_file = "UnionList_Species_Traits_85_present.csv"

# Fixed output column structure - guarantees consistent CSV schema across runs
# This prevents dynamic field expansion and ensures data consistency
BASE_OUTPUT_COLUMNS = [
    "EASIN_ID",       # Unique species identifier in EASIN database
    "ScientificName", # Binomial nomenclature of the species
    "Country",        # ISO 2-letter country code where observation occurred
    "Latitude",       # Geographic coordinate (degrees North)
    "Longitude",      # Geographic coordinate (degrees East)
    "DataPartner",    # Organization or database providing the observation
    "Date",           # Observation date (granular date or year)
    "ObservationId",  # Unique identifier for the observation record
    "Reference",      # Citation or reference for the observation
    "ReferenceUrl",   # URL link to the source reference
    "Timestamp"       # System timestamp of when record was added/updated
]

# European Union member states and territories (ISO 3166-1 alpha-2 codes)
# XI = Northern Ireland (special status post-Brexit)
EU_COUNTRIES = [
    "AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "EL",
    "HU", "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK",
    "SI", "ES", "SE", "XI"
]


def get_credentials() -> Tuple[str, str]:
    """
    Retrieve EASIN API credentials from environment variables.
    
    Expects credentials to be stored in a .env file in the project root:
    - EASIN_EMAIL: Email address for EASIN account
    - EASIN_PW: Password for EASIN account
    
    Returns
    -------
    Tuple[str, str]
        Email and password as strings.
    
    Raises
    ------
    ValueError
        If either credential is missing from environment variables.
    
    Notes
    -----
    Credentials are required for all EASIN API requests.
    Never commit .env files to version control.
    """
    load_dotenv()
    email = os.getenv("EASIN_EMAIL")
    password = os.getenv("EASIN_PW")
    if not email or not password:
        raise ValueError("❌ Please set EASIN_EMAIL and EASIN_PW in your .env file.")
    return email, password


# =====================================================================================================
# 2. COORDINATE & DATE EXTRACTION UTILITIES
# =====================================================================================================

def extract_coordinates(record: Dict) -> Tuple[Union[float, None], Union[float, None]]:
    """
    Parse geographic coordinates from WKT (Well-Known Text) format in API response.
    
    Parameters
    ----------
    record : Dict
        API response record containing occurrence data.
    
    Returns
    -------
    Tuple[Union[float, None], Union[float, None]]
        Latitude and longitude as floats, or (None, None) if parsing fails.
    
    Notes
    -----
    - WKT format used: "POINT (longitude latitude)"
    - Standard geographic coordinate system (EPSG:4326/WGS84)
    - Returns None values rather than raising exceptions for robustness
    
    Examples
    --------
    >>> record = {"WKT": "POINT (4.3517 50.8503)"}
    >>> extract_coordinates(record)
    (50.8503, 4.3517)
    """
    latitude, longitude = None, None

    # Check if WKT field exists and is not empty
    if "WKT" in record and record["WKT"]:
        try:
            wkt = record["WKT"]
            # WKT POINT format: "POINT (longitude latitude)"
            if wkt.startswith("POINT"):
                # Remove "POINT" prefix and parentheses
                coords_part = wkt.replace("POINT", "").strip().strip("()")
                coords = coords_part.split()
                if len(coords) == 2:
                    # Note: WKT convention is (longitude, latitude), opposite of typical usage
                    longitude, latitude = float(coords[0]), float(coords[1])
        except (ValueError, IndexError):
            # Silently handle malformed coordinate data
            pass

    return latitude, longitude


def extract_best_observation_date(record: Dict) -> Union[str, int, None]:
    """
    Extract the most granular available date information from an observation record.
    
    Implements a fallback hierarchy to maximize temporal precision:
    1. EventDate (ISO 8601 format date/datetime)
    2. DateCollected (collection date)
    3. Observation_Date (observation-specific date)
    4. Year (fallback to year only)
    
    Parameters
    ----------
    record : Dict
        API response record containing occurrence data.
    
    Returns
    -------
    Union[str, int, None]
        Date string (ISO format), year as integer, or None if no date available.
    
    Notes
    -----
    - Prioritizes full dates over year-only data for temporal analysis
    - Year field is considered the primary reliable field in EASIN API
    - Returns None rather than raising exceptions for missing data
    
    Examples
    --------
    >>> record = {"EventDate": "2023-05-15", "Year": 2023}
    >>> extract_best_observation_date(record)
    "2023-05-15"
    
    >>> record = {"Year": 2020}
    >>> extract_best_observation_date(record)
    2020
    """
    # List of fields to check, in order of preference (most to least granular)
    granular_date_fields = ["EventDate", "DateCollected", "Observation_Date"]
    
    for field in granular_date_fields:
        if field in record and record[field]:
            return record[field]

    # Fallback to year if no granular date is available
    if "Year" in record and record["Year"]:
        return record["Year"]
    
    return None


# =====================================================================================================
# 3. API DATA FETCHING
# =====================================================================================================

def fetch_occurrences(
    species_id: Union[int, str],
    country_code: str,
    email: str,
    password: str,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None
) -> List[Dict]:
    """
    Retrieve all occurrence records for a species-country combination via paginated API requests.
    
    Implements pagination, retry logic, and optional temporal filtering.
    
    Parameters
    ----------
    species_id : Union[int, str]
        EASIN species identifier.
    country_code : str
        ISO 3166-1 alpha-2 country code (e.g., "BE", "FR").
    email : str
        EASIN account email for authentication.
    password : str
        EASIN account password for authentication.
    start_date : Optional[str], default=None
        Start date filter in format "YYYY-MM-DD". If None, no start date limit.
    end_date : Optional[str], default=None
        End date filter in format "YYYY-MM-DD". If None, no end date limit.
    
    Returns
    -------
    List[Dict]
        List of occurrence records as dictionaries. Empty list if no data or on failure.
    
    Notes
    -----
    - Implements automatic pagination to retrieve all available records
    - Exponential backoff retry strategy for transient network errors
    - Rate limiting: 1 second delay between successful requests
    - Date filters are applied server-side via API parameters
    
    API Payload Structure
    ---------------------
    - Email, Password: Authentication credentials
    - speciesId: Target species EASIN ID
    - countryCode: Geographic filter
    - skip, take: Pagination parameters
    - FromDate, ToDate: Optional temporal filters
    
    Error Handling
    --------------
    - Network errors trigger up to MAX_RETRIES attempts with increasing delays
    - After max retries, function returns partial results (graceful degradation)
    - Empty API responses ({"Empty": ...}) signal end of available data
    
    Examples
    --------
    >>> records = fetch_occurrences(
    ...     species_id=12345,
    ...     country_code="BE",
    ...     email="user@example.com",
    ...     password="secret",
    ...     start_date="2020-01-01",
    ...     end_date="2023-12-31"
    ... )
    """
    all_records = []
    skip = 0  # Pagination offset
    retries = 0  # Current retry attempt counter

    while True:
        # Construct API request payload
        payload = {
            "Email": email,
            "Password": password,
            "speciesId": str(species_id),
            "countryCode": country_code,
            "dataPartners": "",  # Empty = all data partners
            "excludePartners": 0,  # 0 = include all partners
            "skip": skip,
            "take": TAKE_LIMIT,
        }

        # Add optional date range filters if provided
        if start_date:
            payload["FromDate"] = start_date
        if end_date:
            payload["ToDate"] = end_date

        try:
            # Execute POST request to EASIN API
            response = requests.post(
                EASIN_OCCURRENCES_URL,
                json=payload,
                headers={"Content-Type": "application/json"},
                timeout=60,  # 60 second timeout
            )
            response.raise_for_status()  # Raise exception for 4xx/5xx status codes
            data = response.json()

            # Check for empty result indicator from API
            if isinstance(data, dict) and "Empty" in data:
                break
            
            # Filter out non-dictionary records (data quality assurance)
            if isinstance(data, list):
                data = [rec for rec in data if isinstance(rec, dict)]
            
            # No data returned = end of pagination
            if not data:
                break

            all_records.extend(data)
            time.sleep(1)  # Rate limiting: 1 request per second

            # Check if we've retrieved all available records
            if len(data) < TAKE_LIMIT:
                break
            
            # Move to next page
            skip += TAKE_LIMIT
            retries = 0  # Reset retry counter on success

        except RequestException:
            # Handle network/API errors with retry logic
            if retries < MAX_RETRIES:
                retries += 1
                wait_time = 3 * retries  # Linear backoff: 3, 6, 9, 12, 15 seconds
                tqdm.write(
                    f"⚠️ Error fetching {species_id} in {country_code}, "
                    f"retry {retries}/{MAX_RETRIES} in {wait_time}s..."
                )
                time.sleep(wait_time)
                continue
            else:
                # Max retries exceeded - log and move on
                tqdm.write(
                    f"❌ Skipping {species_id} in {country_code} "
                    f"after {MAX_RETRIES} failed retries."
                )
                break

    return all_records


# =====================================================================================================
# 4. DATA PROCESSING & STORAGE (Simplified for Fixed Columns)
# =====================================================================================================

def create_initial_output_file(output_file: str) -> List[str]:
    """
    Initialize output CSV file with fixed column headers if it doesn't exist.
    
    Parameters
    ----------
    output_file : str
        Path to the output CSV file.
    
    Returns
    -------
    List[str]
        List of column names (BASE_OUTPUT_COLUMNS).
    
    Notes
    -----
    - Creates file only if it doesn't exist (safe for resume operations)
    - Validates existing file by attempting to read header
    - Fixed schema prevents column drift across multiple runs
    
    File Structure
    --------------
    Creates CSV with headers but no data rows initially.
    """
    if not os.path.exists(output_file):
        # Create new file with header row only
        header_df = pd.DataFrame(columns=BASE_OUTPUT_COLUMNS)
        header_df.to_csv(output_file, index=False)
        print(
            f"✅ Created new output file '{output_file}' "
            f"with {len(BASE_OUTPUT_COLUMNS)} fixed fields."
        )
    else:
        try:
            # Verify existing file is readable and has correct columns
            existing_df = pd.read_csv(output_file, nrows=0)
            existing_columns = list(existing_df.columns)
            
            # Check if columns match expected schema
            if existing_columns != BASE_OUTPUT_COLUMNS:
                print(
                    f"⚠️ Could not read existing output file header: "
                    f"Column mismatch (expected {BASE_OUTPUT_COLUMNS}, got {existing_columns}). "
                    f"Resume may be unreliable."
                )
        except Exception as e:
            print(
                f"⚠️ Could not read existing output file header: {e}. "
                f"Resume may be unreliable."
            )

    return BASE_OUTPUT_COLUMNS


def save_records_to_csv(
    species_id: Union[int, str],
    species_records: List[Dict],
    output_file: str
):
    """
    Process and append occurrence records to CSV file with explicit field mapping.
    
    Transforms raw API response records into the standardized output schema,
    handling coordinate extraction, date parsing, and field mapping.
    
    Parameters
    ----------
    species_id : Union[int, str]
        EASIN species identifier for the records.
    species_records : List[Dict]
        Raw occurrence records from API response.
    output_file : str
        Path to output CSV file.
    
    Notes
    -----
    - Explicit field mapping ensures data consistency
    - Handles missing/malformed data gracefully (None values)
    - Deduplicates records before appending
    - Creates placeholder record if no occurrences exist (maintains species list)
    
    Field Mapping
    -------------
    API Field          -> Output Column
    -----------------------------------------
    SpeciesName        -> ScientificName
    CountryId          -> Country
    WKT                -> Latitude, Longitude (parsed)
    DataPartnerName    -> DataPartner
    Year/EventDate     -> Date (best available)
    ObservationId      -> ObservationId
    Reference          -> Reference
    Url                -> ReferenceUrl
    Timestamp          -> Timestamp
    
    Placeholder Logic
    -----------------
    If a species has zero occurrences, a single row with EASIN_ID populated
    and all other fields as None is added. This preserves the complete species
    list in the output file.
    """
    formatted_records = []

    if species_records:
        # Process each occurrence record
        for rec in species_records:
            if isinstance(rec, dict):
                # Extract geographic coordinates from WKT format
                latitude, longitude = extract_coordinates(rec)
                
                # Get best available date (granular date or year)
                observation_date = extract_best_observation_date(rec)
                
                # Explicit mapping from API fields to output schema
                record_dict = {
                    "EASIN_ID": species_id,
                    "ScientificName": rec.get("SpeciesName"),
                    "Country": rec.get("CountryId"),
                    "Latitude": latitude,
                    "Longitude": longitude,
                    "DataPartner": rec.get("DataPartnerName") or rec.get("DataPartnerId"),
                    "Date": observation_date,
                    "ObservationId": rec.get("ObservationId"),
                    "Reference": rec.get("Reference"),
                    "ReferenceUrl": rec.get("Url"),
                    "Timestamp": rec.get("Timestamp")
                }
                
                formatted_records.append(record_dict)
            else:
                # Warn about unexpected data types
                tqdm.write(
                    f"⚠️ Unexpected record type for species {species_id}: {type(rec)}"
                )
    else:
        # No occurrences found - create placeholder record
        placeholder = {col: None for col in BASE_OUTPUT_COLUMNS}
        placeholder["EASIN_ID"] = species_id
        formatted_records.append(placeholder)

    # Convert to DataFrame with fixed column order
    df = pd.DataFrame(formatted_records, columns=BASE_OUTPUT_COLUMNS)
    
    # Remove duplicate records (can occur from API pagination overlaps)
    df.drop_duplicates(inplace=True)
    
    # Append to CSV (mode='a'), without writing header (header=False)
    df.to_csv(output_file, mode="a", index=False, header=False)


# =====================================================================================================
# 5. WORKFLOW MANAGER
# =====================================================================================================

def get_processed_ids(output_file: str) -> Set[Union[int, str]]:
    """
    Identify species already processed in the output file to enable resume functionality.
    
    Parameters
    ----------
    output_file : str
        Path to the output CSV file.
    
    Returns
    -------
    Set[Union[int, str]]
        Set of unique EASIN_IDs already present in the output file.
    
    Notes
    -----
    - Enables interrupted runs to resume without reprocessing
    - Reads only EASIN_ID column for efficiency
    - Returns empty set if file doesn't exist or can't be read
    - Resume logic is bypassed when date filters are active (fresh data needed)
    
    Examples
    --------
    >>> processed = get_processed_ids("output.csv")
    ℹ️ Resuming: 42 species already processed in output.csv.
    >>> len(processed)
    42
    """
    if os.path.exists(output_file):
        try:
            # Read only EASIN_ID column for memory efficiency
            existing_df = pd.read_csv(output_file, usecols=["EASIN_ID"])
            
            # Extract unique species IDs, excluding NaN values
            processed_ids = set(existing_df["EASIN_ID"].dropna().unique())
            
            print(
                f"ℹ️ Resuming: {len(processed_ids)} species already processed "
                f"in {output_file}."
            )
            return processed_ids
        except Exception as e:
            print(
                f"⚠️ Could not read existing output file for resume logic: {e}\n"
                f"Starting fresh..."
            )
            return set()
    return set()


def run_easin_fetcher(
    species_file: str = default_species_file,
    output_file: str = default_ouput_file,
    countries: List[str] = EU_COUNTRIES,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
) -> int:
    """
    Main orchestration function for fetching EASIN occurrence data.
    
    Coordinates the complete data acquisition workflow:
    1. Initialize output file with fixed schema
    2. Load species list and authenticate with API
    3. Iterate through species and countries with progress tracking
    4. Fetch, process, and save occurrence data
    5. Report summary statistics
    
    Parameters
    ----------
    species_file : str, default=default_species_file
        Path to CSV containing species list with 'EASIN.ID' column.
    output_file : str, default=default_ouput_file
        Path where occurrence data CSV will be saved.
    countries : List[str], default=EU_COUNTRIES
        List of ISO 3166-1 alpha-2 country codes to query.
    start_date : Optional[str], default=None
        Start date filter (YYYY-MM-DD format). If None, no temporal filter.
    end_date : Optional[str], default=None
        End date filter (YYYY-MM-DD format). If None, no temporal filter.
    
    Returns
    -------
    int
        Total number of occurrence records saved to file.
    
    Raises
    ------
    ValueError
        If credentials are missing or species file lacks required column.
    FileNotFoundError
        If species_file doesn't exist.
    
    Notes
    -----
    - Resume functionality automatically skips already-processed species
    - Resume is disabled when date filters are active (ensures fresh data)
    - Progress bars show real-time status for species and countries
    - Rate limiting: 0.5s between countries, 1s between species
    - Gracefully handles API errors without stopping entire process
    
    Performance Considerations
    --------------------------
    - Processing time scales with: num_species × num_countries × occurrences
    - Large datasets (>1M records) may take hours to complete
    - Intermediate results are saved continuously (safe to interrupt)
    
    Examples
    --------
    >>> # Fetch all data for EU species
    >>> total = run_easin_fetcher()
    
    >>> # Fetch data for specific date range
    >>> total = run_easin_fetcher(
    ...     start_date="2020-01-01",
    ...     end_date="2023-12-31"
    ... )
    
    >>> # Custom species list and countries
    >>> total = run_easin_fetcher(
    ...     species_file="my_species.csv",
    ...     countries=["BE", "NL", "LU"]
    ... )
    """
    start_time = time.time()
    
    # Initialize output file with fixed column schema
    output_columns = create_initial_output_file(output_file)

    # Retrieve API credentials from environment
    email, password = get_credentials()

    # Load species list from CSV
    print(f"🔄 Loading species data from {species_file}...")
    try:
        species_df = pd.read_csv(species_file)
        if "EASIN.ID" not in species_df.columns:
            raise ValueError("❌ Species file missing 'EASIN.ID' column.")
    except FileNotFoundError:
        raise FileNotFoundError(f"❌ CSV file '{species_file}' not found.")

    # Extract unique species IDs
    easin_ids = species_df["EASIN.ID"].dropna().unique()
    print(f"🔍 Found {len(easin_ids)} unique species IDs to process.")

    # Check for already-processed species (resume functionality)
    # Note: Resume logic disabled when date filters are active
    processed_ids = get_processed_ids(output_file)
    total_records_saved = 0

    # Display date filter information
    date_filter_info = (
        f" ({start_date or 'Start'} to {end_date or 'End'})"
        if start_date or end_date
        else ""
    )
    print(f"🔎 Date Filter: {date_filter_info}")

    # Main processing loop with progress tracking
    species_progress = tqdm(
        easin_ids,
        desc="🦎 Processing species",
        unit="species",
        colour="green"
    )

    for species_id in species_progress:
        # Skip if already processed (unless date filters are active)
        if species_id in processed_ids and not (start_date or end_date):
            species_progress.set_postfix({"Status": "Skipped", "Species": species_id})
            continue

        # Collect occurrences across all countries for this species
        all_country_records = []
        country_progress = tqdm(
            countries,
            desc=f"🌍 Countries for {species_id}",
            leave=False,
            colour="blue"
        )

        for country_code in country_progress:
            country_progress.set_postfix({"Country": country_code})
            
            # Fetch occurrences for this species-country combination
            country_records = fetch_occurrences(
                species_id, country_code, email, password, start_date, end_date
            )
            all_country_records.extend(country_records)
            
            # Rate limiting between countries
            time.sleep(0.5)

        # Save all records for this species to CSV
        records_in_batch = len(all_country_records)
        save_records_to_csv(species_id, all_country_records, output_file)
        total_records_saved += records_in_batch

        # Log progress
        tqdm.write(f"✅ Saved {records_in_batch} records for species {species_id}")
        species_progress.set_postfix({"Status": "Saved", "Records": total_records_saved})
        
        # Rate limiting between species
        time.sleep(1)

    # Calculate and display summary statistics
    elapsed_time = time.time() - start_time
    print("\n" + "=" * 80)
    print("🎉 DATA FETCHING COMPLETED!")
    print(f"📊 Processed: {len(easin_ids)} species")
    print(f"🌍 Searched: {len(countries)} countries")
    print(f"💾 Total records saved: {total_records_saved:,}")
    print(f"⏱️ Total time: {elapsed_time/60:.2f} minutes")
    print(f"📁 Output file: {output_file}")
    print(f"📋 Columns: {', '.join(output_columns)}")
    print("=" * 80)

    return total_records_saved


# =====================================================================================================
# SCRIPT ENTRY POINT
# =====================================================================================================

def main():
    """
    Command-line entry point wrapper.
    
    Executes run_easin_fetcher() with default parameters.
    Can be extended to parse command-line arguments for custom configuration.
    """
    run_easin_fetcher()


if __name__ == "__main__":
    print("=" * 80)
    print("🦎 EASIN Species Occurrence Data Fetcher (Fixed Output)")
    print("=" * 80)

    try:
        main()
    except (ValueError, FileNotFoundError) as e:
        # Handle expected errors gracefully
        print(e, file=sys.stderr)
        sys.exit(1)
    except KeyboardInterrupt:
        # Handle user interruption (Ctrl+C)
        print("\n\n⚠️ Process interrupted by user.")
        print("✅ Data saved up to this point. You can resume by running the script again.")
        sys.exit(0)
    except Exception as e:
        # Catch-all for unexpected errors
        print(f"\n❌ Unexpected error: {e}")
        sys.exit(1)