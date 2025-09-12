import os
import sys
import json
import requests
from dotenv import load_dotenv
from requests.exceptions import RequestException

# -------------------------------
# 1. API endpoint
# -------------------------------
REGISTER_URL = "https://easin.jrc.ec.europa.eu/apixg2/auth/register"

# -------------------------------
# 2. Register user
# -------------------------------
def register_user():
    
    """Registers a new user with the EASIN API and checks if a confirmation email was sent."""
    
    load_dotenv()
    EASIN_EMAIL = os.getenv("EASIN_EMAIL")
    EASIN_PW = os.getenv("EASIN_PW")

    if not EASIN_EMAIL or not EASIN_PW:
        sys.exit("❌ Please set EASIN_EMAIL and EASIN_PW in your .env file.")

    payload = {
        "Email": EASIN_EMAIL,
        "Password": EASIN_PW,
        "ConfirmPassword": EASIN_PW
    }
    headers = {"Content-Type": "application/json"}

    print("📡 Sending registration request to EASIN...")

    try:
        response = requests.post(REGISTER_URL, json=payload, headers=headers)

        # Try decoding JSON response, fallback to raw text
        try:
            resp_data = response.json()
        except ValueError:
            resp_data = {"raw_text": response.text}

        if response.status_code == 200:
            resp_text = json.dumps(resp_data).lower()
            print("✅ Registration request sent successfully.")

            # Look for confirmation hints in response
            if any(keyword in resp_text for keyword in ["email", "confirm", "activation", "link"]):
                print("📩 The API indicates a confirmation email has been sent.")
            else:
                print("⚠️ Registration succeeded, but the API response does not explicitly mention email confirmation.")
                print("🔍 Response content:", resp_data)

            # Wait for manual confirmation before exiting
            input("\n⏳ Please confirm your email by clicking the link sent to your inbox.\n"
                  "Press ENTER once you've completed email verification to exit...")

        elif response.status_code == 409:
            print("ℹ️ User already registered. No new registration needed.")
        elif response.status_code == 400:
            print(f"⚠️ Registration failed due to invalid data: {resp_data}")
        else:
            print(f"⚠️ Unexpected response [{response.status_code}]: {resp_data}")

    except Exception as e:  # catch all network errors
        print(f"❌ Registration request failed: {e}")

# -------------------------------
# 3. Run the script
# -------------------------------
if __name__ == "__main__":
    register_user()
