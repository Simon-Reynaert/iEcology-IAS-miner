import unittest
from unittest.mock import patch, MagicMock
import builtins
import sys
import os

# Add src folder to path
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..', 'src')))
from EASIN_mining_and_map_generation.EASIN_API_credentials_registration import register_user

class TestRegisterUser(unittest.TestCase):

    @patch.dict("os.environ", {"EASIN_EMAIL": "test@example.com", "EASIN_PW": "password"})
    @patch("builtins.input", return_value="")  # Simulate pressing ENTER
    @patch("requests.post")
    def test_registration_success_email_confirmation(self, mock_post, mock_input):
        # Mock successful API response including "email confirmation"
        mock_resp = MagicMock()
        mock_resp.status_code = 200
        mock_resp.json.return_value = {"message": "Please confirm your email with the activation link"}
        mock_post.return_value = mock_resp

        register_user()
        mock_post.assert_called_once()
        mock_input.assert_called_once()  # Ensures the script waits for user confirmation

    @patch.dict("os.environ", {"EASIN_EMAIL": "test@example.com", "EASIN_PW": "password"})
    @patch("builtins.input", return_value="")
    @patch("requests.post")
    def test_registration_success_no_email_keyword(self, mock_post, mock_input):
        # Mock 200 OK but no "email confirmation" keywords
        mock_resp = MagicMock()
        mock_resp.status_code = 200
        mock_resp.json.return_value = {"message": "Registration completed"}
        mock_post.return_value = mock_resp

        register_user()
        mock_post.assert_called_once()
        mock_input.assert_called_once()

    @patch.dict("os.environ", {"EASIN_EMAIL": "test@example.com", "EASIN_PW": "password"})
    @patch("requests.post")
    def test_registration_user_already_exists(self, mock_post):
        mock_resp = MagicMock()
        mock_resp.status_code = 409
        mock_resp.json.return_value = {"error": "User exists"}
        mock_post.return_value = mock_resp

        register_user()
        mock_post.assert_called_once()

    @patch.dict("os.environ", {"EASIN_EMAIL": "test@example.com", "EASIN_PW": "password"})
    @patch("requests.post")
    def test_registration_invalid_data(self, mock_post):
        mock_resp = MagicMock()
        mock_resp.status_code = 400
        mock_resp.json.return_value = {"error": "Invalid data"}
        mock_post.return_value = mock_resp

        register_user()
        mock_post.assert_called_once()

    @patch.dict("os.environ", {"EASIN_EMAIL": "test@example.com", "EASIN_PW": "password"})
    @patch("requests.post", side_effect=Exception("Network error"))
    def test_registration_request_exception(self, mock_post):
        register_user()
        mock_post.assert_called_once()

    @patch("dotenv.load_dotenv")
    @patch('EASIN_mining_and_map_generation.EASIN_API_credentials_registration.os.getenv', side_effect=[None, None])
    @patch("requests.post") 
    def test_missing_env_vars_exit(self, mock_post, mock_getenv, mock_load_dotenv):
        with self.assertRaises(SystemExit):
            register_user()

if __name__ == "__main__":
    unittest.main()
