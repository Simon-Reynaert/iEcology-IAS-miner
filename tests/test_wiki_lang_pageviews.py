import unittest
import pandas as pd
from unittest.mock import patch, MagicMock, call
import os
import requests

# NOTE: Adjust this import path to your environment
from src.activity_mining.get_wiki_lang_pageviews_final import (
    run_lang_pageviews_fetcher,
    fetch_daily_pageviews,
    _load_existing_data,
    get_wiki_api_headers
)

MODULE_PATH = 'src.activity_mining.get_wiki_lang_pageviews_final'


class TestWikiPageviewsFetcher(unittest.TestCase):

    def setUp(self):
        self.user_agent = "TestUserAgent"
        self.input_file = "test_sitelinks.csv"
        self.output_file = "test_output.csv"
        self.start_date_str = "2023-01-01"
        self.end_date_str = "2023-01-04"
        self.start_date_api = "20230101"
        self.end_date_api = "20230104"
        self.all_date_cols = ["20230101", "20230102", "20230103"]

        # Mock Sitelinks Data
        self.sitelinks_df = pd.DataFrame({
            "Scientific Name": ["Pica pica", "Panthera leo"],
            "Language": ["en", "fr"],
            "Wikipedia Title": ["Pica pica", "Panthera leo"]
        })
        self.standardized_sitelinks_df = pd.DataFrame({
            "Scientific_Name": ["Pica pica", "Panthera leo"],
            "Language": ["en", "fr"],
            "Wikipedia_Title": ["Pica pica", "Panthera leo"]
        })

        # Mock API Response DataFrames (raw, as returned by fetch_daily_pageviews)
        self.mock_df_pica_raw = pd.DataFrame({
            "timestamp": ["2023010100", "2023010100", "2023010200", "2023010300"],
            "views": [50, 50, 120, 80]
        })
        self.mock_df_leo_raw = pd.DataFrame({
            "timestamp": ["2023010100", "2023010200", "2023010300"],
            "views": [50, 70, 90]
        })

    def tearDown(self):
        if os.path.exists(self.output_file):
            os.remove(self.output_file)


class TestHelperFunctions(unittest.TestCase):

    def test_get_wiki_api_headers(self):
        headers = get_wiki_api_headers("MyTestAgent")
        self.assertEqual(headers, {"User-Agent": "MyTestAgent"})

    @patch(f'{MODULE_PATH}.requests.get')
    def test_fetch_daily_pageviews_success(self, mock_get):
        mock_response = MagicMock(status_code=200)
        mock_response.json.return_value = {"items": [{"timestamp": "2023010100", "views": 100}]}
        mock_get.return_value = mock_response

        headers = {"User-Agent": "Test"}
        df = fetch_daily_pageviews("en", "Test_Article", "20230101", "20230102", headers)
        self.assertFalse(df.empty)

        expected_url = (
            "https://wikimedia.org/api/rest_v1/metrics/pageviews/per-article/"
            "en.wikipedia/all-access/user/Test_Article/daily/20230101/20230102"
        )
        mock_get.assert_called_once_with(expected_url, headers=headers)

    @patch(f'{MODULE_PATH}.requests.get')
    @patch('builtins.print')
    def test_fetch_daily_pageviews_404(self, mock_print, mock_get):
        mock_response = MagicMock(status_code=404)
        http_error = requests.exceptions.HTTPError("404 Client Error: Not Found")
        http_error.response = mock_response
        mock_response.raise_for_status.side_effect = http_error
        mock_get.return_value = mock_response

        df = fetch_daily_pageviews("en", "Missing_Page", "20230101", "20230102", {"User-Agent": "Test"})
        self.assertTrue(df.empty)
        mock_print.assert_called_once()
        self.assertIn("404 Not Found: pageviews for en:Missing_Page not available.", mock_print.call_args[0][0])

    @patch(f'{MODULE_PATH}.os.path.exists')
    @patch(f'{MODULE_PATH}.pd.read_csv')
    def test_load_existing_data_success(self, mock_read_csv, mock_exists):
        mock_exists.return_value = True
        mock_df = pd.DataFrame({"col": [1]})
        mock_read_csv.return_value = mock_df

        result_df = _load_existing_data("test.csv")
        self.assertTrue(result_df.equals(mock_df))
        mock_read_csv.assert_called_once_with("test.csv")

    @patch(f'{MODULE_PATH}.os.path.exists')
    def test_load_existing_data_not_found(self, mock_exists):
        mock_exists.return_value = False
        result_df = _load_existing_data("missing.csv")
        self.assertTrue(result_df.empty)


class TestRunLangPageviewsFetcher(TestWikiPageviewsFetcher):

    @patch(f'{MODULE_PATH}.time.sleep')
    @patch(f'{MODULE_PATH}._load_existing_data')
    @patch(f'{MODULE_PATH}.fetch_daily_pageviews')
    @patch(f'{MODULE_PATH}.pd.read_csv')
    @patch(f'{MODULE_PATH}.os.getenv')
    @patch(f'{MODULE_PATH}.load_dotenv')
    def test_full_run_from_scratch(self, mock_load_dotenv, mock_getenv, mock_read_csv,
                                    mock_fetch_daily_pageviews, mock_load_existing_data, mock_sleep):
        mock_getenv.return_value = self.user_agent
        mock_read_csv.return_value = self.sitelinks_df.copy()
        mock_load_existing_data.return_value = pd.DataFrame()

        mock_fetch_daily_pageviews.side_effect = [
            self.mock_df_pica_raw.copy(),
            self.mock_df_leo_raw.copy()
        ]

        captured_dfs = []

        def capture_to_csv(self, *args, **kwargs):
            captured_dfs.append(self)

        with patch('pandas.DataFrame.to_csv', new=capture_to_csv):
            run_lang_pageviews_fetcher(
                start_date=self.start_date_str,
                end_date=self.end_date_str,
                input_file=self.input_file,
                output_file=self.output_file,
            )

        expected_calls = [
            call('en', 'Pica_pica', self.start_date_api, self.end_date_api, {'User-Agent': self.user_agent}),
            call('fr', 'Panthera_leo', self.start_date_api, self.end_date_api, {'User-Agent': self.user_agent}),
        ]
        mock_fetch_daily_pageviews.assert_has_calls(expected_calls, any_order=False)

        final_df = captured_dfs[-1]

        pica_row = final_df[final_df['Scientific_Name'] == 'Pica pica'].iloc[0]
        self.assertEqual(pica_row['20230101'], 100)
        self.assertEqual(pica_row['20230102'], 120)
        self.assertEqual(pica_row['20230103'], 80)

        leo_row = final_df[final_df['Scientific_Name'] == 'Panthera leo'].iloc[0]
        self.assertEqual(leo_row['20230101'], 50)
        self.assertEqual(leo_row['20230102'], 70)
        self.assertEqual(leo_row['20230103'], 90)

    @patch(f'{MODULE_PATH}.time.sleep')
    @patch(f'{MODULE_PATH}._load_existing_data')
    @patch(f'{MODULE_PATH}.fetch_daily_pageviews')
    @patch(f'{MODULE_PATH}.pd.read_csv')
    @patch(f'{MODULE_PATH}.os.getenv')
    @patch(f'{MODULE_PATH}.load_dotenv')
    def test_run_with_resume_and_update(self, mock_load_dotenv, mock_getenv, mock_read_csv,
                                        mock_fetch_daily_pageviews, mock_load_existing_data, mock_sleep):
        mock_getenv.return_value = self.user_agent
        mock_read_csv.return_value = self.sitelinks_df.copy()

        existing_df = self.standardized_sitelinks_df.copy()
        for date_col in self.all_date_cols:
            existing_df[date_col] = 0
        existing_df.loc[0, '20230101'] = 100
        existing_df.loc[0, '20230102'] = 120
        existing_df.loc[1, '20230101'] = 50
        mock_load_existing_data.return_value = existing_df

        mock_df_pica_resume_raw = pd.DataFrame({"timestamp": ["2023010300"], "views": [85]})
        mock_df_leo_resume_raw = pd.DataFrame({"timestamp": ["2023010200", "2023010300"], "views": [75, 95]})

        mock_fetch_daily_pageviews.side_effect = [
            mock_df_pica_resume_raw,
            mock_df_leo_resume_raw
        ]

        captured_dfs = []

        def capture_to_csv(self, *args, **kwargs):
            captured_dfs.append(self)

        with patch('pandas.DataFrame.to_csv', new=capture_to_csv):
            run_lang_pageviews_fetcher(
                start_date=self.start_date_str,
                end_date=self.end_date_str,
                input_file=self.input_file,
                output_file=self.output_file,
            )

        expected_calls = [
            call('en', 'Pica_pica', '20230103', self.end_date_api, {'User-Agent': self.user_agent}),
            call('fr', 'Panthera_leo', '20230102', self.end_date_api, {'User-Agent': self.user_agent}),
        ]
        mock_fetch_daily_pageviews.assert_has_calls(expected_calls, any_order=False)

        final_df = captured_dfs[-1]

        pica_row = final_df[final_df['Scientific_Name'] == 'Pica pica'].iloc[0]
        self.assertEqual(pica_row['20230101'], 100)
        self.assertEqual(pica_row['20230102'], 120)
        self.assertEqual(pica_row['20230103'], 85)

        leo_row = final_df[final_df['Scientific_Name'] == 'Panthera leo'].iloc[0]
        self.assertEqual(leo_row['20230101'], 50)
        self.assertEqual(leo_row['20230102'], 75)
        self.assertEqual(leo_row['20230103'], 95)


if __name__ == '__main__':
    try:
        with open("test_sitelinks.csv", "w") as f:
            f.write("Scientific Name,Language,Wikipedia Title\nPica pica,en,Pica pica\nPanthera leo,fr,Panthera leo")
        unittest.main(exit=False)
    finally:
        if os.path.exists("test_sitelinks.csv"):
            os.remove("test_sitelinks.csv")
