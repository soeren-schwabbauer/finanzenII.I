import os
import pandas as pd
from datetime import datetime
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from bs4 import BeautifulSoup
import time

print("🔄 Updating Depot data...")

# Skip update on weekends
#today = datetime.today()
#if today.weekday() >= 5:
#    print("⏩ Weekend – no update needed.")
#    exit()

# Mapping ISINs to Investing.com URLs
isin_url_map = {
    "IE00BK5BQT80": "https://www.investing.com/etfs/vanguard-ftse-all-world-ucits-acc-historical-data?cid=1148060",
    "IE00B5BMR087": "https://www.investing.com/etfs/cs-etf-(ie)-on-s-p-500-historical-data?cid=45844",
    "LU0908500753": "https://www.investing.com/etfs/lyxor-stoxx-europe-600-dr-c-historical-data?cid=1156753"
}

# List all CSV files in ./data/depot (excluding "info" files)
data_folder = "./data/depot"
files = [f for f in os.listdir(data_folder) if f.endswith(".csv") and "info" not in f]

# Set up headless Chrome
chrome_path = os.getenv("CHROME_PATH", "/usr/bin/google-chrome")
options = Options()
options.add_argument("--headless=new")
options.add_argument("--no-sandbox")
options.add_argument("--disable-gpu")
options.binary_location = chrome_path

driver = webdriver.Chrome(options=options)

for filename in files:
    filepath = os.path.join(data_folder, filename)

    # Find matching ISIN
    isin = next((key for key in isin_url_map if key in filename), None)
    if not isin:
        print(f"⏩ No matching ISIN for {filename}, skipping.")
        continue

    try:
        # Load existing data
        olddata = pd.read_csv(filepath, parse_dates=['datum'])
        olddata['preis'] = pd.to_numeric(olddata['preis'], errors='coerce')
        olddata = olddata.dropna(subset=['datum', 'preis'])

        # Normalize date column to date only
        olddata['datum'] = olddata['datum'].dt.date

        if len(olddata) > 0:
            olddata = olddata.iloc[1:]  # Remove most recent row
        else:
            print(f"⚠️ Warning: {filename} has no usable data.")
            continue

        maxdate = max(olddata['datum'])

        # Fetch new data
        driver.get(isin_url_map[isin])
        time.sleep(5)

        soup = BeautifulSoup(driver.page_source, 'html.parser')
        table = soup.find_all('table')[1]

        rows = table.find_all('tr')[1:]
        new_rows = []

        for row in rows:
            cols = [td.get_text(strip=True) for td in row.find_all('td')]
            if len(cols) < 2:
                continue
            try:
                datum = datetime.strptime(cols[0], "%b %d, %Y").date()
                preis = float(cols[1].replace(",", ""))
            except Exception:
                continue

            if datum > maxdate:
                new_rows.append({'datum': datum, 'preis': preis})

        if new_rows:
            newdata = pd.DataFrame(new_rows)
            updated = pd.concat([newdata, olddata], ignore_index=True)
            updated = updated.sort_values(by="datum", ascending=False)
            updated.to_csv(filepath, index=False)
            print(f"✅ Successfully updated {filename}")
        else:
            print(f"⏩ {filename} is already up-to-date")

    except Exception as e:
        print(f"❌ Error processing {filename}: {e}")

driver.quit()
