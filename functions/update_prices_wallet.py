import os
import pandas as pd
from datetime import datetime
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
import time

print("🔄 Updating Wallet Data")

# Folder for wallet data
wallet_folder = "./data/wallet"
files = [f for f in os.listdir(wallet_folder) if "BTC" in f and "info" not in f]

# Set up headless Chrome (GitHub Actions compatible)
chrome_path = os.getenv("CHROME_PATH", "/usr/bin/google-chrome")
options = Options()
options.add_argument("--headless=new")
options.add_argument("--no-sandbox")
options.add_argument("--disable-gpu")
options.binary_location = chrome_path

driver = webdriver.Chrome(options=options)

for filename in files:
    filepath = os.path.join(wallet_folder, filename)

    try:
        # Load existing data
        olddata = pd.read_csv(filepath, parse_dates=['datum'])
        olddata['preis'] = pd.to_numeric(olddata['preis'], errors='coerce')
        olddata = olddata.dropna(subset=['datum', 'preis'])
        olddata['datum'] = olddata['datum'].dt.date  # normalize to datetime.date

        # Remove today's row (in case we want to replace it)
        today = datetime.today().date()
        olddata = olddata[olddata['datum'] != today]

        if olddata.empty:
            print(f"⚠️ Warning: {filename} has no remaining rows after removing today.")
            continue

        maxdate = max(olddata['datum'])
        print(f"📅 Latest date in {filename} (excluding today): {maxdate}")

        # Scrape price data from btcdirect.eu
        driver.get("https://btcdirect.eu/de-at/bitcoin-kurs")
        time.sleep(5)

        soup = BeautifulSoup(driver.page_source, 'html.parser')
        table = soup.find('table')

        if not table:
            raise ValueError("No table found on btcdirect.eu")

        rows = table.find_all('tr')[1:]
        new_rows = []

        for row in rows:
            cols = [td.get_text(strip=True) for td in row.find_all('td')]
            if len(cols) < 2:
                continue
            try:
                datum = datetime.strptime(cols[0], "%d.%m.%Y").date()
                preis = float(cols[1].replace("€", "").replace(".", "").replace(",", "."))
            except Exception:
                continue

            if datum > maxdate or datum == today:
                new_rows.append({'datum': datum, 'preis': preis})

        if new_rows:
            newdata = pd.DataFrame(new_rows)
            updated = pd.concat([newdata, olddata], ignore_index=True)
            updated = updated.sort_values(by="datum", ascending=False)
            updated.to_csv(filepath, index=False)
            print(f"✅ Successfully updated {filename} with {len(new_rows)} new rows.")
        else:
            print(f"⏩ No new data found for {filename}")

    except Exception as e:
        print(f"❌ ERROR in {filename}: {e}")

driver.quit()
