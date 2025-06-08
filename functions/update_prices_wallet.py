import os
import pandas as pd
import requests
from bs4 import BeautifulSoup
from io import StringIO

print("Updating Wallet Data")

files = [f for f in os.listdir("./data/wallet") if "info" not in f and f.endswith(".csv")]
files = [os.path.join("./data/wallet", f) for f in files]

for file in files:
    try:
        print(f"\n🔄 Verarbeite Datei: {file}")

        # Datei laden
        olddata = pd.read_csv(file)
        olddata['datum'] = pd.to_datetime(olddata['datum'])
        olddata['preis'] = pd.to_numeric(olddata['preis'], errors='coerce')

        # Maximaldatum holen und entsprechende Zeile entfernen
        maxdate = olddata['datum'].max()
        print("📅 Entferne Eintrag mit Datum:", maxdate.date())
        olddata = olddata[olddata['datum'] < maxdate]

        # BTC-Daten von Website laden
        url = "https://btcdirect.eu/de-at/bitcoin-kurs"
        resp = requests.get(url)
        soup = BeautifulSoup(resp.text, "html.parser")
        table = soup.find("table")

        if table is None:
            raise ValueError("⚠️ Keine Tabelle auf der Seite gefunden")

        newdata = pd.read_html(StringIO(str(table)))[0]
        newdata.columns = newdata.columns.str.strip()
        newdata = newdata.rename(columns={"Datum": "datum", "Preis": "preis"})

        newdata['datum'] = pd.to_datetime(newdata['datum'], format="%m/%d/%y", errors='coerce')
        newdata['preis'] = newdata['preis'].replace({'€': '', ',': ''}, regex=True).astype(float)
        newdata = newdata[['datum', 'preis']]

        print("🆕 Neue Daten (min–max):", newdata['datum'].min().date(), "-", newdata['datum'].max().date())

        # Nur neue Daten ab entferntem Datum einfügen
        newdata = newdata[newdata['datum'] >= maxdate]

        print("➕ Neue Zeilen:", len(newdata))

        if not newdata.empty:
            updated = pd.concat([newdata, olddata], ignore_index=True)
            updated = updated.sort_values("datum", ascending=False)  # neuestes Datum nach oben
            updated.to_csv(file, index=False)
            print(f"✅ {file} aktualisiert mit {len(newdata)} neuen Zeilen")
        else:
            print("ℹ️ Keine neuen Daten vorhanden")

    except Exception as e:
        print(f"❌ Fehler in {file}: {e}")
