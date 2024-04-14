import csv
import requests
from bs4 import BeautifulSoup

# URL of the page to scrape
url = "https://www.cia.gov/the-world-factbook/about/archives/2022/field/ethnic-groups/"

# Send a GET request to the URL
response = requests.get(url)

# Parse the HTML content
soup = BeautifulSoup(response.text, "html.parser")

# Open CSV file for writing
with open("cia-factbook-ethnic-groups.csv", "w", newline="", encoding="utf-8") as csvfile:
    writer = csv.writer(csvfile)
    writer.writerow(["Country", "Ethnic group"])  # Write header row

    # Find all <li> elements
    countries = soup.find_all("li")

    # Iterate over each <li> element
    for country in countries:
        # Extract country name from <h2 class="h3">
        h2_element = country.find("h2", class_="h3")
        country_name = h2_element.text.strip() if h2_element else None

        # Extract ethnic group from <p>
        p_element = country.find("p")
        ethnic_group = p_element.text.strip() if p_element else None

        # Write to CSV only if both country name and ethnic group are not None
        if country_name is not None and ethnic_group is not None:
            writer.writerow([country_name, ethnic_group])

print("Scraping and writing to CSV completed.")
