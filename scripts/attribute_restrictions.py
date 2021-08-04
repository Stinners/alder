import requests
from bs4 import BeautifulSoup

GLOBAL_ATTR_TEXT = "Global Attributes"
VISIBLE_ATTR_TEXT = "All visible elements."
NOT_SUPPORTED_TEXT = "Not supported in HTML 5."

html = requests.get("https://www.w3schools.com/tags/ref_attributes.asp").text
soup = BeautifulSoup(html, 'html.parser')
rows = soup.table.find_all("tr")

for row in rows[1:]:
    cols = row.find_all("td")
    attr_name = cols[0].get_text().strip()

    # If the element isn't supported in html we ignore it 
    restriction_text = cols[1].get_text().strip()
    if restriction_text == NOT_SUPPORTED_TEXT:
        continue

    # For the moment just treat all visible as unretricted
    elif restriction_text in [GLOBAL_ATTR_TEXT, VISIBLE_ATTR_TEXT]:
        restriction = ""

    # For now just collate the restrictions into a string as 
    # a comma separated list 
    else:
        restrictions = restriction_text.split()
        restrictions = [res.strip("<>, ") for res in restrictions]
        restriction = (", ").join(restrictions)

    # TODO figure out how to actually use this information
    print(f"{attr_name}: {restriction}")






