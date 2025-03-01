import json
import calendar
import datetime
import random
from reportlab.lib.pagesizes import letter, landscape
from reportlab.pdfgen import canvas
from reportlab.lib.colors import HexColor

# Load the JSON file
with open("2025.json", "r") as f:
    data = json.load(f)

planet_colors = {
    "sun": "#FFA500",     # Orange
    "moon": "#FFFFFF",    # White
    "mercury": "#00FF00", # Green
    "venus": "#008000",   # Dark Green
    "mars": "#FF0000",    # Red
    "jupiter": "#FFD700", # Gold
    "saturn": "#808080",  # Grey
    "uranus": "#0000FF",  # Blue
    "neptune": "#EE82EE", # Violet
    "pluto": "#8B0000",   # Dark Red
}

# Mapping for aspect letters
aspect_letters = {
    "conjunction": "1",
    "opposition": "2",
    "trine": "3",
    "square": "4",
    "sextile": "6",
}

# Function to blend two hex colors
def blend_colors(color1, color2):
    c1 = int(color1[1:], 16)
    c2 = int(color2[1:], 16)
    r = ((c1 >> 16) + (c2 >> 16)) // 2
    g = (((c1 >> 8) & 0xFF) + ((c2 >> 8) & 0xFF)) // 2
    b = ((c1 & 0xFF) + (c2 & 0xFF)) // 2
    return f"#{r:02X}{g:02X}{b:02X}"

# Define colors for each unique planet combination (blended colors)
aspect_combinations = sorted(set((aspect["planet1"], aspect["planet2"]) for aspect in data["aspect"]))
aspect_colors = {combo: blend_colors(planet_colors[combo[0]], planet_colors[combo[1]]) for combo in aspect_combinations}

# Convert aspect data into a date dictionary
date_aspects = {}
exact_dates = {}
for aspect in data["aspect"]:
    start_date = datetime.datetime.strptime(aspect["startTime"], "%Y-%m-%dT%H:%M:%SZ").date()
    end_date = datetime.datetime.strptime(aspect["endTime"], "%Y-%m-%dT%H:%M:%SZ").date()
    exact_date = datetime.datetime.strptime(aspect["exactTime"], "%Y-%m-%dT%H:%M:%SZ").date()

    current_date = start_date
    while current_date <= end_date:
        if current_date not in date_aspects:
            date_aspects[current_date] = []
        date_aspects[current_date].append(aspect)
        current_date += datetime.timedelta(days=1)

    if exact_date not in exact_dates:
        exact_dates[exact_date] = []
    exact_dates[exact_date].append(aspect)

# Generate the PDF
def create_pdf(filename="Astrology_Calendar_2025.pdf"):
    c = canvas.Canvas(filename, pagesize=landscape(letter))
    width, height = landscape(letter)

    months_per_row = 3
    month_width = width / months_per_row
    month_height = height / 4  # Adjust spacing

    for quarter in range(0, 12, months_per_row):
        y_start = height - 50
        for col, month in enumerate(range(quarter + 1, quarter + months_per_row + 1)):
            c.setFont("Helvetica-Bold", 8)
            if month > 12:
                break
            x_offset = col * month_width + 20
            c.drawString(x_offset + 10, y_start, calendar.month_name[month] + " 2025")
            month_days = calendar.monthrange(2025, month)[1]
            y_pos = y_start - 20

            for day in range(1, month_days + 1):
                date = datetime.date(2025, month, day)
                weekday = date.strftime("%a")  # Get the weekday abbreviation
                c.setFont("Helvetica-Bold", 8)
                c.drawString(x_offset, y_pos, f"{day}")
                c.setFont("Helvetica", 8)
                c.drawString(x_offset + 12, y_pos, f"{weekday}")

                if date in date_aspects:
                    aspect_x_offset = x_offset + 40
                    for aspect in date_aspects[date]:
                        planet1 = aspect["planet1"]
                        planet2 = aspect["planet2"]
                        aspect_type = aspect["type"]
                        color = aspect_colors.get((planet1, planet2), "#000000")
                        c.setFillColor(HexColor(color))
                        c.setFont("Helvetica-Bold" if date in exact_dates and aspect in exact_dates[date] else "Helvetica", 8)
                        letter_sym = aspect_letters.get(aspect_type, "?")
                        c.drawString(aspect_x_offset, y_pos, letter_sym)
                        aspect_x_offset += 5
                    c.setFillColor(HexColor("#000000"))

                if date.weekday() == 6:  # Draw separator at the end of each week
                    c.setStrokeColor(HexColor("#CCCCCC"))
                    c.line(x_offset, y_pos - 2.5, x_offset + month_width - 20, y_pos - 5)
                    c.setStrokeColor(HexColor("#000000"))

                y_pos -= 12  # Adjust line spacing

        # Draw aspect color legend below the calendar
        legend_x = 50
        legend_y = 170
        planets = list(planet_colors.keys())
        c.setFont("Helvetica", 6)
        for i, p1 in enumerate(planets):
            c.drawString(legend_x + (i + 1) * 15, legend_y - 15, p1[:2].upper())
            c.drawString(legend_x, legend_y - (i + 1) * 15, p1[:2].upper())
        for i, p1 in enumerate(planets):
            for j, p2 in enumerate(planets[:i]):
                color = aspect_colors.get((p2, p1), "#000000")
                c.setFillColor(HexColor(color))
                c.rect(legend_x + (j + 1) * 15, legend_y - (i + 1) * 15, 10, 10, fill=1, stroke=0)
                c.setFillColor(HexColor("#000000"))

        c.showPage()

    c.save()

# Run the function to create the PDF
create_pdf()
