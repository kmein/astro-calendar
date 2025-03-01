import json
from reportlab.lib import colors
from reportlab.lib.pagesizes import letter
from reportlab.pdfgen import canvas
import math

# Load JSON data from file
with open("chart.json", "r") as file:
    data = json.load(file)

# Constants for the chart wheel
CENTER_X, CENTER_Y = 300, 400  # Center of the wheel
RADIUS = 200  # Outer radius
INNER_RADIUS = 150  # Inner radius for houses
ZODIAC_SIGNS = [
    "Aries", "Taurus", "Gemini", "Cancer", "Leo", "Virgo",
    "Libra", "Scorpio", "Sagittarius", "Capricorn", "Aquarius", "Pisces"
]

# Load planetary positions and aspects from the JSON data
planetary_positions = data["charts"][0]
aspects = data["aspects"]

# Create PDF
pdf_path = "birth_chart_wheel.pdf"
c = canvas.Canvas(pdf_path, pagesize=letter)

# Draw title
c.setFont("Helvetica-Bold", 16)
c.drawCentredString(CENTER_X, 750, "Astrological Birth Chart")

# Draw outer zodiac circle
c.setStrokeColor(colors.black)
c.circle(CENTER_X, CENTER_Y, RADIUS)

# Draw inner house circle
c.circle(CENTER_X, CENTER_Y, INNER_RADIUS)

# Draw zodiac signs and house divisions
for i in range(12):
    angle = i * 30  # Each sign is 30 degrees
    radian_angle = math.radians(angle)

    # Calculate text position
    text_x = CENTER_X + (RADIUS + 20) * math.cos(radian_angle + math.pi/12)
    text_y = CENTER_Y + (RADIUS + 20) * math.sin(radian_angle + math.pi/12)

    # Draw sign labels
    c.setFont("Helvetica", 10)
    c.drawCentredString(text_x, text_y, ZODIAC_SIGNS[i])

    # Draw house lines
    x1, y1 = CENTER_X + RADIUS * math.cos(radian_angle), CENTER_Y + RADIUS * math.sin(radian_angle)
    x2, y2 = CENTER_X + INNER_RADIUS * math.cos(radian_angle), CENTER_Y + INNER_RADIUS * math.sin(radian_angle)
    c.line(x1, y1, x2, y2)

# Store planet positions in absolute coordinates
planet_positions = {}
for planet in planetary_positions:
    degrees = planet["degrees"] + planet["minutes"] / 60  # Convert to decimal degrees
    sign_index = ZODIAC_SIGNS.index(planet["sign"].capitalize())  # Get zodiac sign index
    absolute_angle = sign_index * 30 + degrees  # Convert to absolute angle in chart
    radian_angle = math.radians(absolute_angle)

    # Calculate planet position
    planet_x = CENTER_X + (INNER_RADIUS - 20) * math.cos(radian_angle)
    planet_y = CENTER_Y + (INNER_RADIUS - 20) * math.sin(radian_angle)

    # Store position for aspect drawing
    planet_positions[planet["planet"].lower()] = (planet_x, planet_y)

    # Draw planet name
    c.setFont("Helvetica-Bold", 8)
    c.drawCentredString(planet_x, planet_y, planet["planet"].capitalize())

# Draw aspect lines
for aspect in aspects:
    p1, p2 = aspect["planet1"].lower(), aspect["planet2"].lower()
    if p1 in planet_positions and p2 in planet_positions:
        x1, y1 = planet_positions[p1]
        x2, y2 = planet_positions[p2]

        # Adjust line transparency based on orb size
        orb = aspect["orb"]
        transparency = max(0.1, min(1.0, 1.0 - (orb / 10)))
        c.setStrokeColorRGB(0, 0, 1, transparency)  # Blue with variable opacity
        c.line(x1, y1, x2, y2)

        # Label aspect type at midpoint
        mid_x, mid_y = (x1 + x2) / 2, (y1 + y2) / 2
        c.setFont("Helvetica", 7)
        c.setFillColor(colors.black)
        c.drawCentredString(mid_x, mid_y, aspect["type"].capitalize())

# Save the PDF
c.save()

print(f"Birth chart wheel saved as {pdf_path}")
