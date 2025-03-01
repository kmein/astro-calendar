
import matplotlib.pyplot as plt
import numpy as np
import json
import networkx as nx

# Unicode symbols for planets, zodiac signs, and aspects
planet_symbols = {
    "sun": "☉", "moon": "☽", "mercury": "☿", "venus": "♀", "mars": "♂",
    "jupiter": "♃", "saturn": "♄", "uranus": "♅", "neptune": "♆", "pluto": "♇"
}

zodiac_symbols = {
    "Aries": "♈", "Taurus": "♉", "Gemini": "♊", "Cancer": "♋", "Leo": "♌", "Virgo": "♍",
    "Libra": "♎", "Scorpio": "♏", "Sagittarius": "♐", "Capricorn": "♑", "Aquarius": "♒", "Pisces": "♓"
}

aspect_symbols = {
    "conjunction": "☌", "opposition": "☍", "trine": "△", "square": "□", "sextile": "✶"
}

# Load data from the JSON file
with open("chart.json", "r") as file:
    data = json.load(file)

aspects = data["aspects"]
chart = data["charts"][0]

# Zodiac signs and their positions
zodiac_signs = list(zodiac_symbols.keys())
num_signs = len(zodiac_signs)
angles = np.linspace(0, 2 * np.pi, num_signs, endpoint=False)

# Position zodiac symbols at 15° of each sign, moved outward
zodiac_positions = {}
for sign, angle in zip(zodiac_signs, angles):
    mid_angle = angle + (15 / 30) * (2 * np.pi / num_signs)
    zodiac_positions[sign] = (1.2 * np.cos(mid_angle), 1.2 * np.sin(mid_angle))

# Map planets to their positions in the zodiac wheel
planet_positions = {}
for planet_data in chart:
    sign_index = zodiac_signs.index(planet_data["sign"].capitalize())
    angle = angles[sign_index] + (planet_data["degrees"] / 30) * (2 * np.pi / num_signs)
    planet_positions[planet_symbols[planet_data["planet"]]] = (np.cos(angle), np.sin(angle))

# Create the graph
G = nx.Graph()
G.add_nodes_from(planet_positions.keys())
for aspect in aspects:
    G.add_edge(planet_symbols[aspect["planet1"]], planet_symbols[aspect["planet2"]], 
               type=aspect["type"], orb=aspect["orb"])

# Draw the chart
fig, ax = plt.subplots(figsize=(10, 10))
ax.set_xlim(-1.5, 1.5)
ax.set_ylim(-1.5, 1.5)
ax.set_xticks([])
ax.set_yticks([])
ax.set_frame_on(False)

# Draw zodiac boundary lines
for angle in angles:
    ax.plot([0, 1.3 * np.cos(angle)], [0, 1.3 * np.sin(angle)], color='black', linestyle='dotted', linewidth=1)

# Draw zodiac signs at 15° of each sign, moved outward
for sign, pos in zodiac_positions.items():
    ax.text(pos[0], pos[1], zodiac_symbols[sign], ha='center', va='center', fontsize=14, fontweight='bold')

# Draw planetary aspects with transparency based on orb size and label them
for edge in G.edges(data=True):
    p1, p2, data = edge
    orb = data["orb"]
    aspect_type = data["type"]
    alpha = max(0.2, 1 - (orb / 10))  # More transparent for larger orbs
    x1, y1 = planet_positions[p1]
    x2, y2 = planet_positions[p2]
    ax.plot([x1, x2], [y1, y2], color='gray', alpha=alpha, linewidth=1.5)
    
    # Label the aspect at the midpoint
    mid_x, mid_y = (x1 + x2) / 2, (y1 + y2) / 2
    if aspect_type in aspect_symbols:
        ax.text(mid_x, mid_y, aspect_symbols[aspect_type], ha='center', va='center', fontsize=12, fontweight='bold')

# Draw planets
for planet, pos in planet_positions.items():
    ax.text(pos[0], pos[1], planet, ha='center', va='center', fontsize=14, fontweight='bold')

plt.title("Astrological Chart with Zodiac Wheel and Aspects")
plt.show()
