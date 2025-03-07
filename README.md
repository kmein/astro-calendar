# AstroCalendar CLI

AstroCalendar is a command-line astrology toolkit that allows users to generate natal charts, synastry charts, and track astrological events over time. It provides various output formats, including plain text, JSON, and iCalendar (.ics) files.

## Features
- Generate natal charts with planetary positions and aspects.
- Perform synastry analysis between two charts.
- Track astrological events such as retrogrades, sign transitions, aspects, transits, and eclipses.
- Multiple output formats: plain text, JSON, and iCalendar (.ics).
- Configurable planet selection and aspect filtering.
- Different orb calculation methods (e.g., Brennan, Greene, Astrodienst, Tarnas).
- Optional AI-generated interpretations for charts and aspects.

## Installation
Ensure you have Haskell installed. Then, build and install the project:

```sh
cabal build
```

If you have Nix, use this command:

```sh
nix build
```

## Usage
AstroCalendar provides several commands:

### Generate a Natal Chart
```console
$ astro-calendar chart --date "$(date -u +"%Y-%m-%d %H:%M" -d "28 June 1971 07:30 SAST")"
☉	♋ 5° 53ʹ
☽	♍ 8° 14ʹ
☿	♋ 14° 4ʹ
♀	♊ 19° 23ʹ
♂	♒ 20° 54ʹ
♃	♏ 27° 38ʹ ℞
♄	♊ 1° 7ʹ
♅	♎ 9° 28ʹ
♆	♐ 0° 48ʹ ℞
♇	♍ 27° 5ʹ

♄ ☍ ♆	0° 19ʹ
♃ ⚹ ♇	0° 33ʹ
♀ △ ♂	1° 31ʹ
☉ ⚹ ☽	2° 21ʹ
♃ ☌ ♆	3° 10ʹ
♃ ☍ ♄	3° 28ʹ
☉ □ ♅	3° 35ʹ
♆ ⚹ ♇	3° 43ʹ
♄ △ ♇	4° 2ʹ
☿ □ ♅	4° 35ʹ
♂ □ ♃	6° 45ʹ
☽ □ ♄	7° 7ʹ
☽ □ ♆	7° 26ʹ
☉ ☌ ☿	8° 11ʹ
♄ △ ♅	8° 22ʹ
☉ □ ♇	8° 48ʹ
```

Example: Elon Musk's birth chart, born June 28, 1971, at 07:30 AM SAST in Pretoria, South Africa.

### Perform a Synastry Chart Analysis
```console
$ astro-calendar synastry "$(date -u +"%Y-%m-%d %H:%M" -d "28 June 1971 07:30 SAST")" "$(date -u +"%Y-%m-%d %H:%M" -d "17 March 1988 23:30 PST")"
☉	♋ 5° 53ʹ	♓ 27° 55ʹ
☽	♍ 8° 14ʹ	♈ 1° 5ʹ
☿	♋ 14° 4ʹ	♓ 2° 29ʹ
♀	♊ 19° 23ʹ	♉ 13° 4ʹ
♂	♒ 20° 54ʹ	♑ 16° 47ʹ
♃	♏ 27° 38ʹ ℞	♉ 2° 3ʹ
♄	♊ 1° 7ʹ	♑ 2° 5ʹ
♅	♎ 9° 28ʹ	♑ 0° 54ʹ
♆	♐ 0° 48ʹ ℞	♑ 10° 1ʹ
♇	♍ 27° 5ʹ	♏ 12° 16ʹ ℞

♃ △ ☉	0° 16ʹ
♅ □ ♆	0° 33ʹ
♇ ☍ ☉	0° 50ʹ
☿ ⚹ ♀	0° 59ʹ
♄ □ ☿	1° 22ʹ
♆ □ ☿	1° 41ʹ
☿ △ ♇	1° 47ʹ
☽ △ ♆	1° 47ʹ
☿ ☍ ♂	2° 43ʹ
♆ △ ☉	2° 53ʹ
♄ ⚹ ☉	3° 12ʹ
☉ △ ☿	3° 24ʹ
☉ ☍ ♄	3° 48ʹ
♇ □ ♅	3° 49ʹ
☉ ⚹ ♃	3° 50ʹ
☿ ☍ ♆	4° 2ʹ
☽ ⚹ ♇	4° 2ʹ
☉ ☍ ♆	4° 8ʹ
☽ △ ♀	4° 50ʹ
♃ □ ☿	4° 51ʹ
☉ ☍ ♅	4° 59ʹ
♇ □ ♄	4° 60ʹ
☽ ☍ ☿	5° 45ʹ
☽ △ ♄	6° 9ʹ
☽ △ ♃	6° 11ʹ
☉ △ ♇	6° 23ʹ
☽ △ ♅	7° 20ʹ
♅ □ ♄	7° 24ʹ
☉ □ ☉	7° 58ʹ
♀ □ ☉	8° 32ʹ
☽ △ ♂	8° 33ʹ
```

Example: Synastry between Elon Musk (born June 28, 1971, at 07:30 AM SAST in Pretoria, South Africa) and Grimes (born March 17, 1988, at 11:30 PM PST in Vancouver, British Columbia, Canada).

### Track Astrological Events
```console
$ astro-calendar --only-planets jupiter,saturn,uranus,neptune,pluto events --begin "2025-03-01" --end "2025-04-01" --retrograde --aspects --signs --eclipses --hourly                                ~main
2025-03-01 00 2025-03-30 11 ♆ ♓
2025-03-01 00 2025-04-01 00 ♃ □ ♄ 2025-03-01 00
2025-03-01 00 2025-04-01 00 ♃ ♊
2025-03-01 00 2025-04-01 00 ♄ ☌ ♆ 2025-04-01 00
2025-03-01 00 2025-04-01 00 ♄ ♓
2025-03-01 00 2025-04-01 00 ♄ ⚹ ♅ 2025-04-01 00
2025-03-01 00 2025-04-01 00 ♅ ♉
2025-03-01 00 2025-04-01 00 ♆ ⚹ ♇ 2025-04-01 00
2025-03-01 00 2025-04-01 00 ♇ ♒
2025-03-14 05 2025-03-14 08 🝶 ☽ TotalLunarEclipse 2025-03-14 06
2025-03-29 08 2025-03-29 12 🝵 ☉ PartialSolarEclipse 2025-03-29 10
2025-03-30 12 2025-04-01 00 ♆ ♈
```

### Generate Personal Transits
```console
$ astro-calendar --orbs-tarnas --only-hard-aspects events --transits "$(date -u +"%Y-%m-%d %H:%M" -d "28 June 1971 07:30 SAST")" --begin "2025-01-01" --end "2025-03-31" --hourly
2025-01-01 00 2025-01-02 06 ☉ □ natal ♇ 2025-01-01 00
2025-01-01 00 2025-01-03 12 ☿ □ natal ☽ 2025-01-01 00
2025-01-01 00 2025-01-08 18 ♀ ☌ natal ♂ 2025-01-01 00
2025-01-01 00 2025-01-10 21 ☉ ☍ natal ☉ 2025-01-01 00
2025-01-01 00 2025-01-11 11 ☿ ☍ natal ♀ 2025-01-01 00
2025-01-01 00 2025-01-13 09 ♃ □ natal ♇ 2025-01-01 00
2025-01-01 00 2025-01-14 09 ☉ □ natal ♅ 2025-01-01 00
2025-01-01 00 2025-01-15 11 ♀ □ natal ♃ 2025-01-01 00
2025-01-01 00 2025-01-16 03 ♄ □ natal ♆ 2025-01-01 00
2025-01-01 00 2025-01-16 15 ☿ □ natal ♇ 2025-01-06 09
2025-01-01 00 2025-01-18 18 ♀ □ natal ♆ 2025-01-03 21
2025-01-01 00 2025-01-18 21 ☉ ☍ natal ☿ 2025-01-04 05
2025-01-01 00 2025-01-19 02 ♀ □ natal ♄ 2025-01-04 05
2025-01-01 00 2025-01-19 09 ♄ □ natal ♄ 2025-01-01 00
2025-01-01 00 2025-01-26 21 ♀ ☍ natal ☽ 2025-01-11 02
2025-01-01 00 2025-03-21 17 ♄ ☍ natal ☽ 2025-01-01 00
2025-01-01 00 2025-03-30 21 ♃ ☍ natal ♆ 2025-02-04 09
2025-01-01 00 2025-03-31 00 ♃ □ natal ☽ 2025-02-04 09
2025-01-01 00 2025-03-31 00 ♃ ☌ natal ♀ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♃ ☌ natal ♄ 2025-02-04 09
2025-01-01 00 2025-03-31 00 ♄ □ natal ♀ 2025-02-18 06
2025-01-01 00 2025-03-31 00 ♄ ☍ natal ♇ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♅ □ natal ☽ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♅ □ natal ♂ 2025-01-30 15
2025-01-01 00 2025-03-31 00 ♅ ☌ natal ♄ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♅ ☍ natal ♃ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♅ ☍ natal ♆ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♆ □ natal ☉ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♆ □ natal ♀ 2025-01-01 00
2025-01-01 00 2025-03-31 00 ♆ ☍ natal ♅ 2025-03-31 00
2025-01-01 00 2025-03-31 00 ♆ ☍ natal ♇ 2025-01-01 00
2025-01-01 18 2025-01-22 09 ☿ ☍ natal ☉ 2025-01-12 12
2025-01-04 12 2025-01-24 15 ☿ □ natal ♅ 2025-01-14 21
2025-01-06 17 2025-03-05 09 ♃ ☍ natal ♃ 2025-02-04 09
2025-01-07 08 2025-02-10 18 ♀ □ natal ♀ 2025-01-22 15
2025-01-07 18 2025-01-27 12 ☿ ☍ natal ☿ 2025-01-17 23
2025-01-08 21 2025-03-31 00 ♂ ☌ natal ☿ 2025-02-24 02
2025-01-14 23 2025-03-31 00 ♀ ☍ natal ♇ 2025-01-31 12
2025-01-20 12 2025-03-31 00 ♂ □ natal ♅ 2025-02-24 02
2025-01-24 06 2025-03-31 00 ♀ □ natal ☉ 2025-03-17 18
2025-01-25 15 2025-02-24 06 ☉ ☌ natal ♂ 2025-02-09 09
2025-01-28 09 2025-03-31 00 ♀ ☍ natal ♅ 2025-02-21 15
2025-01-30 15 2025-03-22 21 ♂ ☌ natal ☉ 2025-02-24 02
2025-01-31 18 2025-02-17 15 ☿ ☌ natal ♂ 2025-02-09 11
2025-02-01 06 2025-03-02 23 ☉ □ natal ♃ 2025-02-16 02
2025-02-03 03 2025-03-28 23 ♀ □ natal ☿ 2025-03-02 00
2025-02-04 09 2025-03-06 03 ☉ □ natal ♆ 2025-02-19 05
2025-02-04 17 2025-03-06 11 ☉ □ natal ♄ 2025-02-19 12
2025-02-04 18 2025-02-21 08 ☿ □ natal ♃ 2025-02-13 05
2025-02-06 14 2025-02-23 00 ☿ □ natal ♆ 2025-02-14 23
2025-02-06 18 2025-02-23 05 ☿ □ natal ♄ 2025-02-15 03
2025-02-10 18 2025-02-27 03 ☿ ☍ natal ☽ 2025-02-18 23
2025-02-11 18 2025-03-13 14 ☉ ☍ natal ☽ 2025-02-26 15
2025-02-16 21 2025-03-06 15 ☿ □ natal ♀ 2025-02-25 00
2025-02-21 00 2025-03-31 00 ☿ ☍ natal ♇ 2025-03-01 12
2025-02-22 18 2025-03-24 18 ☉ □ natal ♀ 2025-03-09 18
2025-02-25 21 2025-03-31 00 ☿ □ natal ☉ 2025-03-23 02
2025-02-26 15 2025-03-31 00 ♃ □ natal ♇ 2025-03-31 00
2025-02-27 23 2025-03-31 00 ☿ ☍ natal ♅ 2025-03-16 12
2025-03-02 11 2025-03-31 00 ☉ ☍ natal ♇ 2025-03-17 11
2025-03-02 15 2025-03-31 00 ♄ □ natal ☉ 2025-03-31 00
2025-03-02 18 2025-03-31 00 ☿ □ natal ☿ 2025-03-15 06
2025-03-05 18 2025-03-31 00 ♆ □ natal ☿ 2025-03-31 00
2025-03-11 06 2025-03-31 00 ☉ □ natal ☉ 2025-03-26 08
2025-03-14 20 2025-03-31 00 ☉ ☍ natal ♅ 2025-03-29 23
2025-03-19 11 2025-03-31 00 ☉ □ natal ☿ 2025-03-31 00
2025-03-20 06 2025-03-31 00 ♀ □ natal ♀ 2025-03-31 00
2025-03-24 21 2025-03-31 00 ☿ □ natal ♀ 2025-03-31 00
```

Example: Personal transits for Elon Musk in 2025.

```console
$ astro-calendar --json events --minutely --transits "$(date -u +"%Y-%m-%d %H:%M" -d "17 March 1988 23:30 PST")" --begin "2025-02-01" --end "2025-02-06" | jq
{
  "aspect": null,
  "eclipses": null,
  "retrograde": null,
  "sign": null,
  "transits": [
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T14:33:00.000016093254Z",
      "natalPlanet": "sun",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "venus",
      "type": "conjunction"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "sun",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "mars",
      "type": "trine"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-06T00:00:00Z",
      "natalPlanet": "sun",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "uranus",
      "type": "sextile"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "sun",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "neptune",
      "type": "conjunction"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "sun",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "pluto",
      "type": "sextile"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-05T19:45:59.999983012676Z",
      "natalPlanet": "moon",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "venus",
      "type": "conjunction"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-06T00:00:00Z",
      "natalPlanet": "moon",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "neptune",
      "type": "conjunction"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "moon",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "pluto",
      "type": "sextile"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-04T09:40:00.000017881393Z",
      "natalPlanet": "mercury",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "jupiter",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T16:42:59.999993741512Z",
      "natalPlanet": "venus",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "sun",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-04T23:08:59.999981224536Z",
      "natalPlanet": "venus",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "mercury",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "venus",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "saturn",
      "type": "sextile"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-06T00:00:00Z",
      "natalPlanet": "mars",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "mars",
      "type": "opposition"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "mars",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "saturn",
      "type": "sextile"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "mars",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "uranus",
      "type": "trine"
    },
    {
      "endTime": "2025-02-03T18:48:59.999985694885Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "jupiter",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "mercury",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T07:59:00.000016987323Z",
      "natalPlanet": "jupiter",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "pluto",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-06T00:00:00Z",
      "natalPlanet": "saturn",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "venus",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-06T00:00:00Z",
      "natalPlanet": "saturn",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "neptune",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-05T14:04:59.999995529651Z",
      "natalPlanet": "uranus",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "venus",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-06T00:00:00Z",
      "natalPlanet": "uranus",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "neptune",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "pluto",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "sun",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-04T11:57:00.000010728836Z",
      "natalPlanet": "pluto",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "mercury",
      "type": "square"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-06T00:00:00Z",
      "natalPlanet": "pluto",
      "startTime": "2025-02-05T09:09:00.000008046627Z",
      "transitingPlanet": "mars",
      "type": "trine"
    },
    {
      "endTime": "2025-02-06T00:00:00Z",
      "exactTime": "2025-02-01T00:00:00Z",
      "natalPlanet": "pluto",
      "startTime": "2025-02-01T00:00:00Z",
      "transitingPlanet": "saturn",
      "type": "trine"
    }
  ]
}
```

Example: Personal transits for Grimes in 2025.

### Output Formats
You can specify the output format using:
- `--text` (default)
- `--json`
- `--ical` (for iCalendar output, supported only for events)

### Example Commands
```console
# Generate Elon Musk's natal chart in JSON format
$ astro-calendar chart --date "$(date -u +"%Y-%m-%d %H:%M" -d "28 June 1971 07:30 SAST")" --json
{
  "aspects": [
    {
      "orb": 0.30916426212831993,
      "planet1": "saturn",
      "planet2": "neptune",
      "type": "opposition"
    },
    {
      "orb": 0.5545969478833683,
      "planet1": "jupiter",
      "planet2": "pluto",
      "type": "sextile"
    },
    {
      "orb": 1.5088570589772132,
      "planet1": "venus",
      "planet2": "mars",
      "type": "trine"
    },
    {
      "orb": 2.3520847316004847,
      "planet1": "sun",
      "planet2": "moon",
      "type": "sextile"
    },
    {
      "orb": 3.1655232464382266,
      "planet1": "jupiter",
      "planet2": "neptune",
      "type": "conjunction"
    },
    {
      "orb": 3.4746875085665465,
      "planet1": "jupiter",
      "planet2": "saturn",
      "type": "opposition"
    },
    {
      "orb": 3.591464165485718,
      "planet1": "sun",
      "planet2": "uranus",
      "type": "square"
    },
    {
      "orb": 3.720120194321595,
      "planet1": "neptune",
      "planet2": "pluto",
      "type": "sextile"
    },
    {
      "orb": 4.029284456449943,
      "planet1": "saturn",
      "planet2": "pluto",
      "type": "trine"
    },
    {
      "orb": 4.584469401528224,
      "planet1": "mercury",
      "planet2": "uranus",
      "type": "square"
    },
    {
      "orb": 6.742301191030464,
      "planet1": "mars",
      "planet2": "jupiter",
      "type": "square"
    },
    {
      "orb": 7.120078007537714,
      "planet1": "moon",
      "planet2": "saturn",
      "type": "square"
    },
    {
      "orb": 7.42924226966602,
      "planet1": "moon",
      "planet2": "neptune",
      "type": "square"
    },
    {
      "orb": 8.175933567013999,
      "planet1": "sun",
      "planet2": "mercury",
      "type": "conjunction"
    },
    {
      "orb": 8.35945744142299,
      "planet1": "saturn",
      "planet2": "uranus",
      "type": "trine"
    },
    {
      "orb": 8.797277732387215,
      "planet1": "sun",
      "planet2": "pluto",
      "type": "square"
    }
  ],
  "charts": [
    [
      {
        "degrees": 5,
        "minutes": 53,
        "planet": "sun",
        "retrograde": false,
        "sign": "cancer"
      },
      {
        "degrees": 8,
        "minutes": 14,
        "planet": "moon",
        "retrograde": false,
        "sign": "virgo"
      },
      {
        "degrees": 14,
        "minutes": 4,
        "planet": "mercury",
        "retrograde": false,
        "sign": "cancer"
      },
      {
        "degrees": 19,
        "minutes": 23,
        "planet": "venus",
        "retrograde": false,
        "sign": "gemini"
      },
      {
        "degrees": 20,
        "minutes": 54,
        "planet": "mars",
        "retrograde": false,
        "sign": "aquarius"
      },
      {
        "degrees": 27,
        "minutes": 38,
        "planet": "jupiter",
        "retrograde": true,
        "sign": "scorpio"
      },
      {
        "degrees": 1,
        "minutes": 7,
        "planet": "saturn",
        "retrograde": false,
        "sign": "gemini"
      },
      {
        "degrees": 9,
        "minutes": 28,
        "planet": "uranus",
        "retrograde": false,
        "sign": "libra"
      },
      {
        "degrees": 0,
        "minutes": 48,
        "planet": "neptune",
        "retrograde": true,
        "sign": "sagittarius"
      },
      {
        "degrees": 27,
        "minutes": 5,
        "planet": "pluto",
        "retrograde": false,
        "sign": "virgo"
      }
    ]
  ]
}

# Generate a synastry chart between Elon Musk and Grimes (born March 17, 1988, at 11:30 PM PST in Vancouver, British Columbia, Canada) with AI interpretation
$ OPENAI_API_KEY=$(pass api-keys/openai.com) astro-calendar synastry "$(date -u +"%Y-%m-%d %H:%M" -d "28 June 1971 07:30 SAST")" "$(date -u +"%Y-%m-%d %H:%M" -d "17 March 1988 23:30 PST")" --interpret
...

Chart 1 features a Cancer Sun and Virgo Moon, indicating a nurturing and
practical approach to emotions and relationships. The presence of Mercury in
Cancer emphasizes strong communication rooted in care and sensitivity. Venus in
Gemini adds a sociable and flirty element, while Mars in Aquarius suggests an
independent and unconventional way of pursuing desires. Jupiter in Scorpio,
though retrograde, points to deep introspection and a transformative approach
to luck and growth. This combination fosters a strong reliance on emotional
intuition and mental connection, with an inclination towards creating a safe
emotional environment.

Chart 2 presents a Pisces Sun and Aries Moon, illustrating a blend of
sensitivity and assertiveness. The Pisces Sun indicates a dreamy, intuitive
nature, while the Aries Moon drives emotional responses to be more immediate
and passionate. Mercury in Pisces supports empathetic communication, while
Venus in Taurus brings a desire for stability and luxury in relationships.
Mars, Saturn, and Uranus all in Capricorn suggest a strong focus on ambition
and practicality, but also an urge to break free from traditional constraints.
This configuration suggests a particularly driven individual who balances
dreams with grounded actions.

The aspects between these two charts reveal a dynamic interplay. The
Jupiter-Sun trine promotes shared optimism and growth potential, encouraging
both partners to inspire each other. However, the Pluto-Sun opposition
indicates potential power struggles or transformative experiences that can feel
intense. The square between Mercury and Saturn suggests communication
challenges that could create misunderstandings. Nevertheless, the supportive
aspects between Mercury, Venus, and Mars facilitate a creative and passionate
exchange. The overall synastry indicates potential for a deep, transformative
relationship that can support both partners' emotional and practical growth.

# List upcoming astrological events including retrogrades and transits
$ astro-calendar events --begin "2025-01-01" --end "2025-12-31" --retrograde --eclipses
2025-01-01 2025-01-30 ♅ ℞
2025-01-01 2025-02-04 ♃ ℞
2025-01-01 2025-02-24 ♂ ℞
2025-03-03 2025-04-13 ♀ ℞
2025-03-14 2025-03-14 🝶 ☽ TotalLunarEclipse 2025-03-14
2025-03-16 2025-04-07 ☿ ℞
2025-03-29 2025-03-29 🝵 ☉ PartialSolarEclipse 2025-03-29
2025-05-05 2025-10-14 ♇ ℞
2025-07-05 2025-12-10 ♆ ℞
2025-07-14 2025-11-28 ♄ ℞
2025-07-19 2025-08-11 ☿ ℞
2025-09-07 2025-09-07 🝶 ☽ TotalLunarEclipse 2025-09-07
2025-09-07 2025-12-31 ♅ ℞
2025-09-21 2025-09-21 🝵 ☉ PartialSolarEclipse 2025-09-21
2025-11-10 2025-11-29 ☿ ℞
2025-11-12 2025-12-31 ♃ ℞
```

## Configuration Options
- **Planets:** Choose from traditional (7 planets), modern (10 planets), or specify custom planets.
- **Aspects:** Use all aspects, only hard aspects, or define a custom list.
- **Orb Methods:** Select from predefined systems such as [AstroDienst](https://astro.com), [Chris Brennan](https://theastrologypodcast.com/transcripts/ep-323-transcript-aspects-in-astrology-the-five-major-configurations/), Liz Greene, or [Richard Tarnas](https://en.wikipedia.org/wiki/Cosmos_and_Psyche).
- **Interpretation:** Enable AI-generated delineations using `--interpret`.

## License
This project is open-source and distributed under the [AGPL License](./LICENSE).

## Contributions
Contributions are welcome! Feel free to submit issues or pull requests to improve functionality or add features.
