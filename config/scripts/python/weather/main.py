from city import city

import requests

import configparser
import sys
import os

# python sucks
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../')))

from resources import getConfig, getJson, getHome


def getCurrentWeather(config: configparser.SectionProxy, city: str) -> str:
    urlApiNow = config['url-api-now']
    owmKey = config['owm-key']
    units = config['units']

    link = f"{urlApiNow}{city}&appid={owmKey}&units={units}"
    weatherData = requests.get(link).json()

    temperature = int(weatherData['main']['temp'])
    temperatureFeelsLike = int(weatherData['main']['feels_like'])
    status = weatherData['weather'][0]['description']

    emojis = getJson(
        f'{getHome()}/.files-balamah/config/scripts/python/weather/emojis.json'
    )

    if status in emojis:
        statusEmoji = emojis[status]
    else:
        statusEmoji = ""

    if config['units'] == "metric":
        mark = "C"
    elif config['units'] == "imperial":
        mark = "F"

    formattedWeather = f"{statusEmoji} {temperature}°{mark}"
    if temperature != temperatureFeelsLike:
        formattedWeather += f" ({temperatureFeelsLike}°{mark})"

    return formattedWeather


def main():
    config = getConfig()['weather']

    try:
        output = getCurrentWeather(config, city)
    except requests.exceptions.ConnectionError:
        output = "󰪎"

    print(output)


if __name__ == '__main__':
    main()
