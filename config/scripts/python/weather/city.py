"""
It looks ugly, and i know it.
I wanted to make city printable,
because geocoder sometimes gives city
that you are not in, therefore works like dogshit.
To print the city, use: --print or -p
"""

import geocoder

import sys
import os

# python sucks
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../')))

from resources import getConfig

city = getConfig()['weather']['city']
if city == '^geocoder':
    city = geocoder.ip('me').city

if sys.argv[-1] in ['--print', '-p']:
    print(city)
