from getpass import getuser
import configparser
import json


def getUsername() -> str:
    return getuser()


def getHome() -> str:
    return f'/home/{getUsername()}'


def getConfig() -> configparser.ConfigParser:
    home = getHome()
    config = configparser.ConfigParser()
    config.read(f'{home}/.files-balamah/python-scripts.ini')

    return config


def getJson(filename: str) -> dict:
    with open(filename, 'r') as file:
        return json.load(file)
