#!/usr/bin/python3

from os import system
from datetime import datetime

import sys
import os

# python sucks
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../')))

from resources import getHome

alarmsFile = f"{getHome()}/.alarms"


def deleteComments(fileLines: list[str]) -> list[str]:
    for line in fileLines:
        if "#" in line:
            del fileLines[fileLines.index(line)]

    return fileLines


def getAlarms() -> list[list[str]]:
    with open(alarmsFile, "r") as file:
        allLines = file.read().splitlines()
        alarmLines = deleteComments(allLines)

        alarmsList = [line.split(" - ") for line in alarmLines]

    return alarmsList


def writeLines(lines: list[str], alarmLine: str, file):
    for fileLine in lines:
        if fileLine != alarmLine:
            file.write(f"{fileLine}\n")


def removeAlarmFromFile(alarmLine: str):
    with open(alarmsFile, "r") as file:
        lines = file.read().splitlines()

    with open(alarmsFile, "w") as file:
        writeLines(lines, alarmLine, file)


def sendNotification(notificationText: str, isAlarm: bool = False):
    notificationCommand = f"notify-send 'alarm daemon' '{notificationText}'"

    if isAlarm:
        system(f"{notificationCommand} -u critical")
    else:
        system(notificationCommand)


def sendAlarms():
    while True:
        currentTime = datetime.now().strftime("%H:%M")
        alarmTime, alarmText = getAlarms()[0]
        alarmLine = f"{alarmTime} - {alarmText}"

        if alarmTime == currentTime:
            sendNotification(alarmText, isAlarm=True)

            removeAlarmFromFile(alarmLine)


def isFileEmpty() -> bool:
    with open(alarmsFile, 'r') as file:
        if not file.read():
            return True

        return False


def startAlarms():
    now = datetime.now().strftime("%d.%m.%Y %H:%M:%S")

    if not isFileEmpty():
        sendNotification('daemon has been started')
        print(f"{now} daemon started", end='\n\n')

        sendAlarms()

    else:
        message = "~/.alarms is empty"
        sendNotification(message)
        exit(message)


def main():
    try:
        startAlarms()

    except KeyboardInterrupt:
        quit()

    except IndexError:
        sendNotification("daemon stopped because there are no more alarms")

        quit()


if __name__ == "__main__":
    main()
