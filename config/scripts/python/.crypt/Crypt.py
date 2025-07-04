import random
import sys
import os

# python sucks
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../')))

from resources import getConfig


class Crypt:
    numbersToLetters = {0: 'e[', 1: 'n)', 2: 'c%', 3: 'i(', 4: 'eI',
                        5: 'a)', 6: '-;', 7: "-'", 8: ';-', 9: 'o;'}
    reverseNumbersToLetters = {v: k for k, v in numbersToLetters.items()}
    config = getConfig()

    def __getCodeLetter(self, letter: str) -> str:
        codeLetter = str(ord(letter))
        match len(codeLetter):
            case 3:
                codeLetter = f"0{codeLetter}"
            case 2:
                codeLetter = f"00{codeLetter}"
            case 1:
                codeLetter = f"000{codeLetter}"

        return codeLetter

    def crypt(self, text: str) -> str:
        cryptedMessage = ""

        for letter in text:
            codeLetter = self.__getCodeLetter(letter)
            cryptedMessage += codeLetter

        randomSymbol = random.randint(1, 9)

        if self.config['crypt']['letters-crypt'] == 'yes':
            randomSymbol = random.choice("()[]+-%;")

            array = [self.numbersToLetters[int(e)] for e in cryptedMessage]
            cryptedMessage = ''.join(array)

        return cryptedMessage + str(randomSymbol)

    def decrypt(self, text: str) -> str:
        text = text[:-1]
        if self.config['crypt']['letters-crypt'] == 'yes':
            text = self.__decryptSymbolsToNumbers(text)

        decryptMsg = ""

        for i in range(0, len(text)):
            if i % 4 == 0:
                t = int(f"{text[i]}{text[i + 1]}{text[i + 2]}{text[i + 3]}")
                decryptMsg += chr(t)

        return decryptMsg

    def __decryptSymbolsToNumbers(self, text: str) -> str:
        decryptedMessage = ""
        for i in range(0, len(text)):
            if i % 2 == 0:
                t = text[i] + text[i + 1]

                decryptedMessage += str(self.reverseNumbersToLetters[t])

        return decryptedMessage
