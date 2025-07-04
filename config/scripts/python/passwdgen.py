import random
import sys

from resources import getConfig


def main():
    passwordLength = sys.argv[-1]

    lowerLetters = "qwertyuiopasdfghjklzxcvbnm"
    upperLetters = lowerLetters.upper()
    numbers = "1234567890"
    specialSymbols = "!@#$%^&*;'"

    allowSpecialSymbols = getConfig()['passwdgen']['allow-special-symbols']
    if allowSpecialSymbols == 'yes':
        allSymbols = lowerLetters + upperLetters + numbers + specialSymbols
    else:
        allSymbols = lowerLetters + upperLetters + numbers

    password = "".join(random.sample(allSymbols, int(passwordLength)))

    print(password)


if __name__ == "__main__":
    main()
