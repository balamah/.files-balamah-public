from Crypt import Crypt

import sys


def main():
    sys.argv.pop(0)
    text = " ".join(sys.argv)
    crypt = Crypt()

    try:
        output = crypt.decrypt(text)

    except KeyError:
        output = crypt.crypt(text)

    except ValueError:
        output = crypt.crypt(text)

    print(output)


if __name__ == "__main__":
    main()
