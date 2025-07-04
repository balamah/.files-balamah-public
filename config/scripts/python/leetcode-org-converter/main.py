from src.Convert import Convert

import sys
import os

# python sucks
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '../')))

from resources import getConfig, getHome


def main():
    convert = Convert()
    config = getConfig()['leetcode-org-converter']

    leetcodeSession = config['session']
    csrfToken = config['csrf-token']
    if len(sys.argv) < 2:
        exit(
            'No problem name provided. Get it from problem url'
        )

    problemTitle = sys.argv[1]
    debug = config['debug']
    language = config['LANGUAGE']
    location = config['output-location'].replace('~', f'{getHome()}')

    convertedProblem = convert.getConvertedProblem(leetcodeSession,
                                                   csrfToken, debug,
                                                   problemTitle,
                                                   language)

    print('writing to file...')
    with open(f"{location}{problemTitle}.org", 'w') as file:
        file.write(convertedProblem)

    print(f"✅ % successfully written to file {file.name}")


if __name__ == "__main__":
    main()
