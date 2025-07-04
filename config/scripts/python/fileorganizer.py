#!/usr/bin/python3

import os


def makeExtensionCatalogs(extensions: list[str]):
    for catalog in extensions:
        try:
            os.mkdir(catalog)

        except FileExistsError:
            pass


def main():
    files = os.listdir(os.getcwd())
    fileExtensions = [file.split('.')[-1] for file in files
                      if not os.path.isdir(f"{os.getcwd()}/{file}")]

    makeExtensionCatalogs(fileExtensions)

    for extension in fileExtensions:
        os.system(f'mv *.{extension} {extension} 2> /dev/null')


if __name__ == '__main__':
    main()
