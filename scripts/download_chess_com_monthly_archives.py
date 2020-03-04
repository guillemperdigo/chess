# chess.com API -> https://www.chess.com/news/view/published-data-api
# The API has been used to download monthly archives for a user using a Python3 program.
# This program works as of 24/09/2018
import sys
import urllib
from urllib.request import urlopen
from pandas import read_csv

username_file = read_csv("/Users/Guillem/Chess/gamesRawData/username.csv")
username = username_file.iloc[0,0]

baseUrl = "https://api.chess.com/pub/player/" + username + "/games/"
archivesUrl = baseUrl + "archives"

#read the archives url and store in a list
f = urllib.request.urlopen(archivesUrl)
archives = f.read().decode("utf-8")
archives = archives.replace("{\"archives\":[\"", "\",\"")
archivesList = archives.split("\",\"" + baseUrl)
archivesList[len(archivesList)-1] = archivesList[len(archivesList)-1].rstrip("\"]}")

#create directory if it doesn't exist
from pathlib import Path
Path("/Users/Guillem/Chess/gamesRawData/" + username).mkdir(parents=True, exist_ok=True)

#download all the archives
for i in range(len(archivesList)-1):
    url = baseUrl + archivesList[i+1] + "/pgn"
    filename = archivesList[i+1].replace("/", "-")
    urllib.request.urlretrieve(url, "/Users/Guillem/Chess/gamesRawData/" + username + "/" + filename + ".pgn")
    print(filename + ".pgn has been downloaded.")
print ("All files have been downloaded.")