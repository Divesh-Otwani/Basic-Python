import requests
import random
from threading import Thread

# Configuring the test
numWords = 4 # Keep this small
concurrDegree = 30 # How much messy thread fights we'd like
threadNum = 8 # Number of threads


word_file = "/usr/share/dict/words"
WORDS = open(word_file).read().splitlines()
random.shuffle(WORDS)
negNumWords = - numWords
randWords = WORDS[0:numWords]
randWords2 = WORDS[0:numWords] + WORDS[negNumWords:]

def sendPosts():
    isite = "http://localhost:9000/input"
    theWords = randWords.copy()
    #random.shuffle(theWords)
    for word in theWords:
        for i in range(concurrDegree):
            ival = {'inputParam': word}
            requests.post(url=isite, data=ival)

def getRequests():
    theWords = randWords2.copy()
    random.shuffle(theWords)
    for word in theWords:
        qsite = "http://localhost:9000/query"
        qval = {'queryParam': word}
        myquery = requests.get(url=qsite, params=qval)
        print("Query response was :\n", myquery.content)


def test():
    threads = []
    print("The words we post are:")
    print(randWords)
    numTimes = concurrDegree * threadNum
    print("We post these each this many times: ", numTimes)
    print("Results: \n\n")

    for i in range(threadNum):
        t = Thread(target=sendPosts(), args = ())
        threads.append(t)

    for t in threads:
        t.start()

    for t in threads:
        t.join()

    getRequests()

test()


