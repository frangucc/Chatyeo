from nltk.book import *

import re

#get all the words form big.txt

def words(text): return re.findall('[a-z]+', text.lower()) 

def data(): return words(file('data/big.txt').read())

WORDS = data()

def known(words): return set(w for w in words if w in WORDS)

def unknown(words): return set(w for w in words if w not in WORDS)
