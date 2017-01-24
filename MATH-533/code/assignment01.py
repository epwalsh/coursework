#!/usr/bin/env python
# =============================================================================
# File Name:     assignment01.py
# Author:        Evan Pete Walsh
# Contact:       epwalsh10@gmail.com
# Creation Date: 2017-01-23
# Last Modified: 2017-01-24 14:58:28
# =============================================================================



ALPHABET = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
            'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z']


def char_freq(m):
    n = 1.0 * len(m)
    return {k: (m.count(k) / n) for k in ALPHABET}


def ioc(m):
    n = 1.0 * len(m)
    return sum([(m.count(k) / n)**2 for k in ALPHABET])

m = "MSCUPTEHCIPHCJICPTEHCQBPUSPJKEOEHCKSCRNCQVOEHCQYXKDCKTEHCJLESXQQVLCEQLCPU"\
    "GFEICSEICFEICIEQCQPONGDVKSPUMNDJEUSPQGNULFEEDNGLCSEEBCQQEEJCBCDKKQXIPUCNJ"\
    "CUSEMCQHCUUCGVTESUJEUUNWEUREQGFPQKCQQCPLPUYXJSFDNUKEUYXJSEYPIP"

frequencies = char_freq(m)
for k in frequencies.keys():
    print k, frequencies[k]

ioc(m)


FRQ_ENGLISH = {'A':  8.167, 'B': 1.492, 'C': 2.782, 'D': 4.253, 'E': 12.702,
               'F': 2.228, 'G': 2.015, 'H': 6.094, 'I': 6.966, 'J': 0.153,
               'K': 0.772, 'L': 4.025, 'M': 2.406, 'N': 6.749, 'O': 7.507,
               'P': 1.929, 'Q': 0.095, 'R': 5.987, 'S': 6.327, 'T': 9.056,
               'U': 2.758, 'V': 0.978, 'W': 2.360, 'X': 0.150, 'Y': 1.974,
               'Z': 0.074}
IOC_ENGLISH = sum([(FRQ_ENGLISH[k] / 100)**2 for k in ALPHABET])
IOC_ENGLISH

import operator
from pprint import pprint


def dgrams(m):
    dgrams = []
    for i in range(len(m)-1):
        dgrams.append(m[i:i+2])
    dgrams = set(dgrams)
    return {d: m.count(d) for d in dgrams}


def tgrams(m):
    dgrams = []
    for i in range(len(m)-2):
        dgrams.append(m[i:i+3])
    dgrams = set(dgrams)
    return {d: m.count(d) for d in dgrams}

digrams = dgrams(m)
trigrams = tgrams(m)
pprint(sorted(frequencies.items(), key=operator.itemgetter(1), reverse=True))
pprint(sorted(digrams.items(), key=operator.itemgetter(1), reverse=True)[0:30])
pprint(sorted(trigrams.items(), key=operator.itemgetter(1), reverse=True)[0:10])


def try_partial_key(m, key):
    return ''.join([key[c] if c in key.keys() else c for c in m])

key = {
    'C': 'T', 
    'E': 'E', 
    'Q': 'O', 
    'U': 'A',
    'P': 'I',
    'S': 'N',
    'K': 'S',
    'J': 'H',
    'I': 'D',
    'N': 'R',
    'H': 'L',
    'D': 'C',
    'G': 'U',
    'L': 'M',
    'F': 'W',
    'X': 'F',
    'T': 'G',
    'V': 'Y',
    'Y': 'P',
    'B': 'B',
    'M': 'V',
    'O': 'K',
    'R': 'J',
    'W': 'X',
    'A': 'Q',
    'Z': 'Z'}
try_partial_key(m, key)


#  m2 = "GPXNCESTGDEWTTVRLVFACTXKWBVTERBVXDCEMHGSEIMZCAEHLZQXKFQHZXKVRANSGGIGTSJPBTVUIWISDCBISQIECHUQLCOOWKKVRMGXWAMXIAVFXUHUMGZHEQVRQVLPZHUBAVUYGVVFVVXNVVKACOGBXIVNLUVSAXKVJVWNJZLKHEQRVMIOGMWSMRDTGCEIMZCAQGRKNBXIPNBARBQEBKVBCMVACTHPWAOTEMSZXVNVVZDWKBNISBJMRWAMWDOAGIZBGAHWOAWBCMLMECCJTBHIVLMYWFTTJHBXXIOGQHEGZQMYVNLVFBQCVKSQIEFBRIMJCZMWZGGIGTSSZHDHUMVYWZVXPGNAAVTRIKVRNVXODYWLZCAIGUKUMGYSEMMLFAMWNWGPTWZNOHECSBAZGYQJLWQBHYWFNKZSALLYSFQFGZLATZRUMKVWFAHDSAQMICTTRTSEQGVWGETJWABKLHUBARHGMKIWOTXGFBLNTHJPHJSRFICCFQOVDBEXIWFXXIVNXLKSABBDSFILXFRIMRGGPTKCSONEDBEWVFNVWNVVKAYOFKTLGRLLFANVRRQPQWVBGATCHUWNXVFQGTSZMTEGUIOVPRMGWCHVWFTGZTEGSWKDWAOBKWABHUMAIFZHRBARHVAHWAVFBEUVBPZHUKERMBZLLUNZHIGBUXJCYQWJIOAMRBPMLLTSQVZSABEPDBZHLGGWAFZQQMKVRLTEURZHLGYQJLWQKTEPRCLVRJQMYABZXJOSMMPPHBWPBNUBKSJILECGSGFKAEAVBGPXTCYWGZGGAPVFRIMNCESHEZVVVFZAQLCOALTERVAMYOGAMLTSOHZBTBHSZBENGHUMKFQXATJYRLIVBPZHWTVVVISQCEFIFTRPSFURWFVMGUOAAPVFRLMYSRVZZBRMKRBQQMNWYTWFOYTMYSOMMKSEABEQRBAVUEIGZHRQLMSEGARFQIGUKVTEFDCWLVABZXISFQLKOAKXKCGPXVLCTHJWBVTERJPXEKVTENSFMXRZYBAZGFQKKCZWKICJEAVBJMARJRLKZZYMWRVBTXRBFEXISQBAVSAOBESRZ"
m2 = "GPXNCESTGDEWTTVRLVFACTXKWBVTERBVXDCEMHGSEIMZCAEHLZQXKFQH"\
     "ZXKVRANSGGIGTSJPBTVUIWISDCBISQIECHUQLCOOWKKVRMGXWAMXIAVF"\
     "XUHUMGZHEQVRQVLPZHUBAVUYGVVFVVXNVVKACOGBXIVNLUVSAXKVJVWN"\
     "JZLKHEQRVMIOGMWSMRDTGCEIMZCAQGRKNBXIPNBARBQEBKVBCMVACTHP"\
     "WAOTEMSZXVNVVZDWKBNISBJMRWAMWDOAGIZBGAHWOAWBCMLMECCJTBHI"\
     "VLMYWFTTJHBXXIOGQHEGZQMYVNLVFBQCVKSQIEFBRIMJCZMWZGGIGTSS"\
     "ZHDHUMVYWZVXPGNAAVTRIKVRNVXODYWLZCAIGUKUMGYSEMMLFAMWNWGP"\
     "TWZNOHECSBAZGYQJLWQBHYWFNKZSALLYSFQFGZLATZRUMKVWFAHDSAQM"\
     "ICTTRTSEQGVWGETJWABKLHUBARHGMKIWOTXGFBLNTHJPHJSRFICCFQOV"\
     "DBEXIWFXXIVNXLKSABBDSFILXFRIMRGGPTKCSONEDBEWVFNVWNVVKAYO"\
     "FKTLGRLLFANVRRQPQWVBGATCHUWNXVFQGTSZMTEGUIOVPRMGWCHVWFTG"\
     "ZTEGSWKDWAOBKWABHUMAIFZHRBARHVAHWAVFBEUVBPZHUKERMBZLLUNZ"\
     "HIGBUXJCYQWJIOAMRBPMLLTSQVZSABEPDBZHLGGWAFZQQMKVRLTEURZH"\
     "LGYQJLWQKTEPRCLVRJQMYABZXJOSMMPPHBWPBNUBKSJILECGSGFKAEAV"\
     "BGPXTCYWGZGGAPVFRIMNCESHEZVVVFZAQLCOALTERVAMYOGAMLTSOHZB"\
     "TBHSZBENGHUMKFQXATJYRLIVBPZHWTVVVISQCEFIFTRPSFURWFVMGUOA"\
     "APVFRLMYSRVZZBRMKRBQQMNWYTWFOYTMYSOMMKSEABEQRBAVUEIGZHRQ"\
     "LMSEGARFQIGUKVTEFDCWLVABZXISFQLKOAKXKCGPXVLCTHJWBVTERJPX"\
     "EKVTENSFMXRZYBAZGFQKKCZWKICJEAVBJMARJRLKZZYMWRVBTXRBFEXI"\
     "SQBAVSAOBESRZ"


def ioc(m, key_len=None):
    if not key_len:
        n = 1.0 * len(m)
        return sum([(m.count(k) / n)**2 for k in ALPHABET])
    out = []
    for i in range(key_len):
        sub_message = [m[j] for j in range(i, len(m), key_len)]
        n = 1.0 * len(sub_message)
        out.append(sum([(sub_message.count(k) / n)**2 for k in ALPHABET]))
    return sum(out) / len(out)


ioc_m = ioc(m2)
ioc(m2, 5)
ioc(m2, 8)
for n in range(1,10):
    print n, ioc(m2, n)

# Decrypt message 
from operator import itemgetter

def freq_vector(m):
    n = 1.0 * len(m)
    return [(m.count(k) / n) for k in ALPHABET]


def shift_vector(m, offset):
    return [m[(j-offset) % 26] for j in range(26)]


def dot(u, v):
    return sum([u * v for u, v in zip(u,v)])

key_len = 5
W = [FRQ_ENGLISH[c] for c in ALPHABET]

key = []
for i in range(key_len):
    sub_message = [m2[j] for j in range(i, len(m2), key_len)]
    dots = []
    for k in range(0, 25):
        F = freq_vector(sub_message)
        Wk = shift_vector(W, k)
        dots.append((k, dot(F, Wk)))
    key.append(max(dots, key=itemgetter(1))[0])

[ALPHABET[j] for j in key]


def decrypt(m, key):
    n = len(key)
    out = ""
    for i in range(len(m)):
        k = key[i % n]
        out += ALPHABET[(ALPHABET.index(m[i]) - k) % 26]
    return out

decrypt(m2, key)
