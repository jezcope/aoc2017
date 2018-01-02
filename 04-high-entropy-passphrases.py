from fileinput import input
from collections import Counter


def is_valid(passphrase):
    counter = Counter(passphrase.split())
    return counter.most_common(1)[0][1] == 1


def is_valid_ana(passphrase):
    counter = Counter(''.join(sorted(word)) for word in passphrase.split())
    return counter.most_common(1)[0][1] == 1


lines = list(input())
print(sum(map(is_valid, lines)))
print(sum(map(is_valid_ana, lines)))
