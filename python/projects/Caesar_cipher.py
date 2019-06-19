"""
A caesar cipher shifts the alphabet right a few letters and replaces each letter in a message to encode it.

If we shift the alphabet right 1 letter, we have:

>>> plaintext = "test"
>>> cipher = encrypt(plaintext, 1)
>>> cipher
uftu
>>> decrypt(cipher, 1)
test

"""

def encrypt(word, shift_right):
    import string
    ret = ''
    shift_dict = alphabet_dict(string.ascii_lowercase, shift_right)
    shift_dict = alphabet_dict(string.ascii_uppercase, shift_right, shift_dict)
    for letter in word:
        ret += shift_dict[letter]
    return ret

def alphabet_dict(alphabet, shift_right,ret = dict()):
    shifted = alphabet[shift_right:] + alphabet[:shift_right]
    for i in range(len(alphabet)):
        ret[alphabet[i]] = shifted[i]
    return ret

def decrypt(cipher, shift_right):
    return encrypt(cipher, 26-shift_right)

