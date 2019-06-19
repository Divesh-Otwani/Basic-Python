"""

A function for all returning all keys in a dict that have unique values.
"""

def uniqueValues(d):
    val_to_key = dict()
    ret = []
    for key in d:
        value = d[key]
        try: 
            val_to_key[value] #does the entry exist?
            val_to_key[value] = None
        except: val_to_key[value] = key
    for key in val_to_key:
        key_or_none = val_to_key[key]
        if key_or_none != None: ret.append(key_or_none)
    ret.sort()
    return ret
