# Counter

This is a simple web service that counts the number of POSTs it receives on
localhost:9000 at route /input with string argument named "inputParam".

It outputs the number of times it sees such strings upon a GET query on
localhost:9000 at route /query with string argument named "queryParam".


## Testing


See the python file in the `test` directory:

```bash
$ python3 send-requests.py
The words we post are:
['interwove', "psychosis's", 'domes', 'impregnated']
We post these each this many times:  240
Results:


Query response was :
 b'We\'ve seen "impregnated" this many times: 240'
Query response was :
 b'We\'ve seen "rollicks" this many times: 0'
Query response was :
 b'We\'ve seen "interwove" this many times: 240'
Query response was :
 b'We\'ve seen "slippage\'s" this many times: 0'
Query response was :
 b'We\'ve seen "puckish" this many times: 0'
Query response was :
 b'We\'ve seen "domes" this many times: 240'
Query response was :
 b'We\'ve seen "sheep" this many times: 0'
Query response was :
 b'We\'ve seen "psychosis\'s" this many times: 240'
```


