"""
In this module, I show an example of encryption and decryption.


----------------------------------------------------- Example Below ---------------------------

>>> message = "RSA is one of the first practical public-key cryptosystems and is widely used for secure data transmission. In such a cryptosystem, the encryption key is public and differs from the decryption key which is kept secret. In RSA, this asymmetry is based on the practical difficulty of factoring the product of two large prime numbers, the factoring problem. RSA is made of the initial letters of the surnames of Ron Rivest, Adi Shamir, and Leonard Adleman, who first publicly described the algorithm in 1977. Clifford Cocks, an English mathematician working for the UK intelligence agency GCHQ, had developed an equivalent system in 1973, but it was not declassified until 1997."
>>> to_send = encrypt(message)
>>> to_send
[1739541816064715192620440563759080580386498684137193359885896722916983722280785677884407611800691919914290674888168808623794214111046513120282117455166840992252808517321282154378898570225653994632313150709218263938937709655038358295234311211667583718012998898946999, 2355589430254874662443122906342582819263004651784965153879184726169121613992607850662160561301284462355116228889791289835426774645161359536612287288172223751454266778877460620853235143232192018263061799056397552408739918415430848396032890405416515898684705406878048, 3748111312833974819825774278587292194009319129879163059004931808624641999459408819786102173007622702994737503594297052558294525133184531117222482070424272145169676491369236288600272453298452660576170185302305183809216903561798028824076931553813108829102831866124641, 4045548549777942607415964241786990226160276246706278158296431332110882114333410580797935806489575042013948688001976860208404781778129720450075094606366360512400937091263193770408033109153568849680525611767216261454842086338016310950569185484449263856166955551345692, 1056536240971886909898784600988822420288592925125795350140022689276190718321237453933263797780209872106144264523201718144122268619335462091317474870288468375136015520116137617865931737492677491085102957368339431514370740620775005777430156977980621460650681532742107, 1045413493525338318851713800387708431442424797620979015569608124576192840617287692435968305399786669704368906313459593757719258421289482071324901618255757580275830209215802222311627682228410877470433753034395106951303593119505582162278438577449571518770547550363516]
>>> seen = decrypt(to_send)
>>> seen
'RSA is one of the first practical public-key cryptosystems and is widely used for secure data transmission. In such a cryptosystem, the encryption key is public and differs from the decryption key which is kept secret. In RSA, this asymmetry is based on the practical difficulty of factoring the product of two large prime numbers, the factoring problem. RSA is made of the initial letters of the surnames of Ron Rivest, Adi Shamir, and Leonard Adleman, who first publicly described the algorithm in 1977. Clifford Cocks, an English mathematician working for the UK intelligence agency GCHQ, had developed an equivalent system in 1973, but it was not declassified until 1997.'


"""

All_Letters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~ \t\n\r\x0b\x0c'

base = len(All_Letters)

Modulus =4119940280779675810022921259284245460559858558280858174443671742727861351717781180850712292669564212633550468652151818522949537822261790748212158977907046663595668441339029103875803587099922960135438455589632921082295829497118338777580461070964820851310719196577479 

DecipherExp = 882049561005533229937769629202088103775201419522387674834355527750959345498759292438108918297246676952886867352157440310361994882440723941454593967348109582002406963680919898687142626313800434615277998100610544549300737093216417726701604602676847544957695405160033

EncipherExp = 2**16 +1







################ The two main functions below ###########################


def encrypt(string):
    return [pow(string_to_num(i), EncipherExp, Modulus) for i in split_len(string, 132)]



def decrypt(integer_list):
    return "".join(number_to_string(pow(i, DecipherExp, Modulus)) for i in integer_list)


#little helper:
# this split_len function was found online (code.activestate.com) and is made by Ian Bicking
def split_len(seq, length):
    return [seq[i:i+length] for i in range(0, len(seq), length)]


######################## If you are cool and want to do this to a text file ###########

def encrypt_txt(filepath, file_name):
    """
    filepath (a string) is of the format 'C:\\Users\\Name\\File1'
    where the text file is in File1

    the file_name is a string of the format "FileName.txt"
    
    """
    import os
    os.chdir(filepath)
    file = open(file_name, 'r')
    return encrypt(file.read())


def decrypt_txt(filepath, file_name, integer_list):
    """
    filepath and name are both integers:

    filepath (a string) is of the format 'C:\\Users\\Name\\File1'
    where the text file is in File1

    the file_name is a string of the format "FileName.txt"
    
    """
    import os
    os.chdir(filepath)
    file = open(file_name, 'w')
    file.write(decrypt(integer_list))


#######################################################################




#This is the needed encryption helper
def string_to_num(string):
    exp = range(len(string))
    num = list(map(All_Letters.find, string))
    return sum((base**i)*(num[i]) for i in exp)


# Below are the needed decryption helpers
def number_to_string(num):
    return to_string(to_base_b_from_ten(num, base))

def to_base_b_from_ten(integer, base):
    ret = tuple()
    remaining = integer

    while remaining > 0:
        ret += (remaining%base,)
        remaining = remaining // base
    return ret


def to_string(tup):
    ret = ""
    for i in tup:
        ret += All_Letters[i]
    return ret


# use this to determing how large the pieces of the string can be
def largest_str_len():
    last= All_Letters[-1]
    string = last
    last_count = 0 # we need one less of we started at 1, need the bound for parsing as one less than too big
    while string_to_num(string) < Modulus:
        string += last
        last_count += 1
    return last_count







