#Normally the annualInt and balance would be defined for us


def remainbal(b,p):
    for g in range(12):        
        b -= p
        b = b*(1+r)
    return b  
     
r = annualInterestRate / 12    
b = balance

low = b/12.0
high = remainbal(b,0)/12.0
while True:
    p = (low+high)/2.0
    if abs(remainbal(b,p)) < 0.0001:
        break
    elif remainbal(b,p) > 0:
        low = p
    elif remainbal(b,p) < 0: 
        high = p
print ('Lowest Payment: '+ str(round(p,2)))
