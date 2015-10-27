import sys
            
def checkForSame(s1, s2):
    shortStLen = 0
    if len(s1) > len(s2):
        shortStLen = -len(s2) - 1
    else:
        shortStLen = -len(s1) - 1
    
    sameSub = ''
    for i in range(-1, shortStLen, -1):
        c = s1[i]
        c2 = s2[i]
        
        if c == c2:
            sameSub = c + sameSub
        else:
            break
    return sameSub
        

string1 = "bobilly"
string2 = "bobby"

sameSub = checkForSame(string1, string2)
print sameSub