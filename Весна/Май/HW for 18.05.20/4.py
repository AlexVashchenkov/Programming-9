s = "iowjeoifewaljoijo"

s2 = "al"

def find_str(s,str):
	for i in range(len(s) - len(s2) + 1):
		if s[i:i+2] == s2:
			return i

print(find_str(s,s2))

