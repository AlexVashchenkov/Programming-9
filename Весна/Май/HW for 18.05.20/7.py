str = "aaaaaaaaaaaaaaabbbbbbbvvvvvvggggggddddd"

def freq(str,s):
	n = 0
	for i in range(len(str)):
		if str[i] == s:
			n += 1
	return n

def count(l,s):
	n = 0
	for i in range(len(l)):
		if l[i] == s:
			n += 1
	return n

def make_list(str):
	l = []
	for i in range(len(str)):
		if count(l,str[i]) == 0:
			l.append(str[i])
	return l

def make_freqs(str):         
	l = make_list(str)
	for i in range(len(l)):
		l[i] = (l[i], (count(str,l[i])))
	return l

def plus(p1,p2):
	[((p1[0],p2[0]),p1[1] + p2[1])]	

def step(l):
	min1 = min(l)
	min2 = min(l.remove(min1))
	min_pair = plus(min1,min2)
	((l.remove(min1)).remove(min2)).append(min_pair)

def haffman(l):
	if len(l) == 1:
		return l[0]
	else:
		(haffman(step(l)))			
	
print(haffman(make_freqs(str)))
