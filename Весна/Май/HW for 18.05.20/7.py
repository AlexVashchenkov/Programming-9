import random

str = "exal greka cherez reku vidit greka v reke rak sunul greka ruku v reku rak za ruku greku tsap"

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
                                                                     
def quicksortPairs(l):
   if len(l) <= 1:
       return l
   else:
       mid = random.choice(l)
       bigger = list(filter(lambda x: x > mid, l))
       smaller = list(filter(lambda x: x < mid, l))
       equal = list(filter(lambda x: x == mid, l))
       return quicksortPairs(smaller) + equal + quicksortPairs(bigger)		

def plus(p1,p2):
	return ((p1[0],p2[0]),p1[1] + p2[1])

def step(l):
	min1 = l[0]
	min2 = l[1]
	min_pair = plus(min1,min2)
	return (quicksortPairs(l[2:len(l)].append(min_pair)))
		
def hafmann(l):
	if len(l) == 1:
		return l[0][0]
	else:
		return hafmann(step(l))	

print(hafmann(quicksortPairs(make_freqs(str))))