import random

l = [6,3,5,8,1,4,2,7]

def quicksort(l):
   if len(l) <= 1:
       return l
   else:
       mid = random.choice(l)
       bigger = list(filter(lambda x: x > mid, l))
       smaller = list(filter(lambda x: x < mid, l))
       equal = list(filter(lambda x: x == mid, l))
       return quicksort(smaller) + equal + quicksort(bigger)		

print(quicksort(l))