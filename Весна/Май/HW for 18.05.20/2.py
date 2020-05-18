import math 

def print_circle(r):
	for y in range (r,-r-1,-1):
			for x in range (-r, r+1):
				if math.sqrt(x**2 + y**2) <= r and math.sqrt(x**2 + y**2) > r-1:
					print("*", end = "")
				else:
					print(" ", end = "")
			print()

print_circle(10)