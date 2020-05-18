n = int(input("n = "))
m = int(input("m = "))

def mid_line():
	print("+-----+", end = "")
	for i in range(m):
		print("-----+" ,end = "" )                          
	print()


mid_line()
print("|     |" ,end = "" )                          
for i in range(1,m+1):
	print(" %3d |" % i, end = "")
print()
mid_line()

for i in range(1,n+1):
	print("| %3d |" % i ,end = "")
	for j in range(1,m+1):
		print(" %3d |" % (j*i), end = "")
	print()
	mid_line()

	
