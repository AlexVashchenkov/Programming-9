def n_th_katalan(n):
	if n > 0:
		return (2*(2*n-1)*(n_th_katalan(n-1)))/(n+1)
	else:
		return 1
      
print(n_th_katalan(3))

def print_katalan(n):
	if n > 0:
		print(n_th_katalan(n))
		n -= 1


	

	