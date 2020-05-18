l = [6,3,5,8,1,4,2,7]

def swap(l,a,b):
    t = l[a]
    l[a] = l[b]
    l[b] = t
    return l

def make_heap(arr, n, i):
    largest = i 
    l = 2 * i + 1   
    r = 2 * i + 2   

    if l < n and arr[i] < arr[l]:
        largest = l

    if r < n and arr[largest] < arr[r]:
        largest = r

    if largest != i:
        arr[i],arr[largest] = arr[largest],arr[i] 

        make_heap(arr, n, largest)

def heap_sort(l):
    n = len(l)

    for i in range(n, -1, -1):
        make_heap(l, n, i)

    for i in range(n-1, 0, -1):
        swap(l,i,0)
        make_heap(l, i, 0)	
    return l

print(heap_sort(l))
