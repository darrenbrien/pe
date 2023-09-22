powers = range(1, 100)

numbers = range(10**10)

cnt = 0
for j in powers:
    for i in numbers:
        k = i ** j
        l = len(str(k)) 
        if l > j:
            break
        elif l == j:
            print(k, i, j)
            cnt += 1
print(cnt)

