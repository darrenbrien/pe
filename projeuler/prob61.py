triangles =[(3,j) for j in [int(i*(i +1)/2) for i in range(1,200)] if 999 < j < 10000]
squares =[(4,j) for j in [int(i*i) for i in range(1,200)] if 999 < j < 10000]
pentagons =[(5,j) for j in [int(i*(3*i -1)/2) for i in range(1,200)] if 999 < j < 10000]
hexagons =[(6,j) for j in [int(i*(2*i - 1)) for i in range(1,200)] if 999 < j < 10000]
heptagons =[(7,j) for j in [int(i*(5*i -3)/2) for i in range(1,200)] if 999 < j < 10000]
octagonals =[(8,j) for j in [int(i*(3*i -2)) for i in range(1,200)] if 999 < j < 10000]

lookit = {}
for i, j in triangles + squares + pentagons + hexagons + heptagons + octagonals:
    head = str(j)[:2]
    if head in lookit:
        lookit[head].append((i,j))
    else:
        lookit[head] = [(i,j)]

lookup = {}
for i, j in triangles + squares + pentagons + hexagons + heptagons + octagonals:
    tail = str(j)[-2:]
    if tail in lookit:
        p = [(k,v) for k, v in lookit[tail] if i != k and str(v)[2] != '0']
        if p:
            lookup[(i,j)] = sorted(p, key=lambda a: a[0], reverse=True)

def count_depth_rec(path):
    if len(path) == 7 and path[0] == path[-1]:
        print(sum([p for _, p in path[:-1]]))
        return
    path_len = len(set([p for p, _  in path]))
    if len(path) > path_len:
        return
    x = path[-1]
    if x not in lookup:
        return
    for y in lookup[x]:
        count_depth_rec(path + [y])

for x in lookup.keys():
    count_depth_rec([x])

