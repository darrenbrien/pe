
def read():
    with open('p096_sudoku.txt') as f:
        lines = f.readlines()
        puzzles = []
        puzzle = []
        for i, l in enumerate(lines):
            if i % 10 == 0:
                if len(puzzle) > 0:
                    puzzles.append(puzzle)
                    puzzle = []
            else:
                puzzle.append(list(map(int, str.strip(l))))
        puzzles.append(puzzle)
        return puzzles

def solve(g):
    if(solved(g)):
        return g
    i, j = next(emps(g))
    ng = list(map(list, g))
    for c in pos_vals(g, i, j):
        ng[i][j] = c
        res = solve(ng)
        if(res):
            return res
    return None

def pos_vals(g, i , j):
    def grid(g, i, j):
        idx = [0,3,6]
        start_i = idx[i//3]
        end_i = start_i + 3
        start_j = idx[j//3]
        end_j = start_j + 3
        l = [g[i][start_j:end_j] for i in range(start_i, end_i)]
        return [y for x in l for y in x]
    def col(g, j):
        return [row[j] for row in g]
    vals = set(list(range(1,10)))
    vals = vals - set(g[i])
    vals = set(vals) - set(col(g,j))
    return set(vals) - set(grid(g, i, j))

def emps(g):
    return ((i,j) for i in range(0,9) for j in range(0,9) if g[i][j] == 0)

def solved(g):
    return not any(map(lambda x: 0 in x, g))

print(sum(map(lambda x: int(''.join(map(str,x[0][0:3]))),[solve(r) for r in read()])))
