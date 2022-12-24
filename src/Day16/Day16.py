class Valve:
    def __init__(self, flowrate, neighbors):
        self.flowrate = flowrate # int
        self.neighbors = neighbors # list

def dp(node, val, mins, graph):
    if mins == 0:
        return val
    cur = graph[node]
    flow = cur.flowrate
    neighs = cur.neighbors
    stay = dp(node, flow + val, (mins-1), dict({node : Valve(0,neighs)}, **graph))
    vals = []
    for n in graph[node].neighbors:
        vals.append(dp(n, val, mins - 1, graph))
    move = max(vals)
    return(max(move,stay))

g = {}

with open('ex.txt', 'r') as file:
    input = file.read().split('\n')
    input.pop()
    for i in input:
        x = i.split()
        name = x[1]
        rate = int(x[4][5 : -1])
        neighs = list(map(lambda x : x.rstrip(',') ,x[9 : ]))
        g[name] = Valve(rate,neighs)
    print(dp("AA",0,1,g))
