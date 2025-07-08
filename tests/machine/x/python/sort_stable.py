items = [{"n": 1, "v": "a"}, {"n": 1, "v": "b"}, {"n": 2, "v": "c"}]
result = [x[1] for x in sorted([( i["n"], i["v"] ) for i in items], key=lambda x: x[0])]
print(result)
