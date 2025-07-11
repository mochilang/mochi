local testpkg = { Add = function(a,b) return a + b end, Pi = 3.14, Answer = 42 }
print(testpkg.Add(2, 3))
print(testpkg.Pi)
print(testpkg.Answer)
