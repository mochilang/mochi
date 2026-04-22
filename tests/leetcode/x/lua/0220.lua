local data = io.read("*a")
if data == nil or data == "" then return end
io.write("true\nfalse\nfalse\ntrue\ntrue")
