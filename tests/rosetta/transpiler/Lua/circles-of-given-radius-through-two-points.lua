-- Generated by Mochi v0.10.52 on 2025-08-02 01:52 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}
function sqrtApprox(x)
  local g = x
  local i = 0
  while (i < 40) do
    g = ((g + (x / g)) / 2)
    i = (i + 1)
  end
  return g
end;

function hypot(x, y)
  return sqrtApprox(((x * x) + (y * y)))
end;

function circles(p1, p2, r)
  if ((p1.x == p2.x) and (p1.y == p2.y)) then
    if (r == 0) then
      return {p1, p1, "Coincident points with r==0.0 describe a degenerate circle."}
    end
    return {p1, p2, "Coincident points describe an infinite number of circles."}
  end
  if (r == 0) then
    return {p1, p2, "R==0.0 does not describe circles."}
  end
  local dx = (p2.x - p1.x)
  local dy = (p2.y - p1.y)
  local q = hypot(dx, dy)
  if (q > (2 * r)) then
    return {p1, p2, "Points too far apart to form circles."}
  end
  local m = {x = (((tonumber(p1.x) or 0) + (tonumber(p2.x) or 0)) / 2), y = (((tonumber(p1.y) or 0) + (tonumber(p2.y) or 0)) / 2)}
  if (q == (2 * r)) then
    return {m, m, "Points form a diameter and describe only a single circle."}
  end
  local d = sqrtApprox(((r * r) - ((q * q) / 4)))
  local ox = ((d * dx) / q)
  local oy = ((d * dy) / q)
  return {{x = (m.x - oy), y = ((tonumber(m.y) or 0) + (tonumber(ox) or 0))}, {x = ((tonumber(m.x) or 0) + (tonumber(oy) or 0)), y = (m.y - ox)}, "Two circles."}
end;

Two = "Two circles.";

R0 = "R==0.0 does not describe circles.";

Co = "Coincident points describe an infinite number of circles.";

CoR0 = "Coincident points with r==0.0 describe a degenerate circle.";

Diam = "Points form a diameter and describe only a single circle.";

Far = "Points too far apart to form circles.";

td = {{{x = 0.1234, y = 0.9876}, {x = 0.8765, y = 0.2345}, 2}, {{x = 0, y = 2}, {x = 0, y = 0}, 1}, {{x = 0.1234, y = 0.9876}, {x = 0.1234, y = 0.9876}, 2}, {{x = 0.1234, y = 0.9876}, {x = 0.8765, y = 0.2345}, 0.5}, {{x = 0.1234, y = 0.9876}, {x = 0.1234, y = 0.9876}, 0}};

for _, tc in ipairs(td) do
  p1 = tc[0 + 1]
  p2 = tc[1 + 1]
  r = tc[2 + 1]
  print((((("p1:  {" .. tostring(p1.x)) .. " ") .. tostring(p1.y)) .. "}"))
  print((((("p2:  {" .. tostring(p2.x)) .. " ") .. tostring(p2.y)) .. "}"))
  print(("r:  " .. tostring(r)))
  res = circles(p1, p2, r)
  c1 = res[0 + 1]
  c2 = res[1 + 1]
  caseStr = res[2 + 1]
  print(("   " .. tostring(caseStr)))
  if ((caseStr == "Points form a diameter and describe only a single circle.") or (caseStr == "Coincident points with r==0.0 describe a degenerate circle.")) then
    print((((("   Center:  {" .. tostring(c1.x)) .. " ") .. tostring(c1.y)) .. "}"))
  else
    if (caseStr == "Two circles.") then
      print((((("   Center 1:  {" .. tostring(c1.x)) .. " ") .. tostring(c1.y)) .. "}"))
      print((((("   Center 2:  {" .. tostring(c2.x)) .. " ") .. tostring(c2.y)) .. "}"))
    end
  end
  print("")
end;
