-- Generated by Mochi v0.10.42 on 2025-07-27 16:50 GMT+7
function input()
  return io.read('*l')
end
local _nil = {}
function main()
  print("program start")
  local ev = {set = false}
  print("program sleeping")
  print("task start")
  ev.set = true
  print("program signaling event")
  if ev.set then
    print("event reset by task")
    ev.set = false
  end
end;

main();
