--gsh
local term=require("term")

print(require("computer").freeMemory())

while true do
  term.write("> ")
  term.read()
end