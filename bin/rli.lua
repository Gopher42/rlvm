--[[

rli
robot language interpreter

--]]

local computer=require("computer")
local os=require("os")
local term=require("term")

if os.getenv("SHELL")=="/bin/sh" then
  os.setenv("SHELL",require("process").running())
  local oldread=term.read
  term.read=function(...) term.read=oldread return "exit" end
  return
end

do
  local keep={computer=true,print=true,package=true,component=true,term=true,filesystem=true,buffer=true,os=true,unicode=true}

  for k,v in pairs(package.loaded) do
    if not keep[k] then
      package.loaded[k]=nil
      package.preload[k]=nil
    end
  end
end
--_G={}


--TODO: figure out a way to generalize this, for now just hard-coded in the raw disk mount
local rlvm=require("/mnt/a4f/lib/rlvm")
local exit=false

local commands={
  exit=function()
      exit=true
    end,
  mem=function()
      print("Available Memory: "..computer.freeMemory())
    end,
  help=function()
    end,
  dump=rlvm.dump,
}

term.clear()
local history={}
print("RobotLang Interpreter v0.5a\n\n/help for control commands\n/exit to exit\n\nbytecode mode")
while not exit  do
  term.write("bc]")
  local prog=term.read(history)

  if prog:match("^/") then
    --command
    local cmd,args=prog:match("^/(%w+)%s*(.*)$")
    local f=commands[cmd]
    if f then
      local argt={}
      args:gsub("(%S+)",function(v) argt[#argt+1]=v end)
      f(argt)
    else
      print("unknown command, /help for help")
    end
  else

    prog=prog:sub(1,#prog-1)

    local res,err=pcall(rlvm.run,prog,true)

    if res then
      print("\nOK")
    else
      print(err)
    end
  end
end

if exit then
  computer.shutdown(true)
end