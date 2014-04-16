--[[

rli
robot language interpreter

--]]

local computer=require("computer")
local os=require("os")
local term=require("term")
local event=require("event")
local component=require("component")

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
local rlvm=require("rlvm")
local exit=false

local commands={
  exit=function()
      exit=true
    end,
  mem=function()
      print("Available Memory: "..computer.freeMemory())
    end,
  help=function()
    print[[Commands:
/exit         exit the rl interpreter
/mem          display free memory
/dump         display the stack and registers
/reset        reset the stack and registers
/slave <addr> slave to a computer
   waits for a computer with the modem address
   <addr> to send programs, runs them, and sends
   results back to the computer. Requires
   wireless network card. For use with hlrl on
   the computer.
/help         display this. Herpderp.]]
    end,
  dump=rlvm.dump,
  reset=rlvm.reset,
  slave=function(partialAddy)
    if not partialAddy then
      print("/slave <address>\n<address> should be the whole or partial address of the modem of the computer to slave to.")
    end

    print("slaving to "..partialAddy.."\nctrl+c to return to prompt\nWaiting for instructions")
    local modem=component.modem
    modem.open(4201)

    modem.broadcast(4201,"slave ready")

    while true do
      local e={event.pull()}
      if e[1]=="key_down" and e[3]==3 then
        break
      end
      if e[1]=="modem_message" then
        sender,program=e[3],e[6]
        print("message from "..e[3])
        if sender:sub(1,#partialAddy)==partialAddy then
          rlvm.reset()
          local res,result=pcall(rlvm.run,program,true)
          modem.send(sender,4201,result)
        end
      end
    end
    modem.close(4201)

    print("ctrl+c detected, leaving slave mode and returning to prompt")

  end

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
      f(table.unpack(argt))
    else
      print("unknown command, /help for help")
    end
  else

    prog=prog:sub(1,#prog-1):gsub("\\n","\n")

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