--[[

RLVM - RobotLanguage virtual machine

--]]

local robot=require("robot")
local term=require("term")
local component=require("component")
local gpu=component.gpu
local computer=require("computer")

local _,nav=pcall(function() return component.navigation end)
nav=_ and nav or nil

local rlvm={}

--registers
local a,b,n,s=1,false,0,""

    --stacks
local sa,sb,sn,ss={},{},{},{}
local stacks={ n=sn, a=sa, b=sb, s=ss }
local program,running="",false
local width,height=gpu.getResolution()


local sideConv={
   [98]="b",
   [102]="f",
   [114]="r",
   [108]="l",
   [100]="d",
   [117]="u",
}

local side4Conv={
   [98]="b",
   [102]="f",
   [114]="r",
   [108]="l",
}

local dirConv={
    [115]="s",
    [119]="w",
    [110]="n",
    [101]="e",
  }

local regConv={
    [98]="b",
    [110]="n",
    [115]="s",
  }

local regxConv={
    [98]="b",
    [110]="n",
    [115]="s",
    [116]="t",
  }

--extended registers, read-only
local regxrConv={
    [98]="b",
    [110]="n",
    [115]="s",
    [116]="t",
    [120]="x",
    [121]="y",
    [122]="z",
    [101]="e",
    [100]="d",
    [108]="l",
    [107]="k",
    [109]="m",
    [100]="d",
    [113]="q",
    [118]="v",
  }

local readReg={
    [ 98]=function() return b end,
    [110]=function() return n end,
    [115]=function() return s end,
    [116]=function() return robot.select() end,
    [120]=function() return nav and ({nav.getPosition()})[1] or 0 end,
    [121]=function() return nav and ({nav.getPosition()})[2] or 0 end,
    [122]=function() return nav and ({nav.getPosition()})[3] or 0 end,
    [101]=function() return math.floor(computer.energy()) end,
    [100]=function() return math.floor(robot.durability()) end,
    [108]=function() return math.floor(robot.level()) end,
    [107]=function() return sn[#sn] end,
    [109]=function() return robot.name() end,
    [113]=function() local d,msg=robot.durability() return d~=nil or msg=="tool cannot be damaged" end,
    [118]=function() return nav and nav.getPosition()~=nil or false end,
  }
local hexConv={
    [48]=1,
    [49]=2,
    [50]=3,
    [51]=4,
    [52]=5,
    [53]=6,
    [54]=7,
    [55]=8,
    [56]=9,
    [57]=10,
    [65]=11,
    [66]=12,
    [67]=13,
    [68]=14,
    [69]=15,
    [70]=16,
}

local b64decode = { [65]=0, [66]=1, [67]=2, [68]=3, [69]=4, [70]=5, [71]=6, [72]=7, [73]=8, [74]=9, [75]=10, [76]=11, [77]=12, [78]=13, [79]=14, [80]=15, [81]=16, [82]=17, [83]=18, [84]=19, [85]=20, [86]=21, [87]=22, [88]=23, [89]=24, [90]=25, [97]=26, [98]=27, [99]=28, [100]=29, [101]=30, [102]=31, [103]=32, [104]=33, [105]=34, [106]=35, [107]=36, [108]=37, [109]=38, [110]=39, [111]=40, [112]=41, [113]=42, [114]=43, [115]=44, [116]=45, [117]=46, [118]=47, [119]=48, [120]=49, [121]=50, [122]=51, [48]=52, [49]=53, [50]=54, [51]=55, [52]=56, [53]=57, [54]=58, [55]=59, [56]=60, [57]=61, [45]=62, [95]=63, }

local function nop() end

--local validInstructions = "MFDUPRSCTHOKB><+-*/%^!&|=JILNV#'01X"
local function nextByte()
  --[[if a>#program then
    error("out of program data")
  end]]
  local b=program:byte(a)
  a=a+1
  return b
end

local function nextB64()
  local v=b64decode[nextByte()]
  --[[if not v then
    error("invalid b64 value")
  end]]
  return v
end

local function pullSide()
  return sideConv[nextByte()]
end

local function pullSide4()
  return side4Conv[nextByte()]
end

local function pullDir()
  return dirConv[nextByte()]
end

local function pullReg()
  return regConv[nextByte()]
end

local function pullRegx()
  return regxConv[nextByte()]
end

local function pullRegxr()
  local byte=nextByte()
  local reg=regxrConv[byte]
  if reg then
    return readReg[byte]()
  else
    error("Invalid regx argument")
  end
end

local function readNumLit()
  local len=nextB64()
  v=0
  for i=1,len do
    v=v*64+nextB64()
  end
  return v
end

local function pullImm()
  local byte=nextByte()
  local v
  if byte==35 then
    v=readNumLit()
  elseif byte==39 then
    local len=nextB64()
    v=program:sub(a,a+len-1)
    a=a+len
  elseif byte==48 then
    v=false
  elseif byte==49 then
    v=true
  else
    error("expected immediate value")
  end
  return v
end

local function pullNum()
  local byte=nextByte()
  local v
  if byte==35 then
    v=readNumLit()
  elseif byte==110 then
    v=n
  elseif byte==107 then
    v=sn[#sn]
  else
    error("Expected number literal, n, or k")
  end
  return v
end

local function pullSlot()
  local byte=nextByte
  if byte==110 then
    return n
  elseif byte==107 then
    return sn[#sn]
  else
    return hexConv[byte]
  end
end


local turnTo={
    l=robot.turnLeft,
    r=robot.turnRight,
    b=robot.turnAround,
    a=robot.turnAround,
    f=nop,
    u=nop,
    d=nop,

  }

local turnBack={
    l=robot.turnRight,
    r=robot.turnLeft,
    b=robot.turnAround,
    f=nop,
    u=nop,
    d=nop,
  }

local move={
    f=robot.forward,
    b=robot.back,
    u=robot.up,
    d=robot.down,
    l=robot.forward,
    r=robot.forward,
  }

local place={
    f=robot.place,
    b=robot.place,
    u=robot.placeUp,
    d=robot.placeDown,
    l=robot.place,
    r=robot.place,
  }


local swing={
    f=robot.swing,
    b=robot.swing,
    u=robot.swingUp,
    d=robot.swingDown,
    l=robot.swing,
    r=robot.swing,
  }

local use={
    f=robot.use,
    b=robot.use,
    u=robot.useUp,
    d=robot.useDown,
    l=robot.use,
    r=robot.use,
  }

local drop={
    f=robot.drop,
    b=robot.drop,
    u=robot.dropUp,
    d=robot.dropDown,
    l=robot.drop,
    r=robot.drop,
  }

local suck={
    f=robot.suck,
    b=robot.suck,
    u=robot.suckUp,
    d=robot.suckDown,
    l=robot.suck,
    r=robot.suck,
  }

local compare={
    f=robot.compare,
    b=robot.compare,
    u=robot.compareUp,
    d=robot.compareDown,
    l=robot.compare,
    r=robot.compare,
  }

local detect={
    f=robot.detect,
    b=robot.detect,
    u=robot.detectUp,
    d=robot.detectDown,
    l=robot.detect,
    r=robot.detect,
  }


local function instr_move()
  --parse out args
  local side, dist=pullSide(),pullNum()
  local res,reason=true
  if side~="b" then
    turnTo[side]()
  end
  for i=1,dist do
    res,reason=move[side]()
    if not res then
      dist=i
      break
    end
  end
  if side~="b" then
    turnBack[side]()
  end
  b,n,s=res,dist,reason or s
end

local function instr_face()
  turnTo[pullSide4()]()
end

local function instr_dig()
  local side=pullSide()
  local res,reason
  turnTo[side]()
  res,reason=swing[side]()
  turnBack[side]()
  b,s=res,reason or s
end

local function instr_use()
  local side=pullSide()
  turnTo[side]()
  local res,reason=use[side]()
  turnBack[side]()
  b,s=res,reason or s
end

local function instr_place()
  local side,slot=pullSide(),pullSlot()
  turnTo[side]()
  robot.select(slot)
  b=place[side]()
  turnBack[side]()
end

local function instr_drop()
  local side,slot,count=pullSide(),pullSlot(),pullNum()
  turnTo[side]()
  robot.select(slot)
  b=drop[side](count)
  turnBack[side]()
end

local function instr_suck()
  local side=pullSide()
  turnTo[side]()
  b=suck[side]()
  turnBack[side]()
end

local function instr_goto()
  --todo: this
end

local function instr_compare()
  local compTo=nextByte()
  local slot=hexConv[compTo]
  if slot then
    local slot2=pullSlot()
    robot.select(slot2)
    b=robot.compareTo(slot)
  else
    side=sideConv[compTo]
    if side then
      local slot=pullSlot()
      turnTo[side]()
      robot.select(slot)
      b=compare[side]()
      turnBack[side]()
    else
      error("invalid first arg to compare, expected slot or side")
    end
  end
end

local function instr_detect()
  local side=pullSide()
  turnTo[side]()
  b,s=detect[side]()
  turnBack[side]()
end

local function instr_transfer()
  local slot1,slot2,count=pullSlot(),pullSlot(),pullNum()
  robot.select(slot1)
  b=robot.transferTo(slot2,count)
end

local function instr_push()
  local byte=nextByte()
  local v
  if regxrConv[byte] then
    v=readReg(byte)
  elseif byte==35 then
    v=readNumLit()
  elseif byte==39 then
    local len=nextB64()
    v=program:sub(a,a+len-1)
    a=a+len
  elseif byte==48 then
    v=false
  elseif byte==49 then
    v=true
  else
    error("invalid argument to push")
  end

  if type(v)=="number" then
    sn[#sn+1]=v
  elseif type(v)=="boolean" then
    sb[#sb+1]=v
  elseif type(v)=="string" then
    ss[#ss+1]=v
  end
end

local function instr_load()
  local val=pullImm()
  if type(val)=="number" then
    n=val
  elseif type(val)=="boolean" then
    b=val
  elseif type(val)=="string" then
    s=val
  end
end

local function instr_loadn()
  n=readNumLit()
end

local function instr_loadb0()
  b=false
end

local function instr_loadb1()
  b=true
end

local function instr_loads()
  local len=b64decode[nextByte()]
  s=program:sub(a,a+len-1)
  a=a+len
end

local function pop(stack)
  local l=#stack
  local v=stack[l]
  table.remove(stack,l)
  return v
end

local function instr_pop()
  local reg=pullRegx()
  if reg=="b" then
    b=pop(sb)
  elseif reg=="n" then
    n=pop(sn)
  elseif reg=="s" then
    s=pop(ss)
  elseif reg=="t" then
    robot.select(pop(sn))
  end
end

local function instr_peek()
  local reg,index=pullRegx(),pullNum()
  if reg=="b" then
    local l=#sb
    b=sb[l-index+1]
  elseif reg=="n" then
    local l=#sn
    n=sn[l-index+1]
  elseif reg=="s" then
    local l=#ss
    s=ss[l-index+1]
  elseif reg=="t" then
    robot.select(sn[l-index+1])
  end
end

local function instr_poke()
  local byte,index=nextByte(),pullNum()
  local reg=regxrConv[byte]
  local v
  if reg then
    v=readReg[byte]()
    local stack=sn
    if type(v)=="boolean" then
      stack=sb
    elseif type(v)=="string" then
      stack=ss
    end
  elseif byte==35 then
    v=readNumLit()
    stack=sn
  elseif byte==39 then
    local len=nextB64()
    v=program:sub(a,a+len-1)
    a=a+len
    stack=ss
  elseif byte==48 then
    v=false
    stack=sb
  elseif byte==49 then
    v=true
    stack=sb
  else
    error("Invalid arg#1, expected register or immediate")
  end
  stack[#stack-index+1]=val
end

local function instr_burn()
  local reg,count=pullReg(),pullNum()
  local cs
  if reg=="b" then
    cs=sb
  elseif reg=="n" then
    cs=sn
  elseif reg=="s" then
    cs=ss
  end
  local csl=#cs
  if csl<count then
    error("attempt to burn past head of stack")
  end

  for i=1,count do
    table.remove(cs,count)
    count=count-1
  end
end

local function instr_gt()
  local n2=pop(sn)
  b=n2>n
end

local function instr_lt()
  local n2=pop(sn)
  b=n2<n
end

local function instr_add()
  local n2=pop(sn)
  n=n2+n
end

local function instr_sub()
  local n2=pop(sn)
  n=n2-n
end

local function instr_mul()
  local n2=pop(sn)
  n=n2*n
end

local function instr_div()
  local n2=pop(sn)
  n=math.floor(n2/n)
end

local function instr_mod()
  local n2=pop(sn)
  n=n2%n
end

local function instr_pow()
  local n2=pop(sn)
  n=n2^n
end

local function instr_not()
  b=not b
end

local function instr_and()
  local b2=pop(sb)
  b=b2 and b
end

local function instr_or()
  local b2=pop(sb)
  b=b2 or b
end

local function instr_eq()
  local reg=pullRegx()
  if reg=="n" then
    b=n==pop(sn)
  elseif reg=="b" then
    b=b==pop(sb)
  elseif reg=="s" then
    b=s==pop(ss)
  elseif reg=="t" then
    b=robot.select()==pop(sn)
  end
end

local function instr_jump()
  a=pullNum()
end

local function instr_jif()
  local addr=pullNum()
  if b then
    a=addr
  end
end

local function instr_call()
  local addr=pullNum()
  sa[#sa+1]=a
  a=addr
end

local function write(v)
  v=tostring(v)
  local x,y=term.getCursor()
  if x+#v>width then
    term.write(v:sub(1,width-x))
    if y==height then
      gpu.copy(1,2,width,height-1,0,-1)
      gpu.fill(1,height,width,1," ")
      y=y-1
    end
    term.setCursor(1,y+1)
    write(v:sub(width-x+1))
  else
    term.write(v)
  end
end

local function instr_print()
  local val=pullRegxr()
  write(val)
end

local function instr_return()
  if #sa==0 then
    running=false
  else
    a=pop(sa)
  end
end


--this table has been keyed by the byte values of the bytecode characters, to save overhead converting
--them all the time
local instrTable = {
    [33]=instr_not,
    [62]=instr_gt,
    [68]=instr_dig,
    [84]=instr_detect,
    [67]=instr_compare,
    [66]=instr_burn,
    [124]=instr_or,
    [61]=instr_eq,
    [60]=instr_lt,
    [43]=instr_add,
    [45]=instr_sub,
    [42]=instr_mul,
    [47]=instr_div,
    [37]=instr_mod,
    [94]=instr_pow,
    [80]=instr_place,
    [79]=instr_pop,
    [78]=instr_return,
    [85]=instr_use,
    [38]=instr_and,
    [83]=instr_suck,
    [82]=instr_drop,
    [73]=instr_jif,
    [72]=instr_push,
    [70]=instr_face,
    [77]=instr_move,
    [76]=instr_call,
    [75]=instr_peek,
    [65]=instr_poke,
    [74]=instr_jump,
    [86]=instr_load,
    [35]=instr_loadn,
    [39]=instr_loads,
    [48]=instr_loadb0,
    [49]=instr_loadb1,
    [46]=instr_print,
    [88]=instr_transfer,
    [32]=function() end,
  }

--[[
!>DTCB|=<+-*/%^PONU&SRIHFMLKJV#'01.X
!>|=<+-*/%^&#'01.BCDFHIJKLMNOPRSTUVX

--]]

function rlvm.run(prog,debug)
  if not debug then
    rlvm.reset()
  end

  program=prog
  a=1
  running=true
  local progLen=#program
  if debug then
    while running and a<=progLen do
      local byte=nextByte()
      if instrTable[byte]==nil then
        error("Invalid instruction "..string.char(byte).."("..byte..")")
      end
      local pa=a
      local res,err=pcall(instrTable[byte])
      if not res then
        print("error executing instruction "..string.char(byte).."("..byte..") : "..(err or ""))
        return
      end
    end
  else
    while running and a<=progLen do
      local byte=nextByte()
      if instrTable[byte]==nil then
        error("Invalid instruction "..string.char(byte).."("..byte..")")
      end
      instrTable[byte]()
    end
  end
end

function rlvm.dump()
  print("a:"..a,"b:"..tostring(b),"n:"..n,"s:"..s)
  local function printStack(n,s)
    local str=n..":"
    for i=1,#s do
      str=str.." "..tostring(s[i])
    end
    print(str)
  end
  printStack("sn",sn)
  printStack("sb",sb)
  printStack("ss",ss)
end

function rlvm.reset()
  program=""
  a,b,n,s=1,false,0,""
  sa,sb,sn,ss={},{},{},{}
  running=true
end


return rlvm