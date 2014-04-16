--[[

compiler. \o/




refs...

only two kinds of refs that'll naturally happen, refs to functions and refs within a statement.

ex,

if (<cond>) {
  <stuff>
}

the if statement exists in one block, and contains a child block. In the case the condition
is not true, will have a ref to the next statement in the block with the if, for skipping to.

while (condition) {
  <stuff>
}
after condition, jump to statement following while
after stuff, jump to condition.


for (<init> ; <cond> ; <update>) {
  <stuff>
}
more complicated, compiles into multiple sections.
init and update are essentially compiled into assignment statements
and cond is compiled into an if. An implied cleanup statement may be added
at the end, to rmeove any new variables created in init.

becomes:
if <condition> {
  <stuff>
  jump to if
}
structure:
parent block {
   <stuff>
   for statement
      block {
        init statement
        ifnot <cond>
          block {
            <stuff>
            update statement
          }
        jumpto -2
        }
      cleanup statement
   <more stuff>

once expanded, we are left with basically the same scenario as the while statement
but the "next" statement jumped to on condition false is the implicitly added cleanup statement,
if any, otherwise the first statement after the for statement itself. We also add,

so, can compile these statements by just converting into a series of statements.

metastatements - not usable explicitly in code, but generated in compiling
ifnot<cond> - if not <cond> skip next
block - collection of sub-statements in a single statement-like wrapper
loop - jump back 2 statements

if(cond) <stat>

becomes simply

ifnot <cond>
<stat>

with a {} body it is the same except the body is compiled into a blockstat

while(cond) <stat>

similarly becomes

ifnot<cond>
<stat>
loop

for(init;cond;update) <stat>

becomes, in a block,
{
  init
  ifnot<cond>
  bodystay {
    <stat>
    <update>
  }
  loop
}

note, even single-statement body forms of for have a block body created
as the update statement gets appended to it.

obj level code is compiled to an array of ... individual instructions? seems like too much, but...
kindof has to be? Can bunch instructions, splitting into a new one only when a jmp is encountered..
if they're the first instruction in each segment, they can be easily indexed by segment number.

The other kind of reference is function calls, which get stored in a refs table
in the target function's definition table.


a=1 if(a<2) a=a+1 print(a)

#AHn#BBHnOnAn#BBKn#BBHn#BC<!I#B1Kn#BBHn#BB+HnOnAn#BBKn#BBHnOn.n
#AHn#BBAn#BBKn#BBHn#BC<!I#B?Kn#BBHn#BB+An#BBKn#BB.n

a,b=1,10000 if (a<0) b=a+1 elseif (a>0) b=a-1 else b=a print(b)

a=1 while(a<10) a=a+1 print(a)
#AHn#BBAn#BBKn#BBHn#BK<!I#BxKn#BBHn#BB+An#BBJ#BNKn#BB.n

a=1 if(true) { a=2 print(a) } print(a)
#AHn#BBAn#BB1!I#Bi#BCAn#BBKn#BB.nKn#BB.n

a=1 if(true) { var a=2 print(a) } print(a)
#AHn#BBAn#BB1!I#Br#AHn#BCAn#BBKn#BB.nBn#BBKn#BB.nBn#BB

for(a=1;a<10;a=a+1) print(a)
#BBAn#BBKn#BBHn#BK<!I#B5Kn#BB.nKn#BBHn#BB+An#BBJ#BJBn#BB

a=1 for(var a=1;a<10;a=a+1) print(a) print(a)
#AHn#BBAn#BB#AHn#BBAn#BBKn#BBHn#BK<!I#CBFKn#BB.nKn#BBHn#BB+An#BBJ#BZBn#BBKn#BB.nBn#BB

a,b=1,2 print(a,b,a+b)
#AHnHn#BBHn#BCAn#BCOnAn#BCKn#BBHnKn#BDHnKn#BDHnKn#BF+.nOn.nOn.nBn#BC
--]]

local term=require("term")
local text=require("text")
local event=require("event")
local component=require("component")


local oldprint=print
--[[
local lines=0
print=function(...)
  if lines==13 then
    term.write("smack a key")
    event.pull("key_down")
    oldprint("")
    lines=0
  end
  lines=lines+1
  oldprint(...)
end
--]]


local function tableJoin(t1,t2)
  if t1==nil then
    error("t1==nil?",2)
  end
  for i=1,#t2 do
    t1[#t1+1]=t2[i]
  end
end



local escapeChars={n="\r",r="\r",t="\t",["\\"]="\\",['"']='"'}

local function numNewlines(str,i1,i2)
  local count=0
  i1=i1 or 1
  str:sub(i1,i2):gsub("\n\r",function() count=count+1 end)
  return count
end


--string ::= "%b\"\"
local function parseString(source,index, lineNum)

  local _, openIndex=source:find("^%s*(%\")",index)
  if openIndex then
    --We either match or there's an error, nothing else can start this way.
    local closeIndex=openIndex+1
    while true do
      local i=closeIndex
      closeIndex=source:find("\"",closeIndex)
      if closeIndex==nil then
        error({line=lineNum, index=index, "Unterminated string"})
      end
      if #source:match("(\\*)\"",i)%2==0 then
        break
      end
      closeIndex=closeIndex+1
    end
    local literal=source:sub(openIndex+1,closeIndex-1)
    --convert escape characters
    literal=literal:gsub("\\(.)",function(v) return escapeChars[v] or "?" end)
    --count the newlines
    lineNum=lineNum+numNewlines(source,index,closeIndex)

    return {type="literal",valType="string",value=literal,index=openIndex,lineNum=lineNum},closeIndex+1,lineNum
  end
  return nil
end

--number ::= "%-?%d+"
local function parseNumber(source,index,lineNum)
  local space,match=source:match("^(%s*)(%-?%d+)",index)
  if match then
    local b,e=index+#space,index+#space+#match
    return {type="literal",valType="number",value=tonumber(match),index=b,lineNum=lineNum},e,lineNum+numNewlines(space)
  end
  return nil
end


--boolean ::= "true" | "false"
local function parseBoolean(source,index,lineNum)
  local space=source:match("^(%s*)",index)
  if index==nil then
    print(debug.traceback())
  end

  local s=index+#space
  local v,e
  if source:match("^true",s) then
    v=true
    e=s+4
  elseif source:match("^false",s) then
    v=false
    e=s+5
  end

  if v~=nil then
    return {type="literal", valType="boolean",value=v,index=s,lineNum=lineNum},e,lineNum+numNewlines(space)
  end
  return nil
end

local function parsePattern(pattern,source,index,lineNum)
  local space,match=source:match("^(%s*)("..pattern..")",index)
  if match then
    if index==nil then
      error("nil index?"..debug.traceback(),3)
    end
    local b,e=index+#space,index+#space+#match
    return {type="pattern",value=match,index=b,lineNum=lineNum},e,lineNum+numNewlines(space)
  end
end

local function parseOneOf(specs,source,index,lineNum)
  for specI=1,#specs do
    local spec=specs[specI]
    local r,i,l
    if type(spec)=="function" then
      r,i,l=spec(source,index,lineNum)
    else
      r,i,l=parsePattern(spec,source,index,lineNum)
    end
    if r then
      return r,i,l
    end
  end
  return nil
end

--literal= <number> | <string> | <boolean>
local function parseLiteral(source,index,lineNum)
  return parseOneOf({parseNumber,parseString,parseBoolean},source,index,lineNum)
end

--identifier ::= "[%a_][%w_]+"
local function parseIdentifier(source,index,lineNum)
  local space,match=source:match("^(%s*)([%a_][%w_]*)",index)
  if match then
    local b,e=index+#space,index+#space+#match
    return {type="identifier",value=match,index=b,lineNum=lineNum},e,lineNum+numNewlines(space)
  end
  return nil
end

local opOutput={
    ["=="]=function(t) return {{"="..t:sub(1)}} end,
    ["<="]=">!",
    [">="]="<!",
    ["~="]=function(t) return {{"="..t:sub(1)},{"!"}} end,
    ["and"]={{"&"}},
    ["or"]={{"|"}},
    ["unot"]={{"!"}},
    ["u++"]={{"H#BB"},{"+"}},
    ["u--"]={{"Hn"},{"#BB"},{"-"}},
    ["u~"]={{"!"}},
    ["u-"]={{"H#A"},{"-"}},
}

--binop ::= "[%%+-*/^<>]" | "and" | "or" | "==" | "<=" | ">=" | "~="
local function parseBinop(source,index,lineNum)
  local r,i,l= parseOneOf({"==","<=",">=","[%%%+%-%*/^<>]","and","or","~="},source,index,lineNum)
  if r then
    r.type="binop"
  end
  return r,i,l
end

--unop  ::= "[-~]" | "not" | "%+%+" | "%-%-"
local function parseUnop(source,index,lineNum)
  local r,i,l=parseOneOf({"[-~]","not", --[["%+%+","%-%-",]]},source,index,lineNum)
  if r then
    r.type="unop"
    r.value="u"..r.value
  end
  return r,i,l
end

--type ::= "num" | "string" | "bool"
local function parseType(source,index,lineNum)
  return parseOneOf({"num","bool","string"},source,index,lineNum)
end

local functionCall

local precedence={
    ["("]=0,
    ["or"]=1,
    ["and"]=1,
    ["=="]=2,
    ["<="]=2,
    [">="]=2,
    ["~="]=2,
    ["<"]=2,
    [">"]=2,
    ["+"]=3,
    ["-"]=3,
    ["*"]=4,
    ["/"]=4,
    ["%"]=4,
    ["~"]=4,
    ["u-"]=4,
    ["u~"]=4,
    ["^"]=5,
    ["unot"]=2,
    ["u++"]=3,
    ["u--"]=3,


  }
local opResTypes={
    ["or"]="boolean",
    ["and"]="boolean",
    ["=="]="boolean",
    ["<="]="boolean",
    [">="]="boolean",
    ["~="]="boolean",
    ["<"]="boolean",
    [">"]="boolean",
    ["+"]="number",
    ["-"]="number",
    ["*"]="number",
    ["/"]="number",
    ["%"]="number",
    ["~"]="boolean",
    ["^"]="number",
    ["u-"]="number",
    ["u~"]="boolean",
    ["u++"]="number",
    ["u--"]="number",
    ["unot"]="boolean",
  }

local opArgTypes={
    ["or"]="boolean",
    ["and"]="boolean",
    ["<="]="number",
    [">="]="number",
    ["~="]="number",
    ["<"]="number",
    [">"]="number",
    ["+"]="number",
    ["-"]="number",
    ["*"]="number",
    ["/"]="number",
    ["%"]="number",
    ["~"]="boolean",
    ["^"]="number",
    ["u-"]="number",
    ["u~"]="boolean",
    ["u++"]="number",
    ["u--"]="number",
    ["unot"]="boolean",
}
local parseFunctionCall

--expression ::=
--  [ <unop> ] <subexpression> [<binop> <expression>]
local function parseExpression(source,index,lineNum)
  --perhaps... kindof trainyard here?

  local outStack={}
  local opStack={}
  local unOpStack={}
  local opNext=false
  local r,i,l=nil,index,lineNum
  local done=false
  while not done do
    if opNext then
      local r2,i2,l2 = parseOneOf({parseBinop,"%)"},source,i,l)
      if r2 then
        if r2.type=="binop" then
          --binop!
          local op=r2.value
          --is this lower precedence than what's there now?
          local op2=opStack[#opStack]
          local p1,p2=precedence[op],precedence[op2]
          while op2 and (p1<p2 or p1==p2 and op~="^") do
            table.remove(opStack,#opStack)
            --pop and apply
            local new={type="binop",value=op2,valType=opResTypes[op2],index=index,lineNum=lineNum}
            if op2:match("^u") then
              if #outStack<1 then
                --fail
                return nil
              end
              new.type="unop"
              new.right=outStack[#outStack]
            else
              if #outStack<2 then
                --fail
                return nil
              end
              new.left=outStack[#outStack-1]
              new.right=outStack[#outStack]
            end

            table.remove(outStack,#outStack)
            outStack[#outStack]=new

            op2=opStack[#opStack]
            p2=precedence[op2]
          end
          --now push the new op
          opStack[#opStack+1]=r2.value
          opNext=false
          i,l=i2,l2

        else
          --by elimination, )
          while true do
            if #opStack==0 then
              done=true
              break--this could be a legitimate end of expression
            end
            local op=opStack[#opStack]
            table.remove(opStack,#opStack)

            if op=="(" then
              i,l=i2,l2
              break
            end
            if #outStack<2 then
              --not enough stuff on the outStack to apply this op!
              return nil
            end
            local new={type="binop",value=op,left=outStack[#outStack-1],right=outStack[#outStack],valType=opResTypes[op],index=index,lineNum=lineNum}
            table.remove(outStack,#outStack)
            outStack[#outStack]=new
          end
        end
      else
        --nothing, legit end possible
        done=true
      end
    else
      local r2,i2,l2 = parseOneOf({parseLiteral,parseUnop,parseFunctionCall,parseIdentifier,"%("},source,i,0)
      if r2 then
        --got one. what kind?
        if r2.value=="(" then
          opStack[#opStack+1]="("
          i,l=i2,l2
        elseif r2.type=="unop" then
          opStack[#opStack+1]=r2.value
          i,l=i2,l2
        else
          --anything else is some sort of value
          outStack[#outStack+1]=r2
          i,l=i2,l2
          opNext=true
        end
      else
        --no match, fail
        return nil
      end
    end
  end

  --ok, it got here, which suggests it got something...
  --pop and apply any dangling stuff
  while #opStack>0 do
    local op=opStack[#opStack]
    table.remove(opStack,#opStack)


    local new={type="binop",value=op,valType=opResTypes[op],index=index,lineNum=lineNum}
    if op:match("^u") then
      if #outStack<1 then
        --fail
        return nil
      end
      new.type="unop"
      new.right=outStack[#outStack]
    else
      if #outStack<2 then
        --fail
        return nil
      end

      new.left=outStack[#outStack-1]
      new.right=outStack[#outStack]
      table.remove(outStack,#outStack)
    end
    outStack[#outStack]=new
  end
  if #outStack~=1 then
    return nil
  end
  return outStack[1],i,l
end

--varDecl ::= <identifier> [":" <type> "]"
local function parseVarDecl(source,index,lineNum)
  local id,i,l
  id,i,l=parseIdentifier(source,index,lineNum)
  if not id then
    return nil
  end
  local res={type="varDecl",identifier=id.value,index=id.index,lineNum=lineNum}
  local _,i2,l2=parsePattern(":",source,i,l)
  if i2 then
    local t
    t,i,l=parseType(source,i2,l2)
    if t then
      res.valType=t.value
    else
      error({line=lineNum, index=index, "Expected type after \":\""})
    end
  end
  return res,i,l
end

local function parseList(valParser,separator,reqNum,source,index,lineNum)
  local v,i,l=nil,index,lineNum
  local vals={}
  print("parseList")
  while true do
    print(#vals)
    v,i,l=valParser(source,i,l)
    if not v then
      break
    end
    vals[#vals+1]=v
    print("separator")
    local _,i2,l2=parsePattern(separator,source,i,l)
    if not i2 then
      break
    end
    i,l=i2,l2
  end

  if #vals>=reqNum then
    return vals,i,l
  else
    return nil
  end
end

--varList ::= [ <varDecl> {"," <varDecl>} ]
local function parseVarList(source,index,lineNum)
  --[[
  local v,i,l=parseVarDecl(source,index,lineNum)
  if not v then
    return nil
  end
  local vars={v}

  while true do
    local _,i2,l2=parsePattern(",",source,i,l)
    if i2 then
      v,i,l=parseVarDecl(source,i2,l2)
      if not v then
        error({line=lineNum, index=index, "Expected identifier after \",\""})
      end
      vars[#vars+1]=v
    else
      break
    end
  end--]]
  local vars,i,l=parseList(parseVarDecl,",",1,source,index,lineNum)
  if vars then
    return {type="varList",index=index,lineNum=lineNum,values=vars},i,l
  end
end

--valList ::= <expression> { "," <expression> }
local function parseValList(source,index,lineNum)
  local vals,i,l={},index,lineNum
  while true do
    local v,i2,l2
    v,i,l=parseExpression(source,i,l)
    if v then
      vals[#vals+1]=v
    else
      return nil
    end
    v,i2,l2=parsePattern(",",source,i,l)
    if not v then
      break
    end
    i,l=i2,l2
  end
  return {type="valList",values=vals,index=index,lineNum=lineNum},i,l
end

--functionCall ::=
--  <identifier>([valList])
function parseFunctionCall(source,index,lineNum)
  local identifier,i,l=parseIdentifier(source,index,lineNum)
  if not identifier then
    return nil
  end
  _,i,l=parsePattern("%(",source,i,l)
  if not i then
    return nil
  end
  local valList,i2,l2=parseValList(source,i,l)
  if valList then
    i,l=i2,l2
  else
    valList={values={}}
  end
  _,i,l=parsePattern("%)",source,i,l)

  if i then
    return {type="functionCall",identifier=identifier.value,args=valList.values,index=index,lineNum=lineNum},i,l
  end
  return nil
end

function parseFunctionCallStat(source,index,lineNum)
  local m,i,l=parseFunctionCall(source,index,lineNum)
  if m then
    m.type="functionCallStat"
  end
  return m,i,l
end

--assignment ::=
--  [ "local" ] varList ['=' <valList> ]
local function parseAssignment(source,index,lineNum)
  local _,i,l=parsePattern("var",source,index,lineNum)
  local isLocal=false
  if i then
    isLocal=true
  else
    i,l=index,lineNum
  end
  local vars,vals
  vars,i,l=parseVarList(source,i,l)
  if not vars then
    return nil
  end
  local _,i2,l2=parsePattern("=",source,i,l)
  if i2 then
    vals,i,l=parseValList(source,i2,l2)
    if vals==nil then
      return nil
    end
  end
  return {type="assignment",vars=vars.values,vals=vals and vals.values or {},isLocal=isLocal,index=index,lineNum=lineNum},i,l
end

local parseStatement

--block ::= {<statement>}+
local function parseBlock(source,index,lineNum)
  local statements={}
  local i,l=index,lineNum
  while true do
    local res,i2,l2=parseStatement(source,i,l)
    if not res then
      break
    end
    i,l=i2,l2
    if #res>0 then
      tableJoin(statements,res)
    else
      statements[#statements+1]=res
    end
  end

  return {type="block",statements=statements, index=index,lineNum=lineNum,},i,l
end


--body ::= <statement> | '{' <block> '}'
local function parseBody(source,index,lineNum)
  --does it start with a {?
  local openBrace,i,l=parsePattern("{",source,index,lineNum)

  if openBrace then
    local res
    res,i,l=parseBlock(source,i,l)
    if not res then
      error({lineNum=lineNum, text="expected statement block after {"})
    end
    _,i,l=parsePattern("}",source,i,l)
    if not i then
      error({lineNum=lineNum,text="expected } to end block at "..lineNum})
    end
    return res,i,l
  end

  return parseStatement(source,index,lineNum)
end

--if ::= "if" "(" <expr> ")" <body> { "elseif" "(" <expr> ")" <body> } [ "else" <body> ]
local function parseIf(source,index,lineNum)
  local conditions,bodies={},{}
  local gotElse
  local temp,i,l=parsePattern("if",source,index,lineNum)
  if not temp then
    return nil
  end
  local stat="if"
  print("parsing if")

  while true do
    --ok, we got the if.
    temp,i,l=parsePattern("%(",source,i,l)
    if not temp then
      error({lineNum=lineNum, text="expected '(' after '"..stat.."'"})
    end
    local condition, body
    condition,i,l=parseExpression(source,i,l)
    if not condition then
      error({lineNum=lineNum,text="expected conditional expression in '"..stat.."' statement"})
    end
    conditions[#conditions+1]=condition

    print("got condition")

    temp,i,l=parsePattern("%)",source,i,l)
    if not temp then
      error({lineNum=lineNum,text="expected ')' after conditional expression in '"..stat.."' statement"})
    end

    body,i,l=parseBody(source,i,l)
    if not body then
      error({lineNum=lineNum,text="expected body following '"..stat.."'"})
    end
    bodies[#bodies+1]=body

    print("got body")

    local temp,i2,l2=parseOneOf({"elseif","else"},source,i,l)
    if temp==nil then
      print("no else or elseif, breaking")
      break
    end
    i,l=i2,l2
    stat=temp.value
    print("got "..stat)
    if stat=="else" then
      body,i,l=parseBody(source,i,l)
      if not body then
        error({lineNum=lineNum,text="expected body following else"})
      end
      print("got else body")
      bodies[#bodies+1]=body
      break
    end
  end


  return {type="if",conditions=conditions,bodies=bodies,lineNum=lineNum,index=index},i,l
end

--while ::= "while" "(" <expression> ")" <body>
local function parseWhile(source,index,lineNum)
  local res,i,l=nil,index,lineNum
  local condition,body

  local temp,i,l=parsePattern("while",source,i,l)
  if not temp then
    return nil
  end

  res,i,l=parsePattern("%(",source,i,l)
  if not temp then
    error({lineNum=lineNum, text="expected '(' after 'while'"})
  end

  condition,i,l=parseExpression(source,i,l)
  if not condition then
    error({lineNum=lineNum,text="expected ')' after conditional expression in 'while' statement"})
  end

  res,i,l=parsePattern("%)",source,i,l)
  if not temp then
    error({lineNum=lineNum, text="expected ')' after 'while' condition"})
  end


  body,i,l=parseBody(source,i,l)
  if not body then
    error({lineNum=lineNum,text="expected body following 'while'"})
  end

  return {type="while", condition=condition,body=body,lineNum=lineNum,index=index},i,l
end



--for::= "for" "(" <assignment> ";" <expression> ";" <expression> ") <body>
local function parseFor(source,index,lineNum)
  local res,i,l=nil,index,lineNum
  local init,condition,update,body

  local temp,i,l=parsePattern("for",source,i,l)
  if not temp then
    return nil
  end

  res,i,l=parsePattern("%(",source,i,l)
  if not res then
    error({lineNum=lineNum, text="expected '(' after 'for'"})
  end

  print("parseAssignment")
  init,i,l=parseAssignment(source,i,l)
  if not init then
    error({lineNum=lineNum,text="expected initializer in 'for' statement"})
  end
  print(";")
  res,i,l=parsePattern(";",source,i,l)
  if not res then
    error({lineNum=lineNum, text="expected ';' after 'for' initializer"})
  end

  condition,i,l=parseExpression(source,i,l)
  if not condition then
    error({lineNum=lineNum,text="expected condition in 'for' statement"})
  end

  res,i,l=parsePattern(";",source,i,l)
  if not res then
    error({lineNum=lineNum, text="expected ';' after 'for' initializer"})
  end

  update,i,l=parseAssignment(source,i,l)
  if not update then
    error({lineNum=lineNum,text="expected update assignment in 'for' statement"})
  end

  res,i,l=parsePattern("%)",source,i,l)
  if not res then
    error({lineNum=lineNum, text="expected ')' after 'for '"})
  end

  body,i,l=parseBody(source,i,l)
  if not body then
    error({lineNum=lineNum,text="expected body following 'for'"})
  end

  return {type="for", initializer=init, condition=condition, update=update,body=body,lineNum=lineNum,index=index},i,l
end


--functionDecl ::= "func" [ <type> { "," <type> } ]<identifier> "(" <varList>")" <body>
function parseFunctionDecl(source,index,lineNum)
  local res,i,l=nil,index,lineNum
  local returnTypes,varList,body

  local temp,i,l=parsePattern("func",source,i,l)
  if not temp then
    return nil
  end



end



--statement ::=
--  <functionCall> |
--  <if> |
--  <for> |
--  <while> |
--  <functionDecl> |
--  <assignment>
function parseStatement(source,index,lineNum)
  return parseOneOf({parseIf,parseWhile,parseFor,parseFunctionCallStat,parseAssignment},source,index,lineNum)

end

--[[*************************************
Actual compile functions!
turn syntax tree nodes into bytecode.
***************************************]]
local b64encode = { [0]="A", [1]="B", [2]="C", [3]="D", [4]="E", [5]="F", [6]="G", [7]="H", [8]="I", [9]="J", [10]="K", [11]="L", [12]="M", [13]="N", [14]="O", [15]="P", [16]="Q", [17]="R", [18]="S", [19]="T", [20]="U", [21]="V", [22]="W", [23]="X", [24]="Y", [25]="Z", [26]="a", [27]="b", [28]="c", [29]="d", [30]="e", [31]="f", [32]="g", [33]="h", [34]="i", [35]="j", [36]="k", [37]="l", [38]="m", [39]="n", [40]="o", [41]="p", [42]="q", [43]="r", [44]="s", [45]="t", [46]="u", [47]="v", [48]="w", [49]="x", [50]="y", [51]="z", [52]="0", [53]="1", [54]="2", [55]="3", [56]="4", [57]="5", [58]="6", [59]="7", [60]="8", [61]="9", [62]="-", [63]="_", }

local function numToB64(num)
  local str=""
  repeat
    local d=num%64
    num=(num-d)/64
    str=b64encode[d]..str
  until num==0
  return str
end


local function resolveVar(ident,context)
  print("resolveVar "..ident)
  local v=context.vars[ident]
  if v then
    return {stackOffs=v.stackOffs,valType=v.valType,}
  else
    v=context.parent and resolveVar(ident,context.parent)

    if v then
      v.stackOffs=v.stackOffs+context.stackUseVars[v.valType:sub(1,1)]
    end
    return v
  end
end


local function resolveFunc(ident,context)
  return context.funcs[ident] or (context.parent and resolveFunc(ident,context.parent))
end


local compile={}

function compile.number(node,context)
  --convert # to base 64
  local str=numToB64(node.value)
  local output={"#"..b64encode[#str]..str}
  return output
end

function compile.string(node,context)
  local v=node.value
  local len=#node.value
  if len>=64 then
    return false, {lineNum=node.lineNum, text="String too large, limit on string literals is 63"}
  end
  local output ={"'"..b64encode[len]..v}
  return output
end

function compile.boolean(node,context)
  local output={node.value and "1" or "0"}
  return output
end

function compile.literal(node,context)
  return compile[node.valType](node,context)
end

function compile.identifier(node,context)
  local ident=node.value

  local var=resolveVar(ident,context)
  if not var then
    return false,{lineNum=node.lineNum,text="Attempt to read undefined variable "..ident}
  end
  local stackOffs=var.stackOffs or "?"
  if stackOffs~="?" then
    stackOffs=numToB64(stackOffs+context.stackUse[var.valType:sub(1,1)])
  end
  return {"K"..(var.valType and var.valType:sub(1,1) or "?").."#"..b64encode[#stackOffs]..stackOffs}
end

function getValType(node,context)
  if node==nil then
    error("nil node",2)
  end
  if node.valType then
    return node.valType
  elseif node.type=="varDecl" then
    local var=resolveVar(node.identifier,context)
    if var then
      return var.valType
    end
  elseif node.type=="identifier" then
    local var=resolveVar(node.value,context)
    if var then
      return var.valType
    end
  elseif node.type=="functionCall" then
    local func=resolveFunc(node.identifier,context)
    if func then
      return func.valType
    end
  end
  return nil
end

function compile.binop(node,context)
  local output={}
  local left,right=node.left,node.right

  local argTypes=opArgTypes[node.value]
  local match=false
  local leftType,rightType=getValType(left,context),getValType(right,context)
  if leftType=="multi" then
    --wrap in something that extracts first from a multi-val
    local func=resolveFunc(left.identifier,context)
    if not func then
      return false,{lineNum=left.lineNum,text="Attempt to call undefined function "..left.identifier}
    end
    leftType=func.valTypes[1]
    left.valTypes=func.valTypes
    left={type="car",base=left,valType=leftType,index=left.index,lineNum=left.lineNum}
  end
  if rightType=="multi" then
    --wrap in something that extracts first from a multi-val
    local func=resolveFunc(right.identifier,context)
    if not func then
      return false,{lineNum=right.lineNum,text="Attempt to call undefined function "..right.identifier}
    end
    rightType=func.valTypes[1]
    right.valTypes=func.valTypes
    right={type="car",base=right,valType=rightType,index=right.index,lineNum=right.lineNum}

  end
  --]]
  if leftType==nil then
    return false,{lineNum=node.lineNum,text="operator "..node.value.." can't apply to function "..left.identifier..", which returns nothing"}
  elseif rightType==nil then
    return false,{lineNum=node.lineNum,text="operator "..node.value.." can't apply to function "..right.identifier..", which returns nothing"}
  elseif argTypes==nil then
    --just make sure they match
    match=leftType==rightType
  elseif type(argTypes)=="table" then

    for i=1,#argTypes do
      if argTypes[i]==leftType then
        --still have to match each other
        match=leftType==rightType
        break
      end
    end
  elseif type(argTypes)=="string" then
    match=argTypes==rightType and argTypes==leftType
  end

  if not match then
    return false,{lineNum=node.lineNum, text="types mismatch: attempt to apply operator "..node.value.." to "..(leftType or "nil").." and "..(rightType or "nil")}
  end
  local typeCh=leftType:sub(1,1)

  local res,err=compile[left.type](left,context)
  if not res then
    return res,err
  end
  output=res
  output[#output+1]="H"..typeCh
  context.stackUse[typeCh]=context.stackUse[typeCh]+1
  local res,err=compile[right.type](right,context)
  if not res then
    return res,err
  end
  tableJoin(output,res)
  context.stackUse[typeCh]=context.stackUse[typeCh]-1
  local comp=opOutput[node.value]
  if type(comp)=="function" then
    tableJoin(output,comp(leftType))
  elseif comp then
    tableJoin(output,comp)
  else
    output[#output+1]=node.value
  end
  return output
end

function compile.unop(node,context)
  local right=node.right
  local rightType=getValType(right,context)
  if rightType==nil then
    return false,{lineNum=node.lineNum,text="operator "..node.value.." can't apply to function "..right.identifier..", which returns nothing"}
  elseif rightType=="multi" then
    --wrap in something that extracts first from a multi-val
    local func=resolveFunc(right.identifier,context)
    if not func then
      return false,{lineNum=right.lineNum,text="Attempt to call undefined function "..right.identifier}
    end
    rightType=func.valTypes[1]
    right.valTypes=func.valTypes
    right={type="car",base=right,valType=rightType,index=right.index,lineNum=right.lineNum}
  end
  local res,err=compile[node.right.type](node.right,context)
  if not res then
    return res,err
  end

  local comp=opOutput[node.value]
  if type(comp)=="function" then
    tableJoin(res,comp(leftType))
  elseif comp then
    tableJoin(res,comp)
  else
    res[#res+1]=node.value
  end
  return res
end

--car is an internal type used when an expression returning a list of values rather
--than a single value - currently only a functionCall - has it's result subjected
--to an operator, which requires discarding all but the first returned result.
function compile.car(node,content)
  local base=node.base
  local output={}

  local burnCounts={n=0,b=0,s=0}
  local firstTypeCh=base.valTypes[1]:sub(1,1)

  for i=2,#base.valTypes do
    local type=base.valTypes[i]
    local typeCh=type:sub(1,1)
    burnCounts[typeCh]=burnCounts[typeCh]+1
  end

  local res,err=compile[base.type](base,content)

  if not res then
    return res,err
  end

  output=res

  --now add burns for all with stack values returned
  for k,v in pairs(burnCounts) do
    if v>1 then
      output[#output+1]="B"..k.."#B"..numToB64(v)
    end
  end
  return output
end

local function compileExpressionListTrueTypes(list,context)
  local valTypes={}
  for i=1,#list do
    local arg=list[i]
    local argType=getValType(arg,context)
    if argType=="multi" then
      --expand!
      local func=resolveFunc(arg.identifier,context)
      for i=1,#func.valTypes do
        valTypes[#valTypes+1]=func.valTypes[i]
      end
    else
      valTypes[#valTypes+1]=argType
    end
  end
  return valTypes
end

function compileExpressionList(list,context)
  --expand the args
  local valTypes=compileExpressionListTrueTypes(list,context)

  local output={}

  local pushCounts={n=0,b=0,s=0}
  for i=1,#list do
    local argVal=list[i]
    local typeCh=getValType(argVal,context)
    if typeCh=="multi" then
      tableJoin(output,compile[argVal.type](argVal,context))
      local func=resolveFunc(argVal.identifier,context)
      for k,v in pairs(func.returnCounts) do
        if v>0 then
          output[#output+1]="H"..k
          context.stackUse[k]=context.stackUse[k]+1
          pushCounts[k]=pushCounts[k]+1
        end
      end
    else
      typeCh=typeCh:sub(1,1)
      tableJoin(output,compile[argVal.type](argVal,context))
      output[#output+1]="H"..typeCh
      context.stackUse[typeCh]=context.stackUse[typeCh]+1
      pushCounts[typeCh]=pushCounts[typeCh]+1
    end
  end

  return output, valTypes, pushCounts
end

local systemFunctions = {
    print={
      valType=nil,
      valTypes={},
      returnCounts={n=0,b=0,s=0},
    },
    move={
      valType="multi",
      valTypes={"boolean","number","string"},
      returnCounts={n=1,b=1,s=1},
    },
  }

local validSides={f=true,b=true,u=true,d=true,l=true,r=true}

function compileSideArg(arg)
  --parse arg as a side - this can only be decided at compile time, as the direction
  --is can only be a literal side character in the vm instruction set
  --so must be a literal string.
  if arg.type~="literal" or arg.valType~="string" then
    return false,{lineNum=arg.lineNum,text="argument #1 (side) to function move can only be a string literal."}
  end
  local side=arg.value:sub(1,1):lower()
  if validSides[side]==nil then
    return false,{lineNum=arg.lineNum,text="Invalid side specified in call to function 'move'"}
  end
  return side
end

function systemFunctions.print.f(args, context)
  local output,valTypes,pushCounts=compileExpressionList(args,context)
  if not output then
    return output,valTypes
  end

  for i=1,#valTypes do
    local typeCh=valTypes[i]:sub(1,1)
    output[#output+1]="O"..typeCh
    output[#output+1]="."..typeCh
    context.stackUse[typeCh]=context.stackUse[typeCh]-1
  end
  return output
end


function systemFunctions.move.f(args,context)
  local side,dist="f","0xBB"

  if #args>2 then
    return false,{lineNum=args.lineNum,text="Too many arguments in call to move"}
  end
  local output={}

  side=compileSideArg(args[1])

  if #args>=2 then
    --here we can take anything. If it's a literal number, just write the instruction with it embedded.
    local arg=args[2]
    local valType=getValType(arg,context)
    if valType~="number" then
      return false,{lineNum=arg.lineNum,text="type mismatch in argument #2 to function 'move': expected number, got "..valType}
    end
    if arg.type=="literal" then
      dist=numToB64(arg.value)
      dist="#"..numToB64(#dist)..dist
    else
      --otherwise, compile the arg!
      local exp,err=compile[arg.type](arg,context)
      if not exp then
        return exp,err
      end
      output=exp
      dist="n"
    end
  end

  output[#output+1]="M"..side..dist
  return output
end



function compile.functionCall(node,context)
  local ident=node.identifier
  --lookup ident address in context
  local func=resolveFunc(ident,context)
  if func==nil then
    --try specials
    func=systemFunctions[ident]
    if not func then
      return false,{lineNum=node.lineNum,text="Attempt to call undefined function "..ident}
    end
    return systemFunctions[ident].f(node.args,context)
  end

  local output, trueArgs, pushCounts=compileExpressionList(node.args,context)

  --verify arg count
  if #trueArgs<func.minArgs then
    return false,{lineNum=node.lineNum,text="insufficient arguments in call to function "..ident}
  elseif #trueArgs>#func.args then
    return false,{lineNum=node.lineNum,text="Too many arguments in call to function "..ident}
  end

  for i=1,#trueArgs do
    if func.args[i].valType~=trueArgs[i] then
      return false,{lineNum=node.lineNum,text="type mismatch on arg #"..i.." in call to function "..ident.."; expected "..func.args[i].valType..", got "..trueArgs[i]}
    end
  end

  --ok, args check out!

  for i=#trueArgs+1,#func.args do
    local def=func.args[i].default
    local typeCh=def.valType:sub(1,1)
    tableJoin(output,compile[def.valType](def,context))
    output[#output+1]="H"..typeCh
    pushCounts[typeCh]=pushCounts[typeCh]+1
  end

  local addr=numToB64(func.addr)


  for k,v in pairs(pushCounts) do
    context.stackUse[k]=context.stackUse[k]-v+math.max(0,func.returnCounts[k]-1)
  end

  output[#output+1]="L#"..numToB64(#addr)..addr
  return output
end

function compile.functionCallStat(node,context)
  local stackUse={n=context.stackUse.n,s=context.stackUse.s,b=context.stackUse.b}

  local res,err=compile.functionCall(node,context)
  if not res then
    return res,err
  end

  for k,v in pairs(stackUse) do
    local dif=context.stackUse[k]-v
    if dif>0 then
      local count=numToB64(dif)
      res[#res+1]="B"..k.."#"..numToB64(#count)..count
      context.stackUse[k]=v
    end
  end
  return res
end


function compileAssignmentHoist(node,context)
  --ok. We have a varlist, which is variable declarations,
  --possibly with a "local" prefix, which simplifies things
  --without local, for each variable must resolve in context and
  --create in this context only if it doesn't exist.
  --we can push defaults to create the new variables on the stack.
  local trueTypes=compileExpressionListTrueTypes(node.vals,context)
  local stackUseVars=context.stackUseVars
  print("compileAssignmentHoist")

  for i=1,#node.vars do
    print("var "..i)
    local typeCh, offset
    local varDef=node.vars[i]
    local var
    local ident=varDef.identifier
    local varType=varDef.valType
    if not varType then
      --compile the expression list only if needed, for EFFICIENCY!
      varType=trueTypes[i]
    elseif varType~=trueTypes[i] then
      return false,{lineNum=node.lineNum, text="Type mismatch: attempted to assign value of type "..trueTypes[i].." to variable "..varDef.identifier.." of type "..varType}
    end
    if not varType then
      --aha!
      return false,{lineNum=node.lineNum, text="Unable to infer implicit type on variable "..varDef.identifier}
    end

    if not node.isLocal then
      --try to find it
      var=resolveVar(ident,varType,context)
    end
    if not var then
      print("making locally")
      --make it locally
      typeCh=varType:sub(1,1)
      stackUseVars[typeCh]=stackUseVars[typeCh]+1
      print("stackUseVars["..typeCh.."]="..stackUseVars[typeCh])
      var={stackOffs=stackUseVars[typeCh],valType=varType,}
      --add to context
      context.vars[ident]=var
    end
  end
  return true

end


function compile.assignment(node,context)
  --ok. We have a varlist, which is variable names. They've been setup by
  --assignmentHoist before we get here, so assume they exist and just resolveVar them.

  --vallist, which is identical to the arg list for a functionCall
  --which can use compileExpressionList
  local output,valTypes,pushCounts=compileExpressionList(node.vals,context)

  --for each in varList, pop and push next value of that type,
  --checking as we do that the types match.
  for i=1,math.min(#node.vars,#valTypes) do
    local varDef=node.vars[i]
    local typeCh=getValType(varDef,context)
    if typeCh==nil then
      return false,{lineNum=node.lineNum,text="Can't declare new variables in this context"}
    end
    typeCh=typeCh:sub(1,1)
    --next of each type will be on stack, just pop it and poke
    local offset=resolveVar(varDef.identifier,context).stackOffs
    context.stackUse[typeCh]=context.stackUse[typeCh]-1
    offset=numToB64(offset+context.stackUse[typeCh])
    output[#output+1]="O"..typeCh
    output[#output+1]="A"..typeCh.."#"..numToB64(#offset)..offset
    pushCounts[typeCh]=pushCounts[typeCh]-1
  end

  --burn the remainders
  if #node.vars<#valTypes then
    for k,v in pairs(pushCounts) do
      if v>0 then
        local count=numToB64(v)
        output[#output+1]="B"..k.."#"..numToB64(#count)..count
        context.stackUse[typeCh]=context.stackUse[typeCh]-v
      end
    end
  end

  return output
end

compile["if"]=function (node,context)
  --this is where jumps and references come into play.
  --give contexts "labels" each having an index to the instruction
  --and an array refs of the indexes of all instructions that go there
  local index=context.index
  local output={}

  local elseRefs=#node.bodies>#node.conditions and {} or nil

  for i=1,#node.conditions do
    local condition=node.conditions[i]
    if getValType(condition)~="boolean" then
      return false,{lineNum=node.lineNum,text="Type mismatch: if conditions must evaluate to type 'boolean'"}
    end

    local cond,err=compile[condition.type](condition,context)
    if not cond then
      return cond,err
    end
    tableJoin(output,cond)

    local body,err=compile[node.bodies[i].type](node.bodies[i],context)
    if not body then
      return body,err
    end

    --ok. after body gets a label...
    local jumpIndex=index + #cond + 1
    index=jumpIndex + 1 + #body

    output[#output+1]="!"
    output[#output+1]="I#B?" --jumpIndex
    tableJoin(output,body)
    if elseRefs then
      output[#output+1]="J#B?"
      elseRefs[i]=index
      index=index+1
    end

    local afterLabel=#context.labels+1
    context.labels[afterLabel]={index=index, refs={jumpIndex}}

  end

  if elseRefs then
    local body,err=compile[node.bodies[#node.bodies].type](node.bodies[#node.bodies],context)
    if not body then
      return body,err
    end
    tableJoin(output,body)
    index=index+#body
    context.labels[#context.labels+1]={index=index,refs=elseRefs}
  end

  return output
end

compile["while"]=function (node,context)
  local index=context.index
  local output={}

  local condition=node.condition
  if getValType(condition)~="boolean" then
    return false,{lineNum=node.lineNum,text="Type mismatch: while conditions must evaluate to type 'boolean'"}
  end

  local cond,err=compile[condition.type](condition,context)
  if not cond then
    return cond,err
  end
  output=cond

  local body,err=compile[node.body.type](node.body,context)
  if not body then
    return body,err
  end

  --ok. top of body gets a label...
  local jumpIndex=index + #cond + 1
  local startIndex=index
  index=jumpIndex + 1 + #body

  output[#output+1]="!"
  output[#output+1]="I#B?" --jumpIndex
  tableJoin(output,body)

  output[#output+1]="J#B?" --loop
  index=index+1
  local beforeLabel=#context.labels+1
  local afterLabel=#context.labels+2

  context.labels[beforeLabel]={index=startIndex, refs={index-1}}
  context.labels[afterLabel]={index=index, refs={jumpIndex}}

  return output
end

local function beginContext(context)
  return {
      index=context and context.index or 1,
      stackUse={n=0,b=0,s=0},
      stackUseVars={n=0,b=0,s=0},
      labels=context and context.labels or {},
      vars={},
      funcs={},
      parent=context,
    }
end

local function endContext(context)

  local output={}
  for k,v in pairs(context.stackUseVars) do
    if v>0 then
      local count=numToB64(v)
      output[#output+1]="B"..k.."#"..numToB64(#count)..count
      context.index=context.index+1
    end
  end

  if context.parent then
    context.parent.index=context.index
  end


  return output
end

local defaultValues={
  n="#A",
  s="'A",
  b="0",
}


compile["for"]=function (node,context)
  local index=context.index
  local output={}

  local myContext=beginContext(context)
  local init=node.init

  local init=node.initializer
  local res,err=compileAssignmentHoist(init,myContext)
  if not res then
    return res,err
  end

  --prepare stack for hoisted variables
  for k,v in pairs(myContext.stackUseVars) do
    if v>0 then
      output[#output+1]=defaultValues[k]
      for i=1,v do
        output[#output+1]="H"..k
      end
    end
  end
  myContext.index=myContext.index+#output


  res,err=compile.assignment(init,myContext)
  if not res then
    return res,err
  end
  myContext.index=myContext.index+#res

  tableJoin(output,res)

  local conditionLabel={index=index+#output, refs={}}
  context.labels[#context.labels+1]=conditionLabel

  if getValType(node.condition)~="boolean" then
    return false,{lineNum=node.lineNum,text="Type mismatch: for conditions must evaluate to type 'boolean'"}
  end

  res,err=compile[node.condition.type](node.condition,myContext)
  if not res then
    return res,err
  end

  tableJoin(output,res)
  myContext.index=myContext.index+#res

  output[#output+1]="!"
  local endLabel={refs={index+#output}}
  context.labels[#context.labels+1]=endLabel
  output[#output+1]="I#B?"


  res,err=compile[node.body.type](node.body,myContext)
  if not res then
    return res,err
  end

  tableJoin(output,res)
  myContext.index=myContext.index+#res

  res,err=compile.assignment(node.update,myContext)
  if not res then
    return res,err
  end
  tableJoin(output,res)
  --add ref to end
  myContext.index=myContext.index+#res

  conditionLabel.refs[1]=index+#output
  output[#output+1]="J#B?"

  endLabel.index=index+#output

  myContext.index=myContext.index+2

  local res=endContext(myContext)
  tableJoin(output,res)


  return output
end




function compile.block(node,context)
  --block compiling!
  --create our own context!
  local myContext=beginContext(context)

  --first: identify assignment statements and hoist
  for i=1,#node.statements do
    local stat=node.statements[i]
    if stat.type=="assignment" then
      local res,err=compileAssignmentHoist(stat,myContext)
      if not res then
        return res,err
      end
    end
  end

  local output={}

  --prepare stack for hoisted variables
  for k,v in pairs(myContext.stackUseVars) do
    if v>0 then
      print("got "..v.." of type "..k)
      output[#output+1]=defaultValues[k]
      for i=1,v do
        output[#output+1]="H"..k
      end
    end
  end
  myContext.index=myContext.index+#output

  --now compile each statement
  for i=1,#node.statements do
    local stat=node.statements[i]
    local res,err=compile[stat.type](stat,myContext)
    if not res then
      return res,err
    end
    myContext.index=myContext.index+#res
    tableJoin(output,res)
  end

  tableJoin(output,endContext(myContext))

  return output,myContext
end

--[[*************************************
Linking and optimization functions!
linking resolves addresses in instruction arrays output by compiler.
optimization simply streamlines the instruction array
***************************************]]
local function link(code,context)
  print("linking")
  local index=1
  local address=1
  --sort the labels, should be already but for goodness

  table.sort(context.labels,function(l1,l2) return l1.index<l2.index end)
  local changedLengths,changedAddr
  local passes=1
  repeat
    address,index=1,1
    changedLengths,changedAddr=false,false
    for i=1,#context.labels do
      local label=context.labels[i]
      for j=index, label.index-1 do
        address=address+#code[j]
      end
      index=label.index
      local prevAddr=label.address or "#B?"
      local addr64=numToB64(address)
      addr64="#"..numToB64(#addr64)..addr64
      label.address=addr64
      if addr64~=prevAddr then
        changedAddr=true
        if #addr64~=#prevAddr then
          --changed lengths
          changedLengths=true
        end
        --now insert new addresses
        for j=1,#label.refs do
          local a=label.refs[j]
          code[a]=code[a]:gsub("#[%w%-_%?]+",addr64)
        end
      end
      if changedLengths then
        break
      end
    end
    passes=passes+1
  until not changedAddr

  return table.concat(code,"")
end

local function optimize(code,context)
  print("optimizing")
  local i=1
  while i<#code do
    local type=code[i]:match("^O(.)")
    if type and i>1 then
      --it's a pop instruction
      if code[i-1]:match("^H"..type) then
        --can remove both!
        table.remove(code,i)
        table.remove(code,i-1)
        for j=1,#context.labels do
          local l=context.labels[j]
          local n=l.index
          if n==i then
            l.index=i-1
          elseif n>i then
            l.index=n-2
          end
          for k=1,#l.refs do
            local r=l.refs[k]
            if r==i then
              l.refs[k]=i-1
            elseif r>i then
              l.refs[k]=r-2
            end
          end
        end
        i=i-2
      end
    end
    i=i+1
  end
  return code,context
end




local hist={}
local doOptimize=true
local running=true
local output
local modem=component.modem

local commands={
  slave=function()
    print("run rli on the robot to slave and enter\n>/slave "..modem.address:sub(1,4))
    while true do
      local e={event.pull()}
      if e[1]=="key_down" and e[3]==3 then
        print("ctrl+c, cancelled")
        break
      elseif e[1]=="modem_message" and e[6]=="slave ready" then
        print("slaved robot with card starting with "..e[3]:sub(1,4).."\nSend the last compiled program by typing /send")
        robot=e[3]
        break
      end
    end
  end,

  send=function()
    if not robot then
      print("no slaved robots to send the program to!")
      return
    end
    if not output then
      print("no last successful program to send!")
      return
    end
    print("sending to slave.")
    modem.send(robot,4201,output)
    print("Waiting for result...")
    while true do
      local e={event.pull("modem_message")}
      if e[3]==robot then
        print("result:"..e[6])
        break
      end
    end
  end,

  exit=function()
    running=false
  end,

}

while running  do
  term.write(">")
  local input=term.read(hist):gsub("\n","")
  if input=="" then
    break
  end

  if input:match("^/") then
    --command
    local cmd,args=input:match("^/(%w+)%s*(.*)$")
    local f=commands[cmd]
    if f then
      local argt={}
      args:gsub("(%S+)",function(v) argt[#argt+1]=v end)
      f(table.unpack(argt))
    else
      print("unknown command, /help for help")
    end
  else

    local res={pcall(parseBlock,input,1,1)}
    --[[
    local file=io.open("/flop/tree","w")
    file:write(text.serialize(res))
    file:close()
    --]]
    print(text.serialize(res))

    if res[1] then
      local node=res[2]
      local res,context=compile.block(node)
      if res then
        if doOptimize then
          output=link(optimize(res,context))
        else
          output=link(res,context)
        end
        print("output:\n"..output)
        --[[
        local file=io.open("/flop/out","w")
        file:write(output)
        file:close()
        --]]
      else
        print(text.serialize(err))
        print("error on line "..err.lineNum..":"..err.text)
      end
    end
  end
end

print=oldprint


--[=[
start:
'AHs'NHello, World!HsOsAs#BBKs#BBHsOs.s
remove HsOs
[['AHs'NHello, World!As#BBKs#BB.s]]
push <type>,literal<type>,poke<type>[1] -> push<literal>
[['AH'NHello, World!Ks#BB.s]]
literal reg set - scan forward, if find something changing it before something using it, remove
[['NHello, World!Ks#BB.s]]


--]=]