COMP304 Assignment 3 (2016), Victoria University of Wellington



ASSIGNMENT DESCRIPTION
--------------------------------------------------------------------------------
Compiler and interpreter for simple straight line programs.

A straight line program is just a list of assignment statements, where an
expression can contain variables, integer constants and arithmetic operators.



POST-SUBMISSION NOTES
--------------------------------------------------------------------------------
A perfect mark was received for this assignment.
The code and comments were slightly cleaned up after submission, but nothing was
actually changed functionality-wise.



IMPLEMENTATION COMPLETION DETAILS
--------------------------------------------------------------------------------
PART 1: COMPLETE
If and While statements are fully working.
If statements take a conditional expression and two lists of statements. When
translated, they work by utilizing JumpF (Jump if False) and Jump statements.
While statements work in the same way. Full details are below, where trans' is
defined. The program uses a "Pc" (Program Counter) Int to keep track of the
current position being executed in the code.
By using relative address jump statements, heavily nested conditionals and
complex blocks of statements can be used without any problems, since the jump
distance is always the same with the currently implemented language features.
Procedure calls within conditionals are also possible without any issues, so I
believe nothing can be improved.

PART 2: COMPLETE
Boolean variables and comparisons exist. When a program is run, it is ensured
that variables aren't declared twice, and that all used variables are declared.
Variables must also be declared with a type. Further details are at the relevant
code. Expressions are also checked to ensure that types are correct.
Declaring all the variables with their types in a separate section is similar to
languages like COBOL. It promotes robustness and makes it a lot simpler to plan
out how the program will function, so I think this is a good solution. Having
preset types makes it less likely for errors or bugs to crop up, which is great
for how simple the language is. However, more type checking in expressions and
such needs to be done.

PART 3: COMPLETE
Procedures are working. They are defined alongside the variable declarations and
main code of a program. They start with a "Label" (name) and must have a Return
statement at the end. Procedures are inserted into the top of the translated
code. This means that a program with procedures will have a non-0 initial Pc
value. Code can jump to a procedure with the Call statement, which takes a
Label. The current Pc is then inserted into the stack so that the Return
statement can take execution back to where it left off when the procedure is
finished. Further details are at the relevant code.
I think this works great, as procedures and their calls stay "out of the way".
Procedure code doesn't affect any relative jumps. Absolute jumps are only used
to go to a procedure and return to where it was called.

PART 4: COMPLETE
Parameters and local variables (both for procedures) work.
The stack isn't used to move variables over through parameters. Instead,
procedures have a "Parameter" statement available. What this does is take a
variable name, and when the Parameter statement is called, it finds this
variable in the store and moves it to the "local stack". Now, manipulating the
variable of this name in the procedure manipulates the local version.
Storing a variable changes the local version if it exists, otherwise it changes
a global version (to allow getting some kind of result from the procedure call).
If additional local variables are required, they can be defined in the local
store which is the second element in the tuple that defines a procedure. Since
these variables will exist from the start, manipulations will affect them
instead of global ones.
This could be implemented better. It'd be easy and cleaner to make everything
only affect local variables, but this isn't an option unless I implement
returning values. But it is at least easy to implement this (everything
affecting only local variables) by changing a single line (how exec' handles
the Store(s)).
A stack of Stores is used so that each procedure has its own local Store. The
Goto command is only used to go to a procedure, so it adds a new store onto the
stack. The ReturnTo command does the opposite, removing the top of the stack. I
think this works really well. You can find anything in scope by looking through
the stack of Stores (easily done with concat), and every function call gets its
own unique Store with parameters copied in and access to every scope above it,
with closer scopes having higher priority for when variables existing in
multiple scopes are manipulated.

EXAMPLE PROGRAMS:
With this design alone, problems such as computing the Ackermann Function are
solved quite easily. For examples of programs written in the language using the
variety of different features implemented, after loading program into ghci, run
any of these commands:

    COMMAND | DESCRIPTION                         | DEFAULT INPUT
    -----------------------------------------------------------------------
    runP5   | Finds the nth Fibonacci number      | 30
    runP6   | Finds the number of primes in range | 100 (inclusive) -> 200
    runP8   | Finds an Ackermann value            | A(3,5)
    runP9   | Collatz Conjecture                  | 2394
    runP10  | Greatest Common Divisor             | GCD(295951656,15383424)





THE CODE
--------------------------------------------------------------------------------
 * Comments on what has been added to the base code begin with ~~~
 * Comments on the WHY of my implementation are surrounded with --- walls.
 * All other comments are there to describe how the following code works.

-------------------------------------------------------------------------------- DATA DEFINITIONS
~~~ Prog has been modified to take a list of variables with their types, a list
    of procedures (which are a lists of statements + a Store themselves), and
    then the actual program code. Seperating everything this way makes it a lot
    easier to think about.

> data Prog = Prog ([(Type, Var)], [Proc], [Stmt])
>             deriving (Show)


~~~ If and While statements are obvious. Call calls a procedure. Label is simply
    used at the beginning of a procedure to name it. Return returns from a
    procedure.

> data Stmt = Asgn Var Exp
>           | If Exp [Stmt] [Stmt]
>           | While Exp [Stmt]
>           | Parameter Var
>           | Call Label
>           | Label Label
>           | Return
>           | DoSkip
>             deriving (Show)


> data Exp = Const Val
>          | Var Char
>          | Bin Op Exp Exp
>            deriving (Show)


~~~ Added Eq, Ne, etc. (for Ints), as well as And, Or, and Not (for Bools).

> data Op = Plus | Minus | Times | Div | Mod
>         | Eq | Ne | Gt | Ge | Lt | Le
>         | And | Or | Not
>           deriving (Show)


~~~ Type is used to define the type of a variable.

> data Type = IntT | BoolT
>             deriving (Show, Eq)


The store is a list of variable names and their values. A variable name is just
a single character.

> type Store = [(Var, Val)]
> type Var   = Char
> data Val   = Int Int | Bool Bool -- ~~~ Variables can be either Ints or Bools.
>              deriving (Show, Eq)


> type Proc  = ([Stmt], Store) -- ~~~ Type for defining a procedure.
> type Label = [Char]          -- ~~~ Used for procedure names.

> type Dist = Int -- ~~~ Basically a relative address for jumps, better thought
>                 --     of as the distance forward or backwards (when negative)
>                 --     to jump.

> type Pc = Int -- ~~~ The current position being executed in the code.


Straight line programs are translated into code for a simple stack-oriented
virtual machine.

A VM code program is a list of commands, where a command is either a load
immediate (which loads a constant onto the stack), a load (which loads the value
of a variable onto the stack), a sstore (which saves the value at the top of the
stack into memory, and deletes it), or an arithmetic operation (which applies
an operation to the two values at the top of the stack and replaces them with
the result).

> type Code = [Command]


~~~ The new commands here are jumps for jumping forwards or backwards a relative
    distance (used for If/While), Skip which is used to make sure jumps don't go
    past the end of the program, Goto which is a jump to an absolute address
    (an exact index of the list of commands that define the translated program),
    ProdLabel which is used for finding the absolute address of a procedure, and
    ReturnTo which returns from a procedure.
    Having both relative and absolute jumps makes it a lot easier to do
    different operations like if statements or procedure calls in sensible ways.

> data Command = LoadI Val | Load Var | Store Var | BinOp Op
>              | Jump Dist | JumpF Dist | Skip
>              | Goto Pc  | ProdLabel Label | ReturnTo
>              | TEMPGOTO Label | MoveToLocal Var
>                deriving (Show)


> type Stack = [Val]



-------------------------------------------------------------------------------- GENERAL FUNCTIONS
~~~ These are used to get certain elements of a 3-tuple or 4-tuple.

> mySnd3 :: (a, b, c) -> b
> mySnd3 (_, x, _) = x
> myTrd3 :: (a, b, c) -> c
> myTrd3 (_, _, x) = x

> mySnd4 :: (a, b, c, d) -> b
> mySnd4 (_, x, _, _) = x
> myTrd4 :: (a, b, c, d) -> c
> myTrd4 (_, _, x, _) = x



-------------------------------------------------------------------------------- PROGRAM COMPILATION
~~~ Run the program. First the code is compiled, returning the translated code
    and the Pc that the program starts at, since procedures exist at the top of
    the code. The program is also checked to make sure that everything is valid,
    so an error displays if this isn't the case. If this all passes, then the
    result is returned in the form of a Store.

> run :: Prog -> Store
> run p = if checkProg p then mySnd4 (exec code ([], [], startPc, []))
>             else error ("Not all variables used were declared.")
>      	  where (code, startPc) = translate p


Translate a straight line program into stack machine code.
--------------------------------------------------------------------------------
~~~ Here we first translate the Procedure code, then the actual program code is
    stuck onto the end. The length of the Prodecure code becomes the start pos.
    Procedures go at the start because we need the absolute address of each of
    them, because then the calls to those procedures need to be converted to
    absolute jumps. So "getProdPositions" is called on the generated Procedure
    code to store this information, which is then passed to "trans" when
    translating the actual program code. The way that this is implemented means
    that procedures cannot have calls to other procedures, unfortunately.

    Translating all the procedure calls to jumps here makes it a lot easier to
    interpret the translated program. The weakness is less versatility.
    Procedures could call other procedures by going through the procedure code
    a second time after finding out the absolute position of each.

    UPDATE:
    Procedures can now call themselves and each other. This is achieved by
    taking the translated procedure code and going through it a second time,
    replacing the untouched procedure calls with the absolute positions that are
    now known from the first time going through the code.
    The TEMPGOTO label exists so that the label can be preserved, so that in the
    second run, it'll know where the calls want to go.

> translate :: Prog -> (Code, Pc)
> translate (Prog stmts)
>     = ((fix prods poss) ++ trans (myTrd3 stmts) poss, length prods)
>       where prods = trans (concat (map fst (mySnd3 stmts))) []
>             poss  = getProdPositions prods 0

> fix :: Code -> [(Label, Pc)] -> Code
> fix []                _    = []
> fix ((TEMPGOTO l):xs) poss = (Goto (findPos l poss)):(fix xs poss)
> fix (c:xs)            poss = c:(fix xs poss)

--------------------------------------------------------------------------------


~~~ trans now takes this list of procedure labels and their absolute positions.

> trans :: [Stmt] -> [(Label, Pc)] -> Code
> trans []           _  = []
> trans (stmt:stmts) pP = (trans' stmt pP) ++ (trans stmts pP)


--------------------------------------------------------------------------------
~~~ Here, If statements are converted by translating the conditional expression,
    then following it with a JumpF (Jump if False) command. This command jumps
    the length of the "then" part of the If statement plus 1, so it goes to the
    start of the "else" part. The "then" part ends with a jump past the "else"
    part. A Skip command is placed at the end so that this jump doesn't jump
    past the end of the program, which would cause an error.

    While statements work in a similar way, except that when jumping back to the
    start, the size of the conditional statement has to be taken into account,
    since it could just be a == b, or it could be heavily nested and consist of
    many commands. A Jump command can take negative Ints, naturally.

    A Call to a procedure is simply replaced with a Goto - a jump to an absolute
    position. This is easily done by finding the procedure's absolute address in
    the provided list.

> trans' :: Stmt -> [(Label, Pc)] -> Code
> trans' (Asgn var exp) _ = (transExp exp) ++ [Store var]

> trans' (If exp s1 s2) a = (transExp exp) ++ [JumpF ((length tS1) + 1)] ++ tS1
>                               ++ [Jump (length tS2)] ++ tS2 ++ [Skip]
>                           where tS1 = trans s1 a
>                                 tS2 = trans s2 a

> trans' (While exp s) a = tE ++ [JumpF ((length tS) + 1)] ++ tS
>                             ++ [Jump (-((length tS) + 2 + (length tE)))]
>                             ++ [Skip]
>                          where tS = trans s a
>                                tE = (transExp exp)

> trans' (Call l) pP = if pos == -1 then [TEMPGOTO l] else [Goto pos]
>                      where pos = findPos l pP

> trans' (Label l)     pP = [ProdLabel l]
> trans' (Return)      pP = [ReturnTo]
> trans' (DoSkip)      pP = [Skip]
> trans' (Parameter v) pP = [MoveToLocal v]

--------------------------------------------------------------------------------


> transExp :: Exp -> Code
> transExp (Const n)      = [LoadI n]
> transExp (Var v)        = [Load v]
> transExp (Bin op e1 e2) = transExp e1 ++ transExp e2 ++ [BinOp op]


~~~ getProdPositions looks through the provided translated procedure code and
    gets an absolute address for each one. Since they're all at the start of the
    code, the absolute addresses found here are easy to jump to.

> getProdPositions :: Code -> Int -> [(Label, Pc)]
> getProdPositions []                 _  = []
> getProdPositions ((ProdLabel x):xs) pc = (x, pc):(getProdPositions xs (pc+1))
> getProdPositions (_:xs)             pc = getProdPositions xs (pc+1)


~~~ findPos takes a procedure label and returns its absolute address.

> findPos :: Label -> [(Label, Pc)] -> Pc
> findPos _ []          = -1
> findPos l ((x, p):xs) | l == x    = p
>                       | otherwise = findPos l xs



-------------------------------------------------------------------------------- COMPILED PROGRAM EXECUTION
~~~ exec is changed so that instead of being recursive, it uses a program
    counter, so that it's easy to jump to different parts of the program. If
    the program counter is greater than the size of the program, it exits by
    returning the store. Otherwise it gets the current command to execute using
    the program counter.

> exec :: Code -> (Stack, Store, Pc, [Store]) -> (Stack, Store, Pc, [Store])
> exec []   ss = ss
> exec cmds ss | (myTrd4 ss) == ((length cmds)) = ss
>              | otherwise = exec cmds (exec' curCmd ss)
>                            where curCmd = cmds !! (myTrd4 ss)

--------------------------------------------------------------------------------


> exec' :: Command -> (Stack, Store, Pc, [Store]) -> (Stack, Store, Pc, [Store])
> exec' (LoadI n) (stack, store, pc, ls) = (n:stack, store, pc+1, ls)

> exec' (Load v) (stack, store, pc, []) = (x:stack, store, pc+1, [])
> 	                                      where x = getVal v store

> exec' (Load v) (stack, store, pc, ls) = (x:stack, store, pc+1, ls)
>   where x = if hasVar v (head ls) then getVal v (head ls) else getVal v store


~~~ These "MoveToLocal" commands are how parameters work. It finds the variable
    listed as a parameter in the scope, which is the entire store stack and the
    global store, and adds it to the local stack if it exists.

> exec' (MoveToLocal v) (stack, store, pc, (ls:rest))
>     = (stack, store, pc+1, ((v, x):ls):rest)
>       where x = if rest /= [] && hasVar v (concat rest)
>                 then getVal v (concat rest)
>                 else getVal v store

> exec' (MoveToLocal v) (stack, store, pc, [])
>     = (stack, store, pc+1, [[(v, x)]])
>       where x = getVal v store


> exec' (Jump l) (stack, store, pc, ls) = (stack, store, pc+1+l, ls)
>     -- ~~~ Relative jump, so simply increases the program counter by the
>     --     desired amount.

> exec' (Skip) (stack, store, pc, ls) = (stack, store, pc+1, ls)
>     -- ~~~ Does nothing.

> exec' (Goto newPc) (stack, store, pc, ls)
>     = ((Int pc):stack, store, newPc, []:ls)
>         -- ~~~ An absolute jump. Works by putting the current program counter
>         --     on the top of the stack before jumping - this allows returning
>         --     to where we were.

> exec' (ProdLabel _) (stack, store, pc, ls) = (stack, store, pc+1, ls)
>     -- ~~~ Does nothing; is used earlier for finding the absolute address of
>     --     each procedure.

> exec' (ReturnTo) ((Int retPc):stack, store, pc, (ls:rest))
>     = (stack, store, retPc+1, rest)
>         -- ~~~ Returns from a procedure. Since Goto is used to go to a
>         --     procedure, it's assumed that the top of the stack has the
>         --     program counter position to return to.

> exec' (ReturnTo) ((Int retPc):stack, store, pc, [])
>     = (stack, store, retPc+1, [])


~~~ Make sure the stack has values if we're trying to use it!

> exec' _ ([], _, _, _)
>     = error "Attempted to retrieve data from an empty stack."


> exec' (Store v) (x:stack, store, pc, (ls:rest))
>     = if hasVar v ls
>       then (stack, store, pc+1, store':rest)
>       else (stack, store', pc+1, ls:rest)
>       where store' = if hasVar v ls then setVal v x ls else setVal v x store

> exec' (Store v) (x:stack, store, pc, []) = (stack, store', pc+1, [])
>                                            where store' = setVal v x store

> exec' (BinOp op) (x:y:stack, store, pc, ls) = (z:stack, store, pc+1, ls)
>                                               where z = apply op x y

> exec' (JumpF l) (x:stack, store, pc, ls) = (stack, store, pc+1+d, ls)
>     -- ~~~ Same as above but only increases the program counter if the top of
>     --     the stack is a False Bool.
>     where d = if x == (Bool False) then l else 0
>     -- ~~~ The previous command should be an expression that puts a Bool on
>     --     the top of the stack.


Apply an arithmetic operator.
~~~ Works with and returns the proper variable types.

> apply :: Op -> Val -> Val -> Val
> apply Plus  (Int x) (Int y) = Int (x + y)
> apply Minus (Int x) (Int y) = Int (y - x)
>     -- ~~~ Flip x and y because of how they get retrieved from the stack.
> apply Times (Int x) (Int y) = Int (x * y)
> apply Div   (Int x) (Int y) = Int (y `div` x) -- ^ (same as previous comment)
> apply Mod   (Int x) (Int y) = Int (y `mod` x) -- ^
> apply Eq    (Int x) (Int y) = Bool (if x == y then True else False)
> apply Ne    (Int x) (Int y) = Bool (if x /= y then True else False)
> apply Gt    (Int x) (Int y) = Bool (if y >  x then True else False) -- ^
> apply Ge    (Int x) (Int y) = Bool (if y >= x then True else False) -- ^
> apply Lt    (Int x) (Int y) = Bool (if y <  x then True else False) -- ^
> apply Le    (Int x) (Int y) = Bool (if y <= x then True else False) -- ^

> apply And (Bool x) (Bool y) = Bool (if x && y     then True else False)
> apply Or  (Bool x) (Bool y) = Bool (if x || y     then True else False)
> apply Not (Bool x) _        = Bool (if x == False then True else False)

> apply op _ _ = error ("Illegal arguments to " ++ (show op))


Store storage operations.

> hasVar :: Var -> Store -> Bool
> hasVar _ []            = False
> hasVar v ((k, val):xs) | v == k    = True
>                        | otherwise = hasVar v xs

> getVal :: Var -> Store -> Val
> getVal v s = foldr (\(u, x) r -> if u == v then x else r)
>                  (error "Variable not found.") s

> setVal :: Var -> Val -> Store -> Store
> setVal v x []         = [(v, x)]
> setVal v x ((u, y):s) | v == u    = (v, x):s
>                       | otherwise = (u, y):setVal v x s



-------------------------------------------------------------------------------- PROGRAM VALIDITY CHECKING
~~~ (FOR PART 2)
    checkProg checks both that declared variables are unique and that they're
    all used in the program. Checking if they're unique is easy: we just make
    sure the list of variable declarations is a set. It also checks that all
    used variables were declared.
    To make sure variables are all declared, we look through each expression in
    the code and make sure that each variable found is in the list of declared
    variables.
    We also look through the procedure code. By checking each expression for the
    correct types, there should be no errors possible. Assignments are also
    checked to make sure the types are correct.

> isSet :: (Eq a) => [a] -> Bool
> isSet []     = True
> isSet (x:xs) = notElem x xs && isSet xs

> checkProg :: Prog -> Bool
> checkProg (Prog (vars, procs, stmts))
>     = if isSet (map snd vars)
>       then varsOk (Prog (vars, procs, stmts))
>         && varsOk (Prog (vars, procs, (concat (map fst procs))))
>       else error "Multiple declarations of a variable is not allowed."

> varsOk :: Prog -> Bool
> varsOk (Prog (_, _, [])) = True
> varsOk (Prog (vars, procs, stmt:stmts))
>     = varsOk' (vars, procs, stmt) && varsOk (Prog (vars, procs, stmts))


~~~ Returning False here results in the "not all variables were declared" error.
    So, other types of errors must be handled here.

> varsOk' :: ([(Type, Var)], [Proc], Stmt) -> Bool
> varsOk' (vars, procs, Asgn v e)
>     = if elem v (map snd vars) then test2 else False
>       where test2 = if expType e vars
>                         == fst (head (filter (\x -> snd x == v) vars))
>                     then True
>                     else error ("Assignment error: " ++ (show v) ++ " <- "
>                              ++ (show e))


> varsOk' (vars, procs, If e s1 s2)
>     = if expType e vars == BoolT
>           && varsOk (Prog (vars, procs, s1))
>           && varsOk (Prog (vars, procs, s2))
>       then True
>       else error ("If statement conditional " ++ (show e)
>                ++ " needs to result in a Bool.")

> varsOk' (vars, procs, While e s)
>     = if expType e vars == BoolT && varsOk (Prog (vars, procs, s))
>       then True
>       else error ("While statement conditional " ++ (show e)
>                ++ " needs to result in a Bool.")

> varsOk' (vars, procs, _) = True

> expType :: Exp -> [(Type, Var)] -> Type
> expType (Const (Bool True))  _ = BoolT
> expType (Const (Bool False)) _ = BoolT
> expType (Const _)            _ = IntT
> expType (Var v) vars
>     | elem v (map snd vars) = fst (head (filter (\x -> snd x == v) vars))
>     | otherwise = error ("Undeclared variable: " ++ (show v))
> expType (Bin op x y) t = opType op (expType x t) (expType y t)

> opType :: Op -> Type -> Type -> Type
> opType Plus  IntT IntT = IntT
> opType Minus IntT IntT = IntT
> opType Times IntT IntT = IntT
> opType Div   IntT IntT = IntT
> opType Mod   IntT IntT = IntT
> opType Eq    IntT IntT = BoolT
> opType Ne    IntT IntT = BoolT
> opType Gt    IntT IntT = BoolT
> opType Ge    IntT IntT = BoolT
> opType Lt    IntT IntT = BoolT
> opType Le    IntT IntT = BoolT

> opType And  BoolT BoolT = BoolT
> opType Or   BoolT BoolT = BoolT
> opType Not  BoolT _     = BoolT
> opType op t1 t2 = error ("Illegal application: " ++ (show op) ++ " "
>                       ++ (show t1) ++ " " ++ (show t2))





EXAMPLE PROGRAMS
--------------------------------------------------------------------------------
~~~ All examples check Part 2 since they will throw an error if anything is
    wrong.
    All tests give correct results.


[BASIC STATEMENT TESTING]

> e1 = Const (Int 1)
> e2 = Var 'a'
> e3 = Bin Plus e1 e1

> s1 = Asgn 'a' e3
> s2 = Asgn 'a' e2

> p1 = Prog ([(IntT, 'a')], [], [s1, s2])


[TESTING IF STATEMENTS (PART 1)]
If the 2 Consts are equal, assign the number to 'a'; otherwise, assign -1.
Does a simply equality check with an If statement.

> e2'1 = Const (Int 1)
> e2'2 = Const (Int 2)
> e2'3 = Const (Int (-1))
> e2'4 = Bin Eq e2'1 e2'2

> s2'1 = Asgn 'a' e2'1
> s2'2 = Asgn 'a' e2'3
> s2'3 = If e2'4 [s2'1] [s2'2]

> p2'1 = Prog ([(IntT, 'a')], [], [s2'3])


[TESTING WHILE STATEMENTS (PART 1)]
Iterate a variable from 0 until it hits 20.
The while loop condition is "while a /= (10 + 10)" to test that the relative
jump distance from the end of the loop back to the start changes correctly
with nested statements in the condition.

> e3'1 = Const (Int 0)
> e3'2 = Const (Int 1)
> e3'3 = Const (Int 10)
> e3'4 = Var 'a'
> e3'5 = Bin Plus e3'4 e3'2
> e3'6 = Bin Ne   e3'4 e3'7
> e3'7 = Bin Plus e3'3 e3'3

> s3'1 = Asgn 'a' e3'1
> s3'2 = Asgn 'a' e3'5
> s3'3 = While e3'6 [s3'2]

> p3'1 = Prog ([(IntT, 'a')], [], [s3'1, s3'3])


[TESTING ERRORS (PART 2)]

> error1 = Prog ([(IntT, 'a')], [], [Asgn 'a' (Const (Bool False))])
>     -- Trying to assign a Bool to an Int should result in an error.

> error2 = Prog ([], [], [If (Bin Plus e3'1 e3'1) [DoSkip] [DoSkip]])
>     -- An Int operation in an If (or While) statement should result in an
>     -- error.

> error3 = Prog ([], [], [Asgn 'a' e3'1])
>     -- Variables must be declared.


[TESTING PROCEDURES (PART 3)]
Same as the previous program, but a procedure that increases 'a' by 1 is called 
in the while loop.

> e4'1 = Const (Int 0)
> e4'2 = Const (Int 1)
> e4'3 = Const (Int 20)
> e4'4 = Var 'a'
> e4'5 = Bin Plus e4'4 e4'2
> e4'6 = Bin Ne   e4'4 e4'3

> s4'1 = Asgn 'a' e4'1
> s4'2 = Asgn 'a' e4'5
> s4'3 = While e4'6 [Call "IncreaseA"]

> p4'1 = Prog ([(IntT, 'a')],
>              [([Label "IncreaseA", s4'2, Return], [])],
>              [s4'1, s4'3])


[FIBONACCI]
This calculates the nth Fibonacci number.
This is to test having two procedures in a program.
e5'fib should be set to the Fibonacci number you want to calculate, and the
result will be stored in n.

> e5'1    = Const (Int 1)
> e5'fib  = Const (Int 30)
> e5'iter = Var 'i'
> e5'num  = Var 'n'
> e5'prev = Var 'p'
> e5'tmp  = Var 't'
> e5'add  = Bin Plus e5'num  e5'prev
> e5'end  = Bin Lt   e5'iter (Bin Minus e5'fib (Const (Int 1)))
> e5'upIt = Bin Plus e5'iter e5'1

> s5'i1   = Asgn 'i' e5'1
> s5'i2   = Asgn 'p' e5'1
> s5'i3   = Asgn 'n' e5'1
> s5'a1   = Asgn 't' e5'num
> s5'a2   = Asgn 'n' e5'add
> s5'a3   = Asgn 'p' e5'tmp
> s5'a4   = Asgn 'i' e5'upIt
> s5'loop = While e5'end [Call "Update"]
> s5'init = Call "Init"

> p5'p = Prog ([(IntT, 'i'), (IntT, 'n'), (IntT, 'p'), (IntT, 't')],
>              [([Label "Init", s5'i1, s5'i2, s5'i3, Return], []),
>                   ([Label "Update", s5'a1, s5'a2, s5'a3, s5'a4, Return], [])],
>              [s5'init, s5'loop])

> runP5 = get 'n' (run p5'p)

> get :: Var -> Store -> Val
> get _ []          = (Int 0)
> get c ((k, v):xs) | k == c    = v
>                   | otherwise = get c xs


[TESTING RECURSION]
This program finds how many prime numbers there are inbetween 's' and 'f'
(inclusive of the lower bound).

> p6_vars = [(IntT, 's'), (IntT, 'f'), (BoolT, 'r'), (IntT, 'n'), (IntT, 't')]

> p6_procInit = ([Label "init",
>                     Asgn 's' (Const (Int 100)),
>                     Asgn 'f' (Const (Int 201)),
>                     Asgn 'n' (Const (Int 0)),
>                     Return], [])

> p6_procIsPrime
>     = ([Label "isPrime",
>             If (Bin Eq (Var 't') (Const (Int 1)))
>                [Asgn 'r' (Const (Bool True))]
>                [If (Bin Eq (Bin Mod (Var 's') (Var 't')) (Const (Int 0)))
>                    [Asgn 'r' (Const (Bool False))]
>                    [Asgn 't' (Bin Minus (Var 't') (Const (Int 1))),
>                     Call "isPrime"]],
>             Return], [])

> p6_code = [Call "init",
>            While (Bin Lt (Var 's') (Var 'f'))
>               [Asgn 't' (Bin Div (Var 's') (Const (Int 2))),
>                Call "isPrime",
>                If (Bin And (Var 'r') (Const (Bool True)))
>                   [Asgn 'n' (Bin Plus (Var 'n') (Const (Int 1)))]
>                   [DoSkip],
>                Asgn 's' (Bin Plus (Var 's') (Const (Int 1)))]]

> p6_prog = Prog (p6_vars, [p6_procInit, p6_procIsPrime], p6_code)
> runP6   = get 'n' (run p6_prog)


[TESTING PROCEDURE PARAMETERS (PART 4)]
Shown here is that changing local variables doesn't affect the same variable in
other scopes. Also, local scope is preserved after deeper procedure calls.
At the end, 'a' is 0, 'b' is 1, and 'c' is 2, which is as it should be when
scope is preserved correctly.

> p7_vars = [(IntT, 'a'), (IntT, 'b'), (IntT, 'c')]

> p7_proc1 = ([Label "test", Parameter 'a',
>                  Asgn 'a' (Const (Int 1)),
>                  Asgn 'b' (Var 'a'),
>                  Call "test2",
>                  Asgn 'a' (Const (Int 3)),
>                  Return], [])

> p7_proc2 = ([Label "test2", Parameter 'a',
>                  Asgn 'a' (Const (Int 2)),
>                  Asgn 'c' (Var 'a'),
>                  Return], [])

> p7_code = [Asgn 'a' (Const (Int 0)),
>            Call "test"]

> p7_prog = Prog (p7_vars, [p7_proc1, p7_proc2], p7_code)


[ACKERMANN FUNCTION]
Tests parameters, recursion, and local variables/scope.
Returning would work well here, but it isn't implemented at this time.
Fortunately, using a global variable as the return value of a procedure works
perfectly.

> p8_vars = [(IntT, 'm'), (IntT, 'n'), (IntT, 'r')]

> p8_ackermann = ([Label "ackermann", Parameter 'm', Parameter 'n',
>                      If (Bin Eq (Var 'm') (Const (Int 0)))
>                         [Asgn 'r' (Bin Plus (Var 'n') (Const (Int 1)))]
>                         [If (Bin Eq (Var 'n') (Const (Int 0)))
>                             [Asgn 'm' (Bin Minus (Var 'm') (Const (Int 1))),
>                              Asgn 'n' (Const (Int 1)),
>                              Call "ackermann"]
>                             [Asgn 'n' (Bin Minus (Var 'n') (Const (Int 1))),
>                              Call "ackermann",
>                              Asgn 'n' (Var 'r'),
>                              Asgn 'm' (Bin Minus (Var 'm') (Const (Int 1))),
>                              Call "ackermann"]],
>                  Return], [])

> p8_code = [Asgn 'm' (Const (Int 3)),
>            Asgn 'n' (Const (Int 5)),
>            Call "ackermann"]

> p8_prog = Prog (p8_vars, [p8_ackermann], p8_code)
> runP8   = get 'r' (run p8_prog)


[COLLATZ CONJECTURE]
'n' is the input number. If it is even, half it. Otherwise, multiply it by 3 and
    add 1. When it reaches 1, the cycle is complete.
'r' holds the result, which is how many cycles it took to reach 1.

> p9_vars = [(IntT, 'n'), (IntT, 'r')]

> p9_iterate = ([Label "iterate",
>                    Parameter 'n',
>                    If (Bin Eq (Var 'n') (Const (Int 1)))
>                       [DoSkip]
>                       [If (Bin Eq (Bin Mod (Var 'n') (Const (Int 2)))
>                            (Const (Int 0)))
>                           [Asgn 'n' (Bin Div  (Var 'n') (Const (Int 2))),
>                            Asgn 'r' (Bin Plus (Var 'r') (Const (Int 1))),
>                            Call "iterate"]
>                           [Asgn 'n' (Bin Times (Var 'n') (Const (Int 3))),
>                            Asgn 'n' (Bin Plus  (Var 'n') (Const (Int 1))),
>                            Asgn 'r' (Bin Plus  (Var 'r') (Const (Int 1))),
>                            Call "iterate"]],
>                Return], [])

> p9_code = [Asgn 'n' (Const (Int 2394)),
>            Asgn 'r' (Const (Int 0)),
>            Call "iterate"]

> p9_prog = Prog (p9_vars, [p9_iterate], p9_code)
> runP9   = get 'r' (run p9_prog)


[GREATEST COMMON DIVISOR]
WARNING: This doesn't seem to be getting the actual greatest common divisor.
         Discovered while cleaning up the code.

> p10_vars = [(IntT, 'a'), (IntT, 'b'), (IntT, 'g'), (IntT, 'd')]

> p10_aEven = Bin Eq (Bin Mod (Var 'a') (Const (Int 2))) (Const (Int 0))
> p10_bEven = Bin Eq (Bin Mod (Var 'b') (Const (Int 2))) (Const (Int 0))

> p10_aHalf = Asgn 'a' (Bin Div (Var 'a') (Const (Int 2)))
> p10_bHalf = Asgn 'b' (Bin Div (Var 'b') (Const (Int 2)))

> p10_GCD = ([Label "GCD", Parameter 'a', Parameter 'b',
>                 Asgn 'd' (Const (Int 0)),
>                 While (Bin And p10_aEven p10_bEven)
>                    [p10_aHalf,
>                     p10_bHalf,
>                     Asgn 'd' (Bin Plus (Var 'd') (Const (Int 1)))],
>                 While (Bin Ne (Var 'a') (Var 'b'))
>                    [If p10_aEven
>                        [p10_aHalf]
>                        [If (p10_bEven)
>                            [p10_bHalf]
>                            [If (Bin Gt (Var 'a') (Var 'b'))
>                                [Asgn 'a' (Bin Div (Bin Minus (Var 'a')
>                                     (Var 'b')) (Const (Int 2)))]
>                                [Asgn 'b' (Bin Div (Bin Minus (Var 'b')
>                                     (Var 'a')) (Const (Int 2)))]]]],
>                 Asgn 'g' (Var 'a'),
>                 Return], [])

> p10_code = [Asgn 'a' (Const (Int 295951656)),
>             Asgn 'b' (Const (Int 15383424)),
>             Call "GCD"]

> p10_prog = Prog (p10_vars, [p10_GCD], p10_code)
> runP10   = get 'g' (run p10_prog)
