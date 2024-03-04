module Stack (Stack(..),StackElem(..), -- exportar o tipo
push, pop, top, -- e as operações
empty, isEmpty, topInt, topBool) where
data StackElem = IntElem Integer | BoolElem Bool deriving Show
data Stack = Stk [StackElem] deriving Show -- implementação usando listas



push :: StackElem -> Stack -> Stack
push x (Stk xs) = Stk (x:xs)

pop :: Stack -> Stack
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack -> StackElem
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack
empty = Stk []

isEmpty :: Stack -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False

-- Get the top element of the stack if it is a integer
topInt :: Stack -> Integer
topInt stack = case top stack of
    IntElem value -> value
    _ -> error "Run-time error"

-- Get the top element of the stack if it is a boolean
topBool :: Stack -> Bool
topBool stack = case top stack of
    BoolElem value -> value
    _ -> error "Run-time error"