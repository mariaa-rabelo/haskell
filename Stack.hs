
module Stack (Stack, -- exportar o tipo
    push, pop, top, -- e as operações
    empty, isEmpty, stackToList) where

data Stack a = Stk [a] -- implementação usando listas

push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)

pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _ = error "Stack.pop: empty stack"

top :: Stack a -> a
top (Stk (x:_)) = x
top _ = error "Stack.top: empty stack"

empty :: Stack a
empty = Stk []

isEmpty :: Stack a -> Bool
isEmpty (Stk [])= True
isEmpty (Stk _) = False

stackToList :: Stack a -> [a]
stackToList (Stk xs) = xs