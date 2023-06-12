module Stack (
   Stack,
   push,
   pop,
   top,
   empty,
   isEmpty,
   makeStack,
   size)
where
data Stack a = Stk [a]
  deriving Show
push :: a -> Stack a -> Stack a
push x (Stk xs) = Stk (x:xs)
pop :: Stack a -> Stack a
pop (Stk (_:xs)) = Stk xs
pop _            = error "Stack.pop: empty stack"
top :: Stack a -> a
top (Stk (x:_)) = x
top _           = error "Stack.top: empty stack"
empty :: Stack a
empty = Stk []
isEmpty :: Stack a -> Bool
isEmpty (Stk []) = True
isEmpty (Stk  _) = False
makeStack :: [a] -> Stack a
makeStack xs = Stk xs
size :: Stack a -> Int
size (Stk xs) = length xs
--Stack> push 'a' empty
--Stk "a"
--Stack> push 'b' (push 'a' empty)
--Stk "ba"
--Stack> top $ push 'b' (push 'a' empty)
--'b'
--Stack> pop $ push 'b' (push 'a' empty)