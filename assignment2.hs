--Ans 1.
--sublist [1,2,3]
sublist :: [a] -> [[a]]
sublist [] = [[]]
sublist (x:xs) = [x:sub|sub <- sublist xs] ++ sublist xs

--Ans 2.
--delete 2 [3,4,5,6,7,8,9]
delete :: Int -> [Int] -> [Int]
delete _ [] = []
delete n xs = take (n-1) xs ++ delete n (drop n xs) 
{-
drop & take functions are utilised for solving this problem.
  Input: drop 5 [1,2,3,4,5,6,7,8,9,10]
  Output: [6,7,8,9,10]
  
  take n, applied to a list xs, returns the prefix of xs of length n, or 
xs itself if n > length xs:
  take 3 [1,2,3,4,5] == [1,2,3]
-}

--Ans 3.
--replic [2,3,4,7,6]
replic :: [a] -> [[a]] 
replic = map f . zip [1..] where
f (c,l1) = replicate c l1
{-
map                     :: (a->b) -> [a] -> [b]
map f  []               =  []
map f (x:xs)            =  f x : map f xs

The map function is polymorphic and its type indicates clearly that its 
first argument is a function; note also that the two a's must be 
instantiated with the same type (likewise for the b's). As an example of 
the use of map, we can increment the elements in a list:
map (add 1) [1,2,3] => [2,3,4]

where zip is a Standard Prelude function that returns the pairwise 
interleaving of its two list arguments: 

zip (x:xs) (y:ys)       = (x,y) : zip xs ys
zip  xs     ys          = []


-}

--Ans 4.
--laste [1,2,3,4,5,6]
laste = head . reverse
{-
reverse & head functions are used to solve this problem.

reverse :: [a] -> [a] 
reverse xs returns the elements of xs in reverse order. xs must be 
finite.

Function:      head
Type:	       [a] -> a
Description:   returns the first item of a list

Input: head [1,2,3]
Output: 1
-}

{- 
Ans 5. The function creates a range between n and m, If n is smaller
then counts up
from n to m. Counts down from m to n if n is larger.
If the value of m & n are equal then it just prints the value of m=n.

mystery 4 10
[4,5,6,7,8,9,10]

mystery 10 4
[10,9,8,7,6,5,4]

mystery 1 1
[1]

-}

--Ans 6.

data Formula
	= Atom Bool			-- atomic formula
	| And Formula Formula 	 	-- f /\ f
	| OR Formula Formula 		-- f \/ f
	| Not Formula			-- not(f)

instance Show Formula where	
	show (Atom x) = "Atom " ++ show x
	show (And x y) = "And (" ++ show x ++ ") (" ++ show y ++ ")"
	show (OR x y) = "OR (" ++ show x ++ ") (" ++ show y ++ ")"
	show (Not x) = "Not (" ++ show x ++ ")"

collect_atoms (Atom z) = [Atom z]
collect_atoms (And x y) = collect_atoms x ++ collect_atoms y 
collect_atoms (OR x y) = collect_atoms x ++ collect_atoms y 
collect_atoms (Not x) = collect_atoms x

eval (Atom x) = x
eval (And x y) = eval x && eval y
eval (OR x y) = eval x || eval y
eval (Not x) = not (eval x)
{-
Input: collect_atoms (And (OR (Atom True) (Atom False)) (Not (Atom 
False)) )
Output: [Atom True, Atom False, Atom False]

Input: eval (And (OR (Atom True) (Atom False)) (Not (Atom False)) )
Output: True

Function:   show
Type:	    Show a => a -> String
Class:	    Show
Description:	converts its argument to a string

Input: show 12
Output: "12"
-}

