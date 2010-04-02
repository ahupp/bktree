{--

An implementation of Burkhard-Keller trees.  These allow fast lookup
of words within a certain distance of a query word.

The implementation is based on the description in this article:

http://blog.notdot.net/2007/4/Damn-Cool-Algorithms-Part-1-BK-Trees

- Adam Hupp <adam@hupp.org>

--}

module BKTree ( queryTree,
                mkTree,
                levenshtein,
                main
              ) where


import List
import qualified Data.Map as Map

type Children a = Map.Map Int (TreeNode a) 

data TreeNode a = TreeNode a (Children a) deriving Show

data Tree a = Tree (DistFunc a) (TreeNode a) 

type DistFunc a = a -> a -> Int




addWord :: DistFunc a -> TreeNode a -> a -> TreeNode a
addWord distfn (TreeNode word kids) w =
    let distance = distfn word w
        newSubTree = if Map.member distance kids then
                         addWord distfn (kids Map.! distance) w
                     else
                         TreeNode w Map.empty
        childUpdate = Map.insert distance newSubTree kids
    in
      TreeNode word childUpdate

    


queryTreeNode :: DistFunc a -> a -> Int -> TreeNode a -> [(Int, a)]
queryTreeNode distfunc qword n (TreeNode word kids)  =
    let dist = distfunc word qword
        range = [dist-n..dist+n+1]
        kidsInRange = mapSelect kids range
        childResults = concatMap (queryTreeNode distfunc qword n) kidsInRange
    in if dist <= n then
           (dist, word) : childResults
       else
           childResults    

compareBy :: Ord b => (a -> b) -> a -> a -> Ordering
compareBy by lhs rhs = compare (by lhs) (by rhs)

mapSelect :: Ord k => Map.Map k v -> [k] -> [v]
mapSelect mp (x:xs) =
    if Map.member x mp then
        (mp Map.! x ) : mapSelect mp xs
    else mapSelect mp xs
mapSelect _ [] = []

mkTree :: DistFunc a -> [a] -> Tree a
mkTree distfn (w:ws) =
    let root = TreeNode w Map.empty
        tree = foldl (addWord distfn) root ws
    in 
      Tree distfn tree


queryTree :: Tree a -> a -> Int -> [(Int,a)]
queryTree (Tree distfn root) qword n =
    sortBy (compareBy fst) $ queryTreeNode distfn qword n root

main = do dictFile <- readFile "/usr/share/dict/american-english"
          words <- return $ lines dictFile
          tree <- return $ mkTree levenshtein words
          putStrLn (show (queryTree tree "the" 2))

-- http://www.cse.unsw.edu.au/~dons/code/lambdabot/Lib/Util.hs
-- | Levenshtein edit-distance algorithm
-- Translated from an Erlang version by Fredrik Svensson and Adam Lindberg
--
levenshtein :: String -> String -> Int
levenshtein [] [] = 0
levenshtein s  [] = length s
levenshtein [] s  = length s
levenshtein s  t  = lvn s t [0..length t] 1

lvn :: String -> String -> [Int] -> Int -> Int
lvn [] _ dl _ = last dl
lvn (s:ss) t dl n = lvn ss t (lvn' t dl s [n] n) (n + 1)

lvn' :: String -> [Int] -> Char -> [Int] -> Int -> [Int]
lvn' [] _ _ ndl _ = ndl
lvn' (t:ts) (dlh:dlt) c ndl ld | length dlt > 0 = lvn' ts dlt c (ndl ++ [m]) m
    where
        m = foldl1 min [ld + 1, head dlt + 1, dlh + (dif t c)]
lvn' _ _ _ _  _  = error "levenshtein, ran out of numbers"

dif :: Char -> Char -> Int
dif = (fromEnum .) . (/=)    

