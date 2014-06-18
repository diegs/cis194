module Party where

import Data.Monoid
import Data.List
import Data.Tree

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es fun) = GL (e:es) (fun + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend gl1 (GL gl2 _) = foldr glCons gl1 gl2

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2) = if (f1 >= f2) then gl1 else gl2

-- treeFold :: (a -> b -> b) -> b -> Tree a -> b
-- treeFold f x xs = x''
--   where x' = foldr (\a b -> (treeFold f b a)) x (subForest xs)
--         x'' = f (rootLabel xs) x''

treeFold :: (a -> [b] -> b) -> [b] -> Tree a -> b
treeFold f x xs = f (rootLabel xs) (map (treeFold f x) (subForest xs))

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (bossList, noBossList)
  where bossList = GL [boss] (empFun boss)
        noBossList = mconcat $ map (uncurry moreFun) gls

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . (treeFold nextLevel mempty)

sortedEmployees :: GuestList -> String
sortedEmployees (GL es fun) = "Total Fun: " ++ show fun ++ "\n" ++ ((unlines . sort . (map empName)) es)

main :: IO ()
main = do
  employees <- readFile "company.txt"
  putStrLn (sortedEmployees (maxFun (read employees)))
