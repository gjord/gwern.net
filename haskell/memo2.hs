main = interact (unlines . tabify . couplet . lines)

couplet [] = []
couplet (_:[]) = []
couplet (a:b:c) = [((erase a), b), (a,b)] ++ [(a, (erase b)), (a,b)] ++ couplet c
                  where erase = map (const '_')

tabify = map (\((x,y), (z,a)) -> x ++ "<br>" ++ y ++ "\t" ++ z ++ "<br>" ++ a) . tuplize
         where tuplize [] = []
               tuplize (a:b:c) = [(a,b)] ++ tuplize c