Pair x y = Pair A B
length = $fix \r.\Nil.Z|Cons _ xs.S(r xs)
reverse = ($fix \r.\acc.\Nil.acc|Cons x xs.r (Cons x acc) xs) Nil
eq = \f.\t.\x.\y.(\Pair A A . t|Pair B B . t|_ . f)(Pair x y)U
_ = reverse (Cons A (Cons B (Cons C Nil)))
