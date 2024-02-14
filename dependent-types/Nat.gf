abstract Nat = {
  flags startcat = Span ;
  cat
    Span ;               -- for testing
    Nat ;                -- natural number
  data
    Zero : Nat ;         -- 0
    Succ : Nat -> Nat ;  -- the successor of x

  cat Less Nat Nat ;
  -- Proof objects generated by function
  fun LessZ : (y : Nat) -> Less Zero (Succ y) ;
  fun LessS : (x,y : Nat) -> Less x y ->
                               Less (Succ x) (Succ y) ;

  fun FromTo : (m,n : Nat) -> Less m n -> Span ;

}