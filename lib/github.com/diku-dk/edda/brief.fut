-- Experimenting with a highly succint programming style based on tiny
-- functions, inspired by APL style.
--
-- I am somewhat dissatisfied with how clumsy much Futhark code ends
-- up looking, and I want to check to what extent the language
-- supports a more compact style of programming.  Maybe this can also
-- serve as inspiration to lift some restrictions (e.g. avoiding type
-- annotations).
--
-- We may use slightly more complicated definitions here and there to
-- build up the fundamental primitives (e.g. the arithmetic needed for
-- random numbers), but otherwise we will try to keep it very compact.
--
-- Point-free programming is acceptable, but not required.  The goal
-- is not golfing - we don't want to minimise at the expense of
-- readability.  The goal is readability *through* concision.
--
-- The purpose is still to create reusable polymorphic, higher-order,
-- and idiomatic Futhark code, just with a specific notion of
-- "idiomatic".

def rep = replicate

def rot = rotate

def red = reduce

def rev = reverse

def len = length

def idx xs i = xs[(i:i64)]

def gather xs = map (idx xs)

def idxs = indices

def f &&& g = \x -> (f x, g x)

def imap f xs = map2 f (idxs xs) xs

type opt 't = #some t | #none

def opt 't x f (o: opt t) =
  match o case #some y -> f y
          case #none -> x

def guard 't p x : opt t = if p x then #some x else #none

def opt' 't x y : opt t = opt y (\x -> #some x) x

def find p xs = xs |> map (guard p) |> red opt' #none

def idxof p xs =
  zip (idxs xs) xs |> find ((.1) >-> p) |> opt (len xs) (.0)

-- | `ljustify (!=0) [0,0,1,2,3] == [1,2,3,0,0]`
def ljustify p xs = rot (idxof p xs) xs

-- | `rjustify (!=0) [1,2,3,0,0] == [0,0,1,2,3]`
def rjustify p xs = xs |> rev |> ljustify p |> rev

def ilog2 n = 63 - i64.clz n

local def padpow2 lte xs =
  let d = i64.i32 (ilog2 (len xs)) in
  if d < 0 || len xs == 2**d then (copy xs, d)
  else let largest = reduce_comm (\x y -> if x `lte` y then y else x) xs[0] xs
       in (xs ++ rep (2**(d+1) - len xs) largest, d+1)

local def bitonic lte a p q =
  let d = 1 << (p-q) in
  let f i a_i =
    let up1 = ((i >> p) & 2) == 0
    in if (i & d) == 0
       then let a_iord = a[i | d] in
            if (a_iord `lte` a_i) == up1
            then a_iord else a_i
       else let a_ixord = a[i ^ d] in
            if (a_i `lte` a_ixord) == up1
            then a_ixord else a_i
  in imap f a

def sort lte xs =
  let (xs', d) = padpow2 lte xs
  in take (len xs)
          (loop xs' for i < d do loop xs' for j < i+1 do bitonic lte xs' i j)

def nub lte xs =
  let neq x y = if x `lte` y then !(y `lte` x) else true
  in sort lte xs |> (id &&& rot 1) |> uncurry zip
     |> imap (\i (x,y) -> (x,i == 0 || neq x y)) |> filter (.1) |> map (.0)
