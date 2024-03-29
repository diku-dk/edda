-- Experimenting with a highly succinct programming style based on
-- tiny functions, inspired by APL style.
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
--
-- Style yardsticks:
--
--  - Short names, even for functions.
--
--  - Single line definitions are ideal; two lines (one for types, one
--    for body) almost as good.  More than that should be considered
--    work-in-progress.  (Exception: matching the various cases of a
--    sum type, but then this should be factored into its own
--    function).
--
--  - Implicit size polymorphism, because why not.

def rep [n] = replicate n

def rot = rotate

def red = reduce

def rev = reverse

def len = length

def idx xs i = xs[(i:i64)]

def gather xs = map (idx xs)

def idxs [n] = iota n

def f &&& g = \x -> (f x, g x)

def matches [n][m] 'a 'b (_: [m]b) (as: [n]a) : [m]a = as :> [m]a

def matching [n][m] 'a 'b (_: [m]b) (as: [n]a) : [m]a = take m as

def islen 'a n (as: [n]a) = as

def exactly m arr = matches (iota m) arr

def imap f xs = map2 f idxs xs

def ifilter f xs = filter (uncurry f) (zip idxs xs) |> map (.1)

def last_or x xs = if len xs == 0 then x else last xs

def exscan 'a [n] (op: a -> a -> a) (ne: a) (as: [n]a) =
  scan op ne (map2 (\i a -> if i == 0 then ne else a) idxs (rot (-1) as))

type opt 't = #some t | #none

def opt 't x f (o: opt t) =
  match o case #some y -> f y
          case #none -> x

def guard 't p x : opt t = if p x then #some x else #none

def opt' 't x y : opt t = opt y (\x -> #some x) x

def find p xs = xs |> map (guard p) |> red opt' #none

def idxof p xs = zip idxs xs |> find ((.1) >-> p) |> opt (len xs) (.0)

def segop op (x_flag,x) (y_flag,y) =
  (x_flag || y_flag, if y_flag then y else x `op` y)

def segscan op ne flags as =
  scan (segop op) (false, ne) (zip flags as) |> map (.1)

def segscan_f op ne f as = segscan op ne (map f as) as

-- | repiota [1,0,2,3] == [0, 2, 2, 3, 3, 3]
def repiota ns =
  segscan_f (+) 0 (>0) (hist i64.max 0 (i64.sum ns) (exscan (+) 0 ns) idxs)

-- | `ljustify (!=0) [0,0,1,2,3] == [1,2,3,0,0]`
def ljustify p xs = rot (idxof p xs) xs

-- | `rjustify (!=0) [1,2,3,0,0] == [0,0,1,2,3]`
def rjustify p xs = xs |> rev |> ljustify p |> rev

def ilog2 n = 63 - i64.clz n

def pad_to k x xs = sized k (xs++islen (k-len xs) (rep x))

def maximum lte xs = red (\x y -> if x `lte` y then y else x) xs[0] xs

local def padpow2 lte xs =
  let d = i64.i32 (ilog2 (len xs)) in
  if d < 0 || len xs == 2**d then (copy xs, d)
  else (pad_to (2**(d+1)) (maximum lte xs) xs, d+1)

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
  in (loop xs' for i < d do loop xs' for j < i+1 do bitonic lte xs' i j)
     |> matching xs

def neq lte x y = if x `lte` y then !(y `lte` x) else true
def eq lte x y = (x `lte` y) && (y `lte` x)
def lte0 lte (a,_) (b,_) = a `lte` b
def lte1 lte (_,x) (_,y) = x `lte` y
def lte01 lte0 lte1 (a,x) (b,y) = if eq lte0 a b then x `lte1` y else a `lte0` b

def wnexts xs = zip xs (rot 1 xs)
def wprevs xs = zip xs (rot (-1) xs)

-- | Remove consecutive duplicates.
def pack lte xs = wprevs xs |> ifilter (\i (x,y) -> i == 0 || neq lte x y) |> map (.0)

-- | Remove all duplicates; does not maintain item order.
--
-- `dedup (<=) [1,10,2,1,5,2] == [1, 2, 5, 10]`
def dedup lte xs = sort lte xs |> pack lte

-- | Remove all duplicates; maintains item order.
--
-- `nub (<=) [1,10,2,1,5,2] == [1, 10, 2, 5]`
def nub lte xs =
  zip xs idxs |> sort (lte01 lte (<=)) |> pack (lte0 lte) |> sort (lte1 (<=)) |> map (.0)

def count p xs = xs |> map p |> map i64.bool |> i64.sum

def dtoi (c: u8): i64 = i64.u8 c - '0'
def is_digit (c: u8) = c >= '0' && c <= '9'
def isnt_digit = not <-< is_digit
def atoi [n] (s: [n]u8): i64 =
  let (sign,s) = if n > 0 && s[0] == '-' then (-1,drop 1 s) else (1,s)
  in sign * (loop (acc,i) = (0,0) while i < length s do
               if is_digit s[i]
               then (acc * 10 + dtoi s[i], i+1)
               else (acc, n)).0
