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

def idx xs i = xs[(i:i64)]

def gather xs = map (idx xs)

def idxs = indices

type opt 't = #some t | #none

def opt 't x f (o: opt t) =
  match o case #some y -> f y
          case #none -> x

def guard 't p x : opt t = if p x then #some x else #none

def opt' 't x y : opt t = opt y (\x -> #some x) x

def find p xs = xs |> map (guard p) |> reduce opt' #none

def idxof p xs =
  zip (idxs xs) xs |> find ((.1) >-> p) |> opt (length xs) (.0)

-- | `ljustify (!=0) [0,0,1,2,3] == [1,2,3,0,0]`
def ljustify p xs = rotate (idxof p xs) xs

-- | `ljustify (!=0) [1,2,3,0,0] == [0,0,1,2,3]`
def rjustify p xs = xs |> reverse |> ljustify p |> reverse
