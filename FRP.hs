module FRP where

import SOE
import Data.Maybe

type Stimulus = (Double, Maybe SOEEvent)

data Behavior a = Behavior (Stimulus -> (Behavior a, a))

data Event a = Event (Stimulus -> (Event a, Maybe a))

-- Abbrevations for common behavior types

type BD  = Behavior Double
type BB  = Behavior Bool
type BP2 = Behavior P2
type BS  = Behavior String
type BG  = Behavior Graphic
type BC  = Behavior Color

-- Basic lifters

lift0 :: a -> Behavior a
lift0 v = f
  where f = Behavior (const (f, v))

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 fn (Behavior a0) = f a0
  where f a = Behavior (\s -> let (Behavior a', av) = a s in
                                (f a', fn av))

lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 fn (Behavior a0) (Behavior a1) = 
	let f a b = Behavior (\s -> let (Behavior a', av) = a s
					(Behavior b', bv) = b s in
				     (f a' b', av `fn` bv))
	  in f a0 a1

lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 fn (Behavior a0) (Behavior a1) (Behavior a2) =
	let f a b c = Behavior (\s -> let 	(Behavior a', av) = a s
						(Behavior b', bv) = b s 
						(Behavior c', cv) = c s in
					  (f a' b' c', fn av bv cv))
	  in f a0 a1 a2

-- Numeric Lifting and the P2 class

intToDouble :: Int -> Double
intToDouble i = fromIntegral $ toInteger i

-- Make most methods in Num reactive

instance Num a => Num (Behavior a) where
  (+)           = lift2 (+)
  (-)           = lift2 (-)
  (*)           = lift2 (*)
  abs           = lift1 abs
  negate        = lift1 negate
  fromInteger i = lift0 (fromInteger i)
  signum        = error "Cant use signum on behaviors"
  
-- Make methods in Fractional reactive

instance Fractional a => Fractional (Behavior a)
  where
    (/) = lift2 (/)
    fromRational r = lift0 (fromRational r)
 
-- Create a class to represent a 2-D point

data P2 = P2 Double Double deriving (Show, Eq)

-- Some of these methods don't work but it's better to make P2 an
-- instance of Num than to create a different class for operators
-- on P2 objects.

instance Num P2 where
   P2 x1 y1 + P2 x2 y2  = P2 (x1+x2) (y1+y2)
   P2 x1 y1 - P2 x2 y2  = P2 (x1-x2) (y1-y2)
   negate (P2 x y)      = P2 (-x) (-y)
   (*)                  = error "No * method for P2"
   abs                  = error "No abs method for P2"
   signum               = error "No * method for P2"
   fromInteger 0        = P2 0 0
   fromInteger _        = error "Only the constant 0 can be used as a P2"

-- Make a reactive constructor

p2 :: BD -> BD -> BP2
p2 = lift2 P2

-- Reactive getter functions

getX, getY :: BP2 -> BD
getX = lift1 (\(P2 x y) -> x)
getY = lift1 (\(P2 x y) -> y)

-- Use * to make reactive versions of Ord operators

(>*), (<*), (>=*), (<=*), (==*) :: Ord a => Behavior a -> Behavior a -> BB
(>*)  = lift2 (>)
(<*)  = lift2 (<)
(>=*) = lift2 (>=)
(<=*) = lift2 (<=)
(==*) = lift2 (==)

-- Reactive and / or

(&&*), (||*) :: BB -> BB -> BB
(&&*) = lift2 (&&)
(||*) = lift2 (||)

-- This class represents values that can be scaled

class Num a => Vec a where
  (*^) :: Double -> a -> a

instance Vec Double where
  (*^) = (*)

instance Vec P2 where
  d *^ (P2 x y) = P2 (d*x) (d*y)

-- Use "choose" as a reactive if statement

choose :: BB -> Behavior a -> Behavior a -> Behavior a
choose = lift3 (\test i e -> if test then i else e)

-- Reactive string operators

(+++) :: BS -> BS -> BS
(+++) = lift2 (++)

showB :: Show a => Behavior a -> BS
showB = lift1 show

-- Reactive colors (all constant!)

black, blue, green, cyan, red, magenta, yellow, white :: BC

black   = lift0 Black
blue    = lift0 Blue
green   = lift0 Green
cyan    = lift0 Cyan
red     = lift0 Red
magenta = lift0 Magenta
yellow  = lift0 Yellow
white   = lift0 White

-- Basic Behaviors

time :: BD
time = f where
   f = Behavior (\(t, _) -> (f, t))

-- You can integrate Double and P2

integral :: Vec a => Behavior a -> Behavior a
integral (Behavior bf) = Behavior (\s@(t,_) -> let (Behavior a',av) = bf s in (f t 0 a', av)) where 
	f oldt b c = Behavior (\s'@(t', _) -> let (Behavior b', bv) = c s' 
						  newv = b + (t'-oldt)*^bv in
					   (f t' newv b', newv))

-- Find events in the stimulus stream.  This is a utility
-- to use for writing other GUI functionality

stimEvent :: (SOEEvent -> Maybe v) -> Event v
stimEvent fn = f where
	f = Event (\(_, s) -> (f, case s of
				Just s' -> fn s'
				Nothing -> Nothing))

lbp, lbr, rbp, rbr :: Event ()
lbp = stimEvent (\ev -> case ev of
			(Button _ True True) -> Just ()
			_		   -> Nothing
		)
lbr = stimEvent (\ev -> case ev of
			(Button _ True False) -> Just ()
			_		   -> Nothing
		)
rbp = stimEvent (\ev -> case ev of
			(Button _ False True) -> Just ()
			_		   -> Nothing
		)
rbr = stimEvent (\ev -> case ev of
			(Button _ False False) -> Just ()
			_		   -> Nothing
		)

-- This is an internal function to return the mouse movement event stream
-- Use hold to turn this into a behavior

mouseEv :: Event P2
mouseEv = stimEvent (\ev -> case ev of
			(MouseMove (a, b)) -> Just $ P2 (intToDouble a) (intToDouble b)
			_		   -> Nothing
		     )

mouse :: Behavior P2
mouse = hold (P2 0 0) mouseEv

key :: Event Char
key = stimEvent (\ev -> case ev of
			 (Key c True)	-> Just c
			 _		-> Nothing
		)

keyUp :: Event Char
keyUp = stimEvent (\ev -> case ev of
			 (Key c False)	-> Just c
			 _		-> Nothing
		)

keyIs :: Char -> Event ()
keyIs k = stimEvent (\ev -> case ev of
			 (Key k _)	-> Just ()
			 _		-> Nothing
		)

-- Event stream combinators

never :: Event a
never = Event (const (never, Nothing))

always :: Event ()
always = Event (const (always, Just ()))

-- This internal function will allow you to build
-- most of the event functionality

mapEs :: st -> ((st, a) -> (st, Maybe b)) -> Event a -> Event b
mapEs state fn ev = error "derp"

filterE :: Event a -> (a -> Bool) -> Event a
filterE (Event ef) fn = f ef where
	f a = Event (\s -> let (Event a', av) = a s in
			(f a', case av of
				Just c  -> if (fn c) then Just c else Nothing
				Nothing -> Nothing))
-- This suppresses events once the tag supply is exhausted

tags :: [a] -> Event b -> Event a
tags l@(l':ls) (Event ef) = f l ef where
	f x@(x':xs) a = Event (\s -> let (Event a', av) = a s in
				case av of
					Just c  -> (f xs a', Just x')
					Nothing -> (f x a', Nothing))

(==>) :: Event a -> (a -> b) -> Event b
(Event a0) ==> fn =
   let f a = Event (\s -> let (Event a', (av)) = a s in
                                (f a', (fmap fn av)))
     in f a0

when :: Behavior Bool -> Event ()
when (Behavior bf) = f bf where
	f a = Event (\s -> let  (Behavior a', av) = a s
				(Behavior b', bv) = a' s in
			     (f b', case (av, bv) of
					(True, True) -> Just ()
					_ -> Nothing))

once :: Event a -> Event a
once = error "once not implemented"

clock :: Double -> Event ()
clock r = f r where
	f a = Event (\(t, _) -> case (t > a) of
				True  -> (f (t+r), Just ())
				False -> (f a, Nothing)
			)

(-=>) :: Event a -> b -> Event b
a -=> b = a ==> (const b)

(.|.) :: Event a -> Event a -> Event a
(Event e1) .|. (Event e2) = 
	f e1 e2 where
		 f a b = Event (\s -> let (Event e1', e1v) = a s
					  (Event e2', e2v) = b s in
					(f e1' e2', case (e1v, e2v) of
					 (Nothing, Nothing) -> Nothing
					 (Just x, Nothing) -> Just x
					 (Nothing, Just x) -> Just x
					 (Just x, Just x') -> Just x
				))

snap :: Event a -> Behavior b -> Event b
snap (Event ef) (Behavior bf) = f ef bf where
	f a b = Event (\s -> let (Event a', av) = a s
				 (Behavior b', bv) = b s in
				(f a' b', case av of
					Just _  -> Just bv
					Nothing -> Nothing
			))

-- Switchers

switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch (Behavior bf) (Event ef) = f bf ef where
	f a b = Behavior (\s -> case b s of
				(_, Just (Behavior bf')) -> (f bf' b, snd $ bf' s)
				(_, Nothing) -> (f a b, snd $ a s)
			)

until :: Behavior a -> Event b -> (b -> Behavior a) -> Behavior a
until (Behavior bf) (Event ef) fn = f bf ef where
	f a b = Behavior (\s -> let   	(Behavior a', av) = a s
					(Event b', bv) = b s in
				    case bv of
				     Nothing -> (f a' b', av)
				     Just x  -> let (Behavior z) = fn x 
						    (Behavior z', zv) = z s in
						(f z' b', zv))
until_ :: Behavior a -> Event b -> Behavior a -> Behavior a
until_ b e fn = FRP.until b e (const fn)

accum :: a -> Event (a -> a) -> Behavior a
accum v (Event ef) = f ef v where
	f a v' = Behavior (\s -> let (Event a', av) = a s in 
				case av of
				Just f' -> (f a' (f' v'), f' v')
				Nothing -> (f a' v', v')
			)

hold :: a -> Event a -> Behavior a
hold v ev = accum v (ev ==> const)

-- Graphics operations

el :: BP2 -> BP2 -> BG
el = lift2 (\(P2 x1 y1) (P2 x2 y2) -> ellipse (round x1, round y1) (round x2, round y2))

($$) :: BG -> BG -> BG
($$) = lift2 overGraphic

(@@) :: BS -> BP2 -> BG -- Text
(@@) = lift2 (\txt (P2 x y) -> text (round x, round y) txt)

(&*) :: BC -> BG -> BG
(&*) = lift2 withColor


animate :: BG -> IO ()
animate (Behavior b)
   = runGraphics (
     do w <- openWindowEx "FRP Test" (Just (0,0)) (Just (500, 500))
               drawBufferedGraphic
        t0 <- timeGetTime
        let loop b =
              do t <- timeGetTime
                 let word32ToInt = fromInteger . toInteger
                 let ft = intToDouble (word32ToInt (t-t0)) / 1000
                 e <- maybeGetWindowSOEEvent w
                 let (Behavior b1, g) = b (ft, e)
                 setGraphic w g
                 case e of
                    Nothing -> loop b1
                    Just Closed -> closeWindow w
                    _ -> loop b1
        loop b
     )
	 
-- Tests lift2 - shows the current time as a text string
t1 = animate $ showB time @@ (p2 50 50)

-- Should work at the same time as t1 - shows a growing ellipse
t2 = animate $ el (p2 0 0) (p2 (time * 10) (time * 20))

-- Requires key and hold -- Shows the last key typed
t3 = animate $ showB (hold 'x' key) @@ (p2 50 50)

-- Requires switch and ==> - color changes when key is pressed
charToColor :: Char -> BC
charToColor 'r' = red
charToColor 'g' = green
charToColor 'b' = blue
charToColor _   = white
t4 = animate $ (switch magenta (key ==> charToColor)) &* el (p2 30 30) (p2 200 200)

-- Requires integral -- A moving ellipse
t5 = animate $ el (p2 0 0) ((p2 300 50) + (integral (p2 (-20) 10)))

-- Requires lbp, lbr, -=> and .|. -- turns red on left button press then green on release
t6 = animate $ (switch blue ((lbp -=> green) .|. (lbr -=> red))) &* el (p2 30 30) (p2 200 200)

-- Requires accum - types text on the screen
t7 = animate $ (accum "" (key ==> (\c -> (++ [c])))) @@ (p2 50 50)

-- Requires mouse - a moving circle
t8 = animate $ el (mouse - (p2 10 10)) (mouse + (p2 10 10))

-- Requires clock - the circle gets bigger each .5 seconds
t9 = animate $ el (p2 0 0) (accum (P2 0 0) (clock 0.5 -=> (+ (P2 10 10))))

-- Requires snap - each click leaves a circle
t10 = animate $ (accum (text (0,0) "") 
                       (snap lbp mouse ==>
                         (\(P2 x y) -> \g -> ellipse (round x - 10, round y - 10)
                                                     (round x + 10, round y + 10)
                                                `overGraphic`
                                             g)))
					     
t11 = animate $ (FRP.until red (key) charToColor) &* el (p2 30 30) (p2 200 200)

t12 = animate $ (FRP.until (el (p2 0 0) ((p2 300 50) + (integral (p2 (-20) 10)))) key 
	(\c -> case c of
		'a' -> (el (p2 0 0) ((p2 300 50) + (integral (p2 (20) 10))))
		'b' -> (el (p2 0 0) ((p2 300 50) + (integral (p2 20 (-10)))))
		'c' -> (el (p2 0 0) ((p2 300 50) + (integral (p2 (-20) (-10)))))
		_ -> (el (p2 0 0) ((p2 300 50) + (integral (p2 (-20) 10))))))
		
t13 = animate $ (until_ red (keyIs 'y') yellow) &* el (p2 30 30) (p2 200 200)

t14 = animate $ (switch green (((when $ (time >=* (lift0 1) &&* (time <=* (lift0 2)))) -=> blue) .|. ((when $ time >* (lift0 2)) -=> red))) &* el (p2 50 50) (p2 100 100) $$ (showB time @@ (p2 200 200))