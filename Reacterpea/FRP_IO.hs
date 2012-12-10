module FRP_IO where

import SOE
import Data.IORef

type Stimulus = (Double, Maybe SOEEvent)

data Behavior a = Behavior (Stimulus -> IO (Behavior a, a, Int))

data Event a = Event (Stimulus -> IO (Event a, Maybe a))

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
  where f = Behavior (const (return (f, v, 1)))

lift1 :: (a -> b) -> Behavior a -> Behavior b
lift1 fn (Behavior a0) = f a0
  where f a = Behavior (\s -> do (Behavior a', av, calls) <- a s
                                 return (f a', fn av, calls + 1)
                        )
                        
lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
lift2 fn (Behavior a0) (Behavior b0) = f a0 b0
  where f a b = Behavior (\s -> do (Behavior a', av, calls1) <- a s
                                   (Behavior b', bv, calls2) <- b s
                                   return (f a' b', fn av bv, calls1 + calls2 + 1)
                          )
                          
lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
lift3 fn (Behavior a0) (Behavior b0) (Behavior c0) = f a0 b0 c0
  where f a b c = Behavior (\s -> do (Behavior a', av, calls1) <- a s
                                     (Behavior b', bv, calls2) <- b s
                                     (Behavior c', cv, calls3) <- c s
                                     return (f a' b' c', fn av bv cv, calls1 + calls2 + calls3 + 1)
                            )

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

(+*) :: Num a => Behavior a -> Behavior a -> Behavior a
(+*) = lift2 (+)

(++*) :: BS -> BS -> BS
(++*) = lift2 (++)

(>*), (<*), (>=*), (<=*), (==*) :: BD -> BD -> BB
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
   f = Behavior (\(t, _) -> return (f, t, 1))

-- You can integrate Double and P2

deltaTime :: Double -> Behavior Double
--deltaTime = error "deltaTime not implemented"
deltaTime lastTime = f lastTime where
    f lT = Behavior (\ (t,_) -> return (f t, t - lT, 1))

integralStep :: Vec a => Double -> a -> Behavior a -> Behavior a
--integralStep = error "integralStep not implemented"
integralStep lT0 lV0 b0 = f lT0 lV0 b0 where
    f lT lV (Behavior fB) = Behavior (\ s@(t,_) -> do (b', a', calls) <- fB s
                                                      let dT = (t - lT)
                                                      let val = (lV + dT *^ a')
                                                      return (f t val b', lV, calls + 1)
                                      )

integral :: Vec a => Behavior a -> Behavior a
integral b = integralStep 0 0 b

integralSince :: Vec a => Behavior a -> Double -> Behavior a
integralSince b t = integralStep t 0 b

-- Find events in the stimulus stream.  This is a utility
-- to use for writing other GUI functionality

stimEvent :: (SOEEvent -> Maybe v) -> Event v
--stimEvent = error "stimEvent not implemented"
stimEvent fn0 = f fn0 where
    f fn = Event (\ s@(_,mSOEEvent) -> case mSOEEvent of
                                            Nothing         -> return (f fn, Nothing)
                                            (Just soeEvent) -> return (f fn, fn soeEvent)
                  )


lbp, lbr, rbp, rbr :: Event ()
--lbp = error "lbr not implemented"
lbp = f where
    f = Event (\ s@(t, mSOEEvent) -> case mSOEEvent of
                                          Nothing         -> return (f, Nothing)
                                          (Just soeEvent) -> case soeEvent of
                                                                  (Button {isLeft = True, isDown = True}) -> return (f, Just ())
                                                                  _                                       -> return (f, Nothing)
               )

--lbr = error "lbr not implemented"
lbr = f where
    f = Event (\ s@(t, mSOEEvent) -> case mSOEEvent of
                                          Nothing         -> return (f, Nothing)
                                          (Just soeEvent) -> case soeEvent of
                                                                  (Button {isLeft = True, isDown = False}) -> return (f, Just ())
                                                                  _                                        -> return (f, Nothing)
               )

--rbp = error "rbp not implemented"
rbp = f where
    f = Event (\ s@(t, mSOEEvent) -> case mSOEEvent of
                                          Nothing         -> return (f, Nothing)
                                          (Just soeEvent) -> case soeEvent of
                                                                  (Button {isLeft = False, isDown = True}) -> return (f, Just ())
                                                                  _                                        -> return (f, Nothing)
               )

--rbr = error "rbr not implemented"
rbr = f where
    f = Event (\ s@(t, mSOEEvent) -> case mSOEEvent of
                                          Nothing         -> return (f, Nothing)
                                          (Just soeEvent) -> case soeEvent of
                                                                  (Button {isLeft = False, isDown = False}) -> return (f, Just ())
                                                                  _                                         -> return(f, Nothing)
               )
-- This is an internal function to return the mouse movement event stream
-- Use hold to turn this into a behavior

mouseEv :: Event P2
--mouseEv = error "mouseEv not implemented"
mouseEv = f where
    f = Event (\ (_,mSOEEvent) -> case mSOEEvent of
                                       (Just (MouseMove { pt = (x,y) })) -> return (f, Just $ P2 (fromIntegral x) (fromIntegral y))
                                       _                                 -> return (f, Nothing)
               )

mouse :: Behavior P2
--mouse = error "mouse not implemented"
mouse = hold (P2 0 0) mouseEv

key :: Event Char
--key = error "key not implemented"
key = f where
    f = Event (\ (_,maybeSOEEvent) -> (case maybeSOEEvent of
                                           Nothing -> return (f, Nothing)
                                           _       -> (let (Just soeEvent) = maybeSOEEvent
                                                            in case soeEvent of
                                                                   (Key {char = c})  -> return (f, Just c)
                                                                   _                 -> return (f, Nothing)
                                                       )
                                       )
                 )

keyUp :: Event Char
keyUp = error "keyUp not implemented (SOE does not give the right events)"

keyIs :: Char -> Event ()
--keyIs = error "keyIs not implemented"
keyIs c0 = f c0 where
    f c = Event (\ (_,maybeSOEEvent) -> (case maybeSOEEvent of
                                           Nothing -> return (f c, Nothing)
                                           _       -> (let (Just soeEvent) = maybeSOEEvent in
                                                           case soeEvent of
                                                                (Key {char = c1 }) | c == c1 -> return (f c, Just ())
                                                                _                            -> return (f c, Nothing)
                                                       )
                                         )
                 )

-- Event stream combinators

never :: Event a
never = Event (const $ return (never, Nothing))

-- This internal function will allow you to build
-- most of the event functionality

mapEs :: st -> ((st, a) -> (st, Maybe b)) -> Event a -> Event b
mapEs = error "mapEs not implemented"

filterE :: Event a -> (a -> Bool) -> Event a
filterE e0 fltr = f e0 where
    f (Event fE) = Event (\s -> do (e', mA') <- fE s
                                   case mA' of
                                        Nothing  -> return (f e', Nothing)
                                        (Just a) -> if fltr a
                                                       then return (f e', Just a)
                                                       else return (f e', Nothing)
                          )

-- This suppresses events once the tag supply is exhausted

tags :: [a] -> Event b -> Event a
--tags = error "tags not implemented"
tags l0 e0 = f l0 e0 where
    f list (Event fE) = Event (\s -> case list of
                                          []   -> return (f [] never, Nothing)
                                          t:ts -> do (e', mB') <- fE s
                                                     case mB' of
                                                          Nothing  -> return (f list e', Nothing)
                                                          (Just _) -> return (f ts e', Just t)
                                  )

type EventScript a = [(Double, a)]
                                  
script :: EventScript a -> Event a
script list = f list where
    f [] = Event (\ _ -> return (never, Nothing))
    f list@((time,a):items) = Event (\(t,_) -> if t >= time
                                                  then return (f items, Just a)
                                                  else return (f list, Nothing)
                                    )


(==>) :: Event a -> (a -> b) -> Event b
--(==>) = error "==> not implemented"
(==>) event0 fn0 = f event0 fn0 where
      f (Event fE) fn = Event (\ s -> do (e', ma') <- fE s
                                         case ma' of
                                              Nothing  -> return (f e' fn, Nothing)
                                              (Just a) -> return (f e' fn, Just $ fn a)
                               )

when :: Behavior Bool -> Event ()
--when = error "when not implemented"
when b0 = f b0 where
    f (Behavior fB) = Event (\ s -> do (b', bool, calls) <- fB s
                                       case bool of
                                            True  -> return (f b', Just ())
                                            False -> return (f b', Nothing)
                             )

once :: Event a -> Event a
--once = error "once not implemented"
once e0 = f e0 where
    f (Event fE) = Event (\s -> do (e', mA') <- fE s
                                   case mA' of
                                        Nothing   -> return (f e', Nothing)
                                        (Just a') -> return (f never, Just a')
                             )

tick :: Double -> Double -> Event ()
--tick = error "tick not implemented"
tick dT0 nT0 = f dT0 nT0 where
    f dT nT = Event (\ (t,_) -> if t >= nT
                                   then return (f dT (t + dT), Just ())
                                   else return (f dT nT, Nothing)
                     )

clock :: Double -> Event ()
--clock = error "clock not implemented"
clock dT = tick dT 0

(-=>) :: Event a -> b -> Event b
--(-=>) = error "-=> not implemented"
(-=>) event0 b0 = f event0 b0 where
    f (Event fE) b = Event (\ s -> do (e', mA') <- fE s
                                      case mA' of
                                           Nothing   -> return (f e' b, Nothing)
                                           (Just a') -> return (f e' b, Just b)
                            )

(.|.) :: Event a -> Event a -> Event a
--(.|.) = error ".|. not implemented"
(.|.) event1 event2 = f event1 event2 where
    f (Event fE1) (Event fE2) = Event (\ s -> do (e1', mA1') <- fE1 s
                                                 (e2', mA2') <- fE2 s
                                                 case mA1' of
                                                     (Just a1') -> return (f e1' e2', Just a1')
                                                     Nothing -> case mA2' of
                                                                     (Just a2') -> return (f e1' e2', Just a2')
                                                                     Nothing    -> return (f e1' e2', Nothing)
                                        )
                                           
snap :: Event a -> Behavior b -> Event b
--snap = error "snap not implemented"
snap e0 b0 = f e0 b0 where
    f (Event fE) (Behavior fB) = Event (\s -> do (e', mA') <- fE s
                                                 (b', bVal, _) <- fB s
                                                 case mA' of
                                                      Nothing  -> return (f e' b', Nothing)
                                                      (Just _) -> return (f e' b', Just bVal)
                                        )

-- Switchers

rTrue, rFalse :: Behavior Bool
rTrue = lift0 True
rFalse = lift0 False

isOn :: Event a -> Event b -> Behavior Bool
isOn evOn evOff = switch rFalse ((evOn -=> rTrue) .|. (evOff -=> rFalse))

toggle :: Event a -> Behavior b -> Behavior b -> Behavior Bool
toggle = error "toggle not implemented"

switch :: Behavior a -> Event (Behavior a) -> Behavior a
--switch = error "switch not implemented"
switch b0 e0 = f b0 e0 where
     f (Behavior fB) (Event fE) = Behavior (\ s -> do (b', a', calls) <- fB s
                                                      (e', mB) <- fE s
                                                      case mB of
                                                           Nothing                 -> return (f b' e', a', calls + 1)
                                                           (Just (Behavior fBNew)) -> do (bNew', aNew', calls2) <- fBNew s
                                                                                         return (f bNew' e', aNew', calls + calls2 +1)
                                            )

until :: Behavior a -> Event (IO (Behavior a)) -> Behavior a
--until = error "until not implemented"
until b0 e0 = f b0 e0 where
    f (Behavior fB) (Event fE) = Behavior (\s -> do (e', mIOB) <- fE s
                                                    case mIOB of
                                                         Nothing    -> do (b', a', calls) <- fB s
                                                                          return (f b' e', a', calls + 1)
                                                         (Just ioB) -> do (Behavior fBnew) <- ioB
                                                                          (b', a', calls) <- fBnew s
                                                                          return (f b' never, a', calls + 1)
                                            )

until_ :: Behavior a -> Event b -> Behavior a -> Behavior a
until_ = error "until_ not implemented"

accum :: a -> Event (a -> IO a) -> Behavior a
--accum = error "accum not implemented"
accum a0 event0 = f a0 event0 where
    f a (Event fE) = Behavior (\ s -> do (e', mFn') <- fE s
                                         case mFn' of
                                              Nothing    -> return (f a e', a, 1)
                                              (Just fn') -> do aNew <- fn' a
                                                               return (f aNew e', aNew, 1)
                               )

hold :: a -> Event a -> Behavior a
--hold a (Event fn) = error "hold not implemented"
hold a0 event0 = f a0 event0 where
     f a (Event eventFn) = Behavior (\ s -> do (event', maybeA) <- eventFn s
                                               case maybeA of
                                                    Nothing   -> return (f a event', a, 1)
                                                    (Just a') -> return (f a' event', a', 1)
                                       )

-- Graphics operations

el :: BP2 -> BP2 -> BG
el = lift2 (\(P2 x1 y1) (P2 x2 y2) -> ellipse (round x1, round y1) (round x2, round y2))

li :: BP2 -> BP2 -> BG
li = lift2 (\(P2 x1 y1) (P2 x2 y2) -> line (round x1, round y1) (round x2, round y2))

($$) :: BG -> BG -> BG
($$) = lift2 overGraphic

(@@) :: BS -> BP2 -> BG -- Text
(@@) = lift2 (\txt (P2 x y) -> text (round x, round y) txt)

(&*) :: BC -> BG -> BG
(&*) = lift2 withColor

traceB :: Show a => String -> Behavior a -> Behavior a
   -- Print a signal/time, prefixed by the string
--traceB = error "traceB not implemented"
traceB str b = f b where
    f (Behavior fB) = Behavior (\s -> do (b', a', calls) <- fB s
                                         putStrLn (str ++ (show a'))
                                         return (f b', a', calls + 1)
                                )
 
--traceB str0 b = f b (printPart str0 "blah") where
--    printPart str1 str2 = ((lift0 str1) ++* (lift0 str2)) @@ (p2 50 50)
--    f (Behavior fB) (Behavior fPrint) = Behavior (\s -> do (b',a') <- fB s
--                                                           (bP',_) <- fPrint s
--                                                           return (f b' (printPart str0 (show a')),a')
--                                                  )

traceE :: Show a => String -> Event a -> Event a
   -- Print event value when it occurs.
--traceE = error "traceE not implemented"
traceE str e = f e where
    f (Event fE) = Event (\s -> do (e', mA') <- fE s
                                   case mA' of
                                        (Just a') -> do putStrLn (str ++ (show a'))
                                                        return (f e', Just a')
                                        Nothing   -> return (f e', Nothing)
                                )

-- Create an observing behavior.  Use an IORef to hold the observed
-- value.  Make the built-in mouse function an observation.
observe :: Behavior a -> IO (Behavior a)
--observe = error "observe not implemented"
observe (Behavior fB0) = do ref <- newIORef (-1, error "Time cannot be -1!")
                            let stepF fB = Behavior (\s@(t,_) -> do (rT, rV) <- readIORef ref
                                                                    if (rT == t)
                                                                       then return (stepF fB, rV, 1)
                                                                       else do (Behavior fB', v', calls) <- fB s
                                                                               writeIORef ref (t, v')
                                                                               return (stepF fB', v', calls)
                                                     )
                            return $ stepF fB0

animate :: IO BG -> IO ()
animate ioB
   = runGraphics $ do
                                     -- Window location and size
        (Behavior b) <- ioB
        w <- openWindowEx "FRP Test" (Just (0,0)) (Just (700, 700))
               drawBufferedGraphic
        t0 <- timeGetTime
        ws <- getWindowSizeGL w
        let 
          run b e = do
            t <- timeGetTime
            let ft = intToDouble (word32ToInt (t-t0)) / 1000
            (Behavior b1, g, calls) <- b (ft, e)
--            putStrLn ("Steps: " ++ (show calls))
            setGraphic w g
            case e of
              Nothing -> l b1
              Just Closed -> closeWindow w
              _ -> do putStrLn (show e)
                      l b1

          l b = do
            e <- maybeGetWindowSOEEvent w
            run b e     
        run b (Just $ Resize ws)

mp = integral (p2 20 20)

-- This should take 17 steps / tick
t1 = animate $ return $ el (mp - p2 10 10) (mp + p2 10 10)

t1b = animate $ return $ showB time @@ (p2 50 50)

-- This should take 13 steps / tick
t2 = animate $ do mp1 <- observe mp
                  return $ el (mp1 - p2 10 10) (mp1 + p2 10 10)
                  
t3 = animate $ return $ traceB "Graphic" $ el (mp - p2 10 10) (mp + p2 10 10)

-- You should see the lbp events in the trace output
t4 = animate $ return $ (red `FRP_IO.until` (traceE "lbp" lbp -=> return blue)) &* 
                        el (p2 10 10) (p2 100 100) 

scale v p = p2 (v*getX p) (v*getY p)
pos :: BP2
pos = p2 200 200 + integral (scale 0.4 (mouse - p2 300 300))

ship p = green &* el (p + p2 20 20) (p - p2 20 20)

mis p0 = let p1 = lift0 p0 + integral (p2 100 0) in
               blue &* el (p1 + p2 10 10) (p1 - p2 10 10)

t5 = animate $
   do pos1 <- observe pos
      return (ship pos1 `FRP_IO.until` 
               (lbp `snap` pos1 ==>
                        (\p0 -> return $ ship pos1 $$ mis p0)))
                        
-- GAME ________________________________________________________________________

gravity = p2 0 900

-- BULLETS

bulletLVel = p2 (-300) 0
bulletRVel = p2 300 0

resetBullet :: P2 -> Event () -> BP2 -> Event(BP2)
resetBullet (P2 x0 y0) e0 bulVel0 = f e0 where
    f (Event fE) = Event (\s@(t,_) -> do (e', mV') <- fE s
                                         case mV' of
                                              Nothing  -> return (f e', Nothing)
                                              (Just _) -> return (f e', Just $ (p2 (lift0 x0) (lift0 y0)) + (integralSince bulletLVel t))
                          )

bulletL :: P2 -> BP2
--bulletL = error "bulletL not implented"
bulletL (P2 x0 y0) = (p2 (lift0 x0) (lift0 y0)) + (integral bulletLVel)
--bulletL (P2 x0 y0) = let start = (p2 (lift0 x0) (lift0 y0)) + (integral bulletLVel)
--                         shouldRestart (P2 x1 y1) = x1 < 0 in
--    switch start (  )

bullets :: [BP2]
bullets = [
            bulletL (P2 700 630)
          , bulletL (P2 700 530)
          ]

drawBullet :: BP2 -> BG
drawBullet bulPos = el (bulPos - (p2 10 10)) (bulPos + (p2 10 10))
    
drawBullets :: BG
drawBullets = foldl1 ($$) (map drawBullet bullets)

-- PLATFORMS

data Platform = Platform P2 P2

platforms :: [Platform]
--platforms = error "platforms not implemented"
platforms = [
              Platform (P2 10 30) (P2 690 30)
            , Platform (P2 10 70) (P2 690 70)
            , Platform (P2 10 440) (P2 690 440)
            , Platform (P2 10 510) (P2 690 510)
            , Platform (P2 10 580) (P2 690 580)
            , Platform (P2 10 650) (P2 690 650)
            ]

drawPlatform :: Platform -> BG
--drawPlatform = error "drawPlatform not implemented"
drawPlatform (Platform (P2 x1 y1) (P2 x2 y2)) = li (p2 (lift0 x1) (lift0 y1)) (p2 (lift0 x2) (lift0 y2))

drawPlatforms :: BG
--drawPlatforms = error "drawPlatforms not implemented"
drawPlatforms = foldl1 ($$) (map drawPlatform platforms)

isOnPlatform :: P2 -> Platform -> Bool
--isOnPlatform = error "isOnPlatform not implemented"
isOnPlatform (P2 x y) 
     (Platform (P2 x1 y1) (P2 x2 y2)) = (x > (min x1 x2)) && (x < (max x1 x2)) && (y > y1) && (y < y1 + 5)

isOnSomePlatform :: P2 -> Bool
--isOnSomePlatform = error "isOnSomePlatform not implemented"
isOnSomePlatform point = foldl1 (||) (map (isOnPlatform point) platforms)

stopAtPlatform :: P2 -> P2 -> P2 -- takes position and velocity and returns new velocity
--stopAtPlatform = error "stopAtPlatform not implemented"
stopAtPlatform pos (vel@(P2 vX vY)) = if (vY > 0) && (isOnSomePlatform pos)
                                         then (P2 vX 0)
                                         else vel

-- JUMPING
jumpVelocity = (p2 0 (-400))

jumpValue :: Double -> Behavior P2 -- takes current time
jumpValue t = ((integralSince gravity t) + jumpVelocity)

jumpEvent :: Event (Behavior P2)
jumpEvent = f (keyIs ' ') where
    f (Event fE) = Event (\ s@(t,_) -> do (e', mKey) <- fE s
                                          case mKey of
                                               Nothing -> return (f e', Nothing)
                                               _       -> return (f e', Just $ (integralSince gravity t) + jumpVelocity)
                          )

-- MOVING LEFT and RIGHT

moveLeft :: Behavior P2
moveLeft = switch (p2 0 0) ( (lbp -=> (p2 (-250) 0)) .|. (lbr -=> (p2 0 0)) )

moveRight :: Behavior P2
moveRight = switch (p2 0 0) ( (rbp -=> (p2 250 0)) .|. (rbr -=> (p2 0 0)) )

-- PLAYER VELOCITY
--playerVy = (lift2 stopAtPlatform) playerP (switch (integral gravity) jumpEvent)
playerVy = switch (integral gravity) jumpEvent
playerVx = moveLeft + moveRight
playerV = playerVx + playerVy

-- PLAYER POSITION
playerP0 = p2 50 600
playerP = playerP0 + (integral playerV)

playerHeight = 20
playerWidth = 20
playerHalfWidth = playerWidth/2
player p = green &* el (p + p2 playerHalfWidth 0) (p - p2 playerHalfWidth playerHeight)

game = animate $ 
     do playerP1 <- observe playerP
        return $ (player playerP1) $$ drawPlatforms $$ drawBullets