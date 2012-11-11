module SOE (
  runGraphics,
  Title,
  Size,
  Window,
  openWindow,
  getWindowSize,
  clearWindow,
  drawInWindow,
  drawInWindowNow,
  setGraphic,
  closeWindow,
  openWindowEx,
  RedrawMode,
  drawGraphic,
  drawBufferedGraphic,
  Graphic,
  emptyGraphic,
  overGraphic ,
  overGraphics,
  Color (..),
  withColor,
  text,
  Point,
  ellipse,
  line,
  getKey,
  getLBP,
  getRBP,
  SOEEvent (..),
  maybeGetWindowSOEEvent,
  getWindowSOEEvent,
  Word32,
  timeGetTime,
  word32ToInt
  ) where

import Data.Ix (Ix)
import Data.Word (Word32)
import Control.Concurrent
import qualified System.Time
import qualified Graphics.UI.GLFW as GLFW
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.Rendering.OpenGL (($=), GLfloat)
import System.IO.Unsafe


-------------------
-- Window Functions
-------------------

runGraphics :: IO () -> IO ()
runGraphics main = main

type Title = String
type Size = (Int, Int)

data Window = Window {
  graphicVar :: MVar (Graphic, Bool), -- boolean to remember if it's dirty
  eventsChan :: Chan SOEEvent
}

-- Graphic is just a wrapper for OpenGL IO
data Graphic = Graphic String (IO ())
instance Show Graphic where
  show (Graphic s _) = s     

initialized, opened :: MVar Bool
initialized = unsafePerformIO (newMVar False)
opened = unsafePerformIO (newMVar False)

initialize = do
  init <- readMVar initialized
  if init then return ()
    else do
      GLFW.initialize
      modifyMVar_ initialized (\_ -> return True)
      return ()

openWindow :: Title -> Size -> IO Window
openWindow title size =
  openWindowEx title Nothing (Just size) drawBufferedGraphic

-- pos is always ignored due to GLFW
openWindowEx :: Title -> Maybe Point -> Maybe Size -> RedrawMode -> IO Window
openWindowEx title position size (RedrawMode useDoubleBuffer) = do
  let siz = maybe (GL.Size 400 300) fromSize size
  initialize
  graphicVar <- newMVar (emptyGraphic, False)
  eventsChan <- newChan
  GLFW.openWindow siz [GLFW.DisplayStencilBits 8, GLFW.DisplayAlphaBits 8] GLFW.Window
  GLFW.windowTitle $= title
  modifyMVar_ opened (\_ -> return True)
  GL.shadeModel $= GL.Smooth
  -- enable antialiasing
  GL.lineSmooth $= GL.Enabled
  GL.blend $= GL.Enabled
  GL.blendFunc $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
  GL.lineWidth $= 1.5

  -- this will hang on Windows
  -- let updateWindow = readMVar graphicVar >>= (\(Graphic g) -> g >> GLFW.swapBuffers)
  -- GLFW.windowRefreshCallback $= updateWindow

  let motionCallback (GL.Position x y) =
        writeChan eventsChan MouseMove { pt = (fromIntegral x, fromIntegral y) }
  GLFW.mousePosCallback $= motionCallback
      
  GLFW.charCallback $= (\char state -> do
    writeChan eventsChan (Key {
        char = char,
        isDown = (state == GLFW.Press) }))

  GLFW.mouseButtonCallback $= (\but state -> do
    GL.Position x y <- GL.get GLFW.mousePos
    writeChan eventsChan (Button {
        pt = (fromIntegral x, fromIntegral y),
        isLeft = (but == GLFW.ButtonLeft),
        isDown = (state == GLFW.Press) }))

  GLFW.windowSizeCallback $= writeChan eventsChan . Resize
  GLFW.windowRefreshCallback $= writeChan eventsChan Refresh
  GLFW.windowCloseCallback $= (closeWindow_ eventsChan >> return True)

  return Window {
    graphicVar = graphicVar,
    eventsChan = eventsChan
  }

getWindowSize :: Window -> IO Size
getWindowSize win = do
  (GL.Size x y) <- GL.get GLFW.windowSize
  return (fromIntegral x, fromIntegral y)

clearWindow :: Window -> IO ()
clearWindow win = setGraphic win (Graphic "" (return ()))

drawInWindow :: Window -> Graphic -> IO ()
drawInWindow win graphic = 
  modifyMVar_ (graphicVar win) (\ (g, _) -> 
    return (overGraphic graphic g, True)) 

-- if window is marked as dirty, mark it clean, draw and swap buffer;
-- otherwise do nothing.
updateWindowIfDirty win = do
   io <- modifyMVar (graphicVar win) (\ (g@(Graphic _ io), dirty) -> do
     return ((g, False), if dirty then io >> GLFW.swapBuffers
                                  else return ()))
   io

drawInWindowNow :: Window -> Graphic -> IO ()
drawInWindowNow win graphic = do
  drawInWindow win graphic
  updateWindowIfDirty win

-- setGraphic set the given Graphic over empty (black) background for
-- display in current Window.
setGraphic :: Window -> Graphic -> IO ()
setGraphic win graphic = do
  modifyMVar_ (graphicVar win) (\_ -> 
    return (overGraphic graphic emptyGraphic, True))

closeWindow :: Window -> IO ()
closeWindow win = closeWindow_ (eventsChan win)

closeWindow_ chan = do
  writeChan chan Closed
  modifyMVar_ opened (\_ -> return False)
  GLFW.closeWindow
  GLFW.pollEvents

--------------------
-- Drawing Functions
--------------------

newtype RedrawMode = RedrawMode Bool

drawGraphic :: RedrawMode
drawGraphic = RedrawMode False

drawBufferedGraphic :: RedrawMode
drawBufferedGraphic = RedrawMode True

data Color = Black
           | Blue
           | Green
           | Cyan
           | Red
           | Magenta
           | Yellow
           | White
  deriving (Eq, Ord, Bounded, Enum, Ix, Show, Read)

type Angle = GLfloat

emptyGraphic :: Graphic
emptyGraphic = Graphic "empty" $ do 
  GL.clearColor $= GL.Color4 0 0 0 0
  GL.clear [GL.ColorBuffer, GL.StencilBuffer]

overGraphic :: Graphic -> Graphic -> Graphic
overGraphic (Graphic o over) (Graphic b base) = Graphic ("(" ++ o ++ " over " ++ b ++ ")")
                                                        (base >> over)

overGraphics :: [Graphic] -> Graphic
overGraphics = foldl1 overGraphic

colorToRGB :: Color -> GL.Color3 GLfloat
colorToRGB Black   = GL.Color3 0 0 0
colorToRGB Blue    = GL.Color3 0 0 1
colorToRGB Green   = GL.Color3 0 1 0
colorToRGB Cyan    = GL.Color3 0 1 1
colorToRGB Red     = GL.Color3 1 0 0
colorToRGB Magenta = GL.Color3 1 0 1
colorToRGB Yellow  = GL.Color3 1 1 0
colorToRGB White   = GL.Color3 1 1 1

withColor :: Color -> Graphic -> Graphic
withColor color (Graphic g' g) = Graphic ("(withColor " ++ show color ++ " " ++ g' ++ ")")
                                         (GL.color (colorToRGB color) >> g)

text :: Point -> String -> Graphic
text (x,y) str = Graphic ("Text " ++ str ++ " at " ++ show (x,y)) $ GL.preservingMatrix $ do
  GL.translate (GL.Vector3 (fromIntegral x) (fromIntegral y + 16) (0::GLfloat))
  GL.scale 1 (-1) (1::GLfloat)
  GLFW.renderString GLFW.Fixed8x16 str

type Point = (Int, Int)

ellipse :: Point -> Point -> Graphic
ellipse pt1 pt2 = Graphic ("Ellipse at " ++ show pt1 ++ " " ++ show pt2) $
                          GL.preservingMatrix $ do
  let (x, y, width, height) = normaliseBounds pt1 pt2
      (r1, r2) = (width / 2, height / 2)
  GL.translate (GL.Vector3 (x + r1) (y + r2) 0)
  GL.renderPrimitive GL.Polygon (circle r1 r2 0 (2 * pi) (20 / (r1 + r2)))
      
line :: Point -> Point -> Graphic
line (x1, y1) (x2, y2) = Graphic ("line " ++ show (x1, y1) ++ " " ++ show (x2, y2) ++ ")") $ 
  GL.renderPrimitive GL.LineStrip (do
    GL.vertex (vertex3 (fromIntegral x1) (fromIntegral y1) 0)
    GL.vertex (vertex3 (fromIntegral x2) (fromIntegral y2) 0))

---------------------------
-- Event Handling Functions
---------------------------

data SOEEvent = Key {
               char :: Char,
               isDown :: Bool
             }
           | Button {
              pt :: Point,
              isLeft :: Bool,
              isDown :: Bool
             }
           | MouseMove {
               pt :: Point
             }
           | Resize GL.Size
           | Refresh
           | Closed
  deriving Show

getWindowSOEEvent :: Window -> IO SOEEvent
getWindowSOEEvent win = do
  event <- maybeGetWindowSOEEvent win
  maybe (getWindowSOEEvent win) return event

maybeGetWindowSOEEvent :: Window -> IO (Maybe SOEEvent)
maybeGetWindowSOEEvent win = do
  updateWindowIfDirty win
  noEvents <- isEmptyChan (eventsChan win)
  if noEvents 
    then GLFW.sleep 0.01 >> GLFW.pollEvents >> return Nothing
    else do
      event <- readChan (eventsChan win)
      case event of
        Refresh -> do
          (Graphic _ io, _) <- readMVar (graphicVar win)
          io
          GLFW.swapBuffers
          maybeGetWindowSOEEvent win
        Resize size@(GL.Size w h) -> do
          GL.viewport $= (GL.Position 0 0, size)
          GL.matrixMode $= GL.Projection
          GL.loadIdentity
          GL.ortho2D 0 (realToFrac w) (realToFrac h) 0
	  -- force a refresh, needed for OS X
	  writeChan (eventsChan win) Refresh
          maybeGetWindowSOEEvent win
        e -> return (Just e)

getKeyEx :: Window -> Bool -> IO Char
getKeyEx win down = loop
  where loop = do e <- getWindowSOEEvent win
                  case e of
                    (Key { char = ch, isDown = d })
                      | d == down -> return ch
                    Closed -> return '\x0'
                    _ -> loop

getKey :: Window -> IO Char
getKey win = do
  ch <- getKeyEx win True
  if ch == '\x0' then return ch
    else getKeyEx win False

getButton :: Window -> Int -> Bool -> IO Point
getButton win but down = loop
  where loop = do e <- getWindowSOEEvent win
                  case e of
                    (Button { pt = pt, isDown = id })
                      | id == down -> return pt
                    _ -> loop

getLBP :: Window -> IO Point
getLBP w = getButton w 1 True

getRBP :: Window -> IO Point
getRBP w = getButton w 2 True

-- use GLFW's high resolution timer
timeGetTime :: IO Word32
timeGetTime = do
  timeInSec <- GL.get GLFW.time
  return $ round $ timeInSec * 1000

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

----------------------
-- Auxiliary Functions
----------------------

vertex4 :: GLfloat -> GLfloat -> GLfloat -> GLfloat -> GL.Vertex4 GLfloat
vertex4 = GL.Vertex4

vertex3 :: GLfloat -> GLfloat -> GLfloat -> GL.Vertex3 GLfloat
vertex3 = GL.Vertex3

normaliseBounds :: Point -> Point -> (GLfloat,GLfloat,GLfloat,GLfloat)
normaliseBounds (x1,y1) (x2,y2) = (x, y, width, height)
  where x = fromIntegral $ min x1 x2
        y = fromIntegral $ min y1 y2
        width  = fromIntegral $ abs $ x1 - x2
        height = fromIntegral $ abs $ y1 - y2

normaliseBounds' :: Point -> Point -> (Int,Int,Int,Int)
normaliseBounds' (x1,y1) (x2,y2) = (x, y, width, height)
  where x = min x1 x2
        y = min y1 y2
        width  = abs $ x1 - x2
        height = abs $ y1 - y2

fromPoint :: Point -> (GLfloat, GLfloat)
fromPoint (x1, x2) = (fromIntegral x1, fromIntegral x2)

fromSize (x, y) = GL.Size (fromIntegral x) (fromIntegral y)

-- we add 20 pixels to the y position to leave space for window title bar
fromPosition (x, y) = GL.Position (fromIntegral x) (20 + fromIntegral y)

circle r1 r2 start stop step =
  let vs = [ (r1 * cos i, r2 * sin i) | i <- segment start stop step ]
  in mapM_ (\(x, y) -> GL.vertex (vertex3 x y 0)) vs

segment start stop step = ts start
  where ts i = if i >= stop then [stop] else (i : ts (i + step))