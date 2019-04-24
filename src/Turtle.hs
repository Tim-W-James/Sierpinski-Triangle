module Turtle where

import CodeWorld

type Radians = Double

-- | The commands that we can send to our turtle.
data TurtleCommand
  = Forward Double -- ^ Drive forward the given number of units,
                   -- drawing if the pen is down.
  | Turn Radians -- ^ Turn the turtle. Positive values are
                 -- anticlockwise; negative values are clockwise.
  | PenUp -- ^ Lift the pen, so that future 'Forward' instructions do
          -- not draw.
  | PenDown -- ^ Lower the pen, so that future 'Forward' instructions
            -- will draw.
  deriving (Eq, Show)

data TurtleState = TurtleState Bool Point Radians

-- Task 1: Drawing Shapes

-- | triangle returns a list of functions needed to draw a triangle
-- with the side length inputted.
triangle :: Double -> [TurtleCommand]
triangle n =
  PenDown :
  Forward n :
  Turn (2 * pi/3) :
  Forward n :
  Turn (2 * pi/3) :
  Forward n :
  Turn (2 * pi/3) :
  PenUp : []

-- | polygon' returns a list of commands needed to draw a polygon
-- with the number of sides and side length inputted.
myPolygon :: Int -> Double -> [TurtleCommand]
myPolygon totalSides sideLength
  | totalSides > 2 =
    polygonHelper (fromIntegral totalSides) sideLength totalSides []
  | otherwise =
    error "a polygon must have at least 3 sides"
  where
    polygonHelper :: Double -> Double -> Int -> [TurtleCommand] -> [TurtleCommand]
    polygonHelper totalSides sideLength remainingSides carry = case carry of
      [] ->
        polygonHelper totalSides sideLength remainingSides [PenUp]
      _
        | remainingSides == 0 ->
          PenDown : carry
        | otherwise ->
          polygonHelper totalSides sideLength (remainingSides - 1)
          (Forward sideLength :
          Turn (pi / 180 * (180 - 180 * (totalSides - 2) / totalSides)) :
          carry)

-- Task 2: Interpreting Turtle Commands

runTurtle :: [TurtleCommand] -> Picture
runTurtle commandList = runTurtleHelper commandList (TurtleState False (0,0) 0) blank
  where
    runTurtleHelper :: [TurtleCommand] -> TurtleState -> Picture -> Picture
    runTurtleHelper commandList (TurtleState isPenDown (currentPosX,currentPosY) currentFacing) carry = case commandList of
      [] -> carry
      x:xs -> case x of
        PenDown ->
          runTurtleHelper xs (TurtleState True (currentPosX,currentPosY) currentFacing) carry
        Forward y
          | isPenDown == True -> runTurtleHelper xs (TurtleState isPenDown (currentPosX - y * (sin currentFacing),currentPosY + y * (cos currentFacing)) currentFacing) (polyline [(currentPosX,currentPosY),(currentPosX - y * (sin currentFacing),currentPosY + y * (cos currentFacing))] & carry)
          | otherwise -> runTurtleHelper xs (TurtleState isPenDown (currentPosX - y * (sin currentFacing),currentPosY + y * (cos currentFacing)) currentFacing) carry
        Turn y ->
          runTurtleHelper xs (TurtleState isPenDown (currentPosX,currentPosY) (y + currentFacing)) carry
        _ ->
          runTurtleHelper xs (TurtleState False (currentPosX,currentPosY) currentFacing) carry

-- Task 3: Sierpinski's Triangle
--   COMP1100: Implement this directly (Task 3A)
--   COMP1130: Implement this using an L-System (Task 3B)

sierpinski :: Int -> Double -> [TurtleCommand]
sierpinski levels sideLength = case levels of
  0 -> blank
  n -> triangle sideLength

-- Task 3B: L-Systems (COMP1130 Only)

lSystemCommands :: [TurtleCommand]
lSystemCommands = undefined -- TODO



-- | A more complex example to test your interpreter.
comp1100 :: [TurtleCommand]
comp1100 = concat [start, c, o, m, p, one, one, o, o]
  where
    start = [PenUp, f 1.5, l, f 9.25, r]
    c =
      [ r, f 1.5
      , l, f 0.5, PenDown
      , l, l', f d
      , r', f 1
      , r', f d
      , r', f 2
      , r', f d
      , r', f 1
      , r', f d, PenUp
      , r', f 2.5
      , l, f 1
      , l
      ]
    o =
      [ r, f 1.5
      , l, f 0.5, PenDown
      , l, l', f d
      , r', f 1
      , r', f d
      , r', f 2
      , r', f d
      , r', f 1
      , r', f d
      , r', f 2, PenUp
      , f 0.5
      , l, f 1
      , l
      ]
    m =
      [ l, f 0.5, r, PenDown
      , f 3
      , r, r', f (d * 2)
      , l, f (d * 2)
      , r, r', f 3, PenUp
      , l, f 1, l
      ]
    p =
      [ l, f 0.5, r, PenDown
      , f 2.5
      , r', f d
      , r', f 1
      , r', f d
      , r', f 1
      , r', f d
      , r', f 1
      , r', f d, PenUp
      , r, r', f 3
      , r, f 1.5
      , l, l
      ]
    one =
      [ PenDown
      , r, f 1
      , l, l, f 0.5
      , r, f 3
      , l, l', f d, PenUp
      , f d
      , l', f 2
      , l, f 2.5
      , l
      ]

    f = Forward

    -- Left/Right turns, 90 degrees. Primed versions (the ones with an
    -- ' after: l', r') are 45 degrees.
    l = Turn (pi / 2)
    l' = Turn (pi / 4)
    r = Turn (-pi / 2)
    r' = Turn (-pi / 4)

    -- Diagonal length of a right-angle triangle with both sides 0.5
    d = sqrt (2 * 0.5 * 0.5)