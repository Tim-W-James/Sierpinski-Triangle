{-|
Module      : Turtle
Description : Defines functions that generate a list of commands to generate
              triangles, polygons, and sierpinski fractals, and a function
              which converts those commands into a Picture. Use cabal run.
Maintainer  : u6947396@anu.edu.au
-}
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

-- | Describes the current state of the turtle from TurtleCommands
data TurtleState =
  -- ^ Contains a Bool that is True when the pen is down, a Point
  -- representing the current position, and Radians representing
  -- the current rotation.
  TurtleState Bool Point Radians
  deriving (Eq, Show)

-- Task 1: Drawing Shapes

-- | Frequently used Radians constant for building triangles.
triRConst :: Radians
triRConst = 2 * pi/3

-- | triangle returns a list of functions needed to draw a triangle
-- with the side length inputted.
triangle :: Double -> [TurtleCommand]
triangle sdLength =
  PenDown : -- List should start with PenDown.
  Forward sdLength :
  Turn triRConst :
  Forward sdLength :
  Turn triRConst :
  Forward sdLength :
  Turn triRConst : -- End position and facing should equal start.
  PenUp : [] -- List should end with PenUp.

-- | myPolygon returns a list of commands needed to draw a polygon
-- with the number of sides and side length inputted.
-- Renamed from 'polygon' as to not conflict with the pre-defined
-- CodeWorld function.
myPolygon :: Int -> Double -> [TurtleCommand]
myPolygon sides sideLength
  | sides > 2 && sideLength >= 0 =
    myPolygonHelper (fromIntegral sides) sideLength sides []
  | sides < 3 =
    error "a polygon must have at least 3 sides"
  | otherwise =
    error "values may not be negative"

  where
    -- myPolygonHelper allows for recursive tracking of remainingSides and
    -- a carry to compound TurtleCommands.
    myPolygonHelper :: Double -> Double -> Int
      -> [TurtleCommand] -> [TurtleCommand]
    myPolygonHelper totalSides sdLength remainingSides carry = case carry of
      [] -> -- List should end with PenUp.
        myPolygonHelper totalSides sdLength remainingSides [PenUp]
      _
        | remainingSides == 0 -> -- List should start with PenDown.
          PenDown : carry
        | otherwise -> -- Forward and Turn commands fall between
                       -- PenDown and PenUp.
          myPolygonHelper totalSides sdLength (remainingSides - 1)
          (Forward sideLength :
          Turn (pi / 180 * (180 - 180 * (totalSides - 2) / totalSides)) :
          carry)

-- Task 2: Interpreting Turtle Commands
-- | Turtle starts facing north at (0,0)
initialState :: TurtleState
initialState = TurtleState False (0,0) 0

-- | Calculates a destination point given a distance traveling,
-- current point, and current direction facing.
posAfterMove :: Double -> Point -> Radians -> Point
posAfterMove dist (posX,posY) facing =
  (posX - dist * (sin facing),posY + dist * (cos facing))

-- | runTurtle constructs a Picture from a list of commands our Turtle
-- responds to, where a polyline is only drawn when pen is down.
runTurtle :: [TurtleCommand] -> Picture
runTurtle commandList = runTurtleHelper commandList initialState blank
  where
    -- runTurtleHelper allows for recursive tracking of TurtleState and
    -- a carry to compound each polyline that composes the Picture.
    runTurtleHelper :: [TurtleCommand] -> TurtleState -> Picture -> Picture
    runTurtleHelper cmdList (TurtleState isPenDown currPos currFacing) carry =
      case cmdList of
      [] -> carry
      x:xs -> case x of
        PenDown -> -- isPenDown is set to True to allow drawing.
          runTurtleHelper xs (TurtleState True currPos currFacing) carry

        Forward y
          | isPenDown == True -> -- If able to draw, move and
            -- add a polyline between start and finish points to the carry.
            runTurtleHelper xs (TurtleState isPenDown
            (posAfterMove y currPos currFacing) currFacing)
            (polyline [currPos,(posAfterMove y currPos currFacing)] & carry)

          | otherwise -> -- Otherwise, only move.
            runTurtleHelper xs (TurtleState isPenDown
            (posAfterMove y currPos currFacing) currFacing) carry

        Turn z -> -- Rotate by z in Radians.
          runTurtleHelper xs (TurtleState isPenDown
          currPos (z + currFacing)) carry

        _ -> -- isPenDown is set to False to not allow drawing.
          runTurtleHelper xs (TurtleState False currPos currFacing) carry

-- Task 3: Sierpinski's Triangle
--   COMP1100: Implement this directly (Task 3A)

-- | sierpinski returns a list of functions needed to
-- draw a sierpinski triangle fractal with the side
-- length and number of fractal levels inputted.
sierpinski :: Int -> Double -> [TurtleCommand]
sierpinski totalLvls sdLength
  | totalLvls < 0 || sdLength < 0 =
    error "values may not be negative"
  | totalLvls == 0 =
    []
  | otherwise = -- Builds each level individually and compounds them.
    sierpinski (totalLvls - 1) sdLength ++
    levelBuilder totalLvls totalLvls sdLength

  where
    -- levelBuilder returns a list of commands required for a single level,
    -- where all triangles have the same side length.
    levelBuilder :: Int -> Int -> Double -> [TurtleCommand]
    levelBuilder currLvl mstrLvl mstrLength
      | currLvl == 1 = -- At level 1, a triangle itself is constructed.
        triangle (sdLengthCalc mstrLvl mstrLength)
      | otherwise = -- Otherwise, each level of commands are
                    -- recursively compounded.
        triangleCompounder currLvl mstrLength
        (levelBuilder (currLvl - 1) mstrLvl mstrLength)

    -- triangleCompounder forms a triangle shape and executes a list
    -- of commands at each vertex facing north.
    triangleCompounder :: Int -> Double -> [TurtleCommand] -> [TurtleCommand]
    triangleCompounder baseLvl baseLength baseCmdLst =
      baseCmdLst ++
      [Forward (sdLengthCalc baseLvl baseLength)] ++
      baseCmdLst ++ [Turn triRConst] ++
      [Forward (sdLengthCalc baseLvl baseLength)] ++ [Turn (-triRConst)] ++
      baseCmdLst ++ [Turn (-triRConst)] ++
      [Forward (sdLengthCalc baseLvl baseLength)] ++ [Turn triRConst]

    -- Calculates a particular side length depending on the fractal level.
    sdLengthCalc :: Int -> Double -> Double
    sdLengthCalc lvl subLength = (subLength / 2^(lvl - 1))

-- Task 3B: L-Systems (COMP1130 Only)

lSystemCommands :: [TurtleCommand]
lSystemCommands = undefined -- COMP1130 Only

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
