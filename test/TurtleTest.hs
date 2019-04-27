{-|
Module      : TurtleTest
Description : Defines tests for functions defined in Turtle.hs.
Maintainer  : u6947396@anu.edu.au
-}
module Main where

import Turtle
import Testing
import CodeWorld

-- The following functions are used exclusively for testing.
-- These functions (excluding totalDist) also follow a similar structure to
-- Turtle.runTurtle, a function which cannot be tested due to the
-- nature of the CodeWorld Picture type.

-- | Calculates the total distance a turtle draws from a list of commands.
totalDist :: [TurtleCommand] -> Double
totalDist commandList = totalDistHelper commandList False
  where
    -- The current state of whether the pen is down must be tracked.
    totalDistHelper :: [TurtleCommand] -> Bool -> Double
    totalDistHelper cmdList isPenDown = case cmdList of
      [] -> 0
      x:xs -> case x of
        Forward y
          | isPenDown == True -> y + totalDistHelper xs isPenDown
          | otherwise -> totalDistHelper xs isPenDown
        PenDown -> totalDistHelper xs True
        PenUp -> totalDistHelper xs False
        _ -> totalDistHelper xs isPenDown

-- | Calculates the final state of the pen from a list of commands.
finalIsPenDown :: [TurtleCommand] -> Bool
finalIsPenDown commandList = finalIsPenDownHelper commandList False
  where
    finalIsPenDownHelper :: [TurtleCommand] -> Bool -> Bool
    finalIsPenDownHelper cmdList isPenDown = case cmdList of
      [] -> isPenDown
      x:xs
        | x == PenDown -> finalIsPenDownHelper xs True
        | x == PenUp -> finalIsPenDownHelper xs False
        | otherwise -> finalIsPenDownHelper xs isPenDown

-- | Calculates the final position (Point) a turtle is in after a list
-- of commands are executed.
finalPos :: [TurtleCommand] -> Point
finalPos commandList = finalPosHelper commandList initialState
  where
    -- finalPosHelper allows for recursive tracking of the current position.
    finalPosHelper :: [TurtleCommand] -> TurtleState -> Point
    finalPosHelper cmdList (TurtleState isPenDown currPos currFacing) =
      case cmdList of
      [] -> currPos
      x:xs -> case x of
        Forward y -> -- Move by y.
          finalPosHelper xs (TurtleState isPenDown
          (posAfterMove y currPos currFacing) currFacing)

        Turn z -> -- Rotate by z in Radians.
          finalPosHelper xs (TurtleState isPenDown
          currPos (z + currFacing))

        _ -> -- PenDown and PenUp have no effect on position.
          finalPosHelper xs (TurtleState isPenDown currPos currFacing)

-- | Calculates the final direction facing (Radians) a turtle is in after
-- a list of commands are executed.
finalFacing :: [TurtleCommand] -> Radians
finalFacing commandList = case commandList of
  [] -> 0
  x:xs -> case x of
    Turn y -> y + finalFacing xs
    _ -> finalFacing xs

-- | Calculates the complete final state a turtle is in after
-- a list of commands are executed.
finalState :: [TurtleCommand] -> TurtleState
finalState commandList =
  TurtleState
  (finalIsPenDown commandList)
  (finalPos commandList)
  (finalFacing commandList)

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked.
tests :: [Test]
tests =
  [ totalDistTest
  , finalIsPenDownTestA
  , finalIsPenDownTestB
  , finalPosTest
  , finalFacingTest
  , finalStateTest
  , triangleTestA
  , triangleTestB
  , triangleTestC
  , triPolyEqTest
  , myPolygonTestA
  , myPolygonTestB
  , myPolygonTestC
  , sierpinskiTestA
  , sierpinskiTestB
  , sierpinskiTestC
  , sierpinskiTriEqTest
  ]

-- The following tests are to verify functions used in testing
-- work as intended.

-- | Basic verification of totalDist,
-- a function used in testing.
totalDistTest :: Test
totalDistTest =
  Test "totalDist [PenDown, Forward 5, Forward 10, PenUp, Forward 1] == 15"
  (assertEqual (totalDist [PenDown, Forward 5, Forward 10,
  PenUp, Forward 1]) (15))

-- | Basic verification of finalIsPenDown,
-- a function used in testing.
finalIsPenDownTestA :: Test
finalIsPenDownTestA =
  Test "finalIsPenDown [PenUp, PenDown] == True"
  (assertEqual (finalIsPenDown
  [PenUp, PenDown]) (True))

finalIsPenDownTestB :: Test
finalIsPenDownTestB =
  Test "finalIsPenDown [PenDown, Forward 1, PenUp] == False"
  (assertEqual (finalIsPenDown
  [PenDown, Forward 1, PenUp]) (False))

-- | Basic verification of finalPos,
-- a function used in testing.
finalPosTest :: Test
finalPosTest =
  Test "finalPos [Turn (pi/2), Forward 1, Turn pi/2, Forward 2] == ((-1),(-2))"
  (assertPointApproxEqual (finalPos
  [Turn (pi/2), Forward 1, Turn (pi/2), Forward 2]) ((-1),(-2)))

-- | Basic verification of finalFacing,
-- a function used in testing.
finalFacingTest :: Test
finalFacingTest =
  Test "finalFacing [Turn pi, Forward 1, Turn (-pi/2)] == pi/2"
  (assertEqual (finalFacing
  [Turn pi, Forward 1, Turn (-pi/2)]) (pi/2))

-- | Basic verification of finalState,
-- a function used in testing.
finalStateTest :: Test
finalStateTest =
  Test ("finalState [PenDown, Turn (-pi), Forward 5] ==" ++
  " TurtleState True (5,0) (-pi)")
  (assertTurtleStateApproxEqual (finalState
  [PenDown, Turn (-pi/2), Forward 5]) (TurtleState True (5,0) (-pi/2)))

-- The following tests are to verify functions within
-- Turtle.hs work as intended.

-- | Check triangle covers the correct distance.
triangleTestA :: Test
triangleTestA =
  Test "totalDist (triangle 4) == 4*3"
  (assertEqual (totalDist (triangle 4.5)) (4.5*3))

-- | Check that the final state of triangle is the same as the initial state.
triangleTestB :: Test
triangleTestB =
  Test "triangle 6 == TurtleState False (0,0) (2 * pi)"
  (assertTurtleStateApproxEqual (finalState (triangle 6))
  (TurtleState False (0,0) (2 * pi)))

-- | Check the expected list of commands for a triangle with a side length of 5
-- are outputted from triangle.
-- The expected list of commands has been verified by following
-- them to draw a shape on a piece of paper.
triangleTestC :: Test
triangleTestC =
  Test ("triangle 5 == [PenDown,Forward 5.0,Turn triRConst,Forward 5.0," ++
  "Turn triRConst,Forward 5.0,Turn triRConst,PenUp]")
  (assertEqual (triangle 5)
  ([PenDown,Forward 5.0,Turn triRConst,Forward 5.0,
  Turn triRConst,Forward 5.0,Turn triRConst,PenUp]))

-- | A polygon with 3 sides is a triangle.
triPolyEqTest :: Test
triPolyEqTest =
  Test "triangle 2 == myPolygon 3 2"
  (assertEqual (triangle 2) (myPolygon 3 2))

-- | Check myPolygon covers the correct distance.
myPolygonTestA :: Test
myPolygonTestA =
  Test "totalDist (myPolygon 5 8) == 5*8"
  (assertEqual (totalDist (myPolygon 5 8.5)) (5*8.5))

-- | Check that the final state of myPolygon is the same as the initial state.
myPolygonTestB :: Test
myPolygonTestB =
  Test "myPolygon 7 3 == TurtleState False (0,0) (2 * pi)"
  (assertTurtleStateApproxEqual (finalState (myPolygon 7 3))
  (TurtleState False (0,0) (2 * pi)))

-- | Check the expected list of commands for a polygon with 4 sides
-- and a side length of 2 are outputted from myPolygon 4 2.
-- The expected list of commands has been verified by following
-- them to draw a shape on a piece of paper.
myPolygonTestC :: Test
myPolygonTestC =
  Test ("myPolygon 4 2 == [PenDown,Forward 2.0,Turn (pi/2),Forward 2.0," ++
  "Turn (pi/2),Forward 2.0,Turn (pi/2),Forward 2.0,Turn (pi/2),PenUp]")
  (assertEqual (myPolygon 4 2)
  ([PenDown,Forward 2.0,Turn (pi/2),
  Forward 2.0,Turn (pi/2),Forward 2.0,
  Turn (pi/2),Forward 2.0,
  Turn (pi/2),PenUp]))

-- | Check sierpinski covers the correct distance.
sierpinskiTestA :: Test
sierpinskiTestA =
  Test "totalDist (sierpinski 3 2) == 28.5"
  (assertEqual (totalDist (sierpinski 3 2)) (28.5))

-- | Check that the final state of sierpinski is the same as the initial state.
sierpinskiTestB :: Test
sierpinskiTestB =
  Test "sierpinski 4 5 == TurtleState False (0,0) (80 * pi)"
  (assertTurtleStateApproxEqual (finalState (sierpinski 4 5))
  (TurtleState False (0,0) (80 * pi)))

-- | Check the expected list of commands for a sierpinski triangle
-- with 2 levels and a side length of 3 are outputted from a sierpinski 2 3.
-- The expected list of commands has been verified by following
-- them to draw a shape on a piece of paper.
sierpinskiTestC :: Test
sierpinskiTestC =
  Test ("sierpinski 2 3 == " ++
  "[PenDown,Forward 3.0,Turn triRConst,Forward 3.0,Turn triRConst," ++
  "Forward 3.0,Turn triRConst,PenUp,PenDown,Forward 1.5,Turn triRConst," ++
  "Forward 1.5,Turn triRConst,Forward 1.5,Turn triRConst,PenUp,Forward 1.5,"++
  "PenDown,Forward 1.5,Turn triRConst,Forward 1.5,Turn triRConst," ++
  "Forward 1.5,Turn triRConst,PenUp,Turn triRConst,Forward 1.5," ++
  "Turn (-triRConst),PenDown,Forward 1.5,Turn triRConst,Forward 1.5," ++
  "Turn triRConst,Forward 1.5,Turn triRConst,PenUp,Turn (-triRConst)" ++
  ",Forward 1.5,Turn triRConst]")
  (assertEqual (sierpinski 2 3)
  ([PenDown,Forward 3.0,Turn triRConst,Forward 3.0,Turn triRConst,
  Forward 3.0,Turn triRConst,PenUp,PenDown,Forward 1.5,Turn triRConst,
  Forward 1.5,Turn triRConst,Forward 1.5,Turn triRConst,PenUp,Forward 1.5,
  PenDown,Forward 1.5,Turn triRConst,Forward 1.5,Turn triRConst,Forward 1.5,
  Turn triRConst,PenUp,Turn triRConst,Forward 1.5,Turn (-triRConst),PenDown,
  Forward 1.5,Turn triRConst,Forward 1.5,Turn triRConst,Forward 1.5,
  Turn triRConst,PenUp,Turn (-triRConst),Forward 1.5,Turn triRConst]))

-- | A sierpinski triangle with only 1 level should be the
-- same as a normal triangle.
sierpinskiTriEqTest :: Test
sierpinskiTriEqTest =
  Test "sierpinski 1 10 == triangle 10"
  (assertEqual (sierpinski 1 10) (triangle 10))

{- Example test cases for reference.
-- | Example test case. The String argument to 'Test' is a label that
-- identifies the test, and gives the reader some idea about what's
-- being tested. For simple arithmetic, these should be obvious, but
-- when you write tests for your turtle interpreter, you can use this
-- space to say things like "driving out and back leaves you in the
-- same position".
exampleTestOne :: Test
exampleTestOne = Test "2 + 2 == 4" (assertEqual (2 + 2) (4 :: Int))

-- | Because we are storing the turtle position in a pair of
-- 'Double's, addition behaves in ways you might not expect. The
-- 'assertApproxEqual' function tests that two 'Double's are close
-- enough to each other.
exampleTestTwo :: Test
exampleTestTwo = Test "0.1 + 0.2 =~ 0.3" (assertApproxEqual (0.1 + 0.2) 0.3)

-- | 0.1 + 0.2 is NOT 0.3 when computed using 'Double's. This is why
-- we have provided you with the 'assertApproxEqual' function.
exampleTestThree :: Test
exampleTestThree = Test "0.1 + 0.2 /= 0.3" (assertNotEqual (0.1 + 0.2) (0.3 :: Double))

-- | You might find it easier to write tests against entire 'Point's.
exampleTestFour :: Test
exampleTestFour = Test "(0.0, 0.1 + 0.2) =~ (0.0, 0.3)"
  (assertPointApproxEqual (0.0, 0.1 + 0.2) (0.0, 0.3))

-- | This test will fail, so you can see what a failing test looks
-- like.
exampleFailure :: Test
exampleFailure = Test "0.1 + 0.2 == 0.3" (assertEqual (0.1 + 0.2) (0.3 :: Double))
-}

-- | A haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests
