-- Here is where you'll define both Langton's Ant, and how
-- to transition a world
-- You'll definitely want to change this
-- Robert 'Probie' Offner
-- PUT YOUR NAME HERE!
--HAOTIAN XUE
--All the stuff are 100% my work!
-- The idea of readDir,applyToEachElement,listOfMaybeOfList is from https://piazza.com/class/ik1udv3647w3bq?cid=47
-- Feb 2016

module StudentSources.LangtonsHex where

import Datastructures.Ant
import Datastructures.Cells
import Datastructures.Transitions
import Datastructures.HexWorld
import Data.Maybe
import Data.List
import Data.Char

-- #####################################################################
-- ######################## Part 2 Starts Here #########################
-- #####################################################################

{--findCell' :: Coord -> HexWorld -> Maybe Cell
findCell' coord (HexWorld _ cs) = go cs
    where
      go [] = Nothing
      go (x:xs)
         | cellPosition x == coord = Just x
	 | otherwise = go xs--}

-- Right now all this does is take the current hexWorld and
-- return the unchanged state. You need to take the Ant in the
-- hexWorld and change it's Direction.
-- So if the Ant was pointing Up and turns 60 degrees to the right (R1) it should
-- be RU
turnAnt :: HexWorld -> HexTurn -> HexWorld
turnAnt hexWorld direction = case (direction,hexantOrientation (theAnt hexWorld)) of
                                       (L2,Up)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L2,Dn)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld 
                                       (L2,RU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld 
                                       (L2,RD)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Up
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld 
                                       (L2,LU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Dn
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L2,LD)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L1,Up)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L1,Dn)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L1,RU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Up
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L1,RD)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L1,LU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (L1,LD)    -> let
                                                       oldAnt = theAnt hexWorld
       						       oldantOrientation = hexantOrientation oldAnt
       						       newantOrientation = Dn
       						       newAnt = oldAnt{hexantOrientation=newantOrientation}
       						       newhexWorld=hexWorld{theAnt=newAnt}
       						     in newhexWorld
                                       (R1,Up)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R1,Dn)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R1,RU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R1,RD)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Dn
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R1,LU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Up
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R1,LD)    -> let
                                                       oldAnt = theAnt hexWorld
       						       oldantOrientation = hexantOrientation oldAnt
       						       newantOrientation = LU
       						       newAnt = oldAnt{hexantOrientation=newantOrientation}
       						       newhexWorld=hexWorld{theAnt=newAnt}
       						     in newhexWorld
                                       (R2,Up)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R2,Dn)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R2,RU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Dn
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R2,RD)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R2,LU)    -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (R2,LD)    -> let
                                                       oldAnt = theAnt hexWorld
       						       oldantOrientation = hexantOrientation oldAnt
       						       newantOrientation = Up
       						       newAnt = oldAnt{hexantOrientation=newantOrientation}
       						       newhexWorld=hexWorld{theAnt=newAnt}
       						     in newhexWorld
                                       (U,Up)     -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Dn
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (U,Dn)     -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = Up
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (U,RU)     -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (U,RD)     -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = LU
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (U,LU)     -> let
                                                       oldAnt = theAnt hexWorld
						       oldantOrientation = hexantOrientation oldAnt
						       newantOrientation = RD
						       newAnt = oldAnt{hexantOrientation=newantOrientation}
						       newhexWorld=hexWorld{theAnt=newAnt}
						     in newhexWorld  
                                       (U,LD)     -> let
                                                       oldAnt = theAnt hexWorld
       						       oldantOrientation = hexantOrientation oldAnt
       						       newantOrientation = RU
       						       newAnt = oldAnt{hexantOrientation=newantOrientation}
       						       newhexWorld=hexWorld{theAnt=newAnt}
       						     in newhexWorld
                                       (None,_)   -> hexWorld						    
-- Just like turnAnt all it does now is return the hexWorld
-- without doing anything. You need to get the Ant and set its
-- Coord to the hex in front of it.

moveAnt :: HexWorld -> HexWorld
moveAnt hexWorld = case hexantOrientation (theAnt hexWorld) of
          Up -> let
	          oldAnt = theAnt hexWorld
	  	  oldantPosition = hexantPosition oldAnt
	   	  oldY = yCoord oldantPosition
	  	  newY = oldY + 1
	  	  newantPosition = oldantPosition{yCoord=newY}
	  	  newAnt = oldAnt{hexantPosition=newantPosition}
	   	  newHexWorld=hexWorld{theAnt=newAnt}
	  	in newHexWorld
	  Dn -> let
	          oldAnt = theAnt hexWorld
	  	  oldantPosition = hexantPosition oldAnt
	   	  oldY = yCoord oldantPosition
	  	  newY = oldY - 1
	  	  newantPosition = oldantPosition{yCoord=newY}
	  	  newAnt = oldAnt{hexantPosition=newantPosition}
	   	  newHexWorld=hexWorld{theAnt=newAnt}
	  	in newHexWorld
	  RU -> let
	          oldAnt = theAnt hexWorld
	  	  oldantPosition = hexantPosition oldAnt
	   	  oldX = xCoord oldantPosition
	  	  newX = oldX + 1
	  	  newantPosition = oldantPosition{xCoord=newX}
	  	  newAnt = oldAnt{hexantPosition=newantPosition}
	   	  newHexWorld=hexWorld{theAnt=newAnt}
	  	in newHexWorld
	  RD -> let
	          oldAnt = theAnt hexWorld
	  	  oldantPosition = hexantPosition oldAnt
	   	  oldY = yCoord oldantPosition
	  	  newY = oldY - 1
	  	  oldX = xCoord oldantPosition
	  	  newX = oldX + 1
	  	  newantPosition = oldantPosition{xCoord=newX,yCoord=newY}
	  	  newAnt = oldAnt{hexantPosition=newantPosition}
	   	  newHexWorld=hexWorld{theAnt=newAnt}
	  	in newHexWorld
	  LU -> let
	          oldAnt = theAnt hexWorld
	  	  oldantPosition = hexantPosition oldAnt
	   	  oldY = yCoord oldantPosition
	  	  newY = oldY + 1
	  	  oldX = xCoord oldantPosition
	  	  newX = oldX - 1
	  	  newantPosition = oldantPosition{xCoord=newX,yCoord=newY}
	  	  newAnt = oldAnt{hexantPosition=newantPosition}
	   	  newHexWorld=hexWorld{theAnt=newAnt}
	  	in newHexWorld
	  LD ->	let
	          oldAnt = theAnt hexWorld
	  	  oldantPosition = hexantPosition oldAnt
	   	  oldX = xCoord oldantPosition
	  	  newX = oldX - 1
	  	  newantPosition = oldantPosition{xCoord=newX}
	  	  newAnt = oldAnt{hexantPosition=newantPosition}
	   	  newHexWorld=hexWorld{theAnt=newAnt}
	  	in newHexWorld
		  

-- This is here to help you to write your turnAnt and moveAnt.
-- All it should do is extract the Direction from the hexWorld
getDirection :: HexWorld -> HexDirection
getDirection hexWorld = hexantOrientation (theAnt hexWorld)

-- This is here to help you to write your moveAnt.
-- All it should do is extract the Coord from the hexWorld
getCoord :: HexWorld -> Coord
getCoord hexWorld = hexantPosition (theAnt hexWorld)

-- The main function you need to write. To start with it might be easier to
-- hardcode the behaviour for langton's ant, but if you want to maximize your
-- marks it should work for any ant transition system
transitionWorld :: CellState -> HexWorld -> HexWorld
transitionWorld firstCell hexWorld = if findCell(getCoord hexWorld) hexWorld == Nothing
                                     then let
				             oldtheWorld = theWorld hexWorld
					     oldAnt = theAnt hexWorld
					     newantTransition = [Transition 0 R1,Transition 1 L1]
					     newAnt = oldAnt{hexantTransition=newantTransition}
				     	     newcellPosition = getCoord hexWorld
				     	     newcellState = nextCellState firstCell
				     	     newfirstCell = Cell{cellPosition=newcellPosition,cellState=newcellState}
				     	     newtheWorld = newfirstCell:oldtheWorld
				     	     newhexWorld = hexWorld{theAnt=newAnt,theWorld=newtheWorld}
				      	  in moveAnt (turnAnt newhexWorld R1)
				     else  case (getStateColour (cellState(fromJust(findCell (getCoord hexWorld) hexWorld)))) of
				        0 -> let
				       	        oldtheWorld = theWorld hexWorld
				       	     	oldfirstCell = fromJust(findCell(getCoord hexWorld) hexWorld)
                                                oldcellState = cellState oldfirstCell				               	 
				       	     	newcellState = nextCellState oldcellState                                                  
                                                newfirstCell = oldfirstCell{cellState=newcellState}
				       		newtheWorld = take (fromJust (elemIndex oldfirstCell oldtheWorld)) oldtheWorld ++ newfirstCell:drop( (fromJust (elemIndex oldfirstCell oldtheWorld))+1) oldtheWorld                                                   
				       	     	newhexWorld = hexWorld{theWorld=newtheWorld}
				       	     in moveAnt (turnAnt newhexWorld R1)
				        1 -> let
					        oldtheWorld = theWorld hexWorld
					     	oldfirstCell = fromJust(findCell(getCoord hexWorld) hexWorld)
					     	oldcellState = cellState oldfirstCell						   
					     	newcellState = firstCell						 
						newfirstCell = oldfirstCell{cellState=newcellState}
                                                newtheWorld = take (fromJust (elemIndex oldfirstCell oldtheWorld)) oldtheWorld ++ newfirstCell:drop( (fromJust (elemIndex oldfirstCell oldtheWorld))+1) oldtheWorld                                                  
					     	newhexWorld = hexWorld{theWorld=newtheWorld}
					     in moveAnt (turnAnt newhexWorld L1)		  
					   

-- ############### Allows for different transition systems #############

-- Take a string of the form L1R1UN and return Just [L1,R1,U,None]
readDir :: String -> Maybe HexTurn
readDir a
  | a == "L1" = Just L1
  | a == "R1" = Just R1
  | a == "L2" = Just L2
  | a == "R2" = Just R2
  | a == "U"  = Just U
  | a == "N"  = Just None
  | otherwise = Nothing

applyToEachElement :: (a -> b) -> [a] -> [b]
applyToEachElement _ [] = []
applyToEachElement f (x:xs) = f x : applyToEachElement f xs

listOfMaybeToMaybeOfList :: (Eq a)=>[Maybe a] -> Maybe [a]
listOfMaybeToMaybeOfList [] = Just []
listOfMaybeToMaybeOfList b
    | length (filter (/=Nothing) b) == length b = Just (map (fromJust) b)
    | otherwise = Nothing


change :: String -> [String]
change (head:second:tail) = if head == 'L' || head == 'R'
   then
       if isDigit second
       then [head:second:[]]++change tail
       else [head:[]]++change (second:tail)
   else [head:[]]++change (second:tail)
change "" = []
change (a:[]) = [a:[]]

readHexTransition :: String -> Maybe [HexTurn]
readHexTransition str =  listOfMaybeToMaybeOfList (applyToEachElement readDir (change str))
