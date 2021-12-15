module TMExamples where

import TM
import UniversalTM

-- following suggestion by Junnan in class
tripletm =
  TM [1 .. 6] "abc" "abc*! " id ' ' '!' trans 6 [5, 6, 1]
  where
    trans = goRight 1 ' ' ' ' 6 ++
            loopRight 1 "*" ++
            goRight 1 'a' '*' 2 ++
            loopRight 2 "a*" ++
            goRight 2 'b' '*' 3 ++
            loopRight 3 "b*" ++
            goRight 3 'c' '*' 4 ++
            loopRight 4 "c*" ++
            goLeft 4 ' ' ' ' 5 ++
            loopLeft 5 "abc*" ++
            goRight 5 '!' '!' 1 

el_utm =
  TM [0 .. 1000] "01?,#." "01?,#.abcd@" id ' ' '?' trans 1 [1000]
  where
    trans = goRight 0 '?' '?' 1 ++
            ---- BEGIN WRITING THE START STATE TO THE LEFT OF THE FIRST INPUT BIT
            goRight 1 '0' 'a' 2 ++  -- case where encoded input state starts with 0
            goRight 1 '1' 'b' 3 ++  -- case where encoded input state starts with 1
            goRight 1 '#' '#' 6 ++ -- finished processing input state, go to NEXT ROUTINE
            loopRight 2 "01.,#ab" ++  -- loop over everything to the start of the input string (from path a)
            loopRight 3 "01.,#ab" ++  -- loop over everything to the start of the input string (from path b)
            goRight 2 '@' '@' 4 ++
            goRight 3 '@' '@' 5 ++
            loopRight 4 "@.01" ++
            loopRight 5 "@.01" ++
            goRight 4 ',' ',' 7 ++
            goRight 5 ',' ',' 8 ++
            loopRight 7 "ab" ++
            loopRight 8 "ab" ++
            goRight 7 '@' 'a' 9 ++  -- write the first bit of the start state (from path a)
            goRight 8 '@' 'b' 9 ++  -- write the first bit of the start state (from path b)
            loopLeft 9 "01,.@ba#" ++  -- go back to the beginning of the tape
            goRight 9 '?' '?' 10 ++
            loopRight 10 "01" ++     -- loop over any bits in the start state that we have already processed
            goRight 10 'a' '0' 1 ++  -- found a placeholder, we have written this bit already so put it back to 0
            goRight 10 'b' '1' 1 ++   -- "" so put it back to 1
            ---- END WRITING THE START STATE TO THE LEFT OF THE FIRST INPUT BIT

            ---- BEGIN CHECK FINAL STATES
            -- first need to get to current state, which was conveniently marked with as and bs
            -- on the way, go ahead and mark the first bit of the first final state
            goRight 6 '0' 'a' 11 ++
            goRight 6 '1' 'b' 12 ++
            goRight 6 '#' '#' 19 ++ -- move on to next algorithm
            loopRight 11 "01cd,.#" ++
            loopRight 12 "01cd,.#" ++
            goRight 11 '@' '@' 100 ++
            goRight 12 '@' '@' 101 ++
            loopRight 100 "@.01" ++
            loopRight 101 "@.01" ++
            goRight 100 ',' ',' 102 ++
            goRight 101 ',' ',' 103 ++
            loopRight 102 "01cd,.#" ++
            loopRight 103 "01cd,.#" ++
            -- should now be at beginning of current state
            goRight 102 'a' 'c' 13 ++
            goRight 103 'b' 'd' 14 ++ -- saw the correct bit
            goRight 102 '@' '@' 15 ++
            goRight 103 '@' '@' 15 ++


            -- when it saw not correct bit
            goRight 102 'b' 'b' 15 ++
            goRight 103 'a' 'a' 15 ++

            -- or move on if @
            -- else, go back and check next state
            goRight 11 'b' 'b' 15 ++
            goRight 12 'a' 'a' 15 ++
            loopLeft 15 "01,.@abcd#" ++ -- go back to beginning of tape
            goRight 15 '?' '?' 16 ++
            loopRight 16 "01,.@#" ++
            goRight 16 'a' '0' 17 ++
            goRight 16 'b' '1' 17 ++
            loopRight 17 "01" ++
            goRight 17 ',' ',' 6 ++
            -- if no next state, move on to next algorithm
            loopLeft 13 "01,.@abcd" ++ -- back to saw the correct bit
            loopLeft 14 "01,.@abcd" ++
            goLeft 13 '#' '#' 18 ++
            goLeft 14 '#' '#' 18 ++
            loopLeft 18 "01,.@#" ++
            goRight 18 'a' '0' 6 ++
            goRight 18 'b' '1' 6 ++
            goRight 18 ',' ',' 1000  -- final state matches current state
            ---- END CHECK FINAL STATES

{-
            ---- BEGIN CHECK TRANSITION STATE
            -- currently at beginning of transition functions, move forward to current state
            loopLeft 19 "ab,.01@" ++ -- looping left to be lazy, because it ended after first bit of the trans functions
            goRight 19 '#' '#' 20 ++    
            goRight 20 '0' 'a' 21 ++
            goRight 20 '1' 'b' 22 ++
            goRight 20 '.' '.' 31 ++ --23 ++
            loopRight 21 "01,.@#" ++
            loopRight 22 "01,.@#" ++
            goRight 21 'a' 'a' 24 ++ -- found a match
            goRight 22 'b' 'b' 25 ++ -- found a match
            -- if no match
            goRight 21 'b' 'b' 26 ++ -- wrong character, need to go back to check next transition
            goRight 22 'a' 'a' 26 ++ -- wrong character, ""
            loopLeft 26 "01,.@ab" ++
            goLeft 26 '#' '#' 27 ++
            loopLeft 27 "01,.@#" ++
            goRight 27 'a' '0' 28 ++ -- put the bit back
            goRight 27 'b' '1' 28 ++ -- ""
            loopRight 28 "01." ++ -- go to comma
            goRight 28 ',' ',' 20 ++ -- start over with next transition
            -- back to case where there was a match
            loopLeft 24 "01,.@ab" ++
            loopLeft 25 "01,.@ab" ++
            goLeft 24 '#' '#' 29 ++
            goLeft 25 '#' '#' 30 ++
            loopLeft 29 "01,.@#" ++
            loopLeft 30 "01,.@#" ++
            goRight 29 'a' '0' 20 ++
            goRight 30 'b' '1' 20 ++ -- case for more state bits to process
            ---- END CHECK TRANSITION STATE


            ---- BEGIN CHECK INPUT CHARACTER
            goRight 31 '0' 'a' 32 ++
            goRight 31 '1' 'b' 32 ++
            -- go mark the transition function bit
            loopRight 32 "01,.#" ++
            goRight 32 'a' 'a' 33 ++
            goRight 32 'b' 'b' 33 ++
            loopRight 33 "@ab" ++
            goRight 33 '.' '.' 34 ++
            goRight 34 '0' 'a' 35 ++
            goRight 34 '1' 'b' 35 ++
            loopLeft 35 "ab.01," ++
            goLeft 35 '#' '#' 36 ++
            loopLeft 36 "01,.@#" ++
            -- now start actuall processing things
            goRight 36 'a' 'a' 37 ++
            goRight 36 'b' 'b' 38 ++
            loopRight 37 "01,.#" ++
            loopRight 38 "01,.#" ++
            goRight 37 'a' 'a' 39 ++
            goRight 37 'b' 'b' 39 ++
            goRight 38 'a' 'a' 40 ++
            goRight 38 'b' 'b' 40 ++
            loopRight 39 "@ab" ++
            loopRight 40 "@ab" ++
            goRight 39 '.' '.' 41 ++
            goRight 40 '.' '.' 42 ++
            loopRight 41 "01" ++
            loopRight 42 "01" ++
            goRight 41 'a' '0' 43 ++
            goRight 41 'b' '1' 43 ++
            goRight 43 '0' 'a' 44 ++
            goRight 42 'a' '0' 45 ++
            goRight 42 'b' '1' 45 ++
            goRight 45 '1' 'b' 44 ++
            goRight 43 '1' '1' 26 ++ 
            goRight 45 '0' '0' 26 ++
            loopLeft 44 "ab01@,." ++
            goLeft 44 '#' '#' 46 ++
            loopLeft 46 "01#,." ++
            goRight 46 'a' '0' 47 ++
            goRight 46 'b' '1' 47 ++ 
            goRight 47 '0' 'a' 37 ++
            goRight 47 '1' 'b' 38 ++
            goRight 47 '.' '.' 1000 
            ---- END CHECK INPUT CHARACTER


            ---- BEGIN WRITE NEW CHARACTER TO TAPE
            loopRight 42 "01" ++ -- skip over goLeft, goRight instruction for now
            goRight 42 '.' '.' 43 ++
            loopRight 43 "01" ++ -- skip over new state for now
            goRight 43 '.' '.' 44 ++ -- now at the beginning of the character to be written
            -- begin write character subroutine
            goRight 44 '0' 'a' 45 ++
            goRight 44 '1' 'b' 47 ++ -- mark character to be written
            goRight 44 '.' '.' 54 ++ -- done writing input, move on to next part 
            loopRight 45 "01.,#" ++
            loopRight 47 "01.,#" ++
            goRight 45 'a' 'a' 48 ++
            goRight 45 'b' 'b' 48 ++
            goRight 47 'a' 'a' 49 ++
            goRight 47 'b' 'b' 49 ++
            loopRight 48 "ab@." ++
            loopRight 49 "ab@." ++
            goRight 50 '0' 'a' 52 ++
            goRight 50 '1' 'a' 52 ++
            goRight 51 '0' 'b' 52 ++
            goRight 51 '1' 'b' 52 ++
            loopLeft 52 "ab@.01" ++
            goLeft 52 '#' '#' 53 ++
            loopLeft 53 "01#,." ++
            goLeft 53 'a' '0' 44 ++ 
            goLeft 53 'b' '1' 44

           
-}




            -- end write character subroutine
            ---- END WRITE NEW CHARACTER TO TAPE










