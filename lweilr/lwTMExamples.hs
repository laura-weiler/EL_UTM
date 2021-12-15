module TMExamples where

import TM
import UniversalTM

-- following suggestion by Junnan in class
tripletm =
  TM [1 .. 6] "abc" "abc*! " id ' ' '!' trans 1 [6]
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
  TM [0 .. 100] "01?,#." "01?,#.ab@" id ' ' '?' trans 1 [100]
  where
    trans = goRight 0 '?' '?' 1 ++
            ---- BEGIN WRITING THE START STATE TO THE LEFT OF THE FIRST INPUT BIT
            goRight 1 '0' 'a' 2 ++  -- case where encoded input state starts with 0
            goRight 1 '1' 'b' 3 ++  -- case where encoded input state starts with 1
            goRight 1 '#' '#' 6 ++ -- finished processing input state, go to NEXT ROUTINE
            loopRight 2 "01,.#ab" ++  -- loop over everything to the start of the input string (from path a)
            loopRight 3 "01,.#ab" ++  -- loop over everything to the start of the input string (from path b)
            goRight 2 '@' 'a' 4 ++  -- write the first bit of the start state (from path a)
            goRight 3 '@' 'b' 4 ++  -- write the first bit of the start state (from path b)
            loopLeft 4 "01,.@ba#" ++  -- go back to the beginning of the tape
            goRight 4 '?' '?' 5 ++
            loopRight 5 "01" ++     -- loop over any bits in the start state that we have already processed
            goRight 5 'a' '0' 1 ++  -- found a placeholder, we have written this bit already so put it back to 0
            goRight 5 'b' '1' 1 ++    -- "" so put it back to 1
            ---- END WRITING THE START STATE TO THE LEFT OF THE FIRST INPUT BIT

            ---- BEGIN CHECK FINAL STATES
            -- first need to get to current state, which was conveniently marked with as and bs
            -- on the way, go ahead and mark the first bit of the first final state
            goRight 6 '0' 'a' 7 ++
            goRight 6 '1' 'b' 9 ++
            goRight 6 '#' '#' 16 ++ -- move on to next algorithm
            loopRight 7 "01,.@#" ++
            loopRight 9 "01,.@#" ++
            -- should now be at beginning of current state
            goRight 7 'a' '0' 10 ++
            goRight 9 'b' '1' 11 ++ -- saw the correct bit
            -- else, go back and check next state
            goRight 7 'b' 'b' 13 ++
            goRight 9 'a' 'a' 13 ++
            loopLeft 13 "01,.@ab#" ++ -- go back to beginning of tape
            goRight 13 '?' '?' 14 ++
            loopRight 14 "01,.@#" ++
            goRight 14 'a' '0' 15 ++
            goRight 14 'b' '1' 15 ++
            loopRight 15 "01" ++
            goRight 15 ',' ',' 6 ++
            -- if no next state, move on to next algorithm
            loopLeft 10 "01,.@ab" ++ -- back to saw the correct bit
            loopLeft 11 "01,.@ab" ++
            goLeft 10 '#' '#' 12 ++
            goLeft 11 '#' '#' 12 ++
            loopLeft 12 "01,.@#" ++
            goRight 12 'a' '0' 6 ++
            goRight 12 'b' '1' 6 ++
            goRight 12 ',' ',' 100  ++ -- final state matches current state
            ---- END CHECK FINAL STATES

            ---- BEGIN CHECK TRANSITION STATE
            -- currently at beginning of transition functions, move forward to current state
            loopLeft 16 "ab,.01@" ++ -- looping left to be lazy, because it ended after first bit of the trans functions
            goRight 16 '#' '#' 17 ++    
            goRight 17 '0' 'a' 18 ++
            goRight 17 '1' 'b' 20 ++
            goRight 17 '.' '.' 28 ++
            loopRight 18 "01,.@#" ++
            loopRight 20 "01,.@#" ++
            goRight 18 'a' 'a' 21 ++ -- found a match
            goRight 20 'b' 'b' 22 ++ -- found a match
            -- if no match
            goRight 18 'b' 'b' 25 ++ -- wrong character, need to go back to check next transition
            goRight 20 'a' 'a' 25 ++ -- wrong character, ""
            loopLeft 25 "01,.@ab" ++
            goLeft 25 '#' '#' 26 ++
            loopLeft 26 "01,.@#" ++
            goRight 26 'a' '0' 27 ++ -- put the bit back
            goRight 26 'b' '1' 27 ++ -- ""
            loopRight 27 "01." ++ -- go to comma
            goRight 27 ',' ',' 17 ++ -- start over with next transition
            -- back to case where there was a match
            loopLeft 21 "01,.@ab" ++
            loopLeft 22 "01,.@ab" ++
            goLeft 21 '#' '#' 23 ++
            goLeft 22 '#' '#' 24 ++
            loopLeft 23 "01,.@#" ++
            loopLeft 24 "01,.@#" ++
            goRight 23 'a' '0' 17 ++
            goRight 24 'b' '1' 17 ++ -- case for more state bits to process
            ---- END CHECK TRANSITION STATE


            ---- BEGIN CHECK INPUT CHARACTER
            goRight 28 '0' 'a' 29 ++
            goRight 28 '1' 'b' 29 ++
            -- go mark the transition function bit
            loopRight 29 "01,.#" ++
            goRight 29 'a' 'a' 30 ++
            goRight 29 'b' 'b' 30 ++
            loopRight 30 "@ab" ++
            goRight 30 '.' '.' 31 ++
            goRight 31 '0' 'a' 32 ++
            goRight 31 '1' 'b' 32 ++
            loopLeft 32 "ab.01," ++
            goLeft 32 '#' '#' 33 ++
            loopLeft 33 "01,.@#" ++
            -- now start actuall processing things
            goRight 33 'a' 'a' 34 ++
            goRight 33 'b' 'b' 35 ++
            loopRight 34 "01,.#" ++
            loopRight 35 "01,.#" ++
            goRight 34 'a' 'a' 36 ++
            goRight 34 'b' 'b' 36 ++
            goRight 35 'a' 'a' 37 ++
            goRight 35 'b' 'b' 37 ++
            loopRight 36 "@ab" ++
            loopRight 37 "@ab" ++
            goRight 36 '.' '.' 38 ++
            goRight 37 '.' '.' 39 ++
            loopRight 38 "01" ++
            loopRight 39 "01" ++
            goRight 38 'a' '0' 40 ++
            goRight 38 'b' '1' 40 ++
            goRight 40 '0' 'a' 41 ++
            goRight 39 'a' '0' 42 ++
            goRight 39 'b' '1' 42 ++
            goRight 42 '1' 'b' 41 ++
            goRight 40 '1' '1' 25 ++ 
            goRight 42 '0' '0' 25 ++
            loopLeft 41 "ab01@,." ++
            goLeft 41 '#' '#' 43 ++
            loopLeft 43 "01#,." ++
            goRight 43 'a' '0' 44 ++
            goRight 43 'b' '1' 44 ++ 
            goRight 44 '0' 'a' 34 ++
            goRight 44 '1' 'b' 35 ++
            goRight 44 '.' '.' 1000 
            ---- END CHECK INPUT CHARACTER

{-
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










