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
            -- Dec 13th changed this from 0, 1 to a, b for possible convenience with next part of algorithm
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
            goRight 28 '1' 'b' 30 ++
            goRight 28 '.' '.' 42 ++ -- end of input character, go to next part of the algorithm
            loopRight 29 "01,.#" ++
            loopRight 30 "01,.#" ++
            goRight 29 'a' 'a' 31 ++
            goRight 29 'b' 'b' 31 ++
            goRight 30 'a' 'a' 32 ++
            goRight 30 'b' 'b' 32 ++
            loopRight 31 "@ab" ++
            loopRight 32 "@ab" ++            
            goRight 31 '.' '.' 33 ++
            goRight 32 '.' '.' 34 ++
            loopRight 33 "01" ++
            loopRight 34 "01" ++
            -- case for if the first letter hasn't been marked yet
            goLeft 33 ',' ',' 38 ++ -- if end of input found, go back to mark the first character
            goLeft 34 ',' ',' 39 ++
            loopLeft 38 "01" ++
            loopLeft 39 "01" ++
            goRight 38 '.' '.' 40 ++
            goRight 39 '.' '.' 41 ++
            goRight 40 '0' 'a' 35 ++ -- match
            goRight 41 '1' 'b' 35 ++
            goRight 40 '1' '1' 25 ++ -- no match
            goRight 41 '0' '0' 25 ++
            -- end case
            goRight 33 'a' '0' 37 ++ -- put the previously checked bit back
            goRight 33 'b' '1' 37 ++ -- put the previously checked bit back
            goRight 34 'a' '0' 38 ++
            goRight 34 'b' '1' 38 ++
            goRight 37 '0' 'a' 35 ++ -- found match
            goRight 38 '1' 'b' 35 ++ -- found match
            goRight 37 '1' '1' 25 ++ --didn't find match, go back to check next transition
            goRight 38 '0' '0' 25 ++ --didn't find match, go back to check next transition
            loopLeft 35 "01,.@ab" ++
            goLeft 35 '#' '#' 36 ++
            loopLeft 36 "01,.#" ++
            goRight 36 'a' '0' 28 ++
            goRight 36 'b' '1' 28
            ---- END CHECK INPUT CHARACTER












