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
            loopRight 2 "01,.#" ++  -- loop over everything to the start of the input string (from path a)
            loopRight 3 "01,.#" ++  -- loop over everything to the start of the input string (from path b)
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

            ---- BEGIN FIND TRANSITION FUNCTION
            -- currently at beginning of transition functions, move forward to current state
            loopLeft 16 "ab,.01@" ++ -- looping left to be lazy, because it ended after first bit of the trans functions
            goRight 16 '#' '#' 17 ++    
            goRight 17 '0' 'a' 18 ++
            goRight 17 '1' 'b' 20 ++
            goRight 17 '.' '.' 25 ++
            loopRight 18 "01,.@#" ++
            loopRight 20 "01,.@#" ++
            goRight 18 'a' 'a' 21 ++
            goRight 20 'b' 'b' 22 ++ -- found a match
            -- now go back
            loopLeft 21 "01,.@ab" ++
            loopLeft 22 "01,.@ab" ++
            goLeft 21 '#' '#' 23 ++
            goLeft 22 '#' '#' 24 ++
            loopLeft 23 "01,.@#" ++
            loopLeft 24 "01,.@#" ++
            goRight 23 'a' '0' 17 ++
            goRight 24 'b' '1' 17 ++ -- case for more state bits to process
            goRight 25 '0' 'a' 26 ++
            goRight 25 '1' 'b' 26 ++ -- mark first bit of input char in transition function
            loopRight 26 "01,.#" ++
            goRight 26 'a' 'a' 27 ++
            goRight 26 'b' 'b' 27 ++
            goRight 27 '@' '@' 28 ++ -- current state in input part of tape is also done, now can move on to input character
            loopRight 28 "@" ++
            goRight 28 '0' 'a' 29 ++
            goRight 28 '1' 'b' 30 ++ -- now go back to see if this matches the bit we marked in the transition function input char
            loopLeft 29 "01,.@ab" ++
            loopLeft 30 "01,.@ab" ++
            goLeft 29 '#' '#' 31 ++
            goLeft 30 '#' '#' 32 ++
            loopLeft 31 "01,.#" ++ 
            loopLeft 32 "01,.#" ++
            goLeft 31 'a' 'a' 33 ++
            goLeft 32 'b' 'b' 33 ++
            goRight 33 'a' '0' 34 ++
            goRight 33 'b' '1' 34 ++
            goRight 34 '0' 'a' 38 ++
            goRight 34 '1' 'b' 40 ++
            loopRight 38 "01,.@#" ++
            loopRight 40 "01,.@#" ++
            goRight 38 'a' '0' 28 ++
            goRight 40 'b' '1' 28 ++
            -- need case for mismatch
            











            ---- END FIND TRANSITION FUNCTION
