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
            goRight 12 ',' ',' 100  -- final state matches current state
            ---- END CHECK FINAL STATES

            ---- BEGIN FIND TRANSITION FUNCTION
            ---- END FIND TRANSITION FUNCTION
