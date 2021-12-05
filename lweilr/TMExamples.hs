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
  TM [0 .. 25] "01?,#." "01?,#.ab@" id ' ' '?' trans 1 [25]
  where
    trans = goRight 0 '?' '?' 1 ++
            ---- BEGIN WRITING THE START STATE TO THE LEFT OF THE FIRST INPUT BIT
            goRight 1 '0' 'a' 2 ++  -- case where encoded input state starts with 0
            goRight 1 '1' 'b' 3 ++  -- case where encoded input state starts with 1
            goRight 1 '#' '#' 25 ++ -- finished processing input state, go to final state 
                                    -- (just for now, until we write the other peices of the algorithm)
            loopRight 2 "01,.#" ++  -- loop over everything to the start of the input string (from path a)
            loopRight 3 "01,.#" ++  -- loop over everything to the start of the input string (from path b)
            goRight 2 '@' '0' 4 ++  -- write the first bit of the start state (from path a)
            goRight 3 '@' '1' 4 ++  -- write the first bit of the start state (from path b)
            loopLeft 4 "01,.a#" ++  -- go back to the beginning of the tape
            goRight 4 '?' '?' 5 ++
            loopRight 5 "01" ++     -- loop over any bits in the start state that we have already processed
            goRight 5 'a' '0' 1 ++  -- found a placeholder, we have written this bit already so put it back to 0
            goRight 5 'b' '1' 1     -- "" so put it back to 1
            ---- END WRITING THE START STATE TO THE LEFT OF THE FIRST INPUT BIT
