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
  TM [0 .. 25] "01?,#." "01?,#.ab" id ' ' '?' trans 1 [25]
  where
    trans = goRight 0 '?' '?' 1 ++
            ---- BEGIN WRITING THE START STATE TO THE FIRST BLANK
            -- now traverse right 
            loopRight 1 "01,."  ++
            goRight 1 '#' '#' 2 ++
            loopRight 2 "01,." ++ 
            goRight 2 '#' '#' 3 ++
            loopRight 3 "01,." ++
            goRight 3 '#' '#' 4 ++
            loopRight 4 "01,." ++
            goRight 4 '#' '#' 5 ++
            -- at this point, should be at the start of the input string
            -- we put a blank here to write the start state
            goRight 5 '0' '0' 6 ++
            goLeft 6 '0' 'a' 7 ++
            loopLeft 7 "01,." ++
            goLeft 7 '#' '#' 8 ++
            loopLeft 8 "01,." ++
            goLeft 8 '#' '#' 9 ++
            loopLeft 9 "01,." ++
            goLeft 9 '#' '#' 10 ++
            -- now the start state is immediately before the tape head. 
            -- go back to the start symbol for the UTM
            -- so that the start state is immediately after the tape head.
            loopLeft 10 "01,." ++
            goRight 10 '?' '?' 12 ++
            goRight 10 'a' '0' 12 ++
            goRight 10 'b' '1' 12 ++
            goRight 12 '0' 'a' 13 ++
            -- now go back to write the start state to the blank we're processing
            -- case for a
            loopRight 13 "01,#." ++
            goRight 13 'a' '0' 14 ++
            -- case for 1,b
            goRight 12 '1' 'b' 15 ++
            loopRight 15 "01,#." ++
            goRight 15 'a' '1' 5
            ---- END WRITING THE START STATE TO THE FIRST BLANK
