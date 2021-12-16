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
            goRight 6 '#' '#' 110 ++ -- move on to next algorithm
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
            -- go to the sink so that it can continue putting the bit back to the final state
            -- and continue checking if there is more final states
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
            goRight 18 ',' ',' 1000 ++ -- final state matches current state
            ---- END CHECK FINAL STATES

            -- putting back c,d to a,b
            loopRight 110 "01,.@#_" ++
            goRight 110 'c' 'a' 111 ++
            goRight 110 'd' 'b' 111 ++
            goRight 110 'a' 'a' 111 ++
            goRight 110 'b' 'b' 111 ++
            --loopLeft 111 "ab" ++
            --goRight 111 ',' ',' 120 ++
            loopRight 111 "ab" ++
            goRight 111 'c' 'a' 111 ++
            goRight 111 'd' 'b' 111 ++
            
            goRight 111 '@' '@' 121 ++ 
            goRight 111 '.' '.' 121 ++
            loopLeft 121 "01,.@#ab_" ++
            goRight 121 '?' '?' 123 ++
            loopRight 123 "01," ++
            goRight 123 '#' '#' 122 ++
            loopRight 122 "01," ++
            goRight 122 '#' '#' 19 ++
            

            ---- BEGIN CHECK TRANSITION STATE
            -- currently at beginning of transition functions, move forward to current state
            loopLeft 19 "ab,.01@" ++ -- looping left to be lazy, because it ended after first bit of the trans functions
            goRight 19 '#' '#' 20 ++    
            goRight 20 '0' 'a' 600 ++
            goRight 20 '1' 'b' 600 ++
            --
            loopRight 600 "ab,.01@_" ++
            goRight 600 '#' '#' 601 ++
            goRight 601 'c' 'a' 601 ++
            goRight 601 'd' 'b' 601 ++
            goRight 601 '@' '@' 602 ++
            goRight 601 '.' '.' 602 ++
            goRight 601 '#' '#' 602 ++
            loopLeft 602 ",.01ab@_" ++
            goLeft 602 '#' '#' 603 ++
            loopLeft 603 "01,.#" ++
            goRight 603 'a' 'a' 21 ++
            goRight 603 'b' 'b' 22 ++
            --
            goRight 20 '.' 'w' 604 ++ --31 ++
            loopRight 604 "ab,.01@_" ++
            goRight 604 '#' '#' 605 ++
            loopRight 605 "ab,.01@_" ++
            goRight 605 'c' 'c' 606 ++
            goRight 605 'd' 'd' 606 ++
            goRight 605 '#' '#' 609 ++
            loopLeft 606 "cdab01@" ++
            goRight 606 ',' ',' 607 ++
            loopRight 607 "ab@" ++
            goRight 607 'c' 'a' 607 ++
            goRight 607 'd' 'b' 607 ++
            goRight 607 '.' '.' 609 ++
            loopLeft 609 ",.01ab@_#" ++
            goRight 609 'w' '.' 31 ++
            --
            loopRight 21 "01,.@_cd#" ++
            loopRight 22 "01,.@_cd#" ++
            goRight 21 'a' 'c' 24 ++ -- found a match
            goRight 22 'b' 'd' 25 ++ -- found a match
             
            -- if no match
            goRight 21 'b' 'b' 26 ++ -- wrong character, need to go back to check next transition
            goRight 22 'a' 'a' 26 ++ -- wrong character, ""
            loopLeft 26 "01,.@ab_" ++
            goLeft 26 '#' '#' 27 ++
            loopLeft 27 "01,.@#" ++
            goRight 27 'a' '0' 28 ++ -- put the bit back
            goRight 27 'b' '1' 28 ++ -- ""
            loopRight 28 "01." ++ -- go to comma
            goRight 28 ',' ',' 20 ++ -- start over with next transition
            -- back to case where there was a match
            loopLeft 24 "01,.@abcd_" ++
            loopLeft 25 "01,.@abcd_" ++
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
            loopRight 32 "01,.#@_" ++ 
            goRight 32 'a' 'a' 33 ++
            goRight 32 'b' 'b' 33 ++
            loopRight 33 "@ab" ++
            goRight 33 '.' '.' 34 ++
            goRight 34 '0' 'a' 35 ++
            goRight 34 '1' 'b' 35 ++
            loopLeft 35 "ab.01@,_" ++
            goLeft 35 '#' '#' 36 ++
            loopLeft 36 "01,.@#" ++
            -- now start actuall processing things
            goRight 36 'a' 'a' 37 ++
            goRight 36 'b' 'b' 38 ++
            loopRight 37 "01,.#@_" ++
            loopRight 38 "01,.#@_" ++
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
            goRight 41 'b' '1' 26 ++ -- bit doesn't match, put it back and go back
            
            -- need to flip the next bit
            goRight 43 '0' 'a' 44 ++
            goRight 43 '1' 'b' 44 ++
            goRight 43 ',' ',' 60 ++
            loopLeft 60 "10.,@ab_" ++
            goLeft 60 '#' '#' 61 ++
            loopLeft 61 "10.,@" ++
            goRight 61 'a' '0' 70 ++
            goRight 61 'b' '1' 70 ++
            loopRight 70 "10" ++
            goRight 70 '.' '.' 200 ++

            goRight 42 'a' '0' 26 ++ -- bit doesn't match, put it back and go back
            goRight 42 'b' '1' 43 ++

            -- !?!?!?!?!?!? wrong or redundant
            loopLeft 44 "ab01@,._" ++
            goLeft 44 '#' '#' 46 ++
            loopLeft 46 "01#,." ++
            goRight 46 'a' '0' 47 ++
            goRight 46 'b' '1' 47 ++ 
            goRight 47 '0' 'a' 37 ++
            goRight 47 '1' 'b' 38 ++
            goRight 47 '.' '.' 200 ++ 
            ---- END CHECK INPUT CHARACTER


            ---- BEGIN WRITE NEW CHARACTER TO TAPE
            
            -- loopRight 200 "01" ++ -- skip over goLeft, goRight instruction for now
            -- instead of looping, mark GoLeft(1) -> b, GoRight(0) -> a
            goRight 200 '1' 'b' 230 ++
            goRight 200 '0' 'a' 230 ++

            goRight 230 '.' '.' 201 ++
            loopRight 201 "01" ++ -- skip over new state for now
            goRight 201 '.' '.' 202 ++ -- now at the beginning of the character to be written
            -- begin write character subroutine
            goRight 202 '0' 'a' 203 ++
            goRight 202 '1' 'b' 204 ++ -- mark character to be written
            goRight 202 '.' '.' 205 ++ -- done writing input, move on to next part
            loopRight 203 "01.,#@_" ++
            loopRight 204 "01.,#@_" ++
            goRight 203 'a' 'a' 206 ++
            goRight 203 'b' 'b' 206 ++
            goRight 204 'a' 'a' 207 ++
            goRight 204 'b' 'b' 207 ++
            loopRight 206 "ab@." ++
            loopRight 207 "ab@." ++
            goRight 206 '0' 'a' 209 ++
            goRight 206 '1' 'a' 209 ++
            goRight 207 '0' 'b' 209 ++
            goRight 207 '1' 'b' 209 ++
            loopLeft 209 "ab@.01,_" ++
            goLeft 209 '#' '#' 211 ++
            loopLeft 211 "01#,." ++
            goRight 211 'a' '0' 202 ++ 
            goRight 211 'b' '1' 202 ++


            -- change remaining bits to ','
            loopRight 205 "01,." ++
            goRight 205 '#' '#' 220 ++
            -- we shouldn't always skip the first input (@@@ + leftend)
            -- because we'll have to update here as well eventually
            -- so I'm looking for the case when we met a/b
            -- + when it has been updated with GoRight GoLeft ... doesn't make any difference on the place we should find? (?)
            -- loop until you find a or b (possible current state)
            loopRight 220 "@01_.," ++
            goRight 220 'a' 'a' 221 ++
            goRight 220 'b' 'b' 221 ++
            -- loop until you get to the input character
            -- the place that you just updated 
            loopRight 221 "ab@" ++
            goRight 221 '.' '.' 222 ++
            -- loop until you find 0 or 1 before ','
            -- replace remaining bit (0/1) to '_'
            -- because we should use ',' as stopping point
            loopRight 222 "ab_" ++
            goRight 222 '1' '_' 222 ++
            goRight 222 '0' '_' 222 ++
            -- if you see ',' need to go back to transition and check direction bit
            goRight 222 ',' ',' 223 ++
            loopLeft 223 "_,ab@.01" ++
            goLeft 223 '#' '#' 224 ++
            loopLeft 224 "01#,._" ++ 

            -- finished updating the character, go back to the goLeft, goRight instruction
            goRight 224 'a' 'a' 225 ++ -- HERE !#!R# should this be 'a' '0' or 'a' 'a' ?
            goRight 224 'b' 'b' 226 ++ -- HERE !FWE!;LFM2
            loopRight 225 "01,.@_" ++ --path for goRight
            loopRight 226 "01,.@_" ++ --path for goLeft
            goRight 225 '#' '#' 227 ++
            goRight 226 '#' '#' 228 ++
            loopRight 227 "01@_,." ++
            loopRight 228 "01@_,." ++
            goRight 227 'a' '0' 229 ++
            goRight 227 'b' '1' 229 ++
            goRight 228 'a' '0' 230 ++
            goRight 228 'b' '1' 230 ++
            loopRight 229 "01@_." ++
            loopRight 230 "01@_." ++
            goRight 229 'a' '0' 229 ++
            goRight 229 'b' '1' 229 ++
            goRight 230 'a' '0' 230 ++
            goRight 230 'b' '1' 230 ++
            goRight 229 ',' ',' 231 ++ -- path for goRight
            goRight 230 ',' ',' 232 ++ -- path for goLeft
            
            -- finished putting as and bs back, now go right or left to write new current state
            goRight 231 '@' 'w' 233 ++ -- case for a
            loopLeft 232 "01._@" ++ -- case for b
            goLeft 232 ',' ',' 234 ++
            loopLeft 234 "01ab@_." ++
            goRight 234 ',' ',' 235 ++
            goRight 235 '@' 'w' 233 ++
            goRight 235 '0' 'w' 233 ++
            goRight 235 '1' 'w' 233 ++
            -- now go back to current state in transition function from 233
            loopLeft 233 "01,#._w@" ++
            goRight 233 'a' '0' 236 ++
            goRight 233 'b' '1' 236 ++
            goRight 236 '.' '.' 237 ++
            -- now begin writing current state to reserved spot at w
            goRight 237 '0' 'a' 238 ++
            goRight 237 '1' 'b' 239 ++
            loopRight 238 "01_@#,." ++
            loopRight 239 "01_@#,." ++
            goLeft 238 'w' 'a' 240 ++
            goLeft 239 'w' 'b' 240 ++
            loopLeft 240 "01_@ab.," ++ -- sfsflnskf removed ab
            goLeft 240 '#' '#' 300 ++
            loopLeft 300 "01,.#" ++
            goRight 300 'a' '0' 241 ++
            goRight 300 'b' '1' 241 ++
            goRight 241 '0' 'a' 242 ++
            goRight 241 '1' 'b' 243 ++
            --goLeft 241 '@' '@' 300 ++
            --goLeft 300 '1' 'b' 246 ++
            --goLeft 300 '0' 'a' 246 ++ 
            goRight 241 '.' '.' 246 ++ -- finished processing, but need to go put any leftover 0s and 1s to @s
            --
            loopRight 246 "01_@#,." ++
            goRight 246 'a' 'a' 247 ++
            goRight 246 'b' 'b' 247 ++
            loopRight 247 "ab@" ++ -- ??
            goRight 247 '1' '@' 247 ++
            goRight 247 '0' '@' 247 ++
            goRight 247 '.' '.' 248 ++ -- now actually done
            loopLeft 248 "01ab@_,.#" ++
            goRight 248 '?' '?' 249 ++
            loopRight 249 "01" ++
            goRight 249 '#' '#' 6 ++ -- go back to check final states
            --
            loopRight 242 "01_@#,." ++
            loopRight 243 "01_@#,." ++
            goRight 242 'a' 'a' 244 ++
            goRight 242 'b' 'b' 244 ++
            goRight 243 'a' 'a' 245 ++
            goRight 243 'b' 'b' 245 ++
            goRight 244 '@' 'a' 240 ++
            goRight 244 '0' 'a' 240 ++
            goRight 244 '1' 'a' 240 ++
            goRight 245 '@' 'b' 240 ++
            goRight 245 '0' 'b' 240 ++
            goRight 245 '1' 'b' 240



