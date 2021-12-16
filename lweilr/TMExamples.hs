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
            -- begin with two separate cases
            -- one where encoded input state starts with 0
            -- and one where encoded input state starts with 1
            goRight 1 '0' 'a' 2 ++
            goRight 1 '1' 'b' 3 ++
            -- finished processing the input state if we see #, so need to go to next routine via 6
            goRight 1 '#' '#' 6 ++
            -- otherwise, loop to start of input string using two paths (2 and 3 for a and b)
            loopRight 2 "01.,#ab" ++
            loopRight 3 "01.,#ab" ++
            -- we need to skip the first set of @ signs because they correspond to the encoded machine's
            -- leftend marker, which is not the first input bit, but there may be a transition function which uses it later
            goRight 2 '@' '@' 4 ++
            goRight 3 '@' '@' 5 ++
            loopRight 4 "@.01" ++
            loopRight 5 "@.01" ++
            goRight 4 ',' ',' 7 ++
            goRight 5 ',' ',' 8 ++
            loopRight 7 "ab" ++
            loopRight 8 "ab" ++
            -- now write the first bit of the start state from path a or b (7 or 8)
            goRight 7 '@' 'a' 9 ++
            goRight 8 '@' 'b' 9 ++
            -- go back to the beginning of the tape to find the next bit to write
            loopLeft 9 "01,.@ba#" ++
            goRight 9 '?' '?' 10 ++
            -- loop over any bits in the start state that we have already processed
            -- and put back the bit that we just finished
            -- go back to state one to continue this routine with the next bits
            loopRight 10 "01" ++
            goRight 10 'a' '0' 1 ++
            goRight 10 'b' '1' 1 ++
            ---- END WRITING THE START STATE TO THE LEFT OF THE FIRST INPUT BIT

            ---- BEGIN CHECK FINAL STATES
            -- first need to get to current state, which was conveniently marked with as and bs
            -- on the way, go ahead and mark the first bit of the first final state
            goRight 6 '0' 'a' 11 ++
            goRight 6 '1' 'b' 12 ++
            -- if end of final states is found (#) move on to next routine via state 110
            goRight 6 '#' '#' 110 ++
            -- otherwise, loop until we get to the current state
            loopRight 11 "01cd,.#" ++
            loopRight 12 "01cd,.#" ++
            goRight 11 '@' '@' 100 ++
            goRight 12 '@' '@' 101 ++
            goRight 11 'b' 'b' 15 ++
            goRight 12 'a' 'a' 15 ++
            loopRight 100 "@.01" ++
            loopRight 101 "@.01" ++
            goRight 100 ',' ',' 102 ++
            goRight 101 ',' ',' 103 ++
            loopRight 102 "01cd,.#" ++
            loopRight 103 "01cd,.#" ++
            -- should now be at beginning of current state
            -- use c, d as a special placeholder for bits that are being processed
            goRight 102 'a' 'c' 13 ++
            goRight 103 'b' 'd' 14 ++
            -- if end of state reached (@), or incorrect bit seen, the mismatch
            -- begin subroutine for mismatch, puts bits back and moves on to check next final state
            goRight 102 '@' '@' 15 ++
            goRight 103 '@' '@' 15 ++
            goRight 102 'b' 'b' 15 ++
            goRight 103 'a' 'a' 15 ++
            loopLeft 15 "01,.@abcd#" ++
            goRight 15 '?' '?' 16 ++
            loopRight 16 "01,.@#" ++
            goRight 16 'a' '0' 17 ++
            goRight 16 'b' '1' 17 ++
            loopRight 17 "01" ++
            goRight 17 ',' ',' 6 ++
            -- end subroutine 
            -- else, back to case where we saw the correct bit, go back to final states
            -- put checked bits back, and go back to the beginning of this routine (6) to check next bits
            loopLeft 13 "01,.@abcd" ++
            loopLeft 14 "01,.@abcd" ++
            goLeft 13 '#' '#' 18 ++
            goLeft 14 '#' '#' 18 ++
            loopLeft 18 "01,.@#" ++
            goRight 18 'a' '0' 6 ++
            goRight 18 'b' '1' 6 ++
            -- case where final state matches current state, go to accepting state
            goRight 18 ',' ',' 1000 ++
            ---- END CHECK FINAL STATES


            -- begin subroutine for putting c, d back to a, b in current state
            loopRight 110 "01,.@#_" ++
            goRight 110 'c' 'a' 111 ++
            goRight 110 'd' 'b' 111 ++
            goRight 110 'a' 'a' 111 ++
            goRight 110 'b' 'b' 111 ++
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
            -- end subroutine 
            

            ---- BEGIN CHECK TRANSITION STATE
            -- currently at beginning of transition functions, move forward to current state
            -- looping left because routine ended after first bit of the trans functions
            loopLeft 19 "ab,.01@" ++
            goRight 19 '#' '#' 20 ++
            -- on the way, mark the first bit of the transition function state to keep our place    
            goRight 20 '0' 'a' 21 ++
            goRight 20 '1' 'b' 22 ++
            -- case for no more bits to check, use w as a special placeholder so we don't lose the transition function
            -- begin subroutine for putting c and d back to a and b
            goRight 20 '.' 'w' 604 ++
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
            -- end subroutine
            -- otherwise, we went to the next bit of the current state, 
            -- if match, mark it with c, d
            loopRight 21 "01,.@_cd#" ++
            loopRight 22 "01,.@_cd#" ++
            goRight 21 'a' 'c' 24 ++
            goRight 22 'b' 'd' 25 ++
            -- if no match, need to go back and proceed to check next transition function
            -- begin subroutine for putting bits back and moving on to next transition function
            goRight 21 'b' 'b' 800 ++
            goRight 22 'a' 'a' 800 ++
            loopLeft 800 "abcd" ++
            goRight 800 ',' ',' 801 ++ 
            loopRight 801 "ab@" ++
            goRight 801 'c' 'a' 801 ++
            goRight 801 'd' 'b' 801 ++
            goRight 801 '.' '.' 26 ++
            loopLeft 26 "01,.@abcd_" ++
            goLeft 26 '#' '#' 27 ++
            loopLeft 27 "01,.@#" ++
            goRight 27 'a' '0' 28 ++
            goRight 27 'b' '1' 28 ++
            loopRight 28 "01." ++
            goRight 28 ',' ',' 20 ++ -- start over with next transition
            -- end subroutine
            -- back to case where there was a match
            -- go back to transition function, flip checked bits back, 
            -- and go back to the start of this routine (20) to check next bits
            loopLeft 24 "01,.@abcd_" ++
            loopLeft 25 "01,.@abcd_" ++
            goLeft 24 '#' '#' 29 ++
            goLeft 25 '#' '#' 30 ++
            loopLeft 29 "01,.@#" ++
            loopLeft 30 "01,.@#" ++
            goRight 29 'a' '0' 20 ++
            goRight 30 'b' '1' 20 ++
            ---- END CHECK TRANSITION STATE
            
            ---- BEGIN CHECK INPUT CHARACTER
            -- begin by marking the first transition function bit (the current input bit we are processing)
            goRight 31 '0' 'a' 32 ++
            goRight 31 '1' 'b' 32 ++
            -- then go forward and mark the first bit of the curernt input in the part of the tape with the encoded 
            -- machine's input
            loopRight 32 "01,.#@_" ++ 
            goRight 32 'a' 'a' 33 ++
            goRight 32 'b' 'b' 33 ++
            loopRight 33 "@ab" ++
            goRight 33 '.' '.' 34 ++
            goRight 34 '0' 'a' 35 ++
            goRight 34 '1' 'b' 35 ++
            -- now that this bit is marked we can go back to the transition funciton and look for a match
            loopLeft 35 "ab.01@,_" ++
            goLeft 35 '#' '#' 36 ++
            loopLeft 36 "01,.@#" ++
            -- we found the bit we marked
            goRight 36 'a' 'a' 37 ++
            goRight 36 'b' 'b' 38 ++
            -- now go back to the bit we marked in the encoded input using separate paths for a and b to check for match
            -- need to first loop over the current state
            loopRight 37 "01,.#@_" ++
            loopRight 38 "01,.#@_" ++
            goRight 37 'a' 'a' 39 ++
            goRight 37 'b' 'b' 39 ++
            goRight 38 'a' 'a' 40 ++
            goRight 38 'b' 'b' 40 ++
            loopRight 39 "@ab" ++
            loopRight 40 "@ab" ++
            -- now we are right before the input we're checking
            goRight 39 '.' '.' 41 ++
            goRight 40 '.' '.' 42 ++
            -- loop over any bits we already processed
            loopRight 41 "01" ++
            loopRight 42 "01" ++
            -- case where bit does match, flip it and move forward
            goRight 41 'a' '0' 43 ++
            goRight 42 'b' '1' 43 ++
            -- case where bit doesn't mactch bit doesn't match, put it back and go back to check the next transition function
            goRight 41 'b' '1' 26 ++
            goRight 42 'a' '0' 26 ++
            -- we matched a bit, now flip the next bit to check in the next iteration
            goRight 43 '0' 'a' 44 ++
            goRight 43 '1' 'b' 44 ++
            -- or, if there are no more bits to check, then go back to transition function via 60 
            goRight 43 '_' '_' 60 ++ 
            goRight 43 ',' ',' 60 ++ 
            loopLeft 60 "10.,@ab_" ++
            goLeft 60 '#' '#' 61 ++
            loopLeft 61 "10.,@" ++
            goRight 61 'a' '0' 70 ++
            goRight 61 'b' '1' 70 ++
            loopRight 70 "10" ++
            -- found a match, move on to next routine via 200
            goRight 70 '.' '.' 200 ++
            -- otherwise, more bits to check, go back to transition function
            -- flip checked bits, and go back to beginning of routine to check next bits via 37 and 38
            loopLeft 44 "ab01@,._" ++
            goLeft 44 '#' '#' 46 ++
            loopLeft 46 "01#,." ++
            goRight 46 'a' '0' 47 ++
            goRight 46 'b' '1' 47 ++ 
            goRight 47 '0' 'a' 37 ++
            goRight 47 '1' 'b' 38 ++
            -- found a match, move on to next routine via 200
            goRight 47 '.' '.' 200 ++ 
            ---- END CHECK INPUT CHARACTER


            ---- BEGIN WRITE NEW CHARACTER TO TAPE
            -- currently right before the goRight, goLeft instruction bit in the transition functions
            -- go ahead and mark this bit to use as a placeholder
            goRight 200 '1' 'b' 230 ++
            goRight 200 '0' 'a' 230 ++
            -- then skip over the next state in the transition function
            goRight 230 '.' '.' 201 ++
            -- now at the beginning of the character to be written
            loopRight 201 "01" ++
            goRight 201 '.' '.' 202 ++
            -- begin write character subroutine
            goRight 202 '0' 'a' 203 ++
            goRight 202 '1' 'b' 204 ++ -- mark character to be written
            goRight 202 '.' '.' 205 ++ -- if no 0, 1 found, done writing input, move on to next part via 205
            -- loop until we find the place for writing the next character
            loopRight 203 "01.,#@_" ++
            loopRight 204 "01.,#@_" ++
            goRight 203 'a' 'a' 206 ++
            goRight 203 'b' 'b' 206 ++
            goRight 204 'a' 'a' 207 ++
            goRight 204 'b' 'b' 207 ++
            -- loop over bits we've already processed
            loopRight 206 "ab@." ++
            loopRight 207 "ab@." ++
            -- write the bits according to the paths for a and b 
            goRight 206 '0' 'a' 209 ++
            goRight 206 '1' 'a' 209 ++
            goRight 207 '0' 'b' 209 ++
            goRight 207 '1' 'b' 209 ++
            -- go find the placeholder in the transition function
            loopLeft 209 "ab@.01,_" ++
            goLeft 209 '#' '#' 211 ++
            loopLeft 211 "01#,." ++
            -- put the bits back, and repeat for additional bits by returing to state 202
            goRight 211 'a' '0' 202 ++ 
            goRight 211 'b' '1' 202 ++
            ---- END WRITE NEW CHARACTER TO TAPE


            -- begin subroutine for tidying up-
            -- if the input we just wrote uses fewer bits than the character we just wrote, then
            -- use special character _ to pad out remaining bits
            loopRight 205 "01,." ++
            goRight 205 '#' '#' 220 ++
            loopRight 220 "@01_.," ++
            goRight 220 'a' 'a' 221 ++
            goRight 220 'b' 'b' 221 ++
            -- loop until you get to the input character
            -- the place that you just updated 
            loopRight 221 "ab@" ++
            goRight 221 '.' '.' 222 ++
            -- loop until you find 0 or 1 before ','
            -- replace remaining bit (0/1) to '_'
            loopRight 222 "ab_" ++
            goRight 222 '1' '_' 222 ++
            goRight 222 '0' '_' 222 ++
            -- if you see ',' need to go back to transition and check direction bit
            goRight 222 ',' ',' 223 ++
            loopLeft 223 "_,ab@.01" ++
            goLeft 223 '#' '#' 224 ++
            loopLeft 224 "01#,._" ++ 
            -- end subroutine for tidying up


            ---- BEGIN MOVETAPEHEAD ACCORDING TO TRANSITION FUNCTION ROUTINE
            -- THIS ROUTINE ALSO WRITES THE NEW CURRENT STATE
            -- currently at the goLeft, goRight instruction placeholder that we previously marked
            goRight 224 'a' 'a' 225 ++
            goRight 224 'b' 'b' 226 ++
            loopRight 225 "01,.@_" ++ --path for goRight (marked a)
            loopRight 226 "01,.@_" ++ --path for goLeft (marked b)
            goRight 225 '#' '#' 227 ++
            goRight 226 '#' '#' 228 ++
            -- need to loop until we find the current state + input pair in the
            -- encoded input part of the tape
            -- loop over these bits and put them back to zeros and ones
            -- so that we can use a and b for the new current state and input
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
            goLeft 230 ',' ',' 232 ++ -- path for goLeft
            -- finished putting as and bs back, now go right or left to write new current state
            -- use special character w as placeholder for where the new current state will be written
            goRight 231 '@' 'w' 233 ++
            -- if instruction is goLeft, need to loop to the begging of the state to the left of current
            loopLeft 232 "01ab._@#" ++
            goLeft 232 ',' ',' 234 ++
            loopLeft 234 "01ab@_." ++
            goRight 234 ',' ',' 235 ++
            goRight 235 '@' 'w' 233 ++
            goRight 235 '0' 'w' 233 ++
            goRight 235 '1' 'w' 233 ++
            -- now go back to current state in transition function via 223
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
            loopLeft 240 "01_@ab.," ++
            goLeft 240 '#' '#' 300 ++
            loopLeft 300 "01,.#" ++
            goRight 300 'a' '0' 241 ++
            goRight 300 'b' '1' 241 ++
            goRight 241 '0' 'a' 242 ++
            goRight 241 '1' 'b' 243 ++
            -- finished processing, but need to go put any leftover 0s and 1s to @s
            -- go to state 246 for this
            goRight 241 '.' '.' 246 ++
            loopRight 246 "01_@#,." ++
            goRight 246 'a' 'a' 247 ++
            goRight 246 'b' 'b' 247 ++
            loopRight 247 "ab@" ++
            goRight 247 '1' '@' 247 ++
            goRight 247 '0' '@' 247 ++
            -- finished padding out with @s
            -- now if dot is seen we know we have finished and can go check if our new
            -- current state is in the set of final states via 6
            goRight 247 '.' '.' 248 ++
            loopLeft 248 "01ab@_,.#" ++
            goRight 248 '?' '?' 249 ++
            loopRight 249 "01" ++
            goRight 249 '#' '#' 6 ++
            -- otherwise, still have bits to process
            -- repeat routine on next bits via 240 after writing our a or b bit
            loopRight 242 "01_@#,." ++
            loopRight 243 "01_@#,." ++
            goRight 242 'a' 'a' 244 ++
            goRight 242 'b' 'b' 244 ++
            goRight 243 'a' 'a' 245 ++
            goRight 243 'b' 'b' 245 ++
            loopRight 244 "ab" ++
            goRight 244 '@' 'a' 240 ++
            goRight 244 '0' 'a' 240 ++
            goRight 244 '1' 'a' 240 ++
            loopRight 245 "ab" ++
            goRight 245 '@' 'b' 240 ++
            goRight 245 '0' 'b' 240 ++
            goRight 245 '1' 'b' 240
            ---- END MOVETAPEHEAD ACCORDING TO TRANSITION FUNCTION ROUTINE


