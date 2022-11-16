{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
  import Data.List
  -- add extra imports if needed, but only standard library functions!

{- Part 1: Simulation of the Enigma -}

  type Rotor = (String, Int) -- the supplied type is not correct; fix it!
  type Reflector = [(Char, Char)] -- the supplied type is not correct; fix it!
  type Offsets = (Int, Int, Int) -- the supplied type is not correct; fix it!
  type Stecker = [(Char, Char)] -- the supplied type is not correct; fix it!
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  encodeMessage :: String -> Enigma -> String
  encodeMessage _ _ = "" -- you need to complete this!

{- You will need to add many more functions. Remember, design it carefully
   - and keep it simple! If things are feeling complicated, step back from your
   - code and think about the design again.
   -}

{- offsetChar takes a character and an offset and returns a character offset by the given amount,
   - can also take a negative offset to go in reverse direction
-}
  offsetChar :: Char -> Int -> Char
  offsetChar char offset = int2let (((alphaPos char) + offset) `mod` 26)

  offsetCharOnRotor :: Char -> Int -> Rotor -> Char
  offsetCharOnRotor char offset rotor = getMappedLetter rotor (mod ((getLetterFromPos (fst rotor) char) - offset) 26)

{- getMappedLetter takes a rotor and returns the letter at the given position in the rotor sequence -}
  getMappedLetter :: Rotor -> Int -> Char
  getMappedLetter rotor position = fst(rotor) !! position

{- takes a string(rotor sequence), a character to search for in that rotor sequence,
  - returns the index of the given char in the sequence
  -}
  getLetterFromPos :: String -> Char -> Int
  getLetterFromPos rotorSequence charSearch = case elemIndex charSearch rotorSequence of
    Just n -> n
    Nothing -> (-1)

{- updateOffsets takes the current offsets and both of the middle and right rotors (to check knock ons), 
  -it then returns the updated offsets based on which ones need to have been knocked on.
-}
  updateOffsets :: Offsets -> Rotor -> Rotor -> Offsets
  updateOffsets (ol, om, or) middleRotor rightRotor 
    | or `mod` 26 == snd(rightRotor) = case() of --middle rotor needs to be updated
      () | (om+1) `mod` 26 == snd(middleRotor) -> (mod26 (ol+1), mod26 (om+1), mod26 (or+1)) -- right rotor needs to be updated
         | otherwise -> (mod26 (ol), mod26 (om+1), mod26 (or+1)) --right rotor need not be updated
    | otherwise = (mod26 ol, mod26 om, mod26(or+1)) -- only the right rotor needs to 

{- offsetCharPos takes a character, the current offsets, and the middle/right rotors and returns the position
  - where the offsetted letter is in the alphabet
-}
  offsetCharPos :: Char -> Offsets -> Rotor -> Rotor -> Int
  offsetCharPos char offsets rm rr = alphaPos (offsetChar char rightmostOffset)
    where
      rightmostOffset = thd (updateOffsets offsets rm rr)

{- encodeCharGoingLeft takes a character, current offsets, all 3 rotors, and returns a char based on it going through all of
 - the rotors one time.
-}
  encodeCharGoingLeft :: Char -> Offsets -> Rotor -> Rotor -> Rotor -> Char
  encodeCharGoingLeft char offsets rl rm rr = getMappedLetter rl (alphaPos (getMappedLetter rm (alphaPos (getMappedLetter rr (offsetCharPos char offsets rm rr))))) -- gets offset of right rotor, offsets char by that amount, passes through rotor

{- encodeChar is my updated function for encoding characters, it takes a character and an array of the rotors and their given offsets
 - to recurse through and return the encoded character
-}
  encodeChar :: Char -> [(Rotor, Int)] -> Char
  encodeChar char [] = '?'
  encodeChar char ((rotor, offset) : xs)
    | xs == [] = getMappedLetter rotor (alphaPos (offsetChar char offset))
    | otherwise = encodeChar (encodeChar char xs) [(rotor, offset)]

  encodeCharReverse :: Char -> [(Rotor, Int)] -> Char
  encodeCharReverse char [] = '?'
  encodeCharReverse char ((rotor, offset) : xs) --given the character to encode and the rotor+accompanying offset on which to do it
    | xs == [] = foobar --once has reached end of rotor array (3rd rotor) get whichever letter is mapped to offset char on the given rotor 
    | otherwise = encodeCharReverse (encodeCharReverse char xs) [(rotor, offset)]
      where 
        foobar = int2let (getLetterFromPos (fst(rotor)) (offsetCharOnRotor char offset rotor))

{- reflectChar takes a character and a reflector and returns the characters reflected partner-}
  reflectChar :: Char -> Reflector -> Char
  reflectChar char [] = '?'
  reflectChar char ((first, second):xs) 
    | first == char = second
    | second == char = first
    | otherwise = reflectChar char xs
    
  encodeString :: String -> Offsets -> (Rotor, Rotor, Rotor) -> String
  encodeString "" _ _ = "jkojk"
  encodeString (x : xs) (oL, oM, oR) (rotorL, rotorM, rotorR)
    | xs == "" = [charEncodedFinal]
    | otherwise = charEncodedFinal : (encodeString xs updatedOffsets (rotorL, rotorM, rotorR))
      where
        reflectedChar = reflectChar (encodeChar x [(rotorL, oL), (rotorM, oM), (rotorR, oR)]) reflectorB --char encoded l->r and the reflected
        charEncodedFinal = encodeCharReverse rROeflectedChar [(rotorR, oR), (rotorM, oM), (rotorL, oL)]
        updatedOffsets = updateOffsets (oL, oM, oR) rotorM rotorR


    

{- Part 2: Finding the Longest Menu -}

  type Menu = [(Char, Integer)] -- the supplied type is not correct; fix it!
  type Crib = String -- the supplied type is not correct; fix it!

  longestMenu :: Crib -> Menu
  longestMenu _ = [('a',1)]

{- Part 3: Simulating the Bombe -}
  
  breakEnigma :: Crib -> Maybe (Offsets, Stecker)
  breakEnigma _ = Nothing

{- Useful definitions and functions -}

   -- substitution cyphers for the Enigma rotors
   -- as pairs of (wirings, knock-on position)
   -- knock-on position is where it will cause the next left wheel to
   -- advance when it moves past this position
 
        --"ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  rotor1=("EKMFLGDQVZNTOWYHXUSPAIBRCJ",17::Int)
  rotor2=("AJDKSIRUXBLHWTMCQGZNPYFVOE",5::Int)
  rotor3=("BDFHJLCPRTXVZNYEIWGAKMUSQO",22::Int)
  rotor4=("ESOVPZJAYQUIRHXLNFTGKDCMWB",10::Int)
  rotor5=("VZBRGITYUPSDNHLXAWMJQOFECK",0::Int)

  {- the standard Enigma reflector (Reflector B)
    swapped A<->Y, B<->R, C<->U,D<->H, E<->Q, F<->S, G<->L, 
            I<->P, J<->X, K<->N, M<->O, T<->Z,V<->W
  -}
  reflectorB= [('A','Y'),
              ('B','R'),
              ('C','U'),
              ('D','H'),
              ('E','Q'),
              ('F','S'),
              ('G','L'),
              ('I','P'),
              ('J','X'),
              ('K','N'),
              ('M','O'),
              ('T','Z'),
              ('V','W')]

  {- alphaPos: given an uppercase letter, returns its index in the alphabet
     ('A' = position 0; 'Z' = position 25)
   -}
  alphaPos :: Char -> Int
  alphaPos c = (ord c) - ord 'A'
{- int2let: given a integer, returns its corresponding letter
     ('A' = position 0; 'Z' = position 25)
   -}
  int2let :: Int -> Char
  int2let n = chr (ord 'A' + n)

  mod26 :: Int -> Int
  mod26 x  = x `mod` 26

  thd :: (Int, Int, Int) -> Int
  thd (a, b, c) = c

  reverseOffsets :: Offsets -> Offsets
  reverseOffsets (a, b, c) = (a * (-1), b * (-1), c * (-1))
