{-- Stub for the grading assignment. Fill it in, making sure you use good
 -- functional style, and add comments (including replacing those that are
 -- already here).
--}

module Enigma where
  import Data.Char  -- to use functions on characters
  import Data.Maybe -- breakEnigma uses Maybe type
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

{- getMappedLetter takes a rotor and returns the letter at the given position in the rotor sequence -}
  getMappedLetter :: Rotor -> Int -> Char
  getMappedLetter rotor position = fst(rotor) !! position

  updateOffsets :: Offsets -> Rotor -> Rotor -> Offsets
  updateOffsets (ol, om, or) middleRotor rightRotor 
    | or `mod` 26 == snd(rightRotor) = case() of --middle rotor needs to be updated
      () | (om+1) `mod` 26 == snd(middleRotor) -> (mod26 (ol+1), mod26 (om+1), mod26 (or+1)) -- right rotor needs to be updated
         | otherwise -> (mod26 (ol), mod26 (om+1), mod26 (or+1)) --right rotor need not be updated
    | otherwise = (mod26 ol, mod26 om, mod26(or+1)) -- only the right rotor needs to 


  offsetCharPos :: Char -> Offsets -> Rotor -> Rotor -> Int
  offsetCharPos char offsets rm rr = alphaPos (offsetChar char foobar)
    where
      foobar = thd (updateOffsets offsets rm rr)

  encodeChar :: Char -> Offsets -> Rotor -> Rotor -> Rotor -> Char
  encodeChar char offsets rl rm rr = getMappedLetter rl (alphaPos (getMappedLetter rm (alphaPos (getMappedLetter rr (offsetCharPos char offsets rm rr))))) -- gets offset of right rotor, offsets char by that amount, passes through rotor
    -- where
    --   offsetCharacter = offsetChar char offset
    --   offsetCharacterPos = alphaPos offsetCharacter
    --   offset = thd (updateOffsets offsets rm rr)
          

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
