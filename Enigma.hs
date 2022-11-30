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

  type Rotor = (String, Int)
  type Reflector = [(Char, Char)]
  type Offsets = (Int, Int, Int)
  type Stecker = [(Char, Char)]
  
  data Enigma = SimpleEnigma Rotor Rotor Rotor Reflector Offsets
                | SteckeredEnigma Rotor Rotor Rotor Reflector Offsets Stecker

  encodeMessage :: String -> Enigma -> String
  encodeMessage "" _ = "" -- you need to complete this!
  --Unsteckered
  encodeMessage (x : xs) (SimpleEnigma rotorL rotorM rotorR reflector (oL, oM, oR)) --recurse through chars in string
    | xs == "" = [charEncodedFinal] -- when char is last in string, return just the encoded char
    | otherwise = charEncodedFinal : (encodeMessage xs (SimpleEnigma rotorL rotorM rotorR reflector (uOL, uOM, uOR))) --append encoded chars to return string
      where
        reflectedChar = reflectChar (encodeChar x [(rotorL, uOL), (rotorM, uOM), (rotorR, uOR)]) reflector --char encoded l->r and the reflected
        charEncodedFinal = encodeCharReverse reflectedChar [(rotorR, uOR), (rotorM, uOM), (rotorL, uOL)]
        (uOL, uOM, uOR) = updateOffsets (oL, oM, oR) rotorM rotorR
  --Steckered
  encodeMessage (x : xs) (SteckeredEnigma rotorL rotorM rotorR reflector (oL, oM, oR) stecker)
    | xs == "" = [charEncodedFinal]-- when char is last in string, return just the encoded char
    | otherwise = charEncodedFinal : (encodeMessage xs (SimpleEnigma rotorL rotorM rotorR reflector (uOL, uOM, uOR))) --append encoded chars to return string
      where
        steckeredChar = steckerChar x stecker --stecker character as it is input 
        reflectedChar = reflectChar (encodeChar steckeredChar [(rotorL, uOL), (rotorM, uOM), (rotorR, uOR)]) reflector --char encoded l->r and the reflected
        charEncodedFinal = steckerChar (encodeCharReverse reflectedChar [(rotorR, uOR), (rotorM, uOM), (rotorL, uOL)]) stecker
        (uOL, uOM, uOR) = updateOffsets (oL, oM, oR) rotorM rotorR


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

{- encodeChar is my updated function for encoding characters, it takes a character and an array of the rotors and their given offsets
 - to recurse through and return the encoded character
-}
  encodeChar :: Char -> [(Rotor, Int)] -> Char
  encodeChar char [] = '?'
  encodeChar char ((rotor, offset) : xs)
    | xs == [] = offsetChar (getMappedLetter rotor (alphaPos (offsetChar char offset))) (-offset)
    | otherwise = encodeChar (encodeChar char xs) [(rotor, offset)]

  encodeCharReverse :: Char -> [(Rotor, Int)] -> Char
  encodeCharReverse char [] = '?'
  encodeCharReverse char ((rotor, offset) : xs) --given the character to encode and the rotor+accompanying offset on which to do it
    | xs == [] = reverseEncodedChar --once has reached end of rotor array (3rd rotor) get whichever letter is mapped to offset char on the given rotor 
    | otherwise = encodeCharReverse (encodeCharReverse char xs) [(rotor, offset)]
      where 
        reverseEncodedChar = offsetChar (int2let (getLetterFromPos (fst(rotor)) (offsetChar char offset))) (-offset)

{- reflectChar takes a character and a reflector and returns the characters reflected partner-}
  reflectChar :: Char -> Reflector -> Char
  reflectChar char [] = '?'
  reflectChar char ((first, second):xs) 
    | first == char = second
    | second == char = first
    | otherwise = reflectChar char xs

{- reflectChar takes a character and a reflector and returns the characters reflected partner-}
  steckerChar :: Char -> Stecker -> Char
  steckerChar char [] = char
  steckerChar char ((first, second):xs) 
    | first == char = second
    | second == char = first
    | otherwise = steckerChar char xs


{- Part 2: Finding the Longest Menu -}

  type Menu = [Integer]
  type Crib = (String, String)
  type IndexedCrib = (Int, (Char, Char))
  type IndexedCribList = [(Int, (Char, Char))]


  longestMenu :: Crib -> Menu
  longestMenu _ = [1]
  
  zipWithIndexes :: Crib -> IndexedCribList
  zipWithIndexes (plain, cypher) = zip [0..] (zip plain cypher)

  getBranches:: IndexedCribList -> IndexedCrib -> [IndexedCribList]
  getBranches formattedCrib pairSearchingFor
    | possibleBranches == [] = [[pairSearchingFor]]
    | otherwise = (map (\m -> pairSearchingFor : m) (exploreSubBranches formattedCrib possibleBranches))
    where
      index = fst(pairSearchingFor)
      charSearchingFor = snd(snd(pairSearchingFor))
      possibleBranches = filter (\(i, (p, c)) -> p == charSearchingFor) formattedCrib -- gives possible branches to go down by filtering for letter to go down 

  exploreSubBranches:: IndexedCribList -> IndexedCribList -> [IndexedCribList] --if there is more than one i have to explore the sub branches
  exploreSubBranches _ [] = []
  exploreSubBranches formattedCrib (currentBranch: branchesToExplore) = (getBranches filteredCrib currentBranch) ++ exploreSubBranches filteredCrib branchesToExplore --at the end, on the last branch, we traverse just that branch
      where
        currentIndex = fst(currentBranch)
        filteredCrib = filter (\(i, (p, c)) -> i /= currentIndex) formattedCrib

  crib1 = ("WETTERVORHERSAGEBISKAYA","RWIVTYRESXBFOGKUHQBAISE")


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

  steckerA = [('A','R'),
              ('B','U'),
              ('C','H'),
              ('D','Q'),
              ('E','S'),
              ('T','Z'),
              ('G','N'),
              ('I','X'),
              ('J','V'),
              ('K','P')]

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