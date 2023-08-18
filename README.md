# Enigma Machine and British Bombe Recreation in Haskell

This repository contains Haskell implementations for simulating the Enigma machine and the British Bombe. Created by Jack Marchant in 2022 for a university course assignment.

## Overview

### Enigma Machine

The Enigma machine was an encryption device used by the Germans during World War II. The simulator in this repository allows for encoding of strings of capital letters using the provided configuration.

There are two variations of the Enigma provided:

1. `SimpleEnigma`: A basic Enigma simulator without plugboard (Stecker) functionality.
2. `SteckeredEnigma`: A version of the Enigma simulator with plugboard functionality.

The main function to encode messages is `encodeMessage` which takes a string and an `Enigma` configuration and returns the encoded string.

### British Bombe

The British Bombe was a machine developed by Alan Turing and his team during World War II to help decipher Enigma-encrypted messages. The simulator of the bombe cannot be posted in this repository for reasons of academic integrity, however I give extensive time to the logic of how it would work in the Testing document also included in the repository.

## How to Use

1. Clone the repository.
2. Ensure you have Haskell and the GHC (Glasgow Haskell Compiler) installed.
3. Load the `Enigma.hs` file in GHCi.
4. Configure your Enigma settings (rotors, reflectors, offsets, and stecker).
5. Use the `encodeMessage` function to encrypt a message of UPPERCASE ENGLISH CHARACTERS.
6. Use the `encodeMessage` function again with the same enigma configuration to decrypt that previously encrypted message.

## Files

- `Enigma.hs`: Contains the main code for the Enigma machine.
- `Main.hs`: Contains some test cases and default configurations for testing purposes.
- `Enigma_Machine_Testing`: Document outlining Design of the Enigma Machine Recreation, LongestMenu Function, the Bombe recreation; as well as testing for the Enigma Machine Recreation and the LongestMeny Function.
## Example

Using a `SimpleEnigma` configuration:

```haskell
let enigmaConfig = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (0, 0, 0)
let message = "HELLO"
let encodedMessage = encodeMessage message enigmaConfig
let decodedMessage = encodeMessage encodedMessage enigmaConfig //This will give original message again
