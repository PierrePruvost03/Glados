# Kong Standard Library

This folder contains all definitions of the Kong's Standard Library.

## About

A standard library contains all of a language's basic functions to work on its types.  

## Structure

The Library is divided into multiple sub folders about each part of the library.

## Math

The math part of the library will help you with your Integers and Float numbers.  
Here is the list of funktions, the number and types of arguments, and want tey return :

FunkName (Arguments Types) -> Return type : Description

abs (Int) -> Int : calculates absolute value of an Integer

fabs (Float) -> Float : calculates absolute value of a Float

pow (Int N, Int M) -> Int : calculates N to the power of M for Integers

powf (Float, Int) -> Float : calculates N to the power of M for floating point numbers

mod (Int N, Int M) -> Int : calculates the remainder of the Euclidian division of N by M

fmod (Float, Float) -> Float : calculates the remainder of the Euclidian division of N by M

min (Int, Int) -> Int : returns the minimum value between two integers

fmin (Float, Float) -> Float : returns the minimum value between two floating point numbers

max (Int, Int) -> Int : returns the maximum value between two integers

fmax (Float, Float) -> Float : returns the maximum value between two floating point numbers

exp (Int N) -> Float : calculates an approximation of Euler's number to the power on N

log (Int) -> Float : calculates the natural logarithm of an integer

sqrt (Float) -> Float : calculates the square root of a number

ceil (Float) -> Float : round up a floating point number to its closest natural value

floor (Float) -> Float : round down a floating point number to its closest natural value

round (Float) -> Float : round a floating point number to its closest natural value

## Khar

The Khar part of the library contains operations to check kharacters' qualities.  
Here is the list of funktions, the number and types of arguments, and want tey return :

isDigit (Khar) -> Bool : returns True if the kharacter is a digit, False otherwise

isAlpha (Khar) -> Bool : returns True if the kharacter is a letter, False otherwise

isAlphaNum (Khar) -> Bool : returns True if the kharacter is a digit or a letter, False otherwise

isLower (Khar) -> Bool : returns True if the kharacter is a lowercase letter, False otherwise

isUpper (Khar) -> Bool : returns True if the kharacter is an uppercase letter, False otherwise

toUpper (Khar) -> Khar : converts a lowercase letter into an uppercase

toLower (Khar) -> Khar : converts an uppercase letter into a lowercase

## String

putStr(String str) -> Int : prints a string and returns the number of characters that were printed

putStrLn(String str) -> Int : prints a string with a newline and returns the number of characters that were printed
