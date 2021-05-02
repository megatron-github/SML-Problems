# SML_problems

The project will implement some functions in standard ML: <br />
1. catstrings = fn string list -> string <br />
Concatenate all the strings in the string list.

2. removedups = fn ('a * 'a -> bool) -> 'a list -> 'a list <br />
Remove all subsequent duplicates from a list. Curried. The first parameter is a function that returns true if items are equal. The result is a list of unique elements. 

3. flatten = fn 'a list list -> 'a list <br />
Flatten a double list into a single list

4. inttostring = fn int -> string <br />
It works like Int.toString.

5. allplaces = fn 'a -> 'a list -> 'a list list <br />
For example: allplaces 1 [2, 3] is [[1, 2, 3], [2, 1, 3], [2, 3, 1]].

6. perms = fn 'a list -> 'a list list <br />
Produce all permutations of the parameter.

Given datatype bigint = BIG of string, assume that each string is either (i) zero ("0") or (ii) a nonempty sequence of digits that does not start with 0, optionally negated with a negation sign as its first character. We are going to write some functions supporting arithmetic BIG integers. <br />

7. val makebigint : string -> bigint <br />
Checks validity of a string raising InvalidBigInt if invalid.

8. val add = fn : bigint * bigint -> bigint <br />
Multiply two BIG integers

9. val sub = fn : bigint * bigint -> bigint <br />
Subtract two BIG integers

10. val mul = fn : bigint * bigint -> bigint <br />
Multiply two BIG integers

11. val pow = fn : bigint * bigint -> bigint <br />
Compute a BIG integer with the power of a BIG integer in logarithmic time

12. val compare = fn : bigint * bigint -> order <br />
Compare two BIG integers
