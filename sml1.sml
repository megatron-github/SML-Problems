(*catstrings = fn string list -> string*)
(*Concatenate all the strings in the string list.*)
fun catstrings [] = ""
  | catstrings (a::rest) = a ^ catstrings rest;

(*remov = fn 'a ('a * 'b -> bool) -> 'b list -> 'b list
remov take a wanted-to-remove item, a comparing predicate, and a list, then
it remove all the items in the list that are applied to the predicate and
wanted-to-remove item*)
fun remov _ _ [] = []
  | remov item pred (a::rest) =
    if pred (item, a) then remov item pred rest
    else a::remov item pred rest;

(*removedups = fn ('a * 'a -> bool) -> 'a list -> 'a list*)
(*Remove all subsequent duplicates from a list. Curried.
The first parameter is a function that returns true if items are equal.
The result is a list of unique elements.*)
fun removedups _ [] = []
  | removedups pred (a::rest) = a::removedups pred (remov a pred rest);

(*flatten = fn 'a list list -> 'a list*)
(*turn a nested list into a single list*)
fun flatten [] = []
  | flatten (a::rest) = a @ flatten rest;

(*int2char = fn int -> char*)
(*convert given integer into a string of that integer*)
fun int2char i = chr (i + 48);

(*int2charlist = fn int -> char list*)
(*create a list of digit characters that made up the given integer*)
fun int2charlist 0 = []
  | int2charlist i = [int2char (i mod 10)] @ int2charlist (i div 10);

(*inttostring = fn int -> string*)
(*It works like Int.toString: turn an integer into a string of that integer*)
fun inttostring 0 = "0"
  | inttostring e =
  if e < 0 then "~" ^ implode (rev (int2charlist (e * ~1)))
  else implode (rev (int2charlist e));

(*place = fn ('a * int * 'a list) -> 'a list*)
(*place given item into a given list at a given index*)
fun place (item, _, []) = [item]
  | place (item, 0, lst) = item::lst
  | place (item, i, (a::rest)) = a::place (item, (i - 1), rest);

(*places = fn ('a * int * 'a list) -> 'a list list*)
(*create a nested list of ways that an item can place into a give list
with help from the length of the given list*)
fun places (item, _, []) = [[item]]
  | places (item, 0, lst) = [item::lst]
  | places (item, i, lst) =
      [place (item, i, lst)] @  places (item, (i - 1), lst);

(*allplaces = fn 'a -> 'a list -> 'a list list*)
(*For example: allplaces 1 [2, 3] is [[1, 2, 3], [2, 1, 3], [2, 3, 1]].*)
fun allplaces item [] = [[item]]
  | allplaces item lst = rev (places (item, length lst, lst));

(*We can use the functions allplaces creates perms rather than to create more 
complicated functions to accomodate perms.*)

(*perms = fn 'a list -> 'a list list*)
(*Produce all permutations of the parameter: create a nested of permutations
that show how all items in a given list can be arranged*)
fun perms [] = [[]]
  | perms alist = flatten (map (allplaces (hd alist)) (perms (tl alist)));

(*is_member = fn ('a * 'a list) -> bool*)
(*check if a given item is in a given list*)
fun is_member (item, []) = false
  | is_member (item, a::rest) = if a = item then true
                                else is_member (item, rest);

(*has_digit =fn char list -> bool*)
(*check if a list of characters has digit characters*)
fun has_digit [] = false
  | has_digit (a::rest) =
    let
      val digits = [#"0", #"1", #"2", #"3", #"4", #"5", #"6", #"7", #"8", #"9"]
    in if is_member (a, digits) andalso length (rest) = 0 then true
       else is_member (a, digits) andalso has_digit rest end;

(*(BIG_converter = fun int list -> char list)*)
(*convert a list of integer into a list of digit characters*)
fun BIG_converter [a] = (map int2char [a])
  | BIG_converter lst =
    let val b::rest = (rev lst)

    (*BIG_converter also eliminate the zero:
    of [#"0", #"1", #"0", #"0"] => 0100 for exmaple*)
    in if b = 0 andalso length rest > 0 then BIG_converter (rev rest)
       else (map int2char (b::rest)) end;

(*is_ValidBigInt = fn char list -> bool*)
(*check if a given string in a valid string of a (Big) integer*)
(*helper for creating a bigint*)
fun is_ValidBigInt [] = false

  (*negative integer string is a valid (Big) integer*)
  | is_ValidBigInt (a::rest) = has_digit (a::rest);

(*declare the datatype of an bigint*)
datatype bigint = BIG of string;

(*error catch for invalide bigint*)
exception InvalidBigInt;

(*val makebigint : string -> bigint*)
(*Checks validity of a string raising InvalidBigInt if invalid.*)
fun makebigint "" = raise InvalidBigInt    (* catching invalid bigint*)
  | makebigint "~0" = raise InvalidBigInt
  | makebigint str =
    let val a::rest = explode str
    in if a = #"0" andalso is_ValidBigInt rest then raise InvalidBigInt
       else if a = #"~" andalso is_ValidBigInt rest then BIG str
       else if is_ValidBigInt (a::rest) then BIG str
       else raise InvalidBigInt end;

(*(dec_add = fn (int * int) -> (int * int)*)
(*helper function for adding two number*)
(*return the sum of two integer and its carry (ex: 9 + 8 = 7 and carry 1)*)
fun dec_add(v1, v2, carry) =
  let val sum = v1 + v2 + carry
  in (sum mod 10, sum div 10) end;

(*big_add_helper = fn (int * int list * int list) -> int list*)
(*helper function for adding two bigint*)
(*create a list of integer that made up the sum of two bigints*)
(*ex: [1, 2, 3] + [2, 3, 4] = [3, 5, 7]*)
fun big_add_helper (0, [], []) = []
  | big_add_helper (1, [], []) = [1]
  | big_add_helper (c, [], list) = big_add_helper (c, [0], list)
  | big_add_helper (c, list, []) = big_add_helper (c, list, [0])
  | big_add_helper (c, a::resta, b::restb) =
    let val (result, carry) = dec_add (a, b, c)
    in result::big_add_helper (carry, resta, restb) end;

(*dec_sub = fn (int * int list * int list) -> int list*)
(*helper function for subtracting two numbers*)
(*return the difference between two integer and its borrow*)
(*ex: 17 - 8 = 9 and borrow 1*)
fun dec_sub (v1, v2, borrow) =
  let val diff = v1 - v2 - borrow
  in if diff < 0 then (10 + diff, 1) else (diff, 0) end;

(*error catch for negative differences*)
exception Neg;

(* big_sub_helper (borrowin, list1, list2 = difflist *)
(*helper function for subtracting two bigint*)
(*create a list of integer that made up the differences of two bigints*)
(*ex: [3, 5, 7] - [1, 2, 3] = [2, 3, 4]*)
fun big_sub_helper (0, [], []) = []
  | big_sub_helper(1, [], []) = raise Neg
  | big_sub_helper(b, [], list) = big_sub_helper(b, [0], list)
  | big_sub_helper(b, list, []) = big_sub_helper(b, list, [0])
  | big_sub_helper(b, x::restx, y::resty) =
    let val (result, borrow) = dec_sub (x,y,b)
    in result::big_sub_helper (borrow, restx, resty) end;

(*has_sign = fun char -> bool*)
(*helper function for bigint arithmetic: checking if a bigint is negative*)
fun has_sign #"~" = true
  | has_sign _ = false;

(*char2int = fun char -> int*)
(*converting a digit character into an integer*)
fun char2int c = ord c - ord #"0";

(*cmp_digit = fn (bool * int list * int list) -> order*)
(*helper function for comparing two int list (left to right, int by int)
with accounting for the sign of the integer made up by this int list*)
fun cmp_digit (_, [], []) = EQUAL

  (*smaller positive integer (or bigger negative integer)
  has less item in int list*)
  | cmp_digit (signed, a::rest, []) = if signed then LESS else GREATER
  | cmp_digit (signed, [], b::rest) = if signed then GREATER else LESS

  (*if both integer has the same length, then consider:*)
  | cmp_digit (signed, a::resta, b::restb) =

    (*smaller negative integer has greater most significant digit*)
    if signed andalso a > b then LESS

    (*larger negative integer has smaller most significant digit*)
    else if signed andalso a < b then GREATER

    (*bigger positive integer has greater most significant digit*)
    else if a > b then GREATER

    (*smaller positive intger has smaller most significant digit*)
    else if a < b then LESS

    (*if most significant digit (the left most) of two list are equal,
    comp the next digit to the right from the two lists*)
    else cmp_digit (signed, resta, restb);

(*big_cmp_helper = fn (bool * int list * int list) -> order*)
(*helper function to comparing two bigints by break comparing the length
and each digit of the two bigints*)
fun big_cmp_helper (_, [], []) = EQUAL
  | big_cmp_helper (signed, lista, listb) =

        (*get length of the two bigints*)
    let val left = length lista
        val right = length listb

       (*consider: smaller negative integer has bigger size*)
    in if signed andalso left > right then LESS

       (*bigger negative integer has smaller size*)
       else if signed andalso left < right then GREATER

       (*bigger positive integer has greater size*)
       else if left > right then GREATER

       (*smaller positive integer has smaller size*)
       else if left < right then LESS

       (*if two bigints has same size,
       then check each digit in the two bigint*)
       else cmp_digit (signed, lista, listb) end;

(*compare = fn : bigint * bigint -> order*)
(*function allows to compare (left to right) two bigints together:
if left hand bigint is lesser than right hand bigint, function returns LESS;
if left hand bigint is greater than right hand bigint, function returns GREATER;
else function returns EQUAL*)
fun compare (BIG bint1, BIG bint2) =
  let val a::resta = explode bint1
      val b::restb = explode bint2
      val signa = has_sign a
      val signb = has_sign b

     (*if two bigints are positive then use bigint comparing helper to compare
     the two bigint togther*)
  in if (signa = true andalso signb = true)
      then big_cmp_helper (true, map char2int resta, map char2int restb)

     (*if two bigints are negative then use bigint comparing helper to compare
     the two bigint togther*)
     else if (signa = false andalso signb = false)
      then big_cmp_helper (false, map char2int (a::resta),
                           map char2int (b::restb))

     (*if only one of the two bigints has sign, then whichever bigint signed
     will be the smaller of the two*)
     else if (signa = true andalso signb = false) then LESS
     else GREATER end;

(*add = fn : bigint * bigint -> bigint*)
(*add function that allow bigint arithmetic with operation left to right*)
fun add (BIG "0", BIG bint2) = BIG bint2
  | add (BIG bint1, BIG "0") = BIG bint1
  | add (BIG bint1, BIG bint2) =

      (* turn the bigints a digit character list*)
  let val a::resta = explode bint1
      val b::restb = explode bint2

      (*check if any of the bigints is signed*)
      val signa = has_sign a
      val signb = has_sign b

     (*if two bigints are negative: then add the two bigints together (with
     their sign eliminated), then add the sign back when the sum is achieved
     -10 + -9 = -19 for example*)
  in if (signa = true andalso signb = true)
     then BIG ("~" ^ implode
            (BIG_converter
              (big_add_helper (0, rev (map char2int resta),
                               rev (map char2int restb)))))

     (*if two bigints are positive: then add the two bigints together by convert
     them into two integer list and add in the style of reversed list
     10 + 9 = 19 for example*)
     else if (signa = false andalso signb = false)
     then BIG (implode
            (BIG_converter
              (big_add_helper (0, rev (map char2int (a::resta)),
                               rev (map char2int (b::restb))))))

     (*if first bigint is negative and second bigint is positive,
     then use bigint subtraction helper to: second bigint - first bigint*)
     else if (signa = true andalso signb = false)

     (*sum of different signed integers depended on the value of the integers*)
     then if compare (BIG (implode resta), BIG bint2) = GREATER

          (*for cases such as: -10 + 9 = 9 - 10 = -1*)
          then BIG ("~" ^ implode
                 (BIG_converter
                   (big_sub_helper (0, rev (map char2int (resta)),
                                    rev (map char2int (b::restb))))))

          (*for cases such as -9 + 10 = 10 - 9 = 1*)
          else BIG (implode
                 (BIG_converter
                   (big_sub_helper (0, rev (map char2int (b::restb)),
                                    rev (map char2int resta)))))

      (*if first bigint is positive and second bigint is negative,
      then use bigint subtraction helper to: first bigint - second bigint*)
      else if compare (BIG bint1, BIG (implode restb)) = LESS

           (*for cases such as 9 + -10 = 9 - 10 = -1*)
           then BIG ("~" ^ implode
                  (BIG_converter
                    (big_sub_helper (0, rev (map char2int (restb)),
                                     rev (map char2int (a::resta))))))

           (*for cases such as 10 + -9 = 10 - 9 = 1                                                         *)
           else BIG (implode
                  (BIG_converter
                    (big_sub_helper (0, rev (map char2int (a::resta)),
                                     rev (map char2int (restb)))))) end;

(*sub = fn : bigint * bigint -> bigint*)
(*subtract function that allow bigin arithmetic with operation left to right*)
fun sub (BIG bint1, BIG "0") = BIG bint1
  | sub (BIG bint1, BIG bint2) =
  let val b::restb = explode bint2
      val signa = has_sign (hd (explode bint1))
      val signb = has_sign b

     (*if two bigints are negative, then:
     second bigint - first bigint or -a - -b = -a + b*)
  in if (signa = true andalso signb = true)
      then add (BIG bint1, BIG (implode restb))

     (*if two bigints are positive, then
     first bigint - second bigint or a - b = a + -b*)
     else if (signa = false andalso signb = false)
        then add (BIG bint1, BIG ("~" ^ implode (b::restb)))

     (*if first bigint is negative and second bigint is positive, the
     first bigint add negated second bigint or -a - b = -a + -b*)
     else if (signa = true andalso signb = false)
      then add (BIG bint1, BIG ("~" ^ implode (b::restb)))

     (*if first bigint is positive and second bigint is negative, then
     first bigint plus second bigint or a - -b = a + b*)
     else add (BIG bint1, BIG (implode restb)) end;

(*shifttenth = fn bigint -> bigint*)
(*shifttenth function take a bigint a return bigint * 10*)
fun shifttenth (BIG "0") = BIG "0"
  | shifttenth (BIG bint) = BIG (implode (rev (#"0"::(rev (explode bint)))));

(*removlast = fn  char list -> char list*)
(*removlast take a charlist and return a charlist with the last item removed*)
fun removlast [] = [#"0"]  (*last item in digit char list is a character zero*)
  | removlast [a] = [#"0"] (*last item in digit char list is a character zero*)
  | removlast lst =
    let val (_::rest) = rev lst in (rev rest) end;

(*big_mul_helper = fn (bigint * bigint) -> bigint*)
fun big_mul_helper (BIG "0", BIG "0") = BIG "0"     (*0 * 0 = 0*)
  | big_mul_helper (BIG _, BIG "0") = BIG "0"       (* anything multiply*)
  | big_mul_helper (BIG "0", BIG _) = BIG "0"       (*by zero is zero*)
  | big_mul_helper (BIG bint1, BIG "1") = BIG bint1 (*multiplicative identiy*)
  | big_mul_helper (BIG "1", BIG bint2) = BIG bint2
  | big_mul_helper (BIG bint1, BIG bint2) =
    let val one = BIG "1"

    (*adding all the product together: 5 * 4 = 5 + 5 * 3 = 5 + 5 + 5 * 2 = ...*)
    (*multiplying bint1 for bint2 times (bint2 as counter, while bint1 multilies
    itself)*)
    in add (BIG bint1, big_mul_helper (BIG bint1, sub (BIG bint2, one))) end;

(*Splitting integer when multiply:
  316 * 152 = (316 * 100) + (316 * 50) + (316 * 2)*)

(*mul = fn : bigint * bigint -> bigint*)
(*multiply function that allow bigint arithmetic (left to right)*)
fun mul (BIG "0", BIG "0") = BIG "0"      (*0 * 0 = 0*)
  | mul (BIG _, BIG "0") = BIG "0"        (* anything multiply*)
  | mul (BIG "0", BIG _) = BIG "0"        (*by zero is zero*)
  | mul (BIG bint1, BIG "1") = BIG bint1  (* one is multiplicative identiy*)
  | mul (BIG "1", BIG bint2) = BIG bint2
  | mul (BIG bint1, BIG bint2) =
      let val a::resta = explode bint1
          val b::restb = explode bint2
          val signa = has_sign a
          val signb = has_sign b

         (*if two bigints are positive*)
      in if (signa = false andalso signb = false)

         (*finding greater bigint since 4 * 3 = 4 + 4 + 4 or 3 + 3 + 3 + 3
         and the later is a little slower*)
         then if compare (BIG bint1, BIG bint2) = GREATER

              (*this formula right here can be visualize as:
              360*198 = 360*8 + 360*190 =...= 360*8 + 360*90 + 360*100*)
              then add (big_mul_helper (BIG bint1,
                                        BIG (implode [(hd (rev (b::restb)))])),
                        shifttenth (mul (BIG bint1,
                                         BIG (implode (removlast (b::restb))))))

          (*in a world where 360 < 198, then the following formula
          can be seen as 198*360 = 198*0 + 198*60 + 198*300*)
         else add (big_mul_helper (BIG bint2,
                                   BIG (implode [(hd (rev (a::resta)))])),
                   shifttenth (mul (BIG bint2,
                                    BIG (implode (removlast (a::resta))))))

           (*if the two bigints are negative, the multiply the two bigints
           toghether without their signs sine (-) * (-) = (+)*)
      else if (signa = true andalso signb = true)

           (*greater negative integer mean more lengthy (and less
           if the integer was positive) for example: -1 > -100*)
           then if compare (BIG bint1, BIG bint2) = GREATER

                (*using bint1 as counter since greater negative integer means
                less positive integer*)
                then add (big_mul_helper (BIG (implode restb),
                                          BIG (implode [(hd (rev resta))])),

                          (*shiftenth shift the next item in the list up from
                          unit to tenth, and tenth to hundreth*)
                          shifttenth (mul (BIG (implode restb),
                                           BIG (implode (removlast resta)))))

           (*if bint1 is lesser than bint2, then bint2 should be
           a counter since it is smaller when it is positive*)
           else add (big_mul_helper (BIG (implode resta),
                                     BIG (implode [(hd (rev restb))])),
                     shifttenth (mul (BIG (implode resta),
                                      BIG (implode (removlast restb)))))

          (*if the two bigints has different signs, then their product is
          negative. since add function already take care of adding negative
          number, mul ony need to find the smaller bigints (in positive sense)
          then apply function adding products formula*)
      else if (signa = true andalso signb = false)

         (*converting bint1 from negative to positive, to compare length
         and the absolute value of bint1, since the lesser one will be
         a positive counter*)
         then if compare (BIG (implode resta), BIG bint2) = LESS

              (*since adding two signed integer is as same as adding two
              positive integer, we can ignore the sign until we have the
              product. then put the sign back*)
              then add (big_mul_helper (BIG ("~" ^ bint2),
                                        BIG (implode [(hd (rev resta))])),
                        shifttenth (mul (BIG ("~" ^ bint2),
                                         BIG (implode (removlast resta)))))
         else add (big_mul_helper (BIG bint1,
                                   BIG (implode [(hd (rev (b::restb)))])),
                   shifttenth (mul (BIG bint1,
                                    BIG (implode (removlast (b::restb))))))
      else if compare (BIG bint1, BIG (implode restb)) = LESS

           (*because the a law of multiplication: (-) * (+) = (+) * (-) = (-),
           it really does not matter which integer is signed. we can switch
           the sign to a larger integer so that the counter can be the lesser
           integer*)
           then add (big_mul_helper (BIG ("~" ^ bint1),
                                    BIG (implode [(hd (rev restb))])),
                    shifttenth (mul (BIG ("~" ^ bint1),
                                     BIG (implode (removlast restb)))))
      else add (big_mul_helper (BIG bint2,
                               BIG (implode [(hd (rev (a::resta)))])),
               shifttenth (mul (BIG bint2,
                                BIG (implode (removlast (a::resta)))))) end;

(*error catch for illegal bigint arithmetic*)
exception DoesNotExist

(*error catch for bigint arithmetic that will result in a nonbigint*)
exception NotAnInterger

(*pow = fn : bigint * bigint -> bigint*)
(*pow function allow bigint arithmetic, first parameter take an integer and
raise that integer to the power in second parameter*)
fun pow (BIG "0", BIG "0") = raise DoesNotExist (* 0^(0) is illegal*)
  | pow (BIG _, BIG "0") = BIG "1"          (*everything to zero power is one*)
  | pow (BIG "1", BIG _) = BIG "1"          (* one to any power is one*)
  | pow (BIG "0", BIG _) = BIG "0"          (* zero to any power is zero*)
  | pow (BIG bint1, BIG "1") = BIG bint1    (* anything^(1) = itself*)
  | pow (BIG bint1, BIG bint2) =
    let val signb = has_sign (hd (explode bint2))
        val one = BIG "1"

       (*if bigint in second parameter is positive, then multiply bigint
       in first parameter or bigint (in second parameter) times*)
    in if (signb = false) then mul (BIG bint1,
                                    pow (BIG bint1, sub (BIG bint2, one)))

       (*if bigint in second parameter is negative, then it is an error since
       the operation will result in a non-bigint num*)
       else raise NotAnInterger end;
