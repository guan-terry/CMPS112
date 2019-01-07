(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)
(* Terry Guan 
   cmps112
   teguan@ucsc.edu
*)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))
    

    let rec cmp' list1 list2 = match (list1, list2) with
    | [],[] -> 0
    | list1, list2 ->
        if (car list1) > (car list2) then 1
        else if (car list1) < (car list2) then 0
        else cmp' (cdr list1) (cdr list2)

    let rec removehead0 list1 = match list1 with
    | [] -> []
    | [0] -> []
    | car::cdr ->
         let back = removehead0 cdr
         in match car, back with
         | 0, [] -> []
         | car, back -> car::back
  

    let rec sub' list1 list2 borrow = match (list1, list2, borrow) with
        | list1, [], 0      -> list1
        | [], list2, 0      -> list2
        | list1, [], borrow -> sub' list1 [borrow] 0
        | [], list2, borrow -> sub' [borrow] list1 0
        | car1::cdr1, car2::cdr2, carry ->
      let dif = car1 - car2 - borrow
      in if dif < 0
        then car1 + radix - car2 - borrow::sub' cdr1 cdr2 1
        else car1 - car2 - borrow :: sub' cdr1 cdr2 0

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        if neg1 = neg2
            then Bigint (neg1, add' value1 value2 0)
        else if (neg1 = Pos && neg2 = Neg) then (
            if compare value1 value2 = 1
                then Bigint(Pos, sub' value1 value2 0)
            else
                Bigint(Neg, sub' value2 value1 0)
        )
        else
            if compare value1 value2 = 1
                then Bigint(Neg, sub' value1 value2 0)
            else
                Bigint(Pos, sub' value2 value1 0)
        


    let compare list1 list2 = match (list1, list2) with
    | [], [] -> 1
    | list1, [] -> 1
    | [], list2 -> 0
    | car1::cdr1, car2::cdr2 ->
        if List.length list1 > List.length list2 then 1
        else if List.length list2 > List.length list1 then 0
       (* else if List.length list2 = 1 && List.length list1 = 1
        (* this needs to be fixed *)
            *)
        else cmp' (reverse list1) (reverse list2)


    let sub (Bigint (neg1, value1)) (Bigint (neg2, value2)) = 
    if (neg1 = Pos && neg2 = Pos)
    then (
        if (compare value1 value2) = 1
            then Bigint(Pos, sub' value1 value2 0)
        else
            Bigint(Neg, sub' value2 value1 0)  
    )
    else if (neg1 = Neg && neg2 = Neg)
    then (
        if (compare value1 value2) = 1
        then Bigint(Neg, sub' value1 value2 0)
        else
            Bigint(Pos, sub' value2 value1 0)
    )
    else if (neg1 = Pos && neg2 = Neg)
    then Bigint(Pos, add' value1 value2 0)
    else (* if neg1 = Neg and neg2 = Pos *)
         Bigint(Neg, add' value1 value2 0)


    let rec mul' list1 pow list2 =
   (* printf "%s\n%!" (string_of_bigint (Bigint(Pos,list1)));
    printf "%s\n%!" (string_of_bigint (Bigint(Pos,pow)));
    *)
    if (compare pow list1) = 1
        then list1, [0]
    else let multiplicand, result =
        mul' list1 (add' pow pow 0) (add' list2 list2 0)
    in if (compare pow multiplicand) = 0
        then (removehead0 (sub' multiplicand pow 0)),
             (add' result list2 0)
    else
         multiplicand, result
     

    let mul (Bigint(neg1, value1)) (Bigint (neg2, value2)) =
   (* printf "testing\n";*)
    let _, result = mul' value1 [1] value2 in
    if (neg1 = neg2)
        then Bigint(Pos, result)
    else 
        Bigint(Neg, result)

    let rec div' list1 list2 pow =
    if (compare list2 list1) = 1
        then list1, [0]
    else let remainder, quotient =
        div' list1 (add' list2 list2 0) (add' pow pow 0)
    in if (compare list2 remainder) = 1
        then remainder,quotient
    else 
       (removehead0 (sub' remainder list2 0)), (add' quotient pow 0)
        

    let div (Bigint(neg1,value1)) (Bigint (neg2, value2)) =
    let _,quotient = div' value1 value2 [1] in
    if neg1 = neg2
        then Bigint(neg1, quotient)
    else 
        if (compare value1 value2) = 1
            then Bigint(Pos, quotient)
        else Bigint(Pos, [])

    (* gets the remainder for bigint1
     and bigint2 or bigint1 % bigint2 *)
    let rem (Bigint(neg1,value1)) (Bigint (neg2,value2)) =
    let remainder, _ = div' value1 value2 [1] in
    if neg1 = neg2
        then Bigint(neg1, remainder)
    else 
        if (compare value1 value2) = 1
            then Bigint(Pos, remainder)
        else Bigint(Pos, [])

    (* Does the actual operation for power *) 
    let rec pow' (Bigint(neg3,base))
                 (Bigint(neg2,expt)) (Bigint(neg1,result))
    = match (Bigint(neg2,expt)) with
        | (Bigint(neg2,[0])) -> (Bigint(neg1, result))
        | (Bigint(neg2,expt)) -> 
            if rem (Bigint(Pos, expt))
            (Bigint(Pos,[2])) = (Bigint(Pos,[1]))
            (* odd  *)
            then pow'
                (Bigint(neg3,base))
                (sub (Bigint(neg2,expt)) (Bigint(Pos,[1])))
                (mul (Bigint(Pos,base)) (Bigint(Pos,result)))
           (* even *)    
            else pow' 
                (mul (Bigint(neg3,base)) (Bigint(neg3,base)))
                (div (Bigint(neg2,expt)) (Bigint(Pos,[2])))
                (Bigint(neg1, result)) 
       
    
    let pow (Bigint(neg1, value1)) (Bigint(neg2, value2)) =
        if neg2 = Neg  
            then (Bigint(Pos,[0]))
        else 
            pow' (Bigint(Pos,value1))
           (Bigint(Pos,value2)) (Bigint(Pos,[1]))
end

