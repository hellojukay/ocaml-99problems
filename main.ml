(* 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (easy) *)
let rec last lst =
  match lst with [] -> None | [ n ] -> Some n | _ :: next -> last next

(* 2. Find the last but one (last and penultimate) elements of a list. (easy) *)
let rec last_two (lst : 'a list) =
  match lst with
  | [] -> None
  | [ n ] -> None
  | [ m; n ] -> Some (m, n)
  | head :: next -> last_two next

(* 3. Find the K'th element of a list. (easy) *)
let rec at (lst : 'a list) n =
  let rec find lst n =
    if n < 0 then None
    else
      match lst with
      | [] -> None
      | head :: next -> if n = 0 then Some head else find next (n - 1)
  in
  let r = find lst n in
  match r with None -> raise (Failure (string_of_int n)) | Some n -> Some n

(* 4. Find the number of elements of a list. (easy) *)
let rec length lst = match lst with [] -> 0 | head :: next -> 1 + length next

(* 5. Reverse a list. (easy) *)
let rec rev lst =
  match lst with [] -> [] | head :: next -> rev next @ [ head ]

(* 6. Find out whether a list is a palindrome. (easy) *)
let rec is_palindrome (lst : 'a list) =
  if List.length lst <= 2 then true
  else
    let i = ref 0 in
    let j = ref (List.length lst - 1) in
    try
      while !i <= !j do
        if List.nth lst !i != List.nth lst !j then raise Exit
        else (
          i := !i + 1;
          j := !j - 1)
      done;
      true
    with Exit -> false

(* 7. Flatten a nested list structure. (medium) *)
(* 8. Eliminate consecutive duplicates of list elements. (medium) *)
let rec compress lst =
  match lst with
  | [] -> []
  | [ n ] -> [ n ]
  | head :: next ->
      let rec delete_prefix n lst =
        match lst with
        | [] -> []
        | [ m ] -> if n = m then [] else [ m ]
        | head :: next ->
            if head = n then delete_prefix n next else head :: next
      in
      head :: compress (delete_prefix head next)

(* 9. Pack consecutive duplicates of list elements into sublists. (medium) *)
(* 10. Run-length encoding of a list. (easy) *)
(* 11. Modified run-length encoding. (easy) *)
(* 12. Decode a run-length encoded list. (medium) *)
(* 13. Run-length encoding of a list (direct solution). (medium) *)
(* 14. Duplicate the elements of a list. (easy) *)
let rec duplicate lst =
  match lst with [] -> [] | head :: next -> head :: head :: duplicate next

(* 15. Replicate the elements of a list a given number of times. (medium) *)
let rec duplicate lst n =
  if n <= 1 then lst
  else
    match lst with
    | [] -> []
    | head :: next ->
        let rec generate x n =
          match n with 1 -> [ x ] | n -> x :: generate x (n - 1)
        in
        generate head n @ duplicate next n

(* 16. Drop every N'th element from a list. (medium) *)
let drop lst n =
  let i = ref 1 in
  List.filter
    (fun x ->
      if !i != 3 then (
        i := !i + 1;
        true)
      else (
        i := 1;
        false))
    lst

(* 17. Split a list into two parts; the length of the first part is given. (easy) *)
let rec split lst n =
  match lst with
  | [] -> ([], [])
  | head :: next ->
      if n = 0 then ([], lst)
      else if n = 1 then ([ head ], next)
      else
        let a, b = split next (n - 1) in
        (head :: a, b)

(* 18. Extract a slice from a list. (medium) *)
(* 19. Rotate a list N places to the left. (medium) *)
(* 20. Remove the K'th element from a list. (easy) *)
let rec remove_at n lst =
  match lst with
  | [] -> []
  | head :: next -> if n = 0 then next else head :: remove_at (n - 1) next

(* 21. Insert an element at a given position into a list. (easy) *)
let rec insert_at e n lst =
  if n = 0 then e :: lst
  else
    match lst with
    | [] -> [ e ]
    | head :: next -> head :: insert_at e (n - 1) next

(* 22. Create a list containing all integers within a given range. (easy) *)
let rec range m n =
  if m = n then [ m ]
  else if m > n then m :: range (m - 1) n
  else m :: range (m + 1) n

(* 23. Extract a given number of randomly selected elements from a list. (medium) *)
(* 24. Lotto: Draw N different random numbers from the set 1..M. (easy) *)
(* 25. Generate a random permutation of the elements of a list. (easy) *)
let permutation lst =
  match lst with
  | [] -> []
  | [ n ] -> [ n ]
  | lst ->
      let n = Random.int (List.length lst) in
      let e = List.nth lst n in
      let rec remove lst n =
        match lst with
        | [] -> []
        | head :: next -> if n = 0 then next else head :: remove next (n - 1)
      in
      e :: remove lst n

(* 26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium) *)
(* 27. Group the elements of a set into disjoint subsets. (medium) *)
(* 28. Sorting a list of lists according to length of sublists. (medium) *)
(* 31. Determine whether a given integer number is prime. (medium) *)
(* 32. Determine the greatest common divisor of two positive integer numbers. (medium) *)
(* 33. Determine whether two positive integer numbers are coprime. (easy) *)
(* 34. Calculate Euler's totient function φ(m). (medium) *)
(* 35. Determine the prime factors of a given positive integer. (medium) *)
(* 36. Determine the prime factors of a given positive integer (2). (medium) *)
(* 37. Calculate Euler's totient function φ(m) (improved). (medium) *)
(* 38. Compare the two methods of calculating Euler's totient function. (easy) *)
(* 39. A list of prime numbers. (easy) *)
let rec all_primes n m =
  if n > m then []
  else
    let is_primes n =
      if n < 0 then false
      else
        try
          for i = 2 to n do
            if n mod i = 0 then if i != n then raise Exit
          done;
          true
        with Exit -> false
    in
    if is_primes n then n :: all_primes (n + 1) m else all_primes (n + 1) m

(* 40. Goldbach's conjecture. (medium) *)
(* 41. A list of Goldbach compositions. (medium) *)
(* 46 & 47. Truth tables for logical expressions (2 variables). (medium) *)
(* 48. Truth tables for logical expressions. (medium) *)
(* 49. Gray code. (medium) *)
(* 50. Huffman code (hard) *)
(* 55. Construct completely balanced binary trees. (medium) *)
(* 56. Symmetric binary trees. (medium) *)
(* 57. Binary search trees (dictionaries). (medium) *)
(* 58. Generate-and-test paradigm. (medium) *)
(* 59. Construct height-balanced binary trees. (medium) *)
(* 60. Construct height-balanced binary trees with a given number of nodes. (medium) *)
(* 61. Count the leaves of a binary tree. (easy) *)
(* 61A. Collect the leaves of a binary tree in a list. (easy) *)
(* 62. Collect the internal nodes of a binary tree in a list. (easy) *)
(* 62B. Collect the nodes at a given level in a list. (easy) *)
(* 63. Construct a complete binary tree. (medium) *)
(* 64. Layout a binary tree (1). (medium) *)
(* 65. Layout a binary tree (2). (medium) *)
(* 66. Layout a binary tree (3). (hard) *)
(* 67. A string representation of binary trees. (medium) *)
(* 68. Preorder and inorder sequences of binary trees. (medium) *)
(* 69. Dotstring representation of binary trees. (medium) *)
(* 70C. Count the nodes of a multiway tree. (easy) *)
(* 70. Tree construction from a node string. (medium) *)
(* 71. Determine the internal path length of a tree. (easy) *)
(* 72. Construct the bottom-up order sequence of the tree nodes. (easy) *)
(* 73. Lisp-like tree representation. (medium) *)
(* 80. Conversions. (easy) *)
(* 81. Path from one node to another one. (medium) *)
(* 82. Cycle from a given node. (easy) *)
(* 83. Construct all spanning trees. (medium) *)
(* 84. Construct the minimal spanning tree. (medium) *)
(* 85. Graph isomorphism. (medium) *)
(* 86. Node degree and graph coloration. (medium) *)
(* 87. Depth-first order graph traversal. (medium) *)
(* 88. Connected components. (medium) *)
(* 89. Bipartite graphs. (medium) *)
(* 90. Generate K-regular simple graphs with N nodes. (hard) *)
(* 91. Eight queens problem. (medium) *)
(* 92. Knight's tour. (medium) *)
(* 93. Von Koch's conjecture. (hard) *)
(* 94. An arithmetic puzzle. (hard) *)
(* 95. English number words. (medium) *)
(* 96. Syntax checker. (medium) *)
(* 97. Sudoku. (medium) *)
(* 98. Nonograms. (hard) *)
(* 99. Crossword puzzle. (hard) *)
