(*根据完成的情况，生成 README*)
;;
#load "str.cma"

let lines_of_file file =
  let input = open_in file in
  let lines = ref [] in
  try
    while true do
      let line = input_line input in
      lines := !lines @ [ line ]
    done;
    !lines
  with End_of_file ->
    close_in input;
    !lines

let is_comment line =
  let r = Str.regexp "^(" in
  Str.string_match r line 0

let is_code line =
  let r = Str.regexp "^let" in
  Str.string_match r line 0

let comment_to_string line =
  let first = Str.replace_first (Str.regexp "^(\\* ") "" line in
  Str.replace_first (Str.regexp "*)$") "" first

let () =
  let title =
    "# ocaml-99problems\n\
     The answer of https://ocaml.org/learn/tutorials/99problems.html\n\n\
    \     \n"
  in
  let file = "main.ml" in
  let output = open_out "README.md" in
  Printf.fprintf output "%s" title;

  let lines = lines_of_file file in
  let length = List.length lines in
  for i = 0 to length - 1 do
    let line = List.nth lines i in
    if is_comment line then
      if i + 1 = length then
        Printf.fprintf output "- [ ] %s\n" (comment_to_string line)
      else if is_code (List.nth lines (i + 1)) then
        Printf.fprintf output "- [x] %s [[finished]](main.ml#L%d) \n"
          (comment_to_string line) (i + 1)
      else Printf.fprintf output "- [ ] %s\n" (comment_to_string line)
  done;
  close_out output
