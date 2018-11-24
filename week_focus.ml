open Base
open Stdio

(* let (<<) f g x = f(g(x));; *)

let filename = "/Users/vincent/Dropbox/wiki/2018_november_strategy.md" 

type tag = {
  label: string;
  days: day list;
} and day = 
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

let to_day = function
  | 1 -> Monday
  | 2 -> Tuesday
  | 3 -> Wednesday
  | 4 -> Thursday
  | 5 -> Friday
  | 6 -> Saturday
  | _ -> Sunday

let is_checked cell =
  String.equal cell "x"

let get_day row day_number =
  let cell_content = String.strip (Re.Group.get row (day_number+1)) in
  if  is_checked cell_content then 
    [to_day day_number] 
  else 
    []

let get_tag line = 
  let label_re = 
    "\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|" 
    |> Re.Pcre.re |> Re.compile in
  match Re.exec_opt label_re line with
  | Some row -> 
    let days = 
      List.range 1 7 
      |> List.bind ~f:(fun day_number -> get_day row day_number )
    in
    if List.is_empty days then None
    else Some { label = (Re.Group.get row 1); days = days }

  | None -> None

let print_tag tag = 
  let string_of_day = function
    | Monday  -> "Monday"
    | Tuesday -> "Tuesday"
    | Wednesday -> "Wednesday"
    | Thursday -> "Thursday"
    | Friday -> "Friday"
    | Saturday -> "Saturday"
    | Sunday -> "Sunday" in

  let days = 
    List.map ~f:string_of_day tag.days
    |> String.concat ~sep:"," 
  in
  printf "{ label=%s; days=[%s] }\n" tag.label days

let get_tags filename =
  let file = In_channel.create filename in 
  let lines = In_channel.input_lines file in
  In_channel.close file;

  lines 
  |> List.fold ~init: [] ~f:(fun acc line -> 
      match get_tag line with
      | Some tag -> tag::acc
      | None -> acc)

let () = 
  let tags = get_tags filename in 
  List.iter ~f:print_tag tags

