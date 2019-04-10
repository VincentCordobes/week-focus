open Base
open Stdio

let (<<) f g x = f(g(x));;

let filename = "/Users/vincent/Dropbox/wiki/2019_april_strategy.md" 
let snippets_filename = "/Users/vincent/dotfiles/vim/snippets/all/var.snippets" 

module Day  = struct 
  type t =
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday

  let create = function
    | 0 -> Sunday
    | 1 -> Monday
    | 2 -> Tuesday
    | 3 -> Wednesday
    | 4 -> Thursday
    | 5 -> Friday
    | 6 -> Saturday
    | _ -> failwith "a day of the week should be in range [0, 6]"

  let to_string = function
    | Monday  -> "Monday"
    | Tuesday -> "Tuesday"
    | Wednesday -> "Wednesday"
    | Thursday -> "Thursday"
    | Friday -> "Friday"
    | Saturday -> "Saturday"
    | Sunday -> "Sunday"

end

type tag = {
  label: string;
  days: Day.t list;
} [@@deriving fields]

type day_summary = {
  day: Day.t;
  items: string list;
}

let is_checked cell =
  String.equal cell "x"

(** 
   Get days that are checked 
   Eg. return [Monday; Wednesday] when
   |       | Mon. | Tue. | Wed. 
   |-------|------|------|------
   | task1 | x    |      | x    
*)
let get_day row day_number =
  (* 0 → 8, 1 → 2, ... 6 → 7 *)
  let group_index = 
    ((day_number + 6) % 7) + 2 
  in
  let cell_content = String.strip (Re.Group.get row group_index) in
  if  is_checked cell_content then [Day.create day_number] else []

let get_tag line = 
  let label_re = 
    "\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|" 
    |> Re.Pcre.re |> Re.compile 
  in
  match Re.exec_opt label_re line with
  | Some row -> 
    let days = 
      List.range ~stop:`inclusive 0 6 
      |> List.bind ~f:(get_day row)
    in
    if List.is_empty days then None
    else Some { label = (Re.Group.get row 1) |> String.strip; days = days }
  | None -> None

let print_tag tag = 
  let days = 
    List.map ~f:Day.to_string tag.days
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
      | Some tag -> acc @ [tag]
      | None -> acc)

let get_day_summary tags day =
  let keep_tag day tag =
    tag.days
    |> List.filter ~f:(fun d -> phys_equal d day)
    |> List.is_empty |> not
  in

  let items = 
    tags
    |> List.filter ~f:(keep_tag day)
    |> List.map ~f:(fun item -> item.label)
  in
  { day = day; items = items}

let print_summary {day; items} =
  let day_str = Day.to_string day in
  let line = List.range 0 (String.length day_str)
             |> List.map ~f:(fun _ -> "-") |> String.concat
  in
  printf "%s\n%s\n" day_str line;
  List.iter items ~f:(fun item -> printf "%s\n" item);
  printf "\n"

let print_items_of_the_day () =
  let tags = get_tags filename in 
  let now = Unix.time () 
            |> Unix.localtime 
  in
  let week_day = now.tm_wday in
  let {day;items} = get_day_summary tags (Day.create week_day) in
  printf "%s focus\n" (Day.to_string day);
  List.iter ~f:(printf "- [ ] %s\n") items


let print_snippet outc day_summary =
  let {day; items} = day_summary in
  let snippet_name =
    match day with
    | Monday  -> "focuslun"
    | Tuesday -> "focusmar"
    | Wednesday -> "focusmer"
    | Thursday -> "focusjeu"
    | Friday -> "focusven"
    | Saturday -> "focussam"
    | Sunday -> "focusdim" 
  in
  Out_channel.fprintf outc "snippet %s\n" snippet_name;
  List.iter ~f:(Out_channel.fprintf outc "- [ ] %s\n") items;
  Out_channel.fprintf outc "endsnippet\n"

let when_snippets strategy ~f=
  match strategy with 
  | `snippets -> f ()
  | `summary -> ()

let print ~strategy  tags =
  let outc = Out_channel.create snippets_filename in

  when_snippets strategy ~f:(fun () -> printf "Generating snippets to:\n  - %s\n" snippets_filename);
  Exn.protect ~f:(fun () ->  
      List.range ~stop:`inclusive 0 6 
      |> List.iter ~f:(fun day_no -> 
          let summary = get_day_summary tags (Day.create day_no) in
          match strategy with
          | `snippets -> print_snippet outc summary
          | `summary -> print_summary summary)
    )
    ~finally:(fun () -> Out_channel.close outc);
  when_snippets strategy ~f:(fun () -> printf "✨ Done\n")


let () = 
  (* let tags = get_tags filename in 
     print ~strategy:`snippets tags *)
  print_items_of_the_day ()
(* List.iter ~f:print_tag tags *)

