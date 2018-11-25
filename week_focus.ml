open Base
open Stdio

(* let (<<) f g x = f(g(x));; *)

let filename = "/Users/vincent/Dropbox/wiki/2018_november_strategy.md" 

let snippets_filename = "/Users/vincent/dotfiles/vim/snippets/all/var.snippets" 

type day = 
  | Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday

type tag = {
  label: string;
  days: day list;
} [@@deriving fields]

type day_summary = {
  day: day;
  items: string list;
}

let string_of_day = function
  | Monday  -> "Monday"
  | Tuesday -> "Tuesday"
  | Wednesday -> "Wednesday"
  | Thursday -> "Thursday"
  | Friday -> "Friday"
  | Saturday -> "Saturday"
  | Sunday -> "Sunday"

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
  let cell_content = String.strip (Re.Group.get row (day_number + 1)) in
  if  is_checked cell_content then [to_day day_number] else []

let get_tag line = 
  let label_re = 
    "\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|(.*?)\\|" 
    |> Re.Pcre.re |> Re.compile in
  match Re.exec_opt label_re line with
  | Some row -> 
    let days = 
      List.range ~stop:`inclusive 1 7 
      |> List.bind ~f:(fun day_number -> 
          get_day row day_number )
    in
    if List.is_empty days then None
    else Some { label = (Re.Group.get row 1) |> String.strip; days = days }

  | None -> None

let print_tag tag = 
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
  let day_str = string_of_day day in
  let line = List.range 0 (String.length day_str)
             |> List.map ~f:(fun _ -> "-") |> String.concat
  in
  printf "%s\n%s\n" day_str line;
  List.iter items ~f:(fun item -> printf "%s\n" item);
  printf "\n"

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

let print ~strategy  tags =
  let outc = Out_channel.create snippets_filename in
  Exn.protect ~f:(fun () ->  
      List.range ~stop:`inclusive 1 7 
      |> List.iter ~f:(fun  day_no -> 
          let summary = get_day_summary tags (to_day day_no) in
          match strategy with
          | `snippets -> print_snippet outc summary
          | `summary -> print_summary summary)
    )
    ~finally:(fun () -> Out_channel.close outc)

let () = 
  let tags = get_tags filename in 
  print ~strategy:`snippets tags
(* List.iter ~f:print_tag tags *)

