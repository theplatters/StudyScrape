open Lwt
open Cohttp_lwt_unix
open Soup

(* Function to fetch and parse one page *)
let fetch_page page =
  let url =
    Printf.sprintf "https://www.studienwahl.at/studien/sozial-und-wirtschaftswissenschaften/?page=%d" page
  in
  Client.get (Uri.of_string url) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= parse >|= fun soup ->
  soup $$ ".course" |> to_list

(* Function to fetch a specific page *)
let fetch_specific_page url =
  Client.get (Uri.of_string url) >>= fun (_resp, body) ->
  Cohttp_lwt.Body.to_string body >|= parse

let remove_newlines s =
  String.map (fun c -> if c = '\n' then ' ' else c) s

(* Function to process a single course *)
let process_course oc course =
  (* Extract the needed texts from each course node *)
  let study  = course $ "h2" |> R.leaf_text |> String.trim in
  let uni    = course $ "p"  |> R.leaf_text |> String.trim in
  let degree = course $ "b"  |> R.leaf_text |> String.trim in
  let link = "https://www.studienwahl.at" ^ (course $ "p a" |> R.attribute "href" |> String.trim) in
  (* Fetch the specific page and extract the text *)
  fetch_specific_page link >>= fun page ->
  let text = page $ ".inhalt p" |> R.leaf_text |> String.trim |> remove_newlines in
  let definition = match page $$ ".definition" |> nth 2 with 
    | Some(node) -> node
    | None -> failwith "Something went wrong!" in
  let degree_exact = match definition $$ "dd" |> nth 3 with
    | Some(node) -> node |> R.leaf_text |> String.trim
    | None -> failwith "Something went wrong! 0" in
  let box = page $ ".box" in
  let uni_url = match box $$ "a" |> nth 1 with  
    | Some(node) -> node |> R.leaf_text |> String.trim
    | None -> "" in
 let plan = match box $$ "a" |> nth 2 with  
    | Some(node) -> node |> R.leaf_text |> String.trim
    | None -> "" in
  let definition2 = match page $$ ".definition" |> nth 4 with 
    | Some(node) -> node
    | None -> failwith "Something went wrong!" in
  let type_of_institution = match definition2 $$ "dd" |> nth 5 with
    | Some(node) -> node |> R.leaf_text |> String.trim
    | None -> "" in
  (* Write the results to the output file *)
  Lwt.return (Printf.fprintf oc "%s| %s| %s| %s| %s| %s| %s| %s\n" study degree uni text degree_exact uni_url plan type_of_institution)

let () =
  (* Create a list of pages from 1 to 45 *)
  let pages = List.init 45 (fun i -> i + 1) in
  (* Fetch all pages in parallel, then concatenate all the extracted ".course" nodes *)
  let all_courses =
    Lwt_main.run (
      Lwt_list.map_p fetch_page pages >|= List.concat
    )
  in
  (* Open the output file *)
  let oc = open_out "studies.csv" in
  try
    (* Process each course and write to the file *)
    Lwt_main.run (
      Lwt_list.iter_s (process_course oc) all_courses
    );
    close_out oc
  with e ->
    close_out oc;
    raise e
