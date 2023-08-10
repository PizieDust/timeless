open Lwt.Syntax
open Lwt.Infix 
open Multipart_form

module type HTTP = Cohttp_mirage.Server.S
let http_src = Logs.Src.create "http" ~doc:"HTTP server"

let storage_src = Logs.Src.create "storage" ~doc:"Storage disk"
 
module Http_log = (val Logs.src_log http_src : Logs.LOG)
module Storage_log = (val Logs.src_log storage_src : Logs.LOG)

module Dispatch (KV: Mirage_kv.RO) (S : HTTP) (B : Mirage_block.S) = struct 
  module Partitioned = Mirage_block_partition.Make(B) 
  module FatPartition = Fat.Make (Partitioned)
  module Map = Map.Make (String)
  let connect_disk disk =
    let* connection = FatPartition.connect disk in 
    Lwt.return connection

  let disconnect_disk disk =
    let* disconnect = FatPartition.disconnect disk in 
    Lwt.return disconnect    
    
  
  let generate_random_filename () =
    let _r = Random.self_init in
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz123456789" in
    let len = String.length chars in
    let random_char () = String.get chars (Random.int len) in
    let rec loop acc count =
      if count = 0 then acc
      else loop (acc ^ String.make 1 (random_char ())) (count - 1)
    in
    loop "" 15
  let file_extension filename = 
    match String.rindex_opt filename '.' with
    | Some dot_index -> String.sub filename (dot_index + 1) (String.length filename - dot_index - 1)
    | None -> ""

  let flatten_csv_string rows =
    let flattened_rows = List.map (String.concat ",") rows in
    String.concat "\n" flattened_rows

  let update_csv_file disk filename caption = 
    let csv_path = "/csv/captions.csv" in
    let* csv_file = FatPartition.read disk csv_path 0 5242880 in
      match csv_file with
      | Ok file ->
        let new_row = [filename ^","^ caption] in
        let file_data = Cstruct.concat file in
        let csv_content = Cstruct.to_string file_data in
        let existing_rows = Csv.of_string ~separator:',' ~has_header:false csv_content in
        let csv_content = Csv.input_all existing_rows in
        let append_row = csv_content @ [new_row] in
        let updated_csv_content = flatten_csv_string append_row in 
        let* write_result = FatPartition.write disk csv_path 0 (Cstruct.of_string updated_csv_content) in
        begin
          match write_result with
          | Ok () ->
            Storage_log.info (fun f -> f "CSV file updated successfully\n")
          | Error err ->
            Storage_log.err (fun f -> f "Error updating CSV file: %a\n" FatPartition.pp_write_error err)
        end;      
        Lwt.return_unit  
      | Error err -> 
        Storage_log.err (fun f -> f "Error listing directory: %a\n" FatPartition.pp_error err);
        Lwt.return_unit
  
  let read_csv_file disk =
    let csv_path = "/csv/captions.csv" in
    let* csv_file = FatPartition.read disk csv_path 0 5242880 in
    match csv_file with
    | Ok file ->
      let file_data = Cstruct.concat file in
      let csv_content = Cstruct.to_string file_data in
      let existing_rows = Csv.of_string ~separator:',' ~has_header:false csv_content in
      let all_csv_content = Csv.input_all existing_rows in
      (* Convert CSV content to JSON *)
      let json_content =
        List.map (fun row ->
          match row with
          | [name; caption] -> `Assoc [("name", `String name); ("caption", `String caption)]
          | _ -> `Null
        ) all_csv_content in
      
      Lwt.return json_content
    | Error err ->
      Storage_log.err (fun f -> f "Error listing directory: %a\n" FatPartition.pp_error err);
      Lwt.return []
        

  let csv_to_json csv_content = 
    let rows = String.split_on_char '\n' csv_content in
    let json_list =
      List.map (fun line ->
          match String.split_on_char ',' line with
          | [name; caption] -> `Assoc [ ("name", `String name); ("caption", `String caption) ]
          | _ -> `Null (* Handle invalid lines as needed *)
        ) rows in 
    Lwt.return json_list

  let save_uploaded_file fat_device filename file_contents =
    let uri = "/images/" ^ filename in
    let* file = FatPartition.create fat_device uri in
    match file with
    | Error err ->
        Storage_log.err (fun f -> f "An error occurred while creating the image file: %a\n"  FatPartition.pp_write_error err);
        Lwt.return_unit
    | Ok () ->
        Storage_log.info (fun f -> f "Image file created successfully\n");
        let file_data = Cstruct.of_string file_contents in
        let* save_image = FatPartition.write fat_device uri 0 file_data in
        match save_image with
        | Error err ->
            Storage_log.err (fun f -> f "An error occurred while saving the image file: %a\n"  FatPartition.pp_write_error err);
            Lwt.return_unit
        | Ok () ->
            Storage_log.info (fun f -> f "Image file saved successfully: %s\n" ("/images/" ^ filename));
            Lwt.return_unit

  let css_contents data = KV.get data (Mirage_kv.Key.v "style.css") >>=
  function
  | Error e ->
      Storage_log.err (fun f -> f "Error reading CSS file: %a\n" KV.pp_error e);
      Lwt.return ""
  | Ok css -> Lwt.return css

  let js_contents data = KV.get data (Mirage_kv.Key.v "main.js") >>=
  function
  | Error e ->
      Storage_log.err (fun f -> f "Error reading JS file: %a\n" KV.pp_error e);
      Lwt.return ""
  | Ok js -> Lwt.return js

  let homepage_handler () =
    let index_html = Homepage.index_page in
    let response = S.respond_string ~status:`OK ~body:index_html () in
    Lwt.return response

  let load_content disk =
    let* image_data = read_csv_file disk in
    let json_string = Yojson.to_string (`List image_data) in
    let response = S.respond_string ~status:`OK ~body:json_string () in 
    Lwt.return response

  let get_image disk image_name =
    let image_path = "/images/"^image_name in
    Storage_log.info (fun f -> f "Trying to load image from: %s\n" image_path);
    let* images = FatPartition.read disk image_path 0 5242880 in
    match images with
    | Ok files ->
      let file_data = Cstruct.concat files in
      let res = Cstruct.to_string file_data in
      let response = S.respond_string ~status:`OK ~body:res () in 
      Lwt.return response    
    | Error err -> 
      Storage_log.err (fun f -> f "Error listing directory: %a\n" FatPartition.pp_error err);
      let response =  S.respond_string ~status:`Not_found ~body:"Error occured while loading image" () in
      Lwt.return response    
    

    (*  let upload_image_handler _req body disk =         
          Cohttp_lwt.Body.to_string body >>= fun body_str ->
          let folder = "images" in  
          let random_filename = generate_random_filename () in
          let filename = random_filename ^ ".jpg" in
          let _saveImg = save_uploaded_file disk folder filename body_str in
          let response = S.respond_string ~status:`OK ~body:"Image uploaded successfully" () in 
          Lwt.return response *)
    let to_map ~assoc m =
      let open Multipart_form in
      let rec go (map, rest) = function
        | Leaf { header; body } -> (
            let filename =
              Option.bind
                (Header.content_disposition header)
                Content_disposition.filename in
            match
              Option.bind
                (Header.content_disposition header)
                Content_disposition.name
            with
            | Some name -> (Map.add name (filename, List.assoc body assoc) map, rest)
            | None -> (map, (body, (filename, List.assoc body assoc)) :: rest))
        | Multipart { body; _ } ->
            let fold acc = function Some elt -> go acc elt | None -> acc in
            List.fold_left fold (map, rest) body in
      go (Map.empty, []) m

     (** let parse_multipart_form body_str content_type =
        match of_string_to_list body_str content_type with
        | Ok (m, assoc) ->
          let m, _r = to_map ~assoc m in
          let m = Map.bindings m in
          let name = List.assoc "caption" m in
          let image_info = List.assoc "image" m in
          Some (name, image_info)
        | Error (`Msg err) -> Http_log.err (fun f -> f "Image parsing error: %s" err); *)

    let upload_image_handler req body disk =
      Cohttp_lwt.Body.to_string body >>= fun body_str ->
      let header = Cohttp.Request.headers req in
      let ct = Cohttp.Header.get header "Content-Type" in
      let content_type =
        Rresult.R.get_ok
        @@ Content_type.of_string ((Option.get ct) ^ "\r\n") in
        match of_string_to_list body_str content_type with
        | Ok (m, assoc) ->
          let m, _r = to_map ~assoc m in
          let m2 = Map.fold (fun key (_opt,value) acc ->
            Map.add key value acc
          ) m Map.empty in 
          let image = Map.find "image" m in
          let caption = Map.find "caption" m2 in
          let* _r = match caption, image with 
           |  caption, (Some image, file) ->
              let caption = caption in
              let image_extension = file_extension image in
              let image_name = generate_random_filename () ^ "." ^ image_extension in
              let file = file in
              let* _saved_image = save_uploaded_file disk image_name file in
              let* _update_csv = update_csv_file disk image_name caption in
              Http_log.info (fun f -> f "Text: %s\n" caption);
              Http_log.info (fun f -> f "Image: %s - %s\n" image (String.sub file 0 5));
              Lwt.return_unit
            | _, _ ->
              Http_log.err (fun f -> f "Empty request sent");
              Lwt.return_unit
        in
        let response = S.respond_string ~status:`Accepted ~body:"Image uploaded succesfully" () in
        Lwt.return response 
        | Error (`Msg err) ->
          Http_log.err (fun f -> f "Failed to parse request: %s" err);
        let response = S.respond_string ~status:`Internal_server_error ~body:"Failed to parse request" () in
        Lwt.return response     
      

  let callback_with_static data disk _conn req body =
    let* response =
      match Uri.path (Cohttp.Request.uri req) with
      | "/" ->
        homepage_handler ()
      | "/upload" ->
        upload_image_handler req body disk
      | path when String.length path > 7 && String.sub path 0 7 = "/image/" ->
        let filename = String.sub path 7 (String.length path - 7) in
        get_image disk filename
      | "/content" -> 
        load_content disk
      | "/style.css" ->
          Http_log.info (fun f -> f "Loading CSS\n");
          let* css_contents = css_contents data in
          let response = S.respond_string ~status:`OK ~body:css_contents () in
          Lwt.return response
      | "/main.js" ->
        Http_log.info (fun f -> f "Loading JS\n");
        let* js_contents = js_contents data in
        let response = S.respond_string ~status:`OK ~body:js_contents () in
        Lwt.return response
      | _ ->
          Http_log.err (fun f -> f "No endpoint for this request\n");
          Lwt.return (S.respond_not_found ())
    in
    response

  let conn_closed _conn =
    Http_log.info (fun f -> f "Connection closed");
    ()
  let start data disk =
    let storage = Lwt_main.run (connect_disk disk) in
    let callback = callback_with_static data storage in
    S.make ~conn_closed ~callback ()
end

module Main (B : Mirage_block.S) (Http : HTTP) (KV : Mirage_kv.RO) (Clock : Mirage_clock.PCLOCK) = struct
  module Partitioned = Mirage_block_partition.Make(B)
  module FatPartition = Fat.Make (Partitioned) 
  module D = Dispatch (KV) (Http) (B)
  module Homepage = Homepage

  let setup_disk block total_sectors =
   let* b1, _rest = Partitioned.connect total_sectors block in
   let* fs_result = FatPartition.format b1 total_sectors in
    let () =
      match fs_result with
      | Ok _fs -> Storage_log.info (fun f -> f "FAT filesystem created\n");
      | Error err ->
        Storage_log.err (fun f -> f "An error occurred: %a\n"  FatPartition.pp_write_error err);
    in 
    let* fat_device = FatPartition.connect b1 in
    let folder = "images" in
    let csv_folder = "csv" in
    let csv_file = "csv/captions.csv" in
    let* folder_created = FatPartition.mkdir fat_device folder in
    let () =
      match folder_created with
      | Ok () ->  Storage_log.info (fun f -> f "Image folder created\n");
      | Error er -> Storage_log.err (fun f -> f "An error occurred: %a\n"  FatPartition.pp_write_error er);
    in
    let* csv_folder_created = FatPartition.mkdir fat_device csv_folder in
    let () =
      match csv_folder_created with
      | Ok () ->  Storage_log.info (fun f -> f "CSV folder created\n");
      | Error er -> Storage_log.err (fun f -> f "An error occurred: %a\n"  FatPartition.pp_write_error er);
    in
    let* csv_file_created = FatPartition.create fat_device csv_file in
    let () =
      match csv_file_created with
      | Ok () ->  Storage_log.info (fun f -> f "CSV file created\n");
      | Error er -> Storage_log.err (fun f -> f "An error occurred: %a\n"  FatPartition.pp_write_error er);
    in
    let* () = FatPartition.disconnect fat_device in
    Lwt.return b1

  let start block http data _pclock =
    let* info = B.get_info block in
    let total_sectors = info.size_sectors in
    Storage_log.info (fun f -> f "Disk info: %a" Mirage_block.pp_info info);
    let* disk = setup_disk block total_sectors in
    let http_port = Key_gen.http_port () in
    let tcp = `TCP http_port in
    let http =
      Http_log.info (fun f -> f "Listening on %d/TCP" http_port);
      http tcp @@ D.start data disk
    in
    Lwt.join [ http ]
end

