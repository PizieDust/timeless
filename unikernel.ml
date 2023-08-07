open Lwt.Syntax
open Lwt.Infix

module type HTTP = Cohttp_mirage.Server.S
let http_src = Logs.Src.create "http" ~doc:"HTTP server"

let storage_src = Logs.Src.create "storage" ~doc:"Storage disk"
 
module Http_log = (val Logs.src_log http_src : Logs.LOG)
module Storage_log = (val Logs.src_log storage_src : Logs.LOG)

module Dispatch (KV: Mirage_kv.RO) (S : HTTP) (B : Mirage_block.S) = struct 
  module Partitioned = Mirage_block_partition.Make(B) 
  module FatPartition = Fat.Make (Partitioned)
  module Tar = Tar_mirage.Make_KV_RO(Partitioned)
  let initialize_disk disk =
    let* connection = FatPartition.connect disk in 
    Lwt.return connection
  
  let generate_random_filename () =
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz123456789" in
    let len = String.length chars in
    let random_char () = String.get chars (Random.int len) in
    let rec loop acc count =
      if count = 0 then acc
      else loop (acc ^ String.make 1 (random_char ())) (count - 1)
    in
    loop "" 10

  let save_uploaded_file fat_device folder filename file_contents =
    let uri = folder ^ "/" ^ filename in
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
            Storage_log.info (fun f -> f "Image file saved successfully: %s\n" filename);
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

    let get_image disk =
      let* images = FatPartition.read disk "/images/CHlpNiYC5y.jpg" 0 95191 in
      match images with
      | Ok files ->
        let file_data = Cstruct.concat files in
        let res = Cstruct.to_string file_data in
        let response = S.respond_string ~status:`OK ~body:res () in 
        Lwt.return response    
      | Error err -> 
        Storage_log.err (fun f -> f "Error listing directory: %a\n" FatPartition.pp_error err);
        let response =  S.respond_string ~status:`Not_found ~body:"Error occured while loading images" () in
        Lwt.return response    
    

  let upload_image_handler req body disk =
    (* Parse the form data to get the uploaded image *)
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    let folder = "images" in  
    let random_filename = generate_random_filename () in
    let filename = random_filename ^ ".jpg" in
    let _content_type = Cohttp.Header.get (Cohttp.Request.headers req) in
    
    let _saveImg = save_uploaded_file disk folder filename body_str in
    let response = S.respond_string ~status:`OK ~body:"Image uploaded successfully" () in 
    Lwt.return response  


  let callback_with_static data disk _conn req body =
    let* response =
      match Uri.path (Cohttp.Request.uri req) with
      | "/" -> homepage_handler ()
      | "/upload" -> upload_image_handler req body disk
      | "/images" -> get_image disk
      | "/style.css" ->
          let* css_contents = css_contents data in
          let response = S.respond_string ~status:`OK ~body:css_contents () in
          Lwt.return response
      | "/main.js" ->
        let* js_contents = js_contents data in
        let response = S.respond_string ~status:`OK ~body:js_contents () in
        Lwt.return response
      | _ ->
          (* Return a "Not Found" response for all other requests *)
          Lwt.return (S.respond_not_found ())
    in
    response

  let conn_closed _conn =
    Http_log.info (fun f -> f "Connection closed");
    ()
  let start data disk =
    let storage = Lwt_main.run (initialize_disk disk) in
    let callback = callback_with_static data storage in
    S.make ~conn_closed ~callback ()
end

module Main (B : Mirage_block.S) (Http : HTTP) (KV : Mirage_kv.RO) = struct
  module Partitioned = Mirage_block_partition.Make(B)
  module FatPartition = Fat.Make (Partitioned)
  module Tar = Tar_mirage.Make_KV_RO(Partitioned)
  module D = Dispatch (KV) (Http) (B)
  module Homepage = Homepage

  let setup_disk block =
    let* b1, _rest = Partitioned.connect 1000000L block in
   let* fs_result = FatPartition.format b1 1000000L in
    let () =
      match fs_result with
      | Ok _fs -> Storage_log.info (fun f -> f "FAT filesystem created\n");
      | Error err ->
        Storage_log.err (fun f -> f "An error occurred: %a\n"  FatPartition.pp_write_error err);
    in
    let* fat_device = FatPartition.connect b1 in
    let folder = "images" in
    let* folder_created = FatPartition.mkdir fat_device folder in
    let () =
      match folder_created with
      | Ok () ->  Storage_log.info (fun f -> f "Image folder created\n");
      | Error er -> Storage_log.err (fun f -> f "An error occurred: %a\n"  FatPartition.pp_write_error er);
    in 
    Lwt.return b1

  let start block http data =
    let* info = B.get_info block in
    Storage_log.info (fun f -> f "Disk info: %a" Mirage_block.pp_info info);
    let* disk = setup_disk block in
    let http_port = Key_gen.http_port () in
    let tcp = `TCP http_port in
    let http =
      Http_log.info (fun f -> f "Listening on %d/TCP" http_port);
      http tcp @@ D.start data disk
    in
    Lwt.join [ http ]
end

