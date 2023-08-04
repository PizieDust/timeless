open Lwt.Syntax
open Tyxml.Html

module Main (B : Mirage_block.S) = struct
  module FatPartition = Fat.Make (B)

  let save_uploaded_file fat_device folder filename file_contents =
    let uri = (folder ^ "/" ^ filename) in
    let* file = FatPartition.create fat_device uri in
    let () = 
      match file with
      | Error err ->
          Format.printf "An error occurred while creating the file: %a\n" FatPartition.pp_write_error err
      | Ok () ->
          Printf.printf "File created successfully!\n";
      in
      let file_data = Cstruct.of_string file_contents in
      let* save_image = FatPartition.write fat_device uri 0 file_data in
      let () = 
        match save_image with
        | Error err ->
          Format.printf "An error occurred while saving the image: %a\n" FatPartition.pp_write_error err;
        | Ok () ->
          Printf.printf "Image %s saved successfully!\n" filename;
        in
        Lwt.return_unit

  let start block =
    let* info = B.get_info block in
    Format.printf "%a\n" Mirage_block.pp_info info;
    
    Lwt.return_unit
end
