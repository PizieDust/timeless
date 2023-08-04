open Mirage

let img =
  Key.(if_impl is_solo5) (block_of_file "storage") (block_of_file "disk.img")

let main =
  let packages =
    [
      package "fat-filesystem";
      package "mbr-format";
      package "http";
      package "tyxml";
      package "cohttp-lwt-unix";
    ]
  in
  main ~packages "Unikernel.Main" (block @-> job)

let () = register "timeless" [ main $ img ]
