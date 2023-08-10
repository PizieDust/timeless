open Mirage

let img =
  Key.(if_impl is_solo5) (block_of_file "storage") (block_of_file "disk.img")

let data_key = Key.(value @@ kv_ro ~group:"data" ())
let data = generic_kv_ro ~key:data_key "frontend"

let stack = generic_stackv4v6 default_network

let http_srv = cohttp_server @@ conduit_direct ~tls:false stack

let http_port =
  let doc = Key.Arg.info ~doc:"Listening HTTP port." [ "http" ] in
  Key.(create "http_port" Arg.(opt int 8080 doc))

let main =
  let packages =
    [
      package "multipart_form";
      package "uri";
      package "magic-mime";
      package "fat-filesystem";
      package "mirage-block-partition";
      package "http";
      package "tyxml";
      package "cohttp-mirage";
      package "mirage-kv";
      package "csv";
      package "yojson";
    ]
  in
  let keys = List.map key [ http_port ] in
  foreign ~packages ~keys "Unikernel.Main" (block @-> http @-> kv_ro @-> pclock @-> job)

let () = register "timeless" [ main $ img $ http_srv $ data $ default_posix_clock ]
