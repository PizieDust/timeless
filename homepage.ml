open Tyxml
let index_page =
  let home =
    Html.(
      html
        (head
            (title (txt "Timeless"))
            [ meta ~a:[a_charset "UTF-8"] ()
            ; meta ~a:[a_name "viewport"; a_content "width=device-width, initial-scale=1.0"] ()
            ; link ~rel:[`Stylesheet] ~href:"style.css" ()
            ; link ~rel:[`Stylesheet] ~href:"https://fonts.googleapis.com" ()
            ; link ~rel:[`Stylesheet] ~href:"https://fonts.gstatic.com" ~a:[a_crossorigin `Anonymous] ()
            ; link ~rel:[`Stylesheet] ~href:"https://fonts.googleapis.com/css2?family=Zeyada&display=swap" ()
            ; script ~a:[a_src "main.js"] (txt "") ])
        (body
            [ section
                [ h1 ~a:[a_class ["title"]] [txt "Timeless Memories"]
                ; p ~a:[a_class ["text"]] [txt "A MirageOS unikernel to demonstrate persistent storage in Unikernels."] ]
            ; section ~a:[a_class ["flex_container"]]
                [ div 
                    [ input ~a:[a_style "display: none"; a_input_type `File; a_name "image"; a_id "imageInput"] ()
                    ; div [ input ~a:[a_input_type `Text; a_id "imageCaption"; a_name "caption" ; a_class ["text_input"] ; a_placeholder "Enter a timeless caption"] () ]
                    ; div [ button ~a:[a_onclick "uploadImage()"; a_class ["btn"]; a_id "uploadButton"] [txt "Upload a timeless memory"] ]
                    ]
                ]
            ; section [  ]]))
    in
    Format.asprintf "%a" (Html.pp()) (home)
