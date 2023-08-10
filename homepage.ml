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
            ; link ~rel:[`Stylesheet] ~href:"https://unpkg.com/aos@2.3.1/dist/aos.css" ()
            ; script ~a:[a_src "https://unpkg.com/aos@2.3.1/dist/aos.js"] (txt "")
            ; script ~a:[a_src "https://cdn.tailwindcss.com"] (txt "") 
            ; script ~a:[a_src "https://cdn.tailwindcss.com"] (txt "")
            ; script ~a:[a_src "main.js"] (txt "")
            ])

        (body ~a:[a_class["px-10 py-10"]]
            [ section
                [ h1 ~a:[a_class ["font-bold text-3xl text-center text-gray-100"]] [txt "Timeless Memories"]
                ; p ~a:[a_class ["font-semibold text-md text-gray-200 text-center"]] [txt "A MirageOS unikernel to demonstrate persistent storage in Unikernels."] ]
            ; section ~a:[a_class ["my-4"]]
                [ div ~a:[a_class["text-center"]] 
                    [ div ~a:[a_id "alert-container"; a_class ["fixed"; "top-4"; "right-4"; "z-50"]] []
                    ; input ~a:[a_style "display: none"; a_input_type `File; a_name "image"; a_id "imageInput"] ()
                    ; div [ input ~a:[a_input_type `Text; a_id "imageCaption"; a_name "caption" ; a_class ["w-full p-3 my-2"] ; a_placeholder "Enter a timeless caption"] () ]
                    ; div [ button ~a:[a_onclick "document.getElementById('imageInput').click()"; a_class ["px-2 py-2 bg-blue-600 text-gray-200 rounded-sm my-2"]; a_id "uploadButton"] [txt "Post a timeless memory"] ]
                    ]
                ]
            ; p ~a:[a_id "no-img"; a_class ["font-semibold text-sm text-gray-200 text-center hidden"]] [txt "Nothing yet. Get the ball rolling, post a timeless memory"]
            ; section ~a:[a_id "image-gallery"; a_class ["my-5 grid grid-cols-4 gap-4"]  ][]]))
    in
    Format.asprintf "%a" (Html.pp()) (home)
