open Core.Std
open Image

let load name = ImageLib.openfile name

let save name img =
  let name' = Filename.chop_extension name in
  ImageLib.writefile (name'^".out.pgm") img

module ImgUtils = struct
  open Bigarray
  open Image.Pixmap
  type t = (int, int8_unsigned_elt , c_layout) Array2.t

  let unwrap_img = function
    | {pixels=Grey(Pix8(arr));_} -> arr
    | _ -> invalid_arg "Wrong image type"

  let map ~f img =
    let arr = unwrap_img img in
    for i=0 to Array2.dim1 arr - 1 do
      for j=0 to Array2.dim2 arr - 1 do
        let x = Array2.get arr i j in
        Array2.set arr i j (f x)
      done
    done;
    img
end

let bin_image threshold img =
  ImgUtils.map ~f:(fun x -> if x < threshold then 0 else 255) img

let command =
  Command.basic
    ~summary:"Use binary image conversion at a given threshold"
    ~readme:(fun () -> "More detailed info")
    Command.Spec.(
      empty
      +> flag "-b" (optional int) ~doc:"int Convert to binary image at given threshold"
      +> anon ("filename" %: file))
    (fun bin_img fname () ->
       let f =
         match bin_img with
         | Some thr -> bin_image thr
         | None -> Fn.id
       in
       load fname |> f |> save fname)

let () =
  Command.run ~version:"1.0" ~build_info:"HW" command

