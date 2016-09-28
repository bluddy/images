open Core.Std
open Image

let some x = Some x
let (|-) = Fn.compose

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
        let x = Array2.unsafe_get arr i j in
        Array2.unsafe_set arr i j (f x)
      done
    done;
    img

  let fold ~f ~init img =
    let arr = unwrap_img img in
    let acc = ref init in
    for i=0 to Array2.dim1 arr - 1 do
      for j=0 to Array2.dim2 arr - 1 do
        let x = Array2.unsafe_get arr i j in
        acc := f x !acc
      done
    done;
    !acc

end

(** Grayscale -> binary using a threshold *)
let bin_image threshold img =
  ImgUtils.map ~f:(fun x -> if x < threshold then 0 else 255) img

(** Sequential labeling algorithm.
    img: a binary image *)
let seq_label img =
  let run () =
    let equi_tbl = Int.Table.create () in
    let label = ref 0 in
    let new_label () = incr label; !label in
    let open Bigarray in
    let arr = ImgUtils.unwrap_img img in
    for i=0 to Array2.dim1 arr - 1 do
      for j=0 to Array2.dim2 arr - 1 do
        let top, left, tl = match i, j with
          | 0, 0 -> 0, 0, 0
          | 0, _ -> 0, arr.{i, j-1}, 0
          | _, 0 -> arr.{i-1, j}, 0, 0
          | _ -> arr.{i-1, j}, arr.{i, j-1}, arr.{i-1, j-1}
        in
        match tl, top, left, arr.{i, j} with
        | _, _,
          _, 0 -> ()

        | 0, 0,
          0, 255 -> arr.{i, j} <- new_label ()

        | 0, 0,
          d, 255

        | 0, d,
          0, 255

        | d, _,
          _, 255 when d <> 0 -> arr.{i, j} <- d

        | 0, b,
          c, 255 when b = c -> arr.{i, j} <- b

        | 0, b,
          c, 255 -> let x, y = if b < c then b, c else c, b in
                    Int.Table.change equi_tbl x
                      ~f:(function None -> some @@ Int.Set.singleton y
                                 | Some set -> some @@ Int.Set.add set y)
                      (* Use equivalence table *)

        | _ -> invalid_arg "Non-binary image"
      done
    done;
    (* minimize number of labels *)
    ignore @@
      ImgUtils.map ~f:(fun lbl ->
        match Int.Table.find equi_tbl lbl with
        | None -> lbl
        | Some s -> Int.Set.min_elt s |> Option.value_exn)
        img
  in
  run ();
  (* get total count of labels *)
  let max_label = ImgUtils.fold img ~init:0 ~f:Int.max in
  let label_gap = 255 / max_label in
  ImgUtils.map img ~f:(fun lbl -> lbl * label_gap)

(* argument parsing *)
let command =
  Command.basic
    ~summary:"Use binary image conversion at a given threshold"
    ~readme:(fun () -> "More detailed info")
    Command.Spec.(
      empty
      +> flag "-b" (optional int) ~doc:"int Convert to binary image at given threshold"
      +> flag "--label" no_arg ~doc:"Use segmented labeling (requires binary image)"
      +> anon ("filename" %: file))
    (fun bin_img label fname () ->
       let f =
         match bin_img with
         | Some thr -> bin_image thr
         | None -> Fn.id
       in
       let g = if label then seq_label else Fn.id in
       load fname |> f |> g |> save fname)

let () =
  Command.run ~version:"1.0" ~build_info:"HW" command

