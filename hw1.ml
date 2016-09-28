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
    for j=0 to Array2.dim2 arr - 1 do
      for i=0 to Array2.dim1 arr - 1 do
        let x = Array2.unsafe_get arr i j in
        Array2.unsafe_set arr i j (f x)
      done
    done;
    img

  let fold ~f ~init img =
    let arr = unwrap_img img in
    let acc = ref init in
    for j=0 to Array2.dim2 arr - 1 do
      for i=0 to Array2.dim1 arr - 1 do
        let x = Array2.unsafe_get arr i j in
        acc := f x !acc
      done
    done;
    !acc

end

module EquivTbl = struct
  let create () = Int.Table.create ()

  let add tbl x y =
    let y_cl = Option.value (Int.Table.find tbl y) ~default:Int.Set.empty in
    let x_cl = Option.value (Int.Table.find tbl x) ~default:Int.Set.empty in
    let equiv_cl = Int.Set.add (Int.Set.add (Int.Set.union x_cl y_cl) x) y in
    Int.Set.iter equiv_cl ~f:(fun x -> Int.Table.set tbl ~key:x ~data:equiv_cl)

  let eq tbl x y =
    Option.value_map (Int.Table.find tbl x) ~default:false ~f:(fun set -> Int.Set.mem set y)

  let min_elt tbl x =
    Option.value_map (Int.Table.find tbl x) ~default:None ~f:(fun set -> Int.Set.min_elt set)

end

(** Grayscale -> binary using a threshold *)
let bin_image threshold img =
  ImgUtils.map ~f:(fun x -> if x < threshold then 0 else 255) img

(** Sequential labeling algorithm.
    img: a binary image *)
let seq_label img =
  let run () =
    let eq_tbl = EquivTbl.create () in
    let label = ref 0 in
    let new_label () = incr label; !label in
    let open Bigarray in
    let arr = ImgUtils.unwrap_img img in
    for j=0 to Array2.dim2 arr - 1 do (* y *)
      for i=0 to Array2.dim1 arr - 1 do (* x *)
        let top, left, tl = match i, j with
          | 0, 0 -> 0, 0, 0
          | 0, _ -> arr.{i, j-1}, 0, 0
          | _, 0 -> 0, arr.{i-1, j}, 0
          | _ -> arr.{i, j-1}, arr.{i-1, j}, arr.{i-1, j-1}
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
          c, 255 -> EquivTbl.add eq_tbl b c;
                    arr.{i, j} <- b

        | _ -> invalid_arg "Non-binary image"
      done
    done;
    Printf.printf "Max label: %d\n" !label;
    (* minimize number of labels *)
    ignore @@ ImgUtils.map ~f:(function 0 -> 0 | x -> Option.value_exn (EquivTbl.min_elt eq_tbl x)) img
  in
  run ();
  (* get total count of labels *)
  let labels = ImgUtils.fold img ~init:Int.Set.empty
      ~f:(fun x set -> if Int.Set.mem set x then set else Int.Set.add set x)
  in
  let labels = Int.Set.remove labels 0 in
  Printf.printf "num of labels: %d\n" (Int.Set.length labels);

  let delta = 255 / Int.Set.length labels in
  let label_map = fst @@ Int.Set.fold labels
      ~f:(fun (map, cnt) x -> Int.Map.add map ~key:x ~data:cnt, cnt + delta)
      ~init:(Int.Map.empty, delta)
  in
  ImgUtils.map img ~f:(fun x -> if x = 0 then 0 else Int.Map.find_exn label_map x)

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

