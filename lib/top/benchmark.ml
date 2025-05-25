open Te_core
module T = Types

module type RUNNER = sig
  type tables

  val tables: ?overexpand:bool -> unit -> tables
  val driver: tables -> Driver.driver

  module Run: sig
    val file: (T.Symbol.t -> unit) -> string -> unit
  end
end

module Make(X: RUNNER) = struct
  let read filename =
    let ch = open_in filename in
    let string = really_input_string ch (in_channel_length ch) in
    close_in ch;
    string

  let benchmark () =
    let t = X.tables () in
    Gc.compact ();

    let fs = Sys.readdir "linear" in
    Array.sort (fun x y ->
        let c = Int.compare (String.length x) (String.length y) in
        if c <> 0 then c else
          String.compare x y)
      fs;
    let every = 10 in
    for i = 0 to Array.length fs - 1 do
      if Int.equal (i mod every) 0 then
        fs.(i / every) <- fs.(i)
    done;
    let fs = Array.sub fs 0 (Array.length fs / every) in

    Array.iter (fun file ->
        let file = "linear/" ^ file in

        let pid = Unix.fork () in
        if pid = 0
        then begin
          let _ = Unix.alarm 60 in
          Sys.set_signal Sys.sigalrm (Sys.Signal_handle (fun _ -> Unix._exit 1));
          let d = X.driver t in

          let input = read file in
          let length = String.length input in

          let t = Sys.time() in
          X.Run.file (fun c -> d#read c) file;
          Fmt.pr "%s %i %b %f@." file length d#accept (Sys.time() -. t);
          Unix._exit 0
        end
        else begin
          let _ = Unix.wait () in
          ()
        end)
      fs
end
