type id = Id of string | Port of id * string | String of string | Int of int | HTML of string

type attr = {key: string; value: id}

type attrs = attr list

type edge_type = Directed | Undirected

type record = field list
and field = RecordString of string | RecordPortString of string * string | Record of record

type subgraph = string * stmt list
and stmt = Node of id * attrs
         | Edge of (edge_type * id * id) * attrs
         | Subgraph of subgraph
         | Attr of attr

type graph_type = Graph | Digraph

type graph = graph_type * string * stmt list

let node ?(attrs = []) id  = Node (id, attrs)

let edge ?(attrs = []) type_ from to_ = Edge ((type_, from, to_), attrs)

let (=>) key value = {key; value}

let escape_string s = 
  let l = String.length s in
  let b = Buffer.create l in
  for i = 0 to l - 1 do
    Buffer.add_string b (match s.[i] with
        | '{' -> "\\{"
        | '}' -> "\\}"
        | '"' -> "\\\""
        | '\\' -> "\\\\"
        | '\n' -> "\\n"
        | c -> String.make 1 c)
  done;
  Buffer.contents b

let rec string_of_id = function
  | Id x -> x
  | Port (x, p) -> string_of_id x ^ ":" ^ p
  | String x -> "\"" ^ x ^ "\""
  | HTML x -> "<" ^ x ^ ">"
  | Int x -> string_of_int x

let string_of_attr {key; value} = key ^ "=" ^ (string_of_id value)

let string_of_attrs attrs =
  "[" ^ String.concat "," (List.map string_of_attr attrs) ^ "]"

let string_of_node id attrs = string_of_id id ^ string_of_attrs attrs

let string_of_edge (edge_type, id_from, id_to) attrs = 
  string_of_id id_from ^ (function Directed -> "->" | Undirected -> "--") edge_type ^ string_of_id id_to ^ string_of_attrs attrs

let rec string_of_record fs =
  String.concat "|" @@ List.map (function
      | RecordString t -> t
      | RecordPortString (p, t) -> "<" ^ p ^ ">" ^ t
      | Record fs' -> "{" ^ string_of_record fs' ^ "}")
    fs

let rec string_of_subgraph (name, stmts) =
  "subgraph" ^ " " ^ name ^ "{" ^ string_of_stmts stmts ^ "}"
and string_of_stmt stmt = 
  (match stmt with
   | Node (n, a) -> string_of_node n a
   | Edge (e, a) -> string_of_edge e a
   | Subgraph sg -> string_of_subgraph sg
   | Attr {key; value}  -> key ^ "=" ^ string_of_id value) ^ ";"
and string_of_stmts stmts = String.concat "" (List.map (fun s -> string_of_stmt s) stmts)

let string_of_graph (graph_type, name, stmts)  =
  match graph_type with Graph -> "graph" | Digraph -> "digraph" ^ " " ^ name ^ "{" ^ string_of_stmts stmts ^ "}"

