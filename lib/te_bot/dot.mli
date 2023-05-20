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

val string_of_id: id -> string
val string_of_edge: (edge_type * id * id) -> attrs -> string
val string_of_node: id -> attrs -> string
val string_of_attr: attr -> string
val string_of_attrs: attr list -> string
val string_of_record: record -> string
val string_of_subgraph: subgraph -> string
val string_of_stmt: stmt -> string
val string_of_stmts: stmt list -> string
val string_of_graph: graph -> string

val escape_string: string -> string

val node: ?attrs: attrs -> id -> stmt
val edge: ?attrs: attrs -> edge_type -> id -> id -> stmt
val (=>): string -> id -> attr
