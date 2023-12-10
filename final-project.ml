(*Sneha Thakur, UIC, Fall 2023*)
(*CS 476: Final Project*)
(* Implementing a toy SQL in OCaml*)

type column = string

type expression =                       (*with three constructors*)
  | Column of column                    (* referencing a column by it's name*)
  | IntLiteral of int                   (*stores integer value*)
  | StringLiteral of string             (*stores string value*)

type clause =
  | Equal of expression * expression
  | LessThan of expression * expression
  | GreaterThan of expression * expression


(* SELECT: Defining the AST for the SELECT query *)
type select = {
  column_list: string list;
  from_table: string;
  where_clause: clause option;
}

(* Implementing a toy SQL Interpreter in OCaml*)
(* Defining the database *)
type table = (column list) * (column list list)
type db = (string * table) list

let database : db = [
  ("Customer", 
    (["Name"; "Age"; "Place"],
    [["Alex"; "20"; "Chicago"];
    ["Boris"; "21"; "New York"];
    ["Catherine"; "22"; "Los Angeles"];
  ]));
  ("Product", 
    (["P_Name"; "Quantity"],
    [["T-Shirt"; "1"];
    ["Jeans"; "2"];
    ["Shoes"; "3"];
  ]));
]

let rec eval_expr expr row =                    (*>> exp & row*)
  match expr with
  | Column col -> 
    (try
      List.assoc col row
    with
      Not_found -> failwith ("Column " ^ col ^ " not found"))
  | IntLiteral i -> string_of_int i
  | StringLiteral s -> s

let rec eval_clause clause row =
  match clause with
  | Equal (e1, e2) -> eval_expr e1 row = eval_expr e2 row
  | LessThan (e1, e2) -> eval_expr e1 row < eval_expr e2 row
  | GreaterThan (e1, e2) -> eval_expr e1 row > eval_expr e2 row

let rec eval_select select =
  let table = List.assoc select.from_table database in
  let filtered_table = 
    match select.where_clause with
    | None -> table
    | Some clause -> (fst table, List.filter(fun row -> eval_clause clause (List.combine(fst table) row))(snd table))
  in
  (fst table, List.map (fun row -> List.map (fun col -> eval_expr (Column col) (List.combine(fst table) row)) select.column_list) (snd filtered_table)) 

let select = {
  column_list = ["Name"; "Age"];
  from_table = "Customer";
  where_clause = Some (Equal (Column "Age", IntLiteral 22));
}

let result = eval_select select

let rec print_table table =
  let rec print_row row =
    match row with
    | [] -> ()
    | [col] -> print_string col
    | col :: rest -> print_string col; print_string ", "; print_row rest
  in
  match table with
  | [] -> ()
  | [row] -> print_row row
  | row :: rest -> print_row row; print_newline (); print_table rest

  
let () = print_table (snd result);      (*It should print "Catherine" , "22" *)
print_newline ();
print_newline ();



(* Defining the AST for the INSERT query *)
type insert = 
{
  into_table: string;
  insert_values: expression list;
}

(* INSERT: Implementing a toy SQL Interpreter in OCaml*)
let rec eval_insert insert =
  let table = List.assoc insert.into_table database in
  let new_row = List.map (fun expr -> eval_expr expr []) insert.insert_values in
  let updated_table = (fst table, snd table @ [new_row]) in
  updated_table

let insert = 
{
  into_table = "Customer";
  insert_values = [StringLiteral "Diana"; IntLiteral 23; StringLiteral "San Francisco"];
}

let updated_table = eval_insert insert

let () = print_table (snd updated_table);         (*It should print original table with new insertion "Diana" , "23" , "San Francisco" *)
print_newline ();
print_endline "Insertion into DB successful!";
print_newline ();



(* Defining the AST for the DELETE query *)
type delete = 
{
  from_table: string;
  where_clause: clause option;
}

(* DELETE: Implementing a toy SQL Interpreter in OCaml*)
let rec eval_delete delete =
  let table = List.assoc delete.from_table database in
  let filtered_table =
    match delete.where_clause with
    | None -> table
    | Some clause ->
      let filtered_rows = List.filter (fun row -> not (eval_clause clause (List.combine (fst table) row))) (snd table) in
      (fst table, filtered_rows)
  in
  filtered_table

let delete =
{
  from_table = "Customer";
  where_clause = Some (Equal (Column "Age", IntLiteral 20));
}

let filtered_table = eval_delete delete

let () = print_table (snd filtered_table);        (*It should delete the row in DB whose age is 20 i.e, details of Alex *)
print_newline ();
print_endline "Deletion from DB successful!";
print_newline ();



(* Defining the AST for the UPDATE query *)
type update = 
{
  table_name: string;
  set_clause: (column * expression) list;
  where_clause: clause option;
}

(* UPDATE: Implementing a toy SQL Interpreter in OCaml*)
let rec eval_update update =
  let table = List.assoc update.table_name database in
  let updated_table =
    match update.where_clause with
    | None -> table
    | Some clause ->
      let updated_rows =
        List.map
          (fun row ->
            if eval_clause clause (List.combine (fst table) row) then
              List.map
                (fun (col, expr) ->
                  if List.mem_assoc col update.set_clause then
                    eval_expr (List.assoc col update.set_clause) (List.combine (fst table) row)
                  else
                    eval_expr (Column col) (List.combine (fst table) row)
                )
                (List.combine (fst table) row) (*columns*)
            else
              row
          )
          (snd table) (*rows*)
      in
      (fst table, updated_rows)
  in
  updated_table
  

let update =
{
  table_name = "Product";
  set_clause = [("Quantity", IntLiteral 100)];
  where_clause = Some (Equal (Column "P_Name", StringLiteral "T-Shirt"));
}

let updated_table1 = eval_update update

let () = print_table (snd updated_table1);        (*It should update the Quantity of T-Shirt from 1 to 100 *)
print_newline ();
print_endline "Updated the DB successful!";
print_newline ();


(* Defining the AST for the CREATE TABLE query*)
type create_table = 
{
  table_name: string;
  columns: (column * string) list;
}

(* CREATE TABLE: Implementing a toy SQL Interpreter in OCaml*)
let rec eval_create_table create_table =
  try
    let _ = List.assoc create_table.table_name database in
    failwith ("Table " ^ create_table.table_name ^ " already exists") (* Table exists? throw an exception *)
  with
  | Not_found ->
    let columns = (List.map fst create_table.columns) in 
    let new_table = (columns, []) in               (* Else Create Table *)
    let updated_database = (create_table.table_name, new_table) :: database in
    (new_table, updated_database)

(* For the above query/function, can be also done as taking database as an argument, so you could call them again on the updated database.  *)


let create_table =
{
  table_name = "Bank";
  columns = [("Account_No", "int"); ("Branch", "string"); ("Balance", "string")];
}

let new_table, updated_database = eval_create_table create_table

let () =
  let column_names = fst new_table in
  let table_data = snd new_table in
  print_table (column_names :: table_data);     (* It should print the empty created table with column names *)
  print_newline ();
  print_endline "Created a new table in DB successful!";
  print_newline ();