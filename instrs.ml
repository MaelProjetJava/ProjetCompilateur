(* Instructions of the CAM *)

open Miniml;;

type instr =
  PrimInstr of primop
| Cons
| Push
| Swap
| Return
| Quote of value
| Cur of code
| App
| Branch of code * code
(* new for recursive calls *)
| Call of var
| AddDefs of (var * code) list
| RmDefs of int
and value =
  NullV 
| VarV of Miniml.var
| IntV of int
| BoolV of bool
| PairV of value * value
| ClosureV of code * value
and code = instr list
  
type stackelem = Val of value | Cod of code

type envelem = EVar of var | EDef of var list;;


let rec chop n l =
	if n = 0 then
		l
	else
		chop (n - 1) (List.tl l)
;;

let rec exec = function
  (PairV (x,y), (PrimInstr (UnOp Fst))::inslist, stack, fds) -> exec (x, inslist, stack, fds)
| (PairV (x,y), (PrimInstr (UnOp Snd))::inslist, stack, fds) -> exec (y, inslist, stack, fds)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAadd)))::inslist, stack, fds) -> exec (IntV (x + y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAsub)))::inslist, stack, fds) -> exec (IntV (x - y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmul)))::inslist, stack, fds) -> exec (IntV (x * y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAdiv)))::inslist, stack, fds) -> exec (IntV (x / y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BArith BAmod)))::inslist, stack, fds) -> exec (IntV (x mod y), inslist, stack, fds)

| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCeq)))::inslist, stack, fds) -> exec (BoolV (x = y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCge)))::inslist, stack, fds) -> exec (BoolV (x >= y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCgt)))::inslist, stack, fds) -> exec (BoolV (x > y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCle)))::inslist, stack, fds) -> exec (BoolV (x <= y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BClt)))::inslist, stack, fds) -> exec (BoolV (x < y), inslist, stack, fds)
| (PairV (IntV (x), IntV (y)), (PrimInstr (BinOp (BCompar BCne)))::inslist, stack, fds) -> exec (BoolV (x <> y), inslist, stack, fds)

| (PairV (BoolV (x), BoolV (y)), (PrimInstr (BinOp (BCompar BCeq)))::inslist, stack, fds) -> exec (BoolV (x = y), inslist, stack, fds)
| (PairV (BoolV (x), BoolV (y)), (PrimInstr (BinOp (BCompar BCge)))::inslist, stack, fds) -> exec (BoolV (x >= y), inslist, stack, fds)
| (PairV (BoolV (x), BoolV (y)), (PrimInstr (BinOp (BCompar BCgt)))::inslist, stack, fds) -> exec (BoolV (x > y), inslist, stack, fds)
| (PairV (BoolV (x), BoolV (y)), (PrimInstr (BinOp (BCompar BCle)))::inslist, stack, fds) -> exec (BoolV (x <= y), inslist, stack, fds)
| (PairV (BoolV (x), BoolV (y)), (PrimInstr (BinOp (BCompar BClt)))::inslist, stack, fds) -> exec (BoolV (x < y), inslist, stack, fds)
| (PairV (BoolV (x), BoolV (y)), (PrimInstr (BinOp (BCompar BCne)))::inslist, stack, fds) -> exec (BoolV (x <> y), inslist, stack, fds)

| (x, (Quote v)::inslist, stack, fds) -> exec (v, inslist, stack, fds)
| (x, Cons::inslist, (Val y)::stack, fds) -> exec (PairV (x, y), inslist, stack, fds)

| (x, Push::inslist, stack, fds) -> exec (x, inslist, (Val (x))::stack, fds)
| (x, Swap::inslist, (Val y)::stack, fds) -> exec (y, inslist, (Val x)::stack, fds)

| (x, (Cur code)::inslist, stack, fds) -> exec (ClosureV (code, x), inslist, stack, fds)
| (PairV (ClosureV (code, value), arg), App::inslist, stack, fds) -> exec (PairV (value, arg), code, (Cod inslist)::stack, fds)
| (x, Return::inslist, (Cod newInsList)::stack, fds) -> exec (x, newInsList, stack, fds)

| (BoolV (true), (Branch (t, e))::inslist, (Val x)::stack, fds) -> exec (x, t, (Cod inslist)::stack, fds)
| (BoolV (false), (Branch (t, e))::inslist, (Val x)::stack, fds) -> exec (x, e, (Cod inslist)::stack, fds)

| (x, (Call (f))::inslist, stack, fds) -> exec (x, (List.assoc f fds)@inslist, stack, fds)
| (x, (AddDefs (defs))::inslist, stack, fds) -> exec (x, inslist, stack, defs@fds)
| (x, (RmDefs(n))::inslist, stack, fds) -> exec (x, inslist, stack, chop n fds)

| config -> config
;;

let rec contains v = function
  x::l -> (v = x) || (contains v l)
| _ -> false
;;

let access (v : var) env =
	let rec helper fstsnd_list = function
		(EVar x)::envt ->
			if v = x then
				fstsnd_list@[PrimInstr (UnOp (Snd))]
			else
				helper (fstsnd_list@[PrimInstr (UnOp (Fst))]) envt
		| (EDef x_list)::envt ->
			if contains v x_list then
				[Call v]
			else
				helper (fstsnd_list@[PrimInstr (UnOp (Fst))]) envt
		| _ -> failwith "La variable n'est pas définie !"
	in
		helper [] env
;;

let rec compile env = function
  Bool(b) -> [Quote(BoolV(b))]
| Int(i) -> [Quote(IntV(i))]
| Var(v) -> (access v env)
| Pair (e1, e2) -> [Push] @ (compile env e1) @ [Swap] @ (compile env e2) @ [Cons]
| App (PrimOp (p), e) -> (compile env e) @ [PrimInstr (p)]
| Fn (v, e) -> [Cur ((compile (EVar(v)::env) e) @ [Return])]
| App (f, a) -> [Push] @ (compile env f) @ [Swap] @ (compile env a) @ [Cons; App]
| Cond (i, t, e) -> [Push] @ (compile env i) @ [Branch ((compile env t) @ [Return], (compile env e) @ [Return])]
| Fix (defs, e) ->
	let new_env = (EDef (List.map fst defs))::env in
		let dc = List.map (fun (v, expr) -> (v, compile new_env expr)) defs in
			let ec = compile new_env e
	in
		[AddDefs dc] @ ec @  [RmDefs (List.length dc)]
| _ -> failwith "Syntaxe invalide !"
;;

let compile_prog = function
	Prog (_, t) -> compile [] t
;;
