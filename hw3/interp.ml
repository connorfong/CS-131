
(* EXCEPTIONS *)

(* This is a marker for places in the code that you have to fill in.
   Your completed assignment should never raise this exception. *)
exception ImplementMe of string

(* This exception is thrown when a type error occurs during evaluation
   (e.g., attempting to invoke something that's not a function).
*)
exception DynamicTypeError

(* This exception is thrown when pattern matching fails during evaluation. *)  
exception MatchFailure  

(* EVALUATION *)

(* See if a value matches a given pattern.  If there is a match, return
   an environment for any name bindings in the pattern.  If there is not
   a match, raise the MatchFailure exception.
*)
let rec patMatch (pat:mopat) (value:movalue) : moenv =
  match (pat, value) with
      (* an integer pattern matches an integer only when they are the same constant;
	 no variables are declared in the pattern so the returned environment is empty *)
		(IntPat(i), IntVal(j)) when i=j -> Env.empty_env()
	|	(BoolPat(i), BoolVal(j)) when i=j -> Env.empty_env()
	|	(WildcardPat, x) -> Env.empty_env()
	(* If pattern matching with a variable, the variable is added to the environment,
		mapped to the value*)
	|	(VarPat(i), x) -> Env.add_binding i x (Env.empty_env())
	|	(NilPat, x) -> (match x with
							ListVal(NilVal) -> Env.empty_env()
						|	_ -> raise (MatchFailure))
	(* When matching with :: the values are in the form ListVal(ConsVal v(Consval ...(NilVal))).
		These values must be unpacked using matches, then repacked to pass to the patMatch function.*) 
	|	(ConsPat(i, j), x) -> (match x with
									ListVal(ConsVal(y, ys)) -> (match ys with
																	NilVal -> let tempEnv = patMatch j (ListVal(NilVal)) in
																				let tempEnv2 = patMatch i y in
																					Env.combine_envs tempEnv tempEnv2
																|	ConsVal(t, u) -> let tempEnv = patMatch j (ListVal(ConsVal(t, u))) in
																						let tempEnv2 = patMatch i y in
																							Env.combine_envs tempEnv tempEnv2)
								|	_ -> raise (MatchFailure))
    |	 _ -> raise (MatchFailure)

	
(*	My helper function that evaluates the correct binary operation given 2 ints and a moop
	from evalExpr. It returns the value in the correct type*)
let evalOp (e1:int) (op:moop) (e2:int) : movalue =
	match op with
		Plus -> IntVal(e1 + e2)
	|	Minus -> IntVal(e1 - e2)
	|	Times -> IntVal(e1 * e2)
	|	Eq -> if e1 = e2 then BoolVal(true)
				else BoolVal(false)
	|	Gt -> if e1 > e2 then BoolVal(true)
				else BoolVal(false)
	|	_ -> raise (DynamicTypeError)

    
(* Evaluate an expression in the given environment and return the
   associated value.  Raise a MatchFailure if pattern matching fails.
   Raise a DynamicTypeError if any other kind of error occurs (e.g.,
   trying to add a boolean to an integer) which prevents evaluation
   from continuing.
*)
let rec evalExpr (e:moexpr) (env:moenv) : movalue =
  match e with
      (* an integer constant evaluates to itself *)
		IntConst(i) -> IntVal(i)
	| 	BoolConst(i) -> BoolVal(i)
	|	Nil -> ListVal(NilVal)
	|	Var(i) -> (try Env.lookup i env with Env.NotBound -> raise (DynamicTypeError))
	(* The BinOp function evaluates both expressions, then uses matches to check the type of the 
		operands.  If ints, they are passed to the helper function, evalOp.  If the right operand is a
		list, it then checks if the op is Cons.  If so it returns the new list*)
	|	BinOp(e1, op, e2) -> (let e1Val = evalExpr e1 env in
								let e2Val = evalExpr e2 env in
									(match (e1Val, e2Val) with
										(IntVal(t), IntVal(u)) when op != Cons -> evalOp t op u
									|	(t, ListVal(u)) when op = Cons -> ListVal(ConsVal(t, u)) 
									|	(_, _) -> raise (DynamicTypeError)))
	(* The If function evaluates the condition, then matches it to a boolean.  This boolean
		is checked using an if statement, if true then evaluate the thn, if false then els.*)
	|	If(cond, thn, els) -> (match evalExpr cond env with
									BoolVal(b) -> if b then evalExpr thn env
													else evalExpr els env
								|	_ -> raise (DynamicTypeError))
	(* A function evaluates to itself, meaning it simply puts itself in the environment*)
	|	Function(pat, exp) -> FunctionVal(None, pat, exp, env)
	(* A function call evaluates the two moexprs passed, and checks the format of the first moexpr.
		If the first is a FunctionVal, it checks if it has a name, meaning it is recursively called,
		or not.  If not, it simply pattern matches the pattern from the function with the argument passed
		and evaluates the function's expression with the patMatch'ed environment.  If the function does 
		have a name, the same steps are followed, but the function name must be added to the new environment,
		mapping to the function itself.*)
	| 	FunctionCall(e1, e2) -> (let f = evalExpr e1 env in
									let arg = evalExpr e2 env in
										match f with
											FunctionVal(Some x, pat, exp, env) -> evalExpr exp (Env.combine_envs env (Env.add_binding x f (patMatch pat arg)))
										|	FunctionVal(None, pat, exp, env) -> evalExpr exp (Env.combine_envs env (patMatch pat arg))
										|	_ -> raise (DynamicTypeError))
	(* Match has a nested recursive function, which trys the patMatch function using the pattern
		from the list of patterns and returns the expression and environment with the mapped pattern.
		If the first element raises MatchFailure, the helper function is recursively until a match
		occurs or the list is exhausted.  The matched expression is evaluated with the patMatch environment,
		combined with the current environment in case of recursion.*)
	|	Match(exp, l) -> let u = evalExpr exp env in
							let rec matchList (value:movalue) (l:(mopat * moexpr) list) : (moexpr * moenv) =
								match l with
									[] -> raise (MatchFailure)
								|	(pat, e)::rest -> try (e, patMatch pat value)
														with MatchFailure -> matchList value rest
							in match matchList u l with
								(e, en) -> evalExpr e (Env.combine_envs env en)



(* Evaluate a declaration in the given environment.  Evaluation
   returns the name of the variable declared (if any) by the
   declaration along with the value of the declaration's top-level expression.
*)
let rec evalDecl (d:modecl) (env:moenv) : moresult =
  match d with
		Expr(e) -> (None, evalExpr e env)
	(* Let simply returns the name x and the evaluated expression, using evalExpr*)
    | 	Let(x, i) -> (Some x, evalExpr i env)
	(* LetRec returns the name, and maps it to the FunctionVal using the passed values,
	and most importantly adds its own name to the name to the FunctionVal so that it can later be
	read and added to the new environment during execution*)
	|	LetRec (x, pat, exp) -> (Some x, FunctionVal(Some x, pat, exp, env))
					
					
					
					
					
					
					
					
					
					
					
					
					
						
						
						
						
						
						
						
						
						
				