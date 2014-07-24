(* ************************************************************************ *)
(*																			*)
(* OCam'OLE - OLE Automation binding for OCaml								*)
(*																			*)
(*	olegen tool - ocaml static code generation from OLE type library		*)
(*																			*)
(* (c)2002 Nicolas Cannasse													*)
(* (c)2002 Motion-Twin														*)
(* (c)2002 Lexifi															*)
(*																			*)
(* ************************************************************************ *)

open Printf
open Ocamole

(* ************************************************************************ *)
(* Type Declaration *)

type caml_type_desc =
  | Unit
  | Float
  | Date
  | String
  | Com of guid
  | Bool
  | Variant
  | Int
  | Int32
  | NotSupported
	
type variant_type = {
	native : type_desc;
	ocaml : caml_type_desc;
}

type arg = {
	aname : string;
	atype : variant_type;
	aopt : bool;
	aout : bool;
}

type func = {
	fname : string;
	fargs : arg list;
	fval : variant_type;
	fkind :	invoke_kind;
}

type prop = {
	pname : string;
	ptype : variant_type;
	pbyref : bool;
}

type var = {
	vname : string;
	vval : variant;
}

type com_type = {
	tname : string;
	ttype : itypeinfo option;
	tguid : guid;
	therited : guid list;
	tfuns : func list;
	tprops : prop list;
	tvars : var list;
	thidden : bool;
	tskip : bool;
	tisenum : bool;
	tenumtype : variant_type;
}

exception Type_found of itypeinfo
exception Enum_found of (guid * itypeinfo)

(* ************************************************************************ *)
(* Globals *)

let gnull = guid_null()
let com_types_done = ref []
let com_types_todo = ref []
let com_enums = ref [(gnull,"guid");(gnull,"dispparams");(gnull,"excepinfo")]

let get_type g =
	List.find (fun t -> guid_eq g t.tguid) !com_types_done

let make_ilist n =
	let rec ilist_rec i =
		if i = n then [] else i::(ilist_rec (i+1))
	in
	ilist_rec 0

let get_tinf_guid t =
	(ITypeInfo.get_type_attr t).ta_guid

let error = failwith

(* ************************************************************************ *)
(* OCaml name generation (from COM identifiers *)

let is_reserved_word =
	let reserved_words = Hashtbl.create 0 in
	let add w = Hashtbl.add reserved_words w () in
		List.iter add [
		"and";"as";"assert";"begin";"class";"constraint";"do";"done";"downto";
		"else";"end";"exception";"external";"false";"for";"fun";"function";
		"functor";"if";"in";"include";"inherit";"initializer";"lazy";"let";
		"match";"method";"module";"mutable";"new";"not";"object";"of";"open";
		"or";"private";"rec";"sig";"struct";"then";"to";"true";"try";"type";
		"val";"virtual";"when";"while";"with";"this";"self"];
	fun w ->
		(try
			Hashtbl.find reserved_words w;
			true;
		with
			Not_found -> false)

let rec make_type_name str =
	let to_lower c =
		if c >= 'A' && c <= 'Z' then char_of_int ((int_of_char c) - (int_of_char 'A') + (int_of_char 'a')) else c
	in
	let rec normalize_word name i inword =
		if String.length name <= i then name else
		if String.get name i >= 'A' && String.get name i <= 'Z' then
			match inword || String.get name (i-1) = '_' with
			| true -> String.set name i (to_lower (String.get name i)); normalize_word name (i+1) inword
			| false ->
				let snd_part = String.sub name i ((String.length name)-i) in
					String.set snd_part 0 (to_lower (String.get snd_part 0));
				(String.sub name 0 i)^"_"^(normalize_word snd_part 1 true)
		else
			normalize_word name (i+1) false
	in
	let name = String.uncapitalize str in
	match is_reserved_word name with
	| true -> make_type_name ("I"^name)
	| false -> normalize_word name 1 true

let make_module_name = String.capitalize

let make_arg_name = make_type_name

let make_func_name n t =
	match t with
	| Invoke_func -> make_type_name n
	| Invoke_propertyget -> "get_"^(make_type_name n)
	| Invoke_propertyput
	| Invoke_propertyputref -> "put_"^(make_type_name n)

let rec type_string t =
	match t.ocaml with
	| Unit -> "unit"   
	| Float -> "float"
	| Date -> "date"
	| String -> "string"
	| Int -> "int"
	| Int32 -> "int32"
	| Bool -> "bool"
	| Variant -> "variant"
	| NotSupported -> "unit (* not supported *)"
	| Com g ->
		try
			make_type_name (get_type g).tname
		with
			Not_found -> failwith (string_of_guid g)

(* ************************************************************************ *)
(* Type Generation *)

let alloc_new_enum tinf =
	let tlib,tindex = ITypeInfo.get_type_lib tinf in
	let tname = ITypeLib.get_type_name tlib tindex in
	let tname = make_type_name tname in
	try
		fst (List.find (fun (_,n) -> n = tname) !com_enums)
	with
		Not_found ->
			let g = guid_of_string (sprintf "{FFFFFFFF-FFFF-FFFF-FFFF-FFFF%.8X}" (List.length !com_enums)) in
			com_enums := (g,tname) :: !com_enums;
			raise (Enum_found (g,tinf))

let variant_type_eq t t' =
	match t.ocaml , t'.ocaml with
	| Com g, Com g' -> guid_eq g g'
	| Com _,_ 
	| _,Com _ -> false
	| _  -> t.ocaml = t'.ocaml

let rec generate_variant_type ?(hasint32=false) = function
  | VTY_VOID | VTY_EMPTY -> Unit
  | VTY_R4 | VTY_R8 -> Float
  | VTY_DATE -> Date
  | VTY_BSTR -> String
  | VTY_DISPATCH -> Com (guid_idispatch())
  | VTY_BOOL -> Bool
  | VTY_VARIANT -> Variant
  | VTY_UNKNOWN -> Com (guid_iunknown())
  | VTY_UI2
  | VTY_I1 
  | VTY_I2 -> Int
  | VTY_I4
  | VTY_UI4
  | VTY_HRESULT
  | VTY_INT
  | VTY_UINT -> if hasint32 then Int32 else Int
  | VTY_PTR v -> generate_variant_type v
  | VTY_NOTSUPPORTED -> NotSupported
  | VTY_USERDEFINED tinf ->
		let tattr = ITypeInfo.get_type_attr tinf in
		let g = tattr.ta_guid in
		match guid_eq g gnull with
		| true -> Com (alloc_new_enum tinf)
		| false ->
			match (List.exists (fun (g',_) -> guid_eq g g') !com_types_todo ||
				  List.exists (fun t -> guid_eq g t.tguid) !com_types_done) with
			| true -> Com g
			| false -> raise (Type_found tinf)

let type_of_variant v = 
	let rec aux = function
	  | VT_VOID -> VTY_VOID
	  | VT_EMPTY -> VTY_EMPTY
	  | VT_R4 _ -> VTY_R4
	  | VT_R8 _ -> VTY_R8
	  | VT_DATE _ -> VTY_DATE
	  | VT_BSTR _ -> VTY_BSTR
	  | VT_DISPATCH _ -> VTY_DISPATCH
	  | VT_BOOL _ -> VTY_BOOL
	  | VT_UNKNOWN _ -> VTY_UNKNOWN
	  | VT_UI2 _ -> VTY_UI2
	  | VT_UI4 _
	  | VT_C_UI4 _ -> VTY_UI4
	  | VT_I1 _ -> VTY_I1
	  | VT_I2 _ -> VTY_I2
	  | VT_I4 _
	  | VT_C_I4 _ -> VTY_I4
	  | VT_INT _
	  | VT_C_INT _ -> VTY_INT
	  | VT_UINT _
	  | VT_C_UINT _ -> VTY_UINT
	  | VT_HRESULT _
	  | VT_C_HRESULT _ -> VTY_HRESULT
	  | VT_CLSID _ -> assert false	
	  | VT_PTR v -> VTY_PTR (aux v)
	in
	let vtype = aux v in
	{ native = vtype; ocaml = generate_variant_type vtype; }

let generate_arg (a,aname) =
	let is_opt_flag f =
		match f with
		| Paramflag_fopt | Paramflag_foptdefault _ -> true
		| _ -> false
	in
	{
		aname = aname;
		atype = { native = a.ed_type; ocaml = generate_variant_type a.ed_type };
		aopt = List.exists is_opt_flag a.ed_param;
		aout = not (List.exists (fun x -> x = Paramflag_fin) a.ed_param);
	}

let generate_func t n =
	let fdesc = ITypeInfo.get_func_desc t n in
	let names = List.rev (ITypeInfo.get_names t fdesc.fd_memid) in
	let fname = List.hd names (* function always has a name *) in
	let arg_names = match fdesc.fd_ikind with
		| Invoke_func
		| Invoke_propertyget -> List.tl names
		| _ ->
			if List.length names > List.length fdesc.fd_params then
				List.tl names
			else
				names
	in
	Some {
		fname = fname;
		fargs = List.map generate_arg (List.combine fdesc.fd_params arg_names);
		fval = (generate_arg (fdesc.fd_retval,"")).atype;
		fkind = fdesc.fd_ikind;
	}

let generate_prop f =
	{
		pname = f.fname;
		ptype = (List.hd f.fargs).atype; (* ensured by filter *)
		pbyref = (f.fkind = Invoke_propertyputref);
	}

let generate_var t n =
	let vdesc = ITypeInfo.get_var_desc t n in
	let names = List.rev (ITypeInfo.get_names t vdesc.vd_memid) in
	match vdesc.vd_kind with
	| Var_const v ->
		{
			vname = List.hd names;
			vval = v;
		}
	| _ -> failwith "Unsupported variable kind"
		

let generate_type (guid,t) =
	let tattr = ITypeInfo.get_type_attr t in
	let rec make_type_unique tname =
		if String.get tname 0 = '_' then
			make_type_unique ((String.sub tname 1 ((String.length tname)-1))^"_")
		else
			let rtname = make_type_name tname in
			match List.exists (fun t -> make_type_name t.tname = rtname) !com_types_done with
			| true -> make_type_unique ("I"^tname)
			| false -> tname
	in
	let rec some_only = function
		| [] -> []
		| None::l -> some_only l
		| (Some x)::l -> x::(some_only l)
	in
	let test_parent n =
		let tinf = ITypeInfo.get_interface t n in
		let g = get_tinf_guid tinf in
		match List.exists (fun (g',_) -> guid_eq g g') !com_types_todo with
		| true -> raise Exit
		| false -> try get_type g with Not_found -> raise (Type_found tinf)
	in
	let rec has_method f t =
		try
			List.exists (fun f' -> f.fname = f'.fname) t.tfuns ||
			List.exists (fun p -> p.pname = f.fname) t.tprops ||
			List.exists (has_method f) (List.map get_type t.therited)
		with
			Not_found -> assert false
	in
	try		
		let parents = List.map test_parent (make_ilist tattr.ta_ninterfaces) in
		let tlib,tindex = ITypeInfo.get_type_lib t in
		let tname = ITypeLib.get_type_name tlib tindex in		
		let methods = some_only (List.map (generate_func t) (make_ilist tattr.ta_nfuncs)) in
		let methods = List.filter (fun f -> not(List.exists (has_method f) parents)) methods in
		let methods = List.sort	(fun f f' -> compare
				(make_func_name f.fname f.fkind)
				(make_func_name f'.fname f'.fkind)
			) methods in
		let props,funs = List.partition (fun f ->
			f.fkind <> Invoke_func &&
			List.exists (fun f' ->
				f.fname = f'.fname &&
				if f.fkind = Invoke_propertyget then
					(f'.fkind = Invoke_propertyput || f'.fkind = Invoke_propertyputref) &&
					(match f'.fargs with a::[] when variant_type_eq a.atype f.fval -> true | _ -> false)
				else
					f'.fkind = Invoke_propertyget &&
					(match f.fargs with a::[] when variant_type_eq a.atype f'.fval -> true | _ -> false)
			) methods) methods in
		let props = List.map generate_prop (List.filter (fun f -> f.fkind <> Invoke_propertyget) props) in
		let props = List.sort (fun p p' -> compare p.pname p'.pname) props in		
		let vars = List.sort (fun v v' -> compare v.vname v'.vname) (List.map (generate_var t) (make_ilist tattr.ta_nvars)) in
		let isenum = (tattr.ta_kind = Tkind_enum) in
		let etype = match isenum with
			| true ->
				(match vars with
				| [] -> failwith ("Empty enum found : "^tname)
				| vref::_ ->
					let tref = type_of_variant vref.vval in
					try
						ignore(List.find (fun v ->							
							let t = type_of_variant v.vval in
							not (variant_type_eq t tref)) vars);
						failwith ("Enums with multiple types are not supported ("^tname^")");
					with
						Not_found -> tref
				);				
			| false -> { native = VTY_VOID; ocaml = Unit; }
		in
		Some {
			tname = (make_type_unique tname);
			tguid = guid;
			ttype = Some t;
			therited = List.map (fun t -> t.tguid) parents;
			tfuns = funs;
			tprops = props;
			tvars = vars;
			thidden = false;
			tskip = false;
			tisenum = isenum;
			tenumtype = etype;
		}
	with
		Exit -> None

let rec generate_types todo tdone =
	let ltodo = float_of_int (List.length todo) in
	let ldone = float_of_int (List.length tdone) in
	let dots = String.make ((List.length tdone) mod 5) '.' in
	prerr_string (sprintf "%d%%%s    \r" (int_of_float (ldone *. 100. /. (ltodo+.ldone))) dots);
	flush stderr;
	com_types_done := tdone;
	com_types_todo := todo;
	match todo with
	| [] -> []
	| x::todo ->
		try
			match generate_type x with
			| None -> generate_types (todo@[x]) tdone
			| Some x ->
				x::(generate_types todo (x::tdone))
		with
			| Type_found tinf ->
				let g = get_tinf_guid tinf in
				generate_types ((g,tinf)::x::todo) tdone
			| Enum_found (g,tinf) ->
				generate_types ((g,tinf)::x::todo) tdone

let generate class_name =
	let basic_type n g funs = {
		tname = n;
		tguid = g;
		ttype = None;
		therited = (if guid_eq g (guid_iunknown()) then [] else [guid_iunknown()]);
		tfuns = List.map (fun n -> {
			fname = n;
			fargs = [];
			fval = { native = VTY_VOID; ocaml = Unit; };
			fkind = Invoke_func; }) funs;
		tprops = [];
		tvars = [];
		thidden = true;
		tskip = true;
		tisenum = false;
		tenumtype = { native = VTY_VOID; ocaml = Unit; };
	}
	in
	let basic_types = [
		basic_type "iunknown" (guid_iunknown()) ["QueryInterface";"AddRef";"Release"];
		basic_type "idispatch" (guid_idispatch()) ["GetTypeInfoCount";"GetTypeInfo";"GetIDsOfNames";"Invoke"];
		basic_type "property" gnull [];	(* reserved type *)	
	] in
	try
		let guid = guid_of_progid class_name in
		let disp = create_idispatch guid in
		match IDispatch.get_type_info_count disp with
		| 0 -> error ("This class name does not have OLE support : "^class_name);
		| 1 ->			
			let tinf = IDispatch.get_type_info disp 0 in
			let tlib, _ = ITypeInfo.get_type_lib tinf in
			let ilist = make_ilist (ITypeLib.get_type_info_count tlib) in
			let tlist = List.map (ITypeLib.get_type_info tlib) ilist in
			let glist = List.map get_tinf_guid tlist in
			let no_enums l = List.filter (fun (g,_) -> not (guid_eq g gnull)) l in
			let types = generate_types (no_enums (List.combine glist tlist)) basic_types in
			let types = List.sort (fun t t' -> compare t.tname t'.tname) types in
			prerr_endline "Generating ML/MLI code...";
			List.filter (fun t -> not t.tskip) types
		| _ ->
			assert false (* does not exists *)
	with
		Invalid_progid str -> error ("Invalid class name : "^class_name)

(* ************************************************************************ *)
(* MLI Module Generation *)

let output = ref stdout

let printf x = fprintf !output x

let out_vals f =			
	(List.filter (fun a -> a.aout) f.fargs)@(
		match f.fval.ocaml with
		| Unit -> []
		| _ -> [{
			aname = "_ret";
			atype = f.fval;
			aopt = false;
			aout = true;
		}])
		
let gen_mli_herited g =
	if not (guid_eq g (guid_iunknown())) then
	try
		let t = get_type g in
		printf "\tval %s : t -> %s\n" (make_func_name t.tname Invoke_func) (make_type_name t.tname)
	with
		Not_found -> assert false

let gen_mli_fun f =
	printf "\tval %s : " (make_func_name f.fname f.fkind);
	let fothers = List.filter (fun a -> not a.aout) f.fargs in
	let fopt,fargs = List.partition (fun a -> a.aopt) fothers in
	(match fopt with
	| [] -> ()
	| _ -> List.iter (fun a -> printf "?%s:%s -> " (make_arg_name a.aname) (type_string a.atype)) fopt);
	printf "t -> ";
	List.iter (fun a -> printf "%s:%s -> " (make_arg_name a.aname) (type_string a.atype)) fargs;
	(match out_vals f with
	| [] -> printf "unit"
	| a::[] -> printf "%s" (type_string a.atype)
	| l -> printf "(%s)" (String.concat " * " (List.map (fun a -> type_string a.atype) l)));
	printf "\n"

let gen_mli_prop p =
	printf "\tval %s : t -> %s property\n" (make_func_name p.pname Invoke_func) (type_string p.ptype)

let gen_mli_type t =
	printf "module %s : sig\n" (make_module_name t.tname);
	printf "\ttype t = %s (* %s *)\n" (make_type_name t.tname) (string_of_guid t.tguid);
	printf "\tval guid : guid\n";
	printf "\tval query : ?unsafe:bool -> idispatch -> t\n";
	(try
		ignore(create_idispatch t.tguid);
		printf "\tval create : unit -> t\n"
	with
		Invalid_guid _ -> ());
	printf "\n";
	List.iter gen_mli_herited t.therited;
	printf "\n";
	List.iter gen_mli_fun t.tfuns;	
	List.iter gen_mli_prop t.tprops;
	List.iter (fun v -> printf "\t(* var %s : not supported *)\n" v.vname) t.tvars;
	printf "end\n\n"

let gen_mli_var v =
	printf "\tval %s : t\n" (make_type_name v.vname)

let gen_mli_enum t =
	printf "module %s : sig\n" (make_module_name t.tname);
	printf "\ttype t = %s\n" (make_type_name t.tname);
	List.iter (fun f -> printf "\t(* func %s : not supported *)\n" f.fname) t.tfuns;
	List.iter gen_mli_var t.tvars;
	printf "end\n\n"

let gen_mli output_file types =
	let ch = open_out (output_file^".mli") in
	output := ch;
	printf "(* Generated by OCam'OLE *)\n";
	printf "open Ocamole\n\n";
	List.iter (fun t -> printf "type %s\n" (make_type_name t.tname)) types;
	printf "\n\n";
	List.iter (fun t -> if t.tisenum then gen_mli_enum t else gen_mli_type t) types;
	close_out ch;
	output := stdout

(* ************************************************************************ *)
(* ML Module Generation *)

let rec vartype_to_variant ?(name = "v") ?(inmatch = false) v =
	match v.native with
  | VTY_NOTSUPPORTED -> "VT_VOID (* no supported *)"
  | VTY_VOID -> "VT_VOID" 
  | VTY_EMPTY -> "VT_EMPTY"
  | VTY_R4 -> "VT_R4 "^name
  | VTY_R8 -> "VT_R8 "^name
  | VTY_DATE -> "VT_DATE "^name
  | VTY_BSTR -> "VT_BSTR "^name
  | VTY_DISPATCH -> "VT_DISPATCH "^name
  | VTY_BOOL -> "VT_BOOL "^name
  | VTY_VARIANT -> name
  | VTY_UNKNOWN -> "VT_UNKNOWN "^name
  | VTY_UI2 -> "VT_UI2 "^name
  | VTY_UI4 -> "VT_UI4 "^name
  | VTY_I1 -> "VT_I1 "^name
  | VTY_I2 -> "VT_I2 "^name
  | VTY_I4 -> "VT_I4 "^name
  | VTY_INT -> "VT_INT "^name
  | VTY_UINT -> "VT_UINT "^name
  | VTY_HRESULT -> "VT_HRESULT "^name
  | VTY_PTR v' ->
		let x = { native = v'; ocaml = v.ocaml } in
		(match v' with
		| VTY_DISPATCH
		| VTY_USERDEFINED _ -> vartype_to_variant x ~name ~inmatch
		| _ -> "VT_PTR ("^(vartype_to_variant x ~name ~inmatch)^")")
  | VTY_USERDEFINED tinf ->
		let g = (match v.ocaml with Com g -> g | _ -> assert false) in
		let t = (try get_type g with Not_found -> assert false) in
		match t.tisenum with
		| true -> vartype_to_variant ~name ~inmatch t.tenumtype
		| false -> 
			if guid_eq g gnull then "VT_CLSID "^name else
			if guid_eq g (guid_iunknown()) then "VT_UNKNOWN "^name else
			if inmatch || (guid_eq g (guid_idispatch())) then "VT_DISPATCH "^name else
			"VT_DISPATCH "^name^".cdispatch"

let rec vartype_from_variant v =
	match v.ocaml with
	| Unit ->  "(fun _ -> ())"
	| Float -> "variant_float"
	| Date -> "variant_date"
	| String -> "variant_string"
	| Bool -> "variant_bool"
	| Int -> "variant_int"
	| Int32 -> "variant_int32"
	| Variant -> "(fun v -> v)"
	| NotSupported -> "(fun _ -> () (* not supported *))"
	| Com g ->
		let t = (try get_type g with Not_found -> assert false) in
		match t.tisenum with
		| true -> vartype_from_variant t.tenumtype
		| false ->
			if guid_eq g gnull then "variant_guid" else
			if guid_eq g (guid_iunknown()) then "variant_iunknown" else
			if guid_eq g (guid_idispatch()) then "variant_idispatch" else
			"(fun v -> { cdispatch = IDispatch.query_interface (variant_idispatch v) (guid_of_string \""^(string_of_guid g)^"\"); cdisptbl = Hashtbl.create 0;})"

let rec vartype_default_value v =
	match v.native with
  | VTY_VOID -> "VT_VOID" 
  | VTY_EMPTY -> "VT_EMPTY"
  | VTY_R4 -> "VT_R4 0.";
  | VTY_R8 -> "VT_R8 0."
  | VTY_DATE -> "VT_DATE 0."
  | VTY_BSTR -> "VT_BSTR \"\""
  | VTY_DISPATCH -> "VT_DISPATCH null_dispatch"
  | VTY_BOOL -> "VT_BOOL false"
  | VTY_VARIANT -> "VT_EMPTY"
  | VTY_UNKNOWN -> "VT_UNKNOWN null_unknown"
  | VTY_UI2 -> "VT_UI2 0"
  | VTY_UI4 -> "VT_UI4 0"
  | VTY_I1 -> "VT_I1 0"
  | VTY_I2 -> "VT_I2 0"
  | VTY_I4 -> "VT_I4 0"
  | VTY_INT -> "VT_INT 0"
  | VTY_UINT -> "VT_UINT 0"
  | VTY_HRESULT -> "VT_HRESULT 0"
  | VTY_PTR v' -> "VT_PTR ("^(vartype_default_value { native = v'; ocaml = v.ocaml })^")"
  | VTY_USERDEFINED tinf ->
		let g = (match v.ocaml with Com g -> g | _ -> assert false) in
		if guid_eq g gnull then
			let t = (try get_type g with Not_found -> assert false) in
			match t.tisenum with
			| false -> "VT_CLSID guid"
			| true -> vartype_default_value t.tenumtype
		else
		if guid_eq g (guid_iunknown()) then "VT_UNKNOWN null_unknown" else
		if guid_eq g (guid_idispatch()) then "VT_DISPATCH null_dispatch" else
		"VT_DISPATCH null_dispatch"
  | VTY_NOTSUPPORTED -> "VT_VOID (* no supported *)"

let gen_ml_arg a =
	let name = make_type_name a.aname in
	match a.aopt, a.aout, a.atype.ocaml with
	| true,_,Variant -> name
	| true,_,_ -> "(match "^name^" with None -> None | Some v -> Some ("^(vartype_to_variant a.atype)^"))"
	| _,true,_ -> "Some ("^(vartype_default_value a.atype)^")"
	| _,false,_ -> "Some ("^(vartype_to_variant a.atype ~name)^")"

let gen_ml_retval_match args =
	let aux a = 
		vartype_to_variant ~name:(make_type_name a.aname) ~inmatch:true a.atype
	in
	String.concat ";" (List.map aux args)

let gen_ml_retval args =
	let aux a =
		match a.atype.native with
		| VTY_NOTSUPPORTED -> "()"
		| _ ->
			let name = make_type_name a.aname in
			match a.atype.ocaml with
			| Com g ->
				if guid_eq g gnull ||
				   guid_eq g (guid_iunknown()) ||
				   guid_eq g (guid_idispatch()) ||
				   (try (get_type g).tisenum with Not_found -> assert false) then
					name
				else
					"{ cdispatch = "^name^"; cdisptbl = Hashtbl.create 0; }"
			| _ -> name
	in
	String.concat "," (List.map aux args)

let gen_ml_fun t f =
	let rec indexes_of_out_args pos = function	
		| [] -> []
		| a::l when a.aout -> pos::(indexes_of_out_args (pos+1) l)
		| _::l -> indexes_of_out_args (pos+1) l
	in
	let fkind_str =
		match f.fkind with
		| Invoke_func -> "Invoke_func"
	    | Invoke_propertyget -> "Invoke_propertyget"
		| Invoke_propertyput -> "Invoke_propertyput"
		| Invoke_propertyputref -> "Invoke_propertyputref"
	in
	printf "\tlet %s " (make_func_name f.fname f.fkind);
	let fout,fothers = List.partition (fun a -> a.aout) f.fargs in
	let fopt,fargs = List.partition (fun a -> a.aopt) fothers in
	(match fopt with
	| [] -> ()
	| _ -> List.iter (fun a -> printf "?%s " (make_arg_name a.aname)) fopt);
	printf "t ";
	List.iter (fun a -> printf "~%s " (make_arg_name a.aname)) fargs;
	printf "=\n";
	printf "\t\tlet did = _dispid t \"%s\" in\n" f.fname;
	printf "\t\tlet args = [%s] in\n" (String.concat ";" (List.rev (List.map gen_ml_arg f.fargs)));
	let indexes = List.map string_of_int (indexes_of_out_args 0 f.fargs) in	
	(match out_vals f with
	| [] -> printf "\t\tignore(invoke t.cdispatch did args [%s] %s)\n" (String.concat ";" indexes) fkind_str;
	| l ->
		printf "\t\tmatch invoke t.cdispatch did args [%s] %s with\n" (String.concat ";" indexes) fkind_str;
		printf "\t\t| [%s] -> (%s)\n" (gen_ml_retval_match l) (gen_ml_retval l);
		printf "\t\t| _ -> invoke_error \"%s.%s\"\n" (make_module_name t.tname) (make_func_name f.fname f.fkind));
	printf "\n"

let gen_ml_herited g =
	if not (guid_eq g (guid_iunknown())) then
	let t = (try get_type g with Not_found -> assert false) in
	printf "\tlet %s t = "  (make_func_name t.tname Invoke_func);
	if guid_eq g (guid_idispatch()) then printf "t.cdispatch\n" else printf "t\n"

let gen_ml_prop p =
	printf "\tlet %s t = \n" (make_func_name p.pname Invoke_func);
	printf "\t\tlet did = _dispid t \"%s\" in\n" p.pname;
	printf "\t\t{\n";
	printf "\t\t\tpdispatch = t.cdispatch;\n";
	printf "\t\t\tpdispid = did;\n";
	printf "\t\t\tpto_variant = (fun v -> %s);\n" (vartype_to_variant p.ptype);
	printf "\t\t\tpfrom_variant = %s;\n" (vartype_from_variant p.ptype);
	printf "\t\t\tpbyref = %b;\n" p.pbyref;
	printf "\t\t}\n\n"

let gen_ml_type t =
	printf "module %s = struct\n" (make_module_name t.tname);
	printf "\ttype t = %s\n" (make_type_name t.tname);
	printf "\tlet guid = guid_of_string \"%s\"\n" (string_of_guid t.tguid);
	printf "\tlet query ?(unsafe=false) d = { cdispatch = if unsafe then d else query_interface d guid; cdisptbl = Hashtbl.create 0; }\n";
	(try
		ignore(create_idispatch t.tguid);
		printf "\tlet create() = { cdispatch = create_idispatch guid; cdisptbl = Hashtbl.create 0; }\n"
	with
		Invalid_guid _ -> ());
	printf "\n";
	List.iter gen_ml_herited t.therited;
	printf "\n";
	List.iter (gen_ml_fun t) t.tfuns;
	List.iter gen_ml_prop t.tprops;
	printf "end\n\n"

let gen_ml_var v =
	let rec aux = function
	  | VT_VOID
	  | VT_EMPTY -> "()"
	  | VT_R4 f
	  | VT_R8 f -> string_of_float f
	  | VT_DATE d -> string_of_float d
	  | VT_BSTR s -> s
	  | VT_DISPATCH _ -> assert false
	  | VT_BOOL b -> if b then "true" else "false"
	  | VT_UNKNOWN _ -> assert false
	  | VT_UI2 i
	  | VT_UI4 i
	  | VT_I1 i
	  | VT_I2 i
	  | VT_I4 i
	  | VT_INT i
	  | VT_UINT i
	  | VT_HRESULT i -> string_of_int i
	  | VT_C_UI4 i
	  | VT_C_I4 i
	  | VT_C_INT i
	  | VT_C_UINT i
	  | VT_C_HRESULT i -> Int32.to_string i
	  | VT_CLSID _ -> assert false	
	  | VT_PTR v -> aux v
	in
	printf "\tlet %s = %s\n" (make_type_name v.vname) (aux v.vval)

let gen_ml_enum t = 
	printf "module %s = struct\n" (make_module_name t.tname);
	printf "\ttype t = %s\n" (make_type_name t.tname);
	printf "\n";
	List.iter gen_ml_var t.tvars;
	printf "end\n\n"

let gen_ml output_file types =
	let ch = open_out (output_file^".ml") in
	output := ch;
	printf "(* Generated by OCam'OLE *)\n";
	printf "open Ocamole\n";
	printf "open IDispatch\n\n";
	List.iter (fun t ->
		printf "type %s = %s\n"
			(make_type_name t.tname)
			(if t.tisenum then type_string t.tenumtype else "caml_dispatch")
		) types;
	printf "\n\n";
	List.iter (fun t -> if t.tisenum then gen_ml_enum t else gen_ml_type t) types;	
	close_out ch;
	output := stdout

(* ************************************************************************ *)
(* MLI Object generation *)

let gen_oo_mli_herited = function
	| g when guid_eq g (guid_iunknown()) -> ()
	| g when guid_eq g (guid_idispatch()) -> printf "\tmethod idispatch : idispatch\n";
	| g ->
		try
			let t = get_type g in
			printf "\tinherit %s\n" (make_type_name t.tname)
		with
			Not_found -> assert false

let gen_oo_mli_type t =
	let can_create = (try ignore(create_idispatch t.tguid); true with Invalid_guid _ -> false) in
	let tname = make_type_name t.tname in
	(match can_create with
	| true -> printf "class %s :\n  object" tname;
	| false -> printf "class type %s =\n  object" tname);
	printf "\n";
	List.iter gen_oo_mli_herited t.therited;
(*	List.iter gen_mli_fun t.tfuns;	
	List.iter gen_mli_prop t.tprops;*)
	List.iter (fun v -> printf "\t(* var %s : not supported *)\n" v.vname) t.tvars;
	printf "  end\n\n";
	printf "val %s_guid : guid\n" tname;
	printf "val %s_query : ?unsafe:bool -> idispatch -> %s\n\n" tname tname

let gen_oo_mli output_file types =
	let ch = open_out (output_file^".mli") in
	output := ch;
	printf "(* Generated by OCam'OLE *)\n";	
	printf "open Ocamole\n\n";
	List.iter (fun t -> if t.tisenum then printf "type %s\n" (make_type_name t.tname)) types;
	printf "\n\n";
	List.iter (fun t -> if t.tisenum then gen_mli_enum t else gen_oo_mli_type t) types;
	close_out ch;
	output := stdout
 
(* ************************************************************************ *)
(* Main *)

;;
let output_file = ref "ole" in
let all_types = ref false in
let class_name = ref None in
let objgen = ref false in
let usage =
  "OLEGEN/OCAMOLE v1.0\n"
  ^"Copyright (C)2002 Nicolas Cannasse\n"
  ^"Copyright (C)2002 Motion-Twin\n"
  ^"Copyright (C)2002 Lexifi\n"
  ^"Last version : http://tech.motion-twin.com\n\n"
  ^"Usage: olegen [args] <class-name>"
in
let arg_spec = [
  ("-o", Arg.String (fun out -> output_file := out), "<file> : set output file");
  ("-obj", Arg.Unit (fun () -> objgen := true), ": generate OO code");
] in
Arg.parse arg_spec (fun arg -> class_name := Some arg) usage;
match !class_name with
  | None -> Arg.usage arg_spec usage
  | Some cname ->
	prerr_endline ("Generating code for "^cname);
	let types = generate cname in
		(match !objgen with
		| false ->
			gen_mli !output_file types;
			gen_ml !output_file types;
			prerr_endline ((string_of_int (List.length types))^" types generated");
		| true ->
			gen_oo_mli !output_file types);
		com_types_done := [];

(* ************************************************************************ *)
