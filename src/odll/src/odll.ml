(*
 *  This file is part of ODLL
 *  Copyright (c)2004 Nicolas Cannasse
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Path (* ocaml/src/typing *)
open Types (* ocaml/src/typing *)

open Format
open Unix

exception Invalid_file of string
exception Unsupported
exception UnsupportedType of Path.t
exception VariableNotSupported
exception CompilationFailed of string list

let cmi_magic_number = Config.cmi_magic_number
let cma_magic_number = Config.cma_magic_number
let cmxa_magic_number = Config.cmxa_magic_number
let version = Config.version

type argname = string

(* OCaml types supported by ODLL *)
type stype =
	| Unit
	| Int
	| Float
	| String
	| Char
	| Bool
	| Array of stype
	| Function of (argname * stype) list (* arguments : (label,argtype) *)

type signs = (string * stype) list

let rec pp_list sep pp ppf = function
  | [] -> ()
  | [e] -> pp ppf e
  | e::es -> fprintf ppf "%a%s%a" pp e sep (pp_list sep pp) es

let rec stype_caml ppf =
	let pprint = pp_print_string ppf in
	function
	| Unit -> pprint "unit"
	| Int -> pprint "int"
	| Float -> pprint "float"
	| String -> pprint "string"
	| Char -> pprint "char"
	| Bool -> pprint "bool"
	| Array t -> fprintf ppf "%a array" stype_caml t
	| Function args ->
		let print_arg ppf = function
			| "", (Function _ as x) ->
				fprintf ppf "(%a)" stype_caml x
			| name, (Function _ as x) ->
				fprintf ppf "%s:(%a)" name stype_caml x
			| "", x ->
				fprintf ppf "%a" stype_caml x
			| name, x ->
				fprintf ppf "%s:%a" name stype_caml x
		in
		fprintf ppf "@[%a@]" (pp_list " -> " print_arg) args

let rec stype_c ppf =
	let pprint = pp_print_string ppf in
	function
	| Unit -> pprint "void"
	| Int -> pprint "int"
	| Float -> pprint "double"
	| String -> pprint "char*"
	| Char -> pprint "char"
	| Bool -> pprint "int"
	| Array s -> fprintf ppf "/* %a */ array" stype_caml s;
	| Function _ ->
		(* not supported, filtered by previous tests *)
		assert false 

let rec value_type v =
	let rec convert_type polys = function
		| Pident t as x ->
			(match Ident.name t with
			| "unit" -> Unit
			| "int" -> Int
			| "float" -> Float
			| "string" -> String
			| "char" -> Char
			| "bool" -> Bool
			| "array" ->
				(match polys with
				| [p] -> Array (value_type p)
				| _ -> assert false)
			| _ -> raise (UnsupportedType x))
		| x -> raise (UnsupportedType x)
	in
	match v.desc with 
	| Tarrow (label,texpr,tnext,vcommutable) ->		
		(match value_type tnext with
		| Function l -> Function ((label,(value_type texpr))::l)
		| x -> Function ((label,(value_type texpr))::[("",x)]))
	| Tlink texpr
	| Tsubst texpr ->
		value_type texpr
	| Tconstr (path_t,texpr_list,abbrev_memo_ref) ->
		convert_type texpr_list path_t
	| _	->
		raise Unsupported

let convert_signature s =
	match s with
	| Tsig_value(t,val_desc) ->
		let vt = value_type val_desc.val_type in
		(match vt with
		| Function tlist ->
			let rec find_function = function
				| Function _ -> true
				| Array x -> find_function x
				| _ -> false
			in
			let vlist = snd (List.split tlist) in
			(* does not support closure args or more than 3 args (callbackN) *)
			if List.exists find_function vlist || List.length tlist >= 4 then raise Unsupported;
			vt
		| _ -> raise VariableNotSupported (* does not support non-function types *)
		) 
	| _ -> raise Unsupported	

let convert_signature acc sign =
	let name = (match sign with
		| Tsig_value (t,_)
		| Tsig_type (t,_,_)
		| Tsig_exception (t,_)
		| Tsig_module (t,_,_)
		| Tsig_modtype (t,_)
		| Tsig_class (t,_,_)
		| Tsig_cltype (t,_,_) -> Ident.name t
	) in
	try
		(name, convert_signature sign)::acc
	with
		| VariableNotSupported ->
			prerr_endline ("Warning : Not functional values such as '"^name^"' are not supported - use an accessor");
			acc
		| Unsupported -> 
			prerr_endline ("Warning : Export of '"^name^"' is not supported.");
			acc
		| UnsupportedType t ->
			prerr_endline ("Warning : Type '"^(Path.name t)^"' in '"^name^"' is not supported.");
			acc

let open_and_check file magic =
	let ic = open_in_bin file in
	let buffer = String.create (String.length magic) in
	really_input ic buffer 0 (String.length magic);
	match buffer = magic with
	| false ->
		close_in ic;
		raise (Invalid_file file)
	| true ->
		ic

let process_cmi file =
	let ic = open_and_check file cmi_magic_number in
	let _ , signs = input_value ic in
	close_in ic;
	List.fold_left convert_signature [] signs

(* we need to define two modules because of the record fields
   name clashes between Emitcode.library and Compilenv.library_infos *)
module CMA = struct
	open Emitcode (* ocaml/src/bytecomp *)

	let list_modules file =
		let ic = open_and_check file cma_magic_number in
		let toc_pos = input_binary_int ic in
		seek_in ic toc_pos;
		let lib = (input_value ic : library) in
		close_in ic;
		List.map (fun cu -> cu.cu_name) lib.lib_units
end

module CMXA = struct
	open Compilenv (* ocaml/src/asmcomp *)

	let list_modules file =
		let ic = open_and_check file cmxa_magic_number in
		let infos = (input_value ic : library_infos) in
		close_in ic;
		List.map (fun (u,_) -> u.ui_name) infos.lib_units
end

let compile cmd verbose =
	let rec read_all f =
		try
			let line = input_line f in
			line::(read_all f)
		with
			End_of_file -> []
	in
	let pout,pin,perr = open_process_full cmd (environment()) in
	let errors = (read_all perr)@(read_all pout) in
	match close_process_full (pout,pin,perr) with
	| WEXITED 0 -> if verbose then List.iter prerr_endline errors		
	| WEXITED exitcode -> raise (CompilationFailed errors)
	| _ -> failwith "Build aborted by signal"

let pp_ocaml ~modules signs ppf =
	fprintf ppf "@[<v>(* Generated by ODLL *)@,";
	pp_list "" (fun ppf m -> fprintf ppf "open %s@," m) ppf modules;
	pp_list "" (fun ppf (s,_) -> fprintf ppf "let _ = Callback.register \"%s\" %s@," s s) ppf signs;
	fprintf ppf "@]@."

let pp_c_stubs signs ~output ppf =
	let callback_number ppf = function
		| 0 -> assert false (* does not exists in OCaml - at least one Unit arg *)
		| 1 -> ()
		| 2 -> fprintf ppf "2"
		| 3 -> fprintf ppf "3"
		| _ -> assert false (* unsupported - has been filtered by previous tests *)
	in
	let rec pp_idents default_id = function
		| [] -> []
		| ("",x)::l ->			
			let id = String.make 1 (char_of_int default_id) in
			(id , x)::(pp_idents (default_id+1) l)
		| x::l ->
			x::(pp_idents default_id l)
	in
	let array_count = ref 0 in
	(* c_to_caml_convert can't be considered as a formatter since it's doing
	   side effects on current function formatter (needed for recursive arrays support ) *)
	let rec c_to_caml_convert ?(bcl=0) (id,t) =
		match t with
		| Unit -> (fun lppf () -> fprintf lppf "Val_unit")
		| Int -> (fun lppf () -> fprintf lppf "Val_int(%s)" id)
		| Float -> (fun lppf () -> fprintf lppf "copy_double(%s)" id)
		| String -> (fun lppf () -> fprintf lppf "copy_string(%s)" id)
		| Char -> (fun lppf () -> fprintf lppf "Val_int((unsigned char)%s)" id)
		| Bool -> (fun lppf () -> fprintf lppf "Val_bool(%s)" id)
		| Array Float ->
			let v = char_of_int (bcl+105) in
			let n = !array_count in
			incr array_count;
			fprintf ppf "value a%d = alloc((%s.size << 1),Double_array_tag);@," n id;
			fprintf ppf "@[<v2>{@,";
			fprintf ppf "int %c;@,@[<v2>for(%c=0;%c<%s.size;%c++)@," v v v id v;
			fprintf ppf "Store_double_field(a%d,%c,((double*)%s.data)[%c]);@]@]@," n v id v;
			fprintf ppf "}@,";
			(fun lppf () -> fprintf lppf "a%d" n);
		| Array x ->
			let v = char_of_int (bcl+105) in
			let n = !array_count in
			incr array_count;
		
			let access_val = "v"^(string_of_int n) in

			fprintf ppf "value a%d = alloc(%s.size,0);@," n id;
			fprintf ppf "@[<v2>{@,";
			fprintf ppf "struct caml__roots_block *caml__frame%d = local_roots;@," n;
			fprintf ppf "struct caml__roots_block caml__roots_%d;@," n;
			fprintf ppf "int %c;@," v;
			fprintf ppf "caml__roots_%d.next = local_roots;@," n;
			fprintf ppf "local_roots = &caml__roots_%d;@," n;
			fprintf ppf "caml__roots_%d.nitems = 1;@," n;
			fprintf ppf "caml__roots_%d.ntables = 1;@," n;
			fprintf ppf "caml__roots_%d.tables[0] = &a%d;@," n n;
			fprintf ppf "@[<v2>for(%c=0;%c<%s.size;%c++) {@," v v id v;
			fprintf ppf "%a %s = ((%a*)%s.data)[%c];@," stype_c x access_val stype_c x id v;
			fprintf ppf "Store_field(a%d,%c,(value)%a);@]@," n v (c_to_caml_convert ~bcl:(bcl+1) (access_val,x)) ();
			fprintf ppf "}@,";
			fprintf ppf "local_roots = caml__frame%d;@]@," n;
			fprintf ppf "}@,";
			(fun lppf () -> fprintf lppf "a%d" n);
		| Function _ -> assert false
	in
	let rec caml_to_c_convert ?(bcl=0) iin iout ppf = function
		| Unit -> fprintf ppf "@]"
		| Int
		| Char -> fprintf ppf "%a = Int_val(%a);@]" iout () iin ()
		| Float -> fprintf ppf "%a = Double_val(%a);@]" iout () iin ()
		| String -> fprintf ppf "%a = strdup(String_val(%a));@]" iout () iin ()
		| Bool -> fprintf ppf "%a = Bool_val(%a);@]" iout () iin ()
		| Array Float ->
			let v = char_of_int (bcl+105) in
			fprintf ppf "%a.size = Wosize_val(%a) >> 1;@," iout () iin ();
			fprintf ppf "%a.data = malloc(sizeof(double)*%a.size);@," iout () iout ();
			fprintf ppf "@[<v2>{@,";
			fprintf ppf "int %c;@,@[<v2>for(%c=0;%c<%a.size;%c++)@," v v v iout () v;
			fprintf ppf "((double*)%a.data)[%c] = Double_field(%a,%c);@]@]@,}@]" iout () v iin () v;
		| Array x ->
			let v = char_of_int (bcl+105) in
			let n = !array_count in
			incr array_count;
			fprintf ppf "%a.size = Wosize_val(%a);@," iout () iin ();
			fprintf ppf "%a.data = malloc(sizeof(%a)*%a.size);@," iout () stype_c x iout ();
			fprintf ppf "@[<v2>{@,";
			fprintf ppf "int %c;@,@[<v2>for(%c=0;%c<%a.size;%c++) {@," v v v iout () v;			
			let ain = (fun ppf () -> fprintf ppf "Field(%a,%c)" iin () v) in
			let aout = (fun ppf () -> fprintf ppf "((%a*)%a.data)[%c]" stype_c x iout () v) in
			caml_to_c_convert ~bcl:(bcl+1) ain aout ppf x;
			fprintf ppf "@,}@]@,}@]";
		| Function _ -> assert false
	in
	fprintf ppf "@[<v>/* ******* Generated by ODLL ******* */@,";
	fprintf ppf "#include <caml/mlvalues.h>@,";
	fprintf ppf "#include <caml/memory.h>@,";
	fprintf ppf "#include <caml/callback.h>@,";
	fprintf ppf "#include <caml/alloc.h>@,";
	fprintf ppf "#include <wtypes.h>@,";
	fprintf ppf "#include <winbase.h>@,";
	fprintf ppf "#define Val_unit Val_int(0)@,";
	fprintf ppf "#define Double_array_tag 254@,";
	fprintf ppf "@,";
	fprintf ppf "@[<v2>typedef struct {@,";
	fprintf ppf "void *data;@,";
	fprintf ppf "int size;@]@,";
	fprintf ppf "} array;@,";
	fprintf ppf "@,";
	let pp_stub ppf (sname,s) =
		match s with
		| Function args ->
			let retval = snd (List.hd (List.rev args)) in
			let args = List.rev (List.tl (List.rev args)) in
			let args = pp_idents (int_of_char 'a') args in
			fprintf ppf "@[<v2>__declspec(dllexport) %a %s(" stype_c retval sname;
			let print_arg ppf = function
				| (_,Unit) -> fprintf ppf "void"
				| (id,t) -> fprintf ppf "%a %s" stype_c t id
			in
			pp_list ", " print_arg ppf args;
			fprintf ppf ") {@,";
			fprintf ppf "value _r;@,";
			if retval <> Unit then fprintf ppf "%a _rc;@," stype_c retval;
			let argscnv = List.map c_to_caml_convert args in
			fprintf ppf "@[<v>_r = callback%a" callback_number (List.length args);
			fprintf ppf "(*_%s, %a);@," sname (pp_list "," (fun ppf f -> f ppf ())) argscnv;
			caml_to_c_convert (fun ppf () -> fprintf ppf "_r") (fun ppf () -> fprintf ppf "_rc") ppf retval;
			if retval <> Unit then fprintf ppf "@,return _rc;";
			fprintf ppf "@]@,}@,@,@,";
		| _ -> assert false
	in
	pp_list "" (fun ppf (sname,_) -> fprintf ppf "static value *_%s = NULL;@," sname) ppf signs;
	fprintf ppf "@,";
	pp_list "" pp_stub ppf signs;
	fprintf ppf "@[<v2>__declspec(dllexport) void caml_free(void *m) {@,";
	fprintf ppf "free(m);@]@,";
	fprintf ppf "}@,@,@,";	
	fprintf ppf "@[<v2>BOOL APIENTRY DllMain(HANDLE module, DWORD reason, void *reserved) {@,";
	fprintf ppf "char * argv[2];@,";
	fprintf ppf "switch (reason) {@,";
	fprintf ppf "@[<v2>case DLL_PROCESS_ATTACH:@,";
    fprintf ppf "argv[0] = \"%s\";@," output;
    fprintf ppf "argv[1] = NULL;@,";
	(* check if caml_startup have raised an exception (?) *)
    fprintf ppf "caml_startup(argv);@,";
	let dlink_primitive ppf (sname,_) =
		fprintf ppf "_%s = caml_named_value(\"%s\");@," sname sname;
	in
	pp_list "" dlink_primitive ppf signs;
	fprintf ppf "break;@]@,";
	fprintf ppf "}@,";
	fprintf ppf "return TRUE;@]@,";
	fprintf ppf "}@,@]@."

let pp_header signs ppf =
	fprintf ppf "@[<v>/* Generated by ODLL */@,";
	fprintf ppf "#ifdef _MSC_VER@,";
	fprintf ppf "#  define caml_import __declspec(dllimport) extern@,";
	fprintf ppf "#else@,";
	fprintf ppf "#  define caml_import extern@,";
	fprintf ppf "#endif@,";
	fprintf ppf "@,";
	fprintf ppf "@[<v2>typedef struct {@,";
	fprintf ppf "void *data;@,";
	fprintf ppf "int size;@]@,";
	fprintf ppf "} array;@,";
	fprintf ppf "@,";
	fprintf ppf "#define afloat(a,n) (((double *)(a).data)[(n)])@,";
	fprintf ppf "#define aint(a,n) (((int *)(a).data)[(n)])@,";
	fprintf ppf "#define aarray(a,n) (((array *)(a).data)[(n)])@,";
	fprintf ppf "#define astring(a,n) (((char **)(a).data)[(n)])@,";
	fprintf ppf "@,";
	fprintf ppf "caml_import void caml_free( void * );@,";
	fprintf ppf "@,";
	fprintf ppf "@[<v2>void afree( array *a, int level ) {@,";
	fprintf ppf "int i;@,";
	fprintf ppf "@[<v2>if( level > 1 ) {@,";
	fprintf ppf "@[<v2>for(i=0;i<a->size;i++)@,";
	fprintf ppf "afree(((array*)a->data)+i,level-1);@]@]@,";
	fprintf ppf "}@,";
	fprintf ppf "caml_free(a->data);@]@,";
	fprintf ppf "}@,";
	fprintf ppf "@,";
	let print_arg ppf = function
		| ("",t) -> stype_c ppf t
		| (id,t) -> fprintf ppf "%a %s" stype_c t id
	in
	let pp_c_header ppf (sname,s) =
		match s with
		| Function args ->
			let retval = List.hd (List.rev args) in
			let args = List.rev (List.tl (List.rev args)) in
			fprintf ppf "caml_import %a %s(%a);@," stype_c (snd retval) sname (pp_list ", " print_arg) args;
		| _ -> assert false (* previously filtered *)
	in
	pp_list "" pp_c_header ppf signs;
	fprintf ppf "@]@."

let delete_file file =
	try Sys.remove file with Sys_error _ -> ()

let build_dll lib cmis output isopt verbose keep header =
	let log str =
		print_endline str;
		Pervasives.flush Pervasives.stdout		
	in
	log "Listing exports...";
	let signs = List.concat (List.map process_cmi cmis) in
	let print_sign (name,s) =
		fprintf str_formatter "  val %s : %a" name stype_caml s;
		log (flush_str_formatter());
	in
	List.iter print_sign signs;
	log "Listing modules...";	
	let modules = (if isopt then CMXA.list_modules else CMA.list_modules) lib in
	List.iter (fun m -> log (sprintf "  %s" m)) modules;
	log "Generating Ocaml code...";
	let ocaml = open_out "_odll_regs.ml" in
	pp_ocaml modules signs (formatter_of_out_channel ocaml);
	close_out ocaml;
	log "Compiling Ocaml code...";
	let compiler = (if isopt then "ocamlopt" else "ocamlc") in
	let cmd = sprintf "%s -output-obj -o _odll_code.obj %s _odll_regs.ml" compiler lib in
	if verbose then log cmd;
	compile cmd verbose;
	if not keep then delete_file "_odll_regs.ml";
	delete_file "_odll_regs.cmi";
	(match isopt with
	| true ->
		delete_file "_odll_regs.cmx";
		delete_file "_odll_regs.obj";
	| false ->
		delete_file "_odll_regs.cmo");
	log "Generating C code...";
	let cfile = open_out "_odll_stubs.c" in
	pp_c_stubs signs output (formatter_of_out_channel cfile);
	close_out cfile;
	log "Compiling C code...";
	let cmd = "ocamlc -c _odll_stubs.c" in
	if verbose then log cmd;
	compile cmd verbose;
	if not keep then delete_file "_odll_stubs.c";
	log "Linking...";
	let exports = String.concat " " (List.map (fun (name,_) -> "/export:"^name) signs) in
	let runlib = (if isopt then "libasmrun" else "libcamlrun") in
	let cmd = sprintf "link /nologo /dll /out:%s.dll /export:caml_free %s _odll_stubs.obj _odll_code.obj %s.lib" output exports runlib in
	if verbose then log cmd;
	compile cmd verbose;
	delete_file (sprintf "%s.exp" output);
	delete_file "_odll_stubs.obj";
	delete_file "_odll_code.obj";
	if header then begin
		log "Generating Header file...";
		let hfile = open_out (output ^ ".h") in
		pp_header signs (formatter_of_out_channel hfile);
		close_out hfile;
	end


;;
try
	let usage =
		"ODLL v1.0 - Copyright (C)2003 Motion-Twin and Lexifi\n"
		^"Last version : http://tech.motion-twin.com\n\n"
		^"  Usage : odll [options] <file.cma/cmxa> <file(s).cmi>" in
	let output = ref None in
	let verbose = ref false in
	let keep = ref false in
	let header = ref false in
	let lib = ref None in
	let isopt = ref false in
	let cmis = ref [] in
	let arg_spec = [
	  ("-o", Arg.String (fun f -> output := Some f), "<file> : set output (without extension)");
	  ("-v", Arg.Unit (fun () -> verbose := true), ": turn on verbose mode");
	  ("-header", Arg.Unit (fun () -> header := true), ": generate also the .H header file");
	  ("-keep", Arg.Unit (fun () -> keep := true), ": keep intermediate generated files");
	] in
	let extension file =
		let rsplit_char str ch =
			let p = String.rindex str ch in
			let len = String.length str in
			(String.sub str 0 p, String.sub str (p + 1) (len - p - 1))	
		in
		let s = try snd(rsplit_char file '.') with Not_found -> "" in
		String.uppercase s
	in
	let get_files f =
		match extension f, !lib with
		| "CMA", Some _ -> failwith "You must specify only one CMA or CMXA"
		| "CMI", _ -> cmis := f :: !cmis
		| ("CMA" as e), None
		| ("CMXA" as e), None ->
			lib := Some f;
			isopt := (e = "CMXA");
			(match !output with
			| Some _ -> ()
			| None -> output := Some (Filename.chop_extension f))
		| _ -> failwith ("Don't know what to do with file " ^ f)
	in
	Arg.parse arg_spec get_files usage;
	match !lib,!cmis with
	| None,_ -> failwith "You must specify a CMA or CMXA to build a DLL"
	| _,[] -> failwith "You must specify one or more CMI files containing functions to export"
	| Some lib, cmilist ->
		match !output with
		| None -> assert false
		| Some output ->
			build_dll lib (List.rev cmilist) output !isopt !verbose !keep !header
with
	| Failure error
	| Sys_error error ->
		prerr_endline error;
	| CompilationFailed errors ->
		prerr_endline "Compilation failed !";
		List.iter prerr_endline errors
	| Invalid_file filename ->
		prerr_endline ("Invalid File format : "^filename^" (ODLL is using ocamlc "^version^")")