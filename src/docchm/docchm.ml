(* ************************************************************************ *)
(*																			*)
(*	DocCHM - CHM Documentation generator for OCamlDoc						*)
(*																			*)
(*	(c)2003 Nicolas Cannasse												*)
(*																			*)
(* ************************************************************************ *)

open Format
open Unix
open Types
open Outcometree

open Odoc_parameter
open Odoc_exception
open Odoc_module
open Odoc_class
open Odoc_value
open Odoc_types
open Odoc_info
open Odoc_type
open Search

type item =
	| DocModule of Odoc_module.t_module
	| DocModuleType of Odoc_module.t_module_type
	| DocClass of Odoc_class.t_class
	| DocClassType of Odoc_class.t_class_type
	| DocValue of Odoc_value.t_value
	| DocType of Odoc_type.t_type
	| DocException of Odoc_exception.t_exception
	| DocAttribute of Odoc_value.t_attribute
	| DocMethod of Odoc_value.t_method
	| DocSection of string * Odoc_types.text
	| DocReference of string * ref_kind option
	| DocNotFound

type t = {
	output_dir : string;
	css : string;
	index : (string , string * int) Hashtbl.t;
	items : (item , int) Hashtbl.t;
	lost_refs : (string, unit) Hashtbl.t;
	modules : Odoc_module.t_module list;
	search_cache : (string * ref_kind option,int) Hashtbl.t;
	mutable tmp_index : int;
	mutable link_not_found : int;
}

(* ************************************************************************ *)
(* TOOLS *)

let not_supported feature =
	prerr_endline ("Not supported by DocChm : "^feature)

let trace log =
	print_endline log;
	Pervasives.flush Pervasives.stdout

let tmp_file i = sprintf "tmp_%d.html" i

let open_tmp t doc =
	let i = t.tmp_index in
	t.tmp_index <- t.tmp_index + 1;
	Hashtbl.add t.items doc i;
	let f = open_out (sprintf "%s/%s"  !Odoc_args.target_dir (tmp_file i)) in
	f,i

let _generate = ref (fun _ _ -> 0)
let generate t item = !_generate t item

let esc_regexp = Str.regexp "[<>\n]"

let keywords = ["let";"and";"as";"assert";"begin";"class";"constraint";"do";
	"done";"downto";"else";"end";"exception";"external";"false";"for";"fun";
	"function";"functor";"if";"in";"include";"inherit";"initializer";"lazy";
	"let";"match";"method";"module";"mutable";"new";"not";"object";"of";"open";
	"or";"private";"rec";"sig";"struct";"then";"to";"true";"try";"type";"val";"virtual";
	"when";"while";"with";"this";"self";"(\\*[^\\*]*\\*)";"\"\\([^\"]\\|\\(\\\\\"\\)\\)*\""]

let keyhash = Hashtbl.create 0

let _ = (List.iter (fun k -> Hashtbl.add keyhash k ()) keywords)

let stdtypes = ["int";"char";"string";"float";"bool";"unit";"exn";"array";"list";"option";"format";"lazy_t"]

let construct_keyword k =
	match k.[0] >= 'a' && k.[0] <= 'z' with
	| true -> "\\(\\b"^k^"\\b\\)"
	| false -> "\\("^k^"\\)"

let code_regexp = Str.regexp (String.concat "\\|" (List.map construct_keyword keywords))

let colorize =
	Str.global_substitute code_regexp
		(fun s -> let m = Str.matched_string s in
			try
				Hashtbl.find keyhash m;
				"<font color='blue'>"^m^"</font>"
			with
				Not_found ->
					match m.[0] with
					| '(' -> "<font color='green'>"^m^"</font>"
					| '"' -> "<font color='darkred'>"^m^"</font>"
					| _ -> assert false)

let escape =
	Str.global_substitute esc_regexp
		(fun s -> match Str.matched_string s with
			| "<" -> "&lt;"
			| ">" -> "&gt;"
			| "\n" -> ""
			| m -> assert false)

let split_path path =
	let rsplit_char str ch =
		let p = String.rindex str ch in
			(String.sub str 0 p,String.sub str (p+1) ((String.length str)-p-1))
	in
	try
		(let m,f = rsplit_char path '.' in
		let c = m.[String.length m - 1] in
		match (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') with
		| true -> m,f
		| false -> (* for Pervasives.(+.) and others *)
			try
				let m, f2 = rsplit_char m '.' in
				m , sprintf "%s.%s" f2 f
			with
				Not_found -> assert false)
	with
		Not_found -> "Pervasives",path

let curt = ref (fun () -> assert false)
let current_module = ref ""

let local_name ident =
	let is_sub_module m cur =
		match m = "Pervasives" || m = cur with
		| true -> true
		| false ->
			try
				String.sub cur 0 (String.length m) = m
			with
			_ -> false
	in
	let m_name, i_name = split_path ident in
	match is_sub_module m_name !current_module with
	| true -> i_name
	| false -> ident


(* ************************************************************************ *)
(* CUSTOM TYPE PRINTING *)


(* the following functions are part of the Ocaml compiler sources, and
   are thus belonging to the following copyright.		   
   - only a minor edition as been done to enable type-link             
   - printing of special chars such as < and > have been replaced by
     their HTML equivalent                                             *)
   
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)


let rec print_ident ppf =
  function
    Oide_ident s -> fprintf ppf "%s" s
  | Oide_dot (id, s) -> fprintf ppf "%a.%s" print_ident id s
  | Oide_apply (id1, id2) -> fprintf ppf "%a(%a)" print_ident id1 print_ident id2

let rec print_list pr sep ppf =
  function
    [] -> ()
  | [a] -> pr ppf a
  | a :: l -> pr ppf a; sep ppf; print_list pr sep ppf l

let pr_present =
  print_list (fun ppf s -> fprintf ppf "`%s" s) (fun ppf -> fprintf ppf "@ ")

let pr_vars =
  print_list (fun ppf s -> fprintf ppf "'%s" s) (fun ppf -> fprintf ppf "@ ")

let rec print_out_type ppf =
  function
  | Otyp_alias (ty, s) ->
      fprintf ppf "@[%a as '%s@]" print_out_type ty s
  | Otyp_poly (sl, ty) ->
      fprintf ppf "@[<hov 2>%a.@ %a@]"
        pr_vars sl
        print_out_type ty
  | ty ->
      print_out_type_1 ppf ty
and print_out_type_1 ppf =
  function
    Otyp_arrow (lab, ty1, ty2) ->
      fprintf ppf "@[%s%a ->@ %a@]" (if lab <> "" then lab ^ ":" else "")
        print_out_type_2 ty1 print_out_type_1 ty2
  | ty -> print_out_type_2 ppf ty
and print_out_type_2 ppf =
  function
    Otyp_tuple tyl ->
      fprintf ppf "@[<0>%a@]" (print_typlist print_simple_out_type " *") tyl
  | ty -> print_simple_out_type ppf ty
and print_simple_out_type ppf =
  function
    Otyp_class (ng, id, tyl) ->
      fprintf ppf "@[%a%s#%a@]" print_typargs tyl (if ng then "_" else "")
        print_ident id
  | Otyp_constr (id, tyl) ->
	  (* Modifications start here *)
	  print_ident str_formatter id;
      let ident = flush_str_formatter() in
	  let link = generate (!curt()) (DocReference (ident,Some RK_type)) in
	  let ident = local_name ident in
	  (match (!curt()).link_not_found = link with
	  | true -> fprintf ppf "@[%a%s@]" print_typargs tyl ident
	  | false -> fprintf ppf "@[%a<a href='%s'>%s</a>@]" print_typargs tyl (tmp_file link) ident);
      (* End - removed one line *)
  | Otyp_object (fields, rest) ->
      fprintf ppf "@[<2>&lt; %a &gt;@]" (print_fields rest) fields
  | Otyp_stuff s -> fprintf ppf "%s" s
  | Otyp_var (ng, s) -> fprintf ppf "'%s%s" (if ng then "_" else "") s
  | Otyp_variant (non_gen, row_fields, closed, tags) ->
      let print_present ppf =
        function
          None | Some [] -> ()
        | Some l -> fprintf ppf "@;<1 -2>&gt; @[<hov>%a@]" pr_present l
      in
      let print_fields ppf =
        function
          Ovar_fields fields ->
            print_list print_row_field (fun ppf -> fprintf ppf "@;<1 -2>| ")
              ppf fields
        | Ovar_name (id, tyl) ->
            fprintf ppf "@[%a%a@]" print_typargs tyl print_ident id
      in
      fprintf ppf "%s[%s@[<hv>@[<hv>%a@]%a]@]" (if non_gen then "_" else "")
        (if closed then if tags = None then " " else "&lt; "
         else if tags = None then "&gt; " else "? ")
        print_fields row_fields
        print_present tags
  | Otyp_alias _ | Otyp_poly _ | Otyp_arrow _ | Otyp_tuple _ as ty ->
      fprintf ppf "@[<1>(%a)@]" print_out_type ty
  | Otyp_abstract | Otyp_sum _ | Otyp_record _ | Otyp_manifest (_, _) -> ()
and print_fields rest ppf =
  function
    [] ->
      begin match rest with
        Some non_gen -> fprintf ppf "%s.." (if non_gen then "_" else "")
      | None -> ()
      end
  | [s, t] ->
      fprintf ppf "%s : %a" s print_out_type t;
      begin match rest with
        Some _ -> fprintf ppf ";@ "
      | None -> ()
      end;
      print_fields rest ppf []
  | (s, t) :: l ->
      fprintf ppf "%s : %a;@ %a" s print_out_type t (print_fields rest) l
and print_row_field ppf (l, opt_amp, tyl) =
  let pr_of ppf =
    if opt_amp then fprintf ppf " of@ &@ "
    else if tyl <> [] then fprintf ppf " of@ "
    else fprintf ppf ""
  in
  fprintf ppf "@[<hv 2>`%s%t%a@]" l pr_of (print_typlist print_out_type " &")
    tyl
and print_typlist print_elem sep ppf =
  function
    [] -> ()
  | [ty] -> print_elem ppf ty
  | ty :: tyl ->
      fprintf ppf "%a%s@ %a" print_elem ty sep (print_typlist print_elem sep)
        tyl
and print_typargs ppf =
  function
    [] -> ()
  | [ty1] -> fprintf ppf "%a@ " print_simple_out_type ty1
  | tyl -> fprintf ppf "@[<1>(%a)@]@ " (print_typlist print_out_type ",") tyl

(* end of INRIA OCaml sources *)

(* ************************************************************************ *)
(* PRINTING *)

let pp_type ?(clear=true) t ppf x =
	curt := (fun () -> t);
	Oprint.out_type := print_out_type; 
	if clear then reset_type_names(); (* reset polymorphic identifier generator *)
	Printtyp.mark_loops x;
	Printtyp.type_expr ppf x

let rec pp_list sep pp ppf = function
	| [] -> ()
	| [e] -> pp ppf e
	| e::es -> fprintf ppf "%a%s%a" pp e sep (pp_list sep pp) es

let rec pp_text t ppf tlist =
	let pp_text_element t ppf =
		function
		| Raw s -> fprintf ppf "%s" (escape s)
		| Code s -> fprintf ppf "<code>%s</code>" (colorize (escape s))
		| CodePre s -> fprintf ppf "<pre>%s</pre>" (colorize (escape s))
		| Verbatim s -> fprintf ppf "<pre>%s</pre>" (escape s)
		| Bold l -> fprintf ppf "<b>%a</b>" (pp_text t) l
		| Italic l -> fprintf ppf "<i>%a</i>" (pp_text t) l
		| Emphasize l -> fprintf ppf "<em>%a</em>" (pp_text t) l
		| Center l -> fprintf ppf "<center>%a</center>" (pp_text t) l
		| Left l -> fprintf ppf "<div align='left'>%a</div>" (pp_text t) l
		| Right l -> fprintf ppf "<div align='right'>%a</div>" (pp_text t) l
		| List l -> fprintf ppf "<ul>%a</ul>" (pp_list " " (fun ppf l2 -> fprintf ppf "<li>%a" (pp_text t) l2)) l
		| Enum l -> fprintf ppf "<ol>%a</ol>" (pp_list " " (fun ppf l2 -> fprintf ppf "<li>%a" (pp_text t) l2)) l
		| Newline -> fprintf ppf "<br>"
		| Block l -> fprintf ppf "<blockquote>%a</blockquote>" (pp_text t) l
		| Title (style,olabel,l) -> fprintf ppf "<h2>%a</h2>" (pp_text t) l
		| Latex s -> not_supported "Latex"
		| Link (url , l) -> fprintf ppf "<a href='%s'>%a</a>" url (pp_text t) l
		| Ref (name, kind) ->
			let n = generate t (DocReference (name,kind)) in
			fprintf ppf "<a href='%s'>%s</a>" (tmp_file n) name
		| Superscript l -> fprintf ppf "<sup>%a</sup>" (pp_text t) l
		| Subscript l -> fprintf ppf "<sub>%a</sub>" (pp_text t) l 
	in
	pp_list " " (pp_text_element t) ppf tlist

let pp_opt pp ppf = function
	| None -> ()
	| Some x -> pp ppf x

let pp_opt_text t fmt ppf = function
	| None -> ()
	| Some x -> fprintf ppf fmt (pp_text t) x

let pp_location ppf l =
	match l.loc_inter, l.loc_impl with
	| Some (file,line), _ -> fprintf ppf "%s" (Filename.basename file)
	| None, Some (file, line) -> fprintf ppf "%s" (Filename.basename file)
	| None, None -> fprintf ppf "<i>Unknwown</i>"

let pp_info t ppf i =
	pp_opt_text t "<i><font color='red'><b>Deprecated :</b></font> %a</i><br><br>@," ppf i.i_deprecated;
	pp_opt_text t "<b>Description :</b> %a<br><br>@," ppf i.i_desc;
	pp_list "" (fun ppf (name,text) -> fprintf ppf "<code><b>%s</b></code> : %a<br><br>" name (pp_text t) text) ppf i.i_params;
	pp_opt_text t "<b>Return :</b> %a<br><br>@," ppf i.i_return_value;
	pp_list "" (fun ppf (ename,text) -> fprintf ppf "<b>Raise :</b> <code>%s</code> %a<br><br>" ename (pp_text t) text) ppf i.i_raised_exceptions;
	if i.i_version <> None then not_supported "@version";
	if i.i_authors <> [] then not_supported "@authors";
	if i.i_sees <> [] then not_supported "@see";
	if i.i_since <> None then not_supported "@since"

let pp_parameters t ppf = function
	| [] -> ()
	| x::[] -> fprintf ppf "%a " (pp_type t) x
	| l ->
		reset_type_names();
		fprintf ppf "(%a) " (pp_list ", " (pp_type ~clear:false t)) l

let pp_class_parameters t ppf = function
	| [] -> ()
	| l ->
		reset_type_names();
		fprintf ppf "[%a] " (pp_list ", " (pp_type ~clear:false t)) l

let pp_record_field t index tyname ppf r =
	let mutword = (match r.rf_mutable with
		| false -> ""
		| true -> "<font color='blue'>mutable</font> ") in
	Hashtbl.add t.index r.rf_name ("type "^tyname,index);
	fprintf ppf "<tr><td valign='top' NOWRAP>&nbsp; %s%s : %a;<td> &nbsp; &nbsp; <td>%a&nbsp;</tr>@," mutword r.rf_name (pp_type t) r.rf_type (pp_opt_text t "<font color='green'>(* %a *)</font>") r.rf_text

let pp_comments t ppf = function
	| None -> fprintf ppf "<td colspan='2'>"
	| Some text -> fprintf ppf "<td> &nbsp; &nbsp; <td><font color='green'>(* %a *)</font>&nbsp;" (pp_text t) text

let pp_constructor t index tyname ppf c =
	Hashtbl.add t.index c.vc_name ("type "^tyname,index);
	match c.vc_args with
	| [] -> fprintf ppf "<tr><td valign='top' NOWRAP>&nbsp; | %s %a</tr>@," c.vc_name (pp_comments t) c.vc_text
	| args -> fprintf ppf "<tr><td valign='top' NOWRAP>&nbsp; | %s <font color='blue'>of</font> %a %a</tr>@," c.vc_name (pp_list " * " (pp_type t)) args (pp_comments t) c.vc_text

let pp_inherit t ppf s =
	let link = generate t (DocReference (s.ic_name,Some RK_class)) in
	fprintf ppf "<tr><td valign='top' NOWRAP>&nbsp; <font color='blue'>inherit</font> <a href='%s'>%s</a>%a</tr>@," (tmp_file link) (local_name s.ic_name) (pp_comments t) s.ic_text

let pp_keywords ppf m =
	match m.met_private, m.met_virtual with
	| true, true -> fprintf ppf "virtual private "
	| true, _ -> fprintf ppf "private "
	| _, true -> fprintf ppf "virtual "
	| _, _ -> ()

let pp_class_element t ppf = function
	| Class_attribute a ->
		let v = a.att_value in
		let _, att_name = split_path v.val_name in
		let link = generate t (DocAttribute a) in
		fprintf ppf "<tr><td valign='top' NOWRAP>&nbsp; <font color='blue'>%s</font><a href='%s'>%s</a> : %a%a</tr>@," (if a.att_mutable then "mutable " else "") (tmp_file link) att_name (pp_type t) v.val_type (pp_comments t) None;
	| Class_method m ->
		let v = m.met_value in
		let _, met_name = split_path v.val_name in
		let link = generate t (DocMethod m) in
		fprintf ppf "<tr><td valign='top' NOWRAP>&nbsp; <font color='blue'>method %a</font><a href='%s'>%s</a> : %a%a</tr>@," pp_keywords m (tmp_file link) met_name (pp_type t) v.val_type (pp_comments t) None;
	| Class_comment c ->
		fprintf ppf "<tr><td colspan='3'><font color='green'>(* %a *)</font></tr>@," (pp_text t) c		

let pp_header t ?(ntype="Module") m_index m_name ppf loc =
	fprintf ppf "@[<v><html>@,@[<v2><head>@,<link rel='stylesheet' href='%s'/>@]@,</head>@,@[<v2><body topmargin='0'>@," t.css;
	fprintf ppf "@[<v2><table class='topbar' cellspacing='0'>@,<tr>@,<td NOWRAP> &nbsp; <i>%s <a href='%s'><b>%s</b></a></i>@,<td align='right'><b>File</b> : %a &nbsp; @,</tr>@]@,</table>@," ntype (tmp_file m_index) m_name pp_location loc;
	fprintf ppf "<br><br><br>@,"

let pp_footer ppf =
	fprintf ppf "<br><br><br><br><br><br><br><br><br>@]@,</body>@]@,</html>@."

let pp_link t ppf item =
	let link = generate t item in
	match item with
	| DocNotFound ->
		fprintf ppf "<font color='red'>Not Found</font><br><br>@,"
	| DocClass c ->
		let _, c_name = split_path c.cl_name in
		fprintf ppf "<code><font color='blue'>%sclass</font> <a href='%s'>%s</a></code><br><br>@," (if c.cl_virtual then "virtual " else "") (tmp_file link) c_name
	| DocClassType c ->
		let _, c_name = split_path c.clt_name in
		fprintf ppf "<code><font color='blue'>%sclass type</font> <a href='%s'>%s</a></code><br><br>@," (if c.clt_virtual then "virtual " else "") (tmp_file link) c_name
	| DocModule m ->
		let _, m_name = split_path m.m_name in
		fprintf ppf "<code><font color='blue'>module</font> <a href='%s'>%s</a></code><br><br>@," (tmp_file link) m_name
	| DocModuleType m ->
		let _, m_name = split_path m.mt_name in
		fprintf ppf "<code><font color='blue'>module type</font> <a href='%s'>%s</a></code><br><br>@," (tmp_file link) m_name
	| DocValue v ->
		let _, v_name = split_path v.val_name in
		fprintf ppf "<code><font color='blue'>val</font> <a href='%s'>%s</a> : %a</code><br><br>@," (tmp_file link) (escape v_name) (pp_type t) v.val_type;
	| DocType ty ->
		let _, t_name = split_path ty.ty_name in
		(match ty.ty_manifest with
		| None -> fprintf ppf "<code><font color='blue'>type</font> <a href='%s'>%s</a></code><br><br>@," (tmp_file link) t_name;
		| Some te -> fprintf ppf "<code><font color='blue'>type</font> <a href='%s'>%s</a> = %a</code><br><br>@," (tmp_file link) t_name (pp_type t) te);
	| DocException e ->
		let _, e_name = split_path e.ex_name in
		fprintf ppf "<code><font color='blue'>exception</font> <a href='%s'>%s</a>" (tmp_file link) e_name;
		(match e.ex_args with
		| [] -> ()
		| args -> fprintf ppf " <font color='blue'>of</font> %a" (pp_list " * " (pp_type t)) args);
		fprintf ppf "</code><br><br>@,";
	(* cannot be found in a module definition *)
	| DocMethod _
	| DocAttribute _
	| DocReference _
	| DocSection _
		->
		assert false

let pp_module_element t ppf = function
	| Element_module m ->
		pp_link t ppf (DocModule m)
	| Element_module_type m ->
		pp_link t ppf (DocModuleType m)
	| Element_included_module m ->
		(match m.im_module with
		| None -> fprintf ppf "<code><font color='blue'>include</font> %s</code><br><br>@," m.im_name
		| Some mdata ->
			let index = generate t (match mdata with Mod m -> DocModule m | Modtype m -> DocModuleType m) in 
			fprintf ppf "<code><font color='blue'>include</font> <a href='%s'>%s</a></code><br><br>@," (tmp_file index) m.im_name);
	| Element_class c ->
		pp_link t ppf (DocClass c)
	| Element_class_type c ->
		pp_link t ppf (DocClassType c)
	| Element_value v ->
		pp_link t ppf (DocValue v)
	| Element_exception e ->
		pp_link t ppf (DocException e)
	| Element_type ty ->
		pp_link t ppf (DocType ty)
	| Element_module_comment text ->
		fprintf ppf "<br>%a<br>@," (pp_text t) text;
		match List.hd (List.rev text) with
		| Title _ -> ()
		| _ -> fprintf ppf "<br>"

let pp_functor_arg t ppf a =
	curt := (fun () -> t);
	Oprint.out_type := print_out_type; 
	reset_type_names();
	fprintf ppf "%s : %a" a.mp_name Printtyp.modtype a.mp_type

let rec pp_mtkind t ppf = function
	| None -> fprintf ppf "<i>Abstract</i>"
	| Some Module_type_struct elts -> fprintf ppf "%a" (pp_list "" (pp_module_element t)) elts
	| Some Module_type_alias a ->
		let link = generate t (DocReference (a.mta_name,Some RK_module_type)) in
		fprintf ppf "<b>Alias :</b> <code><font color='blue'>module type</font> <a href='%s'>%s</a>" (tmp_file link) (local_name a.mta_name)
	| Some Module_type_functor (args,kind) ->
		fprintf ppf "<code><font color='blue'>functor</font> (%a) -> </code><br><br>@,%a" (pp_list ", " (pp_functor_arg t)) args (pp_mtkind t) (Some kind)
	| Some Module_type_with (kind,str) ->
		fprintf ppf "<code>%s</code><br><br>@,%a" (colorize str) (pp_mtkind t) (Some kind)

let rec pp_mkind t ppf = function
	| Module_struct elts -> fprintf ppf "%a" (pp_list "" (pp_module_element t)) elts
	| Module_alias a ->
		let link = generate t (DocReference (a.ma_name,Some RK_module)) in
		fprintf ppf "<b>Alias :</b> <code><font color='blue'>module</font> <a href='%s'>%s</a>" (tmp_file link) (local_name a.ma_name)
	| Module_functor (args,kind) ->
		fprintf ppf "<code><font color='blue'>functor</font> (%a) -> </code><br><br>@,%a" (pp_list ", " (pp_functor_arg t)) args (pp_mkind t) kind
	| Module_with (kind,str) ->
		fprintf ppf "<code>%s</code><br><br>@,%a" (colorize str) (pp_mtkind t) (Some kind)
	| Module_constraint _ -> not_supported "ModuleConstraint"
	| Module_apply _ -> not_supported "ModuleApply"

(* ************************************************************************ *)
(* GENERATION *)

let gen_exception t e =
	let tmp_ch, itmp = open_tmp t (DocException e) in
	let ppf = formatter_of_out_channel tmp_ch in
	let m_name, e_name = split_path e.ex_name in
	current_module := m_name;
	let m_index = generate t (DocReference (m_name,Some RK_module)) in
	pp_header t m_index m_name ppf e.ex_loc;
	fprintf ppf "<font size='+1'><code><font color='blue'>exception</font> %s" e_name;
	(match e.ex_args with
	| [] -> ()
	| args -> fprintf ppf " <font color='blue'>of</font> %a" (pp_list " * " (pp_type t)) args);
	(match e.ex_alias with
	| None -> ()
	| Some a ->
		let link = generate t (DocReference (a.ea_name,Some RK_exception)) in
		fprintf ppf " = <a href='%s'>%s</a>" (tmp_file link) (local_name a.ea_name));
	fprintf ppf "</code></font><br><br>@,";
	pp_opt (pp_info t) ppf e.ex_info;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index e_name ("module "^m_name,itmp);
	itmp

let gen_value t v =
	let tmp_ch, itmp = open_tmp t (DocValue v) in
	let ppf = formatter_of_out_channel tmp_ch in
	let m_name, v_name = split_path v.val_name in
	current_module := m_name;
	let m_index = generate t (DocReference (m_name,Some RK_module)) in
	pp_header t m_index m_name ppf v.val_loc;
	fprintf ppf "<font size='+1'><code><font color='blue'>val</font> %s : %a</code></font><br><br>@," (escape v_name) (pp_type t) v.val_type;
	pp_opt (pp_info t) ppf v.val_info;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index v_name ("module "^m_name,itmp);
	itmp	

let gen_attribute t a =
	let v = a.att_value in
	let tmp_ch, itmp = open_tmp t (DocAttribute a) in
	let ppf = formatter_of_out_channel tmp_ch in
	let fullc_name, att_name = split_path v.val_name in
	let m_name, c_name = split_path fullc_name in
	current_module := m_name;
	let c_index = generate t (DocReference (fullc_name,Some RK_class)) in
	pp_header t ~ntype:"Class" c_index c_name ppf v.val_loc;
	fprintf ppf "<font size='+1'><code><i>(attribute)</i> &nbsp; <font color='blue'>%s</font>%s : %a</code></font><br><br>@," (if a.att_mutable then "mutable " else "") att_name (pp_type t) v.val_type;
	pp_opt (pp_info t) ppf v.val_info;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index att_name ("class "^fullc_name,itmp);
	itmp

let gen_method t m =
	let v = m.met_value in
	let tmp_ch, itmp = open_tmp t (DocMethod m) in
	let ppf = formatter_of_out_channel tmp_ch in
	let fullc_name, met_name = split_path v.val_name in
	let m_name, c_name = split_path fullc_name in
	current_module := m_name;
	let c_index = generate t (DocReference (fullc_name,Some RK_class)) in
	pp_header t ~ntype:"Class" c_index c_name ppf v.val_loc;
	fprintf ppf "<font size='+1'><code><font color='blue'>method %a</font>%s : %a</code></font><br><br>@," pp_keywords m met_name (pp_type t) v.val_type;
	pp_opt (pp_info t) ppf v.val_info;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index met_name ("class "^fullc_name,itmp);
	itmp

let gen_type t ty =
	let tmp_ch, itmp = open_tmp t (DocType ty) in
	let ppf = formatter_of_out_channel tmp_ch in
	let m_name, t_name = split_path ty.ty_name in
	current_module := m_name;
	let m_index = generate t (DocReference (m_name,Some RK_module)) in
	pp_header t m_index m_name ppf ty.ty_loc;
	fprintf ppf "<font size='+1'><code><font color='blue'>type</font> %a%s" (pp_parameters t) ty.ty_parameters t_name;
	(match ty.ty_manifest with
	| Some te -> fprintf ppf " = %a" (pp_type t) te
	| None ->
		match ty.ty_kind with
		| Type_abstract -> ()
		| Type_variant clist -> fprintf ppf " =<br>@,<table border='0' cellspacing='0' cellpadding='0'>%a</table>" (pp_list "" (pp_constructor t itmp ty.ty_name)) clist;
		| Type_record fields -> fprintf ppf " = {<br>@,<table border='0' cellspacing='0' cellpadding='0'>%a</table>}" (pp_list "" (pp_record_field t itmp ty.ty_name)) fields
	);
	fprintf ppf "</code></font><br><br>@,";
	pp_opt (pp_info t) ppf ty.ty_info;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index t_name ("module "^m_name,itmp);
	itmp

let gen_class_type t c =
	let tmp_ch, itmp = open_tmp t (DocClassType c) in
	let ppf = formatter_of_out_channel tmp_ch in
	let m_name, c_name = split_path c.clt_name in
	current_module := m_name;
	let m_index = generate t (DocReference (m_name,Some RK_module)) in
	pp_header t m_index m_name ppf c.clt_loc;
	let virtword = (match c.clt_virtual with
		  | false -> ""
		  | true -> "virtual ") in
	fprintf ppf "<font size='+1'><code><font color='blue'>%sclass type</font> %a%s" virtword (pp_class_parameters t) c.clt_type_parameters c_name;
	(match c.clt_kind with
	| Class_signature (supers,elts) ->
		fprintf ppf " = <font color='blue'>object</font><br>@,<table border='0' cellspacing='0' cellpadding='0'>%a%a</table><font color='blue'>end</font>" (pp_list "" (pp_inherit t)) supers (pp_list "" (pp_class_element t)) elts
	| Class_type a ->
		let link = generate t (DocReference (a.cta_name,Some RK_class_type)) in
		fprintf ppf " =	<a href='%s'>%s</a>" (tmp_file link) (local_name a.cta_name));
	fprintf ppf "</code></font><br><br>@,";
	pp_opt (pp_info t) ppf c.clt_info;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index c_name ("module "^m_name,itmp);
	itmp

let gen_class t c =
	let tmp_ch, itmp = open_tmp t (DocClass c) in
	let ppf = formatter_of_out_channel tmp_ch in
	let m_name, c_name = split_path c.cl_name in
	current_module := m_name;
	let m_index = generate t (DocReference (m_name,Some RK_module)) in
	pp_header t m_index m_name ppf c.cl_loc;
	let virtword = (match c.cl_virtual with
		  | false -> ""
		  | true -> "virtual ") in
	fprintf ppf "<font size='+1'><code><font color='blue'>%sclass</font> %a%s" virtword (pp_class_parameters t) c.cl_type_parameters c_name;
	(match c.cl_kind with
	| Class_structure (supers,elts) ->
		fprintf ppf " = <font color='blue'>object</font><br>@,<table border='0' cellspacing='0' cellpadding='0'>%a%a</table><font color='blue'>end</font>" (pp_list "" (pp_inherit t)) supers (pp_list "" (pp_class_element t)) elts
	| Class_apply a -> not_supported "ClassApply"
	| Class_constr _ -> not_supported "ClassConstr"
	| Class_constraint _ -> not_supported "ClassConstraint");
	fprintf ppf "</code></font><br><br>@,";
	pp_opt (pp_info t) ppf c.cl_info;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index c_name ("module "^m_name,itmp);
	itmp

let gen_module t m =
	trace ("Generating : module "^m.m_name);
	Hashtbl.clear t.search_cache;
	current_module := m.m_name;
	let pm_name, m_name = split_path m.m_name in
	let tmp_ch, itmp = open_tmp t (DocModule m) in
	let plink, pm_name = (match m_name = m.m_name with
		  | true -> itmp,m_name
		  | false -> generate t (DocReference (pm_name,Some RK_module)),pm_name) in
	let ppf = formatter_of_out_channel tmp_ch in
	pp_header t plink pm_name ppf m.m_loc;
	fprintf ppf "<h2>module %s<br></h2>@," m.m_name;
	pp_opt (pp_info t) ppf m.m_info;
	pp_mkind t ppf m.m_kind;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index m.m_name ("module "^m.m_name,itmp);
	if m_name <> m.m_name then Hashtbl.add t.index m_name ("module "^m.m_name,itmp);
	itmp

let gen_module_type t m =
	current_module := m.mt_name;
	let pm_name, m_name = split_path m.mt_name in
	let tmp_ch, itmp = open_tmp t (DocModuleType m) in
	let plink, pm_name = (match m_name = m.mt_name with
		  | true -> itmp,m_name
		  | false -> generate t (DocReference (pm_name,Some RK_module)),pm_name) in
	let ppf = formatter_of_out_channel tmp_ch in
	pp_header t plink pm_name ppf m.mt_loc;
	fprintf ppf "<h2>module %s<br></h2>@," m.mt_name;
	pp_opt (pp_info t) ppf m.mt_info;
	pp_mtkind t ppf m.mt_kind;
	pp_footer ppf;
	close_out tmp_ch;
	Hashtbl.add t.index m.mt_name ("module type "^m.mt_name,itmp);
	if m_name <> m.mt_name then Hashtbl.add t.index m_name ("module type "^m.mt_name,itmp);
	itmp

let gen_reference t ((name,kind) as ref) =
	try
		Hashtbl.find t.search_cache ref
	with
		Not_found ->
			let l = Search.search_by_name t.modules (Str.regexp ("^"^name^"$")) in
			let generate_result = function
				| Res_module m -> generate t (DocModule m)
				| Res_module_type m -> generate t (DocModuleType m)
				| Res_class c -> generate t (DocClass c)
				| Res_class_type c -> generate t (DocClassType c)
				| Res_value v -> generate t (DocValue v)
				| Res_type ty -> generate t (DocType ty)
				| Res_exception e -> generate t (DocException e)
				| Res_attribute a -> generate t (DocAttribute a)
				| Res_method m -> generate t (DocMethod m)
				| Res_section (s,n) -> generate t (DocSection (s,n))
			in
			match kind, l with
			| _ , [] ->
				let m_name, _ = split_path name in
				if not (List.exists ((=) name) stdtypes) && m_name <> "Pervasives" then begin
					try
						Hashtbl.find t.lost_refs name
					with
						Not_found ->
							Hashtbl.add t.lost_refs name ();
							trace ("Reference not found : "^name);
				end;
				generate t DocNotFound
			| None, x::l ->
				let link = generate_result x in
				Hashtbl.add t.search_cache ref link;
				link
			| Some kind, _ ->
				try
					let link = generate_result (List.find (fun r ->
						match r with
						| Res_module _ -> kind = RK_module
						| Res_module_type _ -> kind = RK_module_type || kind = RK_module
						| Res_class _ -> kind = RK_class
						| Res_class_type _ -> kind = RK_class_type || kind = RK_class || kind = RK_type
						| Res_value _ -> kind = RK_value
						| Res_type _ -> kind = RK_type
						| Res_exception _ -> kind = RK_exception
						| Res_attribute _ -> kind = RK_attribute
						| Res_method _ -> kind = RK_method
						| Res_section (_,text) -> kind = RK_section text) l) in
					Hashtbl.add t.search_cache ref link;
					link
				with
					Not_found ->
						trace ("Reference with good type not found : "^name);
						generate t DocNotFound

let gen_notfound t =
	let tmp_ch, itmp = open_tmp t DocNotFound in
	let ppf = formatter_of_out_channel tmp_ch in
	fprintf ppf "@[<v><html>@,@[<h2><head>@,<title>Page Not Found</title>@,<link rel='stylesheet' href='%s'/>@]@,</head>@,@[<v2><body topmargin='0'>@," t.css;
	fprintf ppf "@[<h2><table class='topbar' cellspacing='0'>@,<tr>@,<td><b>Page Not Found</b>@,<td align='right'>@,</tr>@]@,</table>@,";
	fprintf ppf "<br><br>@,";
	fprintf ppf "<h2>Document Not Found<br></h2>@,";
	fprintf ppf "<br>@,";
	fprintf ppf "An error must have occured during documentation generation, see DocCHM output for more informations.";
	pp_footer ppf;
	close_out tmp_ch;
	t.link_not_found <- itmp;
	itmp

let rec generate t item =	
	try
		Hashtbl.find t.items item
	with
		Not_found ->
			match item with
			| DocNotFound -> gen_notfound t
			| DocModule m -> gen_module t m
			| DocModuleType m -> gen_module_type t m
			| DocValue v -> gen_value t v
			| DocType ty -> gen_type t ty
			| DocException e -> gen_exception t e
			| DocClass c -> gen_class t c
			| DocClassType c -> gen_class_type t c
			| DocMethod m -> gen_method t m
			| DocAttribute a -> gen_attribute t a
			| DocReference (name,kind) -> gen_reference t (name,kind)
			| DocSection (name,text) ->
				not_supported "Section";
				generate t DocNotFound

let generate_hook t item =
	let safe = !current_module in
	let result = generate t item in
	current_module := safe;
	result

let _ = (_generate := generate_hook)

let delete_file filename =
	try
		Sys.remove filename
	with
		_ -> ()

let do_generate mlist =
	let output_dir = !Odoc_args.target_dir in
	let default_file = sprintf "%s/default.html" output_dir in
	let index_file = sprintf "%s/index.hhk" output_dir in
	let css_file = "style.css" in
	let t = {
		tmp_index = 0;
		output_dir = output_dir;
		css = "../"^css_file;
		index = Hashtbl.create 0;
		items = Hashtbl.create 0;
		lost_refs = Hashtbl.create 0;
		search_cache = Hashtbl.create 0;
		modules = mlist;
		link_not_found = -1;
	} in
	List.iter (fun m -> ignore (generate t m)) (List.map (fun m -> DocModule m) mlist);
	let f = open_out css_file in
	let ppf = formatter_of_out_channel f in
	fprintf ppf "body { padding: 0px 20px 0px 26px; background: #ffffff; color: #000000; font-family: Verdana, Arial, Helvetica, sans-serif; font-size: 70%%; }@,";
	fprintf ppf "h1, h2, h3, h4 { font-family: Verdana, Arial, Helvetica, sans-serif; margin-left: -26px; }@,";
	fprintf ppf "h1 { font-size: 145%%; margin-top: .5em; margin-bottom: .5em; }@,";
	fprintf ppf "h2 { font-size: 130%%; margin-top: 1em; margin-bottom: .6em; }@,";
	fprintf ppf "h3 { font-size: 115%%; margin-top: 1em; margin-bottom: .6em; }@,";
	fprintf ppf "h4 { font-size: 100%%; margin-top: 1em; margin-bottom: .6em; }@,";
	fprintf ppf "td, code, pre { font-size: 13; }@,";
	fprintf ppf "a:link, a:visited, a:active { text-decoration: none; }@,";
	fprintf ppf "a:link { color: #000077; }@,";
	fprintf ppf "a:visited { color: #000077; }@,";
	fprintf ppf "a:hover { color: #cc9900; }@,";
	fprintf ppf ".topbar { position: absolute; margin: 0; left: 0; top: 0; width: 105%%; height: 21px; cellspacing: 0; }@,";
	fprintf ppf "table.topbar td { font-family: Verdana, sans-serif; font-size: 9pt; background: #99ccff; }@,";
	fprintf ppf "@.";
	close_out f;
	let project_file = "docchm.hhp" in
	let f = open_out project_file in
	let ppf = formatter_of_out_channel f in
	fprintf ppf "[OPTIONS]\nCompatibility=1.1 or later\n";
	fprintf ppf "Compiled file=%s\n" !Odoc_args.out_file;
	fprintf ppf "Default topic=%s\n" default_file;
	fprintf ppf "Index file=%s\n" index_file;
	fprintf ppf "Title=OCaml DocCHM Help\n";
	fprintf ppf "Language=0x409 English\n\n";
	fprintf ppf "[FILES]\n%s\n" css_file;
	for i = 0 to t.tmp_index-1 do
		fprintf ppf "%s/%s\n" output_dir (tmp_file i);
	done;
	fprintf ppf "\n[INFOTYPES]\n";
	fprintf ppf "@.";
	close_out f;
	let f = open_out default_file in
	let ppf = formatter_of_out_channel f in
	fprintf ppf "@[<v><html>@,@[<v2><head>@,<link rel='stylesheet' href='%s'/>@]@,</head>@,@[<v2><body topmargin='0'>@," t.css;
	fprintf ppf "@[<v2><table class='topbar' cellspacing='0'>@,<tr>@,<td NOWRAP> &nbsp; <i>Generated by <a href='http://tech.motion-twin.com/'>DocCHM</a></i>@,<td align='right'>(c)2003 Nicolas Cannasse &nbsp; @,</tr>@]@,</table>@,";
	let pp_module_index ppf m =
		let link = generate t (DocModule m) in
		fprintf ppf "<code><font color='blue'>module</font> <a href='%s'>%s</a></code><br>" (tmp_file link) m.m_name
	in
	fprintf ppf "<br><br><br>This documentation has been generated with OCamlDoc (thanks to Maxence Guesdon), using the DocCHM custom generator. The last version and the full source code of DocCHM are available at <a href='http://tech.motion-twin.com'>http://tech.motion-twin.com</a>. Please go to the website and send an e-mail if you're experiencing any problem with the documentation generated by DocCHM.<br><br>";
	fprintf ppf "<h3>Module Index :</h3><br><font size='70%%'>%a</font>" (pp_list "" pp_module_index) (List.sort (fun m m' -> compare m.m_name m'.m_name) (Search.modules mlist));
	fprintf ppf "<br><br><br>@,";
	pp_footer ppf;
	close_out f;
	let get_keys k _ acc =
		match List.exists (( = ) k) acc with
		| true -> acc
		| false -> k::acc
	in
	let keys = Hashtbl.fold get_keys t.index [] in
	let keys = List.sort (fun x y -> compare (String.lowercase x) (String.lowercase y)) keys in
	let f = open_out index_file in
	let ppf = formatter_of_out_channel f in
	fprintf ppf "<HTML>\n<BODY>\n<UL>\n";
	List.iter (fun k ->
		let refs = Hashtbl.find_all t.index k in
		List.iter (fun (title,nb) ->
			fprintf ppf "\t<LI> <OBJECT type=\"text/sitemap\">\n";
			fprintf ppf "\t\t<param name=\"Keyword\" value=\"%s\">\n" k;
			fprintf ppf "\t\t<param name=\"Name\" value=\"%s\">\n" title;
			fprintf ppf "\t\t<param name=\"Local\" value=\"%s/%s\">\n" output_dir (tmp_file nb);
			fprintf ppf "\t\t</OBJECT>\n";
		) refs;
	) keys;
	fprintf ppf "</UL>\n</BODY>\n</HTML>\n";
	fprintf ppf "@.";
	close_out f;
	trace "Running HHC...";
	let cmd = sprintf "hhc.exe %s" project_file in
	let pout, pin, perr = open_process_full cmd (Unix.environment()) in
	let read ch =
		let rec read_aux acc =
		try
			let l = input_line ch in
			read_aux (l :: acc)
		with
			End_of_file -> acc
		in
		List.rev (read_aux [])
	in
	let data, edata = read pout, read perr in
	match close_process_full (pout, pin, perr) with
	| WEXITED _ ->
		List.iter print_endline data;
		List.iter prerr_endline edata;
		trace "Deleting temp files...";
		for i = 0 to t.tmp_index-1 do
			delete_file (sprintf "%s/%s" !Odoc_args.target_dir (tmp_file i));
		done;
		delete_file default_file;
		delete_file index_file;
		delete_file project_file;
		delete_file css_file;
		trace "Done."
	| _ -> failwith "Build aborted by signal"


(* ************************************************************************ *)
(* OCAMLDOC REGISTER *)

class my_doc_gen : Odoc_args.doc_generator = 
  object
	method generate mlist =
		do_generate mlist
  end

let my_generator = new my_doc_gen
let _ = Odoc_args.set_doc_generator (Some (my_generator :> Odoc_args.doc_generator))



(* ************************************************************************ *)
