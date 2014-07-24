(* ************************************************************************ *)
(*																			*)
(* OCam'OLE - OLE Automation binding for OCaml								*)
(*																			*)
(* (c)2002 Nicolas Cannasse													*)
(* (c)2002 Motion-Twin														*)
(* (c)2002 Lexifi															*)
(*																			*)
(* ************************************************************************ *)

type guid
type iunknown
type idispatch
type memberid
type dispid
type date = float (* DATE OLE is C-double *)

type itypeinfo
type itypelib


exception Invalid_progid of string
exception Invalid_guid of guid
exception Invalid_enum of string
exception Invoke_exc of (string * string)
exception Com_error of string

external guid_of_progid : string -> guid = "guid_from_progid"
external guid_eq : guid -> guid -> bool = "guid_eq"
external string_of_guid : guid -> string = "string_from_guid"
external guid_of_string : string -> guid = "guid_from_string"

external guid_iunknown : unit -> guid = "guid_iunknown"
external guid_idispatch : unit -> guid = "guid_idispatch" 
external guid_null : unit -> guid = "guid_null"

external _get_null_unknown : unit -> iunknown = "get_null_com"
external _get_null_dispatch : unit -> idispatch = "get_null_com"

external do_not_close : unit -> unit = "com_do_not_close"

(*
   NATIVE FUNCTIONS :
   We can't use C DLL's with native compilation, so
   DllMain is not called properly, we need to init/close
   COM by hand. OCamole is doing that for the user, so don't worry !
*)

external _com_init : unit -> bool = "com_init"
external _com_close : unit -> unit = "com_close"

let _ = _com_init()
let null_unknown = _get_null_unknown()
let null_dispatch = _get_null_dispatch()

type type_kind =
	Tkind_enum
  | Tkind_record
  | Tkind_module
  | Tkind_interface
  | Tkind_dispatch
  | Tkind_coclass
  | Tkind_alias
  | Tkind_union

type type_attr = {
	ta_guid : guid;
	ta_constructor : memberid option;
	ta_destructor: memberid option;
	ta_nfuncs : int;
	ta_nvars : int;
	ta_ninterfaces : int;
	ta_version : (int * int);
	ta_kind : type_kind;
}

type type_desc =
  | VTY_VOID
  | VTY_EMPTY
  | VTY_R4
  | VTY_R8
  | VTY_DATE
  | VTY_BSTR
  | VTY_DISPATCH
  | VTY_BOOL
  | VTY_VARIANT
  | VTY_UNKNOWN
  | VTY_UI2
  | VTY_UI4
  | VTY_I1
  | VTY_I2
  | VTY_I4
  | VTY_INT
  | VTY_UINT
  | VTY_HRESULT
  (* flagged constructors *)
  | VTY_PTR of type_desc
  | VTY_USERDEFINED of itypeinfo
  (* special constructor to handle unimplemented "rare" variant types *)
  | VTY_NOTSUPPORTED

type variant =
	(* constant constructors *)
  | VT_VOID
  | VT_EMPTY
	(* blocks *)
  | VT_R4 of float
  | VT_R8 of float
  | VT_DATE of date
  | VT_BSTR of string 
  | VT_DISPATCH of idispatch
  | VT_BOOL of bool
(*  VT_VARIANT - is telled invalid in the docs *)
  | VT_UNKNOWN of iunknown
  | VT_UI2 of int
  | VT_UI4 of int
  | VT_C_UI4 of int32
  | VT_I1 of int
  | VT_I2 of int
  | VT_I4 of int
  | VT_C_I4 of int32
  | VT_INT of int
  | VT_C_INT of int32
  | VT_UINT of int
  | VT_C_UINT of int32
  | VT_HRESULT of int
  | VT_C_HRESULT of int32
  | VT_CLSID of guid
  | VT_PTR of variant


type func_kind =
	Func_virtual
  | Func_purevirtual
  | Func_nonvirtual
  | Func_static
  | Func_dispatch

type invoke_kind =
	Invoke_func
  | Invoke_propertyget
  | Invoke_propertyput
  | Invoke_propertyputref

type param_flag =
	Paramflag_fin
  | Paramflag_fout
  | Paramflag_flcid
  | Paramflag_fretval
  | Paramflag_fopt
  | Paramflag_foptdefault of variant

type param_desc = param_flag list	

type elem_desc = {
	ed_type : type_desc;
	ed_param : param_desc;
}

type func_desc = {
	fd_memid : memberid;
	fd_fkind : func_kind;
	fd_ikind : invoke_kind;
	fd_nparamsopt : int;
	fd_retval : elem_desc;
	fd_params : elem_desc list;
	fd_hidden : bool;
}

type var_kind =
    Var_perinstance
  | Var_static
  | Var_const of variant 
  | Var_dispatch

type var_desc = {
	vd_memid : memberid;
	vd_desc : elem_desc;
	vd_kind : var_kind;
	vd_hidden : bool;
	vd_readonly : bool;
}

module IDispatch =
  struct

	type t = idispatch

	external get_type_info_count : t -> int = "idisp_get_type_info_count"
	external get_type_info : t -> int -> itypeinfo = "idisp_get_type_info"
	external get_dispid : t -> string -> dispid = "idisp_get_dispid"
	external query_interface : t -> guid -> t = "idisp_query_interface"
	external invoke : t -> dispid -> variant option list -> int list -> invoke_kind -> variant list = "idisp_invoke"

	external get_active_object : guid -> t option = "get_active_object"

  end

module ITypeInfo =
  struct

	type t = itypeinfo

	external get_type_attr : t -> type_attr = "itinf_get_type_attr"
	external get_names : t -> memberid -> string list = "itinf_get_names"
	external get_func_desc : t -> int -> func_desc = "itinf_get_func_desc"
	external get_var_desc : t -> int -> var_desc = "itinf_get_var_desc"
	external get_type_lib : t -> (itypelib * int) = "itinf_get_type_lib"
	external get_interface : t -> int -> itypeinfo = "itinf_get_interface"

  end

module ITypeLib =
  struct
	
	type t = itypelib

	external get_type_info_count : t -> int = "itlib_get_type_info_count"
	external get_type_info : t -> int -> itypeinfo = "itlib_get_type_info"
	external get_type_name : t -> int -> string = "itlib_get_type_name"

  end

external create_idispatch : guid -> idispatch = "create_idispatch"

(* OLEGEN SPECIFIC *)

type caml_dispatch = {
	cdispatch : idispatch;
	cdisptbl : (string,dispid) Hashtbl.t;
}

let _dispid cd name =
	try
		Hashtbl.find cd.cdisptbl name
	with
		Not_found ->
			let did = IDispatch.get_dispid cd.cdispatch name in
			Hashtbl.add cd.cdisptbl name did;
			did

type 'a property = {
	pdispatch  : idispatch;
	pdispid : dispid;
	pbyref : bool;
	pto_variant : 'a -> variant;
	pfrom_variant : variant -> 'a;	
}

external _pget : 'a property -> variant = "property_get"
external _pset : 'a property -> variant -> unit = "property_set"

let pget p = p.pfrom_variant (_pget p)
let pset p v = _pset p (p.pto_variant v)

exception Variant_conversion of string
exception Invoke_return_badtype of string

let variant_error t =
	raise (Variant_conversion t)

let variant_float = function
  | VT_R4 f | VT_R8 f -> f
  | _ -> variant_error "float"

let variant_date = function
  | VT_DATE d -> d
  | _ -> variant_error "date"

let variant_string = function
  | VT_BSTR s -> s
  | _ -> variant_error "string"

let variant_bool = function
  | VT_BOOL b -> b
  | _ -> variant_error "bool"

let variant_int = function
  | VT_UI2 i
  | VT_UI4 i
  | VT_I1 i
  | VT_I2 i
  | VT_I4 i
  | VT_INT i
  | VT_UINT i
  | VT_HRESULT i -> i
  | VT_C_UI4 _
  | VT_C_I4 _
  | VT_C_INT _
  | VT_C_UINT _ 
  | VT_C_HRESULT _ -> variant_error "int is int32"
  | _ -> variant_error "int" 

let variant_idispatch = function
  | VT_DISPATCH d -> d
  | _ -> variant_error "idispatch"

let variant_iunknown = function
  | VT_UNKNOWN u -> u
  | _ -> variant_error "iunknown"

let variant_ptr = function
  | VT_PTR v -> v
  | _ -> variant_error "ptr"

let variant_guid = function
  | VT_CLSID g -> g
  | _ -> variant_error "guid"

let variant_date = function
  | VT_DATE d -> d
  | _ -> variant_error "date"


let invoke_error msg =
	raise (Invoke_return_badtype msg)

;;
Callback.register_exception "Com_error" (Com_error "");
Callback.register_exception "Invalid_progid" (Invalid_progid "");
Callback.register_exception "Invalid_enum" (Invalid_enum "");
Callback.register_exception "Invoke_exc" (Invoke_exc ("",""));
Callback.register_exception "Invalid_guid" (Invalid_guid (guid_null()));

(* Perform full Garbage collection to release COM objects *)
at_exit (fun () -> Gc.full_major(); _com_close(); ); 