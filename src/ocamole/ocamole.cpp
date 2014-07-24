/* ************************************************************************ */
/*																			*/
/* OCam'OLE - OLE Automation binding for OCaml								*/
/*																			*/
/* (c)2002 Nicolas Cannasse													*/
/* (c)2002 Motion-Twin														*/
/* (c)2002 Lexifi															*/
/*																			*/
/* ************************************************************************ */
/*

  TODO:
  - add extra variant support
  - check proper alloc/cleanup of bstr & variants

*/
// HEADERS
#include <windows.h>
#include <objbase.h>
#include <oleauto.h>
#include <stdlib.h>

#ifdef _DEBUG
#	define DEBUGINFOS(x) printf(#x" (%d)\n",__LINE__)
#	define DEBUGMETHOD(i,x) printf("0x%.8X : "#x" (%d)\n",i,__LINE__)
#else
#	define DEBUGINFOS(x)
#	define DEBUGMETHOD(i,x)
#endif

#include <stdio.h>
#if 0
#	include <crtdbg.h>
#	define new new(_NORMAL_BLOCK, __FILE__, __LINE__)
#endif

#pragma warning( disable : 4146 )

#define LCID LANG_NEUTRAL

extern "C" {

#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/fail.h>

/* ************************************************************************ */
// VALUE TYPES ( for clear C calls conventions & access )

const GUID IID_INull = { 0,0,0, { 0,0,0,0,0,0,0,0 } };
bool uninit = true;

#ifdef _DEBUG
char *current_call = NULL;
#endif

#define Val_none (Val_int(0))

#define IUnknown_val(v)	(*(IUnknown**)Data_custom_val(v))
#define IDispatch_val(v) ((IDispatch*)IUnknown_val(v))
#define ITypeInfo_val(v) ((ITypeInfo*)IUnknown_val(Field(v,0)))
#define ITypeLib_val(v) ((ITypeLib*)IUnknown_val(Field(v,0)))
#define GUID_val(v) (*(GUID*)(v))
#define MEMBERID_val(v) Int32_val(v)
#define DISPID_val(v) Int32_val(v)

/* ************************************************************************ */
// ENUMS

#define ENUM_DEFINE(x)	\
	int x##_size = sizeof(x) / sizeof(DWORD);\
	const char *x##_name = #x;

const DWORD type_kind[] = {
	  TKIND_ENUM
	, TKIND_RECORD
	, TKIND_MODULE
	, TKIND_INTERFACE
	, TKIND_DISPATCH
	, TKIND_COCLASS
	, TKIND_ALIAS
	, TKIND_UNION
};

ENUM_DEFINE(type_kind);

const DWORD func_kind[] = {
	FUNC_VIRTUAL,
	FUNC_PUREVIRTUAL,
	FUNC_NONVIRTUAL,
	FUNC_STATIC,
	FUNC_DISPATCH
};

ENUM_DEFINE(func_kind);

const DWORD invoke_kind[] = {
	INVOKE_FUNC,
	INVOKE_PROPERTYGET,
	INVOKE_PROPERTYPUT,
	INVOKE_PROPERTYPUTREF
};

ENUM_DEFINE(invoke_kind);

const WORD call_type[] = {
	DISPATCH_METHOD,
	DISPATCH_PROPERTYGET,
	DISPATCH_PROPERTYPUT,
	DISPATCH_PROPERTYPUTREF
};

const DWORD var_type_param[] = {
	VT_PTR,
	VT_USERDEFINED,
};

ENUM_DEFINE(var_type_param);

const DWORD param_flag[] = {
	PARAMFLAG_FIN,
	PARAMFLAG_FOUT,
	PARAMFLAG_FLCID,
	PARAMFLAG_FRETVAL,
	PARAMFLAG_FOPT
	//PARAMFLAG_FHASDEFAULT - special case
};

ENUM_DEFINE(param_flag);

/* ************************************************************************ */
// TOOLS FUNS

BSTR string2wide(const char * s) {
  int len = strlen( s );
  wchar_t * result = new wchar_t[len+1];
  mbstowcs( result, s, len );
  result[len] = 0;
  return result;
}
 
char * wide2string(BSTR s)  {
  int len = wcslen( s );
  char * result = new char[len+1];
  wcstombs( result, s, len);
  result[len] = 0;
  return result;
}

/* ************************************************************************ */
// EXCEPTIONS

void raise_invalid_progid( char *progid ) {
    static value *e = caml_named_value("Invalid_progid");
	raise_with_string(*e, progid);
}

void raise_invalid_guid( value g ) {
	static value *e = caml_named_value("Invalid_guid");
	raise_with_arg(*e,g);
}

void raise_invalid_enum( const char *enum_name ) {
	static value *e = caml_named_value("Invalid_enum");
	raise_with_string(*e,(char*)enum_name);
}

void raise_dispatch_exc( EXCEPINFO *exc ) {
	static value *e = caml_named_value("Invoke_exc");
	CAMLparam0();
	CAMLlocal3(vsrc,vmsg,eval);
	char *source = wide2string(exc->bstrSource);
	char *msg = exc->bstrDescription?wide2string(exc->bstrDescription):NULL;
	vsrc = copy_string(source?source:"Unknown message");
	vmsg = copy_string(msg?msg:"");
	eval = alloc_small(2,0);
	Field(eval,0) = vsrc;
	Field(eval,1) = vmsg;
	delete source;
	delete msg;
	/** free exc ??? **/
	raise_with_arg(*e,eval);
}

void raise_com_error( const char *func ) {
	static value *e = caml_named_value("Com_error");
	raise_with_string(*e,(char*)func);
}

/* ************************************************************************ */
// ALLOC / FREE

void free_bstr( BSTR str ) {
	delete str;
}

extern const DWORD var_type[];
extern int var_type_size;

value alloc_guid( const GUID *guid ) {
	CAMLparam0();
	CAMLlocal1(v);	
	v = alloc_small(sizeof(GUID) / sizeof(value),Abstract_tag);
	*(GUID*)v = *guid;
	CAMLreturn(v);
}

static void
com_finalize( value v )
{
	IUnknown *i = IUnknown_val(v);
	DEBUGMETHOD(i,com_finalize);
	if( i )
		i->Release();
}

static int
com_compare( value v1, value v2 ) {
	return (int)Data_custom_val(v2)-(int)Data_custom_val(v1);
}

static struct custom_operations com_ops = {
	"COM",
	com_finalize,
	com_compare,
	custom_hash_default,
	custom_serialize_default,
	custom_deserialize_default
};

value alloc_com( IUnknown *iunk ) {
	CAMLparam0();
	CAMLlocal1(v);	
	v = alloc_custom(&com_ops,4,1,1);
	IUnknown_val(v) = iunk;
	CAMLreturn(v);
}

value alloc_com_ref( IUnknown *iunk, value vref ) {
	CAMLparam1(vref);
	CAMLlocal2(v,vcom);
	vcom = alloc_com(iunk);
	v = alloc_small(2, 0);
	Field(v,1) = vref;
	Field(v,0) = vcom;
	CAMLreturn(v);
}

value alloc_some(value item) {
    CAMLparam1(item);
    CAMLlocal1(v);
    v = alloc_small(1, 0);
    Field(v,0) = item;
    CAMLreturn(v);
}

value alloc_cons( value item, value queue ) {
	CAMLparam2(item,queue);
	CAMLlocal1(v);
	v = alloc_small(2, 0);
	Field(v,0) = item;
	Field(v,1) = queue;
	CAMLreturn(v);
}

value alloc_memberid( MEMBERID mid ) {
	return copy_int32(mid);
}

value alloc_memberid_option( MEMBERID mid ) {
	CAMLparam1(mid);
	CAMLlocal1(v);
	if( mid == MEMBERID_NIL)
		v = Val_none;
	else
		v = alloc_some(alloc_memberid(mid));
	CAMLreturn(v);	
}

#define alloc_enum(id,e) _alloc_enum(id,e,e##_size,e##_name)
#define alloc_enum_safe(id,e) _alloc_enum_safe(id,e,e##_size)
#define enum_val(id,e) _enum_val(id,e,e##_size,e##_name)

value _alloc_enum( DWORD id, const DWORD *enum_values, int enum_size, const char *enum_name ) {	
	int i;
	for(i=0;i<enum_size;i++)
		if( enum_values[i] == id )
			return Val_int(i);
	raise_invalid_enum(enum_name);
	return -1;
}

value _alloc_enum_safe( DWORD id, const DWORD *enum_values, int enum_size ) {
	int i;
	for(i=0;i<enum_size;i++)
		if( enum_values[i] == id )
			return Val_int(i);
	// do not raise an exception
	return -1;
}

DWORD _enum_val( value v, const DWORD *enum_values, int enum_size, const char *enum_name ) {
	int i = Int_val(v);
	if( i >= enum_size )
		raise_invalid_enum(enum_name);
	return enum_values[i];
}

value alloc_date( DATE d ) {
	return copy_double(d);
}

DATE date_val( value v ) {	
	return Double_val(v);
}

value alloc_type_desc( ITypeInfo *tinf, TYPEDESC *desc ) {
	CAMLparam0();
	CAMLlocal2(v,tmp);
		
	switch(desc->vt) {
	case VT_PTR:
		tmp = alloc_type_desc(tinf,desc->lptdesc);
		v = alloc_small(1,Int_val(alloc_enum(desc->vt,var_type_param)));
		Field(v,0) = tmp;
		break;
	case VT_USERDEFINED:
		{
			ITypeInfo *reftype;
			if( tinf->GetRefTypeInfo(desc->hreftype,&reftype) != S_OK )
				raise_com_error("ITypeInfo.GetRefTypeInfo");			
			tmp = alloc_com_ref(reftype,Val_int(0));
			v = alloc_small(1,Int_val(alloc_enum(desc->vt,var_type_param)));
			Field(v,0) = tmp; 
		}
		break;
	default:
		v = alloc_enum_safe(desc->vt,var_type);
		if( v == -1 ) // not in enum, so not supported !
			v = Val_int(var_type_size);
		break;
	}
	CAMLreturn(v);
}

value alloc_variant( VARIANT *v );

value alloc_param_desc( PARAMDESC *desc ) {
	CAMLparam0();
	CAMLlocal3(v,tmp,var);
	v = Val_int(0); // []
	if( desc->wParamFlags & PARAMFLAG_FIN )
		v = alloc_cons(alloc_enum(PARAMFLAG_FIN,param_flag),v);
	if( desc->wParamFlags & PARAMFLAG_FOUT )
		v = alloc_cons(alloc_enum(PARAMFLAG_FOUT,param_flag),v);
	if( desc->wParamFlags & PARAMFLAG_FLCID )
		v = alloc_cons(alloc_enum(PARAMFLAG_FLCID,param_flag),v);
	if( desc->wParamFlags & PARAMFLAG_FRETVAL )
		v = alloc_cons(alloc_enum(PARAMFLAG_FRETVAL,param_flag),v);
	if( (desc->wParamFlags & PARAMFLAG_FHASDEFAULT) && (desc->wParamFlags & PARAMFLAG_FOPT) ) {
		var = alloc_variant((VARIANT*)desc->pparamdescex);
		tmp = alloc_small(1,0);
		Field(tmp,0) = var;
		v = alloc_cons(tmp,v);
	}
	else if( desc->wParamFlags & PARAMFLAG_FOPT )
		v = alloc_cons(alloc_enum(PARAMFLAG_FOPT,param_flag),v);
	CAMLreturn(v);
}

value alloc_elem_desc( ITypeInfo *tinf, ELEMDESC *desc ) {
	CAMLparam0();
	CAMLlocal3(v,tdesc,pdesc);
	tdesc = alloc_type_desc(tinf,&desc->tdesc);
	pdesc = alloc_param_desc(&desc->paramdesc);
	v = alloc_small(2,0);
	Field(v,0) = tdesc;
	Field(v,1) = pdesc;
	CAMLreturn(v);
}

value alloc_var_kind( VARKIND vk, VARIANT *vptr ) {
	CAMLparam0();
	CAMLlocal2(v,tmp);
	switch( vk ) {
	case VAR_PERINSTANCE:
		v = Val_int(0);
		break;
	case VAR_STATIC:
		v = Val_int(1);
		break;
	case VAR_CONST:
		tmp = alloc_variant(vptr);
		v = alloc_small(1,0);
		Field(v,0) = tmp;
		break;
	case VAR_DISPATCH:
		v = Val_int(2);
		break;
	default:
		raise_invalid_enum("var_kind");
		break;
	}
	CAMLreturn(v);
}	 

#define alloc_dispid copy_int32


/* ************************************************************************ */
// VARIANT CONVERSIONS

/**
enum VARENUM
    {
	VT_EMPTY= 0,
	VT_NULL	= 1,
	VT_I2	= 2,
	VT_I4	= 3,
	VT_R4	= 4,
	VT_R8	= 5,
	VT_CY	= 6,
	VT_DATE	= 7,
	VT_BSTR	= 8,
	VT_DISPATCH	= 9,
	VT_ERROR	= 10,
	VT_BOOL	= 11,
	VT_VARIANT	= 12,
	VT_UNKNOWN	= 13,
	VT_DECIMAL	= 14,
	VT_I1	= 16,
	VT_UI1	= 17,
	VT_UI2	= 18,
	VT_UI4	= 19,
	VT_I8	= 20,
	VT_UI8	= 21,
	VT_INT	= 22,
	VT_UINT	= 23,
	VT_VOID	= 24,
	VT_HRESULT	= 25,
	VT_PTR	= 26,
	VT_SAFEARRAY	= 27,
	VT_CARRAY	= 28,
	VT_USERDEFINED	= 29,
	VT_LPSTR	= 30,
	VT_LPWSTR	= 31,
	VT_RECORD	= 36,
	VT_FILETIME	= 64,
	VT_BLOB	= 65,
	VT_STREAM	= 66,
	VT_STORAGE	= 67,
	VT_STREAMED_OBJECT	= 68,
	VT_STORED_OBJECT	= 69,
	VT_BLOB_OBJECT	= 70,
	VT_CF	= 71,
	VT_CLSID	= 72,
	VT_BSTR_BLOB	= 0xfff,
	VT_VECTOR	= 0x1000,
	VT_ARRAY	= 0x2000,
	VT_BYREF	= 0x4000,
	VT_RESERVED	= 0x8000,
	VT_ILLEGAL	= 0xffff,
	VT_ILLEGALMASKED	= 0xfff,
	VT_TYPEMASK	= 0xfff
    };
  **/

const DWORD variant_const_tags[] = {
	VT_VOID,
	VT_EMPTY
};
ENUM_DEFINE(variant_const_tags);

const DWORD var_type[] = {
    VT_VOID
  , VT_EMPTY
  , VT_R4
  , VT_R8
  , VT_DATE
  , VT_BSTR
  , VT_DISPATCH
  , VT_BOOL
  , VT_VARIANT
  , VT_UNKNOWN
  , VT_UI2
  , VT_UI4
  , VT_I1
  , VT_I2
  , VT_I4
  , VT_INT
  , VT_UINT
  , VT_HRESULT
};
ENUM_DEFINE(var_type);

// duplicate values are reserved constructors ID to distinguish between
// int using the 32nth bit and caml int's
const DWORD variant_tags[] = {
  //VT_VOID - constant
  //VT_EMPTY - constant
    VT_R4
  , VT_R8
  , VT_DATE
  , VT_BSTR
  , VT_DISPATCH
  , VT_BOOL
//, VT_VARIANT - invalid
  , VT_UNKNOWN
  , VT_UI2
  , VT_UI4
  , /**/VT_UI4
  , VT_I1
  , VT_I2
  , VT_I4
  , /**/VT_I4
  , VT_INT
  , /**/VT_INT
  , VT_UINT
  , /**/VT_UINT
  , VT_HRESULT
  , /**/VT_HRESULT
  , VT_CLSID
  , VT_PTR
};
ENUM_DEFINE(variant_tags);


VARIANT VARIANT_val( value v ) {
	VARIANT r;
	if( Is_long(v) ) {
		r.vt = (VARTYPE)enum_val(v,variant_const_tags);
		return r;
	}
	int ptag = Tag_val(v);
	value data = Field(v,0);
	if( ptag >= variant_tags_size )
		failwith("Unsupported Variant Value");
	r.vt = (VARTYPE)variant_tags[ptag];

	if( ptag > 0 && variant_tags[ptag-1] == variant_tags[ptag] ) // int32's
		r.lVal = Int32_val(data);
	else
	switch( r.vt ) {
	case VT_VOID:
	case VT_EMPTY:
		break;
	case VT_R4: r.fltVal = (FLOAT)Double_val(data); break;
	case VT_R8:	r.dblVal = Double_val(data); break;
	case VT_DATE: r.date = date_val(data); break;
	case VT_BSTR: {
			BSTR str = string2wide(String_val(data));
			r.bstrVal = SysAllocString(str);
			break; /* TOFREE */
		}
	case VT_DISPATCH: r.pdispVal = IDispatch_val(data); break;
	case VT_BOOL: r.boolVal = Bool_val(data); break;
	case VT_UNKNOWN: r.punkVal = IUnknown_val(data); break;
	case VT_UI2:
	case VT_UI4: 
	case VT_I1:
	case VT_I2:
	case VT_I4:
	case VT_INT: 
	case VT_UINT:
	case VT_HRESULT:
		r.lVal = Int_val(data);
		break;
	case VT_CLSID: ((PROPVARIANT*)&r)->puuid = new GUID(GUID_val(data)); break; /* TOFREE */
	case VT_PTR: r.pvarVal = new VARIANT(VARIANT_val(data)); break; /*  TOFREE */
	default:
		failwith("Oops i forget to implement a variant !");
		break;
	}
	return r;
}

value alloc_variant( VARIANT *v ) {
	CAMLparam0();
	CAMLlocal2(construct,result);
	bool is_int32 = false;
	switch( v->vt  ) {
	case VT_VOID:
	case VT_EMPTY:
		CAMLreturn(alloc_enum(v->vt,variant_const_tags));
	case VT_R4:	result = copy_double(v->fltVal); break;
	case VT_R8: result = copy_double(v->dblVal); break;
	case VT_DATE: result = alloc_date(v->date); break;
	case VT_BSTR: {
		char *str = wide2string(v->bstrVal);
		result = copy_string(str);
		delete str;
		break;
	}
	case VT_DISPATCH: result = alloc_com(v->pdispVal); break;
	case VT_BOOL: result = Val_bool(v->boolVal); break;
	case VT_UNKNOWN: result = alloc_com(v->punkVal); break;
	case VT_UI2:
	case VT_I2:
	case VT_I1:
		result = Val_int(v->iVal);
		break;	
	case VT_INT: 
	case VT_UINT:
	case VT_UI4:
	case VT_I4: 
	case VT_HRESULT:
		is_int32 = (v->lVal < 0);
		if( is_int32 )
			result = copy_int32(v->lVal);
		else
			result = Val_int(v->lVal);
		break;
	case VT_CLSID: result = alloc_guid( ((PROPVARIANT*)v)->puuid ); break;
	case VT_PTR: result = alloc_variant( v->pvarVal ); break;
	default:
		failwith("Oops i forget to implement a variant !");
		break;
	}
	int tag = Int_val(alloc_enum(v->vt,variant_tags));
	construct = alloc_small(1,tag+(is_int32?1:0));
	Field(construct,0) = result;
	CAMLreturn(construct);
}

// free VARIANT_val allocated variant
void free_variant( VARIANT *v ) {
	switch( v->vt ) {
	case VT_BSTR: SysFreeString(v->bstrVal); break;
	case VT_CLSID: delete ((PROPVARIANT*)v)->puuid; break;
	case VT_PTR: free_variant(v->pvarVal); delete v->pvarVal; break;
	}
}

// free sytem/com allocated variant
void sys_free_variant( VARIANT *v ) {
	switch( v->vt ) {
	case VT_BSTR: SysFreeString(v->bstrVal); break;
	/* ? */
	}
}

/* ************************************************************************ */
// OLE Funs

CAMLprim value guid_from_progid( value progname ) {
	DEBUGINFOS(guid_from_progid);
	BSTR progwide = string2wide(String_val(progname));
	GUID guid;
	HRESULT r;
	if( progwide == NULL )
		raise_invalid_progid(String_val(progname));
	r = CLSIDFromProgID(progwide,&guid);
	free_bstr(progwide);
	if( r != S_OK )
		raise_invalid_progid(String_val(progname));
	return alloc_guid(&guid);
}

#define MAX_GUID_LENGTH 76

CAMLprim value string_from_guid( value g ) {
	DEBUGINFOS(string_from_guid);
	BSTR guidstr;
	OLECHAR wstr[MAX_GUID_LENGTH];
	char* str;
	value result;
    int lenW = StringFromGUID2(GUID_val(g),wstr,MAX_GUID_LENGTH);
    guidstr = SysAllocStringLen(wstr,lenW);
	str = wide2string(guidstr);
	SysFreeString(guidstr);
	result = copy_string(str);
	delete str;
	return result;
}

CAMLprim value guid_from_string( value str ) {
	DEBUGINFOS(guid_from_string);
	BSTR guidstr = string2wide(String_val(str));
	GUID g;
	HRESULT h = CLSIDFromString(guidstr,&g);
	if( h != S_OK )
		raise_com_error("CLSIDFromString");
	free_bstr(guidstr);
	return alloc_guid(&g);
}

CAMLprim value guid_eq( value g1, value g2 ) {
	DEBUGINFOS(guid_eq);
	return Val_bool(GUID_val(g1) == GUID_val(g2));
}

CAMLprim value guid_iunknown( value _ ) {
	DEBUGINFOS(guid_iunknown);
	return alloc_guid(&IID_IUnknown);
}

CAMLprim value guid_idispatch( value _ ) {
	DEBUGINFOS(guid_idispatch);
	return alloc_guid(&IID_IDispatch);
}

CAMLprim value guid_null( value _ ) {	
	DEBUGINFOS(guid_null);
	return alloc_guid(&IID_INull);
}

CAMLprim value get_null_com( value _ ) {
	DEBUGINFOS(get_null_com);
	return alloc_com(NULL);
}

CAMLprim value create_idispatch( value g ) {
	DEBUGINFOS(create_idispatch);
	IDispatch *disp;
	HRESULT r = CoCreateInstance(GUID_val(g),NULL,CLSCTX_SERVER,IID_IDispatch,(void**)&disp);
	if( r != S_OK )
		raise_invalid_guid(g);
	return alloc_com(disp);
}

CAMLprim value get_active_object( value g ) {
	DEBUGINFOS(get_active_object);
	IUnknown *unk;
	HRESULT r = GetActiveObject(GUID_val(g),NULL,&unk);
	if( r != S_OK )
		return Val_none;
	return alloc_some(alloc_com(unk));
}

CAMLprim value property_get( value p ) {
	DEBUGINFOS(property_get);
	DISPPARAMS params = { NULL, NULL, 0, 0 };
	VARIANT result;
	EXCEPINFO exc;
	DISPID did = DISPID_val(Field(p,1));
	HRESULT r = IDispatch_val(Field(p,0))->Invoke( did, IID_NULL, LCID, DISPATCH_PROPERTYGET, &params, &result, &exc, NULL);
	if( r == DISP_E_EXCEPTION )
		raise_dispatch_exc(&exc);
	if( r != S_OK )
		raise_com_error("IDispatch.Invoke(PGET)");
	value ret = alloc_variant(&result);
	sys_free_variant(&result);
	return ret;
}

CAMLprim value property_set( value p, value ml_v ) {
	DEBUGINFOS(property_set);
	VARIANT v = VARIANT_val(ml_v);
	DISPID putid = DISPID_PROPERTYPUT;
	DISPPARAMS params = { &v, &putid, 1, 1 };
	EXCEPINFO exc;
	DISPID did = DISPID_val(Field(p,1));
	WORD calltype = Bool_val(Field(p,2))?DISPATCH_PROPERTYPUTREF:DISPATCH_PROPERTYPUT;
	HRESULT r = IDispatch_val(Field(p,0))->Invoke( did, IID_NULL, LCID, calltype,&params,NULL,&exc,NULL);
	free_variant(&v);
	if( r == DISP_E_EXCEPTION )
		raise_dispatch_exc(&exc);
	if( r != S_OK )
		raise_com_error("IDispatch.Invoke(PSET)");	
	return Val_unit;
}

/* ************************************************************************ */
// IDispatch Funs

CAMLprim value idisp_get_type_info_count( value disp ) {
	DEBUGINFOS(idisp_get_type_info_count);
	unsigned int count;
	if( IDispatch_val(disp)->GetTypeInfoCount(&count) != S_OK )
		raise_com_error("IDispatch.GetTypeInfoCount");
	return Val_int(count);
}

CAMLprim value idisp_get_type_info( value disp, value tnb ) {
	DEBUGINFOS(idisp_get_type_info);
	ITypeInfo *tinf;
	if( IDispatch_val(disp)->GetTypeInfo(Int_val(tnb),LCID,&tinf) != S_OK )
		raise_com_error("IDispatch.GetTypeInfo");
	return alloc_com_ref(tinf,disp);
}

CAMLprim value idisp_get_dispid( value disp, value name ) {
	DEBUGINFOS(idisp_get_dispid);
	CAMLparam2(disp,name);
	CAMLlocal1(result);
#ifdef _DEBUG
	current_call = String_val(name);
#endif
	BSTR fname = string2wide(String_val(name));
	DISPID id;
	if( IDispatch_val(disp)->GetIDsOfNames(IID_NULL,&fname,1,LCID,&id) != S_OK ) {
		free_bstr(fname);
		raise_com_error("IDispatch.GetIDsOfNames");
	}
	free_bstr(fname);
	result = alloc_dispid(id);
	CAMLreturn(result);
}

CAMLprim value idisp_query_interface( value disp, value guid ) {
	DEBUGINFOS(idisp_query_interface);
	IUnknown *ptr;
	if( IDispatch_val(disp)->QueryInterface(GUID_val(guid),(void**)&ptr) != S_OK )
		raise_com_error("IDispatch.QueryInterface");
	return alloc_com(ptr);
}

CAMLprim value idisp_invoke( value disp, value id, value args, value indexes, value disphow ) {
	DEBUGINFOS(idisp_invoke);
	CAMLparam4(disp,id,args,indexes);	
	CAMLlocal2(v,vreturn);
	DISPID did = DISPID_val(id);
	int nargs = 0;
	v = args;
	while( v != Val_int(0) ) {
		v = Field(v,1);
		nargs++;
	}
	VARIANT *vargs = new VARIANT[nargs];
	v = args;
	int n = 0;
	while( v != Val_int(0) ) {
		if( Field(v,0) != Val_none )
			vargs[n++] = VARIANT_val(Field(Field(v,0),0));
		else {
			vargs[n].vt = VT_ERROR;
			vargs[n].scode = DISP_E_PARAMNOTFOUND;
			n++;
		}
		v = Field(v,1);
	}
	EXCEPINFO exc;
	DISPPARAMS params = { vargs, NULL, nargs, 0 };
	VARIANT result;
	WORD calltype = call_type[Int_val(disphow)];
	HRESULT r = IDispatch_val(disp)->Invoke( did, IID_NULL, LCID, calltype, &params, &result, &exc, NULL );
	if( r == DISP_E_EXCEPTION )
		raise_dispatch_exc(&exc);
	if( r != S_OK )
		raise_com_error("IDispatch.Invoke");
	
	vreturn = Val_int(0);
	while( indexes != Val_int(0) ) {
		v = alloc_variant(&vargs[Int_val(Field(indexes,0))]);
		indexes = Field(indexes,1);
		vreturn = alloc_cons(v,vreturn);
	}

	for(n=0;n<nargs;n++)
		free_variant(&vargs[n]);
	delete vargs;

	v = alloc_variant(&result);
	sys_free_variant(&result);
	vreturn = alloc_cons(v,vreturn);

	CAMLreturn(vreturn);	
}

CAMLprim value com_do_not_close( value _ ) {
	DEBUGINFOS(com_do_not_close);
	uninit = false;
	return Val_unit;
}

/* ************************************************************************ */
// ITypeInfo Funs

CAMLprim value itinf_get_type_attr( value tinf ) {
	DEBUGINFOS(itinf_get_type_attr);
	CAMLparam1(tinf);	
	CAMLlocal5(result,vguid,mid1,mid2,version);

	TYPEATTR *tattr;
	ITypeInfo *tt = ITypeInfo_val(tinf);

	if( ITypeInfo_val(tinf)->GetTypeAttr(&tattr) != S_OK )
		raise_com_error("ITypeInfo.GetTypeAttr");	

	vguid = alloc_guid(&tattr->guid);
	mid1 = alloc_memberid_option(tattr->memidConstructor);
	mid2 = alloc_memberid_option(tattr->memidDestructor);
	version = alloc_tuple(2);
	Field(version,0) = Val_int(tattr->wMajorVerNum);
	Field(version,1) = Val_int(tattr->wMinorVerNum);
	result = alloc_small(8,0);
	Field(result,0) = vguid;
	Field(result,1) = mid1;
	Field(result,2) = mid2;
	Field(result,3) = Val_int(tattr->cFuncs);
	Field(result,4) = Val_int(tattr->cVars);
	Field(result,5) = Val_int(tattr->cImplTypes);
	Field(result,6) = version;
	Field(result,7) = alloc_enum(tattr->typekind,type_kind);

	ITypeInfo_val(tinf)->ReleaseTypeAttr(tattr);

	CAMLreturn(result);
}

CAMLprim value itinf_get_names( value tinf, value mid ) {
	DEBUGINFOS(itinf_get_names);
	CAMLparam2(tinf,mid);
	CAMLlocal1(result);

	BSTR *names = new BSTR[256];
	unsigned int nbstr = 0;	
	if( ITypeInfo_val(tinf)->GetNames( MEMBERID_val(mid), names, 256, &nbstr ) != S_OK || !nbstr )
		raise_com_error("ITypeInfo.GetNames");

	result = Val_int(0); // []
	unsigned int i;
	for(i=0;i<nbstr;i++) {
		char *str = wide2string(names[i]);
		result = alloc_cons(copy_string(str),result);
		delete str;
		SysFreeString(names[i]);
	}
	delete names;	
	CAMLreturn(result);
}

CAMLprim value itinf_get_func_desc( value tinf, value fnb ) {
	DEBUGINFOS(itinf_get_func_desc);
	CAMLparam2(tinf,fnb);	
	CAMLlocal4(result,tmp,mid,edesc);

	FUNCDESC *fdesc;
	ITypeInfo *itinf = ITypeInfo_val(tinf);

	if( itinf->GetFuncDesc(Int_val(fnb),&fdesc) != S_OK )
		raise_com_error("ITypeInfo.GetFuncDesc");

	mid = alloc_memberid(fdesc->memid);
	edesc = alloc_elem_desc(itinf,&fdesc->elemdescFunc);
	result = alloc_small(7,0);
	Field(result,0) = mid;
	Field(result,1) = alloc_enum(fdesc->funckind,func_kind);
	Field(result,2) = alloc_enum(fdesc->invkind,invoke_kind);
	Field(result,3) = Val_int(fdesc->cParamsOpt);
	Field(result,4) = edesc;
	Field(result,5) = Val_int(0); // []
	Field(result,6) = Val_bool(fdesc->wFuncFlags & FUNCFLAG_FHIDDEN);

	// store params in an ordered list
	int i;
	for(i=fdesc->cParams-1;i>=0;i--) {
		tmp = alloc_elem_desc(itinf,&fdesc->lprgelemdescParam[i]);
		modify(&Field(result,5), alloc_cons(tmp,Field(result,5)));
	}	

	ITypeInfo_val(tinf)->ReleaseFuncDesc(fdesc);
	
	CAMLreturn(result);
}

CAMLprim value itinf_get_var_desc( value tinf, value vnb ) {
	DEBUGINFOS(itinf_get_var_desc);
	CAMLparam2(tinf,vnb);
	CAMLlocal4(result,mid,edesc,vkind);

	VARDESC *vdesc;
	ITypeInfo *itinf = ITypeInfo_val(tinf);
	if( itinf->GetVarDesc(Int_val(vnb),&vdesc) != S_OK )
		raise_com_error("ITypeInfo.GetVarDesc");

	mid = alloc_memberid(vdesc->memid);
	edesc = alloc_elem_desc(itinf,&vdesc->elemdescVar);
	vkind = alloc_var_kind(vdesc->varkind,vdesc->lpvarValue);
	result = alloc_small(5,0);
	Field(result,0) = mid;
	Field(result,1) = edesc;
	Field(result,2) = vkind;
	Field(result,3) = Val_bool(vdesc->wVarFlags & VARFLAG_FHIDDEN);
	Field(result,4) = Val_bool(vdesc->wVarFlags & VARFLAG_FREADONLY);

	CAMLreturn(result);
}

CAMLprim value itinf_get_type_lib( value tinf ) {
	DEBUGINFOS(itinf_get_type_lib);
	CAMLparam1(tinf);
	ITypeLib *tlib;
	unsigned int index;
	CAMLlocal2(result,vcom);
	
	if( ITypeInfo_val(tinf)->GetContainingTypeLib(&tlib,&index) != S_OK )
		raise_com_error("ITypeInfo.GetContainingTypeLib");
	
	vcom = alloc_com_ref(tlib,tinf);
	result = alloc_tuple(2);
	Field(result,0) = vcom;
	Field(result,1) = Val_int(index);

	CAMLreturn(result);
}

CAMLprim value itinf_get_interface( value tinf, value inb ) {
	DEBUGINFOS(itinf_get_interface);
	CAMLparam2(tinf,inb);
	CAMLlocal1(result);

	ITypeInfo *tref;
	HREFTYPE href;
	if( ITypeInfo_val(tinf)->GetRefTypeOfImplType(Int_val(inb),&href) != S_OK )
		raise_com_error("ITypeInfo.GetRefTypeOfImplType");
	if( ITypeInfo_val(tinf)->GetRefTypeInfo(href,&tref) != S_OK )
		raise_com_error("ITypeInfo.GetRefTypeInfo");
	result = alloc_com_ref(tref,tinf);
	CAMLreturn(result);
}

/* ************************************************************************ */
// ITypeLib Funcs

CAMLprim value itlib_get_type_info_count( value tlib ) {
	DEBUGINFOS(itlib_get_type_info_count);
	HRESULT count = ITypeLib_val(tlib)->GetTypeInfoCount();
	if( count == E_NOTIMPL )
		raise_com_error("ITypeLib.GetTypeInfoCount");
	return Val_int(count);
}

CAMLprim value itlib_get_type_info( value tlib, value index ) {
	DEBUGINFOS(itlib_get_type_info);
	CAMLparam2(tlib,index);
	ITypeInfo *tinf;
	CAMLlocal1(result);
	if( ITypeLib_val(tlib)->GetTypeInfo(Int_val(index),&tinf) != S_OK )
		raise_com_error("ITypeLib.GetTypeInfo");
	result = alloc_com_ref(tinf,tlib);
	CAMLreturn(result);
}

CAMLprim value itlib_get_type_name( value tlib, value index ) {
	DEBUGMETHOD(ITypeLib_val(tlib),itlib_get_type_name);
	BSTR name;
	if( ITypeLib_val(tlib)->GetDocumentation(Int_val(index),&name,NULL,NULL,NULL) != S_OK )
		raise_com_error("ITypeLib.GetDocumentation");
	DEBUGINFOS(OK);
	char *str = wide2string(name);
	value v = copy_string(str);
	SysFreeString(name);
	delete str;
	return v;
}

/* ************************************************************************ */
// INIT / CLOSE ( native mode )

CAMLprim value com_init() {
	DEBUGINFOS(com_init);
#ifndef DLL	
	return Val_bool( CoInitialize(NULL) );	
#else
	return Val_true;
#endif
}

CAMLprim value com_close() {
	DEBUGINFOS(com_close);
#ifndef DLL
	if( uninit )
		CoUninitialize();
#endif
	return Val_unit;
}

/* ************************************************************************ */
// ENTRY POINT

CAMLprim BOOL WINAPI DllMain(HINSTANCE hinstDLL,DWORD fdwReason,LPVOID lpvReserved) {
	switch( fdwReason ) {
	case DLL_PROCESS_ATTACH:
#		if 0 // turn on MSVC CRT check
		_CrtSetDbgFlag ( _CRTDBG_ALLOC_MEM_DF | _CRTDBG_DELAY_FREE_MEM_DF | _CRTDBG_LEAK_CHECK_DF );
#		endif
		if( CoInitialize(NULL) != S_OK )
			return FALSE;
		break;
	case DLL_PROCESS_DETACH:
		if( uninit )
			CoUninitialize();
		break;
	}
	return TRUE;
}

/* ************************************************************************ */
}; // extern "C"
/* ************************************************************************ */
