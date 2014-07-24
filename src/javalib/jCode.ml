(*
 *  This file is part of JavaLib
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
open IO.BigEndian
open ExtList
open JClass

exception Invalid_opcode of int

let kind = function
	| 0 -> KInt
	| 1 -> KLong
	| 2 -> KFloat
	| 3 -> KDouble
	| _ -> assert false

let parse_opcode op ch =
	match op with
	| 0 ->
		OpNop
	(* ---- push ----------------------------------- *)
	| 1 ->
		OpAConstNull
	| 2 ->
		OpIConst Int32.minus_one
	| 3 | 4 | 5 | 6 | 7 | 8 ->
		OpIConst (Int32.of_int (op - 3))
	| 9 ->
		OpLConst Int64.zero
	| 10 ->
		OpLConst Int64.one
	| 11 | 12 | 13 ->
		OpFConst (float_of_int (op - 11))
	| 14 ->
		OpDConst 0.
	| 15 ->
		OpDConst 1.
	| 16 ->
		OpBIPush (IO.read_byte ch)
	| 17 ->
		OpSIPush (IO.read_i16 ch)
	| 18 ->
		OpLdc1 (IO.read_byte ch)
	| 19 ->
		OpLdc2 (read_ui16 ch)
	| 20 ->
		OpLdc2w (read_ui16 ch)
	(* ---- load ----------------------------------- *)
	| 21 | 22 | 23 | 24 ->
		OpLoad (kind (op - 21),IO.read_byte ch)
	| 25 ->
		OpALoad (IO.read_byte ch)
	| 26 | 27 | 28 | 29 ->
		OpLoad (KInt,op - 26)
	| 30 | 31 | 32 | 33 ->
		OpLoad (KLong,op - 30)
	| 34 | 35 | 36 | 37 ->
		OpLoad (KFloat,op - 34)
	| 38 | 39 | 40 | 41 ->
		OpLoad (KDouble,op - 38)
	| 42 | 43 | 44 | 45 ->
		OpALoad (op - 42)
	(* ---- array load ---------------------------- *)
	| 46 | 47 | 48 | 49 ->
		OpArrayLoad (kind (op - 46))
	| 50 ->
		OpAALoad
	| 51 ->
		OpBALoad
	| 52 ->
		OpCALoad
	| 53 ->
		OpSALoad		
	(* ---- store ----------------------------------- *)
	| 54 | 55 | 56 | 57 ->
		OpStore (kind (op - 54),IO.read_byte ch)
	| 58 ->
		OpAStore (IO.read_byte ch)
	| 59 | 60 | 61 | 62 ->
		OpStore (KInt , op - 59)
	| 63 | 64 | 65 | 66 ->
		OpStore (KLong , op - 63)
	| 67 | 68 | 69 | 70 ->
		OpStore (KFloat , op - 67)
	| 71 | 72 | 73 | 74 ->
		OpStore (KDouble , op - 71)
	| 75 | 76 | 77 | 78 ->
		OpAStore (op - 75)
	(* ---- array store ---------------------------- *)
	| 79 | 80 | 81 | 82 ->
		OpArrayStore (kind (op - 79))
	| 83 ->
		OpAAStore
	| 84 ->
		OpBAStore
	| 85 ->
		OpCAStore
	| 86 ->
		OpSAStore
	(* ---- stack ---------------------------------- *)
	| 87 ->
		OpPop
	| 88 ->
		OpPop2
	| 89 ->
		OpDup
	| 90 ->
		OpDupX1
	| 91 ->
		OpDupX2
	| 92 ->
		OpDup2
	| 93 ->
		OpDup2X1
	| 94 ->
		OpDup2X2
	| 95 ->
		OpSwap
	(* ---- arithmetics ---------------------------- *)
	| 96 | 97 | 98 | 99 ->
		OpAdd (kind (op - 96))
	| 100 | 101 | 102 | 103 ->
		OpSub (kind (op - 100))
	| 104 | 105 | 106 | 107 ->
		OpMult (kind (op - 104))
	| 108 | 109 | 110 | 111 ->
		OpDiv (kind (op - 108))
	| 112 | 113 | 114 | 115 ->
		OpRem (kind (op - 112))
	| 116 | 117 | 118 | 119 ->
		OpNeg (kind (op - 116))
	(* ---- logicals ------------------------------- *)
	| 120 ->
		OpIShl 
	| 121 ->
		OpLShl
	| 122 ->
		OpIShr
	| 123 ->
		OpLShr
	| 124 ->
		OpIUShr
	| 125 ->
		OpLUShr
	| 126 ->
		OpIAnd
	| 127 ->
		OpLAnd
	| 128 ->
		OpIOr
	| 129 ->
		OpLOr
	| 130 ->
		OpIXor
	| 131 ->
		OpLXor
	(* ---- incr ----------------------------------- *)
	| 132 ->
		let idx = IO.read_byte ch in
		let c = IO.read_signed_byte ch in
		OpIInc (idx,c)
	(* ---- conversions ---------------------------- *)
	| 133 ->
		OpI2L 
	| 134 ->
		OpI2F
	| 135 ->
		OpI2D
	| 136 ->
		OpL2I
	| 137 ->
		OpL2F
	| 138 ->
		OpL2D
	| 139 ->
		OpF2I
	| 140 ->
		OpF2L
	| 141 ->
		OpF2D
	| 142 ->
		OpD2I
	| 143 ->
		OpD2L
	| 144 ->
		OpD2F
	| 145 ->
		OpIntToByte
	| 146 ->
		OpIntToChar
	| 147 ->
		OpIntToShort
	(* ---- jumps ---------------------------------- *)
	| 148 ->
		OpLCmp
	| 149 ->
		OpFCmpL
	| 150 ->
		OpFCmpG
	| 151 ->
		OpDCmpL
	| 152 ->
		OpDCmpG
	| 153 ->
		OpIfEq (read_i16 ch)
	| 154 ->
		OpIfNe (read_i16 ch)
	| 155 ->
		OpIfLt (read_i16 ch)
	| 156 ->
		OpIfGe (read_i16 ch)
	| 157 ->
		OpIfGt (read_i16 ch)
	| 158 ->
		OpIfLe (read_i16 ch)
	| 159 ->
		OpICmpEq (read_i16 ch)
	| 160 ->
		OpICmpNe (read_i16 ch)
	| 161 ->
		OpICmpLt (read_i16 ch)
	| 162 ->
		OpICmpGe (read_i16 ch)
	| 163 ->
		OpICmpGt (read_i16 ch)
	| 164 ->
		OpICmpLe (read_i16 ch)
	| 165 ->
		OpACmpEq (read_i16 ch)
	| 166 ->
		OpACmpNe (read_i16 ch)
	| 167 ->
		OpGoto (read_i16 ch)
	| 168 ->
		OpJsr (read_i16 ch)
	| 169 ->
		OpRet (IO.read_byte ch)
	| 170 ->
		let def = read_i32 ch in
		let low = read_real_i32 ch in
		let high = read_real_i32 ch in
		let tbl = Array.init (Int32.to_int (Int32.sub high low) + 1) (fun _ -> read_i32 ch) in
		OpTableSwitch (def,low,high,tbl)
	| 171 ->
		let def = read_i32 ch in
		let npairs = read_i32 ch in
		let tbl = List.init npairs (fun _ ->
			let v = read_real_i32 ch in
			let j = read_i32 ch in
			v , j
		) in
		OpLookupSwitch (def,tbl)
	(* ---- returns --------------------------------- *)
	| 172 | 173 | 174 | 175 ->
		OpReturn (kind (op - 172))
	| 176 ->
		OpAReturn
	| 177 ->
		OpReturnVoid
	(* ---- OO ------------------------------------- *)
	| 178 ->
		OpGetStatic (read_ui16 ch)
	| 179 ->
		OpPutStatic (read_ui16 ch)
	| 180 ->
		OpGetField (read_ui16 ch)
	| 181 ->
		OpPutField (read_ui16 ch)
	| 182 ->
		OpInvokeVirtual (read_ui16 ch)
	| 183 ->
		OpInvokeNonVirtual (read_ui16 ch)
	| 184 ->
		OpInvokeStatic (read_ui16 ch)
	| 185 ->
		let idx = read_ui16 ch in
		let nargs = IO.read_byte ch in
		let _ = IO.read_byte ch in
		OpInvokeInterface (idx,nargs)
	(* ---- others --------------------------------- *)
	| 187 ->
		OpNew (read_ui16 ch)
	| 188 ->
		OpNewArray (match IO.read_byte ch with
			| 4 -> ATBool
			| 5 -> ATChar
			| 6 -> ATFloat
			| 7 -> ATDouble
			| 8 -> ATByte
			| 9 -> ATShort
			| 10 -> ATInt
			| 11 -> ATLong
			| _ -> raise Exit)
	| 189 ->
		OpANewArray (read_ui16 ch)
	| 190 ->
		OpArrayLength
	| 191 ->
		OpThrow
	| 192 ->
		OpCheckCast (read_ui16 ch)
	| 193 ->
		OpInstanceOf (read_ui16 ch)
	| 194 ->
		OpMonitorEnter
	| 195 ->
		OpMonitorExit
	| 196 ->
		OpWide (IO.read_byte ch)
	| 197 ->
		let idx = read_ui16 ch in
		let dims = IO.read_byte ch in
		OpAMultiNewArray (idx,dims)
	| 198 ->
		OpIfNull (read_i16 ch)
	| 199 ->
		OpIfNonNull (read_i16 ch)
	| 200 ->
		OpGotoW (read_i32 ch)
	| 201 ->
		OpJsrW (read_i32 ch)
	| 202 ->
		OpBreakpoint
	| 209 ->
		OpRetW (read_ui16 ch)
	| n ->
		raise Exit

let parse_code ch len =
	let ch , pos = IO.pos_in ch in
	let code = Array.create len OpInvalid in
	while pos() < len do
		let p = pos() in
		let op = IO.read_byte ch in
		if (op = 170 || op = 171) && (p + 1) mod 4 > 0 then ignore(IO.nread ch (4 - ((p + 1) mod 4)));
		try
			code.(p) <- parse_opcode op ch
		with
			Exit -> raise (Invalid_opcode op)
	done;
	code