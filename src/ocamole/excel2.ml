open Ocamole
open Excel
;;
let app = IApplication.application_ (IApplication.create()) in
let books = Application_.get_workbooks app in
let book = Workbooks.add books in
let sheets = Workbook_.get_worksheets (Workbook.workbook_ book) in
let sheet = Worksheet_.query ~unsafe:true (Sheets.get_item sheets ~index:(VT_INT 1)) in
print_endline (pget (Worksheet_.name sheet));
