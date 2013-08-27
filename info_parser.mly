%{
(***********************************************************************)
(*                             OCamldoc                                *)
(*                                                                     *)
(*            Maxence Guesdon, projet Cristal, INRIA Rocquencourt      *)
(*                                                                     *)
(*  Copyright 2001 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Info
open Bindoc_errors

let remove_opening_blanks s = 
  let length = String.length s in  
  let rec loop i =
    if i >= length then "" else
    match s.[i] with
    | '\010' | '\013' | ' ' | '\009' | '\012' -> loop (i + 1)
    | _ -> String.sub s i (length - i)
  in
    loop 0

let remove_closing_blanks s =
  let length = String.length s in
  let rec loop i =
    if i < 0 then "" else
    match s.[i] with
    | '\010' | '\013' | ' ' | '\009' | '\012' -> loop (i - 1)
    | _ -> String.sub s 0 (i + 1)
  in
    loop (length - 1)

let remove_blanks s =
  let s = remove_opening_blanks s in
    remove_closing_blanks s

let remove_text_blanks tl =
  let rec loop remove tl =
      match tl with
        Raw s :: rest -> begin
          match remove s with
            "" -> loop remove rest
          | s -> Raw s :: rest
        end
      | Newline :: rest -> 
          loop remove rest
      | _ -> tl
  in
  let tl = loop remove_opening_blanks tl in
  let rtl = loop remove_closing_blanks (List.rev tl) in
    List.rev rtl

let check_blank s =
  let length = String.length s in  
  let rec loop i =
    if i >= length then () else
    match s.[i] with
    | '\010' | '\013' | ' ' | '\009' | '\012' -> loop (i + 1)
    | _ -> raise Parsing.Parse_error
  in
    loop 0

let unclosed opening_name opening_num closing_name closing_num =
  raise (Error(Parser (Unclosed(Location.rhs_loc opening_num, opening_name, closing_name)), Location.rhs_loc closing_num))

let expecting pos nonterm =
    raise (Error(Parser(Expecting nonterm), Location.rhs_loc pos))

let title_to_string (i, _) =
  let i = string_of_int i in
    "{" ^ i

let style_to_string = function
  | SK_bold -> "{b"
  | SK_italic -> "{i"
  | SK_emphasize -> "{e"
  | SK_center -> "{C"
  | SK_left -> "{L"
  | SK_right -> "{R"
  | SK_superscript -> "{^"
  | SK_subscript -> "{_"
  | SK_custom s -> "{" ^ s

let html_open_to_string t = "<" ^ t ^ ">"
let html_close_to_string t = "</" ^ t ^ ">"

%}

%token <string> Param 
%token AUTHOR
%token <string> Version
%token <Info.see_ref> See
%token SINCE
%token <string> Before
%token DEPRECATED
%token <string> Raise
%token RETURN
%token <string> Custom

%token BEGIN
%token END

%token <int * string option> Title
%token <Info.style_kind> Style
%token LIST
%token ENUM
%token <bool> Item

%token <Info.ref_kind * string> Ref
%token <Info.special_ref_kind> Special_Ref

%token <string> Code
%token <string> Pre_Code
%token <string> Verb
%token <string option * string> Target

%token <string> HTML_Bold
%token HTML_END_BOLD
%token <string> HTML_Center
%token HTML_END_CENTER
%token <string> HTML_Left
%token HTML_END_LEFT
%token <string> HTML_Right
%token HTML_END_RIGHT
%token <string> HTML_Italic
%token HTML_END_ITALIC
%token <string * int> HTML_Title
%token <int> HTML_END_Title
%token <string> HTML_List
%token HTML_END_LIST
%token <string> HTML_Enum
%token HTML_END_ENUM
%token <string> HTML_Item
%token HTML_END_ITEM

%token SHORTCUT_LIST_ITEM
%token SHORTCUT_ENUM_ITEM

%token BLANK_LINE

%token EOF
%token <string> Char

%left SHORTCUT_LIST_ITEM SHORTCUT_ENUM_ITEM BLANK_LINE
%nonassoc Shortcut_Text
%right Char

/* Start Symbols */
%start info
%type <Info.t> info

%%
info:
  text tags EOF { {$2 with i_desc = Some (remove_text_blanks $1)} }
| tags EOF { $1 }
;

tags:
  /* empty */ { Info.dummy }
| Param text tags 
    { let info = $3 in
      let i_params = ($1, remove_text_blanks $2) :: info.i_params in
        {info with i_params} }
| Param error
    { expecting 2 "text" }
| Param text error
    { expecting 3 "tag" }
| AUTHOR string tags
    { let info = $3 in
      let i_authors = (remove_blanks $2) :: info.i_authors in
        {info with i_authors} }
| AUTHOR error
    { expecting 2 "string" }
| AUTHOR string error
    { expecting 3 "tag" }
| Version string tags     
    { (* TODO: a test that the string only contains blanks *)
      {$3 with i_version = Some $1} }
| Version error
    { expecting 2 "string" }
| Version string error
    { expecting 3 "tag" }
| See text tags
    { let info = $3 in
      let i_sees = ($1, remove_text_blanks $2) :: info.i_sees in
        {info with i_sees} }
| See error
    { expecting 2 "text" }
| See text error
    { expecting 3 "tag" }
| SINCE string tags
    { {$3 with i_since = Some (remove_blanks $2)} }
| SINCE error
    { expecting 2 "string" }
| SINCE string error
    { expecting 3 "tag" }
| Before text tags
    { let info = $3 in
      let i_before = ($1, remove_text_blanks $2) :: info.i_before in
        {info with i_before} }
| Before error
    { expecting 2 "text" }
| Before text error
    { expecting 3 "tag" }
| DEPRECATED text tags
    { {$3 with i_deprecated = Some (remove_text_blanks $2)} }
| DEPRECATED error
    { expecting 2 "text" }
| DEPRECATED text error
    { expecting 3 "tag" }
| Raise text tags
    { let info = $3 in
      let i_raised_exceptions = ($1, remove_text_blanks $2) :: info.i_raised_exceptions in
        {info with i_raised_exceptions} }
| Raise error
    { expecting 2 "text" }
| Raise text error
    { expecting 3 "tag" }
| RETURN text tags 
    { {$3 with i_return_value = Some (remove_text_blanks $2)} }
| RETURN error
    { expecting 2 "text" }
| RETURN text error
    { expecting 3 "tag" }
| Custom text tags
    { let info = $3 in
      let i_custom = ($1, remove_text_blanks $2) :: info.i_custom in
        {info with i_custom} }
| Custom error
    { expecting 2 "text" }
| Custom text error
    { expecting 3 "tag" }
;

text:
  text_element { [ $1 ] }
| text_element text { $1 :: $2 }
;

text_element:
| Title text END { let n, l_opt = $1 in Title (n, l_opt, $2) }
| Title text error { unclosed (title_to_string $1) 1 "}" 3 }
| Title error { expecting 2 "text" }
| Style text END { Style($1, $2) }
| Style text error { unclosed (style_to_string $1) 1 "}" 3 }
| Style error { expecting 2 "text" }
| LIST list END { List (List.rev $2) }
| LIST blanks END { List [] }
| LIST list error { unclosed "{ul" 1 "}" 3 }
| LIST blanks error { expecting 2 "list item" }
| ENUM list END { Enum (List.rev $2) }
| ENUM END { Enum [] }
| ENUM list error { unclosed "{ol" 1 "}" 3 }
| ENUM blanks error { expecting 2 "list item" }
| Ref { let k, n = $1 in Ref (k, n, None) }
| BEGIN Ref text END { let k, n = $2 in Ref (k, n, Some $3) }
| BEGIN Ref text error { unclosed "{" 1 "}" 3 }
| BEGIN Ref error { expecting 2 "text" }
| Special_Ref { Special_ref $1 }
| Code { Code $1 }
| Pre_Code { PreCode $1 }
| Verb { Verbatim $1 }
| Target { let t_opt, s = $1 in Target (t_opt, s) }
| BLANK_LINE { Newline }
| html_text_element { $1 }
| shortcut_list { List $1 }
| shortcut_enum { Enum $1 }
| string { Raw $1 }
;

list:
| blanks item blanks { [ $2 ] }
| blanks item list { $2 :: $3 }
;

blanks:
| /* empty */ { () }
| string blanks { check_blank $1 }
| BLANK_LINE blanks { () }
;

item:
  Item text END { $2 }
| Item text error { unclosed (if $1 then "{-" else "{li") 1 "}" 3 }
| Item error { expecting 2 "text" }
;

shortcut_text_non_empty:
| text_element %prec Shortcut_Text { [$1] }
| text_element shortcut_text_non_empty { $1 :: $2 }
;

shortcut_text:
  /* empty */ %prec Shortcut_Text { [] }
| shortcut_text_non_empty { $1 }

shortcut_list:
  SHORTCUT_LIST_ITEM shortcut_text shortcut_list { $2 :: $3 }
| SHORTCUT_LIST_ITEM shortcut_text BLANK_LINE { [$2] }
| SHORTCUT_LIST_ITEM shortcut_text EOF { [$2] }
| SHORTCUT_LIST_ITEM error { expecting 2 "text" }
;

shortcut_enum:
  SHORTCUT_ENUM_ITEM shortcut_text shortcut_enum { $2 :: $3 }
| SHORTCUT_ENUM_ITEM shortcut_text BLANK_LINE { [$2] }
| SHORTCUT_ENUM_ITEM shortcut_text EOF { [$2] }
| SHORTCUT_ENUM_ITEM error { expecting 2 "text" }
;

string:
  Char { $1 }
| Char string { $1^$2 }
;

html_text_element:
  HTML_Title text HTML_END_Title 
    { let _, n = $1 in
      if n <> $3 then raise Parse_error; 
      Title(n, None, $2) }
| HTML_Title text error 
    { let tag, _ = $1 in
      unclosed (html_open_to_string tag) 1 (html_close_to_string tag) 3 }
| HTML_Title error { expecting 2 "text" }
| HTML_Bold text HTML_END_BOLD { Style(SK_bold, $2) }
| HTML_Bold text error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_Bold error { expecting 2 "text" }
| HTML_Italic text HTML_END_ITALIC { Style(SK_italic, $2) }
| HTML_Italic text error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_Italic error { expecting 2 "text" }
| HTML_Center text HTML_END_CENTER { Style(SK_center, $2) }
| HTML_Center text error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_Center error { expecting 2 "text" }
| HTML_Left text HTML_END_LEFT { Style(SK_left, $2) }
| HTML_Left text error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_Left error { expecting 2 "text" }
| HTML_Right text HTML_END_RIGHT { Style(SK_right, $2) }
| HTML_Right text error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_Right error { expecting 2 "text" }
| HTML_List html_list HTML_END_LIST { List $2 }
| HTML_List blanks HTML_END_LIST { List [] }
| HTML_List html_list error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_List blanks error { expecting 2 "html list item" }
| HTML_Enum html_list HTML_END_ENUM { Enum $2 }
| HTML_Enum blanks HTML_END_ENUM { Enum [] }
| HTML_Enum html_list error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_Enum blanks error { expecting 2 "html list item" }
;

html_list:
| blanks html_item blanks { [ $2 ] }
| blanks html_item list { $2 :: $3 }
;

html_item:
  HTML_Item text HTML_END_ITEM { $2 }
| HTML_Item text error 
    { unclosed (html_open_to_string $1) 1 (html_close_to_string $1) 3 }
| HTML_Item error { expecting 2 "text" }
;

%%
