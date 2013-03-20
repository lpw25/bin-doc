{
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

(** The lexer for string to build text structures. *)

open Info
open Info_parser
open Errors
open Lexing

(* Update the current location with file name and line number. *)

let update_loc lexbuf loc =
  lexbuf.lex_curr_p <- loc

let incr_line ?(lines = 1) ?(chars = 0) lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- 
    { lexbuf.lex_curr_p with
      pos_lnum = pos.pos_lnum + lines;
      pos_bol = pos.pos_cnum - chars; }

(* To buffer verbatim strings and code sections *)

let string_buffer = Buffer.create 32

let buffer_empty = ref true

let reset_string_buffer () = 
  Buffer.reset string_buffer;
  buffer_empty := true

let buffer_char c = 
  Buffer.add_char string_buffer c;
  buffer_empty := false

let buffer_lexeme lexbuf = 
  let s = Lexing.lexeme lexbuf in
    Buffer.add_string string_buffer s;
    buffer_empty := false

let get_buffered_string () = Buffer.contents string_buffer

let buffer_not_empty () = not !buffer_empty

(* To store the position of the beginning of a 
   verbatim string or code section *)
let string_start_loc = ref Location.none;;
let code_start_locs = ref [];;

(* To store the format of a target *)
let target_format = ref None;;

(* To store the kind of a reference *)
let ref_kind = ref RK_element;;

(* To store the modules of a module list *)
let module_list_modules = ref [];;

let reset_module_list () =
  module_list_modules := [];;

let add_module md = 
  module_list_modules := md :: !module_list_modules

let get_module_list () =
  List.rev !module_list_modules

(* Count number of newlines in a string *)
let count_newlines str =
  let lines = ref 0 in
  let do_char c = if c = '\n' then incr lines in
    String.iter do_char str;
    !lines

(* Hash tabe of styles *)
let style_table = Hashtbl.create 19
let _ =
  List.iter 
    (fun (kwd, tok) -> Hashtbl.add style_table kwd tok)
    [ ("b", Style SK_bold);
      ("e", Style SK_emphasize);
      ("C", Style SK_center);
      ("L", Style SK_left);
      ("R", Style SK_right);
      ("i", Style SK_italic);
      ("ul", LIST);
      ("ol", ENUM);
      ("li", Item false); ]

(* Hash table of reference kinds *)
let ref_kind_table = Hashtbl.create 19
let _ =
  List.iter 
    (fun (kind, tok) -> Hashtbl.add ref_kind_table kind tok)
    [ ("val", RK_value);
      ("type", RK_type);
      ("exception", RK_exception);
      ("module", RK_module);
      ("modtype", RK_module_type);
      ("class", RK_class);
      ("classtype", RK_class_type);
      ("attribute", RK_attribute);
      ("method", RK_method);
      ("section", RK_section);
      ("recfield", RK_recfield);
      ("const", RK_const); ]

(* Hash table of tags *)
let tag_table = Hashtbl.create 19
let _ =
  List.iter 
    (fun (tag, tok) -> Hashtbl.add tag_table tag tok)
    [ ("author", AUTHOR);
      ("deprecated", DEPRECATED);
      ("return", RETURN);
      ("since", SINCE); ]

}

let newline = ('\010' | "\013\010" )
let blank = [' ' '\009' '\012']

let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let versionchar = 
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' 
   '0'-'9' '+' '-' '.' '/' ':' '<' '=' '>' '?' '^' '~' ]

let ident = identchar+
let version = versionchar+
let uident = uppercase identchar*
let exception = uident ('.' uident)*

(* The characters which are not the start of any tokens other than Char *)
let safe = [^ '\010' '\013' '\\' '{' '}' '[' ']' '<' 'v' '%' '@']
let safe_not_blank = [^ ' ' '\009' '\012' '\010' '\013' '\\' '{' '}' '[' ']' '<' 'v' '%' '@']

let escape = '\\' (['{' '}' '[' ']' '@'] as chr)

let tag = '@'

let begin = '{'
let end = '}'

let alpha = ['a'-'z''A'-'Z']
let digit = ['0' - '9']
let letter = ['a'-'z''A'-'Z''0'-'9' '-' '_'] 

let label = alpha letter*
let op = ['-' '_' '^' ]

let item = '-'
let superscript = '^'
let subscript = '_'

let verb = 'v' 

let target = '%'
let target_format = (letter+ as _fmt) ':'

let begin_code = "[" 
let end_code = "]" 

let begin_pre_code = "{["
let end_pre_code = "]}"

let link = ':'

let ref = '!'
let ref_kind = (label as _kind) ':'

let title = digit+ as _num
let title_label =  ":" (label as _lbl)

(* Shortcut format for lists *)

let shortcut_list_item = newline (blank* '-' blank) as _line
let shortcut_enum_item = newline (blank*'+' blank) as _line

(* html marks, to use as alternative markup *)

let html_bold = '<' ['b' 'B'] as _tag '>'
let html_end_bold = "</" ['b' 'B'] '>'
let html_italic = '<' ['i' 'I']  as _tag '>'
let html_end_italic = "</" ['i' 'I'] '>'
let html_title = '<' (['h' 'H'](['0'-'9']+ as num)) as _tag '>'
let html_end_title = "</" ['h' 'H'](['0'-'9']+ as num) '>'
let html_list = '<' (['u' 'U']['l' 'L']) as _tag '>'
let html_end_list = "</" ['u' 'U']['l' 'L'] '>'
let html_enum = '<' (['o' 'O']['l' 'L']) as _tag '>'
let html_end_enum = "</" ['o' 'O']['l' 'L'] '>'
let html_item = '<' (['l' 'L']['i' 'I']) as _tag '>'
let html_end_item = "</" ['l' 'L']['i' 'I'] '>'
let html_code = '<' ['c' 'C']['o' 'O']['d' 'D']['e' 'E'] '>'
let html_end_code = "</" ['c' 'C']['o' 'O']['d' 'D']['e' 'E'] '>'
let html_center = '<' (['c' 'C']['e' 'E']['n' 'N']['t' 'T']['e' 'E']['r' 'R']) as _tag '>'
let html_end_center = "</" ['c' 'C']['e' 'E']['n' 'N']['t' 'T']['e' 'E']['r' 'R'] '>'
let html_left = '<' (['l' 'L']['e' 'E']['f' 'F']['t' 'T']) as _tag '>'
let html_end_left = "</" ['l' 'L']['e' 'E']['f' 'F']['t' 'T'] '>'
let html_right = '<' (['r' 'R']['i' 'I']['g' 'G']['h' 'H']['t' 'T']) as _tag '>'
let html_end_right = "</" ['r' 'R']['i' 'I']['g' 'G']['h' 'H']['t' 'T'] '>'

rule main = parse
| escape 
    { Char (String.make 1 chr) }
| tag (ident as tag)
    { match tag with
      | "see" -> see_tag lexbuf
      | "before" -> before_tag lexbuf
      | "param" -> param_tag lexbuf
      | "raise" -> raise_tag lexbuf
      | "version" -> version_tag lexbuf
      | _ ->
        try
          Hashtbl.find tag_table tag
        with Not_found -> Custom tag }
| begin                   
    { BEGIN }
| end
    { END }
| begin verb newline
    { incr_line lexbuf;
      reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      verb lexbuf }
| begin verb blank?
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      verb lexbuf }
| newline verb end
    { incr_line ~chars:2 lexbuf;
      raise (Error (Lexer Unmatched_verbatim, Location.curr lexbuf)) }
| blank? verb end
    { raise (Error (Lexer Unmatched_verbatim, Location.curr lexbuf)) }
| begin target target_format newline
    { incr_line lexbuf;
      reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      target_format := Some _fmt;
      target lexbuf }
| begin target target_format blank?
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      target_format := Some _fmt;
      target lexbuf }
| begin target newline
    { incr_line lexbuf;
      reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      target_format := None;
      target lexbuf }
| begin target blank?
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      target_format := None;
      target lexbuf }
| newline target end
    { incr_line ~chars:2 lexbuf;
      raise (Error (Lexer Unmatched_target, Location.curr lexbuf)) }
| blank? target end
    { raise (Error (Lexer Unmatched_target, Location.curr lexbuf)) }
| begin_code end_code
    { Char (Lexing.lexeme lexbuf) }
| begin_code
    { reset_string_buffer ();
      code_start_locs := [Location.curr lexbuf];
      code lexbuf }
| end_code
    { raise (Error (Lexer Unmatched_code, Location.curr lexbuf)) }
| begin_pre_code end_pre_code
    { Char (Lexing.lexeme lexbuf) }
| begin_pre_code
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      pre_code lexbuf }
| end_pre_code
    { raise (Error (Lexer Unmatched_pre_code, Location.curr lexbuf)) }
| begin ref newline
    { incr_line lexbuf;
      reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      ref_kind := RK_element;
      reference lexbuf }
| begin ref blank?
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      ref_kind := RK_element;
      reference lexbuf }
| begin ref (label as lbl) end
    { if lbl = "indexlist" then Special_Ref SRK_index_list
      else Ref(RK_element, lbl) }
| begin ref ref_kind newline
    { incr_line lexbuf;
      reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      if _kind = "modules" then begin
        reset_module_list ();
        module_list lexbuf
      end else begin
        let kind = 
          try
            Hashtbl.find ref_kind_table _kind
          with Not_found -> RK_custom _kind
        in
          ref_kind := kind;
          reference lexbuf
      end }
| begin ref ref_kind blank?
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      if _kind = "modules" then begin
        reset_module_list ();
        module_list lexbuf
      end else begin
        let kind = 
          try
            Hashtbl.find ref_kind_table _kind
          with Not_found -> RK_custom _kind
        in
          ref_kind := kind;
          reference lexbuf
      end }
| begin link
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      ref_kind := RK_link;
      reference lexbuf }
| begin title title_label? newline
    { Title (int_of_string _num, _lbl) }
| begin title title_label? blank?
    { Title (int_of_string _num, _lbl) }
| begin (label as style) newline
    { incr_line lexbuf;
      try
        Hashtbl.find style_table style
      with Not_found -> Style (SK_custom style) }
| begin (label as style) blank?
    { try
        Hashtbl.find style_table style
      with Not_found -> Style (SK_custom style) }
| begin item newline
    { incr_line lexbuf;
      Item true }
| begin item blank? { Item true }
| begin superscript newline
    { incr_line lexbuf;
      Style SK_superscript }
| begin superscript blank? { Style SK_superscript }
| begin subscript newline
    { incr_line lexbuf;
      Style SK_subscript }
| begin subscript blank? { Style SK_subscript }
| html_code
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      html_code lexbuf }
| html_end_code
    { raise (Error (Lexer Unmatched_html_code, Location.curr lexbuf)) }
| html_title
    { HTML_Title(_tag, int_of_string num) }
| html_end_title
    { HTML_END_Title (int_of_string num) }
| html_bold
    { HTML_Bold _tag}
| html_end_bold
    { HTML_END_BOLD }
| html_italic
    { HTML_Italic _tag}
| html_end_italic
    { HTML_END_ITALIC }
| html_center
    { HTML_Center _tag}
| html_end_center
    { HTML_END_CENTER }
| html_left
    { HTML_Left _tag}
| html_end_left
    { HTML_END_LEFT }
| html_right
    { HTML_Right _tag}
| html_end_right
    { HTML_END_RIGHT }
| html_list
    { HTML_List _tag}
| html_end_list
    { HTML_END_LIST }
| html_enum
    { HTML_Enum _tag}
| html_end_enum
    { HTML_END_ENUM }
| html_item
    { HTML_Item _tag}
| html_end_item
    { HTML_END_ITEM }
| shortcut_list_item
    { incr_line ~chars:(String.length _line) lexbuf;
      SHORTCUT_LIST_ITEM }
| shortcut_enum_item
    { incr_line ~chars:(String.length _line) lexbuf;
      SHORTCUT_ENUM_ITEM }
| newline blank* newline
    { incr_line ~lines:2 lexbuf;
      BLANK_LINE }
| newline blank* newline ((blank* newline)+ as extra)
    { let lines = 2 + (count_newlines extra) in
      incr_line ~lines lexbuf;
      BLANK_LINE }
| newline           
    { incr_line lexbuf;
      Char (Lexing.lexeme lexbuf) }
| safe+ | _
    { Char (Lexing.lexeme lexbuf) }
| eof                    { EOF }

and see_tag = parse
| blank+
    { see_tag lexbuf }
| newline
    { incr_line lexbuf;
      see_tag lexbuf }
| '<'
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      see_tag_url lexbuf }
| '\''
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      see_tag_file lexbuf }
| '"'
    { reset_string_buffer ();
      string_start_loc := Location.curr lexbuf;
      see_tag_doc lexbuf }
| _
    { raise (Error (Lexer Expected_see, Location.curr lexbuf)) }

and see_tag_url = parse
| '>'
    { See (See_url (get_buffered_string ())) }
| eof
    { raise (Error (Lexer Unterminated_see_url, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_tag_url lexbuf }
| [^ '>' '\010' '\013' ]+ | _
    { buffer_lexeme lexbuf; 
      see_tag_url lexbuf }

and see_tag_file = parse
| '\\' '\'' 
    { buffer_char '\''; verb lexbuf }
| '\''
    { See (See_file (get_buffered_string ())) }
| eof
    { raise (Error (Lexer Unterminated_see_file, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_tag_file lexbuf }
| [^ '\'' '\\' '\010' '\013' ]+ | _
    { buffer_lexeme lexbuf; 
      see_tag_file lexbuf }

and see_tag_doc = parse
| '\\' '\"' 
    { buffer_char '\''; verb lexbuf }
| '\"'
    { See (See_doc (get_buffered_string ())) }
| eof
    { raise (Error (Lexer Unterminated_see_doc, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      see_tag_doc lexbuf }
| [^ '\"' '\\' '\010' '\013' ]+ | _
    { buffer_lexeme lexbuf; 
      see_tag_doc lexbuf }

and param_tag = parse
| blank+
    { param_tag lexbuf }
| newline
    { incr_line lexbuf;
      param_tag lexbuf }
| (ident as id) newline
    { incr_line lexbuf;
      Param id }
| (ident as id) blank*
    { Param id }
| _
    { raise (Error (Lexer Expected_ident, Location.curr lexbuf)) }

and before_tag = parse
| blank+
    { before_tag lexbuf }
| newline
    { incr_line lexbuf;
      before_tag lexbuf }
| (version as ver) newline
    { incr_line lexbuf;
      Before ver }
| (version as ver) blank*
    { Before ver }
| _
    { raise (Error (Lexer Expected_version, Location.curr lexbuf)) }

and version_tag = parse
| blank+
    { version_tag lexbuf }
| newline
    { incr_line lexbuf;
      version_tag lexbuf }
| (version as ver) newline
    { incr_line lexbuf;
      Version ver }
| (version as ver) blank*
    { Version ver }
| _
    { raise (Error (Lexer Expected_version, Location.curr lexbuf)) }

and raise_tag = parse
| blank+
    { raise_tag lexbuf }
| newline
    { incr_line lexbuf;
      raise_tag lexbuf }
| (exception as exn) newline
    { incr_line lexbuf;
      Raise exn }
| (exception as exn) blank*
    { Raise exn }
| _
    { raise (Error (Lexer Expected_exception, Location.curr lexbuf)) }

and verb = parse
| escape 
    { buffer_char chr; verb lexbuf }
| begin verb newline
    { incr_line lexbuf;
      raise (Error (Lexer Nested_verbatim, Location.curr lexbuf)) }
| begin verb blank?
    { raise (Error (Lexer Nested_verbatim, Location.curr lexbuf)) }
| newline verb end
    { incr_line ~chars:2 lexbuf;
      Verb (get_buffered_string ()) }
| blank? verb end
    { Verb (get_buffered_string ()) }
| eof
    { raise (Error (Lexer Unterminated_verbatim, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      verb lexbuf }
| safe+ | _
    { buffer_lexeme lexbuf; verb lexbuf }

and target = parse
| escape 
    { buffer_char chr; target lexbuf }
| begin target target_format? newline
    { incr_line lexbuf;
      raise (Error (Lexer Nested_target, Location.curr lexbuf)) }
| begin target target_format? blank?
    { raise (Error (Lexer Nested_target, Location.curr lexbuf)) }
| newline target end
    { incr_line ~chars:2 lexbuf;
      Target(!target_format, get_buffered_string ()) }
| blank? target end
    { Target(!target_format, get_buffered_string ()) }
| eof
    { raise (Error (Lexer Unterminated_target, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      target lexbuf }
| safe+ | _
    { buffer_lexeme lexbuf; target lexbuf }

and code = parse
| escape 
    { buffer_char chr; code lexbuf }
| begin_code
    { code_start_locs := (Location.curr lexbuf) :: !code_start_locs;
      buffer_lexeme lexbuf;
      code lexbuf }
| end_code
    { match !code_start_locs with
        [] -> assert false
      | [l] ->
        code_start_locs := [];
        Code(get_buffered_string ())
      | _ :: rest ->
        code_start_locs := rest;
        buffer_lexeme lexbuf;
        code lexbuf }
| eof
    { match !code_start_locs with
        [] -> assert false
      | l:: _ -> raise (Error (Lexer Unterminated_code, l)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      code lexbuf }
| safe+ | _
    { buffer_lexeme lexbuf; code lexbuf }

and pre_code = parse
| escape 
    { buffer_char chr; pre_code lexbuf }
| begin_pre_code
    { raise (Error (Lexer Nested_pre_code, Location.curr lexbuf)) }
| end_pre_code
    { Pre_Code (get_buffered_string ()) }
| eof
    { raise (Error (Lexer Unterminated_pre_code, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      pre_code lexbuf }
| safe+ | _
    { buffer_lexeme lexbuf; pre_code lexbuf }

and html_code = parse
| escape 
    { buffer_char chr; html_code lexbuf }
| html_code
    { raise (Error (Lexer Nested_html_code, Location.curr lexbuf))  }
| html_end_code
    { Code(get_buffered_string ()) }
| eof 
    { raise (Error (Lexer Unterminated_html_code, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      html_code lexbuf }
| safe+ | _
    { buffer_lexeme lexbuf; html_code lexbuf }

and reference = parse
| escape 
    { buffer_char chr; reference lexbuf }
| end
    { Ref(!ref_kind, get_buffered_string ()) }
| eof
    { raise (Error (Lexer Unterminated_ref, !string_start_loc)) }
| newline
    { incr_line lexbuf;
      buffer_lexeme lexbuf;
      reference lexbuf }
| safe+ | _
    { buffer_lexeme lexbuf; reference lexbuf }

and module_list = parse
| escape 
    { buffer_char chr; reference lexbuf }
| end
    { if buffer_not_empty () then add_module (get_buffered_string ());
      Special_Ref(SRK_module_list (get_module_list ())) }
| eof
    { raise (Error (Lexer Unterminated_ref, !string_start_loc)) }
| blank+
    { if buffer_not_empty () then begin
        add_module (get_buffered_string ());
        reset_string_buffer ()
      end;
      reference lexbuf }
| newline
    { incr_line lexbuf;
      if buffer_not_empty () then begin
        add_module (get_buffered_string ());
        reset_string_buffer ()
      end;
      reference lexbuf }
| safe_not_blank+ | _
    { buffer_lexeme lexbuf; reference lexbuf }

