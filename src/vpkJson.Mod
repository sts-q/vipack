MODULE Json;   (* :file: Json.Mod  --  A simple Json parser. *)
(* IMPORT DDX;  (* DDX: only for sts-q style debugging, to be removed/replaced in production *) *)

(** A simple Json parser and printer.
https://www.json.org/json-en.html
https://ecma-international.org/wp-content/uploads/ECMA-404_2nd_edition_december_2017.pdf

Limitations:
 * Size of json string values and names is fixed.
 * No floating point numbers.
 * Integer values of size signed 64 bit.
 * No strings escape characters like \n, currently.
 * Return NIL and provide an error message in case of malformed json.

 W I P

 TODO
        - buffer overflow check
        - integer overflow check
        - ints 64 bit for all json ints
        - ?

 *)


(* ------------------------------------------------------------------------- *)
(* :section:    DECLARATIONS *)
CONST
   IndentSize  =   4;                   (* 'tabsize' for indentation of output *)
   jStringSize = 256;                   (* for both: element-names and string-values *)


TYPE
   jInt    = LONGINT;                    (* Json number values: 64 bit signed integer *)
   jString = ARRAY jStringSize OF CHAR;  (* Json string values: name|value *)


   Value*  = POINTER TO ValueDesc;
   Bool*   = POINTER TO BoolDesc;
   Int*    = POINTER TO IntDesc;
   Null*   = POINTER TO NullDesc;
   Str*    = POINTER TO StrDesc;
   Arr*    = POINTER TO ArrDesc;
   Obj*    = POINTER TO ObjDesc;

   ValueDesc  = RECORD END;
   BoolDesc   = RECORD( ValueDesc )  b*   : BOOLEAN     END;
   IntDesc    = RECORD( ValueDesc )  i*   : INTEGER     END;
   NullDesc   = RECORD( ValueDesc )                     END;
   StrDesc    = RECORD( ValueDesc )  str* : jString     END;

   ArrDesc    = RECORD( ValueDesc )
                   value* : Value;
                   next*  : Arr;
                END;

   ObjDesc    = RECORD( ValueDesc )
                   name*  : jString;
                   value* : Value;
                   next*  : Obj;
               END;


VAR
   src     : POINTER TO ARRAY OF CHAR;          (* copy of json source *)
   cc      : CHAR;                              (* CurrentChar: cc := source[srcPos] *)
   srcPos  : INTEGER;
   srcLine : INTEGER;                           (* current line in source *)
   srcCol  : INTEGER;                           (* current column in current line *)
   nesting : INTEGER;
   noError : BOOLEAN;                           (* noError = TRUE --> keep going *)
   errmess : ARRAY 112 OF CHAR;
   currentIndentation : INTEGER;                (* to format output *)


(* ------------------------------------------------------------------------- *)
(* :section:    lib *)
PROCEDURE ConsMember (VAR obj: Obj; k: ARRAY OF CHAR; v: Value);
VAR
   n   : Obj;
BEGIN
   NEW (n);
   n.name  := k;
   n.value := v;
   n.next  := obj;
   obj := n;
END ConsMember;

PROCEDURE ConsSsp (VAR obj: Obj; k, v: ARRAY OF CHAR);
(** Cons string-string-pair *)
VAR val: Str;
BEGIN
   NEW (val);
   val.str := v;
   ConsMember (obj, k, val);
END ConsSsp;

PROCEDURE ReverseArr(l: Arr): Arr;
  VAR np,nl : Arr;
BEGIN
  nl := NIL;
  WHILE l # NIL DO
    NEW( np );
    np.value := l.value;
    np.next  := nl;
    nl := np;
    l  := l.next;
  END;
  RETURN nl
END ReverseArr;

PROCEDURE ReverseObj(l: Obj): Obj;
  VAR np,nl : Obj;
BEGIN
  nl := NIL;
  WHILE l # NIL DO
    NEW( np );
    np.name  := l.name;
    np.value := l.value;
    np.next  := nl;
    nl := np;
    l  := l.next;
  END;
  RETURN nl
END ReverseObj;

PROCEDURE NextCC;
BEGIN
   IF cc # 0X THEN
      IF cc = 0AX THEN
         INC (srcLine);
         srcCol := 1;
      ELSE
         INC (srcCol);
      END;
      INC (srcPos);
      cc := src[srcPos];
   END;
END NextCC;

PROCEDURE Error (mess: ARRAY OF CHAR);
BEGIN
   COPY (mess, errmess);
   cc      := 0X;
   noError := FALSE;
END Error;

PROCEDURE SkipSpaces;
BEGIN
   WHILE   (cc # 0X)
         & ((cc = ' ') OR (cc = 0AX) OR (cc = 0DX) OR (cc = 09X))
   DO NextCC
   END;
END SkipSpaces;

PROCEDURE ShowInt(VAR s: ARRAY OF CHAR; n: jInt);
(** Write ascii representation of n to s. *)
CONST
  sa = 32;
VAR
  i, j, d: jInt;
  neg: BOOLEAN;
  r: ARRAY sa OF CHAR;
BEGIN
  (* store string at tail of s *)
  i := sa;   (* i points full *)
  DEC(i);
  s[i] := 0X;

  IF n = 0 THEN
    (* s := "0" *)
    COPY("0", s);
  ELSIF n = -2147483647 THEN
    COPY("-2147483647", s)
  ELSIF n = 2147483648 THEN
    COPY("2147483648", s)
  ELSE

    IF n < 0 THEN
      neg := TRUE;
      n := -n
    ELSE
      neg := FALSE
    END;

    WHILE n > 0 DO
      d := n MOD 10;
      n := n DIV 10;
      DEC( i );
      r[i] := CHR(d + 48);
    END;

    IF neg THEN
      DEC(i);
      r[i] := "-"
    END;

    j := 0;
    WHILE j < sa - i DO
      s[j] := r[i+j];
      INC(j)
    END;
    s[j-1] := 0X;

  END;
END ShowInt;


(* ------------------------------------------------------------------------- *)
(* :section:    push to buffer *)
PROCEDURE Indent (delta: INTEGER);
BEGIN
   INC (currentIndentation, delta * IndentSize);
END Indent;

PROCEDURE PushChr (VAR buffer: ARRAY OF CHAR; VAR pos: INTEGER; c: CHAR);
BEGIN
   ASSERT (pos < LEN (buffer), 201);
   buffer[pos] := c;
   INC (pos);
END PushChr;

PROCEDURE PushNewline (VAR buffer: ARRAY OF CHAR; VAR pos: INTEGER);
VAR i : INTEGER;
BEGIN
   PushChr (buffer, pos, 0AX);
   FOR i := 1 TO currentIndentation DO PushChr (buffer, pos, ' ') END;
END PushNewline;

PROCEDURE PushText (VAR buffer: ARRAY OF CHAR; VAR pos: INTEGER; str: ARRAY OF CHAR);
VAR i : INTEGER;
BEGIN
   i := 0;
   WHILE str[i] # 0X DO
      PushChr (buffer, pos, str[i]);
      INC (i)
   END;
END PushText;

PROCEDURE PushStr (VAR buffer: ARRAY OF CHAR; VAR pos: INTEGER; str: ARRAY OF CHAR);
VAR i : INTEGER;
BEGIN
   PushChr  (buffer, pos, CHR(34));
   PushText (buffer, pos, str);
   PushChr  (buffer, pos, CHR(34));
END PushStr;

PROCEDURE PushInt (VAR buffer: ARRAY OF CHAR; VAR pos: INTEGER; n: INTEGER);
VAR s: ARRAY 32 OF CHAR;
BEGIN
   ShowInt  (s, n);
   PushText (buffer, pos, s);
END PushInt;


(* ------------------------------------------------------------------------- *)
(* :section:    format *)
PROCEDURE DoFormat (VAR buffer: ARRAY OF CHAR; VAR pos: INTEGER; ob: Value);
VAR itm: Value;
BEGIN
   IF ob = NIL THEN
      PushStr (buffer, pos, "<NIL>")
   ELSE
      itm := ob;
      WITH
        itm: Str  DO PushStr  (buffer, pos, itm.str)
      | itm: Bool DO IF itm.b THEN PushText(buffer, pos, "TRUE") ELSE PushText(buffer, pos,"FALSE") END
      | itm: Int  DO PushInt  (buffer, pos, itm.i)
      | itm: Null DO PushText (buffer, pos, "NULL")
      | itm: Arr  DO
                PushChr (buffer, pos, '[');
                Indent (+1);
                WHILE itm # NIL DO
                   DoFormat (buffer, pos, itm.value);
                   IF itm.next # NIL THEN PushText (buffer, pos, ", ") END;
                   itm := itm.next;
                END;
                Indent (-1);
                (* PushNewline (buffer, pos); *)
                PushChr     (buffer, pos, ']');
      | itm: Obj  DO
                PushChr (buffer, pos, '{');
                Indent  (+1);
                WHILE itm # NIL DO
                   PushNewline (buffer, pos);
                   PushStr  (buffer, pos, itm.name);
                   PushChr  (buffer, pos, ':');
                   PushChr  (buffer, pos, ' ');
                   DoFormat (buffer, pos, itm.value);
                   IF itm.next # NIL THEN PushChr (buffer, pos, ',') END;
                   itm := itm.next;
                END;
                Indent (-1);
                PushNewline (buffer, pos);
                PushChr     (buffer, pos, '}')
      ELSE ASSERT (noError # noError, 200)
      END;
   END
END DoFormat;

PROCEDURE Format* (VAR buffer: ARRAY OF CHAR; ob: Value): BOOLEAN;
VAR pos : INTEGER;
BEGIN
   (* DDX.LMess ("Start formating..."); *)
   pos := 0;
   DoFormat     (buffer, pos, ob);
   PushNewline  (buffer, pos);
   PushChr      (buffer, pos, 0X);
   RETURN TRUE;
END Format;

PROCEDURE FormatError* (VAR mess: ARRAY OF CHAR);
VAR pos: INTEGER;
BEGIN
   pos := 0;
   PushInt (mess, pos, srcPos);  PushText (mess, pos, ":");
   PushInt (mess, pos, srcLine); PushText (mess, pos, ":");
   PushInt (mess, pos, srcCol);  PushText (mess, pos, "  (pos:line:col)  ");
   PushText(mess, pos, errmess);
END FormatError;


(* ------------------------------------------------------------------------- *)
(* :section:    parse json *)
PROCEDURE ReadString (VAR s: ARRAY OF CHAR);
VAR
   start, stop, len, i: INTEGER;
BEGIN
   ASSERT (cc = '"', 100);
   NextCC;
   start := srcPos;
   WHILE (cc # 0X) & (cc # '"') DO NextCC END;
   stop := srcPos;
   len  := stop - start;
   ASSERT (len < LEN (s), 200);                 (* terminating 0X required *)
   NextCC; SkipSpaces;
   FOR i := 0 TO len - 1 DO  s[i] := src[start + i] END;
   s[len] := 0X;
   (* DDX.LMess (s); *)
END ReadString;

PROCEDURE ReadStringValue (VAR value: Value);
VAR
   s   : jString;
   str : Str;
BEGIN
   ReadString (s);
   NEW (str); str.str := s;
   value := str;
END ReadStringValue;


(* ------------------------------------------------------------------------- *)
(* :section:    ReadOthers *)
PROCEDURE IsValid (c: CHAR): BOOLEAN;
(** valid character for: true, false, null, integer *)
BEGIN
   RETURN    ('a' <= cc) & (cc <= 'z')
          OR ('A' <= cc) & (cc <= 'Z')
          OR ('0' <= cc) & (cc <= '9')
          OR (cc = '+') OR (cc = '-')
          OR (cc = '_')                         (* 1_000_000 *)
END IsValid;

PROCEDURE IsInt (VAR s: ARRAY OF CHAR): BOOLEAN;
VAR
   i   : INTEGER;
   c   : CHAR;
BEGIN
   IF (s[0] = '+') OR (s[0] = '-')
   THEN
      IF s[1] = 0X THEN RETURN FALSE END;       (* found "+" or "-" *)
      i := 1
   ELSE i := 0;
   END;
   REPEAT c := s[i]; INC (i);
   UNTIL (c = 0X) OR ((c < '0') OR ('9' < c)) & (c # '_');
   RETURN c = 0X;
END IsInt;

PROCEDURE ParseInt (VAR s: ARRAY OF CHAR): INTEGER;
VAR
   res,i,sign : INTEGER;
   c : CHAR;
BEGIN
   res := 0;
   IF    s[0] = '+' THEN sign :=  1; i := 1
   ELSIF s[0] = '-' THEN sign := -1; i := 1
   ELSE sign := 1; i := 0
   END;
   REPEAT
      c := s[i]; INC (i);
      IF (c # 0X) & (c # '_') THEN res := 10 * res + ORD(c) - 48 END;
   UNTIL c = 0X;
   RETURN sign * res;
END ParseInt;


PROCEDURE ReadOthers (VAR value: Value);
VAR
   s : ARRAY jStringSize OF CHAR;
   start, stop, len, i: INTEGER;
   bool : Bool;
   null : Null;
   int  : Int;
BEGIN
   start := srcPos;
   WHILE (cc # 0X) & IsValid (cc) DO NextCC END;
   stop := srcPos;
   len  := stop - start;
   FOR i := 0 TO len - 1 DO s[i] := src[start + i] END;
   s[len] := 0X;
   IF    (s = "true")  OR (s = "TRUE")  THEN NEW (bool); bool.b := TRUE;  value := bool
   ELSIF (s = "false") OR (s = "FALSE") THEN NEW (bool); bool.b := FALSE; value := bool
   ELSIF (s = "null")  OR (s = "NULL")  THEN NEW (null);                  value := null
   ELSIF IsInt (s)                      THEN NEW (int);  int.i := ParseInt (s); value := int
   ELSE Error ("ReadOthers: strange item found"); value := NIL
   END;
   (* DDX.LMess (s); *)
   (* DDX.Lf; *)
END ReadOthers;


(* ------------------------------------------------------------------------- *)
(* :section:    parse *)
PROCEDURE ReadValue (VAR value: Value);
        PROCEDURE ReadArray (VAR value: Value);
        VAR
           arr: Arr;
           mem: Arr;
           v  : Value;
        BEGIN
           ASSERT (cc = '[');
           (* DDX.C ("["); *)
           NEW (arr); arr := NIL;
           NextCC;
           REPEAT
              SkipSpaces;
              ReadValue (v);
              IF cc = ',' THEN NextCC END;
              NEW (mem);
              mem.value := v;
              mem.next  := arr;
              arr       := mem;
              IF cc = '}' THEN Error ("']' or ',' expected. Got '}'") END;
           UNTIL (cc = 0X) OR (cc = ']') OR ~noError;
           NextCC; SkipSpaces;
           value := ReverseArr (arr);
           (* DDX.C ("]"); *)
        END ReadArray;

        PROCEDURE ReadObject (VAR value: Value);
        VAR
           obj,elt: Obj;
           name   : jString;
           v      : Value;
        BEGIN
           NEW (obj); obj := NIL;
           NextCC;
           REPEAT
              (* DDX.C ("."); *)
              NEW (elt);
              SkipSpaces;
              IF cc # '"' THEN Error ("Element name expected."); RETURN END;
              ReadString(name);
              IF cc # ':' THEN Error ("':' expected"); RETURN END;
              NextCC; SkipSpaces;
              ReadValue (v); SkipSpaces;
              IF cc = ',' THEN NextCC; SkipSpaces END;
              elt.name  := name;
              elt.value := v;
              elt.next  := obj;
              obj := elt;
              (* DDX.C (","); *)
              IF cc = ']' THEN Error ("'}' or ',' expected. Got ']'") END;
           UNTIL (cc = 0X) OR (cc = '}') OR ~noError;
           NextCC; SkipSpaces;
           value := ReverseObj (obj);
        END ReadObject;

BEGIN
   IF cc = 0X THEN RETURN END;
   SkipSpaces;
   IF    FALSE          THEN
   ELSIF cc = CHR(34)   THEN ReadStringValue (value)
   ELSIF cc = '['       THEN ReadArray  (value)
   ELSIF cc = '{'       THEN ReadObject (value)
   ELSIF IsValid (cc)   THEN ReadOthers (value)
   ELSE Error ("Strange item found.")
   END;
END ReadValue;


(* ------------------------------------------------------------------------- *)
(* :section:    main *)
PROCEDURE Parse* (VAR value: Value; VAR jsonSource: ARRAY OF CHAR): BOOLEAN;
VAR res : BOOLEAN;
BEGIN
   NEW  (src, LEN(jsonSource));
   COPY (jsonSource, src^);
   cc      := src[0];
   srcPos  := 0;
   srcCol  := 1;
   srcLine := 1;
   nesting := 1;
   noError := TRUE;
   value   := NIL;

   ReadValue (value);

   IF noError THEN
      (* DDX.LMess ("Parsing succeded."); *)
      (* DDX.Lf; *)
      res := TRUE
   ELSE
      (* DDX.LMess ("Parsing failed."); *)
      (* DDX.Lf; *)
      value := NIL;
      res   := FALSE
   END;

   RETURN res;
END Parse;


BEGIN (* MAIN *)
   currentIndentation := 0;
   COPY ("Nothing to complain about, so far.", errmess);
   srcPos  := 0;
   srcLine := 1;
   srcCol  := 1;
END Json.
(* :file: END (c) sts-q 2024-Feb *)

