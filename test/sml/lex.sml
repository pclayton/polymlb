structure L = PolyMLB.Lex

datatype z = datatype L.token

fun pos (a, b, c, d) =
  { file = ""
  , startLine = a, startPosition = b
  , endLine = c, endPosition = d
  }

val lex = map #1 o #1 o L.lex "";

"Lex.makePos works correctly"
assert
  L.toPolyLoc
    ("", L.makePos { startLine = 0, startCol = 12, endLine = 56, endCol = 2 })
eq
  { file = ""
  , startLine = 0, startPosition = 12
  , endLine = 56,  endPosition = 2
  };

"Lex.joinPos works correctly"
assert
  L.toPolyLoc ("", L.joinPos
    ( L.makePos { startLine = 8, startCol = 3,  endLine = 56, endCol = 2  }
    , L.makePos { startLine = 0, startCol = 12, endLine = 32, endCol = 29 }
    ))
eq
  { file = ""
  , startLine = 0, startPosition = 12
  , endLine = 56,  endPosition = 2
  };

"Lex.lex recognizes all tokens"
assert
  lex "\"x\" y and ann bas basis end = functor in let local open ; signature \
  \structure"
eq
  [ String "x", Symbol "y", And, Ann, Bas, Basis, End, Eq, Functor, In, Let
  , Local, Open, Semi, Signature, Structure
  ];

"Lex.lex splits non space separated tokens"
assert
  lex "Foo=Bar;local\"baz\""
eq
  [Symbol "Foo", Eq, Symbol "Bar", Semi, Local, String "baz"];

fun lex s () = L.lex "" s;

"Lex.lex raises on unclosed comment"
assert lex "(* (*\n *)" raisesExact L.Lex (L.UnclosedComment, pos (1, 1, 2, 4));

app
  (fn s =>
    "Lex.lex raises on invalid reserved word '" ^ s ^ "'"
    assert lex s raisesExact L.Lex (L.BadWord s, pos (1, 1, 1, 1 + size s)))
  [ "abstype", "andalso", "as", "case", "datatype", "do", "else", "exception"
  , "fn", "fun", "handle" , "if", "infix", "infixr", "nonfix", "of", "op"
  , "orelse", "raise", "rec", "sig", "struct", "then", "type", "val", "with"
  , "withtype", "while"
  ];

app
  (fn c =>
    "Lex.lex raises on bad char '" ^ str c ^ "'"
    assert lex (str c) raisesExact L.Lex (L.BadChar c, pos (1, 1, 1, 2)))
  [#"@", #",", #":", #"[", #"{", #"}", #"]", #"\\"];

"Lex.lex raises on unclosed before new line string"
assert lex "\"abc\n\"" raisesExact L.Lex (L.UnclosedString, pos (1, 1, 1, 5));

"Lex.lex raises on unclosed string"
assert lex "\"abc" raisesExact L.Lex (L.UnclosedString, pos (1, 1, 1, 4));
