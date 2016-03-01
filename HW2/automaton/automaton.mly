%token IF ELSE NULL
%start s
%type <int> s

%%

s : IF s        { 0 }
  | NULL        { 0 }
t : /* empty */ { 0 }
  | ELSE s      { 0 }