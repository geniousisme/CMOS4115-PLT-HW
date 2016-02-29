%token NEWLINE
%token <float> FLOAT

%start expr
%type <float> expr

%%

expr: 
	FLOAT NEWLINE { $1 }
