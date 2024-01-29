grammar Function;
prog         : function* EOF;
function     : modifier* type NAME LB_ROUND parameters? RB_ROUND block;
modifier     : MODIFIER;
type         : TYPE;
parameters   : parameter (COMMA parameter)*;
parameter    : type NAME;
block        : LB_CURLY statement* RB_CURLY;
statement    : return | assignment | if_statement | while_loop | block;
if_statement : IF condition block (ELIF condition block)* (ELSE block)?;
while_loop   : WHILE condition block;
condition    : LB_ROUND (literal | NAME) (EQUALS (literal | NAME))? RB_ROUND;
assignment   : type? NAME ASSIGN literal SEMICOLON;
return       : RETURN (literal | NAME)? SEMICOLON;
literal      : NUMBER | CHAR | BOOLEAN | STRING;
MODIFIER  : 'public' | 'private' | 'protected' | 'final' | 'static' | 'abstract';
TYPE      : 'void' | 'int' | 'char' | 'bool' | 'String';
RETURN    : 'return';
IF        : 'if';
ELIF      : 'else if';
ELSE      : 'else';
WHILE     : 'while';
BOOLEAN   : 'true' | 'false';
NAME      : [a-zA-Z_][a-zA-Z0-9_]*;
NUMBER    : ('-')? [0-9]+ ;
CHAR      : '\'' ( ESC | ~[\r\n'] ) '\'';
STRING    : '"' ( ESC | ~[\r\n"] )* '"';
LB_ROUND  : '(';
RB_ROUND  : ')';
LB_CURLY  : '{';
RB_CURLY  : '}';
SEMICOLON : ';';
COMMA     : ',';
ASSIGN    : '=';
EQUALS    : '==';
NEWLINE   : [ \r\n]+ -> skip;
fragment ESC : '\\' [btnfr"'\\];