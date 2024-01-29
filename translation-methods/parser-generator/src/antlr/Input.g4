grammar Input;
prog : rule+ term+ code?;
rule : NT_NAME inh? synt? COL part (OR part)* ;
inh : ATTR ;
synt : ARR ATTR ;
part : 'eps' #eps | nonTermRule+ #nonEps;
nonTermRule : CODE #ruleCode | NT_NAME inh? #ruleNonTerm | T_NAME #ruleTerm;
term : T_NAME '=' REGEX ;
code : EXTRA_CODE;
NT_NAME : [a-z]+ ;
T_NAME : [A-Z][A-Z0-9]* ;
ATTR : '[' .*? ']' ;
CODE : '{' .*? '}' ;
REGEX : '"' .*? '"' ;
ARR : '->' ;
COL : ':' ;
OR : '|';
EXTRA_CODE : '```' .*? '```';
WS : [ \t\r\n]+ -> skip ;