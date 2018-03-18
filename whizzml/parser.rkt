#lang brag

whizzml-program: (definition | @sexp)*

@sexp: bool | number | string | ident | list-lit | map-lit | @form

form: /'(' (lambda | if | when | cond | and | or | let | app | prog
            | loop | recur | for | iterate | break
            | raise | handle | try | catch) /')'

if: /'if' sexp sexp [sexp]
when: /'when' sexp @body
and: /'and' sexp*
or: /'or' sexp*

cond: /'cond' branch* [def]
branch: sexp sexp
def: sexp

let: /'let' /'(' bindings /')' body
binding: ((ident | ident-list) sexp)
bindings: binding+
ident-list: /'[' @formals /']'
body: sexp+

loop: /'loop' /'(' bindings /')' body
recur: /'recur' sexp*
for: /'for' /'(' @binding /')' body
iterate: /'iterate' /'(' bindings /')' body
break: /'break' sexp

applicable: ident | map-lit | list-lit | /'(' (lambda | app) /')'

raise: /'raise' sexp
handle: /'handle' @applicable body
try: /'try' body /'(' catch /')'
catch: /'catch' ident @body

app: @applicable sexp*

list-lit: /'[' sexp* /']'
kvpair: @string sexp
map-lit: /'{' kvpair* /'}'

lambda: /'lambda' [name] /'(' formals /')' body
name: @ident
formals: @ident* [/'.' opt]
opt: @ident

definition: /'(' /'define' (@binding | funsign body) /')'
funsign: /'(' @ident @formals /')'

prog: /'prog' sexp*

ident: IDENT
number: INTEGER | REAL | RATIONAL
string: STRING
bool: TRUE | FALSE
