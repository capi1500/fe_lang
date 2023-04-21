# Opis języka
Fe to język programowania silnie inspirowany Rust-em. 

Elementy języka:
- Statyczne typowanie
- Typy:
	- typy prymitywne: i64, char, bool
	- struktury (struct z C) (wbudowany typ string)
	- warianty (union z informacją o typie)
	- funkcje (w tym funkcje anonimowe, wyższych rzędów) 
	- statyczne tablice (w tym wielowymiarowe)
	- referencje
- Rozróżnienie na wartości stałe, zmienne, referencje oraz mutowalne referencje
- Częściową rekonstrukcję typów (występuje przy deklaracji stałych i zmiennych)
- Instrukcje warunkowe: if, if else
- Instrukcję do patern-matching: match
- Pętle: while, for (w tym break i continue)
- Operacje arytmetyczne i boolowskie
- Ownership based memory model oraz borrow checker https://doc.rust-lang.org/stable/book/ch04-01-what-is-ownership.html i https://doc.rust-lang.org/stable/book/ch04-02-references-and-borrowing.html
- kilka wbudowanych funkcji
	- I/O: print_i64, print_char, print_bool, print_str i analogicznie input_i64, ...
	- copy_str

# Przykłady programów
```
fn main() {
	var x = 5 + 3 * 4;
	print_i64(x);
}
```

```
fn nice_print(printer: fn() -> string) {
    print_str("Very nice printing");
    print_str(printer());
}

fn main() {
    for (x in (1..10)) {
        nice_print(|&x|() -> string { // borrow x
            int_to_string((x + 6 * 4 - 3) % 6)
        }); // Should print (x + (6 * 4) - 3) % 6
    }
}
```

```
fn main() {
	fn when_odd(x: i64) -> i64 {
		x * 3
	}
	fn when_even(x: i64) -> i64 {
		x / 2
	}
	
	var x = input_i64();
	while (x != 1) {
		print_i64(x);
		x = if (x % 2 == 0) {
			when_even
		} else {
			when_odd
		}(x);
	}
}
```

```
struct Foo {
	x: i64,
	flag: bool,
	name: str,
}

struct None {}

fn change_foo(v: &mut Foo) {
	if (v.flag) {
		v.x += 5;
		v.flag = false;
	} else {
		v.flag = true;
	}
}

fn print_foo(v: &Foo) {
	print_str("Foo {\n");
	print_char('\t');
	print_i64(v.x);
	print_str(",\n");
	print_char('\t');
	print_bool(v.flag);
	print_str(",\n");
	print_char('\t');
	print_str(v.name);
	print_str("\n}");
}

variant Option_Foo {
	Foo,
	None
}

fn some_foo(value: Foo) -> Option_Foo {
	Option_Foo(value)
}

fn none_foo() -> Option_Foo {
	Option_Foo(None{})
}

fn min(first: &'a Foo, second: &'a Foo) -> &'a Foo {
	if (first.x < second.x) {
		first
	} else {
		second
	}
}


fn main() {
	const name = input_str();
	const len = name.length;
	const flag = len % 2 == 0;

	const foo_option = if (len == 0) {
		some_foo(Foo{
			x: len,
			flag,
			name,
		})
	} else {
		none_foo()
	};
	
	match (foo_option) {
		Some(foo) => {
			const copied = copy(&foo);
			change_foo(&mut foo);
			print_foo(min(&copied, &foo));
		},
		_ => print_str("None"),
	}
}
```

# Gramatyka
Inspirowana gramatyką RUSTa z https://doc.rust-lang.org/reference/

```bnf
Code. Code ::= [Statement] ;
SemicolonStatement. Statement ::= ";" ;
ItemStatement. Statement ::= Item ;
ExpressionStatement. Statement ::= Expression ;
separator Statement "" ;

rules Item ::= Function | Struct | Variant | Variable | Const ;

Function. Function ::= "fn" Ident "(" [FunctionParam] ")" FunctionReturnType Expression17 ;
Parameter. FunctionParam ::= Ident ":" Type ;
separator FunctionParam "," ;
ReturnValue. FunctionReturnType ::= "->" Type ;
ReturnUnit. FunctionReturnType ::= ;

Struct. Struct ::= "struct" Ident "{" [StructField] "}" ;
StructField. StructField ::= Ident ":" Type ;
separator StructField "," ;

Variant. Variant ::= "variant" Ident "{" [VariantItem] "}" ;
VariantItem. VariantItem ::= Type ;
separator VariantItem "," ;


Variable. Variable ::= "var" CVDeclaration ;
Const. Const ::= "const" CVDeclaration ;
CVDeclaration. CVDeclaration ::= Ident TypeDeclaration "=" Expression ";" ;
Typed. TypeDeclaration ::= ":" Type ;
Untyped. TypeDeclaration ::= ;

rules Type ::= TypeModifier SimpleType | TypeModifier ArrayType | FunctionType ;
None. TypeModifier ::= "" ; 
Ref. TypeModifier ::= "&" ;
RefLifetime. TypeModifier ::= "&" Lifetime ;
MutRef. TypeModifier ::= "&" "mut" ;
MutRefLifetime. TypeModifier ::= "&" Lifetime "mut" ;
Lifetime. Lifetime ::= "'" Ident ;

SimpleType. SimpleType ::= Ident ;
ArrayType. ArrayType ::= "[" Type ";" Expression "]" ;
FunctionType. FunctionType ::= "fn" "(" [FunctionTypeParam] ")" FunctionTypeReturnType ;
FunctionTypeParam. FunctionTypeParam ::= Type ;
separator FunctionTypeParam "," ;
FunctionTypeReturnType. FunctionTypeReturnType ::= "->" Type ;
FunctionTypeReturnTypeUnit. FunctionTypeReturnType ::= ;

GroupedExpression. Expression18 ::= "(" Expression1 ")" ;
BlockExpression. Expression17 ::= "{" [Statement] "}" ;

IfExpression. Expression16 ::= IfExpression ;
If. IfExpression ::= "if" Expression18 Expression17 ;
IfElse. IfExpression ::= "if" Expression18 Expression17 "else" Expression17 ;
IfElseIf. IfExpression ::= "if" Expression18 Expression17 "else" IfExpression ;
WhileExpression. Expression16 ::= "while" Expression18 Expression17 ;
ForExpression. Expression16 ::= "for" "(" Pattern "in" Expression1 ")" Expression17 ;
MatchExpression. Expression16 ::= "match" Expression18 "{" [MatchArm] "}" ;
MatchArm. MatchArm ::= Pattern "=>" Expression;
separator MatchArm "," ;

rules Literal ::= Char | String | Integer | Double | Boolean ;
LiteralExpression. Expression16 ::= Literal ;
VariableExpression. Expression16 ::= Ident ;
StructExpression. Expression16 ::= Ident "{" [StructExpressionField] "}" ;
StructExpressionField. StructExpressionField ::= Ident ":" Expression1 ;
StructExpressionFieldDefault. StructExpressionField ::= Ident ;
separator StructExpressionField "," ;
ArrayExpressionItems. Expression16 ::= "[" [ArrayElement] "]" ;
ArrayExpressionDefault. Expression16 ::= "[" Expression1 ";" Expression1 "]" ;
ArrayElement. ArrayElement ::= Expression1 ;
separator ArrayElement "," ;
ClousureExpression. Expression16 ::= "|" [Capture] "|" "(" [FunctionParam] ")" FunctionReturnType Expression17 ;
Capture. Capture ::= TypeModifier Ident ;
separator Capture "," ;

FieldExpression. Expression15 ::= Expression15 "." Ident ;

CallExpression. Expression14 ::= Expression14 "(" [CallParam] ")" ;
CallParam. CallParam ::= Expression1 ;
separator CallParam "," ;
IndexExpression. Expression14 ::= Expression14 "[" Expression1 "]" ;

UnaryExpression. Expression13 ::= UnaryOperator Expression14 ;
TypeCastExpression. Expression12 ::= Expression13 "as" Type ;
MultiplyExpression. Expression11 ::= Expression11 "*" Expression12 ;
DivideExpression. Expression11 ::= Expression11 "/" Expression12 ;
ModuloExpression. Expression11 ::= Expression11 "%" Expression12 ;
PlusExpression. Expression10 ::= Expression10 "+" Expression11 ;
MinusExpression. Expression10 ::= Expression10 "-" Expression11 ;
LShiftExpression. Expression8 ::= Expression9 "<<" Expression10 ;
RShiftExpression. Expression8 ::= Expression9 ">>" Expression10 ;
BitAndExpression. Expression8 ::= Expression8 "&" Expression9 ;
BitXOrExpression. Expression7 ::= Expression7 "^" Expression8 ;
BitOrExpression. Expression6 ::= Expression6 "|" Expression7 ;
ComparisonExpression. Expression5 ::= Expression5 ComparisonOperator Expression6 ;
LazyAndExpression. Expression4 ::= Expression4 "&&" Expression5 ;
LazyOrExpression. Expression3 ::= Expression3 "||" Expression4 ;
RangeExpression. Expression2 ::= Expression3 ".." Expression3 ;
AssignmentExpression. Expression1 ::= Expression2 AssignmentOperator Expression1;

BreakExpression. Expression ::= "break" ;
ContinueExpression. Expression ::= "continue" ;
ReturnExpressionUnit. Expression ::= "return" ;
ReturnExpressionValue. Expression ::= "return" Expression1 ;

coercions Expression 18 ;

UnaryMinus. UnaryOperator ::= "-" ;
UnaryNegation. UnaryOperator ::= "!" ;
Dereference. UnaryOperator ::= "*" ;
Reference. UnaryOperator ::= "&" ;
ReferenceMut. UnaryOperator ::= "&" "mut" ;


Assign. AssignmentOperator ::= "=" ;
PlusEqual. AssignmentOperator ::= "+=" ;
MinusEqual. AssignmentOperator ::= "-=" ;
MultiplyEqual. AssignmentOperator ::= "*=" ;
DivideEqual. AssignmentOperator ::= "/=" ;
ModuloEqual. AssignmentOperator ::= "%=" ;
AndEqual. AssignmentOperator ::= "&=" ;
OrEqual. AssignmentOperator ::= "|=" ;
XorEqual. AssignmentOperator ::= "^=" ;
LShiftEqual. AssignmentOperator ::= "<<=" ;
RShiftEqual. AssignmentOperator ::= ">>=" ;

Equals. ComparisonOperator ::= "==" ;
NotEquals. ComparisonOperator ::= "!=" ;
Greater. ComparisonOperator ::= ">" ;
Smaller. ComparisonOperator ::= "<" ;
GreaterEquals. ComparisonOperator ::= ">=" ;
SmallerEquals. ComparisonOperator ::= "<=" ;

VariantPattern. Pattern ::= Ident "(" Pattern ")" ;
StructPattern. Pattern ::= Ident "{" [Pattern] "}" ;
AssignPattern. Pattern ::= Ident ;
AllPattern. Pattern ::= "_" ;
separator Pattern "," ;

BoolTrue. Boolean ::= "true" ;
BoolFalse. Boolean ::= "false" ;

comment "//" ;
comment "/*" "*/" ;
```

# Tabelka cech
Na 15 punktów
+ 01 (trzy typy)
+ 02 (literały, arytmetyka, porównania)
+ 03 (zmienne, przypisanie)
+ 04 (print)
+ 05 (while, if)
+ 06 (funkcje lub procedury, rekurencja)
+ 07 (przez zmienną / przez wartość / in/out)
+ 08 (zmienne read-only i pętla for)

Na 20 punktów
+ 09 (przesłanianie i statyczne wiązanie)
+ 10 (obsługa błędów wykonania)
+ 11 (funkcje zwracające wartość)

Na 30 punktów
+ 12 (4) (statyczne typowanie)
+ 13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
+ 14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe) - tablice, tablice wielowymiarowe
- 15 (2) (krotki z przypisaniem)
+ 16 (1) (break, continue)
+ 17 (4) (funkcje wyższego rzędu, anonimowe, domknięcia)
- 18 (3) (generatory)

Dodatkowo:
ownership-based memory model
statyczna analiza poprawności pamięciowej - borrow checker
typy wariantowe
pattern matching wraz ze sprawdzaniem kompletności pattern matchingu
częściowa rekonstrukcja typów (w tym rekonstrukcja lifetime annotations dla referencji)

Razem: 30 (+ może jakiś bonus za dodatki :) )
