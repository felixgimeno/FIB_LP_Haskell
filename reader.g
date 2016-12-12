#header
<<
#include <string>
#include <iostream>
#include <map>
using namespace std;
typedef struct {
  string kind;
  string text;
} Attrib;
void zzcr_attr(Attrib *attr, int type, char *text);
#define AST_FIELDS string kind; string text;
#include "ast.h"
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
>>
<<
#include <cstdlib>
#include <cmath>

AST *root;

void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == ID) {
    attr->kind = "ID";
    attr->text = text;
  }
  else {
    attr->kind = text;
    attr->text = "";
  }
}
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind; 
  as->text = attr->text;
  as->right = NULL; 
  as->down = NULL;
  return as;
}

AST* createASTlist(AST *child) {
 AST *as=new AST;
 as->kind="list";
 as->right=NULL;
 as->down=child;
 return as;
}

AST* child(AST *a,int n) {
AST *c=a->down;
for (int i=0; c!=NULL && i<n; i++) c=c->right;
return c;
}
void ASTPrintIndent(AST *a);
void IDNExprPrint(AST* a){
	if (a->kind == "ID"){
		cout << " (Var (Ident \"" << a->text << "\") ) ";
	} else {
		ASTPrintIndent(a);
	}
}
void ASTPrintIndent(AST *a)
{
	if (a==NULL) return;
	if (a->kind == "list"){
		cout << "Seq [ ";
		AST *i = a->down;
		bool first = true;
		while (i!=NULL /*&& i->right!=NULL*/) {
			if (first){
				first=false;
			} else {
				cout << " , ";
			}
			cout << " ( ";
			ASTPrintIndent(i);
			cout << " ) ";
			i=i->right;
		}
		cout << " ] ";
		return;	
	}
	else if (a->kind == "ID"){
		cout << " (Ident \"" << a->text << "\") ";
	}
	else if (a->kind == "INPUT"){
		cout << "Input ";
		ASTPrintIndent(a->down);
	}
	else if (a->kind == "PRINT"){
		cout << "Print ";
		ASTPrintIndent(a->down);
	}
	else if (a->kind == "EMPTY"){
		cout << "Empty ";
		ASTPrintIndent(a->down);
	}
	else if (a->kind == "SIZE"){
		cout << "Size ";
		ASTPrintIndent(a->down);
		ASTPrintIndent(a->down->right);
	}
	else if (a->kind == "POP"){
		cout << "Pop ";
		ASTPrintIndent(a->down);
		ASTPrintIndent(a->down->right);
	}	
	else if (a->kind == "PUSH"){
		cout << "Push (";
		ASTPrintIndent(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else if (a->kind == ":="){
		cout << "Assign (";
		ASTPrintIndent(a->down);
		cout << " ) ( ";
		ASTPrintIndent(a->down->right);
		cout << ")";
	}
	else if (a->kind == "IF"){
		cout << "Cond (";
		ASTPrintIndent(a->down);
		cout << " ) ( ";
		ASTPrintIndent(a->down->right);
		cout << " ) ( ";
		ASTPrintIndent(a->down->right->right);
		cout << ")";
	}
	else if (a->kind == "WHILE"){
		cout << "Loop (";
		ASTPrintIndent(a->down);
		cout << " ) ( ";
		ASTPrintIndent(a->down->right);
		cout << ")";
	}
	else if (a->kind == "*"){
		cout << "Times (";
		IDNExprPrint(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else if (a->kind == "+"){
		cout << "Plus (";
		IDNExprPrint(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else if (a->kind == "-"){
		cout << "Minus (";
		IDNExprPrint(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else if (a->kind == "AND"){
		cout << "And (";
		IDNExprPrint(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else if (a->kind == "OR"){
		cout << "Or (";
		IDNExprPrint(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else if (a->kind == "NOT"){
		cout << "Not (";
		IDNExprPrint(a->down);
		cout << ")";
	}
	else if (a->kind == "="){
		cout << "Eq (";
		IDNExprPrint(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else if (a->kind == ">"){
		cout << "Gt (";
		IDNExprPrint(a->down);
		cout << " ) ( ";
		IDNExprPrint(a->down->right);
		cout << ")";
	}
	else {
		cout << "Const " << a->kind;
	}
}
int main() {
  root = NULL;
  ANTLR(command(&root), stdin);
  ASTPrintIndent(root);
}
>>
#lexclass START
#token INPUT "INPUT"
#token IF "IF"
#token THEN "THEN"
#token GT "\>"
#token LT "\<"
#token ASSIG ":=" 
#token EQ "="
#token OR "OR"
#token NOT "NOT"
#token AND "AND"
#token ELSE "ELSE"
#token WHILE "WHILE"
#token DO "DO"
#token END "END"
#token PRINT "PRINT"
#token EMPTY "EMPTY"
#token PUSH "PUSH"
#token SIZE "SIZE"
#token POP "POP"
#token PLUS "\+"
#token MINUS "\-"
#token TIMES "\*"
#token COMMA ";"
#token SPACE "[\ \n]" << zzskip();>>;
#token TAB "[\t]" << zzskip();>>;
#token NUM "[0-9]+|\-[0-9]+.[0-9]+|[0-9]+.[0-9]+|\-[0-9]+"
#token ID "[a-zA-Z]+[0-9]*"
command: (atomic_command)* <<#0=createASTlist(_sibling);>>;
atomic_command: INPUT^ ID  | ID ASSIG^ nexpr | PRINT^ ID | 
	EMPTY^ ID | PUSH^ ID nexpr | POP^ ID ID | SIZE^ ID ID | 
	IF^ bexpr THEN! command ELSE! command END! | 
	WHILE^ bexpr DO! command END! ;
nexpr: nexpr2 (TIMES^ nexpr|);
nexpr2: (NUM | ID) (MINUS^ nexpr2|PLUS^ nexpr2|);
bexpr: bexpr2 ((AND^ | OR^) bexpr|) | NOT^ bexpr2;
bexpr2: nexpr (EQ^ | GT^) nexpr;
