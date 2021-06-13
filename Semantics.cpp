//
// Created by 1912m on 12/05/2021.
//

#include "Semantics.h"

#include "iostream"
#include <memory>
#include <cstring>

extern char *yytext;
int DEBUG = 0;
vector<shared_ptr<SymbolTable>> symTabStack;
vector<int> offsetStack;
vector<string> varTypes = {"VOID", "INT", "BYTE", "BOOL", "STRING"};

string currentRunningFunctionScopeId;

void printVector(vector<string> vec) {
    for (auto &i : vec) {
        std::cout << i << " ";
    }
}

void printMessage(string message) {
    std::cout << message << std::endl;
}

void printSymTabRow(shared_ptr<SymbolTableRow> row) {
    std::cout << row->name << " | ";
    printVector(row->type);
    std::cout << " | " << row->offset << " | " << row->isFunc << std::endl;
}

void printSymTableStack() {
    std::cout << "Size of global symbol table stack is: " << symTabStack.size() << std::endl;
    std::cout << "id | parameter types | offset | is func" << std::endl;
    for (int i = symTabStack.size() - 1; i >= 0; --i) {
        for (auto &row : symTabStack[i]->rows) {
            printSymTabRow(row);
        }
    }
}

int loopCounter = 0;
int switchCounter = 0;

void enterSwitch() {
    if (DEBUG) printMessage("Entering Switch block");
    switchCounter++;
}

void exitSwitch() {
    if (DEBUG) printMessage("Exiting Switch block");
    switchCounter--;
}

void enterLoop() {
    loopCounter++;
}

void exitLoop() {
    loopCounter--;
}

void exitProgramFuncs() {
    currentRunningFunctionScopeId = "";
}

void exitProgramRuntime() {
    if (DEBUG) {
        printMessage("I am entering program runtime");
    }
    shared_ptr<SymbolTable> globalScope = symTabStack.front();
    bool mainFunc = false;
    for (auto &row : globalScope->rows) {
        if (row->isFunc && row->name == "main" && row->type.back() == "VOID" && row->type.size() == 1) {
            mainFunc = true;
        }
    }
    if (!mainFunc) {
        output::errorMainMissing();
        exit(0);
    }
    closeCurrentScope();
    if (DEBUG) printMessage("I am exiting program runtime");
}

void openNewScope() {
    if (DEBUG) printMessage("creating scope");
    shared_ptr<SymbolTable> nScope = make_shared<SymbolTable>();
    symTabStack.push_back(nScope);
    offsetStack.push_back(offsetStack.back());
    if (DEBUG) printMessage("done creating");
}

void closeCurrentScope() {
    output::endScope();
    shared_ptr<SymbolTable> currentScope = symTabStack.back();
    for (auto &row : currentScope->rows) {
        if (!row->isFunc) {
            // Print a normal variable
            output::printID(row->name, row->offset, row->type[0]);
        } else {
            string funcReturnType = row->type.back();
            // Taking out the return type from the vector for easy printing
            row->type.pop_back();
            output::printID(row->name, row->offset,
                            output::makeFunctionType(funcReturnType, row->type));
        }
    }

    currentScope->rows.clear();
    symTabStack.pop_back();
    offsetStack.pop_back();

}

bool isDeclared(const string &name) {
    if (DEBUG) {
        printMessage("In is declared for");
        printMessage(name);
        printSymTableStack();
    }
    for (int i = symTabStack.size() - 1; i >= 0; --i) {
        for (auto &row : symTabStack[i]->rows) {
            if (row->name == name) {
                if (DEBUG) printMessage("found id");
                return true;
            }
        }
    }
    if (DEBUG) printMessage("can't find id");
    return false;
}

bool isDeclaredVariable(const string &name) {
    if (DEBUG) {
        printMessage("In is declared for");
        printMessage(name);
        printSymTableStack();
    }
    for (int i = symTabStack.size() - 1; i >= 0; --i) {
        for (auto &row : symTabStack[i]->rows) {
            if (row->name == name && !row->isFunc) {
                if (DEBUG) printMessage("found id");
                return true;
            }
        }
    }
    if (DEBUG) printMessage("can't find id");
    return false;
}

SymbolTableRow::SymbolTableRow(string name, vector<string> type, int offset, bool isFunc) : name(std::move(name)), type(std::move(type)),
                                                                                            offset(offset), isFunc(isFunc) {

}

TypeNode::TypeNode(string str) : value() {
    if (str == "void") {
        value = "VOID";
    } else if (str == "bool") {
        value = "BOOL";
    } else if (str == "int") {
        value = "INT";
    } else if (str == "byte") {
        value = "BYTE";
    } else {
        value = str;
    }
}

TypeNode::TypeNode() {
    value = "";
}

ostream &operator<<(ostream &os, const TypeNode &node) {
    os << "value: " << node.value;
    return os;
}

Program::Program() : TypeNode("Program") {
    shared_ptr<SymbolTable> symTab = std::make_shared<SymbolTable>();
    shared_ptr<SymbolTableRow> printFunc = std::make_shared<SymbolTableRow>(SymbolTableRow("print", {"STRING", "VOID"}, 0, true));
    shared_ptr<SymbolTableRow> printiFunc = std::make_shared<SymbolTableRow>(SymbolTableRow("printi", {"INT", "VOID"}, 0, true));
    // Placing the print and printi function at the bottom of the global symbol table
    symTab->rows.push_back(printFunc);
    symTab->rows.push_back(printiFunc);
    // Placing the global symbol table at the bottom of the global symbol table stack
    symTabStack.push_back(symTab);
    // Placing the global symbol table at the bottom of the offset stack
    offsetStack.push_back(0);
}

RetType::RetType(TypeNode *type) : TypeNode(type->value) {

}

Type::Type(TypeNode *type) : TypeNode(type->value) {

}

FormalDecl::FormalDecl(Type *t, TypeNode *id) : TypeNode(id->value), type(t->value) {

}

FormalsList::FormalsList(FormalDecl *formal) {
    formals.insert(formals.begin(), formal);
}

FormalsList::FormalsList(FormalDecl *formal, FormalsList *fList) {
    formals = vector<FormalDecl *>(fList->formals);
    formals.insert(formals.begin(), formal);
}

Formals::Formals() = default;

Formals::Formals(FormalsList *formList) {
    formals = vector<FormalDecl *>(formList->formals);
}

FuncDecl::FuncDecl(RetType *rType, TypeNode *id, Formals *funcParams) {
    if (DEBUG) printMessage("I am in func decl");
    if (isDeclared(id->value)) {
        // Trying to redeclare a name that is already used for a different variable/fucntion
        output::errorDef(yylineno, id->value);
        exit(0);
    }

    for (unsigned int i = 0; i < funcParams->formals.size(); ++i) {
        if (isDeclared(funcParams->formals[i]->value) || funcParams->formals[i]->value == id->value) {
            // Trying to shadow inside the function a variable that was already declared
            // Or trying to name a function with the same name as one of the function parameters
            output::errorDef(yylineno, funcParams->formals[i]->value);
            exit(0);
        }

        for (unsigned int j = i + 1; j < funcParams->formals.size(); ++j) {
            if (funcParams->formals[i]->value == funcParams->formals[j]->value) {
                // Trying to declare a function where 2 parameters or more have the same name
                output::errorDef(yylineno, funcParams->formals[i]->value);
                exit(0);
            }
        }
    }

    // This is the name of the newly declared function
    value = id->value;
    if (funcParams->formals.size() != 0) {
        // Saving the types of all the different function parameters
        for (auto &formal : funcParams->formals) {
            type.push_back(formal->type);
        }
    } else {
        // The func has no input parameters, no need to add a specific parameter type
    }
    // Saving the return type of the function
    type.push_back(rType->value);

    // Adding the new function to the symTab
    shared_ptr<SymbolTableRow> nFunc = std::make_shared<SymbolTableRow>(value, type, 0, true);
    symTabStack.back()->rows.push_back(nFunc);
    currentRunningFunctionScopeId = value;
    if (DEBUG) printMessage("exiting func decl");
}

Call::Call(TypeNode *id) {
    for (auto &symTab : symTabStack) {
        for (auto &row : symTab->rows) {
            if (row->name == id->value) {
                if (!row->isFunc) {
                    // We found a declaration of a variable with the same name, illegal
                    output::errorUndefFunc(yylineno, id->value);
                    exit(0);
                } else if (row->isFunc && row->type.size() == 1) {
                    // We found the right function, has the same name, it really is a function, and receives no parameters
                    // Saving the type of the function call return value
                    value = row->type.back();
                    return;
                } else {
                    row->type.pop_back();
                    output::errorPrototypeMismatch(yylineno, id->value, row->type);
                    exit(0);
                }
            }
        }
    }
    // We didn't find a declaration of the desired function
    output::errorUndefFunc(yylineno, id->value);
    exit(0);
}

Call::Call(TypeNode *id, ExpList *list) {
    if (DEBUG) printMessage("in call id list");
    for (auto &symTab : symTabStack) {
        for (auto &row : symTab->rows) {
            if (row->name == id->value) {
                if (!row->isFunc) {
                    // We found a declaration of a variable with the same name, illegal
                    output::errorUndefFunc(yylineno, id->value);
                    exit(0);
                } else if (row->isFunc && row->type.size() == list->list.size() + 1) {
                    // We found the right function, has the same name, it really is a function
                    // Now we need to check that the parameter types are correct between what the function accepts, and what was sent
                    for (unsigned int i = 0; i < list->list.size(); ++i) {
                        if (list->list[i].type == row->type[i]) {
                            // This parameter is of matching type so it is ok
                            continue;
                        } else if (list->list[i].type == "BYTE" && row->type[i] == "INT") {
                            // The function receives int as a paramter, in this instance a byte was sent, but it is ok to cast from BYTE to INT
                            continue;
                        }
                        // Removing the return type of the function so we have an easy list of requested parameters to print
                        row->type.pop_back();
                        output::errorPrototypeMismatch(yylineno, id->value, row->type);
                        exit(0);
                    }
                    // Saving the type of the function call return value
                    value = row->type.back();
                    return;
                } else {
                    // The number of parameters we received does not match the number the function takes as arguments
                    // Removing the return type of the function so we have an easy list of requested parameters to print
                    row->type.pop_back();
                    output::errorPrototypeMismatch(yylineno, id->value, row->type);
                    exit(0);
                }
            }
        }
    }
    // We didn't find a declaration of the desired function
    output::errorUndefFunc(yylineno, id->value);
    exit(0);
}

Exp::Exp(Call *call) {
    // Need to just take the return value of the function and use it as the return type of the expression
    if (DEBUG) printMessage("in exp call");
    value = call->value;
    type = call->value;
}

Exp::Exp(TypeNode *id) {
    // Need to make sure that the variable/func we want to use is declared
    if (DEBUG) {
        printMessage("creating exp from id:");
        printMessage(id->value);
    }
    if (!isDeclaredVariable(id->value)) {
        output::errorUndef(yylineno, id->value);
        exit(0);
    }

    // Need to save the type of the variable function as the type of the expression
    for (int i = symTabStack.size() - 1; i >= 0; --i) {
        for (auto &row : symTabStack[i]->rows) {
            if (row->name == id->value) {
                if (DEBUG) {
                    printMessage("found a variable with name in symtab:");
                    printMessage(row->name);
                }
                // We found the variable/func we wanted to use in the expression
                value = id->value;
                // Getting the type of the variable, or the return type of the function
                type = row->type.back();
                return;
            }
        }
    }
}

Exp::Exp(TypeNode *notNode, Exp *exp) {
    if (exp->type != "BOOL") {
        // This is not a boolean expression, can't apply NOT
        output::errorMismatch(yylineno);
        exit(0);
    }
    type = "BOOL";
    valueAsBooleanValue = !valueAsBooleanValue;
}

Exp::Exp(TypeNode *terminal, string taggedTypeFromParser) : TypeNode(terminal->value) {
    if (DEBUG) {
        printMessage("in for num byte");
        printMessage(terminal->value);
        printMessage("tagged type:");
        printMessage(taggedTypeFromParser);

    }
    type = taggedTypeFromParser;
    if (taggedTypeFromParser == "NUM") {
        type = "INT";
    }
    if (type == "BYTE") {
        // Need to check that BYTE size is legal
        if (stoi(terminal->value) > 255) {
            // Byte is too large
            output::errorByteTooLarge(yylineno, terminal->value);
            exit(0);
        }
    }
    if (type == "BOOL") {
        if (terminal->value == "true") {
            valueAsBooleanValue = true;
        } else {
            valueAsBooleanValue = false;
        }
    }
    if (DEBUG) {
        printMessage("now tagged as:");
        printMessage(type);
    }
}

Exp::Exp(Exp *ex) {
    if (DEBUG) {
        printMessage("=====exp ex=====");
        printMessage(ex->value);
        printMessage(ex->type);
    }
//    if (ex->type != "BOOL") {
//        output::errorMismatch(yylineno);
//        exit(0);
//    }
    value = ex->value;
    type = ex->type;
    valueAsBooleanValue = ex->valueAsBooleanValue;
}

// for Exp RELOP, MUL, DIV, ADD, SUB, OR, AND Exp
Exp::Exp(Exp *e1, TypeNode *op, Exp *e2, const string &taggedTypeFromParser) {
    // Need to check the type of the expressions on each side, to make sure that a logical operator (AND/OR) is used on a boolean type
    if ((e1->type == "INT" || e1->type == "BYTE") && (e2->type == "INT" || e2->type == "BYTE")) {
        if (taggedTypeFromParser == "EQ_NEQ_RELOP" || taggedTypeFromParser == "REL_RELOP") {
            // This is a boolean operation performed between 2 numbers (>=,<=,>,<,!=,==)
            type = "BOOL";
        } else if (taggedTypeFromParser == "ADD_SUB_BINOP" || taggedTypeFromParser == "MUL_DIV_BINOP") {
            // This is an arithmetic operation between two numbers
            if (e1->type == "INT" || e2->type == "INT") {
                // An automatic cast to int will be performed in case one of the operands is of integer type
                type = "INT";
            } else {
                type = "BYTE";
            }
        }
    } else if (e1->type == "BOOL" && e2->type == "BOOL") {
        // Both operands are boolean so this should be a boolean operation
        type = "BOOL";
        if (taggedTypeFromParser == "AND" || taggedTypeFromParser == "OR") {
            if (op->value == "AND") {
                if (e1->valueAsBooleanValue && e2->valueAsBooleanValue) {
                    valueAsBooleanValue = true;
                } else {
                    valueAsBooleanValue = false;
                }
            } else if (op->value == "OR") {
                if (e1->valueAsBooleanValue || e2->valueAsBooleanValue) {
                    valueAsBooleanValue = true;
                } else {
                    valueAsBooleanValue = false;
                }
            }
        } else {
            output::errorMismatch(yylineno);
            exit(0);
        }
    } else {
        output::errorMismatch(yylineno);
        exit(0);
    }
}

Exp::Exp(Exp *e1, string tag) {
    if (tag == "switch" && (e1->type != "INT" && e1->type != "BYTE")) {
        output::errorMismatch(yylineno);
        exit(0);
    }
}

ExpList::ExpList(Exp *exp) {
    list.insert(list.begin(), exp);
}

ExpList::ExpList(Exp *exp, ExpList *expList) {
    list = vector<Exp>(expList->list);
    list.insert(list.begin(), exp);
}

Statement::Statement(TypeNode *type) {
    if (DEBUG) {
        printMessage("In BREAK/CONTINUE");
        printMessage(type->value);
        printMessage(to_string(yylineno));
    }
//    int l = loopCounter;
//    int s = switchCounter;
    if (loopCounter == 0 && switchCounter == 0) {
        // We are not inside any loop, so a break or continue is illegal in this context
        if (type->value == "break") {
            output::errorUnexpectedBreak(yylineno);
            exit(0);
        } else if (type->value == "continue") {
            output::errorUnexpectedContinue(yylineno);
            exit(0);
        } else {
            if (DEBUG) {
                printMessage("not break or continue");
            }
        }
    } else if (loopCounter != 0) {
        // We are inside a loop so break and continue are both legal
    } else if (type->value == "continue" && switchCounter != 0) {
        output::errorUnexpectedContinue(yylineno);
        exit(0);
    }
    dataTag = "break or continue";
}

Statement::Statement(string type, Exp *exp) {
    if (DEBUG) {
        printMessage("exp type ex");
        printMessage(type);
        printMessage(exp->type);
        printMessage(exp->value);
    }
    if (exp->type != "BOOL") {
        output::errorMismatch(yylineno);
        exit(0);
    }
    dataTag = "if if else while";
}

// For Return SC -> this is for a function with a void return type
Statement::Statement(const string &funcReturnType) {
    if (DEBUG) printMessage("statement func ret type");
    // Need to check if the current running function is of void type
    for (int i = symTabStack.size() - 1; i >= 0; i--) {
        // Need to search in the current symtab with no particular order for the current function
        for (auto &row : symTabStack[i]->rows) {
            if (row->isFunc && row->name == currentRunningFunctionScopeId) {
                // We found the current running function
                if (row->type.back() == funcReturnType) {
                    dataTag = "void return value";
                } else {
                    output::errorMismatch(yylineno);
                    exit(0);
                }
            }
        }
    }
}

Statement::Statement(Exp *exp) {
    if (DEBUG) {
        printMessage("statement exp!!!!!!");
        printMessage(exp->type);
        printMessage(exp->value);
        printMessage("current func:");
        printMessage(currentRunningFunctionScopeId);
        //printSymTableStack();
    }
    // Need to check if the current running function is of the specified type
    if (exp->type == "VOID") {
        // Attempt to return a void expression from a value returning function
        output::errorMismatch(yylineno);
        exit(0);
    }

    for (int i = symTabStack.size() - 1; i >= 0; i--) {
        // Need to search in the current symtab with no particular order for the current function
        for (auto &row : symTabStack[i]->rows) {
            if (row->isFunc && row->name == currentRunningFunctionScopeId) {
                // We found the current running function
                if (row->type.back() == exp->type) {
                    dataTag = exp->value;
                } else if (row->type.back() == "INT" && exp->type == "BYTE") {
                    // Allowing automatic cast from byte to int
                    dataTag = row->type.back();
                } else {
                    output::errorMismatch(yylineno);
                    exit(0);
                }
            }
        }
    }
}

Statement::Statement(Call *call) {
    dataTag = "function call";
}

Statement::Statement(TypeNode *id, Exp *exp) {
    if (!isDeclared(id->value)) {
        output::errorUndef(yylineno, id->value);
        exit(0);
    }

    // Searching for the variable in the symtab
    for (int i = symTabStack.size() - 1; i >= 0; i--) {
        // Need to search in the current symtab with no particular order for the current function
        for (auto &row : symTabStack[i]->rows) {
            if (!row->isFunc && row->name == id->value) {
                // We found the desired variable
                if ((row->type.back() == exp->type) || (row->type.back() == "INT" && exp->type == "BYTE")) {
                    dataTag = row->type.back();
                }
            }
        }
    }
}

Statement::Statement(Type *t, TypeNode *id, Exp *exp) {
    if (DEBUG) printMessage("statement t id exp");
    if (isDeclared(id->value)) {
        // Trying to redeclare a name that is already used for a different variable/fucntion
        output::errorDef(yylineno, id->value);
        exit(0);
    }
    if ((t->value == exp->type) || (t->value == "INT" && exp->type == "BYTE")) {
        dataTag = t->value;
        // Creating a new variable on the stack will cause the next one to have a higher offset
        int offset = offsetStack.back()++;
        vector<string> varType = {t->value};
        shared_ptr<SymbolTableRow> nVar = std::make_shared<SymbolTableRow>(id->value, varType, offset, false);
        symTabStack.back()->rows.push_back(nVar);
    } else {
        output::errorMismatch(yylineno);
        exit(0);
    }
}

Statement::Statement(Type *t, TypeNode *id) {
    if (isDeclared(id->value)) {
        // Trying to redeclare a name that is already used for a different variable/fucntion
        output::errorDef(yylineno, id->value);
        exit(0);
    }
    // Creating a new variable on the stack will cause the next one to have a higher offset
    int offset = offsetStack.back()++;
    vector<string> varType = {t->value};
    shared_ptr<SymbolTableRow> nVar = std::make_shared<SymbolTableRow>(id->value, varType, offset, false);
    symTabStack.back()->rows.push_back(nVar);
    dataTag = t->value;
    if (DEBUG) printSymTableStack();
}

Statement::Statement(Statements *states) {
    if (DEBUG) {
        printMessage("In statement from statements");
        printMessage(states->value);
    }
    dataTag = "statement block";
}

Statement::Statement(Exp *exp, CaseList *cList) {
    // Need to check that exp is a number (int,byte) and that all case decl in caselist are int or byte
    if (DEBUG) {
        if (!exp) {
            printMessage("RECEIVED A NULLPTR");
        }
        printMessage("statement exp caselist");
        printMessage(exp->value);
        printMessage(exp->type);
    }
    if (exp->type != "INT" && exp->type != "BYTE") {
        if (DEBUG) printMessage("Mismatch in exp type");
        output::errorMismatch(yylineno);
        exit(0);
    }

    for (auto &i : cList->cases) {
        if (i->value != "INT" && i->value != "BYTE") {
            if (DEBUG) {
                printMessage("Mismatch in case type");
                printMessage(i->value);
            }
            output::errorMismatch(yylineno);
            exit(0);
        }
    }

    dataTag = "switch block";
}

Statements::Statements(Statement *state) {

}

Statements::Statements(Statements *states, Statement *state) {

}

CaseDecl::CaseDecl(Exp *num, Statements *states) {
    if (DEBUG) {
        printMessage("value of statements is:");
        printMessage(states->value);
        printMessage("value of exp:");
        printMessage(num->value);
        printMessage("type of exp:");
        printMessage(num->type);
    }
    if (num->type != "INT" && num->type != "BYTE") {
        //if (num->value != "INT" && num->value != "BYTE") {
        output::errorMismatch(yylineno);
        exit(0);
    }
    value = num->type;
}

CaseList::CaseList(CaseDecl *cDec, CaseList *cList) {
    cases = vector<CaseDecl *>(cList->cases);
    cases.push_back(cDec);
    value = "case list";
}

CaseList::CaseList(CaseDecl *cDec) {
    cases.push_back(cDec);
    value = "case list";
}

CaseList::CaseList(Statements *states) {

}

void insertFunctionParameters(Formals *formals) {
    for (unsigned int i = 0; i < formals->formals.size(); ++i) {
        vector<string> nType = {formals->formals[i]->type};
        shared_ptr<SymbolTableRow> nParameter = make_shared<SymbolTableRow>(formals->formals[i]->value, nType, -i - 1, false);
        symTabStack.back()->rows.push_back(nParameter);
    }
}

Funcs::Funcs() {
    if (DEBUG) printMessage("I am in funcs");
    if (strcmp(yytext, "") != 0) {
        output::errorSyn(yylineno);
        exit(0);
    }
}
