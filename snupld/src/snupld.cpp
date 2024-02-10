
/// @brief SnuPL/2 language server
/// @author Jaewoo Ahn <kotlincpp@snu.ac.kr>
#include <nlohmann/json.hpp>
#include "parser.h"
#include "scanner.h"
#include <fstream>
#include <iostream>
#define DEBUG 0
using json = nlohmann::json;
namespace DiagnosticSeverity
{
  enum Type
  {
    Error = 1,
    Warning = 2,
    Information = 3,
    Hint = 4
  };
}
namespace LogLevel
{
  enum Type
  {
    Error = 1,
    Warning = 2,
    Information = 3,
    Log = 4,
    Debug = 5
  };
}
namespace CompletionItemKind
{
  enum Type
  {
    Text = 1,
    Method = 2,
    Function = 3,
    Constructor = 4,
    Field = 5,
    Variable = 6,
    Class = 7,
    Interface = 8,
    Module = 9,
    Property = 10,
    Unit = 11,
    Value = 12,
    Enum = 13,
    Keyword = 14,
    Snippet = 15,
    Color = 16,
    File = 17,
    Reference = 18,
    Folder = 19,
    EnumMember = 20,
    Constant = 21,
    Struct = 22,
    Event = 23,
    Operator = 24,
    TypeParameter = 25
  };
}
namespace SymbolKind
{
  enum Type
  {
    File = 1,
    Module = 2,
    Namespace = 3,
    Package = 4,
    Class = 5,
    Method = 6,
    Property = 7,
    Field = 8,
    Constructor = 9,
    Enum = 10,
    Interface = 11,
    Function = 12,
    Variable = 13,
    Constant = 14,
    String = 15,
    Number = 16,
    Boolean = 17,
    Array = 18,
    Object = 19,
    Key = 20,
    Null = 21,
    EnumMember = 22,
    Struct = 23,
    Event = 24,
    Operator = 25,
    TypeParameter = 26
  };
}

namespace SemanticTokenTypes {
  enum Type {
    Namespace = 0,
    Type = 1,
    Class = 2,
    Enum = 3,
    Interface = 4,
    Struct = 5,
    TypeParameter = 6,
    Parameter = 7,
    Variable = 8,
    Property = 9,
    EnumMember = 10,
    Event = 11,
    Function = 12,
    Method = 13,
    Macro = 14,
    Keyword = 15,
    Modifier = 16,
    Comment = 17,
    String = 18,
    Number = 19,
    Regexp = 20,
    Operator = 21,
    Decorator = 22,
  };
}
struct Position
{
  int line;
  int character;
};
static map<string, unique_ptr<CAstNode>> nodes;
static map<string, string> codes;

void SendResponse(json response, int id = -1)
{
  response["jsonrpc"] = "2.0";
  std::cout << "Content-Length: " << response.dump().size() << "\r\n\r\n"
            << response.dump();
  std::cout.flush();
}
void ConsoleLog(std::string msg, LogLevel::Type level = LogLevel::Log)
{
  if (level <= LogLevel::Warning || DEBUG)
  {
    json response;
    response["method"] = "window/logMessage";
    response["params"] = {{"type", 1}, {"message", msg}};
    SendResponse(response);
  }
}
json BuildDiagnostic(DiagnosticSeverity::Type severity, Position start, Position end,
                     std::string msg)
{
  return {{"range",
           {{"start", {{"line", start.line}, {"character", start.character}}},
            {"end", {{"line", end.line}, {"character", end.character}}}}},
          {"severity", severity},
          {"source", "snuplc"},
          {"message", msg}};
}
json BuildDiagnostic(DiagnosticSeverity::Type severity, const CToken *token, std::string msg)
{
  return BuildDiagnostic(severity, {token->GetLineNumber() - 1, token->GetCharPosition() - 1},
                         {token->GetLineNumber() - 1,
                          static_cast<int>(token->GetCharPosition() - 1 + token->GetValue().length())},
                         msg);
}

void SendDiagnostics(std::string uri, json::array_t diagnostics)
{
  json response;
  response["method"] = "textDocument/publishDiagnostics";
  response["params"] = {{"uri", uri}, {"diagnostics", diagnostics}};
  SendResponse(response);
}

void Validate(std::string uri)
{
  ConsoleLog("validating " + uri);
  unique_ptr<CScanner> s(new CScanner(codes[uri]));
  unique_ptr<CParser> p(new CParser(s.get()));
  CAstNode *n = p->Parse();

  json diagnostics = json::array();

  CToken t;
  std::string msg;
  if (p->HasError())
  {
    const CToken *error = p->GetErrorToken();
    const std::string msg = p->GetErrorMessage();
    diagnostics.push_back(BuildDiagnostic(DiagnosticSeverity::Error, error, msg));
    SendDiagnostics(uri, diagnostics);
    return;
  }

  if (n == nullptr)
  {
    diagnostics.push_back(
        BuildDiagnostic(DiagnosticSeverity::Error, {0, 0}, {0, 0}, "invalid ast"));
    SendDiagnostics(uri, diagnostics);
    return;
  }

  auto m = dynamic_cast<CAstModule *>(n);
  if (m == nullptr)
  {
    // invalid ast
    diagnostics.push_back(
        BuildDiagnostic(DiagnosticSeverity::Error, {0, 0}, {0, 0}, "invalid ast"));
    SendDiagnostics(uri, diagnostics);
    return;
  }
  auto ni = nodes.find(uri);
  if (ni == nodes.end())
  {
    nodes[uri] = unique_ptr<CAstNode>(n);
  }
  else
  {
    nodes[uri].reset(n);
  }
  ConsoleLog("updated ast");
  // type check

  // if (!m->TypeCheck(&t, &msg))
  // {
  //   diagnostics.push_back(BuildDiagnostic(DiagnosticSeverity::Error, &t, msg));
  // }
  vector<pair<CToken, string>> errors;
  ConsoleLog("type checking");
  m->CollectiveTypeCheck(errors);
  ConsoleLog("type checked");
  for(auto error: errors) {
    diagnostics.push_back(BuildDiagnostic(DiagnosticSeverity::Error, &error.first, error.second));
  }

  SendDiagnostics(uri, diagnostics);
  return;
}
const CToken GetTokenAt(string uri, int line, int character, EToken type)
{
  ConsoleLog("getting token at " + std::to_string(line) + ":" +
             std::to_string(character));
  unique_ptr<CScanner> s(new CScanner(codes[uri]));
  CToken t;
  while (true)
  {
    t = s->Get();
    if (t.GetType() == tEOF || !s->Good())
      break;
    if (t.GetLineNumber() == line &&
        t.GetCharPosition() <= character &&
        t.GetCharPosition() + t.GetValue().length() >= character && (t.GetType() == type || type == tUndefined))
      break;
    if (t.GetLineNumber() > line)
      return CToken(t.GetLineNumber(), t.GetCharPosition(), tUndefined, "");
  }
  return t;
}
const CToken GetTokenBefore(string uri, int line, int character, EToken type)
{
  ConsoleLog("getting token before " + std::to_string(line) + ":" +
             std::to_string(character));
  unique_ptr<CScanner> s(new CScanner(codes[uri]));
  CToken t;
  CToken prev;
  while (true)
  {
    prev = t;
    t = s->Get();
    if (t.GetType() == tEOF || !s->Good())
      break;
    if (t.GetLineNumber() == line &&
        t.GetCharPosition() <= character &&
        t.GetCharPosition() + t.GetValue().length() >= character && (t.GetType() == type || type == tUndefined))
      break;
    if (t.GetLineNumber() > line)
      return prev;
  }
  return prev;
}
string GetTypeDescription(const CType *type, bool unwrapPointer = false, bool isArrayFirstDim = true)
{
  ostringstream oss;
  if (type->IsArray())
  {
    const CArrayType *array = dynamic_cast<const CArrayType *>(type);
    if (isArrayFirstDim)
    {
      oss << GetTypeDescription(array->GetBaseType(), false, false);
    }
    oss << "[";
    if (array->GetNElem() != CArrayType::OPEN)
    {
      oss << array->GetNElem();
    }
    oss << "]";
    if (array->GetInnerType()->IsArray())
    {
      oss << GetTypeDescription(array->GetInnerType(), false, false);
    }
  }
  else if (type->IsPointer())
  {
    const CPointerType *pointer = dynamic_cast<const CPointerType *>(type);
    if (unwrapPointer)
    {
      oss << GetTypeDescription(pointer->GetBaseType(), false, true);
    }
    else
    {
      oss << "*" << GetTypeDescription(pointer->GetBaseType(), false, true);
    }
  }
  else
  {
    oss << type->GetName();
  }
  return oss.str();
}
string DataToString(const CDataInitializer *dataInit, const CType *type)
{
  ostringstream oss;
  if (type->IsBoolean())
  {
    const CDataInitBoolean *data = dynamic_cast<const CDataInitBoolean *>(dataInit);
    oss << (data->GetData() ? "true" : "false");
  }
  else if (type->IsChar())
  {
    const CDataInitChar *data = dynamic_cast<const CDataInitChar *>(dataInit);
    char c = data->GetData();
    string escaped = CToken::escape(tCharConst, string(1, c));
    oss << "'" << escaped << "'";
  }
  else if (type->IsInteger())
  {
    const CDataInitInteger *data = dynamic_cast<const CDataInitInteger *>(dataInit);
    oss << data->GetData();
  }
  else if (type->IsLongint())
  {
    const CDataInitLongint *data = dynamic_cast<const CDataInitLongint *>(dataInit);
    oss << data->GetData() << "L";
  }
  else if (type->IsArray())
  {
    const CArrayType *array = dynamic_cast<const CArrayType *>(type);
    if (array->GetInnerType()->IsChar())
    {
      const CDataInitString *data = dynamic_cast<const CDataInitString *>(dataInit);
      oss << "\"" << data->GetData() << "\"";
    }
  }
  return oss.str();
}
string GetSymbolDescription(const CSymbol *symbol);
string GetParamsDescription(const CSymProc *proc)
{
  ostringstream oss;
  for (int i = 0; i < proc->GetNParams(); i++)
  {
    if (i > 0)
      oss << "; ";
    oss << GetSymbolDescription(proc->GetParam(i));
  }
  return oss.str();
}
string GetSymbolDescription(const CSymbol *symbol)
{
  ostringstream oss;
  switch (symbol->GetSymbolType())
  {
  case stProcedure:
  {
    bool isFunc = symbol->GetDataType() != CTypeManager::Get()->GetNull();
    oss << (isFunc ? "function" : "procedure") << " " << symbol->GetName()
        << "(";
    const CSymProc *proc = dynamic_cast<const CSymProc *>(symbol);
    oss << GetParamsDescription(proc);
    oss << ")";
    if (isFunc)
      oss << ": " << GetTypeDescription(symbol->GetDataType());
    break;
  }
  case stConstant:
    oss << "const " << symbol->GetName() << ": " << GetTypeDescription(symbol->GetDataType()) << " = " << DataToString(symbol->GetData(), symbol->GetDataType());
    break;
  case stParam:
    oss << symbol->GetName() << ": " << GetTypeDescription(symbol->GetDataType(), false);
    break;
  default:
    oss << "var " << symbol->GetName() << ": " << GetTypeDescription(symbol->GetDataType(), false);
  }
  return oss.str();
}
const CAstScope *FindNearestScope(string uri, int line, int column, int length = 1)
{
  ConsoleLog("finding scope for " + uri + " " + std::to_string(line) + ":" +
             std::to_string(column) + " " + std::to_string(length));
  auto ni = nodes.find(uri);
  if (ni == nodes.end())
  {
    ConsoleLog("module not exists");
    return nullptr;
  }

  auto m = dynamic_cast<CAstModule *>(ni->second.get());
  if (m == nullptr)
  {
    ConsoleLog("ast not exists");
    return nullptr;
  }
  ConsoleLog("ast exists");
  // find nearest scope
  // TODO: Find better way than playing with begin/end token
  const CAstScope *scope = m; // module will be the result if none of its children embraces the position
  for (int i = 0; i < scope->GetNumChildren(); i++)
  {
    CToken begin = scope->GetChild(i)->GetScopeBeginToken();
    CToken end = scope->GetChild(i)->GetEndToken();

    // find innermost scope
    if (begin.GetLineNumber() <= line &&
        end.GetLineNumber() >= line)
    {
      if (begin.GetLineNumber() == line &&
          begin.GetCharPosition() > column)
        continue;
      if (end.GetLineNumber() == line &&
          end.GetCharPosition() + end.GetValue().length() <
              column + length)
        continue;
      ConsoleLog("scope " + std::to_string(i) + " " + begin.GetValue() + " " +
                 std::to_string(begin.GetLineNumber()) + ":" +
                 std::to_string(begin.GetCharPosition()) + " " + end.GetValue() +
                 " " + std::to_string(end.GetLineNumber()) + ":" +
                 std::to_string(end.GetCharPosition()));
      return scope->GetChild(i);
    }
  }
  return scope;
}
const CSymbol *GetSymbol(string uri, const CToken t)
{
  assert(t.GetType() == tIdent);
  const CAstScope *scope = FindNearestScope(uri, t.GetLineNumber(), t.GetCharPosition(), t.GetValue().length());
  // find symbol
  if (scope == nullptr)
  {
    return nullptr;
  }
  const CSymtab *symtab = scope->GetSymbolTable();
  if (symtab == nullptr)
  {
    return nullptr;
  }

  const CSymbol *symbol = symtab->FindSymbol(t.GetValue(), sGlobal);
  return symbol;
}
void GoToDefinition(int id, string uri, int line, int character)
{
  CToken t = GetTokenAt(uri, line, character, tIdent);
  json response;
  response["id"] = id;
  if (t.GetType() == tUndefined)
  {
    // return empty response for textDocument/definition
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  const CSymbol *symbol = GetSymbol(uri, t);
  if (symbol == nullptr)
  {
    response["result"] = nullptr;
    SendResponse(response);
    // log
    ConsoleLog("no symbol");
    return;
  }
  // find definition pos
  int lineno = symbol->GetLine() - 1;
  int column = symbol->GetColumn() - 1;
  response["result"] = {{"uri", uri}, {"range", {{"start", {{"line", lineno}, {"character", column}}}, {"end", {{"line", lineno}, {"character", column + t.GetValue().length()}}}}}};
  SendResponse(response);
  // log
  ConsoleLog("found symbol for " + t.GetValue() + " at " + std::to_string(lineno) + ":" + std::to_string(column));
}
vector<CToken> FindReferences(string uri, CToken t, bool excludeDefinition = true)
{
  const CSymbol *symbol = GetSymbol(uri, t);
  vector<CToken> references;
  unique_ptr<CScanner> s2(new CScanner(codes[uri]));
  while (true)
  {
    CToken t2 = s2->Get();
    if (t2.GetType() == tEOF || !s2->Good())
      break;
    if (t2.GetType() == tIdent && t2.GetValue() == t.GetValue())
    {

      const CSymbol *symbol2 = GetSymbol(uri, t2);
      // skip definition
      if (excludeDefinition)
      {
        if (symbol2 != nullptr && t2.GetLineNumber() == symbol->GetLine() && t2.GetCharPosition() == symbol->GetColumn())
        {
          continue;
        }
      }

      // if definition position is same, we consider them as same symbol
      if (symbol2 != nullptr && symbol2->GetColumn() == symbol->GetColumn() && symbol2->GetLine() == symbol->GetLine())
      {
        references.push_back(t2);
      }
    }
  }
  return references;
}
void FindReferences(int id, string uri, int line, int character)
{
  auto ni = nodes.find(uri);
  if (ni == nodes.end())
  {
    ConsoleLog("module not exists");
    return;
  }
  auto m = dynamic_cast<CAstModule *>(ni->second.get());
  if (m == nullptr)
  {
    ConsoleLog("ast not exists");
    return;
  }
  ConsoleLog("ast exists");
  // find surrounding token

  CToken t = GetTokenAt(uri, line, character, tIdent);

  json response;
  response["id"] = id;
  if (t.GetType() == tUndefined)
  {
    // return empty response for textDocument/references
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }

  // find references
  json references = json::array();
  vector<CToken> references_t = FindReferences(uri, t);
  for (auto t2 : references_t)
  {
    references.push_back({{"uri", uri}, {"range", {{"start", {{"line", t2.GetLineNumber() - 1}, {"character", t2.GetCharPosition() - 1}}}, {"end", {{"line", t2.GetLineNumber() - 1}, {"character", t2.GetCharPosition() - 1 + t2.GetValue().length()}}}}}});
  }
  response["result"] = references;
  SendResponse(response);
  // log
  ConsoleLog("found " + std::to_string(references.size()) + " references for " + t.GetValue());
}

void Hover(int id, string uri, int line, int character)
{
  auto ni = nodes.find(uri);
  if (ni == nodes.end())
  {
    ConsoleLog("module not exists");
    return;
  }
  auto m = dynamic_cast<CAstModule *>(ni->second.get());
  if (m == nullptr)
  {
    ConsoleLog("ast not exists");
    return;
  }
  json response;
  response["id"] = id;

  ConsoleLog("ast exists");
  // find surrounding token

  CToken t = GetTokenAt(uri, line, character, tIdent);

  if (t.GetType() == tUndefined)
  {
    // return empty response for textDocument/references
    response["result"] = nullptr;
    SendResponse(response);
    ConsoleLog("not pointing to identifier");
    return;
  }
  const CSymbol *symbol = GetSymbol(uri, t);
  if (symbol == nullptr)
  {
    response["result"] = nullptr;
    SendResponse(response);
    // log
    ConsoleLog("no symbol");
    return;
  }
  // find definition pos
  int lineno = symbol->GetLine() - 1;
  int column = symbol->GetColumn() - 1;
  string markdown = "```snupl2\n" + GetSymbolDescription(symbol) + "\n```";
  response["result"] = {{"contents", {{"kind", "markdown"}, {"value", markdown}}}};
  SendResponse(response);
  // log
  ConsoleLog("found symbol for " + t.GetValue() + " at " + std::to_string(lineno) + ":" + std::to_string(column));
}

void Completion(int id, string uri, int line, int character)
{
  const CAstScope *scope = FindNearestScope(uri, line, character);
  json response;
  response["id"] = id;
  if (scope == nullptr)
  {
    ConsoleLog("no scope");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  // get currently typed word
  CToken t = GetTokenAt(uri, line, character, tIdent);
  bool isInStatSeq = false;

  CToken begin = scope->GetStatementSequenceBeginToken();
  CToken end = scope->GetEndToken();
  ConsoleLog("begin:" + begin.GetValue() + " " + std::to_string(begin.GetLineNumber()) + ":" + std::to_string(begin.GetCharPosition()));
  ConsoleLog("end:" + end.GetValue() + " " + std::to_string(end.GetLineNumber()) + ":" + std::to_string(end.GetCharPosition()));
  if (begin.GetLineNumber() <= line &&
      end.GetLineNumber() >= line)
  {
    isInStatSeq = true;
    if (begin.GetLineNumber() == line &&
        begin.GetCharPosition() > character)
      isInStatSeq = false;
    if (end.GetLineNumber() == line &&
        end.GetCharPosition() + end.GetValue().length() <
            character + t.GetValue().length())
      isInStatSeq = false;
  }

  bool isInDecl = false;
  const CToken before = GetTokenBefore(uri, line, character, tUndefined);
  switch (before.GetType())
  {
  case tProcedure:
  case tFunction:
  case tVarDecl:
  case tConstDecl:
    isInDecl = true;
    break;
  default:
    isInDecl = false;
  }
  ConsoleLog("is in declaration:" + std::to_string(isInDecl));
  if (isInDecl)
  {
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }

  ConsoleLog("is in statement sequence:" + std::to_string(isInStatSeq));
  if (t.GetType() == tUndefined)
  {
    // return empty response for textDocument/definition
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  ConsoleLog("typed word:" + t.GetValue());
  // traverse symbol table
  json completions = json::array();
  const CSymtab *symtab = scope->GetSymbolTable();
  if (symtab == nullptr)
  {
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  ConsoleLog("symtab exists");
  vector<CSymbol *> symbols;
  while (symtab != nullptr)
  {
    vector<CSymbol *> symbols2 = symtab->GetSymbols();
    for (auto symbol : symbols2)
    {
      // check position
      if (symbol->GetLine() > line)
        continue;
      if (symbol->GetLine() == line && symbol->GetColumn() > character)
        continue;
      symbols.push_back(symbol);
    }
    symtab = symtab->GetParent();
  }

  if (symbols.size() == 0)
  {
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  ConsoleLog("symbols exists");
  for (auto symbol : symbols)
  {
    if (symbol == nullptr)
      continue;
    // check if starts with typed word
    if (symbol->GetName().find(t.GetValue()) != 0)
      continue;
    CompletionItemKind::Type kind;
    switch (symbol->GetSymbolType())
    {
    case stProcedure:
      kind = CompletionItemKind::Function;
      break;
    case stConstant:
      kind = CompletionItemKind::Constant;
      break;
    case stParam:
      kind = CompletionItemKind::Variable;
      break;
    case stReserved:
      continue;
    default:
      kind = CompletionItemKind::Variable;
    }
    if (!isInStatSeq && symbol->GetSymbolType() != stConstant)
    {
      continue;
    }

    json labelDetails = json::object();

    labelDetails["description"] = GetTypeDescription(symbol->GetDataType());
    if (symbol->GetSymbolType() == stProcedure)
    {
      labelDetails["detail"] = "(" + GetParamsDescription(dynamic_cast<const CSymProc *>(symbol)) + ")";
    }
    if (symbol->GetSymbolType() == stConstant)
    {
      labelDetails["detail"] = " = " + DataToString(symbol->GetData(), symbol->GetDataType());
    }
    completions.push_back({{"label", symbol->GetName()}, {"kind", kind}, {"detail", GetSymbolDescription(symbol)}, {"labelDetails", labelDetails}});
  }
  response["result"] = completions;
  SendResponse(response);
}
json BuildSymbolTreeRecursive(string uri, const CSymtab *symtab)
{
  json children = json::array();
  vector<CSymbol *> symbols = symtab->GetSymbols();
  for (auto symbol : symbols)
  {
    if (symbol == nullptr)
      continue;
    if (symbol->GetLine() == 0 || symbol->GetColumn() == 0)
    {
      ConsoleLog("Skipping symbol " + symbol->GetName() + " at " + std::to_string(symbol->GetLine()) + ":" + std::to_string(symbol->GetColumn()));
      continue;
    }
    json child = json::object();
    child["name"] = symbol->GetName();
    child["kind"] = SymbolKind::File;
    child["range"] = {{"start", {{"line", symbol->GetLine() - 1}, {"character", symbol->GetColumn() - 1}}}, {"end", {{"line", symbol->GetLine() - 1}, {"character", symbol->GetColumn() - 1 + symbol->GetName().length()}}}};
    child["selectionRange"] = child["range"];
    switch (symbol->GetSymbolType())
    {
    case stProcedure:
    {
      child["kind"] = SymbolKind::Function;
      child["name"] = symbol->GetName() + "(" + GetParamsDescription(dynamic_cast<const CSymProc *>(symbol)) + ")";
      const CAstScope *scope = FindNearestScope(uri, symbol->GetLine(), symbol->GetColumn());
      if (scope != nullptr && scope->GetSymbolTable() != nullptr)
      {
        child["children"] = BuildSymbolTreeRecursive(uri, scope->GetSymbolTable());
      }
      break;
    }
    case stConstant:
      child["kind"] = SymbolKind::Constant;
      child["name"] = symbol->GetName() + " = " + DataToString(symbol->GetData(), symbol->GetDataType());
      break;
    case stParam:
      continue;
    default:
      child["kind"] = SymbolKind::Variable;
    }
    children.push_back(child);
  }
  return children;
}
void DocumentSymbol(int id, string uri)
{
  auto ni = nodes.find(uri);
  json response;
  response["id"] = id;
  if (ni == nodes.end())
  {
    ConsoleLog("module not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  auto m = dynamic_cast<CAstModule *>(ni->second.get());
  if (m == nullptr)
  {
    ConsoleLog("ast not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }

  const CSymtab *symtab = m->GetSymbolTable();
  if (symtab == nullptr)
  {
    ConsoleLog("symtab not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  response["result"] = BuildSymbolTreeRecursive(uri, symtab);
  SendResponse(response);
}

void Rename(int id, string uri, int line, int character, string newName)
{
  auto ni = nodes.find(uri);
  json response;
  response["id"] = id;
  if (ni == nodes.end())
  {
    ConsoleLog("module not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  auto m = dynamic_cast<CAstModule *>(ni->second.get());
  if (m == nullptr)
  {
    ConsoleLog("ast not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }

  const CAstScope *scope = FindNearestScope(uri, line, character);
  if (scope == nullptr)
  {
    ConsoleLog("scope not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  const CSymtab *symtab = scope->GetSymbolTable();
  if (symtab == nullptr)
  {
    ConsoleLog("symtab not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  const CSymbol *symbol = GetSymbol(uri, GetTokenAt(uri, line, character, tIdent));
  if (symbol == nullptr)
  {
    ConsoleLog("symbol not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  ConsoleLog("renaming " + symbol->GetName() + " to " + newName);
  vector<CToken> references_t = FindReferences(uri, GetTokenAt(uri, line, character, tIdent), false);
  // found
  ConsoleLog("found " + std::to_string(references_t.size()) + " references");
  // WorkspaceEdit
  json changes = json::object();
  json textEdits = json::array();
  for (auto t : references_t)
  {
    textEdits.push_back({{"range", {{"start", {{"line", t.GetLineNumber() - 1}, {"character", t.GetCharPosition() - 1}}}, {"end", {{"line", t.GetLineNumber() - 1}, {"character", t.GetCharPosition() - 1 + t.GetValue().length()}}}}}, {"newText", newName}});
  }
  changes[uri] = textEdits;
  json workspaceEdit = {{"changes", changes}};
  response["result"] = workspaceEdit;
  SendResponse(response);
}

void SemanticTokensFull(int id, string uri)
{
  auto ni = nodes.find(uri);
  json response;
  response["id"] = id;
  if (ni == nodes.end())
  {
    ConsoleLog("module not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  auto m = dynamic_cast<CAstModule *>(ni->second.get());
  if (m == nullptr)
  {
    ConsoleLog("ast not exists");
    response["result"] = nullptr;
    SendResponse(response);
    return;
  }
  ConsoleLog("ast exists");
  // find surrounding token
  unique_ptr<CScanner> s(new CScanner(codes[uri]));
  json data = json::array();
  int line = 1;
  int start = 1;
  while (true)
  {
    CToken t = s->Get();
    if (t.GetType() == tEOF || !s->Good())
      break;
    if (t.GetType() == tIdent)
    {
      const CSymbol *symbol = GetSymbol(uri, t);
      if (symbol == nullptr)
      {
        continue;
      }
      int deltaLine = t.GetLineNumber() - line;
      int deltaStart = t.GetCharPosition() - start;
      line = t.GetLineNumber();
      start = t.GetCharPosition();
      if(deltaLine != 0) {
        deltaStart = t.GetCharPosition() - 1;
        start = deltaStart+1;
      }
      int length = t.GetValue().length();
      int tokenType = 0;
      switch (symbol->GetSymbolType())
      {
      case stProcedure:
        tokenType = SemanticTokenTypes::Function;
        break;
      case stConstant:
        tokenType = SemanticTokenTypes::Variable;
        break;
      case stParam:
        tokenType = SemanticTokenTypes::Parameter;
        break;
      default:
        tokenType = SemanticTokenTypes::Variable;
      }
      data.push_back(deltaLine);
      data.push_back(deltaStart);
      data.push_back(length);
      data.push_back(tokenType);
      data.push_back(0);
    }
  }
  ConsoleLog("found " + std::to_string(data.size()) + " semantic tokens");
  response["result"] = {{"data", data}};
  ConsoleLog("semantic tokens:"+response.dump());
  SendResponse(response);
}
int main()
{
  while (true)
  {

    std::string content;
    if (std::cin.eof() || !std::cin.good())
      break;
    // read header
    int content_length = 0;
    while (true)
    {
      if (std::cin.eof() || !std::cin.good())
        break;
      std::string header;
      std::getline(std::cin, header);
      if (header == "\r")
        break;
      if (header.find("Content-Length: ") == 0)
      {
        content_length = std::stoi(header.substr(16));
      }
    }
    // read content
    for (int i = 0; i < content_length; i++)
    {
      if (std::cin.eof() || !std::cin.good())
        break;
      char c;
      std::cin.get(c);
      content += c;
    }
    ConsoleLog("got content:" + content);
    // parse json
    try
    {
      auto request = json::parse(content);
      auto method = request["method"].get<std::string>();
      if (method == "initialize")
      {
        json legend = json::object();
        legend["tokenTypes"] = {"namespace", "type", "class", "enum", "interface", "struct", "typeParameter", "parameter", "variable", "property", "enumMember", "event", "function", "method", "macro", "keyword", "modifier", "comment", "string", "number", "regexp", "operator", "decorator"};
        legend["tokenModifiers"] = {"declaration", "definition", "readonly", "static", "deprecated", "abstract", "async", "modification", "documentation", "defaultLibrary"};
        json semanticTokensProvider = json::object();
        semanticTokensProvider["legend"] = legend;
        semanticTokensProvider["full"] = true;
        semanticTokensProvider["range"] = false;
        // listen openclose
        json textDocumentSync = json::object();
        textDocumentSync["openClose"] = true;
        textDocumentSync["change"] = 1;
        json response;
        response["result"] = {{"capabilities", {{"textDocumentSync", textDocumentSync}, {"definitionProvider", true}, {"semanticTokensProvider", semanticTokensProvider},
         {"referencesProvider", true}, {"hoverProvider", true}, {"renameProvider", true}
        , {"documentSymbolProvider", true}, {"workspaceSymbolProvider", true}, {"documentFormattingProvider", true}, {"documentRangeFormattingProvider", true}, {"completionProvider", {{"resolveProvider", false}, {"triggerCharacters", {"."}}}}, {"documentOnTypeFormattingProvider", {{"firstTriggerCharacter", "{"}, {"moreTriggerCharacter", {"}"}}}}}}};
        response["id"] = request["id"];
        SendResponse(response);
        ConsoleLog("Init done");
        ConsoleLog(semanticTokensProvider.dump());
        continue;
      }
      if (method == "textDocument/didChange")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        codes[uri] =
            request["params"]["contentChanges"][0]["text"].get<std::string>();
        Validate(uri);
        continue;
      }
      if (method == "textDocument/didOpen")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        codes[uri] =
            request["params"]["textDocument"]["text"].get<std::string>();
        Validate(uri);
        continue;
      }
      // didClose
      if (method == "textDocument/didClose")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        codes.erase(uri);
        nodes.erase(uri);
        // if empty, exit
        if (codes.size() == 0)
        {
          return 0;
        }
        continue;
      }
      // definition
      if (method == "textDocument/definition")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        auto line = request["params"]["position"]["line"].get<int>() + 1;
        auto character = request["params"]["position"]["character"].get<int>() + 1;
        GoToDefinition(request["id"], uri, line, character);
        continue;
      }
      // references
      if (method == "textDocument/references")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        auto line = request["params"]["position"]["line"].get<int>() + 1;
        auto character = request["params"]["position"]["character"].get<int>() + 1;

        FindReferences(request["id"], uri, line, character);
        continue;
      }
      // hover
      if (method == "textDocument/hover")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        auto line = request["params"]["position"]["line"].get<int>() + 1;
        auto character = request["params"]["position"]["character"].get<int>() + 1;
        Hover(request["id"], uri, line, character);
        continue;
      }
      // completion
      if (method == "textDocument/completion")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        auto line = request["params"]["position"]["line"].get<int>() + 1;
        auto character = request["params"]["position"]["character"].get<int>() + 1;
        Completion(request["id"], uri, line, character);
        continue;
      }
      // documentSymbol
      if (method == "textDocument/documentSymbol")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        DocumentSymbol(request["id"], uri);
        continue;
      }
      // rename
      if (method == "textDocument/rename")
      {
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        auto line = request["params"]["position"]["line"].get<int>() + 1;
        auto character = request["params"]["position"]["character"].get<int>() + 1;
        auto newName = request["params"]["newName"].get<std::string>();
        Rename(request["id"], uri, line, character, newName);
        continue;
      }
      // semantic tokens (full)
      if (method == "textDocument/semanticTokens/full")
      {
        ConsoleLog("semantic tokens full");
        auto uri = request["params"]["textDocument"]["uri"].get<std::string>();
        SemanticTokensFull(request["id"], uri);
        continue;
      }
    }
    catch (std::exception &e)
    {
      ConsoleLog("json parsing error:" + std::string(e.what()), LogLevel::Error);
      continue;
    }
  }
  ConsoleLog("exiting");
}