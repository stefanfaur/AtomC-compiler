#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "lexer.h"
#include "utils.h"

Token *tokens; // single linked list of tokens
Token *lastTk; // the last token in list

int line = 1; // the current line in the input file
int column = 1; // the current column in the input file
char **lines; // the lines from the input file

char *getTokenType(enum LexAtom code) {
  switch (code) {
  case ID:
    return "ID";
  case TYPE_CHAR:
    return "TYPE_CHAR";
  case TYPE_DOUBLE:
    return "TYPE_DOUBLE";
  case ELSE:
    return "ELSE";
  case IF:
    return "IF";
  case TYPE_INT:
    return "TYPE_INT";
  case RETURN:
    return "RETURN";
  case STRUCT:
    return "STRUCT";
  case VOID:
    return "VOID";
  case WHILE:
    return "WHILE";
  case INT:
    return "INT";
  case DOUBLE:
    return "DOUBLE";
  case CHAR:
    return "CHAR";
  case STRING:
    return "STRING";
  case COMMA:
    return "COMMA";
  case SEMICOLON:
    return "SEMICOLON";
  case LPAR:
    return "LPAR";
  case RPAR:
    return "RPAR";
  case LBRACKET:
    return "LBRACKET";
  case RBRACKET:
    return "RBRACKET";
  case LACC:
    return "LACC";
  case RACC:
    return "RACC";
  case END:
    return "END";
  case ADD:
    return "ADD";
  case SUB:
    return "SUB";
  case MUL:
    return "MUL";
  case DIV:
    return "DIV";
  case DOT:
    return "DOT";
  case AND:
    return "AND";
  case OR:
    return "OR";
  case NOT:
    return "NOT";
  case ASSIGN:
    return "ASSIGN";
  case EQUAL:
    return "EQUAL";
  case NOTEQ:
    return "NOTEQ";
  case LESS:
    return "LESS";
  case LESSEQ:
    return "LESSEQ";
  case GREATER:
    return "GREATER";
  case GREATEREQ:
    return "GREATEREQ";
  default:
    return "UNKNOWN_TOKEN";
  }
}

int countDots(char *str) {
  int count = 0;
  for (int i = 0; i < strlen(str); i++) {
    if (str[i] == '.') {
      count++;
    }
  }
  return (count <= 1);
}

// adds a token to the end of the tokens list and returns it
// sets its code and line
Token *addTk(int code) {
  Token *tk = safeAlloc(sizeof(Token));
  tk->code = code;
  tk->line = line;
  tk->col = column;
  tk->next = NULL;
  tk->lineText = lines[line - 1];
  if (lastTk) {
    lastTk->next = tk;
  } else {
    tokens = tk;
  }
  lastTk = tk;
  return tk;
}

char *extract(const char *begin, const char *end) {
  int length = end - begin;
  char *tempStr = safeAlloc((length + 1) * sizeof(char));
  memcpy(tempStr, begin, length);
  tempStr[length] = '\0';
  return tempStr;
}

char **extractLines(const char *inbuf, int *lineCount) {
  int capacity = 10; // Initial capacity for the array of lines
  char **lines = safeAlloc(capacity * sizeof(char *));
  // copy the input buffer to a new buffer
  char *buf = safeAlloc(strlen(inbuf) + 1);
  memcpy(buf, inbuf, strlen(inbuf) + 1);

  *lineCount = 0;                        // Initialize line count
  char *currentLine = strtok(buf, "\n"); // Tokenize the input on newlines

  while (currentLine != NULL) {
    if (*lineCount >= capacity) {
      // Need more space
      capacity *= 2;
      char **newLines = realloc(lines, capacity * sizeof(char *));
      if (!newLines) {
        perror("Failed to reallocate memory for lines");
        free(lines);
        return NULL;
      }
      lines = newLines;
    }

    lines[*lineCount] =
        malloc(strlen(currentLine) + 1); // +1 for the null terminator
    if (!lines[*lineCount]) {
      perror("Failed to allocate memory for a line");
      // Free previously allocated lines
      for (int i = 0; i < *lineCount; i++) {
        free(lines[i]);
      }
      free(lines);
      return NULL;
    }
    strcpy(lines[*lineCount], currentLine);
    (*lineCount)++;

    currentLine = strtok(NULL, "\n"); // Get the next line
  }
  return lines;
}

Token *tokenize(const char *pch) {
  int lineNr = 0;
  lines = extractLines(pch, &lineNr);
  const char *start;
  const char *lineStart = pch;
  Token *tk;
  for (;;) {
    column = (pch - lineStart) + 1;
    switch (*pch) {
    case ' ':
    case '\t':
      pch++;
      break;
    case '\r': // handles different kinds of newlines (Windows: \r\n, Linux: \n,
               // MacOS, OS X: \r or \n)
      if (pch[1] == '\n')
        pch++;
      // fallthrough to \n
    case '\n':
      line++;
      pch++;
      lineStart = pch; // update line start since we are on a new line
      break;
    case '\0':
      addTk(END);
      return tokens;
    case ',':
      addTk(COMMA);
      pch++;
      break;
    case '=':
      if (pch[1] == '=') {
        addTk(EQUAL);
        pch += 2;
      } else {
        addTk(ASSIGN);
        pch++;
      }
      break;
    case '/':
      if (pch[1] == '/') {
        // addTk(LINECOMMENT); this is not actually a token, I just have to skip
        // it
        pch += 2;
        // skip chars until newline
        while (*pch && *pch != '\n' && *pch != '\r')
          pch++;
      } else {
        addTk(DIV);
        pch++;
      }
      break;
    case '(':
      addTk(LPAR);
      pch++;
      break;
    case ')':
      addTk(RPAR);
      pch++;
      break;
    case '[':
      addTk(LBRACKET);
      pch++;
      break;
    case ']':
      addTk(RBRACKET);
      pch++;
      break;
    case '{':
      addTk(LACC);
      pch++;
      break;
    case '}':
      addTk(RACC);
      pch++;
      break;
    case ';':
      addTk(SEMICOLON);
      pch++;
      break;
    // operators
    case '+':
      addTk(ADD);
      pch++;
      break;
    case '-':
      addTk(SUB);
      pch++;
      break;
    case '*':
      addTk(MUL);
      pch++;
      break;
    case '.': // is this for dot notation in structs?
      addTk(DOT);
      pch++;
      break;
    case '&':
      if (pch[1] == '&') {
        addTk(AND);
        pch += 2;
      } else {
        err("\'&\' did you mean: \'&&\'? at line %d", tk->line);
      }
      break;
    case '|':
      if (pch[1] == '|') {
        addTk(OR);
        pch += 2;
      } else {
        err("\'|\' did you mean: \'||\'? at line %d", tk->line);
      }
      break;
    case '!':
      if (pch[1] == '=') {
        addTk(NOTEQ);
        pch += 2;
      } else {
        addTk(NOT);
        pch++;
      }
      break;
    case '<':
      if (pch[1] == '=') {
        addTk(LESSEQ);
        pch += 2;
      } else {
        addTk(LESS);
        pch++;
      }
      break;
    case '>':
      if (pch[1] == '=') {
        addTk(GREATEREQ);
        pch += 2;
      } else {
        addTk(GREATER);
        pch++;
      }
      break;
    // constants: STRING, CHAR
    case '\"':
      for (start = pch++; *pch != '\"'; pch++) {
        if (*pch == '\0')
          err("String does not have terminator\n%s at line %d", start,
              tk->line);
      }
      pch++;
      char *text = extract(start, pch);
      tk = addTk(STRING);
      tk->text = removeQuotes(text);
      break;
    case '\'':
      for (start = pch++; *pch != '\''; pch++) {
        if (*pch == '\0')
          err("Char does not have terminator\n%c at line %d", start, tk->line);
      }
      pch++;
      if (pch - start > 3)
        err("Char %s contains more than one character at line %d",
            extract(start, pch), tk->line);
      if (pch - start == 2)
        err("Char is empty at line %d", tk->line);
      text = extract(start, pch);
      tk = addTk(CHAR);
      tk->c = removeQuotes(text)[0];
      break;
    default:
      if (isalpha(*pch) || *pch == '_') {
        for (start = pch++; isalnum(*pch) || *pch == '_'; pch++) {
        }
        char *text = extract(start, pch);
        // keywords
        if (strcmp(text, "char") == 0)
          addTk(TYPE_CHAR);
        else if (strcmp(text, "double") == 0)
          addTk(TYPE_DOUBLE);
        else if (strcmp(text, "else") == 0)
          addTk(ELSE);
        else if (strcmp(text, "if") == 0)
          addTk(IF);
        else if (strcmp(text, "int") == 0)
          addTk(TYPE_INT);
        else if (strcmp(text, "return") == 0)
          addTk(RETURN);
        else if (strcmp(text, "struct") == 0)
          addTk(STRUCT);
        else if (strcmp(text, "void") == 0)
          addTk(VOID);
        else if (strcmp(text, "while") == 0)
          addTk(WHILE);
        // id
        else {
          tk = addTk(ID);
          tk->text = text;
        }
      } else if (isdigit(*pch)) {
        // constants: INT, DOUBLE
        int isDouble = 0;
        for (start = pch++; isdigit(*pch) || *pch == '.' || *pch == 'e' ||
                            *pch == 'E' || *pch == '-';
             pch++) {
          if (*pch == '.' || *pch == 'e' || *pch == 'E') {
            isDouble = 1;
            if (*pch == '.' && !isdigit(*(pch + 1)) && *(pch + 1) != 'e' &&
                *(pch + 1) != 'E') // after . we can only have digits or e/E
            {
              pch--; // error handling
              err("Double conversion error: incompatible character after "
                  "\"%c\" at line %d\n",
                  pch[0], tk->line);
            }
          }
          if (*pch == 'e' || *pch == 'E') {
            pch++;
            if (*pch != '-' && *pch != '+' &&
                !isdigit(*pch)) // after e/E we can only have -/+ or digits
            {
              pch--; // error handling
              err("Double conversion error: incompatible character after "
                  "\"%c\" at line %d\n",
                  pch[0], tk->line);
            }
          }
          if (*pch == '-' || *pch == '+') { // after - there can only be digits
            pch++;
            if (!isdigit(*pch)) {
              err("Double conversion error: incompatible character after sign "
                  "at line %d\n",
                  tk->line);
            }
          }
        }
        char *text = extract(start, pch);
        if (isDouble == 1) // when it is a double
        {
          if (!countDots(text)) {
            err("Invalid double at line %d\n", tk->line);
          }
          tk = addTk(DOUBLE);
          sscanf(text, "%lf", &tk->d);
        } else if (isDouble == 0) // INT
        {
          tk = addTk(INT);
          tk->i = atoi(text);
        } else {
          err("Invalid value for isDouble at line %d. How did we get here?\n",
              tk->line);
        }
      } else
        err("invalid character at line %d: %c (%d)\n", tk->line, *pch, *pch);
    }
  }
}

void showTokens(const Token *tokens) {
  for (const Token *tk = tokens; tk; tk = tk->next) {
    if (tk->code == STRING || tk->code == ID) {
      printf("%d    %s:%s\n", tk->line, getTokenType(tk->code), tk->text);
    } else if (tk->code == CHAR)
      printf("%d    %s:%c\n", tk->line, getTokenType(tk->code), tk->c);
    else if (tk->code == INT)
      printf("%d    %s:%d\n", tk->line, getTokenType(tk->code), tk->i);
    else if (tk->code == DOUBLE)
      printf("%d    %s:%f\n", tk->line, getTokenType(tk->code), tk->d);
    else
      printf("%d    %s\n", tk->line, getTokenType(tk->code));
  }
}
