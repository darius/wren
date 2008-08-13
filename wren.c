#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/* Configuration */

enum {
  /* Capacity in bytes. */
  store_capacity = 4096,

  /* True iff voluminous tracing is wanted. */
  loud = 0,
};

/* Pick the definition that goes with the endianness of your computer.
   (Yucko, sorry.)
   I've used the first one on PowerPC Mac (big-endian) and the second
   on Linux x86 (little-endian), with gcc both times.  XXX recode this
   without the machine-dependent bitfields instead. */
#if 0
# define PRIM_HEADER(opcode, arity, name_length) \
    a_primitive<<6, opcode, ((arity)<<4|(name_length))
#else
# define PRIM_HEADER(opcode, arity, name_length) \
    (opcode)<<2|a_primitive, 0, ((name_length)<<4|(arity))
#endif

/* Type of a Wren-language value. */
typedef int Value;


/* Error state */

static const char *complaint = NULL;

static void complain (const char *msg)
{
  if (!complaint)
    complaint = msg;
}


/* Main data store in RAM

   Most of the memory we use is doled out of one block.

   From the top, growing downwards, is a dictionary: a stack of 
   header/name pairs. The header distinguishes the kind of name and
   what address it denotes, along with the length of the name.

   From the bottom, growing upwards, are the bindings of the names:
   the code, for procedures, or the data cell, for globals. (Locals
   denote an offset in a transient stack frame. We'd have interleaved
   the dictionary headers with the values, like in Forth, except the
   entries for locals would get in the way while we're compiling the
   body of a procedure; moving all of the headers out of the way was
   the simplest solution.)

   At runtime, the stack grows down from the bottom of the dictionary
   (but 32-bit word-aligned). 
*/

typedef enum { a_primitive, a_procedure, a_global, a_local } NameKind;
typedef struct Header Header;
struct Header {
  unsigned kind:        2;
  unsigned binding:    14;
  unsigned arity:       4;
  unsigned name_length: 4;
  unsigned char name[0];
} __attribute__((packed));  /* XXX gcc dependency */

static unsigned char the_store[store_capacity];
#define store_end  (the_store + store_capacity)

/* We make compiler_ptr accessible as a global variable to Wren code;
   it's located in the first Value cell of the_store. (See
   primitive_dictionary, below.) This requires that
   sizeof (unsigned char *) == sizeof (Value). Sorry!
   (If you change Value to a short type, then change compiler_ptr to a
   short offset from the_store instead of a pointer type.
 */
#define compiler_ptr ( *(unsigned char **) the_store )

static unsigned char *dictionary_ptr = store_end;

static int available (unsigned amount)
{
  if (compiler_ptr + amount <= dictionary_ptr)
    return 1;
  complain ("Store exhausted");
  return 0;
}

static const unsigned char *next_header (const unsigned char *header)
{
  const Header *h = (const Header *) header;
  return h->name + h->name_length;
}

static Header *bind (const char *name, unsigned length, 
                     NameKind kind, unsigned binding, unsigned arity)
{
  assert (name);
  assert (length < (1<<4));
  assert (kind <= a_local);
  assert (binding < (1<<14));
  assert (arity < (1<<4));
  if (available (sizeof (Header) + length))
    {
      dictionary_ptr -= sizeof (Header) + length;
      {
        Header *h = (Header *) dictionary_ptr;
        h->kind = kind;
        h->binding = binding;
        h->arity = arity;
        h->name_length = length;
        memcpy (h->name, name, length);
        return h;
      }
    }
  return NULL;
}

static const Header *lookup (const unsigned char *dict, 
                             const unsigned char *end,
                             const char *name, unsigned length)
{
  for (; dict < end; dict = next_header (dict))
    {
      const Header *h = (const Header *) dict;
      if (h->name_length == length && 0 == memcmp (h->name, name, length))
        return h;
    }
  return NULL;
}

#ifndef NDEBUG
#if 0
static void dump (const unsigned char *dict, 
                  const unsigned char *end)
{
  for (; dict < end; dict = next_header (dict))
    {
      const Header *h = (const Header *) dict;
      printf ("  %*.*s\t%x %x %x\n", 
              h->name_length, h->name_length, h->name, 
              h->kind, h->binding, h->arity);
    }
}
#endif
#endif


/* The virtual machine */

typedef unsigned char Instruc;

enum {
  HALT,
  PUSH, POP, PUSH_STRING,
  GLOBAL_FETCH, GLOBAL_STORE, LOCAL_FETCH,
  CALL, RETURN,
  BRANCH, JUMP,
  ADD, SUB, MUL, DIV, MOD, UMUL, UDIV, UMOD, NEGATE,
  EQ, LT, ULT,
  AND, OR, XOR, SLA, SRA, SRL,
  GETC, PUTC,
  FETCH_BYTE, PEEK, POKE,
};

#ifndef NDEBUG
static const char *opcode_names[] = {
  "HALT",
  "PUSH", "POP", "PUSH_STRING",
  "GLOBAL_FETCH", "GLOBAL_STORE", "LOCAL_FETCH",
  "CALL", "RETURN",
  "BRANCH", "JUMP",
  "ADD", "SUB", "MUL", "DIV", "MOD", "UMUL", "UDIV", "UMOD", "NEGATE",
  "EQ", "LT", "ULT",
  "AND", "OR", "XOR", "SLA", "SRA", "SRL",
  "GETC", "PUTC",
  "FETCH_BYTE", "PEEK", "POKE",
};
#endif

static const unsigned char primitive_dictionary[] = 
  {
    PRIM_HEADER(UMUL, 2, 4), 'u', 'm', 'u', 'l',
    PRIM_HEADER(UDIV, 2, 4), 'u', 'd', 'i', 'v',
    PRIM_HEADER(UMOD, 2, 4), 'u', 'm', 'o', 'd',
    PRIM_HEADER(ULT,  2, 3), 'u', 'l', 't',
    PRIM_HEADER(SLA,  2, 3), 's', 'l', 'a',
    PRIM_HEADER(SRA,  2, 3), 's', 'r', 'a',
    PRIM_HEADER(SRL,  2, 3), 's', 'r', 'l',
    PRIM_HEADER(GETC, 0, 4), 'g', 'e', 't', 'c',
    PRIM_HEADER(PUTC, 1, 4), 'p', 'u', 't', 'c',
    PRIM_HEADER(PEEK, 1, 4), 'p', 'e', 'e', 'k',
    PRIM_HEADER(POKE, 1, 4), 'p', 'o', 'k', 'e',
  };

#ifndef NDEBUG
#if 0
static void dump_dictionary (void)
{
  printf ("dictionary:\n");
  dump (dictionary_ptr, store_end);
  dump (primitive_dictionary,
        primitive_dictionary + sizeof primitive_dictionary);
}
#endif
#endif

/* Run VM code starting at 'pc', with the stack allocated the space between
   'end' and dictionary_ptr. Return the result on top of the stack. */
static Value run (const Instruc *pc, const Instruc *end)
{
  /* Stack pointer and base pointer 
     Initially just above the first free aligned Value cell below
     the dictionary. */
  Value *sp = (Value *) (((unsigned)dictionary_ptr) & ~(sizeof (Value) - 1));
  Value *bp = sp;

#define need(n)                                        \
  do {                                                 \
    if ((unsigned char *)sp - (n)*sizeof(Value) < end) \
      goto stack_overflow;                             \
  } while (0)

  for (;;)
    {
#ifndef NDEBUG
  if (loud)
    printf ("%u\t%s\n", pc - the_store, opcode_names[*pc]);
#endif
      
    switch (*pc++)
      {
      case HALT:
        return sp[0];
        break;

      case PUSH: 
        need (1);
        *--sp = *(Value*)pc;
        pc += sizeof (Value);
        break;

      case POP:
        ++sp;
        break;

      case PUSH_STRING:
        need (1);
        *--sp = (Value)pc;
        /* N.B. this op is slower the longer the string is! */
        pc += strlen ((const char *)pc) + 1;
        break;

      case GLOBAL_FETCH:
        need (1);
        *--sp = *(Value *)(the_store + *(unsigned short *)pc);
        pc += sizeof (unsigned short);
        break;

      case GLOBAL_STORE:
        *(Value *)(the_store + *(unsigned short *)pc) = sp[0];
        pc += sizeof (unsigned short);
        break;

      case LOCAL_FETCH:
        need (1);
        *--sp = bp[-*pc++];
        break;

/* A stack frame looks like this:
     bp[0]: leftmost argument
            (This is also where the return value will go.)
     ...
     bp[-(n-1)]: rightmost argument (where n is the number of arguments)
     bp[-n]: pair of old bp and return address (in two half-words)
     ...temporaries...
     sp[0]: topmost temporary

   The bp could be dispensed with, but it simplifies the compiler and VM
   interpreter slightly, and ought to make basic debugging support
   significantly simpler, and if we were going to make every stack slot be
   32 bits wide then we don't even waste any extra space.

   By the time we return, there's only one temporary in this frame:
   the return value. Thus, &bp[-n] == &sp[1] at this time, and the 
   RETURN instruction doesn't need to know the value of n. CALL,
   otoh, does. It looks like <CALL> <n> <addr-byte-1> <addr-byte-2>.
 */ 
      case CALL:
        {
          /* Optimize tail calls.

             Why doesn't the compiler emit a tail-call instruction instead
             of us checking this at runtime? Because I don't see how it
             could without some greater expense there: when we finish parsing
             a function with lots of if-then-else branches, we may discover
             only then that a bunch of calls we've compiled were in tail
             position. 

             (Maybe that expense would be worth incurring, though, for the
             sake of smaller compiled code.)
           */
          const Instruc *cont = pc + 1 + sizeof (unsigned short);
          while (*cont == JUMP)
            {
              ++cont;
              cont += *(unsigned short *)cont;
            }
          if (*cont == RETURN)
            {
              /* This is a tail call. Reuse the current frame. */
              unsigned char n = pc[0];
          /* XXX portability: this assumes two unsigned shorts fit in a Value */
              Value frame_info = sp[n];
              memmove (bp, sp, n * sizeof (Value));
              sp = bp - n;
              sp[0] = frame_info;
            }
          else
            {
              /* This is a non-tail call. Build a new frame. */ 
              need (1);
              --sp;
              {
          /* XXX portability: this assumes two unsigned shorts fit in a Value */
                unsigned short *f = (unsigned short *)sp;
                f[0] = (unsigned char *)bp - the_store;
                f[1] = cont - the_store;
                bp = sp + pc[0];
              }
            }
          pc = the_store + *(unsigned short *)(pc + 1);
        }
        break;

      case RETURN:
        {
          Value result = sp[0];
          unsigned short *f = (unsigned short *)(sp + 1);
          sp = bp;
          bp = (Value *)(the_store + f[0]);
          pc = the_store + f[1];
          sp[0] = result;
        }
        break;

      case BRANCH:
        if (0 == *sp++)
          pc += *(unsigned short *)pc;
        else
          pc += sizeof (unsigned short);
        break;

      case JUMP:
        pc += *(unsigned short *)pc;
        break;

      case ADD:  sp[1] += sp[0]; ++sp; break;
      case SUB:  sp[1] -= sp[0]; ++sp; break;
      case MUL:  sp[1] *= sp[0]; ++sp; break;
      case DIV:  sp[1] /= sp[0]; ++sp; break;
      case MOD:  sp[1] %= sp[0]; ++sp; break;
      case UMUL: sp[1] = (unsigned)sp[1] * (unsigned)sp[0]; ++sp; break;
      case UDIV: sp[1] = (unsigned)sp[1] / (unsigned)sp[0]; ++sp; break;
      case UMOD: sp[1] = (unsigned)sp[1] % (unsigned)sp[0]; ++sp; break;
      case NEGATE: sp[0] = -sp[0]; break;

      case EQ:   sp[1] = sp[1] == sp[0]; ++sp; break;
      case LT:   sp[1] = sp[1] < sp[0];  ++sp; break;
      case ULT:  sp[1] = (unsigned)sp[1] < (unsigned)sp[0]; ++sp; break;

      case AND:  sp[1] &= sp[0]; ++sp; break;
      case OR:   sp[1] |= sp[0]; ++sp; break;
      case XOR:  sp[1] ^= sp[0]; ++sp; break;

      case SLA:  sp[1] <<= sp[0]; ++sp; break;
      case SRA:  sp[1] >>= sp[0]; ++sp; break;
      case SRL:  sp[1] = (unsigned)sp[1] >> (unsigned)sp[0]; ++sp; break;

      case GETC:
        need (1);
        *--sp = getc (stdin);
        break;

      case PUTC:
        putc (sp[0], stdout);
        break;

      case FETCH_BYTE:
        /* XXX boundschecking */
        sp[0] = *(unsigned char *)(sp[0]);;
        break;

      case PEEK:
        sp[0] = *(Value *)(sp[0]);;
        break;

      case POKE:
        *(Value *)(sp[1]) = sp[0];
        ++sp;
        break;

      default: assert (0);
      }
    }

 stack_overflow:
  complain ("Stack overflow");
  return 0;
}


/* The 'assembler' */

static Instruc *prev_instruc = NULL;

static void gen (Instruc opcode)
{
#ifndef NDEBUG
  if (loud)
    printf ("%u\t%s\n", compiler_ptr - the_store, opcode_names[opcode]);
#endif
  if (available (1))
    {
      prev_instruc = compiler_ptr;
      *compiler_ptr++ = opcode;
    }
}

static void gen_ubyte (unsigned char b)
{
  if (loud)
    printf ("%u\tubyte %u\n", compiler_ptr - the_store, b);
  if (available (1))
    *compiler_ptr++ = b;
}

static void gen_ushort (unsigned short u)
{
  if (loud)
    printf ("%u\tushort %u\n", compiler_ptr - the_store, u);
  if (available (sizeof u))
    {
      *(unsigned short *)compiler_ptr = u;
      compiler_ptr += sizeof u;
    }
}

static void gen_value (Value v)
{
  if (loud)
    printf ("%u\tvalue %d\n", compiler_ptr - the_store, v);
  if (available (sizeof v))
    {
      *(Value *)compiler_ptr = v;
      compiler_ptr += sizeof v;
    }
}

static Instruc *forward_ref (void)
{
  Instruc *ref = compiler_ptr;
  compiler_ptr += sizeof (unsigned short);
  return ref;
}

static void resolve (Instruc *ref)
{
  if (loud)
    printf ("%u\tresolved: %u\n", ref - the_store, compiler_ptr - ref);
  *(unsigned short *)ref = compiler_ptr - ref;
}


/* Scanning */

enum { unread = EOF - 1 };
static int input_char = unread;
static int token;
static Value token_value;
static char token_name[16];

static int ch (void)
{
  if (input_char == unread)
    input_char = getc (stdin);
  return input_char;
}

static void next_char (void)
{
  if (input_char != EOF)
    input_char = unread;
}

static void skip_line (void)
{
  while (ch () != '\n' && ch () != EOF)
    next_char ();
}

static unsigned hex_char_value (char c)
{
  return c <= '9' ? c - '0' : toupper (c) - ('A'-10);
}

static void next (void)
{
again:

  if (isdigit (ch ()))
    {
      token = PUSH;
      token_value = 0;
      do {
        /* XXX check for numeric overflow. It might save space to
           use strtol() and strtoul() instead. */
        token_value = 10 * token_value + ch () - '0';
        next_char ();
        if (ch () == 'x' && token_value == 0)
          {
            /* Oh, it's a hex literal, not decimal as we presumed. */
            next_char ();
            for (; isxdigit (ch ()); next_char ())
              token_value = 16 * token_value + hex_char_value (ch ());
            /* XXX check that there was at least one hex digit after the 'x' */
            break;
          }
      } while (isdigit (ch ()));
    }
  else if (isalpha (ch ()) || ch () == '_')
    {
      char *n = token_name;
      do {
        if (token_name + sizeof token_name == n + 1)
          {
            complain ("Identifier too long");
            break;
          }
        *n++ = ch ();
        next_char ();
      } while (isalnum (ch ()) || ch () == '_');
      *n++ = '\0';
      if (0 == strcmp (token_name, "then"))
        token = 't';  /* XXX gee, how mnemonic */
      else if (0 == strcmp (token_name, "forget"))
        token = 'o';
      else if (0 == strcmp (token_name, "let"))
        token = 'l';
      else if (0 == strcmp (token_name, "if"))
        token = 'i';
      else if (0 == strcmp (token_name, "fun"))
        token = 'f';
      else if (0 == strcmp (token_name, "else"))
        token = 'e';
      else
        token = 'a';
    }
  else
    switch (ch ())
      {
      case '\'':
        next_char ();
        {
          /* We need to stick this string somewhere; after reaching
             the parser, if successfully parsed, it would be compiled
             into the instruction stream right after the next opcode.
             So just put it there -- but don't yet update compiler_ptr. */
          unsigned char *s = compiler_ptr + 1;
          for (; ch () != '\''; next_char ())
            {
              if (ch () == EOF)
                {
                  complain ("Unterminated string");
                  token = EOF;
                  return;
                }
              if (!available (s + 2 - compiler_ptr))
                {
                  token = '\n';
                  return;
                }
              *s++ = ch ();
            }
          next_char ();
          *s = '\0';
          token = '\'';
        }
        break;

      case '+':
      case '-':
      case '*':
      case '/':
      case '%':
      case '<':
      case '&':
      case '|':
      case '^':
      case '(':
      case ')':
      case '=':
      case ':':
      case ';':
      case '\n':
      case EOF:
        token = ch ();
        next_char ();
        break;

      case ' ':
      case '\t':
      case '\r':
        next_char ();
        goto again;

      case '#':
        skip_line ();
        goto again;

      default:
        complain ("Lexical error");
        token = '\n';  /* XXX need more for error recovery */
        break;
      }
}


/* Parsing and compiling */

static int expect (unsigned char expected, const char *plaint)
{
  if (token == expected)
    return 1;
  complain (plaint);
  return 0;
}

static void skip_newline (void)
{
  while (!complaint && token == '\n')
    next ();
}

static void parse_expr (int precedence);

static void parse_arguments (unsigned arity)
{
  unsigned i;
  for (i = 0; i < arity; ++i)
    parse_expr (20); /* 20 is higher than any operator precedence */
}

/* XXX probably we should have a separate newlines-ok flag instead
   of pushing the precedence down here... */
static void parse_factor (int precedence)
{
  skip_newline ();
  switch (token)
    {
    case PUSH:
      gen (PUSH);
      gen_value (token_value);
      next ();
      break;

    case '\'':                  /* string constant */
      gen (PUSH_STRING);
      compiler_ptr += strlen ((const char *)compiler_ptr) + 1;
      next ();
      break;

    case 'a':                   /* identifier */
      {
        const Header *h = lookup (dictionary_ptr, store_end,
                                  token_name, strlen (token_name));
        if (!h)
          h = lookup (primitive_dictionary, 
                      primitive_dictionary + sizeof primitive_dictionary,
                      token_name, strlen (token_name));
        if (!h)
          complain ("Unknown identifier");
        else
          {
            next ();
            switch (h->kind)
              {
              case a_global:
                gen (GLOBAL_FETCH);
                gen_ushort (h->binding);
                break;

              case a_local:
                gen (LOCAL_FETCH);
                gen_ubyte (h->binding);
                break;

              case a_procedure:
                parse_arguments (h->arity);
                gen (CALL);
                gen_ubyte (h->arity);
                gen_ushort (h->binding);
                break;

              case a_primitive:
                parse_arguments (h->arity);
                gen (h->binding);
                break;

              default:
                assert (0);
              }
          }
      }
      break;

    case 'i':                   /* if-then-else */
      {
        Instruc *branch, *jump;
        next ();
        parse_expr (0);
        gen (BRANCH);
        branch = forward_ref ();
        skip_newline ();
        if (expect ('t', "Expected 'then'"))
          {
            next ();
            parse_expr (3);
            gen (JUMP);
            jump = forward_ref ();
            skip_newline ();
            if (expect ('e', "Expected 'else'"))
              {
                next ();
                resolve (branch);
                parse_expr (3);
                resolve (jump);
              }
          }
      }
      break;

    case '*':                   /* character fetch */
      next ();
      parse_factor (precedence);
      gen (FETCH_BYTE);
      break;

    case '-':                   /* unary minus */
      next ();
      parse_factor (precedence);
      gen (NEGATE);
      break;

    case '(':
      next ();
      parse_expr (0);
      if (expect (')', "Syntax error: expected ')'"))
        next ();
      break;

    default:
      complain ("Syntax error: expected a factor");
    }
}

static void parse_expr (int precedence) 
{
  if (complaint)
    return;
  parse_factor (precedence);
  while (!complaint)
    {
      int l, rator;   /* left precedence and operator */

      if (precedence == 0)
        skip_newline ();

      switch (token) {
      case ';': l = 1; rator = POP; break;

      case ':': l = 3; rator = GLOBAL_STORE; break;

      case '&': l = 5; rator = AND; break;
      case '|': l = 5; rator = OR;  break;
      case '^': l = 5; rator = XOR; break;

      case '<': l = 7; rator = LT;  break;
      case '=': l = 7; rator = EQ;  break;

      case '+': l = 9; rator = ADD; break;
      case '-': l = 9; rator = SUB; break;
        
      case '*': l = 11; rator = MUL; break;
      case '/': l = 11; rator = DIV; break;
      case '%': l = 11; rator = MOD; break;

      default: return;
      }

      if (l < precedence || complaint)
        return;

      next ();
      skip_newline ();
      if (rator == POP)
        gen (rator);
      else if (rator == GLOBAL_STORE)
        {
          if (prev_instruc && *prev_instruc == GLOBAL_FETCH)
            {
              unsigned short addr = *(unsigned short *)(prev_instruc + 1);
              compiler_ptr = prev_instruc;
              parse_expr (l);
              gen (GLOBAL_STORE);
              gen_ushort (addr);
              continue;
            }
          else
            {
              complain ("Not an l-value");
              break;
            }
        }
      parse_expr (l + 1);
      if (rator != POP)
        gen (rator);
    }
}

static void parse_done (void)
{
  if (token != EOF && token != '\n')
    complain ("Syntax error: unexpected token");
}

static Value scratch_expr (void)
{
  Instruc *start = compiler_ptr;
  parse_expr (-1);
  parse_done ();
  gen (HALT);
  {
    Instruc *end = compiler_ptr;
    compiler_ptr = start;
    return complaint ? 0 : run (start, end);
  }
}

static void run_expr (void)
{
  Value v = scratch_expr ();
  if (!complaint)
    printf ("%d\n", v);
}

static void run_let (void)
{
  if (expect ('a', "Expected identifier")
      && available (sizeof (Value)))
    {
      unsigned char *cell = compiler_ptr;
      gen_value (0);
      bind (token_name, strlen (token_name),
            a_global, cell - the_store, 0);
      next ();
      if (expect ('=', "Expected '='"))
        {
          next ();
          *(Value*)cell = scratch_expr ();
        }
    }
}

static void run_forget (void)
{
  if (expect ('a', "Expected identifier"))
    {
      const Header *h = lookup (dictionary_ptr, store_end,
                                token_name, strlen (token_name));
      if (!h)
        complain ("Unknown identifier");
      else if (h->kind != a_global && h->kind != a_procedure)
        complain ("Not a definition");
      next ();
      parse_done ();
      if (!complaint)
        {
          unsigned char *cp = the_store + h->binding;
          unsigned char *dp = 
            (unsigned char *) next_header ((const unsigned char *) h);
          if (the_store <= cp && cp <= dp && dp <= store_end)
            {
              compiler_ptr = cp;
              dictionary_ptr = dp;
            }
          else
            complain ("Dictionary corrupted");
        }
    }
}

static void run_fun (void)
{
  if (expect ('a', "Expected identifier"))
    {
      unsigned char *dp = dictionary_ptr;
      Header *f = bind (token_name, strlen (token_name),
                        a_procedure, compiler_ptr - the_store, 0);
      next ();
      if (f)
        {
          unsigned char *dp = dictionary_ptr;
          while (token == 'a')
            {
              /* XXX check for too many parameters */
              bind (token_name, strlen (token_name),
                    a_local, f->arity++, 0);
              next ();
            }
          if (expect ('=', "Expected '='"))
            {
              next ();
              parse_expr (-1);
              parse_done ();
              gen (RETURN);
            }
          dictionary_ptr = dp;  /* forget parameter names */
        }
      if (complaint)
        dictionary_ptr = dp;  /* forget function. XXX should also forget code */
    }
}

static void run_command (void)
{
  complaint = NULL;

  if (token == 'f')             /* 'fun' */
    {
      next ();
      run_fun ();
    }
  else if (token == 'l')        /* 'let' */
    {
      next ();
      run_let ();
    }
  else if (token == 'o')        /* 'forget' */
    {
      next ();
      run_forget ();
    }
  else
    run_expr ();

  if (complaint)
    {
      printf ("%s\n", complaint);
      skip_line ();  /* i.e., flush any buffered input, sort of */
      next ();
    }
}


/* The top level */

static void read_eval_print_loop (void)
{
  printf ("> ");
  next_char ();
  next ();
  while (token != EOF)
    {
      run_command ();
      printf ("> ");
      skip_newline ();
    }
  printf ("\n");
}

int main ()
{
  bind ("cp", 2, a_global, 0, 0);
  compiler_ptr = the_store + sizeof (Value);
  read_eval_print_loop ();
  return 0;
}
