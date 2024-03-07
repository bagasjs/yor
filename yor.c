#include <assert.h>
#include <ctype.h>
#include <string.h>
#include <stdint.h>
#include "helper.h"
#include <stdbool.h>

typedef struct Location {
    size_t row, col;
} Location;

typedef enum Token_Kind {
    TOK_UNKNOWN = 0,
    TOK_NAME,
    TOK_STRING,
    TOK_INTEGER,
    TOK_TRUE,
    TOK_FALSE,

    TOK_SWAP,
    TOK_COPY,
    TOK_OVER,
    TOK_DROP,

    TOK_ADD,
    TOK_EQ,
    TOK_NE,
    TOK_GE,
    TOK_GT,
    TOK_LE,
    TOK_LT,

    TOK_IF,
    TOK_ELIF,
    TOK_ELSE,
    TOK_WHILE,
    TOK_DO,
    TOK_END,

    TOK_PROC,
    TOK_EXTERN,

    TOK_EOF,

    COUNT_TOKENS,
} Token_Kind;

struct Token_Info {
    Token_Kind kind;
    const char *name;
};

static struct Token_Info token_infos[COUNT_TOKENS] = {
    [TOK_UNKNOWN] = { .kind = TOK_UNKNOWN, .name = "TOK_UNKNOWN", },
    [TOK_NAME] = { .kind = TOK_NAME, .name = "TOK_NAME", },
    [TOK_STRING] = { .kind = TOK_STRING, .name = "TOK_STRING", },
    [TOK_INTEGER] = { .kind = TOK_INTEGER, .name = "TOK_INTEGER", },
    [TOK_TRUE] = { .kind = TOK_TRUE, .name = "TOK_TRUE", },
    [TOK_FALSE] = { .kind = TOK_FALSE, .name = "TOK_FALSE", },

    [TOK_SWAP] = { .kind = TOK_SWAP, .name = "TOK_SWAP", },
    [TOK_COPY] = { .kind = TOK_COPY, .name = "TOK_COPY", },
    [TOK_OVER] = { .kind = TOK_OVER, .name = "TOK_OVER", },
    [TOK_DROP] = { .kind = TOK_DROP, .name = "TOK_DROP", },

    [TOK_ADD] = { .kind = TOK_ADD, .name = "TOK_ADD", },
    [TOK_EQ] = { .kind = TOK_EQ, .name = "TOK_EQ", },
    [TOK_NE] = { .kind = TOK_NE, .name = "TOK_NE", },
    [TOK_GT] = { .kind = TOK_GT, .name = "TOK_GT", },
    [TOK_GE] = { .kind = TOK_GE, .name = "TOK_GE", },
    [TOK_LT] = { .kind = TOK_LT, .name = "TOK_LT", },
    [TOK_LE] = { .kind = TOK_LE, .name = "TOK_LE", },

    [TOK_IF] = { .kind = TOK_IF, .name = "TOK_IF", },
    [TOK_ELIF] = { .kind = TOK_ELIF, .name = "TOK_ELIF", },
    [TOK_ELSE] = { .kind = TOK_ELSE, .name = "TOK_ELSE", },
    [TOK_WHILE] = { .kind = TOK_WHILE, .name = "TOK_WHILE", },
    [TOK_DO] = { .kind = TOK_DO, .name = "TOK_DO", },
    [TOK_END] = { .kind = TOK_END, .name = "TOK_END", },

    [TOK_PROC] = { .kind = TOK_PROC, .name = "TOK_PROC", },
    [TOK_EXTERN] = { .kind = TOK_EXTERN, .name = "TOK_EXTERN", },

    [TOK_EOF] = { .kind = TOK_EOF, .name = "TOK_EOF", },
};

typedef struct Token {
    Token_Kind kind;
    String_View text;
    Location loc;
} Token;

typedef struct Lex_State  {
    size_t i;
    size_t cc;
    String_View source;
    Location current;
    struct {
        Token data[255];
        uint16_t top;
    } token_stack;
} Lex_State;

bool lex_advance(Lex_State *lexer)
{
    if(lexer->cc == '\n') {
        lexer->current.row += 1;
        lexer->current.col = 0;
    } else {
        lexer->current.col += 1;
    }
    lexer->i += 1;
    if(lexer->i >= lexer->source.length + 1) return false;
    lexer->cc = lexer->source.ptr[lexer->i];
    return true;
}

bool lex_tokenize(Lex_State *lexer, Token *result)
{
    while(isspace(lexer->cc) && lex_advance(lexer));

    if(lexer->cc == '#')  {
        while(lexer->cc != '\n' && lexer->cc != '\0') lex_advance(lexer);
        return lex_tokenize(lexer, result);
    }

    switch(lexer->cc) {
        case '\0':
            {
                result->loc = lexer->current;
                result->kind = TOK_EOF;
                result->text = sv_slice(lexer->source, lexer->i, lexer->i + 1);
            } break;
        case '+':
            {
                result->loc = lexer->current;
                result->kind = TOK_ADD;
                result->text = sv_slice(lexer->source, lexer->i, lexer->i + 1);
            } break;
        case '"':
            {
                lex_advance(lexer);
                size_t start = lexer->i;
                while(lexer->cc != '"' && lexer->i < lexer->source.length) 
                    lex_advance(lexer);
                result->loc = lexer->current;
                result->kind = TOK_STRING;
                result->text = sv_slice(lexer->source, start, lexer->i);
                lex_advance(lexer);
            } break;
        default:
            {
                if(isdigit(lexer->cc)) {
                    size_t start;
                    start = lexer->i;
                    while(isdigit(lexer->cc) && lexer->i < lexer->source.length) lex_advance(lexer);
                    result->loc = lexer->current;
                    result->kind = TOK_INTEGER;
                    result->text = sv_slice(lexer->source, start, lexer->i);
                } else {
                    size_t start;
                    start = lexer->i;
                    while(!isspace(lexer->cc) && lexer->i < lexer->source.length) {
                        lex_advance(lexer);
                    }
                    result->loc = lexer->current;
                    result->text = sv_slice(lexer->source, start, lexer->i);

                    if(sv_eq(result->text, SV("=="))) {
                        result->kind = TOK_EQ;
                    } else if(sv_eq(result->text, SV("!="))) {
                        result->kind = TOK_NE;
                    } else if(sv_eq(result->text, SV(">"))) {
                        result->kind = TOK_GT;
                    } else if(sv_eq(result->text, SV(">="))) {
                        result->kind = TOK_GE;
                    } else if(sv_eq(result->text, SV("<"))) {
                        result->kind = TOK_LT;
                    } else if(sv_eq(result->text, SV("<="))) {
                        result->kind = TOK_LE;
                    } else if(sv_eq(result->text, SV("swap"))) {
                        result->kind = TOK_SWAP;
                    } else if(sv_eq(result->text, SV("copy"))) {
                        result->kind = TOK_COPY;
                    } else if(sv_eq(result->text, SV("over"))) {
                        result->kind = TOK_OVER;
                    } else if(sv_eq(result->text, SV("drop"))) {
                        result->kind = TOK_DROP;
                    } else if(sv_eq(result->text, SV("if"))) {
                        result->kind = TOK_IF;
                    } else if(sv_eq(result->text, SV("elif"))) {
                        result->kind = TOK_ELIF;
                    } else if(sv_eq(result->text, SV("else"))) {
                        result->kind = TOK_ELSE;
                    } else if(sv_eq(result->text, SV("while"))) {
                        result->kind = TOK_WHILE;
                    } else if(sv_eq(result->text, SV("do"))) {
                        result->kind = TOK_DO;
                    } else if(sv_eq(result->text, SV("end"))) {
                        result->kind = TOK_END;
                    } else if(sv_eq(result->text, SV("proc"))) {
                        result->kind = TOK_PROC;
                    } else if(sv_eq(result->text, SV("extern"))) {
                        result->kind = TOK_EXTERN;
                    } else if(sv_eq(result->text, SV("true"))) {
                        result->kind = TOK_TRUE;
                    } else if(sv_eq(result->text, SV("false"))) {
                        result->kind = TOK_FALSE;
                    } else {
                        result->kind = TOK_NAME;
                    }
                }
            } break;
    }

    return lex_advance(lexer);
}

bool lex_peek(Lex_State *lex, Token *result, size_t i)
{
    while(lex->token_stack.top < i) {
        if(!lex_tokenize(lex, &lex->token_stack.data[lex->token_stack.top])) return false;
        lex->token_stack.top += 1;
    }
    *result = lex->token_stack.data[lex->token_stack.top - i];
    return result->kind != TOK_EOF || result->kind != TOK_UNKNOWN;
}

bool lex_next(Lex_State *lex, Token *result)
{
    if(lex->token_stack.top < 1) {
        if(!lex_tokenize(lex, &lex->token_stack.data[lex->token_stack.top])) return false;
        lex->token_stack.top += 1;
    } 
    *result = lex->token_stack.data[lex->token_stack.top - 1];
    lex->token_stack.top -= 1;
    return result->kind != TOK_EOF || result->kind != TOK_UNKNOWN;
}

typedef enum Opcodes {
    OP_UNKNOWN = 0,
    OP_SWAP,
    OP_COPY,
    OP_OVER,
    OP_DROP,
    OP_ADD,
    OP_EQ,
    OP_NE,
    OP_LT,
    OP_LE,
    OP_GT,
    OP_GE,

    OP_LOADB,
    OP_LOADW,
    OP_STOREB,
    OP_STOREW,

    /** x86 only */
    OP_SYSCALL4,

    COUNT_OPCODES,
} Opcodes;

typedef enum Inst_Kind {
    INST_UNKNOWN = 0,
    INST_PUSH_INT,
    INST_PUSH_STRING,
    INST_PUSH_BOOL,

    INST_OPERATION,
    INST_IF,
    INST_ELIF,
    INST_ELSE,
    INST_DO,
    INST_END,
    INST_WHILE,

    INST_DEFINE_PROC,
    INST_CALL_PROC,
    INST_EXTERNAL_PROC,

    COUNT_INSTRUCTIONS,
} Inst_Kind;

struct Inst_Info {
    Inst_Kind kind;
    const char *name;
};

static struct Inst_Info inst_infos[COUNT_INSTRUCTIONS] = {
    [INST_UNKNOWN] = {0},
    [INST_PUSH_INT] = { .kind = INST_PUSH_INT, .name = "INST_PUSH_INT", },
    [INST_PUSH_STRING] = { .kind = INST_PUSH_STRING, .name = "INST_PUSH_STRING", },
    [INST_PUSH_BOOL] = { .kind = INST_PUSH_BOOL, .name = "INST_PUSH_BOOL", },

    [INST_OPERATION] = { .kind = INST_OPERATION, .name = "INST_OPERATION", },
    [INST_IF] = { .kind = INST_IF, .name = "INST_IF", },
    [INST_ELIF] = { .kind = INST_ELIF, .name = "INST_ELIF", },
    [INST_ELSE] = { .kind = INST_ELSE, .name = "INST_ELSE", },
    [INST_DO] = { .kind = INST_DO, .name = "INST_DO", },
    [INST_END] = { .kind = INST_END, .name = "INST_END", },
    [INST_WHILE] = { .kind = INST_WHILE, .name = "INST_WHILE", },
    [INST_DEFINE_PROC] = { .kind = INST_DEFINE_PROC, .name = "INST_DEFINE_PROC", },
    [INST_CALL_PROC] = { .kind = INST_CALL_PROC, .name = "INST_CALL_PROC", },
    [INST_EXTERNAL_PROC] = { .kind = INST_EXTERNAL_PROC, .name = "INST_EXTERNAL_PROC", },
};

typedef enum Data_Type {
    DATA_TYPE_VOID = 0,
    DATA_TYPE_INT,
    DATA_TYPE_PTR,
    DATA_TYPE_BOOL,
    COUNT_DATA_TYPE
} Data_Type;

struct Data_Type_Info {
    Data_Type type;
    const char *name;
};

static struct Data_Type_Info dt_infos[COUNT_DATA_TYPE] = {
    [DATA_TYPE_VOID] = { .type=DATA_TYPE_VOID, .name="void" },
    [DATA_TYPE_INT] = { .type=DATA_TYPE_INT, .name="int" },
    [DATA_TYPE_PTR] = { .type=DATA_TYPE_PTR, .name="ptr" },
    [DATA_TYPE_BOOL] = { .type=DATA_TYPE_BOOL, .name="bool" },
};

typedef struct Proc {
    String_View name;
    da(Data_Type) params;
    Data_Type return_type;
    bool external;
} Proc;

typedef struct Jump_Target {
    size_t parent;

    size_t next;
    size_t self;
    size_t prev;
} Jump_Target;

typedef struct Inst_As_End {
    bool is_end_of_a_process;
    union {
        Data_Type return_type;
        Jump_Target jt;
    };
} Inst_As_End;

typedef struct Inst_As_Do {
    bool is_do_of_a_process;
    union {
        Jump_Target jt;
    };
} Inst_As_Do;

typedef struct Inst {
    Inst_Kind kind;
    union {
        int64_t push_int;
        bool push_bool;
        Opcodes operation;
        String_View push_string;
        Jump_Target _if;
        Jump_Target _while;
        Jump_Target _else;
        Jump_Target _elif;
        Inst_As_Do  _do;
        Inst_As_End _end;
        Proc _def_proc;
        Proc _external_proc;
        Proc _call_proc;
    } as;
} Inst;

bool parse_instruction(Lex_State *lex, Inst *result)
{
    Token token;
    if(!lex_next(lex, &token)) return false;

    switch(token.kind) {
        case TOK_INTEGER:
            {
                result->as.push_int = sv_to_int(token.text);
                result->kind = INST_PUSH_INT;
            } break;
        case TOK_STRING:
            {
                result->as.push_string = token.text;
                result->kind = INST_PUSH_STRING;
            } break;
        case TOK_FALSE:
            {
                result->as.push_bool = false;
                result->kind = INST_PUSH_BOOL;
            } break;
        case TOK_TRUE:
            {
                result->as.push_bool = true;
                result->kind = INST_PUSH_BOOL;
            } break;
        case TOK_IF:
            {
                result->kind = INST_IF;
            } break;
        case TOK_ELIF:
            {
                result->kind = INST_ELIF;
            } break;
        case TOK_ELSE:
            {
                result->kind = INST_ELSE;
            } break;
        case TOK_WHILE:
            {
                result->kind = INST_WHILE;
            } break;
        case TOK_END:
            {
                result->kind = INST_END;
            } break;
        case TOK_DO:
            {
                result->kind = INST_DO;
            } break;

        case TOK_SWAP:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_SWAP;
            } break;
        case TOK_COPY:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_COPY;
            } break;
        case TOK_OVER:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_OVER;
            } break;
        case TOK_DROP:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_DROP;
            } break;

        case TOK_ADD:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_ADD;
            } break;
        case TOK_EQ:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_EQ;
            } break;
        case TOK_NE:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_NE;
            } break;
        case TOK_LT:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_LT;
            } break;
        case TOK_LE:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_LE;
            } break;
        case TOK_GT:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_GT;
            } break;
        case TOK_GE:
            {
                result->kind = INST_OPERATION;
                result->as.operation = OP_GE;
            } break;
        case TOK_PROC:
            {
                Proc proc = {0};
                assert(lex_next(lex, &token) && "Expecting a TOK_NAME after TOK_PROC");
                proc.name = token.text;
                proc.return_type = DATA_TYPE_VOID;
                bool return_is_void = false;
                while(lex_peek(lex, &token, 1) && token.kind != TOK_DO) {
                    bool type_is_exist = false;
                    for(Data_Type i = 0; i < COUNT_DATA_TYPE; ++i) {
                        struct Data_Type_Info info = dt_infos[i];
                        if(sv_eq(token.text, SV(info.name))) {
                            type_is_exist = true;
                            return_is_void = i == DATA_TYPE_VOID;
                            if(!return_is_void) {
                                da_append(&proc.params, i);
                            }
                            break;
                        }
                    }
                    assert(type_is_exist && "Unknown data type for function arguments");
                    lex_next(lex, &token);
                }

                if(!return_is_void && proc.params.count > 0) {
                    proc.return_type = proc.params.data[proc.params.count - 1];
                    proc.params.count -= 1;
                }

                result->kind = INST_DEFINE_PROC;
                result->as._def_proc = proc;
            } break;
        case TOK_EXTERN:
            {
                Proc proc = {0};
                proc.external = true;
                assert(lex_next(lex, &token) && "Expecting token `proc` after `extern`");
                assert(token.kind == TOK_PROC && "Expecting a `proc` after `extern`");

                assert(lex_next(lex, &token) && "Expecting name after `proc`");
                assert(token.kind == TOK_NAME && "Expecting a name after `proc`");

                proc.name = token.text;
                proc.return_type = DATA_TYPE_VOID;
                bool return_is_void = false;
                while(lex_next(lex, &token) && token.kind != TOK_END) {
                    bool type_is_exist = false;
                    for(Data_Type i = 0; i < COUNT_DATA_TYPE; ++i) {
                        struct Data_Type_Info info = dt_infos[i];
                        if(sv_eq(token.text, SV(info.name))) {
                            type_is_exist = true;
                            return_is_void = i == DATA_TYPE_VOID;
                            if(!return_is_void) {
                                da_append(&proc.params, i);
                            }
                            break;
                        }
                    }
                    printf("[INFO] TOKEN: "SV_FMT"\n", SV_ARGV(token.text));
                    assert(type_is_exist && "Unknown data type for function arguments");
                }

                if(!return_is_void && proc.params.count > 0) {
                    proc.return_type = proc.params.data[proc.params.count - 1];
                    proc.params.count -= 1;
                }

                result->kind = INST_EXTERNAL_PROC;
                result->as._external_proc = proc;
            } break;
        case TOK_NAME:
            {
                result->kind = INST_UNKNOWN;
                if(sv_eq(token.text, SV("lb"))) {
                    result->kind = INST_OPERATION;
                    result->as.operation = OP_LOADB;
                } else if(sv_eq(token.text, SV("sb"))) {
                    result->kind = INST_OPERATION;
                    result->as.operation = OP_STOREB;
                } else if(sv_eq(token.text, SV("syscall4"))) {
                    result->kind = INST_OPERATION;
                    result->as.operation = OP_SYSCALL4;
                } else {
                    result->kind = INST_CALL_PROC;
                    result->as._call_proc.name = token.text;
                }
            } break;
        default:
            {
                result->kind = INST_UNKNOWN;
            } break;
    }
    return true;
}

#define JUMP_TARGET_STACK_CAP 256

typedef struct Program {
    struct {
        size_t count, capacity;
        Inst *data;
    } insts;
    struct {
        Jump_Target data[JUMP_TARGET_STACK_CAP];
        size_t count;
    } jump_target_stack;
} Program;

void push_jump_target(Program *state, const Jump_Target jt)
{
    assert(state->jump_target_stack.count < JUMP_TARGET_STACK_CAP);
    state->jump_target_stack.data[state->jump_target_stack.count] = jt;
    state->jump_target_stack.count += 1;
}

Jump_Target pop_jump_target(Program *program)
{
    assert(program->jump_target_stack.count > 0);
    Jump_Target target = program->jump_target_stack.data[program->jump_target_stack.count - 1];
    program->jump_target_stack.count -= 1;
    return target;
}

Program load_program_from_source(const char *source)
{
    Program result;
    Lex_State lexer;

    lexer.source.length = strlen(source);
    lexer.source.ptr = source;
    lexer.i = 0;
    lexer.cc = lexer.source.ptr[lexer.i];
    lexer.current.col = 0;
    lexer.current.row = 0;
    lexer.token_stack.top = 0;

    result.insts.count = 0;
    result.insts.capacity = 0;
    result.jump_target_stack.count = 0;

    // Load the program
    Inst inst;
    while(parse_instruction(&lexer, &inst)) 
        da_append(&result.insts, inst);

    da(Proc) procs;

    // Link all target that requires non-linear execution
    for(size_t self = 0; self < result.insts.count; ++self) {
        Inst *inst = &result.insts.data[self];

        switch(inst->kind) {
            case INST_EXTERNAL_PROC:
                {
                    da_append(&procs, inst->as._external_proc);
                    printf("PUSHING EXTERNAL PROC: "SV_FMT"\n", SV_ARGV(inst->as._external_proc.name));
                } break;
            case INST_DEFINE_PROC:
                {
                    Jump_Target jt;
                    jt.parent = self;
                    jt.prev = 0;
                    jt.self = self;
                    jt.next = 0;
                    da_append(&procs, inst->as._def_proc);
                    push_jump_target(&result, jt);
                } break;

            case INST_CALL_PROC:
                {
                    bool handled = false;
                    for(size_t i = 0; i < procs.count; ++i) {
                        Proc proc = procs.data[i];
                        if(sv_eq(inst->as._call_proc.name, proc.name)) {
                            inst->as._call_proc = proc;
                            handled = true;
                            break;
                        }
                    }
                    printf("[INFO]: Searching for process "SV_FMT"\n", SV_ARGV(inst->as._call_proc.name));
                    assert(handled && "Undefined process");
                } break;

            case INST_IF:
                {
                    inst->as._if.parent = self;
                    inst->as._if.prev = 0;
                    inst->as._if.self = self;
                    inst->as._if.next = 0;
                    push_jump_target(&result, inst->as._if);
                } break;

            case INST_ELSE:
                {
                    Jump_Target target = pop_jump_target(&result);
                    inst->as._else.parent = target.parent;
                    inst->as._else.prev = target.self;
                    inst->as._else.self = self;
                    inst->as._else.next = 0;
                    assert(result.insts.data[inst->as._else.prev].kind == INST_DO
                            && "The previous of `else` must be `do` block instruction");
                    result.insts.data[inst->as._else.prev].as._do.jt.next = self;
                    push_jump_target(&result, inst->as._else);
                } break;

            case INST_ELIF:
                {
                    Jump_Target target = pop_jump_target(&result);
                    inst->as._elif.parent = target.parent;
                    inst->as._elif.prev = target.self;
                    inst->as._elif.self = self;
                    inst->as._elif.next = 0;
                    assert(result.insts.data[inst->as._elif.prev].kind == INST_DO
                            && "The previous of `elif` must be `do` block instruction");
                    result.insts.data[inst->as._elif.prev].as._do.jt.next = self;
                    push_jump_target(&result, inst->as._elif);
                } break;

            case INST_WHILE:
                {
                    inst->as._while.parent = self;
                    inst->as._while.self = self;
                    inst->as._while.prev = 0;
                    inst->as._while.next = 0;
                    push_jump_target(&result, inst->as._while);
                } break;

            case INST_DO:
                {
                    Jump_Target target = pop_jump_target(&result);
                    inst->as._do.is_do_of_a_process = false;
                    inst->as._do.jt.parent = target.parent;
                    inst->as._do.jt.prev = target.self;
                    inst->as._do.jt.self = self;
                    inst->as._do.jt.next = 0;

                    switch(result.insts.data[inst->as._do.jt.prev].kind) {
                        case INST_IF:
                            result.insts.data[inst->as._do.jt.prev].as._if.next = self;
                            break;
                        case INST_ELIF:
                            result.insts.data[inst->as._do.jt.prev].as._elif.next = self;
                            break;
                        case INST_WHILE:
                            result.insts.data[inst->as._do.jt.prev].as._elif.next = self;
                            break;
                        case INST_DEFINE_PROC:
                            inst->as._do.is_do_of_a_process = true;
                            break;
                        default:
                            assert(0 && "The previous of `do` must be `if`, `proc` `elif`, or `while` block instruction");
                            break;
                    }

                    push_jump_target(&result, inst->as._do.jt);
                } break;
                
            case INST_END:
                {
                    Jump_Target target = pop_jump_target(&result);
                    inst->as._end.is_end_of_a_process = false;
                    inst->as._end.jt.parent = target.parent;
                    inst->as._end.jt.prev = target.self;
                    inst->as._end.jt.self = self;
                    inst->as._end.jt.next = inst->as._end.jt.parent; // The next of `end` inst is always its parent

                    switch(result.insts.data[inst->as._end.jt.prev].kind) {
                        case INST_DO:
                            result.insts.data[inst->as._end.jt.prev].as._do.jt.next = self; 
                            break;
                        case INST_ELSE:
                            result.insts.data[inst->as._end.jt.prev].as._else.next = self; 
                            break;
                        default:
                            assert(0 && "The previous of `end` must be `do` or `else` block instruction");
                            break;
                    }

                    switch(result.insts.data[inst->as._end.jt.parent].kind) {
                        case INST_IF:
                            // The prev of `if` is always `end` (simplified)
                            result.insts.data[inst->as._end.jt.parent].as._if.prev = self;
                            break;
                        case INST_WHILE:
                            // The prev of `while` is always `end` (simplified)
                            result.insts.data[inst->as._end.jt.parent].as._while.prev = self;
                            break;
                        case INST_DEFINE_PROC:
                            inst->as._end.is_end_of_a_process = true;
                            inst->as._end.return_type =  
                                result.insts.data[inst->as._end.jt.parent].as._def_proc.return_type;
                            break;
                        default:
                            assert(0 && "The valid parent of `end` must be `if`, `proc` or `while` block instruction");
                            break;
                    }
                } break;

            default:
                break;
        }
    }

    assert(result.jump_target_stack.count == 0 && "Not all jump target is handled");

    return result;
}

void unload_program(Program program)
{
    da_free(&program.insts);
}

void type_check_program(const Program *program)
{
    #define DATA_TYPE_STACK_CAP (32*1024)
    Data_Type stack[DATA_TYPE_STACK_CAP];
    size_t sp = 0;
    #define PUSH(dt) do { assert(sp < DATA_TYPE_STACK_CAP && "Stack overflow"); stack[sp++] = (dt); } while(0)
    #define POP(dt) do { assert(sp > 0 && "Stack underflow"); dt = stack[--sp]; } while(0)
    #define EXPECT(dt) do {                                             \
        assert(sp > 0 && "Stack underflow but expecting data type "#dt);\
        assert(stack[--sp] == (dt) && "Expecting data type " #dt);      \
    } while(0);

    size_t block_stack[1024] = {0};
    size_t bssp = 0;
    Data_Type x, y;

    for(size_t i = 0; i < program->insts.count; ++i) {
        Inst *inst = &program->insts.data[i];
        switch(inst->kind) {
            case INST_PUSH_INT: 
                {
                    PUSH(DATA_TYPE_INT);
                } break;
            case INST_PUSH_STRING:
                {
                    PUSH(DATA_TYPE_PTR);
                } break;
            case INST_PUSH_BOOL:
                {
                    PUSH(DATA_TYPE_BOOL);
                } break;
            case INST_IF:
                {
                } break;
            case INST_ELSE:
                {
                } break;
            case INST_ELIF:
                {
                } break;
            case INST_WHILE:
                {
                } break;
            case INST_DO:
                {
                    if(inst->as._do.is_do_of_a_process) {
                    } else {
                        EXPECT(DATA_TYPE_BOOL);
                        block_stack[bssp] = sp;
                        bssp++;
                    }
                } break;
            case INST_END:
                {
                    if(inst->as._end.is_end_of_a_process) {
                        if(inst->as._end.return_type != DATA_TYPE_VOID) 
                            POP(inst->as._end.return_type);
                    } else {
                        assert(bssp > 0 && "You aren't in any scope");
                        assert(block_stack[bssp - 1] == sp && "This scope makes the stack overflow");
                        sp = block_stack[bssp - 1];
                        bssp -= 1;
                    }
                } break;
            case INST_EXTERNAL_PROC:
                {
                } break;
            case INST_DEFINE_PROC:
                {
                    for(int i = (int)inst->as._def_proc.params.count - 1; i >= 0; --i) 
                        PUSH(inst->as._def_proc.params.data[i]);
                } break;
            case INST_CALL_PROC:
                {
                    Proc call = inst->as._call_proc;
                    for(int i = (int)call.params.count - 1; i >= 0; --i) {
                        EXPECT(call.params.data[i]);
                    }
                    if(call.return_type != DATA_TYPE_VOID) 
                        PUSH(call.return_type);
                } break;
            case INST_OPERATION:
                {
                    switch(inst->as.operation) {
                        case OP_SWAP:
                            {
                                POP(x);
                                POP(y);
                                PUSH(x);
                                PUSH(y);
                            } break;
                        case OP_COPY:
                            {
                                POP(x);
                                PUSH(x);
                                PUSH(x);
                            } break;
                        case OP_OVER:
                            {
                                POP(x);
                                POP(y);
                                PUSH(y);
                                PUSH(x);
                                PUSH(y);
                            } break;
                        case OP_DROP:
                            {
                                POP(x);
                            } break;
                        case OP_ADD:
                            {
                                POP(x);
                                EXPECT(DATA_TYPE_INT);
                                PUSH(x);
                            } break;

                        case OP_EQ:
                        case OP_NE:
                        case OP_LT:
                        case OP_LE:
                        case OP_GT:
                        case OP_GE:
                            {
                                POP(x);
                                POP(x);
                                PUSH(DATA_TYPE_BOOL);
                            } break;

                        case OP_LOADB:
                        case OP_STOREB:
                        case OP_SYSCALL4:
                        default:
                            {
                                assert(0 && "Unreachable operations compile_instruction_nasm_x86_64()");
                            } break;
                    }
                } break;
            default:
                break;
        }

        printf("--- %s ---\n", inst_infos[inst->kind].name);
        for(size_t j = 0; j < sp; ++j) 
            printf("[%zu] %s\n", j, dt_infos[stack[j]].name);
    }

    assert(sp == 0 && "Stack is not empty");
}

int compile_program_to_nasm_x86_64(const Program *program, FILE *f)
{

    fprintf(f, "section .text\n");
    fprintf(f, "global _start\n");
    static const char *param_regs[] = { "rdi", "rsi", "rdx", "r10", "r8", };
    da(String_View) strings = {0};

    for(size_t i = 0; i < program->insts.count; ++i) {
        Inst inst = program->insts.data[i];
        switch(inst.kind) {
            case INST_PUSH_INT:
                {
                    fprintf(f, "    ;; --- push_int --- \n");
                    fprintf(f, "    mov rax, %ld\n", inst.as.push_int);
                    fprintf(f, "    push rax\n");
                } break;
            case INST_PUSH_STRING:
                {
                    fprintf(f, "    ;; --- push_string --- \n");
                    fprintf(f, "    push str_%zu\n", strings.count);
                    da_append(&strings, inst.as.push_string);
                } break;
            case INST_PUSH_BOOL:
                {
                    fprintf(f, "    ;; --- push_bool --- \n");
                    fprintf(f, "    mov rax, %d\n", inst.as.push_bool);
                    fprintf(f, "    push rax\n");
                } break;
            case INST_IF:
                {
                    fprintf(f, "    ;; --- if --- \n");
                } break;
            case INST_ELSE:
                {
                    fprintf(f, "    ;; --- else --- \n");
                    fprintf(f, "    jmp .addr_%zu\n", inst.as._else.next);
                    fprintf(f, "    .addr_%zu:\n", inst.as._else.self);
                } break;
            case INST_ELIF:
                {
                    fprintf(f, "    ;; --- elif --- \n");
                    // The previous of if is always end
                    fprintf(f, "    jmp .addr_%zu\n", program->insts.data[inst.as._elif.parent].as._if.prev);
                    fprintf(f, "    .addr_%zu:\n", inst.as._elif.self);
                } break;
            case INST_WHILE:
                {
                    fprintf(f, "    ;; --- while --- \n");
                    fprintf(f, "    .addr_%zu: \n", inst.as._while.self);
                } break;
            case INST_DO:
                {
                    fprintf(f, "    ;; --- do --- \n");
                    if(inst.as._do.is_do_of_a_process) {
                    } else {
                        fprintf(f, "    pop rax\n");
                        fprintf(f, "    cmp rax, 1\n");
                        fprintf(f, "    jne .addr_%zu\n", inst.as._do.jt.next);
                    }
                } break;
            case INST_END:
                {
                    fprintf(f, "    ;; --- end --- \n");
                    if(inst.as._end.is_end_of_a_process) {
                        if(inst.as._end.return_type != DATA_TYPE_VOID) 
                            fprintf(f, "    pop rax ;; -- return value\n");
                        fprintf(f, "    pop rbp\n");
                        fprintf(f, "    ret\n");
                    } else {
                        if(program->insts.data[inst.as._end.jt.next].kind == INST_WHILE) 
                            fprintf(f, "    jmp .addr_%zu\n", inst.as._end.jt.next);
                        fprintf(f, "    .addr_%zu: \n", inst.as._end.jt.self);
                    }
                } break;
            case INST_EXTERNAL_PROC:
                {
                    fprintf(f, "extern "SV_FMT"\n", SV_ARGV(inst.as._external_proc.name));
                } break;
            case INST_DEFINE_PROC:
                {
                    fprintf(f, ";; --- define proc `"SV_FMT"` --- \n", SV_ARGV(inst.as._def_proc.name));
                    fprintf(f, SV_FMT":\n", SV_ARGV(inst.as._def_proc.name));
                    fprintf(f, "    push rbp\n");
                    fprintf(f, "    mov  rbp, rsp\n");
                    assert(inst.as._def_proc.params.count <= 5 && "Currently a process could only have 5 parameters");
                    for(int i = (int)inst.as._def_proc.params.count - 1; i >= 0; --i) 
                        fprintf(f, "    push %s\n", param_regs[i]);
                } break;
            case INST_CALL_PROC:
                {
                    fprintf(f, "    ;; --- call proc --- \n");
                    fprintf(f, "    mov rax, 0\n");
                    for(int i = (int)inst.as._def_proc.params.count - 1; i >= 0; --i) 
                        fprintf(f, "    pop %s\n", param_regs[i]);
                    fprintf(f, "    call "SV_FMT"\n", SV_ARGV(inst.as._call_proc.name));
                    if(inst.as._call_proc.return_type != DATA_TYPE_VOID) 
                        fprintf(f, "    push rax;; -- add the return value\n");
                } break;
            case INST_OPERATION:
                {
                    fprintf(f, "    ;; --- operation (%d) --- \n", inst.as.operation);
                    switch(inst.as.operation) {
                        case OP_SWAP:
                            {
                                fprintf(f, "    pop  rax\n");
                                fprintf(f, "    pop  rbx\n");
                                fprintf(f, "    push  rax\n");
                                fprintf(f, "    push  rbx\n");
                            } break;
                        case OP_COPY:
                            {
                                fprintf(f, "    pop  rax\n");
                                fprintf(f, "    push rax\n");
                                fprintf(f, "    push rax\n");
                            } break;
                        case OP_OVER:
                            {
                                fprintf(f, "    pop  rax\n");
                                fprintf(f, "    pop  rbx\n");
                                fprintf(f, "    push rbx\n");
                                fprintf(f, "    push rax\n");
                                fprintf(f, "    push rbx\n");
                            } break;
                        case OP_DROP:
                            {
                                fprintf(f, "    pop rax\n");
                            } break;
                        case OP_ADD:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    add rax, rbx\n");
                                fprintf(f, "    push rax\n");
                            } break;
                        case OP_EQ:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    cmp rax, rbx\n");
                                fprintf(f, "    mov rcx, 0\n");
                                fprintf(f, "    mov rdx, 1\n");
                                fprintf(f, "    cmove rcx, rdx\n");
                                fprintf(f, "    push rcx\n");
                            } break;
                        case OP_NE:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    cmp rax, rbx\n");
                                fprintf(f, "    mov rcx, 0\n");
                                fprintf(f, "    mov rdx, 1\n");
                                fprintf(f, "    cmovne rcx, rdx\n");
                                fprintf(f, "    push rcx\n");
                            } break;
                        case OP_LT:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    cmp rax, rbx\n");
                                fprintf(f, "    mov rcx, 0\n");
                                fprintf(f, "    mov rdx, 1\n");
                                fprintf(f, "    cmovl rcx, rdx\n");
                                fprintf(f, "    push rcx\n");
                            } break;
                        case OP_LE:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    cmp rax, rbx\n");
                                fprintf(f, "    mov rcx, 0\n");
                                fprintf(f, "    mov rdx, 1\n");
                                fprintf(f, "    cmovle rcx, rdx\n");
                                fprintf(f, "    push rcx\n");
                            } break;
                        case OP_GT:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    cmp rax, rbx\n");
                                fprintf(f, "    mov rcx, 0\n");
                                fprintf(f, "    mov rdx, 1\n");
                                fprintf(f, "    cmovg rcx, rdx\n");
                                fprintf(f, "    push rcx\n");
                            } break;
                        case OP_GE:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    cmp rax, rbx\n");
                                fprintf(f, "    mov rcx, 0\n");
                                fprintf(f, "    mov rdx, 1\n");
                                fprintf(f, "    cmovge rcx, rdx\n");
                                fprintf(f, "    push rcx\n");
                            } break;

                        case OP_LOADB:
                            {
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    xor rbx, rbx\n");
                                fprintf(f, "    mov bl, BYTE [rax]\n");
                                fprintf(f, "    push rbx\n");
                            } break;
                        case OP_STOREB:
                            {
                                fprintf(f, "    pop rbx\n");
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    mov [rax], bl\n");
                            } break;

                        case OP_SYSCALL4:
                            {
                                fprintf(f, "    pop rax\n");
                                fprintf(f, "    pop rdi\n");
                                fprintf(f, "    pop rsi\n");
                                fprintf(f, "    pop rdx\n");
                                fprintf(f, "    syscall\n");
                            } break;

                        default:
                            {
                                assert(0 && "Unreachable operations compile_instruction_nasm_x86_64()");
                            } break;
                    }
                } break;
            default:
                {
                    assert(0 && "Unreachable at compile_instruction_nasm_x86_64()");
                } break;
        }
    }

    fprintf(f, "_start:\n");
    fprintf(f, "    mov rax, 0\n");
    fprintf(f, "    call main\n");
    fprintf(f, "    mov rdi, rax\n");
    fprintf(f, "    mov rax, 60\n");
    fprintf(f, "    syscall\n");

    fprintf(f, "section .data\n");
    for(size_t i = 0; i < strings.count; ++i) {
        fprintf(f, "str_%zu:\n", i);
        fprintf(f, "db \""SV_FMT"\", 0\n", SV_ARGV(strings.data[i]));
    }

    return 0;
}

int main(int argc, char **argv)
{
    const char *file_path;
    char *file_content;

    assert(argc > 1 && "Please provide a file path");
    file_path = argv[1];

    file_content = load_file_text(file_path);
    assert(file_content != NULL && "Failed to load file content");

    Program program = load_program_from_source(file_content);
    type_check_program(&program);

    FILE *f = fopen("output.s", "w");
    assert(f && "Failed to open output file");
    int result = compile_program_to_nasm_x86_64(&program, f);
    unload_program(program);
    return result;
}
