#ifndef HELPER_H_
#define HELPER_H_

#ifdef __cplusplus
extern "C" {
#endif

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#ifndef ASSERT
#   include <assert.h>
#   define ASSERT assert
#endif

#if !defined(MALLOC) && !defined(FREE) && !defined(REALLOC)
#   include <stdlib.h>
#   define MALLOC malloc
#   define FREE free
#   define REALLOC realloc
#endif

#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SWAP(T, a, b) do { T tmp = b; b = a; a = tmp; } while(0)

#ifndef CAST
#define CAST(T, v) ((T)(v))
#endif

typedef struct String_View {
    const char *ptr;
    size_t length;
} String_View;

#define SV_ARGV(sv) (int)(sv).length, (sv).ptr
#define SV_FMT "%.*s"
#define SV_STATIC(cstr_lit) { .length = sizeof(cstr_lit) - 1, .ptr = (cstr_lit) }
#define SV(cstr_lit) (String_View){ .length = strlen(cstr_lit), .ptr = (cstr_lit) }

static String_View sv_slice(String_View src, size_t start, size_t end)
{
    String_View result;
    if(end < start) SWAP(size_t, start, end);
    result.ptr = &src.ptr[start];
    result.length = end - start;
    return result;
}

static int sv_to_int(String_View strv)
{
    bool is_negative = false;
    if(strv.ptr[0] == '-') {
        is_negative = true;
        strv.length -= 1;
        strv.ptr += 1;
    }
    int result = 0;
    for (size_t i = 0; i < strv.length && isdigit(strv.ptr[i]); ++i) {
        result = result * 10 + (int) strv.ptr[i] - '0';
    }
    if(is_negative) result *= -1;
    return result;
}

static bool sv_eq(String_View a, String_View b)
{
    if(a.length != b.length) 
        return false;

    for(size_t i = 0; i < b.length; ++i) {
        if(a.ptr[i] != b.ptr[i]) 
            return false;
    }
    return true;
}

static char *load_file_text(const char *file_path)
{
    FILE *f = fopen(file_path, "r");
    if(!f) return NULL;

    fseek(f, 0L, SEEK_END);
    size_t filesz = ftell(f);
    fseek(f, 0L, SEEK_SET);
    char *result = (char *)malloc(sizeof(char) * (filesz + 1));
    if(!result) {
        fclose(f);
        fprintf(stderr, "Failed to read file %s", file_path);
        return result;
    }

    size_t read_length = fread(result, sizeof(char), filesz, f);
    result[read_length] = '\0';
    fclose(f);
    return result;
}

// dynamic array macros
#define DA_INIT_CAPACITY 32

#define da(T) struct { T* data; size_t count, capacity; }
#define da_free(da) FREE((da)->data)
#define da_append(da, item) \
    do {                                                                \
        if((da)->count >= (da)->capacity) {                             \
            size_t new_capacity = (da)->capacity * 2;                   \
            if(new_capacity == 0) new_capacity = DA_INIT_CAPACITY;      \
            void *new_data = MALLOC(new_capacity * sizeof(*(da)->data));\
            ASSERT(new_data && "Buy more RAM LOL!");                    \
            memcpy(new_data, (da)->data,                                \
                (da)->count * sizeof(*(da)->data));                     \
            FREE((da)->data);                                           \
            (da)->data = new_data;                                      \
            (da)->capacity = new_capacity;                              \
        }                                                               \
        (da)->data[(da)->count++] = (item);                             \
    } while(0)

#define da_append_many(da, new_items, new_items_count) \
    do {                                                                \
        if((da)->count + new_items_count > (da)->capacity) {            \
            if((da)->capacity == 0) (da)->capacity = DA_INIT_CAPACITY;  \
            new_capacity = (da)->capacity * 2 + new_items_count;        \
            void *new_data = MALLOC(new_capacity * sizeof(*(da)->data); \
            ASSERT(new_data && "Buy more RAM LOL!");                    \
            memcpy(new_data, (da)->data,                                \
                (da)->count * sizeof(*(da)->data));                     \
            FREE((da)->data);                                           \
            (da)->data = new_data;                                      \
            (da)->capacity = new_capacity;                              \
        }                                                               \
        memcpy((da)->data + (da)->count, new_items,                     \
                new_items_count * sizeof(*(da)->data));                 \
        (da)->count += new_items_count;                                 \
    } while(0)

#ifdef __cplusplus
}
#endif

#endif // HELPER_H_
