#ifndef C_VER_INC_PARSER_H_
#define C_VER_INC_PARSER_H_

#include <glib-2.0/glib.h>
#include <igraph/igraph.h>

struct GHashTable;
struct igraph_t;

void parse_file(igraph_t *graph, char *fname, GHashTable *node_mapping);

#endif
