#ifndef C_VER_INC_PARSER_H_
#define C_VER_INC_PARSER_H_

#include <glib-2.0/glib.h>
#include <igraph/igraph.h>

/**
 * Parses a file consisting of a list of edges.
 *
 * An `edge' in this case is represented by a string "v1 v2", where v1
 * and v2 are nodes in the graph, and the edge is directed towards v2.
 *
 * The output graph constrains nodes to a contiguous range [0..n - 1]
 * where n is the number of nodes. If the input graph does not obey
 * this already, then nodes will be relabeled. This function is not
 * stable, so relabeling may occur even if the set of nodes is already
 * labelled contiguously. The renaming is saved and output as
 * node_mapping.
 *
 * @param[out] graph The resultant graph.
 * @param[out] node_mapping The renaming table, where keys are the
 * original nodes and values are the new value.
 * @param[in] fname The name of the file to parse.
 */
void parse_file(igraph_t *graph, GHashTable *node_mapping, char *fname);

#endif
