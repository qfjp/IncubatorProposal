#include <igraph/igraph.h>
#include <glib-2.0/glib.h>
#include <stdlib.h>

void parse_file(igraph_t *graph, char *fname, GHashTable *node_mapping) {
    FILE *infile;
    infile = fopen(fname, "r");

    igraph_empty(graph, 0, 1);
    igraph_vector_t edges;
    igraph_vector_init(&edges, 0);

    guint freshest_num = 0;

    int result = 0;
    while (1) {
        guint32 *from = malloc(sizeof(guint32));
        guint32 *to = malloc(sizeof(guint32));
        result = fscanf(infile, "%u %u\n", from, to);
        if (result < 0) break;

        // Map both nodes to new "freshest", if they don't already
        // have a map
        if (!g_hash_table_contains(node_mapping, from)) {
            guint32 *fresh = malloc(sizeof(guint32));
            *fresh = freshest_num++;
            g_hash_table_insert(node_mapping, from, fresh);
        }
        if (!g_hash_table_contains(node_mapping, to)) {
            guint32 *fresh = malloc(sizeof(guint32));
            *fresh = freshest_num++;
            g_hash_table_insert(node_mapping, to, fresh);
        }
        guint32 from_v = *((guint32*) g_hash_table_lookup(node_mapping, from));
        guint32 to_v = *((guint32*) g_hash_table_lookup(node_mapping, to));

        // Add the pair to the edge vector
        igraph_vector_push_back(&edges, from_v);
        igraph_vector_push_back(&edges, to_v);
    }
    GList *new_nodes = g_hash_table_get_values(node_mapping);
    guint num_nodes = g_list_length(new_nodes);

    igraph_add_vertices(graph, num_nodes, 0);
    igraph_add_edges(graph, &edges, 0);

    g_list_free(new_nodes);
}
