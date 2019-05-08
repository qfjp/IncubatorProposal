#include <gsl/gsl_statistics_int.h>
#include <stdio.h>
#include "parser.h"
#include "graph.h"

GHashTable *node_mapping;

int main(int argc, char** argv)
{
    char *fname;
    if (argc != 2) {
        fname = "test.csv";
    } else {
        fname = argv[1];
    }

    node_mapping = g_hash_table_new_full(g_int_hash, g_int_equal, free, free);
    igraph_t graph;
    parse_file(&graph, node_mapping, fname);

    for (int i = 0; i < igraph_vcount(&graph); i++) {
        igraph_vector_t neighbs;
        igraph_vector_init(&neighbs, 0);

        igraph_neighbors(&graph, &neighbs, i, IGRAPH_OUT);
        for (int j = 0; j < igraph_vector_size(&neighbs); j++) {
            printf("%d -> %d\n", i, (int) igraph_vector_e(&neighbs, j));
        }
    }
    igraph_vector_t *influencers = highly_followed(&graph);

    printf("Influencer nodes:\n");
    printf("[");
    int size = igraph_vector_size(influencers);
    for (int i = 0; i < size - 1; i++) {
        printf("%d, ", (int) igraph_vector_e(influencers, i));
    }
    printf("%d]\n", (int) igraph_vector_e(influencers, size - 1));


    g_hash_table_destroy(node_mapping);
    return 0;
}
