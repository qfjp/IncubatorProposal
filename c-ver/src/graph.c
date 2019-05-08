#include "graph.h"
#include <gsl/gsl_statistics_double.h>

igraph_vector_t *highly_followed(igraph_t *graph) {
    igraph_vector_t sample_vector;
    igraph_vector_init(&sample_vector, 0);

    int tot_vertices = igraph_vcount(graph);

    for (int i = 0; i < tot_vertices; i++) {
        igraph_vector_t followers;
        igraph_vector_init(&followers, 0);
        igraph_neighbors(graph, &followers, i, IGRAPH_IN);

        igraph_vector_push_back(&sample_vector, igraph_vector_size(&followers));

        igraph_vector_destroy(&followers);
    }
    double *sample = malloc(sizeof(double) * tot_vertices);
    igraph_vector_copy_to(&sample_vector, sample);
    igraph_vector_destroy(&sample_vector);

    double mean = gsl_stats_mean(sample, sizeof(double), tot_vertices);
    double sdev = gsl_stats_sd_m(sample, sizeof(double), tot_vertices, mean);

    igraph_vector_t *most_followed = malloc(sizeof(igraph_vector_t));
    igraph_vector_init(most_followed, 0);

    for (int i = 0; i < tot_vertices; i++) {
        if (sample[i] > mean + sdev) {
            igraph_vector_push_back(most_followed, i);
        }
    }
    return most_followed;
}
