import csv
import pickle
import time
from collections import OrderedDict
from itertools import islice

import networkx as nx
from Bio import SeqIO
from Bio.Align import substitution_matrices

from Trie import find_similar_keys


class Debruijn:

    def __init__(self, k, sequences=None, theta=0, similarity_const=0.5, weight_threshold=0.8, sub='BLOSUM62'):
        self.k = k
        if sequences is None:
            raise ValueError('sequences must not be None')
        self.sequences = sequences
        self.similarity_const = similarity_const
        self.weight_threshold = weight_threshold
        self.theta = theta
        self.subs = substitution_matrices.load(sub)

    def traverse_in_order(self):
        threshold = max(dict(self.graph.edges).items(), key=lambda x: x[1]['weight'])[-1]['weight'] * self.weight_threshold
        step = 0
        seed_dict = OrderedDict()
        heaviest_weight = max(dict(self.graph.edges).items(), key=lambda x: x[1]['weight'])[-1]['weight']
        while heaviest_weight >= threshold:
            step += 1
            seed = self.__seed_of_consensus(threshold)
            print(heaviest_weight / (threshold / self.weight_threshold), heaviest_weight, '-', threshold, seed)
            if seed not in seed_dict:
                seed_dict[seed] = 1
            elif next(reversed(seed_dict)) == seed:
                seed_dict[seed] += 1
            heaviest_weight = max(dict(self.graph.edges).items(), key=lambda x: x[1]['weight'])[-1]['weight']

        return seed_dict

    def all_traverse_in_order(self, graph_load=False, graph_save=False, graph_path=None, out_file='out.csv'):
        self.alphabet = self.__init_alphabet()
        if not graph_load:
            self.graph = self.generate_graph(self.sequences)
            print(self.graph)

            self.W = nx.get_edge_attributes(self.graph, 'weight')
            tuples = nx.get_edge_attributes(self.graph, 'tuple_substr')
            similarity_property, weights_property = find_similar_keys(tuples, self.W, self.theta, self.similarity_const,
                                                                      self.subs)
            nx.set_edge_attributes(self.graph, similarity_property, 'similar_edges')
            nx.set_edge_attributes(self.graph, weights_property, 'weight')
            if graph_save:
                with open(graph_path, 'wb') as binary_file:
                    pickle.dump(self.graph, binary_file)
        else:
            with open(graph_path, 'rb') as binary_file:
                self.graph = pickle.load(binary_file)
            binary_file.close()
            print('Graph Loaded!')
        seed_list = self.traverse_in_order()
        with open(out_file, mode='w', newline='') as csvfile:
            writer = csv.writer(csvfile)

            # Write the header (optional)
            writer.writerow(['Pattern', 'Count'])

            # Write the key-value pairs
            for key, value in seed_list.items():
                writer.writerow([key, value])

    def generate_output(self, out_file, headers, dict_list, time_list):
        with open('Outputs/' + out_file, 'w') as out:
            writer = csv.writer(out)
            headers = ["Time"] + sorted(list(headers), reverse=True)
            writer.writerow(headers)
            for time, seed_dict in zip(time_list, dict_list):
                row = [time] + [0] * (len(headers) - 1)
                for i, seed in enumerate(headers):
                    if i != 0 and seed in seed_dict:
                        row[i] = seed_dict[seed]
                writer.writerow(row)

    def __seed_of_consensus(self, threshold):
        max_edge = max(self.graph.edges.items(), key=lambda x: x[1]['weight'])

        if not max_edge:
            return None

        forward_current_node = max_edge[0][1]
        back_current_node = max_edge[0][0]
        seed = self.graph.edges[max_edge[0]]['tuple_substr']
        visited = {max_edge[0]}

        # Forward
        while True:
            forward_node_list = [(n[0], n[1]['weight']) for n in self.graph[forward_current_node].items()
                                 if (forward_current_node, n[0]) not in visited]
            if not forward_node_list:
                break

            next_node = max(forward_node_list, key=lambda edge: edge[1])
            if next_node[1] < threshold:
                break

            visited.add((forward_current_node, next_node[0]))
            seed += next_node[0][-1]
            forward_current_node = next_node[0]

        # Backwards
        while True:
            back_node_list = [(n[0], n[2]['weight']) for n in self.graph.in_edges(back_current_node, data=True)
                              if (n[0], n[1]) not in visited]
            if not back_node_list:
                break

            next_node = max(back_node_list, key=lambda edge: edge[1])
            if next_node[1] < threshold:
                break

            visited.add((next_node[0], back_current_node))
            seed = next_node[0][0] + seed
            back_current_node = next_node[0]

        for edge in visited:
            self.graph.edges[edge]['weight'] -= 1
            for sim_edge in self.graph.edges[edge]['similar_edges']:
                self.graph.edges[sim_edge]['weight'] -= (
                        self.similarity_const * self.graph.edges[edge]['similar_edges'][sim_edge])

        return seed

    def generate_graph(self, sequences):
        debruijn_graph = nx.DiGraph()
        prev = None
        for sequence in sequences:
            for index, current in enumerate(self.__window(sequence, self.k)):
                if index != 0:
                    node1 = ''.join(prev)
                    node2 = ''.join(current)
                    if debruijn_graph.has_edge(node1, node2):
                        debruijn_graph.edges[node1, node2]['weight'] += 1
                    else:
                        debruijn_graph.add_edge(node1, node2, weight=1.0, similar_edges=dict(),
                                                tuple_substr=self.get_tuple_from_edge((node1, node2)))
                prev = current
        return debruijn_graph

    def __window(self, seq, k):
        it = iter(seq)
        result = tuple(islice(it, k - 1))
        if len(result) == k - 1:
            yield result
        for elem in it:
            result = result[1:] + (elem,)
            yield result

    def __init_alphabet(self):
        alphabet = set()
        for sequence in self.sequences:
            for char in sequence:
                alphabet.add(char)
        return alphabet

    def get_tuple_from_edge(self, edge):
        merged = ''
        for lt1, lt2 in zip(edge[0][1:], edge[1][:-1]):
            if lt1 in self.alphabet:
                merged += lt1
            elif lt2 in self.alphabet:
                merged += lt2
            else:
                if lt1 != lt2:
                    raise Exception('Graph edge error!')
                merged += lt1
        return edge[0][0] + merged + edge[1][-1]


def read_fasta_file(file_name):
    sequences = []
    with open(file_name, "r") as fasta_file:
        for record in SeqIO.parse(fasta_file, "fasta"):
            sequences.append(str(record.seq))
    return sequences


if __name__ == '__main__':
    start = time.time()
    path = 'rg-rgg.fasta' # Fasta file path
    sequences = read_fasta_file(path)
    sub = 'BLOSUM80'  # BLOSUM90, PAM30, ...
    threshold = 0.5  # 0, ...
    test = Debruijn(5, sequences=sequences, weight_threshold=threshold, sub=sub)
    test.all_traverse_in_order()
    end = time.time()
    print("Program finished")
    print(end - start)


