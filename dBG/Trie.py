import time


class TrieNode:
    def __init__(self):
        self.children = {}
        self.end_of_word = False
        self.word = ""
        self.edge = None


class Trie:
    def __init__(self, subs):
        self.root = TrieNode()
        self.subs = subs

    def add(self, edge, word):
        node = self.root
        for char in word:
            if char not in node.children:
                node.children[char] = TrieNode()
            node = node.children[char]
        node.end_of_word = True
        node.word = word
        node.edge = edge

    def search(self, word, weight, node, index, length, threshold):
        if index == length:
            return dict(), 0
        results = dict()
        new_weight = 0
        for char in node.children:
            score = self.subs[(char, word[index])]
            if score >= threshold:
                if node.children[char].end_of_word:
                    rel_sim = relative_similarity(self.subs, word, node.children[char].word)
                    results[node.children[char].edge] = rel_sim
                    new_weight += rel_sim * weight[node.children[char].edge]
                child_results, child_weight = self.search(word, weight, node.children[char], index + 1, length,
                                                          threshold)
                results.update(child_results)
                new_weight += child_weight
        return results, new_weight


def relative_similarity(subs, edge1, edge2):
    similarity = 0
    for char1, char2 in zip(edge1, edge2):
        if char1 != '.' and char2 != '.':  # Check for gapped
            similarity += (subs[(char1, char2)] / subs[(char1,char1)])
    return similarity


def find_similar_keys(keys, weights, threshold, similarity_const, subs):
    trie = Trie(subs)
    for key in keys.keys():
        trie.add(key, keys[key])
    similarity_property = dict()
    weights_property = dict()
    for cnt, key in enumerate(keys.keys()):
        start = time.time()
        similar_nodes, new_weight = trie.search(keys[key], weights, trie.root, 0, len(keys[key]), threshold)
        if key in similar_nodes.keys():
            del similar_nodes[key]  # Remove self
        similarity_property[key] = similar_nodes
        weights_property[key] = weights[key] + new_weight * similarity_const
        end = time.time()
        print(cnt, key[0]+key[1][-1], end-start)
    return similarity_property, weights_property
