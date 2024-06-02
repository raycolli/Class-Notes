// Program counts the number of distinct vertices in a graph given the graph edges as input
// Created by Gleici Pereira and Rachel Collier
// 04/08/2023


#include <iostream>
#include <vector>
#include <set>

int main() {
    //input graph edges
    std::vector<std::pair<int, int>> edges = { {1,2}, {1,3}, {3,2}, {3,5}, {3,7}, {3,15}, {5,1}, {5,2}, {6,4}, {6,11}, {6,13}, {7,1}, {7,3} };

    //insert vertices into a set
    std::set<int> vertices;
    for (const auto & edge : edges) {
        vertices.insert(edge.first);
        vertices.insert(edge.second);
    }

    // output unique elements
    std::cout << vertices.size() << std::endl;

    return 0;

    // the output is 10

}