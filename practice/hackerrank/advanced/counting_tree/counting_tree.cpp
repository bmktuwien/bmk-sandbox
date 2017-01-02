#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

struct Node {
  int id;
  long data;
  int parentId;
  int height;
  int run;
};

void connect(Node nodes[], Node& child, Node& parent) {
    if (child.parentId != -1) {
        vector<int> stack;

        int i = child.id;
        while (i != -1) {
            stack.push_back(i);
            i = nodes[i].parentId;
        }

        for (int j = stack.size() - 1; j > 0; j--) {
            nodes[stack[j]].parentId = stack[j-1];
        }
    }

    child.parentId = parent.id;
}

void markPath(Node nodes[], int n1, int n2, int run) {
        int h1 = nodes[n1].height;
        int h2 = nodes[n2].height;

	int ticks = 0;
        if (h1 > h2) {
            while (h1 != h2) {
		nodes[n1].run = run;
                n1 = nodes[n1].parentId;
                h1--;
            }
        } else if (h2 > h1) {
            while (h1 != h2) {
		nodes[n2].run = run;
                n2 = nodes[n2].parentId;
                h2--;
            }
        }

        while (n1 != n2) {
            nodes[n1].run = run;
	    nodes[n2].run = run;
            
            n1 = nodes[n1].parentId;
            n2 = nodes[n2].parentId;
        }

        nodes[n1].run = run;
}

void query(Node nodes[], int x1, int y1, int x2, int y2, int run) {
	markPath(nodes, x1, y1, run);
	markPath(nodes, x2, y2, run);
	
	cout << 0 << endl;
}

int main() {
    	int n, q;
    	cin >> n >> q;

	Node nodes[n];

	// read nodes
	for (int i = 0; i < n; i++) {
		long c;
		cin >> c;
		nodes[i].id = i;
		nodes[i].data = c;
		nodes[i].parentId = -1;
		nodes[i].run = -1;	
	}

	// read edges
	for (int i = 0; i < n - 1; i++) {
            	int xId;
            	int yId;
		cin >> xId;
		cin >> yId;

            	connect(nodes, nodes[xId-1], nodes[yId-1]);
        }

	int rootId;
	vector<int> children[n];
	// add children
        for (int i = 0; i < n; i++) {
		if (nodes[i].parentId != -1) {
			children[nodes[i].parentId].push_back(i);		
		} else {
			rootId = i;		
		}	
	}
	
	// add height info
        int height = 1;
        vector<int> nextLvlNodes = children[rootId];
	nodes[rootId].height = 0;

        while (!nextLvlNodes.empty()) {
            vector<int> l;

            for (int i = 0; i < nextLvlNodes.size(); i++) {
                nodes[nextLvlNodes[i]].height = height;
                
		
		l.insert(l.end(), children[nextLvlNodes[i]].begin(), children[nextLvlNodes[i]].end());
            }

            height++;
            nextLvlNodes = l;
        }


	// read queries
	for (int i = 0; i < q; i++) {
		int x1;
            	int y1;
		int x2;
		int y2;
		cin >> x1;
		cin >> y1;
		cin >> x2;
		cin >> y2;

		query(nodes, x1-1, y1-1, x2-1, y2-1, i);
	}
}



