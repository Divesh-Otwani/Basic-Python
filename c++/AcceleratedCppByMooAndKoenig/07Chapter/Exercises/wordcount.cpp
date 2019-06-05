#include <iostream>
#include <map>
#include <algorithm>
#include <vector>
using namespace std;


bool comp(const pair<string, int>& a, const pair<string, int>& b){
	return (a.second) < (b.second);
}


int main(){
	string s;
	map<string, int> counters;
	// store each word and an associated counter
	// read the input, keeping track of each word and how often we see it
	cout << "Enter words: " << endl;
	while (cin >> s)
		++counters[s];
		// counters[s] default initializes to 0
	//write the words and associated counts
	vector<pair<string, int> > toPrint;
	
	for (map<string, int>::const_iterator it = counters.begin();
			it != counters.end(); ++it) {
		toPrint.push_back(*it);
	}
	sort(toPrint.begin(), toPrint.end(), comp);
	
	for (vector<pair<string, int> >::const_iterator it = toPrint.begin();
		it != toPrint.end(); ++it) {
		cout << it->first << "\t" << it->second << endl;
	}

	return 0;
}




