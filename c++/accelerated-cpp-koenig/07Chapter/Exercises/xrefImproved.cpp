#include "split.h"
#include <map>
#include <unordered_set>
using namespace std;

map<string, unordered_set<int> >
xref(istream& in, vector<string> find_words(const string&) = split){
	// default argument is split
	string line;
	int line_number = 0;
	map<string, unordered_set<int> > ret;
	while (getline(in, line)) {
		++line_number; // so it matches
		vector<string> words = find_words(line);
			// could use url_finder above if we wanted
		for (vector<string>::const_iterator it = words.begin();
				it != words.end(); ++it)
			ret[*it].insert(line_number); // Doesn't account for repeats
	}
	return ret;
}

int main() {
	cout << "Enter lines: " << endl;
	map<string, unordered_set<int> > ret = xref(cin);

	for(map<string, unordered_set<int> >::const_iterator it = ret.begin();
			it != ret.end(); ++it){
		string svar = it->second.size() > 1 ? "s" : "" ;
		cout << "\"" << it->first << "\"" << " occurs on line" << svar <<  ": ";
		unordered_set<int>::const_iterator line_it = it->second.begin();
		cout << *line_it; // write the first line number
		++line_it;
		// Write the rest
		while (line_it != it->second.end()) {
			cout << ", " << *line_it;
			++line_it;
		}
		cout << endl; //nice output format
	}

	return 0;
}
     






