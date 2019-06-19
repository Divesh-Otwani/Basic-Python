#include "split.h"
#include <map>
using namespace std;

map<string, vector<int> >
xref(istream& in,
		vector<string> find_words(const string&) = split){
	// default argument is split
	string line;
	int line_number = 0;
	map<string, vector<int> > ret;
	while (getline(in, line)) {
		++line_number; // so it matches
		vector<string> words = find_words(line);
			// could use url_finder above if we wanted
		for (vector<string>::const_iterator it = words.begin();
				it != words.end(); ++it)
			ret[*it].push_back(line_number); // Doesn't account for repeats
	}
	return ret;
}

int main() {
	cout << "Enter lines: " << endl;
	map<string, vector<int> > ret = xref(cin);

	for(map<string, vector<int> >::const_iterator it = ret.begin();
			it != ret.end(); ++it){
		cout << "\"" << it->first << "\"" << " occurs on line(s): ";
		vector<int>::const_iterator line_it = it->second.begin();
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
     






