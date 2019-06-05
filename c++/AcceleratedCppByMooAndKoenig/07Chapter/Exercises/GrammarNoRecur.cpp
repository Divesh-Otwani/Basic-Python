#include "split.h"
#include <map>
#include <cstdlib>
#include <stdexcept>
#include <list>
//#include <ctime> // crazyness
using namespace std;

typedef vector<string> Rule;
typedef vector<Rule> Rule_collection;
typedef map<string, Rule_collection> Grammar;

Grammar read_grammar(istream& in){
	Grammar ret;
	string line;

	//Read in the grammar
	while(getline(in, line)){
		vector<string> entry = split(line);
		if(!entry.empty()){
			ret[entry[0]].push_back(Rule(entry.begin() + 1, entry.end()));
		}
	}
	return ret;
}

bool bracketed(const string& s){
	return s.size() > 1 && s[0] == '<' && s[s.size() - 1] == '>';

}

int nrand(int n){
	if (n <= 0 || n > RAND_MAX){
		throw domain_error("Argument to nrand is out of range");
	}
	const int bucket_size = RAND_MAX / n;
	int r;
	
	do {
		//srand(time(NULL));
		r = rand() / bucket_size;
	} while (r >= n);
	return r;
}
		// To understand the recursion get the postcontition:
		// ret is the terminal form of word appended in order.
void gen_aux(const Grammar& g, const string& word, list<string>& ret){
	vector<string> stack;
	stack.push_back(word);
	while(!stack.empty()){
		string last = stack[stack.size() - 1];
		stack.pop_back();
		if(!bracketed(last)){
			ret.push_front(last);
		}else{
			Grammar::const_iterator it = g.find(last);
			if(it == g.end()){
				throw logic_error("empty rule");
			}
			const Rule_collection& rc = it->second;
			const Rule& r = rc[nrand(rc.size())];
			for(Rule::const_iterator i=r.begin(); i != r.end(); ++i){
				stack.push_back(*i);
			}
		}
	}
}

list<string> gen_sentence(const Grammar& g){
	list<string> ret;
	gen_aux(g, "<sentence>", ret);
	return ret;

}

int main(){
	cout << "Enter a grammar: " << endl;
	list<string> sentence = gen_sentence(read_grammar(cin));
	list<string>::const_iterator it = sentence.begin();
	cout << "Random Sentence: " << endl << endl;
	if (!sentence.empty()){
		cout << *it;
		++it;
	}
	while(it != sentence.end()){
		cout << " " << *it;
		++it;
	}
	cout << endl;
	
	return 0;
}






































