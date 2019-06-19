#include <vector>
#include <iostream>
#include <iterator>
#include <set>
#include <stdexcept>
#include <algorithm>
using namespace std;

template <class T, class In>
T my_median(In b, In e){
	if (b==e)
		throw domain_error("Median of an empty sequence");
	
	vector<T> use;
	while(b!=e){
		use.push_back(*b++);
	}
	sort(use.begin(), use.end());
	typename vector<T>::size_type size = use.size();
	typename vector<T>::size_type mid = size / 2;
	if(size % 2){
		return use[mid];
	}else{
		return (use[mid] + use[mid-1]) / 2;
	}
	return *b;
}


int main(){
	vector <int> input;
	cout << "Enter integers: " << endl;
	copy(istream_iterator<int>(cin), istream_iterator<int>(), back_inserter(input));
	cout << "Median: " << my_median<int, vector<int>::const_iterator>(input.begin(), input.end()) << endl;

	return 0;
}


