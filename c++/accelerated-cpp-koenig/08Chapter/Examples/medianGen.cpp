#include <vector>
#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;
	
template<class T>
T median(vector<T> v)
{
	typedef typename vector<T>::size_type vec_sz;
	vec_sz size = v.size();
	if (size == 0)
		throw domain_error("median of an empty vector");
	sort(v.begin(), v.end());
	vec_sz mid = size/2;
	return size % 2 == 0 ? (v[mid] + v[mid-1]) / 2 : v[mid];

}



int main(){
	vector<int> store;
	cout << "Enter int values: " << endl;
	copy(istream_iterator<int>(cin), istream_iterator<int>(), back_inserter(store));
	copy(store.begin(), store.end(), ostream_iterator<int>(cout, " "));
	int med = median(store);
	cout << "\nMedian is " << med << endl; 
	return 0;
}













