#include <algorithm>
#include <iomanip>
#include <ios>
#include <iostream>
#include <string>
#include <vector>

using std::cin; 	using std::sort;
using std::cout; 	using std::streamsize;
using std::endl; 	using std::string;
using std::vector; 	using std::setprecision;

int main(){
	cout << "First name: ";
	string name;
	cin >> name;
	cout << "Hi, " << name << endl;

	cout << "Midterm and final grades: ";
	double midterm, fin;
	cin >> midterm >> fin;

	cout << "Enter all HW grades"
		"Followed by EOF: ";
	vector<double> homework;
	double x;
	while(cin >> x){
		homework.push_back(x);
	}

	sort(homework.begin(), homework.end());
	typedef vector<double>::size_type vec_sz;
	vec_sz size = homework.size();
	vec_sz mid = size/2;

	if (size==0){
		cout << "No Grades Entered." << endl;
		return 1;
	}
	
	double median;
	median = size % 2 == 0 ? 
		(homework[mid] + homework[mid-1])/2 
		: homework[mid];
	
	streamsize prec = cout.precision();
	cout << "Final grade: " << setprecision(3) 
		<< 0.2 * midterm + 0.4 * fin + 0.4 * median
		<< setprecision(prec) << endl;


	return 0;
}











