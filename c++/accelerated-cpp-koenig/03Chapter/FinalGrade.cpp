#include <iomanip>
#include <ios>
#include <iostream>
#include <string>

using std::cin;    using std::cout;
using std::setprecision;    using std::string;
using std::endl;    using std::streamsize;
int main(){
	cout << "Enter your name: ";
	string name;
	cin >> name;
	cout << "Hi, " << name << endl;

	cout << "Enter midterm and final grades: ";
	double midterm, final;
	cin >> midterm >> final;

	cout << "Enter all your HW grades"
		"Followed by EOF (control D in linux): ";
	int count = 0;
	double sum = 0;
	double x;
	while(cin >> x){
		count++;
		sum += x;
	}
	streamsize prec = cout.precision();
	cout << "Final Grade: " << setprecision(3)
		<< 0.2 * midterm + 0.4 * final + 0.4 * sum / count
		<< setprecision(prec) << endl;

	return 0;

}
