#include <vector>
#include "ExampleCode/Student_info.h"
#include "ExampleCode/grade.h"
#include <string>
using namespace std;

// predicate to determine whether a student failed
bool fgrade(const Student_info& s){
	return grade(s) < 60;
} 

vector<Student_info> extract_fails(vector<Student_info>& students)
{
	vector<Student_info> pass, fail;
	for (vector<Student_info>::size_type i = 0;
			i != students.size(); ++i){
		if (fgrade(students[i])){
			fail.push_back(students[i]);
		}else{
			pass.push_back(students[i]);
		}
	}
	students = pass; // O(n) overwrite; slow.
	return fail;
}



int main(){
	cout << "Enter students: ";
	Student_info student;
	vector<Student_info> students;
	while(read(cin, student)){
		students.push_back(student);
	}
	vector<Student_info> failures = extract_fails(students);
	cout << "Not Failures:" << endl;
	for(vector<Student_info>::const_iterator i=students.begin(); i!=students.end(); ++i){
		cout << i->name << endl;
	}

	return 0;
}


