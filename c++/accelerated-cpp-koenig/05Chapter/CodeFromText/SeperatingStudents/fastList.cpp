#include <list>
#include "ExampleCode/Student_info.h"
#include "ExampleCode/grade.h"
#include <string>
using namespace std;

// predicate to determine whether a student failed
bool fgrade(const Student_info& s){
	return grade(s) < 60;
} 


list<Student_info> extract_fails(list<Student_info>& students)
{
	list<Student_info> fail;
	list<Student_info>::iterator iter = students.begin();
	while (iter != students.end()) {
		if (fgrade(*iter)) {
			fail.push_back(*iter);
			iter = students.erase(iter);

		} else
			++iter;

	}
	return fail;

}


int main(){
	cout << "Enter students: ";
	Student_info student;
	list<Student_info> students;
	while(read(cin, student)){
		students.push_back(student);
	}
	list<Student_info> failures = extract_fails(students);
	cout << "Not Failures:" << endl;
	for(list<Student_info>::const_iterator i=students.begin(); i!=students.end(); ++i){
		cout << i->name << endl;
	}

	return 0;
}


