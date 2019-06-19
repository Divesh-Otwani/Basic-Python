"""
>>> mathstart = dt(10,4,"AM")
>>> mathend = dt(12, 6, "PM")
>>> me2 = dt(12,6,"PM")
>>> me2 == mathend
True
>>> print(mathstart)
10:20 AM
>>> mathtime = rb([1,2,3], mathstart, mathend)
>>> print(mathtime)
M Tu W  10:20 AM--12:30 PM
>>> math = cs("Analysis","Lippel", "HC", mathtime)
>>> print(math)
Course name: Analysis
Teacher: Lippel
Campus: HC
Time: M Tu W  10:20 AM--12:30 PM
>>> ctime = rb([1,4,5], dt(12,5,"PM"), dt(1,6,"PM"))
>>> comps = cs("Arch", "JD", "HC", ctime)
>>> print(comps)
Course name: Arch
Teacher: JD
Campus: HC
Time: M Th F  12:25 PM--1:30 PM
>>> math.conflicts(comps)
True
>>> ptime = rb([3,4,5], dt(9,3,"AM"), dt(10, 6, "AM"))
>>> print(ptime)
W Th F  9:15 AM--10:30 AM
>>> poetry = cs("Poetry", "Devan", "HC", ptime)
>>> print(poetry)
Course name: Poetry
Teacher: Devan
Campus: HC
Time: W Th F  9:15 AM--10:30 AM
>>> poetry.conflicts(comps)
False
>>> poetry.conflicts(math)
True
>>> x = Schedule("my first schedule")
>>> x.addCourse(poetry)
>>> x.addCourse(comps)

"""

from functools import reduce


def inRange(item, start, end):
    return (start < item) and (item < end) 

def RangeOverlap(s1, e1, s2, e2):
    return inRange(s1, s2, e2) or inRange(e1, s2, e2)



class DayTime:
    def __init__(self, hour, minute, dh): #true is AM
        self.hour = hour # 0 to 11
        self.minute = minute
        self.dh = dh
    
    def fill(self, message):
        print(message)
        self.hour = int(input("Enter the hour: "))
        self.minute = int(input("Enter the minutes in intervals of 5 (0-11): "))
        self.dh = bool(int(input("Enter 1 for am and 0 for pm: ")))
        assert(self.hour >0 and self.hour < 13)
        assert(self.minute >= 0 and self.minute < 12)

    def __str__(self):
        dayhf = "AM" if self.dh else "PM"
        mins = str(self.minute * 5)
        mins = "0" + mins if len(mins)==1 else mins
        return str(self.hour) + ":" + mins + " " + self.dh

    def comp(self):
        offset = {"AM": 0, "PM": 12*60}
        return (self.hour%12) *60 + self.minute * 5 + offset[self.dh]

    def __lt__(self, other):
        return self.comp() < other.comp()

    def __eq__(self, other):
        return self.comp() == other.comp()


dt = DayTime

class RecurringBlock:
    def __init__(self, days, start, end):
        self.days = days
        self.start = start
        self.end = end

    def fill(self, message):
        print("Message")
        print("\tMon: 1, Tue:2, Wed:3, Thur:4, Fri:5 is the key.")
        self.days = list(set(list(map(int, input("Enter days, comma sep: ").split(',')))))
        self.start = DayTime("\tEnter start time.")
        self.end = DayTime("\tEnter end time.")
    
    def __str__(self):
        dmap = {1:"M ", 2:"Tu ", 3:"W ", 4:"Th ", 5:"F "}
        days = reduce(lambda x,y: x+y, [dmap[i] for i in self.days]) + " "
        return days + str(self.start) + "--" + str(self.end)
    
    
    def conflicts(self, other):
        assert(type(other)==RecurringBlock)
        adays = set(self.days)
        bdays = set(other.days)
        if adays.intersection(bdays) == set():
            return False
        else:
            return RangeOverlap(self.start, self.end, other.start, other.end)


rb = RecurringBlock

class Course:

    def __init__(self, name, teacher, campus, timing):
        self.name = name
        self.teacher = teacher
        self.campus = campus
        self.timing = timing

    def fill(self):
        self.name = input("Enter course name: ")
        self.teacher = input("Enter teacher name: ")
        self.campus = input("Enter campus: ")
        self.timing = rb("Enter the course time\n\n")

    def __hash__(self):
        return self.name.__hash__()

    def __str__(self):
        ret = "{Course name: "+ self.name  
        ret += " Teacher: " + self.teacher
        ret += " Campus: " + self.campus
        ret += " Time: " + str(self.timing) + "}"
        return ret

    def conflicts(self, other):
        assert(type(other)==Course)
        #returns true iff conflicts
        return self.timing.conflicts(other.timing)

    def confwith(self, otherlist): #course v.s. list of courses
        return any([self.conflicts(i) for i in otherlist])

cs = Course


class Schedule:
    def __init__(self, name):
        self.name = name
        self.courses = list();
    def addCourse(self, course):
        assert(type(course) == Course)
        #Add some check that course is of a right type
        #Also check the course is not in conflict
        assert(not course.confwith(list(self.courses)))
        self.courses.append(course)

    def removeCourse(self, index):
        return self.courses.pop(index)

    def conflicts(self, crsList):
        """
        returns which courses this current schedule does not conflict with

        """
        return filter(lambda crs: not crs.confwith(list(self.courses)), crsList)


    def __str__(self):
        ret = ""
        i = 0
        for course in self.courses:
            ret += "index: "+ str(i)+ " " + course.__str__() + "\n"
            i += 1
        return ret

    def save(self, path):
        with open(path, 'w') as f:
            f.write(self.__str__())

    



def main():
    courses = list()
    curr = Schedule("CurrentSchedule")
    coursefile = input("Enter path of course file: ")
    with open(coursefile, 'r') as f:
        content = f.read().split('\n')
        for line in content:
            courses.append(courseline(line))
    while True: #use control C to break out.
        print("")
        ops = "1: print courses, 2: print schedule, 3: save schedule" + \
                "\n4: See compatible remaining courses, 5: Add a course" + \
                " 6: Remove a course: \n"
        option = int(input(ops))
        if option==1:
            i = 0
            for course in courses:
                print("index: ", i, course)
                i += 1
        elif option==2:
            print(curr)
        elif option==3:
            path = input("Enter path to save it: ")
            curr.save(path)
        elif option==4:
            compatible = curr.conflicts(courses)
            count = 0
            for crs in compatible:
                print("count:", count, crs)
                count += 1
        elif option==5:
            index = int(input("Enter index of course: "))
            curr.addCourse(courses.pop(index))
        elif option==6:
            index = int(input("Enter index of crs to remove: "))
            courses.append(curr.removeCourse(index))
        else:
            print("Invalid input entered.")
        print("")




def courseline(line):
    line = line.split()
    endtime = dt(int(line[-3]), int(line[-2])//5, line[-1])
    line = line[:-3]
    starttime = dt(int(line[-3]), int(line[-2])//5, line[-1])
    line = line[:-3]
    days = line[-1] #like '1,2,3'
    days = list(map(int, days.split(",")))
    ctime = rb(days, starttime, endtime)
    course = cs(line[0], line[1], line[2], ctime)
    return course
            
main()


"""
   
def _test():
    import doctest
    result = doctest.testmod()
    if result[0] == 0:
        print("Wahoo! Passed all", result[1], __file__.split('/')[-1], "tests!")
    else:
        print("Darn it")

_test()
"""



