#include <iostream>
#include <vector>
using namespace std;



template <class For>
bool my_equal(For b, For e, For d){
	while(b!=e){
		if(b!=d){
			return false;
		}
		++b;
		++d;
	}
	return true;
}

template <class In, class T>
In my_find(In b, In e, const T& t){
	while(b != e && *b != t){
		++b;
	}
	return b;
}

template <class In, class Out>
void my_copy(In b, In e, Out d){
	while(b!=e){
		*d++ = *b++;
	}
}





template <class In>
In my_search(In b, In e, In b2, In e2){
	//precondition: b2 < e2
	while(b!=e){
		if(*b == *b2){
			In tempb = b;
			In tempb2 = b2;
			b++;
			b2++;
			bool found = true;
			while(b != e2){
				if (*b != *b2){
					b = tempb;
					b2 = tempb2;
					found = false;
					break;
				}
				++b;
				++b2;
			}
			if (found){
				return b;
			}
		}
		++b;
	}
	return b;
}

template <class In>
In my_find_if(In b, In e, bool p(In &)){
	while(b!=e && !p(*b)){
		++b;
	}
	return b;
}


template <class In, class Out, class T>
Out my_remove_copy(In b, In e, Out d, const T& t){
	while(b!=e){
		if(t != *b){
			*d++ = *b;
		}
		++b;
	}
	return d;
}

template <class In, class Out, class T>
Out my_remove_copy_if(In b, In e, Out d, bool p(const T& t)){
	while(b!=e){
		if(p(*b)){
			*d++ = *b; 
		}
		++b;
	}
	return d;
}


template<class In, class T>
T my_accumulate(In b, In e, const T& t){
	T sum = t; // do I need typename?
	while(b != e){
		sum += *b++;
	}
	return sum;
}

template <class In, class Out, class T, class R>
Out my_transform(In b, In e, Out d, R (const T&)){
	while(b != e){
		*d++ = p(*b++);
	}
	return d;
}



//Remove is a pain to write, I'll do it later
// Same for partition





int main(){
	/* Testing, eventually */
	return 0;
}


















































