//#include <pair>
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;


bool compare(pair<int, int> a, pair<int, int> b){
    return a.first < b.first;
}

void print(vector< pair<int, int> > a, int len){
    for(int i = 0; i< len; ++i){
        cout << "(" << a[i].first << ", " << a[i].second << ")" << endl;
    }
}

int main(){
    int numCompetitors;
    int _case = 1;
    while (cin >> numCompetitors){
        vector< pair<int, int> > j1score; // Value then index
        vector< pair<int, int > > j2score;
        int score;
        for(int i = 0; i<numCompetitors; ++i){
            cin >> score;
            j1score.push_back(make_pair(score, i));
        }
        sort(j1score.begin(), j1score.end(), compare);
        //print(j1score, numCompetitors);
        for(int i = 0; i<numCompetitors; ++i){
            cin >> score;
            j2score.push_back(make_pair(score, i));
        }
        sort(j2score.begin(), j2score.end(), compare);
        //print(j2score, numCompetitors);
        bool not_printed = true;
        for(int i = 0; i < numCompetitors; ++i ){
            if(j1score[numCompetitors - i - 1].second != j2score[numCompetitors - i - 1].second){
                cout << "Case " << _case << ": " <<  i + 1  << endl;
                not_printed = false;
                break;
            }
        }
        if (not_printed){
            cout << "Case " <<  _case << ": agree" << endl;
        }
    ++_case;
    }
    return 0;
}









