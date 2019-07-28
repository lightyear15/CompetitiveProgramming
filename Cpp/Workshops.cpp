#include<bits/stdc++.h>

using namespace std;
#include <algorithm>

using Workshop = std::pair<int,int>;
struct Available_Workshops {
    std::vector<Workshop> workshop;
};
Available_Workshops* initialize(int start_time[], int duration[], int n)
{
    Available_Workshops *obj = new Available_Workshops();
    obj->workshop.resize(n);
    for (int i = 0; i < n; i++) {
        obj->workshop[i] = {start_time[i], duration[i]};
    }

    return obj;
}

unsigned FindFirstCompatible(const std::vector<Workshop> vec, unsigned idx)
{
    unsigned i = idx+1;
    int endTime = vec[idx].first + vec[idx].second;
#if 0
    while (i < vec.size() && endTime > vec[i].first)
    {
        i++;
    }
#else
    auto it = std::upper_bound(vec.cbegin() + i, vec.cend(), endTime, [](const int &endTime, const Workshop& el)
            {
                return endTime <= el.first;
            });
    i = it - vec.cbegin();
#endif
    return i;
}

static std::map<unsigned, unsigned> memoiz;

unsigned CalculateMaxWorkshops__(const std::vector<Workshop>& vec, unsigned index)
{
    if (index >= vec.size()) return 0;

    auto memIt = memoiz.find(index);
    if (memIt != memoiz.end())  return memIt->second;

    unsigned i = FindFirstCompatible(vec, index);
    if (i == vec.size()) return 0;

    unsigned maxK = 0;
    for (unsigned j = i; j < vec.size(); j++)
    {
        unsigned v = CalculateMaxWorkshops__(vec, j);
        if (v > maxK)
        {
            maxK = v;
        }
    }
    memoiz[index] = maxK+1;
    return maxK+1;
}

int CalculateMaxWorkshops(Available_Workshops* obj) {
    std::sort(obj->workshop.begin(), obj->workshop.end(), [](const auto& el1, const auto& el2) 
            {
                return el1.first < el2.first || ( el1.first == el2.first && el1.second < el2.second);
            });
    auto it = std::unique(obj->workshop.begin(), obj->workshop.end(), [](const auto& el1, const auto& el2)
            {
                return el1.first == el2.first;
            });
    obj->workshop.erase(it, obj->workshop.end());

    int maxWrk = 0;
    for (unsigned i = 0; i < obj->workshop.size(); i++)
    {
        int v = CalculateMaxWorkshops__(obj->workshop, i);
        if (v > maxWrk)
        {
            maxWrk = v;
        }
    }
    return maxWrk+1;
}

int main(int argc, char *argv[]) {
    int n; // number of workshops
    cin >> n;
    // create arrays of unknown size n
    int* start_time = new int[n];
    int* duration = new int[n];

    for(int i=0; i < n; i++){
        cin >> start_time[i];
    }
    for(int i = 0; i < n; i++){
        cin >> duration[i];
    }

    Available_Workshops * ptr;
    ptr = initialize(start_time,duration, n);
    cout << CalculateMaxWorkshops(ptr) << endl;
    return 0;
}

