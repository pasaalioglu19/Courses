#include <iostream>
#include <vector>
using namespace std;

void quicksort(vector<int>& arr,int start,int end){
    if (start >= end)
        return;
    
    int green_p = start-1;
    int orange_p = start-1;

    while(green_p < end)
    {
        green_p++;
        if (arr[green_p] <= arr[end]) //compare current index value with pivot (last element)
        {
            orange_p++;
            if (green_p > orange_p)
            {
                int temp = arr[green_p];
                arr[green_p] = arr[orange_p];
                arr[orange_p] = temp;
            }       
        }      
    }  

    quicksort(arr,start,orange_p-1);
    quicksort(arr,orange_p+1,end);
}

int main(){
    vector <int> k = {3,12,5,732,35,26,78,6,83,0,6,93,-4,-7,42,2,5,-2,5};
    int size = k.size();

    quicksort(k,0,size-1);
    cout << "Sorted array: ";
    for (int i = 0; i < size; i++) {
        cout << k[i] << " ";
    }
}