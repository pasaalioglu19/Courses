#include <iostream>
#include <cstdlib> 
#include <vector>
#include <ctime> // for srand 
using namespace std;

void quicksort(vector<int>& arr,int start,int end){
    if (start >= end)
        return;
    int pivot = start + std::rand() % (end - start + 1);
    int temp = arr[pivot];
    arr[pivot] = arr[end];
    arr[end] = temp;

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
                temp = arr[green_p];
                arr[green_p] = arr[orange_p];
                arr[orange_p] = temp;
            }       
        }      
    }  

    quicksort(arr,start,orange_p-1);
    quicksort(arr,orange_p+1,end);
}

int main(){
    vector <int> k = {3,12,5,732,35,26,78,83,0,6,93};
    int size = k.size();
    quicksort(k,0,size-1);
    cout << "Sorted array: ";
    for (int i = 0; i < size; i++) {
        cout << k[i] << " ";
    }

}
