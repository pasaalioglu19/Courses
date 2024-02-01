#include <iostream>
#include <cstdlib> 
#include <vector>
#include <ctime> // for srand 
using namespace std;

void quicksort(vector<int>& arr,int start,int end){
    if (start >= end)
        return;
    int pivot;

    if (end - start >= 2){ // If exist at least 3 elements
        int rand1 = start + std::rand() % (end - start + 1);
        int rand2,rand3;
        do{
            rand2 = start + std::rand() % (end - start + 1);
        } while (rand1 == rand2);
        do{
            rand3 = start + std::rand() % (end - start + 1);
        } while (rand1 == rand3 || rand2 == rand3);

        if ((rand1 < rand2 && rand2 < rand3) || (rand3 < rand2 && rand2 < rand1))
            pivot = rand2;
        else if ((rand2 < rand1 && rand1 < rand3) || (rand3 < rand1 && rand1 < rand2))
            pivot = rand1;
        else{
            pivot = rand3;
        }
        int temp = arr[pivot];
        arr[pivot] = arr[end];
        arr[end] = temp;
    }
    else{
        pivot = end;
    }

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
    vector <int> k = {3,12,5,732,35,26,78,83,0,6,93};
    int size = k.size();
    quicksort(k,0,size-1);
    cout << "Sorted array: ";
    for (int i = 0; i < size; i++) {
        cout << k[i] << " ";
    }

}
