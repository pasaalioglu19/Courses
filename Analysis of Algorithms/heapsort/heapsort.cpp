#include <iostream>
#include <vector>
#include <cmath> //for log
#include <fstream> //for i/o operations
#include <sstream> //for i/o operations
#include <chrono> //for measure time

using namespace std;

//Structure for holding cities and numbers for values to be read from the file
struct CityData {
    string city;
    int population;
};

void max_heapify(vector<CityData>& arr, int s, int size) {
    int left = 2*s; //left child
    int right = 2*s + 1; //right child
    
    int largest;
    //compare left child with parent
    if (left <= size && arr[left-1].population > arr[s-1].population)
    {
        largest = left;
    }
    else{
        largest = s;
    }

    //compare right child with parent
    if (right <= size && arr[right-1].population > arr[largest-1].population)
    {
        largest = right;
    }
    
    //if largest value changed, parent and parent's child swap. Then call max_heapify for this child
    if (largest != s)
    {
        int temp_pop = arr[s-1].population;
        string temp_city = arr[s-1].city;

        arr[s-1].population = arr[largest-1].population;
        arr[s-1].city = arr[largest-1].city;

        arr[largest-1].population = temp_pop;
        arr[largest-1].city = temp_city;
    
        max_heapify(arr,largest,size);
    }
}

void build_max_heap(vector<CityData>& arr){
    int heapSize = arr.size();
    //call max_heapify for all non-leaf nodes
    for (int i = heapSize/2; i >=1; i--)
    {   
        max_heapify(arr, i,heapSize);
    }
}

void heapsort(vector<CityData>&arr){
    int heapSize = arr.size();

    // Max heap structure created
    build_max_heap(arr);

    for (int i = heapSize; i > 1; i--)
    {   
        //swap root and last element
        int temp_pop = arr[0].population;
        string temp_city = arr[0].city;

        arr[0].population = arr[i-1].population;
        arr[0].city = arr[i-1].city;

        arr[i-1].population = temp_pop;
        arr[i-1].city = temp_city;

        //call max_heapify without last element
        max_heapify(arr,1,i-1);
    }
}

void max_heap_insert(vector<CityData>& arr, int key, string city) {
    // Insert the new element at the end of the heap
    CityData new_city;
    new_city.city = city;
    new_city.population = key;
    arr.push_back(new_city);

    int index_s = arr.size(); // Index of the new element+1
    int parent_s = index_s / 2; // Index of the parent element+1

    // Heapify-Up
    while (index_s > 1 && arr[parent_s - 1].population < arr[index_s - 1].population) {
        // Swap the new element with its parent
        int temp_pop = arr[parent_s-1].population;
        string temp_city = arr[parent_s-1].city;

        arr[parent_s-1].population = arr[index_s-1].population;
        arr[parent_s-1].city = arr[index_s-1].city;

        arr[index_s-1].population = temp_pop;
        arr[index_s-1].city = temp_city;

        // Move up to the parent level
        index_s = parent_s;
        parent_s /= 2;
    }
}

CityData heap_extract_max(vector<CityData>& arr) {
    CityData max = arr[0]; // since its max heap, max value is root
    int size = arr.size();
    arr[0].population = arr[size-1].population; //last element moved to root
    arr[0].city = arr[size-1].city;
    arr.pop_back();

    max_heapify(arr,1,size-1); //update array for maintain max heap sort

    return max;
}

void heap_increase_key(vector<CityData>& arr, int index, int key){
    int size = arr.size();
    if (index <= 0 || index > size){ 
        cout << "Invalid index value" << endl;
    }
    
    int parent;

    arr[index-1].population = key; //increases the key of a specified element

    //check parent and index for protect max heap structure
    while (index >= 1){ 
        parent = index / 2;
        if (arr[index-1].population > arr[parent-1].population)
        {
            int temp_pop = arr[index-1].population;
            string temp_city = arr[index-1].city;

            arr[index-1].population = arr[parent-1].population;
            arr[index-1].city = arr[parent-1].city;

            arr[parent-1].population = temp_pop;
            arr[parent-1].city = temp_city;
        }
 
        else{
            //if new key smaller than parent, max_heapify function called for this index
            max_heapify(arr, index, size);
            return;
        }

        index /= 2;
    }
}

CityData heap_maximum(vector<CityData>& arr){
    if (arr.empty())
    {
        cout << "Heap is empty" << endl;
        return CityData{"", 0};
    }

    return arr[0]; //return max value
}

int dary_calculate_height(int n, int d){
    float lgd_np1 = log(n*d - n + 1) / log(d); //the equation i obtained from the geometric sequence summation formula
    int lgd_np1_base = lgd_np1;
    if (lgd_np1 != lgd_np1_base)
    {
        lgd_np1_base++; //round to next whole number
    }
    return lgd_np1_base;
}

void dary_max_heapify(vector<CityData>& arr, int s, int size, int d){
    int largest = s;  
    int first_child = d*s - d + 2; //first child
    int child;

    //travel all childs
    for (int i = 0; i < d; i++)
    {   
        child = first_child + i;
        //compare child with parent
        if (child <= size && arr[child-1].population > arr[largest-1].population)
        {
            largest = child;
        }
    }

    //if largest value changed, parent and parent's child swap. Then call dary_max_heapify for this child
    if (largest != s)
    {
        int temp_pop = arr[s-1].population;
        string temp_city = arr[s-1].city;

        arr[s-1].population = arr[largest-1].population;
        arr[s-1].city = arr[largest-1].city;

        arr[largest-1].population = temp_pop;
        arr[largest-1].city = temp_city;
  
        dary_max_heapify(arr,largest,size,d);
    }
}

void dary_build_max_heap(vector<CityData>& arr, int d){
    int heapSize = arr.size();
    //call dary_max_heapify for all non-leaf nodes
    for (int i = (heapSize + d - 2) / d; i >=1; i--)
    {
        dary_max_heapify(arr, i,heapSize,d);
    }
}

CityData dary_extract_max(vector<CityData>& arr, int d){
    CityData max = arr[0]; // since its max heap, max value is root

    int size = arr.size();
    arr[0].population = arr[size-1].population; //last element moved to root
    arr[0].city = arr[size-1].city;
    arr.pop_back();

    dary_max_heapify(arr,1,size-1, d); //update array for maintain max heap sort

    return max;   
}

void dary_insert_element(vector<CityData>& arr, int key, int d, string city){
    // Insert the new element at the end of the heap
    CityData new_city;
    new_city.city = city;
    new_city.population = key;

    arr.push_back(new_city);

    int index_s = arr.size(); // Index of the new element+1
    int parent_s = (index_s + d - 2) / d; //Index of the parent element+1

    // Heapify-Up
    while (index_s > 1 && arr[parent_s - 1].population < arr[index_s - 1].population) {
        // Swap the new element with its parent
        int temp_pop = arr[parent_s-1].population;
        string temp_city = arr[parent_s-1].city;

        arr[parent_s-1].population = arr[index_s-1].population;
        arr[parent_s-1].city = arr[index_s-1].city;

        arr[index_s-1].population = temp_pop;
        arr[index_s-1].city = temp_city;


        // Move up to the parent level
        index_s = parent_s;
        parent_s = (index_s + d - 2) / d;
    }
}

void dary_increase_key(vector<CityData>& arr, int index, int key, int d){
    int size = arr.size();
    if (index <= 0 || index > size){ 
        cout << "Invalid index value" << endl;
    }
    
    int parent;

    arr[index-1].population = max(arr[index-1].population,key); // set A[i] to max(A[i], k) 

    //check parent and index for protect max heap structure
    while (index >= 2){ 
        parent = (index+d-2)/ d;
        
        //if child bigger than parent, they exchange
        if (arr[index-1].population > arr[parent-1].population)
        {
            int temp_pop = arr[index-1].population;
            string temp_city = arr[index-1].city;

            arr[index-1].population = arr[parent-1].population;
            arr[index-1].city = arr[parent-1].city;

            arr[parent-1].population = temp_pop;
            arr[parent-1].city = temp_city;
        }
        else{
            return;
        }

        //moving to parent node
        index = (index+d-2)/ d;;
    } 
}

int main(int argc, char *argv[]){
    //parameters are assigned to the required variables
    string filename = argv[1];
    string function_type = argv[2];
    string out_filename = argv[3];
    int d,i,k;
    string k_city;

    //The values of d i and k are appropriately defined by sequentially controlling the parameters
    for (int arg_index = 4; arg_index < argc; ++arg_index) {
        // Check if the argument is valid
        if (string(argv[arg_index]).size() > 1) {
            char parameterType = argv[arg_index][0];
            string numberPart = string(argv[arg_index]).substr(1);
            
            if (parameterType == 'd')
            {
                d = stoi(numberPart);
            }
            else if (parameterType == 'i')
            {
                i = stoi(numberPart);
            }
            else if (parameterType == 'k')
            {
                // so input style is k_cityname_population
                if (function_type == "max_heap_insert" || function_type == "dary_insert_element")
                {
                    int iter = 1;
                    char temp = numberPart[iter++];
                    do{
                        k_city += temp;
                        temp = numberPart[iter++];
                    }
                    while(temp != '_');

                    k = stoi(string(numberPart).substr(iter));
                }
                // so input style is kpopulation
                else{
                    k = stoi(numberPart);
                }
            }
        } 
        else {
            cout << "Invalid argument format: " << argv[arg_index] << endl;
        }
    }

    ifstream file(filename);
    if (!file.is_open()) {
        cerr << "Unable to open the file." << endl;
        return 1; // Return an error status
    }

    // Create a vector of type CityData
    vector<CityData> dataVector;

    string line;
    // Read each line from the file
    while (getline(file, line)) {
        istringstream lineStream(line);
        string city;
        string populationStr;

        // Read comma-separated columns
        if (getline(lineStream, city, ';') && getline(lineStream, populationStr)) {
            // Add city and population to the CityData struct
            CityData data;
            data.city = city;
            data.population = stoi(populationStr); 
            dataVector.push_back(data);
        }
    } 

    int size = dataVector.size();

    auto start_time = chrono::high_resolution_clock::now(); //Define an initial value for measuring time

    // The appropriate function is called according to the parameter sent
    if (function_type == "max_heapify")
    {
        max_heapify(dataVector, i, size);
    }
    else if (function_type == "build_max_heap")
    {
        build_max_heap(dataVector);
    }
    else if (function_type == "heapsort")
    {
        heapsort(dataVector);
    }
    else if (function_type == "max_heap_insert")
    {
        build_max_heap(dataVector);
        max_heap_insert(dataVector, k, k_city);
    }
    else if (function_type == "heap_extract_max")
    {
        build_max_heap(dataVector);
        CityData k = heap_extract_max(dataVector);
        cout << "Max heap city is " << k.city << " and its population is " << k.population << endl;
    }
    else if (function_type == "heap_increase_key")
    {
        build_max_heap(dataVector);
        heap_increase_key(dataVector,i,k);
    }
    else if (function_type == "heap_maximum")
    {
        build_max_heap(dataVector);
        CityData k = heap_maximum(dataVector);
        cout << "Max heap city is " << k.city << " and its population is " << k.population << endl;
    }
    else if (function_type == "dary_calculate_height")
    {
        cout << "dary_calculate_height is " << dary_calculate_height(size,d) << endl;
    }
    else if (function_type == "dary_extract_max")
    {
        dary_build_max_heap(dataVector,d);
        CityData k = dary_extract_max(dataVector,d);
        cout << "Max dary heap city is " << k.city << " and its population is " << k.population << endl;
    }
    else if (function_type == "dary_insert_element")
    {
        dary_insert_element(dataVector,k,d,k_city);
    }
    else if (function_type == "dary_increase_key")
    {
        dary_increase_key(dataVector,i,k,d);
    }
    else{
        cout << "Invalid function type" << endl;
    }

    auto end_time = chrono::high_resolution_clock::now(); //Define an finish value for measuring time
    auto elapsed_time = chrono::duration_cast<chrono::nanoseconds>(end_time - start_time).count(); //Calculate passing time as nanosecond 

    cout << "Time taken by "<< function_type << " is " << elapsed_time << " ns." << endl;

    //The sorted city names and their populations are printed to the file entered as a parameter (e.g. out.csv)
    ofstream outFile(out_filename);
    if (outFile.is_open()) {
        for (const auto& data : dataVector) {
            outFile << data.city << ";" << data.population << endl;
        }

        outFile.close();
    } 
    else {
        cerr << "Unable to open the output file." << endl;
    }
    
    return 0;
}