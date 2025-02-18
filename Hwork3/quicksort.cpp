#include <iostream>
#include <stack>
#include <vector>

using namespace std;

// Function to manually swap elements 
void manualSwap(int& a, int& b) {
    int temp = a;
    a = b;
    b = temp;
}

// Function to partition the array 
int partition(vector<int>& arr, int low, int high) {
    int pivot = arr[high]; // Pivot is the last element
    int i = low - 1;

    for (int j = low; j < high; j++) {
        if (arr[j] < pivot) {
            i++;
            
            manualSwap(arr[i], arr[j]);
        }
    }
    // Final pivot placement
    manualSwap(arr[i + 1], arr[high]);

    return (i + 1);
}

// Non-recursive QuickSort function 
void quickSort(vector<int>& arr) {
    stack<pair<int, int>> stk;

    // Push initial left and right indexes
    if (!arr.empty()) { 
        stk.push({0, arr.size() - 1});
    }

    while (!stk.empty()) {
        int low = stk.top().first;
        int high = stk.top().second;
        stk.pop();

        if (low < high) {
            int pivotIndex = partition(arr, low, high);

            // Push right subarray 
            if (pivotIndex + 1 < high) {
                stk.push({pivotIndex + 1, high});
            }
            // Push left subarray 
            if (low < pivotIndex - 1) {
                stk.push({low, pivotIndex - 1});
            }
        }
    }
}

// Function to print the array
void printArray(const vector<int>& arr) {
    for (size_t i = 0; i < arr.size(); i++) { 
        cout << arr[i] << " ";
    }
    cout << endl;
}

// Main function to test the quicksort
int main() {
    vector<int> arr = {20, 5, 67, 34, 2, 33};

    cout << "Original array: ";
    printArray(arr);

    quickSort(arr);

    cout << "Sorted array: ";
    printArray(arr);

    return 0;
}
