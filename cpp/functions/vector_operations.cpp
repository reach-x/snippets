#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>

int main() {
    std::vector<int> numbers = {1, 2, 3, 4, 5};

    std::cout << "Numbers: ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    std::cout << "Size: " << numbers.size() << std::endl;

    int sum = std::accumulate(numbers.begin(), numbers.end(), 0);
    std::cout << "Sum: " << sum << std::endl;

    int max = *std::max_element(numbers.begin(), numbers.end());
    std::cout << "Max: " << max << std::endl;

    int min = *std::min_element(numbers.begin(), numbers.end());
    std::cout << "Min: " << min << std::endl;

    numbers.push_back(6);
    std::cout << "After push_back: ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    numbers.pop_back();
    std::cout << "After pop_back: ";
    for (int num : numbers) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    std::vector<int> squared;
    std::transform(numbers.begin(), numbers.end(), std::back_inserter(squared),
                   [](int x) { return x * x; });
    std::cout << "Squared: ";
    for (int num : squared) {
        std::cout << num << " ";
    }
    std::cout << std::endl;

    return 0;
}
