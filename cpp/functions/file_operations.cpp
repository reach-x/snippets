#include <iostream>
#include <fstream>
#include <string>

int main() {
    const std::string test_file = "../tmp/test_output.txt";

    // Write to file
    std::ofstream outfile(test_file);
    if (outfile.is_open()) {
        outfile << "Hello from C++!" << std::endl;
        outfile << "This is a test file." << std::endl;
        outfile.close();
        std::cout << "Written to " << test_file << std::endl;
    }

    // Read from file
    std::ifstream infile(test_file);
    if (infile.is_open()) {
        std::cout << "Read content:" << std::endl;
        std::string line;
        while (std::getline(infile, line)) {
            std::cout << line << std::endl;
        }
        infile.close();
    }

    // Check file size
    std::ifstream file(test_file, std::ios::ate | std::ios::binary);
    if (file.is_open()) {
        std::streamsize size = file.tellg();
        std::cout << "\nFile size: " << size << " bytes" << std::endl;
        file.close();
    }

    // Append to file
    std::ofstream appendfile(test_file, std::ios::app);
    if (appendfile.is_open()) {
        appendfile << "Appended line" << std::endl;
        appendfile.close();
        std::cout << "Appended to file" << std::endl;
    }

    return 0;
}
