#include <iostream>
#include "ascii.h"
using namespace std;

const string RESET = "\033[0m";        // Reset all formatting
const string RED = "\033[31m";          // Error messages (failures, exceptions)
const string GREEN = "\033[32m";        // Success messages (compilation successful)
const string YELLOW = "\033[33m";       // Warning messages (progress indicators)
const string BLUE = "\033[34m";         // Information messages (prompts, help)
const string MAGENTA = "\033[35m";      // Code output formatting (generated code)
const string CYAN = "\033[36m";         // Input prompts (REPL interface)
const string BOLD = "\033[1m";          // Bold text (emphasis, headers)

void printAscii(){
 cout << CYAN << BOLD;
    cout << R"(
  _____                _____                         _ _           
 |  __ \              / ____|                      (_) |          
 | |__) |___  ___    | |     ___  _ __ ___  _ __ _  _| | ___ _ __ 
 |  _  // _ \/ __|   | |    / _ \| '_ ` _ \| '__  || | |/ _ \ '__|
 | | \ \  __/\__ \   | |___| (_) | | | | | |_ | || | |  \ __/ |   
 |_|  \_\___||___/    \_____\___/|_| |_| |_|_ _ __||_|_| \__| |   
                                           | |  
                                           |_|
)" << RESET << endl;
    
    cout << YELLOW << "╔═══════════════════════════════════════════════════════════════════════════════════" << RESET << endl;
    cout << YELLOW << "║ " << GREEN << "Welcome to the Res Programming Language! " << RESET << endl;
    cout << YELLOW << "║ " << BLUE << "After writing code, type " << BOLD << "\\g" << RESET << BLUE << " to compile, " << BOLD << "Ctrl+C" << RESET << endl;
    cout << YELLOW << "║ " << CYAN << "All files will be saved to the 'saves' directory" << RESET << endl;
    cout << YELLOW << "║ " << CYAN << "Don't know what to write? Check out samples.md" << RESET << endl;
    cout << YELLOW << "║ " << RED << "Notes: You can either paste the code directly, or write it line by line" << RESET << endl;
    cout << YELLOW << "╚═══════════════════════════════════════════════════════════════════════════════════" << RESET << endl;
}
