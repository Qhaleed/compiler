# Res Programming Language - Code Samples

This file contains various code snippets demonstrating the features of the Res programming language. Each sample can be copy-pasted into the Res compiler for testing. When copy pasting make sure to know paste it as one line. The user interface is based on the mysql cli, hence each line of code should be inputted one at a time.

## Sample 1: Basic Variables and Arithmetic

```res
// Basic variable declarations and arithmetic
typeshi int x = 10;
typeshi int y = 20;
typeshi int sum = x + y;
typeshi int product = x * y;
typeshi int difference = y - x;
typeshi int quotient = y / x;

coms("Sum:", sum);
coms("Product:", product);
coms("Difference:", difference);
coms("Quotient:", quotient);
```

## Sample 2: Working with Strings

```res
// String variables and operations
typeshi string greeting = "Hello";
typeshi string name = "World";
typeshi string message = "Welcome to Res!";

coms(greeting, name);
coms(message);
coms("Programming in Res is fun!");
```

## Sample 3: Floating Point Numbers

```res
// Float calculations
typeshi float pi = 3.14159;
typeshi float radius = 5.0;
typeshi float area = pi * radius * radius;
typeshi float circumference = 2.0 * pi * radius;

coms("Pi value:", pi);
coms("Radius:", radius);
coms("Area:", area);
coms("Circumference:", circumference);
```

## Sample 5: Simple Function Definitions

```res
// Function to multiply two numbers
func multiply(int x, int y) => {
    typeshi int result = x * y;
    coms("Result of multiplication:", result);
}

// Function to calculate square
func square(int num) => {
    typeshi int squared = num * num;
    coms("Square of", num, "is", squared);
}

// Test the functions
multiply(6, 7);
square(8);
```

## Sample 6: Calculator Functions

```res
// Calculator with multiple operations
func calculator(int a, int b) => {
    typeshi int sum = a + b;
    typeshi int diff = a - b;
    typeshi int prod = a * b;
    typeshi int div = a / b;

    coms("Calculator for", a, "and", b);
    coms("Addition:", sum);
    coms("Subtraction:", diff);
    coms("Multiplication:", prod);
    coms("Division:", div);
}

calculator(20, 4);
calculator(15, 3);
```

## Sample 7: Comparison Operations

```res
// Comparison and logical operations
typeshi int num1 = 10;
typeshi int num2 = 5;

// Note: These would work in a more complete implementation
// For now, we can demonstrate basic structure
typeshi int greater = num1;  // Would be: num1 > num2
typeshi int lesser = num2;   // Would be: num1 < num2

coms("Number 1:", num1);
coms("Number 2:", num2);
coms("Greater number:", greater);
coms("Lesser number:", lesser);
```

## Sample 8: Working with Multiple Data Types

```res
// Mixed data types
typeshi string studentName = "Alice";
typeshi int studentAge = 20;
typeshi float studentGPA = 3.75;
typeshi string course = "Computer Science";

coms("Student Information:");
coms("Name:", studentName);
coms("Age:", studentAge);
coms("GPA:", studentGPA);
coms("Course:", course);
```

## Sample 9: Function with String Parameters

```res
// Function working with strings
func greetStudent(string name, int age) => {
    coms("Hello", name);
    coms("You are", age, "years old");
    coms("Welcome to the programming course!");
}

func displayInfo(string title, float value) => {
    coms(title, ":", value);
}

greetStudent("Bob", 22);
displayInfo("Temperature", 98.6);
displayInfo("Price", 29.99);
```

## Sample 10: Mathematical Functions

```res
// Mathematical utility functions
func findAverage(int a, int b) => {
    typeshi int sum = a + b;
    typeshi int average = sum / 2;
    coms("Average of", a, "and", b, "is", average);
}

func calculatePerimeter(int length, int width) => {
    typeshi int perimeter = (length + width) * 2;
    coms("Rectangle perimeter:", perimeter);
}

func calculateArea(int length, int width) => {
    typeshi int area = length * width;
    coms("Rectangle area:", area);
}

// Test mathematical functions
findAverage(10, 20);
calculatePerimeter(5, 3);
calculateArea(5, 3);
```

## Sample 11: Nested Function Calls

```res
// Functions calling other functions
func double(int x) => {
    typeshi int result = x * 2;
    coms("Double of", x, "is", result);
}

func triple(int x) => {
    typeshi int result = x * 3;
    coms("Triple of", x, "is", result);
}

func processNumber(int num) => {
    coms("Processing number:", num);
    double(num);
    triple(num);
}

processNumber(7);
```

## Sample 12: Real-World Example - Grade Calculator

```res
// Grade calculator example
typeshi string studentName = "John Doe";
typeshi int exam1 = 85;
typeshi int exam2 = 92;
typeshi int exam3 = 78;

// Calculate total and average
typeshi int total = exam1 + exam2 + exam3;
typeshi int average = total / 3;

coms("Grade Report for:", studentName);
coms("Exam 1:", exam1);
coms("Exam 2:", exam2);
coms("Exam 3:", exam3);
coms("Total Points:", total);
coms("Average:", average);

// Function to display grade summary
func displayGradeSummary(string name, int avg) => {
    coms("Final Summary:");
    coms("Student:", name);
    coms("Final Average:", avg);
}

displayGradeSummary(studentName, average);
```

## Sample 13: Temperature Converter

```res
// Temperature conversion functions
func celsiusToFahrenheit(float celsius) => {
    typeshi float fahrenheit = (celsius * 9.0 / 5.0) + 32.0;
    coms(celsius, "Celsius =", fahrenheit, "Fahrenheit");
}

func fahrenheitToCelsius(float fahrenheit) => {
    typeshi float celsius = (fahrenheit - 32.0) * 5.0 / 9.0;
    coms(fahrenheit, "Fahrenheit =", celsius, "Celsius");
}

// Test temperature conversions
celsiusToFahrenheit(25.0);
fahrenheitToCelsius(77.0);
celsiusToFahrenheit(0.0);
fahrenheitToCelsius(32.0);
```

## Sample 14: Comments and Documentation

```res
// This is a comprehensive example with lots of comments

// Variable declarations for a simple inventory system
typeshi string product = "Laptop";
typeshi int quantity = 50;
typeshi float price = 899.99;

// Display product information
coms("Product Information:");
coms("Name:", product);          // Product name
coms("Quantity:", quantity);     // Available quantity
coms("Price:", price);           // Unit price

// Function to calculate total value
func calculateInventoryValue(int qty, float unitPrice) => {
    // Calculate total value of inventory
    typeshi float totalValue = qty * unitPrice;

    // Display the result
    coms("Total inventory value:", totalValue);
}

// Calculate and display total inventory value
calculateInventoryValue(quantity, price);

// End of inventory program
```

## Sample 15: Multiple Variable Types in One Program

```res
// Comprehensive example using all data types
typeshi string companyName = "TechCorp";
typeshi int employees = 150;
typeshi float revenue = 2500000.50;
typeshi int foundedYear = 2010;
typeshi string ceo = "Jane Smith";

// Display company info
coms("Company Profile:");
coms("Name:", companyName);
coms("Employees:", employees);
coms("Revenue:", revenue);
coms("Founded:", foundedYear);
coms("CEO:", ceo);

// Function to display growth metrics
func displayGrowth(int currentYear, int founded) => {
    typeshi int yearsInBusiness = currentYear - founded;
    coms("Years in business:", yearsInBusiness);
}

displayGrowth(2024, foundedYear);
```

---

## How to Use These Samples

1. Copy any sample code block
2. Run the Res compiler: `./main`
3. Paste the code into the interactive prompt
4. Type `\g` on a new line to compile
5. The compiler will generate C++ code and save it to a file

## Notes

- Each sample demonstrates different aspects of the Res language
- All samples follow the syntax rules defined in `tutorial.md`
- These examples can be combined or modified to create more complex programs
- Comments are included to explain the code functionality
