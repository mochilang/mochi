#include <iostream>
#include <string>
struct Todo { std::string title; };
int main(){
    Todo todo{"hi"};
    std::cout << todo.title;
    return 0;
}
