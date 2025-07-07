#include <iostream>
#include <string>
struct Person{std::string name;int age;};
struct Book{std::string title;Person author;};
int main(){
    Book book{"Go",{"Bob",42}};
    std::cout<<book.author.name<<std::endl;
    return 0;
}
