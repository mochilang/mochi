#include <iostream>
#include <string>
std::string classify(int n){
    switch(n){case 0:return "zero";case 1:return "one";default:return "many";}}
int main(){
    int x=2;
    std::string label;
    switch(x){case 1:label="one";break;case 2:label="two";break;case 3:label="three";break;default:label="unknown";}
    std::cout<<label<<std::endl;
    std::string day="sun";
    std::string mood;
    if(day=="mon") mood="tired"; else if(day=="fri") mood="excited"; else if(day=="sun") mood="relaxed"; else mood="normal";
    std::cout<<mood<<std::endl;
    bool ok=true;
    std::cout<<(ok?"confirmed":"denied")<<std::endl;
    std::cout<<classify(0)<<std::endl;
    std::cout<<classify(5)<<std::endl;
    return 0;
}
