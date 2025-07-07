#include <iostream>
#include <string>
int main(){
    int x=2;
    std::string label;
    switch(x){
        case 1: label="one"; break;
        case 2: label="two"; break;
        case 3: label="three"; break;
        default: label="unknown"; break;
    }
    std::cout<<label<<std::endl;
    return 0;
}
