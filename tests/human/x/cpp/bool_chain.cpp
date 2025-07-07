#include <iostream>
bool boom(){
    std::cout << "boom" << std::endl;
    return true;
}
int main(){
    std::cout << ((1<2)&&(2<3)&&(3<4)?"true":"false") << std::endl;
    // second chain short-circuits before boom
    if((1<2)&&(2>3)&&boom()){
        std::cout << "true" << std::endl;
    } else {
        std::cout << "false" << std::endl;
    }
    if((1<2)&&(2<3)&&(3>4)&&boom()){
        std::cout << "true";
    } else {
        std::cout << "false";
    }
    return 0;
}
