// Generated by the Mochi experimental C++ compiler
#include <bits/stdc++.h>
using namespace std;

template<typename T, typename=void> struct has_size : false_type {};
template<typename T> struct has_size<T, void_t<decltype(declval<T>().size()), decltype(declval<T>()[0])>> : true_type {};
template<typename T, typename=void> struct has_key_type : false_type {};
template<typename T> struct has_key_type<T, void_t<typename T::key_type>> : true_type {};

int main() {
	auto nums = vector<int>{1, 2, 3};
	([&](const auto& __v){ if constexpr(has_size<decay_t<decltype(__v)>>::value){ for(size_t i=0;i<__v.size();++i){ if(i) cout<<' '; cout<<__v[i]; } cout<<endl; } else { cout<<__v<<endl; } })(([&](const auto& __c,const auto& __e){ using C=decay_t<decltype(__c)>; if constexpr(is_same_v<C,string>) return __c.find(__e) != string::npos; else if constexpr(has_key_type<C>::value) return __c.find(__e) != __c.end(); else return find(__c.begin(), __c.end(), __e) != __c.end(); })(nums,2));
	([&](const auto& __v){ if constexpr(has_size<decay_t<decltype(__v)>>::value){ for(size_t i=0;i<__v.size();++i){ if(i) cout<<' '; cout<<__v[i]; } cout<<endl; } else { cout<<__v<<endl; } })(([&](const auto& __c,const auto& __e){ using C=decay_t<decltype(__c)>; if constexpr(is_same_v<C,string>) return __c.find(__e) != string::npos; else if constexpr(has_key_type<C>::value) return __c.find(__e) != __c.end(); else return find(__c.begin(), __c.end(), __e) != __c.end(); })(nums,4));
	return 0;
}
