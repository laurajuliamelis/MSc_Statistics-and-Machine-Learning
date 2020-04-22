#include <Rcpp.h>
#include <vector>
//#include <algorithm>
#include <string>
using namespace Rcpp;
using namespace std;

//' The knapsack problem: brute force algorithm.
//' 
//' @param x an object of class data.frame with two variables v (values) and w (weights).
//' @param head numeric scalar object that represents the knapsack size.
//'
//' @return  \code{brute_force_knapsack} returns a list with two elements: the elements added to the knapsack and the maximum knapsack value.
//'
//' @examples
//' intToBinary(4,8)
//'
//' 
//' @export
// [[Rcpp::export]]
std::vector<int> intToBinary(int x, unsigned int head) {
  
  std::vector<int> arr;
  int q = x;
  int r;
  while(q != 0 || arr.size() < head) {
    if(arr.size() == head){
      break;
    }
    r = q % 2;
    q = q / 2;
    arr.push_back(r);
  }
  
  //std::reverse(arr.begin(), arr.end());
  
  return arr;
}