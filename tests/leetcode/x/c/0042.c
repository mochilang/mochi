#include <stdio.h>
#include <stdlib.h>
int trap(int* h, int n) {
    int left = 0, right = n - 1, leftMax = 0, rightMax = 0, water = 0;
    while (left <= right) {
        if (leftMax <= rightMax) {
            if (h[left] < leftMax) water += leftMax - h[left]; else leftMax = h[left];
            left++;
        } else {
            if (h[right] < rightMax) water += rightMax - h[right]; else rightMax = h[right];
            right--;
        }
    }
    return water;
}
int main(){int t;if(scanf("%d",&t)!=1)return 0;for(int tc=0;tc<t;tc++){int n;scanf("%d",&n);int* a=(int*)malloc(sizeof(int)*(n>0?n:1));for(int i=0;i<n;i++)scanf("%d",&a[i]);if(tc)printf("\n");printf("%d",trap(a,n));free(a);}return 0;}
