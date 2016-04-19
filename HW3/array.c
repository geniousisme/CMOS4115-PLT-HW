#include <stdio.h>

int arr[2][3] = {{1, 2, 3}, {4, 5, 6}};

int main(void) {
	int i = 0, j = 0, sum = 0;
	for (i = 0; i < 2; i++) {
		for (j = 0; j < 3; j++) {
			sum += arr[i][j];
			printf("%x %d\n", &arr[i][j], arr[i][j]);
		}
	}
	// printf("&arr[1][2]: %x\n", &arr[1][1]);
	return 0;
}