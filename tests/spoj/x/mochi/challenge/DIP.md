SPOJ DIP - Image Denoising (rank 1422)

Reduces salt-and-pepper noise using a 3x3 median filter. For each pixel, collects all available neighbors (up to 9, fewer at borders), sorts them with insertion sort, and outputs the median value. Since only 2-20% of pixels are noisy, this reliably corrects most corrupted pixels without blurring edges significantly.
