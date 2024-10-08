#define DIM_M 32
#define DIM_N 32
#define ARRAY_SIZE 1024


float input_matrix[ARRAY_SIZE] = {
  94, 91, 7, 31, 65, 87, 56, 25,
  45, 24, 22, 28, 8, 18, 79, 25,
  41, 73, 91, 80, 23, 34, 73, 42,
  36, 51, 79, 30, 47, 81, 50, 41,
  68, 79, 3, 47, 52, 91, 17, 11,
  66, 8, 11, 33, 82, 7, 67, 95,
  20, 15, 80, 19, 64, 81, 57, 14,
  50, 31, 55, 80, 26, 54, 67, 31,
  64, 25, 38, 53, 32, 93, 97, 35,
  44, 71, 22, 93, 58, 70, 56, 70,
  98, 14, 10, 52, 14, 8, 17, 78,
  78, 18, 16, 91, 65, 70, 74, 81,
  32, 3, 60, 37, 53, 82, 24, 46,
  22, 96, 71, 44, 50, 98, 7, 24,
  74, 13, 62, 25, 96, 97, 90, 29,
  27, 20, 84, 60, 28, 95, 81, 94,
  24, 21, 40, 84, 88, 66, 1, 81,
  76, 18, 56, 47, 73, 25, 66, 44,
  59, 26, 78, 2, 87, 10, 65, 16,
  44, 92, 53, 56, 29, 86, 31, 99,
  54, 39, 76, 40, 26, 22, 85, 9,
  11, 12, 73, 18, 8, 27, 14, 61,
  6, 4, 99, 44, 27, 75, 77, 4,
  74, 30, 79, 55, 42, 72, 25, 92,
  54, 18, 95, 7, 71, 33, 98, 99,
  63, 35, 85, 81, 29, 17, 33, 49,
  36, 88, 41, 27, 84, 42, 31, 53,
  98, 19, 1, 56, 43, 43, 63, 47,
  6, 89, 42, 58, 74, 90, 7, 59,
  98, 7, 31, 89, 45, 86, 47, 61,
  72, 47, 20, 34, 91, 1, 6, 12,
  35, 70, 90, 13, 24, 93, 30, 7,
  61, 29, 66, 86, 87, 27, 78, 36,
  9, 6, 79, 88, 49, 94, 75, 46,
  93, 19, 64, 65, 46, 30, 28, 75,
  39, 16, 38, 39, 82, 48, 9, 3,
  32, 15, 71, 55, 3, 73, 28, 92,
  22, 59, 73, 19, 9, 80, 50, 60,
  37, 11, 69, 5, 78, 98, 34, 48,
  37, 14, 34, 5, 10, 72, 15, 6,
  81, 33, 88, 74, 9, 16, 8, 64,
  39, 40, 62, 63, 67, 63, 16, 61,
  10, 14, 17, 43, 65, 40, 41, 10,
  34, 46, 63, 87, 5, 81, 26, 61,
  76, 70, 44, 77, 31, 91, 3, 5,
  9, 53, 40, 31, 41, 71, 62, 31,
  68, 62, 76, 2, 17, 12, 48, 5,
  76, 26, 78, 79, 16, 34, 6, 54,
  7, 78, 4, 99, 77, 70, 99, 69,
  83, 4, 98, 39, 28, 35, 16, 7,
  43, 13, 71, 56, 79, 38, 44, 29,
  94, 96, 93, 53, 48, 27, 10, 74,
  6, 80, 74, 60, 40, 84, 22, 67,
  47, 3, 27, 81, 62, 94, 32, 95,
  48, 84, 70, 15, 83, 96, 23, 37,
  39, 62, 51, 31, 52, 95, 24, 35,
  67, 80, 16, 74, 82, 1, 88, 42,
  51, 88, 70, 90, 19, 18, 25, 97,
  40, 20, 65, 63, 81, 62, 73, 5,
  35, 39, 17, 66, 85, 51, 14, 87,
  30, 27, 58, 3, 89, 43, 38, 6,
  53, 43, 72, 83, 91, 11, 73, 69,
  6, 91, 67, 34, 18, 31, 5, 28,
  74, 4, 8, 5, 39, 39, 21, 41,
  6, 13, 60, 74, 53, 41, 6, 90,
  80, 30, 71, 35, 94, 29, 17, 30,
  7, 57, 66, 59, 7, 0, 74, 13,
  25, 62, 93, 73, 12, 20, 79, 11,
  15, 84, 0, 72, 73, 96, 96, 19,
  42, 67, 4, 25, 49, 1, 60, 93,
  10, 46, 63, 2, 67, 70, 28, 88,
  9, 44, 14, 55, 18, 99, 64, 46,
  40, 72, 84, 37, 59, 44, 23, 31,
  14, 12, 74, 29, 64, 54, 95, 94,
  50, 25, 81, 15, 27, 27, 13, 11,
  31, 76, 87, 59, 35, 76, 91, 61,
  64, 31, 22, 37, 56, 2, 66, 65,
  90, 61, 17, 52, 69, 80, 49, 98,
  61, 16, 58, 54, 0, 59, 39, 76,
  21, 83, 59, 3, 84, 75, 90, 59,
  56, 2, 28, 5, 9, 7, 40, 72,
  23, 39, 86, 35, 36, 91, 17, 92,
  17, 16, 27, 2, 74, 67, 92, 95,
  22, 7, 50, 92, 97, 8, 71, 33,
  32, 51, 78, 72, 33, 8, 70, 66,
  5, 7, 35, 21, 59, 85, 69, 87,
  3, 17, 53, 59, 80, 73, 15, 51,
  25, 49, 57, 19, 25, 86, 29, 20,
  78, 65, 53, 7, 16, 48, 45, 50,
  18, 80, 4, 64, 16, 88, 30, 72,
  43, 19, 36, 77, 48, 9, 65, 91,
  94, 54, 1, 73, 83, 32, 12, 61,
  79, 6, 9, 5, 56, 74, 38, 56,
  32, 68, 7, 68, 63, 57, 79, 49,
  93, 40, 6, 13, 37, 62, 99, 58,
  78, 35, 86, 65, 93, 43, 55, 11,
  30, 22, 79, 3, 33, 93, 71, 19,
  11, 78, 60, 51, 57, 72, 45, 61,
  14, 21, 51, 0, 58, 1, 70, 7,
  94, 50, 35, 12, 38, 24, 47, 9,
  91, 12, 73, 56, 58, 28, 46, 98,
  99, 84, 89, 29, 74, 22, 66, 83,
  26, 83, 73, 5, 80, 84, 71, 57,
  54, 56, 8, 26, 57, 56, 4, 12,
  7, 40, 41, 47, 68, 88, 44, 6,
  9, 74, 75, 16, 20, 6, 48, 0,
  11, 56, 19, 13, 4, 23, 62, 40,
  45, 15, 81, 38, 7, 30, 76, 87,
  43, 85, 41, 10, 60, 65, 95, 51,
  90, 21, 37, 22, 26, 45, 89, 59,
  81, 93, 87, 87, 49, 20, 39, 82,
  99, 16, 14, 23, 63, 80, 71, 7,
  10, 31, 12, 65, 28, 5, 28, 38,
  30, 19, 46, 94, 4, 8, 51, 7,
  60, 9, 89, 10, 57, 94, 56, 0,
  42, 88, 25, 82, 18, 8, 93, 47,
  15, 74, 70, 51, 86, 84, 42, 13,
  74, 97, 2, 57, 58, 7, 77, 11,
  14, 58, 42, 95, 43, 85, 53, 74,
  9, 42, 52, 49, 52, 7, 38, 72,
  9, 73, 99, 9, 20, 19, 69, 53,
  67, 96, 70, 1, 11, 20, 36, 44,
  10, 68, 50, 22, 71, 28, 90, 84,
  45, 4, 36, 96, 86, 33, 56, 0,
  8, 99, 24, 82, 52, 35, 96, 17,
  80, 74, 0, 26, 50, 13, 47, 83,
  78, 56, 91, 38, 47, 51, 78, 6,
  75, 51, 96, 76, 8, 81, 38, 37,
};
float verify_data[ARRAY_SIZE] = {
  94, 68, 64, 32, 24, 54, 54, 6,
  61, 32, 81, 76, 7, 6, 67, 30,
  6, 15, 40, 64, 56, 32, 78, 79,
  30, 91, 7, 43, 10, 15, 9, 8,
  91, 79, 25, 3, 21, 39, 18, 89,
  29, 15, 33, 70, 78, 80, 80, 27,
  13, 84, 72, 31, 2, 51, 65, 6,
  22, 12, 40, 85, 31, 74, 73, 99,
  7, 3, 38, 60, 40, 76, 95, 42,
  66, 71, 88, 44, 4, 74, 16, 58,
  60, 0, 84, 22, 28, 78, 53, 9,
  79, 73, 41, 41, 12, 70, 99, 24,
  31, 47, 53, 37, 84, 40, 7, 58,
  86, 55, 74, 77, 99, 60, 74, 3,
  74, 72, 37, 37, 5, 72, 7, 5,
  3, 56, 47, 10, 65, 51, 9, 82,
  65, 52, 32, 53, 88, 26, 71, 74,
  87, 3, 9, 31, 77, 40, 82, 89,
  53, 73, 59, 56, 9, 33, 16, 56,
  33, 58, 68, 60, 28, 86, 20, 52,
  87, 91, 93, 82, 66, 22, 33, 90,
  27, 73, 16, 91, 70, 84, 1, 43,
  41, 96, 44, 2, 7, 8, 48, 74,
  93, 28, 88, 65, 5, 84, 19, 35,
  56, 17, 97, 24, 1, 85, 98, 7,
  78, 28, 8, 3, 99, 22, 88, 38,
  6, 96, 23, 66, 40, 70, 45, 38,
  71, 46, 44, 95, 28, 42, 69, 96,
  25, 11, 35, 46, 81, 9, 99, 59,
  36, 92, 64, 5, 69, 67, 42, 6,
  90, 19, 31, 65, 72, 66, 50, 56,
  19, 98, 6, 51, 38, 13, 53, 17,
  45, 66, 44, 22, 76, 11, 63, 98,
  9, 22, 39, 9, 83, 47, 51, 53,
  80, 42, 14, 90, 23, 5, 18, 32,
  11, 99, 9, 90, 30, 74, 67, 80,
  24, 8, 71, 96, 18, 12, 35, 7,
  6, 59, 40, 53, 4, 3, 88, 43,
  30, 67, 12, 61, 39, 7, 80, 68,
  78, 84, 74, 21, 19, 97, 96, 74,
  22, 11, 22, 71, 56, 73, 85, 31,
  79, 73, 62, 40, 98, 27, 70, 72,
  71, 4, 74, 17, 86, 35, 4, 7,
  60, 89, 75, 37, 46, 2, 70, 0,
  28, 33, 93, 44, 47, 18, 81, 89,
  88, 19, 63, 31, 39, 81, 90, 83,
  35, 25, 29, 52, 35, 21, 64, 68,
  51, 29, 16, 22, 94, 57, 1, 26,
  8, 82, 58, 50, 73, 8, 29, 45,
  49, 9, 67, 41, 28, 62, 19, 91,
  94, 49, 64, 69, 36, 59, 16, 63,
  57, 74, 20, 26, 4, 58, 11, 50,
  18, 7, 70, 98, 25, 27, 17, 86,
  94, 80, 63, 71, 35, 94, 18, 11,
  29, 1, 54, 80, 91, 85, 88, 57,
  72, 22, 6, 45, 8, 7, 20, 13,
  79, 67, 56, 7, 66, 14, 33, 47,
  75, 50, 16, 62, 16, 32, 25, 73,
  17, 60, 95, 49, 17, 69, 30, 79,
  45, 66, 48, 89, 51, 77, 36, 47,
  25, 95, 70, 24, 44, 61, 49, 61,
  46, 60, 61, 31, 7, 95, 97, 69,
  30, 93, 94, 98, 92, 87, 72, 49,
  61, 83, 0, 59, 7, 11, 44, 83,
  41, 20, 98, 74, 59, 6, 36, 72,
  93, 37, 10, 68, 43, 48, 40, 6,
  7, 10, 50, 61, 17, 3, 43, 93,
  14, 26, 11, 81, 60, 14, 10, 78,
  73, 15, 14, 13, 26, 4, 88, 47,
  19, 11, 14, 62, 13, 84, 20, 91,
  57, 46, 25, 16, 16, 17, 19, 40,
  21, 83, 56, 93, 9, 58, 68, 56,
  91, 80, 10, 62, 78, 99, 41, 20,
  64, 69, 17, 76, 71, 70, 65, 67,
  66, 63, 81, 58, 27, 53, 36, 6,
  51, 73, 19, 87, 89, 42, 50, 91,
  80, 19, 52, 25, 2, 44, 27, 34,
  65, 5, 43, 2, 56, 15, 63, 34,
  59, 2, 15, 54, 2, 59, 77, 13,
  0, 5, 13, 87, 10, 95, 22, 38,
  23, 64, 14, 96, 87, 27, 84, 91,
  46, 78, 65, 17, 79, 83, 81, 18,
  7, 67, 27, 0, 74, 80, 48, 37,
  58, 80, 4, 49, 57, 43, 71, 47,
  34, 81, 8, 97, 10, 75, 42, 1,
  30, 98, 40, 12, 38, 96, 62, 31,
  0, 70, 27, 59, 67, 73, 9, 62,
  1, 84, 23, 20, 94, 85, 28, 51,
  73, 57, 17, 90, 65, 77, 31, 6,
  28, 34, 41, 48, 44, 23, 73, 5,
  74, 28, 13, 39, 92, 15, 65, 99,
  70, 71, 62, 39, 56, 53, 90, 78,
  42, 14, 78, 29, 16, 4, 53, 12,
  75, 48, 10, 5, 29, 37, 5, 28,
  13, 88, 11, 76, 95, 51, 91, 58,
  7, 57, 40, 82, 0, 74, 84, 6,
  36, 50, 78, 27, 44, 74, 98, 35,
  39, 37, 34, 76, 94, 39, 35, 74,
  25, 9, 31, 21, 22, 25, 94, 78,
  94, 54, 45, 99, 42, 9, 45, 75,
  51, 31, 18, 20, 92, 30, 19, 70,
  16, 14, 46, 26, 96, 62, 39, 4,
  62, 44, 76, 83, 7, 49, 54, 35,
  50, 56, 15, 16, 88, 42, 4, 51,
  79, 55, 16, 84, 53, 79, 1, 90,
  38, 34, 63, 78, 93, 51, 17, 8,
  93, 14, 87, 59, 50, 57, 1, 86,
  35, 8, 81, 14, 25, 52, 36, 96,
  30, 80, 91, 60, 56, 55, 56, 13,
  39, 5, 87, 79, 53, 31, 66, 5,
  73, 55, 59, 3, 92, 19, 73, 65,
  12, 26, 38, 23, 82, 49, 96, 76,
  47, 26, 65, 28, 29, 42, 43, 24,
  82, 10, 5, 16, 48, 52, 85, 39,
  12, 18, 35, 84, 97, 25, 83, 93,
  38, 57, 7, 63, 18, 52, 86, 8,
  81, 54, 70, 95, 86, 72, 43, 93,
  48, 72, 81, 34, 27, 95, 51, 39,
  20, 99, 76, 75, 8, 86, 32, 43,
  24, 56, 30, 80, 8, 7, 33, 81,
  50, 67, 74, 81, 31, 25, 63, 30,
  9, 15, 26, 6, 10, 24, 14, 21,
  79, 64, 91, 90, 71, 29, 12, 55,
  47, 4, 76, 71, 93, 38, 56, 38,
  41, 31, 81, 94, 99, 92, 47, 7,
  3, 6, 61, 54, 74, 35, 87, 41,
  11, 46, 61, 59, 33, 20, 61, 11,
  9, 12, 87, 7, 47, 72, 0, 37,
};
